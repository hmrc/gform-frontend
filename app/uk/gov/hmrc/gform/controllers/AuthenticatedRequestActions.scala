/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform
package controllers

import cats.Id
import cats.data.NonEmptyList
import cats.instances.future._
import cats.syntax.applicative._
import cats.syntax.eq._
import com.softwaremill.quicklens._
import org.slf4j.LoggerFactory
import play.api.i18n.{ I18nSupport, Langs, Messages, MessagesApi }
import play.api.mvc.Results._
import play.api.mvc._
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.retrieve.v2._
import uk.gov.hmrc.auth.core.{ InsufficientEnrolments, AuthConnector => _, _ }
import uk.gov.hmrc.gform.auth._
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.commons.MarkDownUtil
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers.GformSessionKeys.REFERRER_CHECK_DETAILS
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluator, SmartStringEvaluatorFactory }
import uk.gov.hmrc.gform.eval.{ DbLookupChecker, DelegatedEnrolmentChecker, SeissEligibilityChecker }
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.gform.SessionUtil.jsonFromSession
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.{ GraphException, Recalculation }
import uk.gov.hmrc.gform.lookup.LocalisedLookupOptions
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.userdetails.Nino
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ AffinityGroup, _ }
import uk.gov.hmrc.http.{ HeaderCarrier, SessionKeys }
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import java.util.UUID
import scala.concurrent.{ ExecutionContext, Future }

trait AuthenticatedRequestActionsAlgebra[F[_]] {
  def refreshSession(formTemplateId: FormTemplateId): Action[AnyContent]

  def authWithoutRetrievingForm(formTemplateId: FormTemplateId, operation: OperationWithoutForm)(
    f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => F[Result]
  ): Action[AnyContent]

  def authWithOptReferrerCheckWithoutRetrievingForm(formTemplateId: FormTemplateId, operation: OperationWithoutForm)(
    f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => F[Result]
  ): Action[AnyContent]

  def authAndRetrieveForm[U <: SectionSelectorType: SectionSelector](
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    operation: OperationWithForm
  )(
    f: Request[AnyContent] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[
      DataOrigin.Mongo
    ] => F[Result]
  ): Action[AnyContent]
}

class AuthenticatedRequestActions(
  gformConnector: GformConnector,
  fileUploadService: FileUploadService,
  authService: AuthService,
  appConfig: AppConfig,
  frontendAppConfig: FrontendAppConfig,
  val authConnector: AuthConnector,
  i18nSupport: I18nSupport,
  langs: Langs,
  actionBuilder: ActionBuilder[Request, AnyContent],
  errResponder: ErrResponder,
  sessionCookieBaker: SessionCookieBaker,
  recalculation: Recalculation[Future, Throwable],
  smartStringEvaluatorFactory: SmartStringEvaluatorFactory,
  lookupOptions: LocalisedLookupOptions
)(implicit
  ec: ExecutionContext,
  messagesApi: MessagesApi
) extends AuthenticatedRequestActionsAlgebra[Future] with AuthorisedFunctions {

  private val logger = LoggerFactory.getLogger(getClass)

  def getAffinityGroup(implicit request: Request[AnyContent]): Unit => Future[Option[AffinityGroup]] =
    _ => {

      val predicate = AuthProviders(AuthProvider.GovernmentGateway)

      authorised(predicate)
        .retrieve(Retrievals.affinityGroup) { case affinityGroup =>
          Future.successful(affinityGroup.map(AffinityGroupUtil.localAffinityGroup))
        }
    }

  def getGovermentGatewayId(implicit request: Request[AnyContent]): Unit => Future[Option[GovernmentGatewayId]] =
    _ =>
      authorised(AuthProviders(AuthProvider.GovernmentGateway))
        .retrieve(Retrievals.credentials) {
          case Some(maybeCredentials) =>
            Future.successful(toGovernmentGatewayId(maybeCredentials))
          case _ =>
            Future.successful(None)
        }
        .recover { case _ =>
          None
        }

  implicit def hc(implicit request: Request[_]): HeaderCarrier =
    HeaderCarrierConverter.fromRequestAndSession(request, request.session)

  def checkEnrolment(serviceId: ServiceId, identifiers: NonEmptyList[Identifier])(implicit
    hc: HeaderCarrier
  ): Future[CheckEnrolmentsResult] = {

    val predicate = Enrolment(serviceId.value)

    authorised(predicate)
      .retrieve(Retrievals.allEnrolments) { case enrolments =>
        checkIdentifiers(identifiers)(enrolments).pure[Future]
      }
      .recoverWith {
        case ex @ InsufficientEnrolments(enrolment) =>
          logger.error("tax-enrolment service returned 201, but enrolment check in auth failed", ex)
          CheckEnrolmentsResult.InsufficientEnrolments.pure[Future]
        case ex =>
          logger.error("tax-enrolment service returned 201, but auth call failed unexpectedly", ex)
          CheckEnrolmentsResult.Failed.pure[Future]
      }
  }

  private def toIdentifier(ei: EnrolmentIdentifier): Identifier = Identifier(ei.key, ei.value)

  private def checkIdentifiers(identifiers: NonEmptyList[Identifier])(enrolments: Enrolments): CheckEnrolmentsResult = {

    val matIdentifiers: Set[Identifier] = enrolments.enrolments.flatMap(_.identifiers).map(toIdentifier)
    if (identifiers.toList.toSet.subsetOf(matIdentifiers))
      CheckEnrolmentsResult.Successful
    else
      CheckEnrolmentsResult.InvalidIdentifiers
  }

  def refreshSession(formTemplateId: FormTemplateId): Action[AnyContent] = actionBuilder.async { implicit request =>
    implicit val lang: LangADT = getCurrentLanguage(request)
    val formTemplateWithRedirect = request.attrs(FormTemplateKey)
    val formTemplate = formTemplateWithRedirect.formTemplate
    for {
      authResult <- authService
                      .authenticateAndAuthorise(
                        formTemplateWithRedirect,
                        getAffinityGroup,
                        getGovermentGatewayId,
                        ggAuthorised(request),
                        getCaseWorkerIdentity(request)
                      )
      result <- authResult match {
                  case _: AuthSuccessful => Future.successful(Ok("success"))
                  case _ =>
                    errResponder.forbidden("Access denied - Unsuccessful Auth", Some(formTemplate))

                }
    } yield result
  }

  def getCurrentLanguage(request: Request[AnyContent]) = LangADT.fromRequest(request, langs)

  private def getCaseWorkerIdentity(request: Request[AnyContent]): Option[Cookie] =
    request.cookies.get(appConfig.`case-worker-assumed-identity-cookie`)

  override def authWithoutRetrievingForm(formTemplateId: FormTemplateId, operation: OperationWithoutForm)(
    f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => Future[Result]
  ): Action[AnyContent] =
    actionBuilder.async { implicit request =>
      authenticateAndProceed(formTemplateId, operation, f)
    }

  def authWithOptReferrerCheckWithoutRetrievingForm(formTemplateId: FormTemplateId, operation: OperationWithoutForm)(
    f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => Future[Result]
  ): Action[AnyContent] =
    actionBuilder.async { implicit request =>
      implicit val lang: LangADT = getCurrentLanguage(request)
      val formTemplateWithRedirect = request.attrs(FormTemplateKey)
      val formTemplate = formTemplateWithRedirect.formTemplate
      formTemplate.referrerConfig match {
        case Some(referrerConfig: ReferrerConfig) =>
          val referrerCheckDetails: ReferrerCheckDetails =
            jsonFromSession(request, REFERRER_CHECK_DETAILS, ReferrerCheckDetails.empty)

          def isRequestAllowedViaReferrer: Boolean =
            request.headers.get("Referer") match {
              case Some(referrer) => referrerConfig.isAllowed(referrer)
              case None           => false
            }

          if (referrerCheckDetails.checkDone(formTemplateId) || isRequestAllowedViaReferrer) {
            authenticateAndProceed(formTemplateId, operation, f).map(
              _.addingToSession(
                GformSessionKeys.REFERRER_CHECK_DETAILS -> JsonUtils.toJsonStr(referrerCheckDetails + formTemplateId)
              )
            )
          } else {
            errResponder.forbidden(
              "Restricted by referrer config",
              Some(formTemplate),
              Some(
                views.html.form.snippets.markdown_wrapper(MarkDownUtil.markDownParser(referrerConfig.exitMessage.value))
              )
            )
          }
        case None =>
          authenticateAndProceed(formTemplateId, operation, f)
      }
    }

  private def authenticateAndProceed(
    formTemplateId: FormTemplateId,
    operation: OperationWithoutForm,
    f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => Future[Result]
  )(implicit request: Request[AnyContent]): Future[Result] = {
    val formTemplateWithRedirect = request.attrs(FormTemplateKey)
    val formTemplate = formTemplateWithRedirect.formTemplate
    implicit val lang: LangADT = getCurrentLanguage(request)
    for {
      _ <- MDCHelpers.addFormTemplateIdToMdc(formTemplateId)
      authResult <- authService
                      .authenticateAndAuthorise(
                        formTemplateWithRedirect,
                        getAffinityGroup,
                        getGovermentGatewayId,
                        ggAuthorised(request),
                        getCaseWorkerIdentity(request)
                      )
      result <- handleAuthResults(
                  authResult,
                  formTemplate,
                  onSuccess = retrievals =>
                    role => {
                      val cache = AuthCacheWithoutForm(retrievals, formTemplate, role, lookupOptions)
                      Permissions.apply(operation, cache.role) match {
                        case PermissionResult.Permitted => f(request)(lang)(cache)
                        case PermissionResult.NotPermitted =>
                          errResponder.forbidden("Access denied - The form has not been permitted", Some(formTemplate))
                        case PermissionResult.FormSubmitted =>
                          errResponder
                            .forbidden("Access denied - The form has already been submitted", Some(formTemplate))
                      }
                    }
                )
    } yield result
  }

  def asyncNoAuth(formTemplateId: FormTemplateId)(
    f: Request[AnyContent] => LangADT => FormTemplate => Future[Result]
  ): Action[AnyContent] = actionBuilder.async { implicit request =>
    implicit val l: LangADT = getCurrentLanguage(request)

    val formTemplateContext = request.attrs(FormTemplateKey)
    val formTemplate = formTemplateContext.formTemplate
    for {
      result <- f(request)(l)(formTemplate)
    } yield result
  }

  def asyncGGAuth(
    formTemplateId: FormTemplateId
  )(f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => Future[Result]): Action[AnyContent] =
    actionBuilder.async { implicit request =>
      val predicate = AuthProviders(AuthProvider.GovernmentGateway)
      val formTemplateContext = request.attrs(FormTemplateKey)
      val formTemplate = formTemplateContext.formTemplate
      for {
        _          <- MDCHelpers.addFormTemplateIdToMdc(formTemplateId)
        authResult <- ggAuthorised(request)(RecoverAuthResult.noop)(predicate)
        result <- authResult match {
                    case AuthSuccessful(retrievals, role) =>
                      f(request)(getCurrentLanguage(request))(
                        AuthCacheWithoutForm(retrievals, formTemplate, role, lookupOptions)
                      )
                    case _ =>
                      errResponder.forbidden("Access denied - Unsuccessful GGAuth", Some(formTemplate))
                  }
      } yield result
    }

  def authAndRetrieveForm[U <: SectionSelectorType: SectionSelector](
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    operation: OperationWithForm
  )(
    f: Request[AnyContent] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[
      DataOrigin.Mongo
    ] => Future[Result]
  ): Action[AnyContent] =
    async(formTemplateId, maybeAccessCode) {
      implicit request => lang => cache => smartStringEvaluator => formModelOptics =>
        val formTemplateContext = request.attrs(FormTemplateKey)
        val formTemplate = formTemplateContext.formTemplate
        Permissions.apply(operation, cache.role, cache.form.status) match {
          case PermissionResult.Permitted => f(request)(lang)(cache)(smartStringEvaluator)(formModelOptics)
          case PermissionResult.NotPermitted =>
            errResponder.forbidden(
              "Access denied - The retrieved form has not been permitted",
              Some(formTemplate)
            )
          case PermissionResult.FormSubmitted =>
            Redirect(
              uk.gov.hmrc.gform.gform.routes.AcknowledgementController
                .showAcknowledgement(maybeAccessCode, cache.formTemplate._id)
            ).pure[Future]
        }
    }

  def async[U <: SectionSelectorType: SectionSelector](
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  )(
    f: Request[AnyContent] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[
      DataOrigin.Mongo
    ] => Future[Result]
  ): Action[AnyContent] =
    actionBuilder.async { implicit request =>
      import i18nSupport._
      implicit val l: LangADT = getCurrentLanguage(request)

      val formTemplateContext = request.attrs(FormTemplateKey)
      val formTemplate = formTemplateContext.formTemplate
      for {
        _ <- MDCHelpers.addFormTemplateIdToMdc(formTemplateId)
        _ <- MDCHelpers.addAccessCodeToMdc(maybeAccessCode)
        authResult <- authService
                        .authenticateAndAuthorise(
                          formTemplateContext,
                          getAffinityGroup,
                          getGovermentGatewayId,
                          ggAuthorised(request),
                          getCaseWorkerIdentity(request)
                        )
        result <- handleAuthResults(
                    authResult,
                    formTemplate,
                    onSuccess = withForm[U](f(request)(l))(maybeAccessCode, formTemplateContext)
                  )
      } yield result
    }

  private def withForm[U <: SectionSelectorType: SectionSelector](
    f: AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[DataOrigin.Mongo] => Future[Result]
  )(
    maybeAccessCode: Option[AccessCode],
    formTemplateContext: FormTemplateContext
  )(
    retrievals: MaterialisedRetrievals
  )(
    role: Role
  )(implicit
    messages: Messages,
    hc: HeaderCarrier,
    l: LangADT
  ): Future[Result] = {

    val formTemplate = formTemplateContext.formTemplate

    /* When user access form having email auth via bookmark, it can happen that user's form doesn't exist
     * and due to bookmark user didn't go via dashboard endpoint to create a new one, so we need to send him there.
     */
    def formNotFound(formIdData: FormIdData): Future[Result] = {
      val url = uk.gov.hmrc.gform.gform.routes.NewFormController.dashboard(formTemplate._id).url
      logger.info(s"Attempt to access form $formIdData, but form not found in MongoDB, redirecting user to $url")
      Redirect(url).pure[Future]
    }

    def noSpecimen(formTemplateId: FormTemplateId): FormTemplateId = FormTemplateId(
      formTemplateId.value.replace("specimen-", "")
    )

    def whenFormExists(form: Form): Future[Result] =
      for {
        _ <- MDCHelpers.addFormIdToMdc(form._id)
        formTemplateForForm <- if (form.formTemplateId === formTemplate._id)
                                 formTemplateContext.formTemplate.pure[Future]
                               else gformConnector.getFormTemplate(form.formTemplateId)
        specimenSource <- if (formTemplateForForm.isSpecimen) {
                            gformConnector.getFormTemplate(noSpecimen(formTemplateForForm._id)).map(Some(_))
                          } else None.pure[Future]
        formUpd = if (form.status === Submitted) {
                    form.copy(formTemplateId = formTemplate._id)
                  } else form
        cache = AuthCacheWithForm(
                  retrievals,
                  formUpd,
                  FormTemplateContext.basicContext(formTemplateForForm, specimenSource),
                  role,
                  maybeAccessCode,
                  lookupOptions
                )

        formModelOptics <-
          FormModelOptics
            .mkFormModelOptics[DataOrigin.Mongo, Future, U](cache.variadicFormData, cache, recalculation)

        formModelOpticsUpd =
          formModelOptics
            .modify(_.formModelVisibilityOptics.recalculationResult.evaluationContext.formTemplateId)
            .setTo(formUpd.formTemplateId)

        smartStringEvaluator =
          smartStringEvaluatorFactory
            .apply(
              formModelOpticsUpd.formModelVisibilityOptics
            )
        envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)(cache.formTemplate.objectStore)
        result   <- f(cache)(smartStringEvaluator)(formModelOptics)
      } yield result

    val formIdData = FormIdData(retrievals, formTemplate._id, maybeAccessCode)

    gformConnector.maybeForm(formIdData, formTemplate).flatMap(_.fold(formNotFound(formIdData))(whenFormExists))
  }

  private def handleAuthResults(
    result: AuthResult,
    formTemplate: FormTemplate,
    onSuccess: MaterialisedRetrievals => Role => Future[Result]
  )(implicit
    request: Request[_],
    l: LangADT
  ): Future[Result] =
    result match {
      case AuthSuccessful(retrievals: AnonymousRetrievals, role) =>
        onSuccess(retrievals)(role)
      case AuthSuccessful(retrievals: VerifyRetrievals, role) =>
        onSuccess(retrievals)(role)
      case AuthSuccessful(retrievals: AuthenticatedRetrievals, role) =>
        onSuccess(retrievals)(role)
      case AuthSuccessful(retrievals: EmailRetrievals, role) =>
        onSuccess(retrievals)(role)
      case AuthRedirect(loginUrl, flashing) => Redirect(loginUrl).flashing(flashing: _*).pure[Future]
      case AuthEmailRedirect(redirectUrl) =>
        Redirect(redirectUrl.url)
          .pure[Future]
      case AuthCustomRedirect(redirectUrl) =>
        Redirect(redirectUrl.url)
          .pure[Future]
      case AuthAnonymousSession(redirectUrl) =>
        Redirect(redirectUrl.url, request.queryString)
          .withSession(SessionKeys.sessionId -> s"anonymous-session-${UUID.randomUUID()}")
          .pure[Future]
      case AuthRedirectFlashingFormName(loginUrl) =>
        Redirect(loginUrl).flashing("formTitle" -> formTemplate.formName.value).pure[Future]
      case AuthBlocked(message) =>
        errResponder.forbiddenWithReason(message, Some(formTemplate))
      case AuthForbidden(message) =>
        errResponder.forbidden(message, Some(formTemplate))
    }

  val defaultRetrievals = Retrievals.credentials and
    Retrievals.allEnrolments and
    Retrievals.affinityGroup and
    Retrievals.groupIdentifier and
    Retrievals.nino and
    Retrievals.email and
    Retrievals.name and
    Retrievals.confidenceLevel

  private def ggAuthorised(
    request: Request[AnyContent]
  )(
    recoverPF: PartialFunction[Throwable, AuthResult]
  )(
    predicate: Predicate
  ): Future[AuthResult] = {
    import uk.gov.hmrc.auth.core.retrieve.~

    implicit val hc: HeaderCarrier =
      HeaderCarrierConverter.fromRequestAndSession(request, request.session)

    authorised(predicate)
      .retrieve(defaultRetrievals) {
        case maybeCredentials ~ enrolments ~ maybeAffinityGroup ~ maybeGroupIdentifier ~ maybeNino ~ maybeEmail ~ maybeName ~ confidenceLevel =>
          val maybeRetrievals =
            for {
              govermentGatewayId <- maybeCredentials.flatMap(toGovernmentGatewayId)
              affinityGroup      <- maybeAffinityGroup
              groupIdentifier    <- maybeGroupIdentifier
            } yield AuthenticatedRetrievals(
              govermentGatewayId,
              enrolments,
              AffinityGroupUtil.localAffinityGroup(affinityGroup),
              groupIdentifier,
              maybeNino.map(Nino(_)),
              OtherRetrievals(
                name = maybeName,
                email = maybeEmail
              ),
              confidenceLevel
            )

          val maybeVerifyRetrievals =
            for {
              verifyId <- maybeCredentials.flatMap(toVerifyId)
              nino     <- maybeNino
            } yield VerifyRetrievals(verifyId, Nino(nino))

          maybeRetrievals
            .orElse(maybeVerifyRetrievals)
            .fold[AuthResult](
              AuthForbidden(s"""|Missing affinityGroup or groupIdentifier or govermentGateway credentials:
                                |AffinityGroup: $maybeAffinityGroup
                                |Credentials: $maybeCredentials
                                |GroupIdentifier: $maybeGroupIdentifier""".stripMargin)
            )(retrievals => AuthSuccessful(retrievals, roleFromMaterialisedRetrievals(retrievals)))
            .pure[Future]

      }
      .recover(recoverPF orElse RecoverAuthResult.basicRecover(request, appConfig))
  }

  private def toGovernmentGatewayId(credentials: Credentials): Option[GovernmentGatewayId] = credentials match {
    case Credentials(ggId, "GovernmentGateway") => Some(GovernmentGatewayId(ggId))
    case _                                      => None
  }

  private def toVerifyId(credentials: Credentials): Option[VerifyId] = credentials match {
    case Credentials(id, "Verify") => Some(VerifyId(id))
    case _                         => None
  }

  private def roleFromMaterialisedRetrievals(affinityGroup: MaterialisedRetrievals): Role = affinityGroup match {
    case x: AuthenticatedRetrievals => ggRoleFromAffinityGroup(x.affinityGroup)
    case _                          => Role.Customer
  }

  private def ggRoleFromAffinityGroup(affinityGroup: AffinityGroup): Role = affinityGroup match {
    case AffinityGroup.Individual   => Role.Customer
    case AffinityGroup.Organisation => Role.Customer
    case AffinityGroup.Agent        => Role.Agent
  }

  def getItmpRetrievals(implicit
    request: Request[AnyContent]
  ): Future[ItmpRetrievals] = {
    import uk.gov.hmrc.auth.core.retrieve.~
    val itmpRetrievals = Retrievals.itmpName and
      Retrievals.itmpDateOfBirth and
      Retrievals.itmpAddress
    val predicate = AuthProviders(AuthProvider.GovernmentGateway)

    authorised(predicate)
      .retrieve(itmpRetrievals) { case itmpName ~ itmpDateOfBirth ~ itmpAddress =>
        val updatedCountry = itmpAddress.flatMap(_.countryName).filterNot(isInUK)
        val updatedItmpAddress = itmpAddress.map(_.copy(countryName = updatedCountry))
        ItmpRetrievals(itmpName, itmpDateOfBirth, updatedItmpAddress).pure[Future]
      }
  }

  def isInUK(country: String): Boolean = ukParts(country.toUpperCase)

  private val ukParts = Set("ENGLAND", "SCOTLAND", "WALES", "NORTHERN IRELAND", "GREAT BRITAIN", "UNITED KINGDOM")
}

sealed trait AuthCache {
  def retrievals: MaterialisedRetrievals
  def formTemplate: FormTemplate
  def role: Role
  def accessCode: Option[AccessCode]
  def countryLookupOptions: LocalisedLookupOptions
}

case class AuthCacheWithForm(
  retrievals: MaterialisedRetrievals,
  form: Form,
  formTemplateContext: FormTemplateContext,
  role: Role,
  accessCode: Option[AccessCode],
  countryLookupOptions: LocalisedLookupOptions
) extends AuthCache {
  val formTemplate: FormTemplate = formTemplateContext.formTemplate
  val formTemplateId: FormTemplateId = formTemplate._id
  def formModel[U <: SectionSelectorType: SectionSelector](implicit
    hc: HeaderCarrier
  ): FormModel[DependencyGraphVerification] = {
    import uk.gov.hmrc.gform.typeclasses.identityThrowableMonadError
    FormModelBuilder
      .fromCache(
        this,
        toCacheData,
        new Recalculation[Id, Throwable](
          SeissEligibilityChecker.alwaysEligible,
          DelegatedEnrolmentChecker.alwaysDelegated,
          DbLookupChecker.alwaysPresent,
          (s: GraphException) => new IllegalArgumentException(s.reportProblem)
        ),
        form.componentIdToFileId,
        countryLookupOptions
      )
      .dependencyGraphValidation
  }

  def toCacheData: CacheData = new CacheData(
    form.envelopeId,
    form.thirdPartyData,
    formTemplate
  )
  def variadicFormData[U <: SectionSelectorType: SectionSelector](implicit
    hc: HeaderCarrier
  ): VariadicFormData[SourceOrigin.OutOfDate] =
    VariadicFormData.buildFromMongoData(formModel, form.formData.toData)

}

case class AuthCacheWithoutForm(
  retrievals: MaterialisedRetrievals,
  formTemplate: FormTemplate,
  role: Role,
  countryLookupOptions: LocalisedLookupOptions
) extends AuthCache {
  override val accessCode: Option[AccessCode] = None
  def toCacheData: CacheData = new CacheData(
    EnvelopeId(""),
    ThirdPartyData.empty,
    formTemplate
  )
  def toAuthCacheWithForm(form: Form, accessCode: Option[AccessCode]) =
    AuthCacheWithForm(
      retrievals,
      form,
      FormTemplateContext.basicContext(formTemplate, None),
      role,
      accessCode,
      countryLookupOptions
    )
}

class CacheData(
  val envelopeId: EnvelopeId,
  val thirdPartyData: ThirdPartyData,
  val formTemplate: FormTemplate
)
