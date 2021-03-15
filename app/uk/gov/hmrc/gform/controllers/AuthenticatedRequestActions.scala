/*
 * Copyright 2021 HM Revenue & Customs
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
import org.slf4j.LoggerFactory

import java.util.UUID
import play.api.i18n.{ I18nSupport, Langs, MessagesApi }
import play.api.mvc.Results._
import play.api.mvc._

import scala.concurrent.ExecutionContext
import scala.language.higherKinds
import uk.gov.hmrc.auth.core.{ AffinityGroup, AuthConnector => _, _ }
import uk.gov.hmrc.gform.auth._
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.eval.{ DbLookupChecker, DelegatedEnrolmentChecker, SeissEligibilityChecker }
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadConnector }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.GraphException
import uk.gov.hmrc.gform.models.{ DependencyGraphVerification, FormModel, FormModelBuilder, SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.userdetails.Nino
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormComponentIdToFileIdMapping, FormIdData, FormModelOptics, ThirdPartyData, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.SessionKeys
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.retrieve.v2._
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.InsufficientEnrolments
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluator, SmartStringEvaluatorFactory }
import uk.gov.hmrc.gform.FormTemplateKey

import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

trait AuthenticatedRequestActionsAlgebra[F[_]] {
  def keepAlive(): Action[AnyContent]

  def authWithoutRetrievingForm(formTemplateId: FormTemplateId, operation: OperationWithoutForm)(
    f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => F[Result]): Action[AnyContent]

  def authAndRetrieveForm[U <: SectionSelectorType: SectionSelector](
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    operation: OperationWithForm)(
    f: Request[AnyContent] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[
      DataOrigin.Mongo] => F[Result]): Action[AnyContent]
}

class AuthenticatedRequestActions(
  gformConnector: GformConnector,
  fileUploadConnector: FileUploadConnector,
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
  smartStringEvaluatorFactory: SmartStringEvaluatorFactory
)(
  implicit
  ec: ExecutionContext,
  messagesApi: MessagesApi
) extends AuthenticatedRequestActionsAlgebra[Future] with AuthorisedFunctions {

  private val logger = LoggerFactory.getLogger(getClass)

  def getAffinityGroup(implicit request: Request[AnyContent]): Unit => Future[Option[AffinityGroup]] =
    _ => {

      val predicate = AuthProviders(AuthProvider.GovernmentGateway)

      authorised(predicate)
        .retrieve(Retrievals.affinityGroup) {
          case affinityGroup => Future.successful(affinityGroup)
        }
    }

  implicit def hc(implicit request: Request[_]): HeaderCarrier =
    HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

  def checkEnrolment(serviceId: ServiceId, identifiers: NonEmptyList[Identifier])(
    implicit hc: HeaderCarrier): Future[CheckEnrolmentsResult] = {

    val predicate = Enrolment(serviceId.value)

    authorised(predicate)
      .retrieve(Retrievals.allEnrolments) {
        case enrolments => checkIdentifiers(identifiers)(enrolments).pure[Future]
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

  def keepAlive(): Action[AnyContent] = actionBuilder.async { implicit request =>
    val predicate = AuthProviders(AuthProvider.GovernmentGateway)
    for {
      authResult <- ggAuthorised(request)(RecoverAuthResult.noop)(predicate)
      result <- authResult match {
                 case _: AuthSuccessful => Future.successful(Ok("success"))
                 case _                 => errResponder.forbidden(request, "Access denied")
               }
    } yield result
  }

  def getCurrentLanguage(request: Request[AnyContent]) = LangADT.fromRequest(request, langs)

  private def getCaseWorkerIdentity(request: Request[AnyContent]): Option[Cookie] =
    request.cookies.get(appConfig.`case-worker-assumed-identity-cookie`)

  private def authWithoutRetrievingForm(formTemplateId: FormTemplateId)(
    f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => Future[Result]): Action[AnyContent] =
    async(formTemplateId)(f)

  override def authWithoutRetrievingForm(formTemplateId: FormTemplateId, operation: OperationWithoutForm)(
    f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => Future[Result]): Action[AnyContent] =
    authWithoutRetrievingForm(formTemplateId) { request => lang => cache =>
      Permissions.apply(operation, cache.role) match {
        case PermissionResult.Permitted     => f(request)(lang)(cache)
        case PermissionResult.NotPermitted  => errResponder.forbidden(request, "Access denied")
        case PermissionResult.FormSubmitted => errResponder.forbidden(request, "Access denied")
      }
    }

  def asyncNoAuth(formTemplateId: FormTemplateId)(
    f: Request[AnyContent] => LangADT => FormTemplate => Future[Result]): Action[AnyContent] = actionBuilder.async {
    implicit request =>
      implicit val l: LangADT = getCurrentLanguage(request)

      val formTemplate = request.attrs(FormTemplateKey)
      for {
        result <- f(request)(l)(formTemplate)
      } yield result
  }

  private def async(formTemplateId: FormTemplateId)(
    f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => Future[Result]): Action[AnyContent] =
    actionBuilder.async { implicit request =>
      implicit val l: LangADT = getCurrentLanguage(request)

      val formTemplate = request.attrs(FormTemplateKey)
      for {
        _ <- MDCHelpers.addFormTemplateIdToMdc(formTemplateId)
        authResult <- authService
                       .authenticateAndAuthorise(
                         formTemplate,
                         getAffinityGroup,
                         ggAuthorised(request),
                         getCaseWorkerIdentity(request))
        result <- handleAuthResults(
                   authResult,
                   formTemplate,
                   request,
                   onSuccess = retrievals => role => f(request)(l)(AuthCacheWithoutForm(retrievals, formTemplate, role))
                 )
      } yield result
    }

  def asyncGGAuth(formTemplateId: FormTemplateId)(
    f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => Future[Result]): Action[AnyContent] =
    actionBuilder.async { implicit request =>
      val predicate = AuthProviders(AuthProvider.GovernmentGateway)
      val formTemplate = request.attrs(FormTemplateKey)
      for {
        _          <- MDCHelpers.addFormTemplateIdToMdc(formTemplateId)
        authResult <- ggAuthorised(request)(RecoverAuthResult.noop)(predicate)
        result <- authResult match {
                   case AuthSuccessful(retrievals, role) =>
                     f(request)(getCurrentLanguage(request))(AuthCacheWithoutForm(retrievals, formTemplate, role))
                   case _ => errResponder.forbidden(request, "Access denied")
                 }
      } yield result
    }

  def authAndRetrieveForm[U <: SectionSelectorType: SectionSelector](
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    operation: OperationWithForm
  )(
    f: Request[AnyContent] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[
      DataOrigin.Mongo] => Future[Result]
  ): Action[AnyContent] =
    async(formTemplateId, maybeAccessCode) { request => lang => cache => smartStringEvaluator => formModelOptics =>
      Permissions.apply(operation, cache.role, cache.form.status) match {
        case PermissionResult.Permitted    => f(request)(lang)(cache)(smartStringEvaluator)(formModelOptics)
        case PermissionResult.NotPermitted => errResponder.forbidden(request, "Access denied")
        case PermissionResult.FormSubmitted =>
          Redirect(
            uk.gov.hmrc.gform.gform.routes.AcknowledgementController
              .showAcknowledgement(maybeAccessCode, formTemplateId)).pure[Future]
      }
    }

  def async[U <: SectionSelectorType: SectionSelector](
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  )(
    f: Request[AnyContent] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[
      DataOrigin.Mongo] => Future[Result]
  ): Action[AnyContent] =
    actionBuilder.async { implicit request =>
      implicit val l: LangADT = getCurrentLanguage(request)

      val formTemplate = request.attrs(FormTemplateKey)
      for {
        _ <- MDCHelpers.addFormTemplateIdToMdc(formTemplateId)
        _ <- MDCHelpers.addAccessCodeToMdc(maybeAccessCode)
        authResult <- authService
                       .authenticateAndAuthorise(
                         formTemplate,
                         getAffinityGroup,
                         ggAuthorised(request),
                         getCaseWorkerIdentity(request))
        result <- handleAuthResults(
                   authResult,
                   formTemplate,
                   request,
                   onSuccess = withForm[U](f(request)(l))(maybeAccessCode, formTemplate)
                 )
      } yield result
    }

  private def withForm[U <: SectionSelectorType: SectionSelector](
    f: AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[DataOrigin.Mongo] => Future[Result]
  )(
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate
  )(
    retrievals: MaterialisedRetrievals
  )(
    role: Role
  )(
    implicit
    hc: HeaderCarrier,
    l: LangADT
  ): Future[Result] = {
    val formIdData = FormIdData(retrievals, formTemplate._id, maybeAccessCode)
    for {
      form <- gformConnector.getForm(formIdData)
      _    <- MDCHelpers.addFormIdToMdc(form._id)
      cache = AuthCacheWithForm(retrievals, form, formTemplate, role, maybeAccessCode)

      formModelOptics <- FormModelOptics
                          .mkFormModelOptics[DataOrigin.Mongo, Future, U](cache.variadicFormData, cache, recalculation)

      smartStringEvaluator = smartStringEvaluatorFactory
        .apply(formModelOptics.formModelVisibilityOptics, retrievals, maybeAccessCode, form, formTemplate)
      envelope <- fileUploadConnector.getEnvelope(cache.form.envelopeId)
      _        <- updateMappingIfInFlight(envelope, formIdData, form) // Delete after 28 days of deployment of this change
      result   <- f(cache)(smartStringEvaluator)(formModelOptics)
    } yield result
  }

  /**
    * This is compatibility layer for forms without `componentIdToFileId` fields. Once this
    * field will be long enough in production, we can remove this.
    */
  private def updateMappingIfInFlight(envelope: Envelope, formIdData: FormIdData, form: Form)(
    implicit
    hc: HeaderCarrier
  ): Future[Unit] = {
    val componentIdToFileId: FormComponentIdToFileIdMapping = form.componentIdToFileId
    if (envelope.files.nonEmpty && componentIdToFileId.mapping.isEmpty) {
      // If some file exists in envelope and mapping is empty,
      // it means we are in-flight when we deployed form.componentIdToFileId feature
      val mapping = envelope.files.map { file =>
        file.fileId.toFieldId -> file.fileId
      }.toMap

      val userData: UserData = UserData(
        form.formData,
        form.status,
        form.visitsIndex,
        form.thirdPartyData,
        FormComponentIdToFileIdMapping(mapping)
      )

      logger.warn("Adding fileupload-component <-> fileId mapping for form " + formIdData.toFormId)
      gformConnector.updateUserData(formIdData, userData)
    } else {
      ().pure[Future]
    }
  }

  private def handleAuthResults(
    result: AuthResult,
    formTemplate: FormTemplate,
    request: Request[_],
    onSuccess: MaterialisedRetrievals => Role => Future[Result]
  )(
    implicit
    l: LangADT
  ): Future[Result] =
    result match {
      case AuthSuccessful(retrievals: AnonymousRetrievals, role) =>
        onSuccess(retrievals)(role)
      case AuthSuccessful(retrievals: VerifyRetrievals, role) =>
        onSuccess(retrievals)(role)
      case AuthSuccessful(retrievals: AuthenticatedRetrievals, role) =>
        onSuccess(retrievals)(role)
      case AuthRedirect(loginUrl, flashing) => Redirect(loginUrl).flashing(flashing: _*).pure[Future]
      case AuthAnonymousSession(redirectUrl) =>
        Redirect(redirectUrl.url, request.queryString)
          .withSession(SessionKeys.sessionId -> s"anonymous-session-${UUID.randomUUID()}")
          .pure[Future]
      case AuthRedirectFlashingFormName(loginUrl) =>
        Redirect(loginUrl).flashing("formTitle" -> formTemplate.formName.value).pure[Future]
      case AuthBlocked(message) =>
        errResponder.forbiddenWithReason(request, message)
      case AuthForbidden(message) =>
        errResponder.forbidden(request, message)
    }

  val defaultRetrievals = Retrievals.credentials and
    Retrievals.allEnrolments and
    Retrievals.affinityGroup and
    Retrievals.groupIdentifier and
    Retrievals.nino

  private def ggAuthorised(
    request: Request[AnyContent]
  )(
    recoverPF: PartialFunction[Throwable, AuthResult]
  )(
    predicate: Predicate
  ): Future[AuthResult] = {
    import uk.gov.hmrc.auth.core.retrieve.~

    implicit val hc: HeaderCarrier =
      HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

    authorised(predicate)
      .retrieve(defaultRetrievals) {
        case maybeCredentials ~ enrolments ~ maybeAffinityGroup ~ maybeGroupIdentifier ~ maybeNino =>
          val maybeRetrievals =
            for {
              govermentGatewayId <- maybeCredentials.flatMap(toGovernmentGatewayId)
              affinityGroup      <- maybeAffinityGroup
              groupIdentifier    <- maybeGroupIdentifier
            } yield {
              AuthenticatedRetrievals(
                govermentGatewayId,
                enrolments,
                affinityGroup,
                groupIdentifier,
                maybeNino.map(Nino(_))
              )
            }

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
                                |GroupIdentifier: $maybeGroupIdentifier""".stripMargin))(retrievals =>
              AuthSuccessful(retrievals, roleFromMaterialisedRetrievals(retrievals)))
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
}

sealed trait AuthCache {
  def retrievals: MaterialisedRetrievals
  def formTemplate: FormTemplate
  def role: Role
  def accessCode: Option[AccessCode]
}

case class AuthCacheWithForm(
  retrievals: MaterialisedRetrievals,
  form: Form,
  formTemplate: FormTemplate,
  role: Role,
  accessCode: Option[AccessCode]
) extends AuthCache {
  def formModel[U <: SectionSelectorType: SectionSelector](
    implicit
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
        form.componentIdToFileId
      )
      .dependencyGraphValidation
  }

  def toCacheData: CacheData = new CacheData(
    form.envelopeId,
    form.thirdPartyData,
    formTemplate
  )
  def variadicFormData[U <: SectionSelectorType: SectionSelector](
    implicit
    hc: HeaderCarrier
  ): VariadicFormData[SourceOrigin.OutOfDate] =
    VariadicFormData.buildFromMongoData(formModel, form.formData.toData)

}

case class AuthCacheWithoutForm(
  retrievals: MaterialisedRetrievals,
  formTemplate: FormTemplate,
  role: Role
) extends AuthCache {
  override val accessCode: Option[AccessCode] = None
  def toCacheData: CacheData = new CacheData(
    EnvelopeId(""),
    ThirdPartyData.empty,
    formTemplate
  )
  def toAuthCacheWithForm(form: Form, accessCode: Option[AccessCode]) =
    AuthCacheWithForm(retrievals, form, formTemplate, role, accessCode)
}

class CacheData(
  val envelopeId: EnvelopeId,
  val thirdPartyData: ThirdPartyData,
  val formTemplate: FormTemplate
)
