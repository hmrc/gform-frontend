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

package uk.gov.hmrc.gform.gform

import cats.data.{ EitherT, Kleisli, NonEmptyList, ReaderT }
import play.api.data.Form
import cats.instances.future._
import cats.instances.list._
import cats.mtl.{ Ask, Raise }
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{ Applicative, Monad, Traverse }
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.{ AnyContent, MessagesControllerComponents, Request, Result }
import play.twirl.api.Html
import uk.gov.hmrc.gform.auth._
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers.{ AuthenticatedRequestActions, Direction }
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.eval.InitFormEvaluator
import uk.gov.hmrc.gform.eval.smartstring.{ RealSmartStringEvaluatorFactory, SmartStringEvaluator }
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.gform.handlers.{ FormHandlerResult, FormValidator }
import uk.gov.hmrc.gform.gform.processor.EnrolmentResultProcessor
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.{ RecData, Recalculation }
import uk.gov.hmrc.gform.models.optics.FormModelRenderPageOptics
import uk.gov.hmrc.gform.models.{ DataExpanded, FormModel, SectionSelectorType, Singleton }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.form.FormComponentIdToFileIdMapping
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluatorFactory
import uk.gov.hmrc.gform.sharedmodel.taxenrolments.TaxEnrolmentsResponse
import uk.gov.hmrc.gform.validation.{ ValidationResult, ValidationService }
import uk.gov.hmrc.gform.views.hardcoded.{ EnrolmentAlreadyLinkedPage, InsufficientCredentialsPage }
import uk.gov.hmrc.govukfrontend.views.Aliases.Table
import uk.gov.hmrc.govukfrontend.views.html.components.GovukTable
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.ErrorLink
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.TableRow
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.gform.views.html.hardcoded.pages._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax
import uk.gov.hmrc.auth.core.Assistant

import scala.concurrent.{ ExecutionContext, Future }

sealed trait SubmitEnrolmentError
private case object NoIdentifierProvided extends SubmitEnrolmentError
private case class RegimeIdNotMatch(identifier: IdentifierRecipe) extends SubmitEnrolmentError
private case object EnrolmentFormNotValid extends SubmitEnrolmentError

case class Env(
  formTemplate: FormTemplate,
  retrievals: MaterialisedRetrievals,
  formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo]
)

class EnrolmentController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
  renderer: SectionRenderingService,
  validationService: ValidationService,
  enrolmentService: EnrolmentService,
  appConfig: AppConfig,
  recalculation: Recalculation[Future, Throwable],
  taxEnrolmentConnector: TaxEnrolmentsConnector,
  ggConnector: GovernmentGatewayConnector,
  frontendAppConfig: FrontendAppConfig,
  messagesControllerComponents: MessagesControllerComponents,
  smartStringEvaluatorFactory: SmartStringEvaluatorFactory,
  gformConnector: GformConnector,
  englishMessages: Messages
)(implicit
  ec: ExecutionContext
) extends FrontendController(messagesControllerComponents) {

  type Ctx[A] = ReaderT[Future, Env, A]
  type EnrolM[A] = EitherT[Ctx, SubmitEnrolmentError, A]

  private def liftEM[A](a: Future[A]): EnrolM[A] = EitherT.liftF(Kleisli(Function.const(a)))

  private def toSingleton(enrolmentSection: EnrolmentSection): Singleton[DataExpanded] =
    Singleton(enrolmentSection.toPage).asInstanceOf[Singleton[DataExpanded]]

  private def enrolmentConnect(implicit hc: HeaderCarrier): EnrolmentConnect[EnrolM] =
    new EnrolmentConnect[EnrolM] {
      def enrolGGUser(
        request: TaxEnrolment,
        service: ServiceId,
        retrievals: MaterialisedRetrievals
      ): EnrolM[ServiceCallResponse[TaxEnrolmentsResponse]] =
        liftEM(taxEnrolmentConnector.enrolGGUser(request, service, retrievals))
    }

  private def ggConnect(implicit hc: HeaderCarrier): GGConnect[EnrolM] =
    new GGConnect[EnrolM] {
      def enrolGGUser(request: GGEnrolmentRequest): EnrolM[HttpResponse] =
        liftEM(ggConnector.enrolGGUser(request))
    }

  import i18nSupport._

  def technicalFailurePage(formTemplateId: FormTemplateId) =
    withEnrolmentSection(formTemplateId)(formTemplate =>
      enrolmentSection =>
        enrolmentOutcomes =>
          implicit sse =>
            implicit request =>
              implicit l =>
                Ok(
                  enrolment_technical_failure_page(
                    enrolmentOutcomes.technicalFailurePage,
                    frontendAppConfig,
                    formTemplate
                  )
                )
    )

  def successPage(formTemplateId: FormTemplateId) =
    withEnrolmentSection(formTemplateId)(formTemplate =>
      enrolmentSection =>
        enrolmentOutcomes =>
          implicit sse =>
            implicit request =>
              implicit l =>
                Ok(
                  enrolment_success_page(
                    enrolmentOutcomes.successPage,
                    frontendAppConfig,
                    formTemplate
                  )
                )
    )

  def successPageSubmit(formTemplateId: FormTemplateId) =
    withEnrolmentSection(formTemplateId)(formTemplate =>
      enrolmentSection =>
        enrolmentOutcomes =>
          sse =>
            request => l => Redirect(uk.gov.hmrc.gform.gform.routes.NewFormController.dashboard(formTemplate._id).url)
    )

  def alreadyLinkedPage(formTemplateId: FormTemplateId, params: String) =
    withEnrolmentSection(formTemplateId)(formTemplate =>
      enrolmentSection =>
        enrolmentOutcomes =>
          implicit sse =>
            implicit request =>
              implicit l => {
                val table = new GovukTable()(
                  paramsTable(params, enrolmentSection)
                )
                val enrolmentAlreadyLinkedPage = new EnrolmentAlreadyLinkedPage(formTemplate, changeOrSignoutChoice)
                Ok(
                  enrolment_already_linked_page(
                    enrolmentAlreadyLinkedPage,
                    enrolmentOutcomes.alreadyLinkedPage,
                    table,
                    frontendAppConfig,
                    params
                  )
                )
              }
    )

  def insufficientCredentialsPage(formTemplateId: FormTemplateId) =
    withEnrolmentSection(formTemplateId)(formTemplate =>
      enrolmentSection =>
        enrolmentOutcomes =>
          implicit sse =>
            implicit request =>
              implicit l => {
                val insufficientCredentialsPage = new InsufficientCredentialsPage(formTemplate, changeOrSignoutChoice)
                Ok(
                  insufficient_credentials_page(
                    insufficientCredentialsPage,
                    enrolmentOutcomes.insufficientCredentialsPage,
                    frontendAppConfig
                  )
                )
              }
    )

  def notMatchedPage(formTemplateId: FormTemplateId, params: String) =
    withEnrolmentSection(formTemplateId)(formTemplate =>
      enrolmentSection =>
        enrolmentOutcomes =>
          implicit sse =>
            implicit request =>
              implicit l => {
                val table = new GovukTable()(
                  paramsTable(params, enrolmentSection)
                )
                Ok(
                  enrolment_not_matched_page(
                    enrolmentOutcomes.notMatchedPage,
                    table,
                    frontendAppConfig,
                    formTemplate
                  )
                )
              }
    )

  private def withEnrolmentSection(
    formTemplateId: FormTemplateId
  )(
    f: FormTemplate => EnrolmentSection => EnrolmentOutcomes => SmartStringEvaluator => Request[
      AnyContent
    ] => LangADT => Result
  ) =
    auth.asyncGGAuth(formTemplateId) { implicit request: Request[AnyContent] => implicit l => cache =>
      val initFormEvaluator = InitFormEvaluator(cache, cache.formTemplate.authConfig, Option.empty[ItmpRetrievals])
      val sse = new RealSmartStringEvaluatorFactory(englishMessages).noForm(initFormEvaluator.evalExpr)
      cache.formTemplate.authConfig match {
        case HasEnrolmentSection((_, enrolmentSection, _, _, enrolmentOutcomes)) =>
          Future.successful(
            f(cache.formTemplate)(enrolmentSection)(enrolmentOutcomes)(sse)(request)(l)
          )
        case _ =>
          throw new Exception("Enrolment section not found")
      }
    }

  private val changeOrSignoutChoice: Form[String] = Form(
    play.api.data.Forms.single(
      "enrolment.change.or.sign.out" -> play.api.data.Forms.nonEmptyText
    )
  )

  def alreadyLinkedPageSubmit(formTemplateId: FormTemplateId, params: String) =
    withEnrolmentSection(formTemplateId)(formTemplate =>
      enrolmentSection =>
        enrolmentOutcomes =>
          implicit sse =>
            implicit request =>
              implicit l => {
                val table = new GovukTable()(
                  paramsTable(params, enrolmentSection)
                )

                changeOrSignoutChoice
                  .bindFromRequest()
                  .fold(
                    errorForm => {
                      val enrolmentAlreadyLinkedPage = new EnrolmentAlreadyLinkedPage(formTemplate, errorForm)
                      BadRequest(
                        enrolment_already_linked_page(
                          enrolmentAlreadyLinkedPage,
                          enrolmentOutcomes.alreadyLinkedPage,
                          table,
                          frontendAppConfig,
                          params
                        )
                      )
                    },
                    {
                      case "change-gg-account" =>
                        Redirect(
                          uk.gov.hmrc.gform.gform.routes.NewFormController.dashboardWithNewSession(formTemplate._id)
                        )
                      case "sign-out" =>
                        Redirect(uk.gov.hmrc.gform.gform.routes.SignOutController.signOut(formTemplate._id))
                      case unknown =>
                        throw new Exception(s"Unexpected value of linked account parameter: '$unknown'")
                    }
                  )
              }
    )

  def inSufficientCredentialsPageSubmit(formTemplateId: FormTemplateId) =
    withEnrolmentSection(formTemplateId)(formTemplate =>
      enrolmentSection =>
        enrolmentOutcomes =>
          implicit sse =>
            implicit request =>
              implicit l =>
                changeOrSignoutChoice
                  .bindFromRequest()
                  .fold(
                    errorForm => {
                      val insufficientCredentialsPage = new InsufficientCredentialsPage(formTemplate, errorForm)
                      BadRequest(
                        insufficient_credentials_page(
                          insufficientCredentialsPage,
                          enrolmentOutcomes.insufficientCredentialsPage,
                          frontendAppConfig
                        )
                      )
                    },
                    {
                      case "change-gg-account" =>
                        Redirect(
                          uk.gov.hmrc.gform.gform.routes.NewFormController.dashboardWithNewSession(formTemplate._id)
                        )
                      case "sign-out" =>
                        Redirect(uk.gov.hmrc.gform.gform.routes.SignOutController.signOut(formTemplate._id))
                      case unknown =>
                        throw new Exception(s"Unexpected value of linked account parameter: '$unknown'")
                    }
                  )
    )

  private def paramsTable(params: String, enrolmentSection: EnrolmentSection)(implicit
    sse: SmartStringEvaluator
  ): Table = {
    val idenPairs: List[(String, String)] =
      params.split("~").toList.flatMap { p =>
        p.split("=").toList match {
          case identifierKey :: value :: Nil => Some(identifierKey -> value)
          case _                             => None
        }
      }

    val identifierLookup: Map[String, FormCtx] =
      enrolmentSection.identifiers.map(i => i.key -> i.value).toList.toMap
    val verifierLookup: Map[String, FormCtx] =
      enrolmentSection.verifiers.map(v => v.key -> v.value).toList.toMap

    val lookup: Map[String, FormCtx] = identifierLookup ++ verifierLookup

    val rowData: List[(SmartString, String)] = idenPairs.flatMap { case (key, value) =>
      lookup.get(key).flatMap { formCtx =>
        val fcId = formCtx.formComponentId
        val maybeFormComponent: Option[FormComponent] =
          enrolmentSection.fields.find(formComponent => formComponent.id == fcId)
        maybeFormComponent.map(fc => fc.shortName.getOrElse(fc.label) -> value)
      }
    }

    val rows = rowData.map { case (label, value) =>
      List(
        TableRow(
          content = uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text(label.value())
        ),
        TableRow(
          content = uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text(value)
        )
      )
    }

    Table(
      firstCellIsHeader = true,
      rows = rows
    )
  }

  def showEnrolment(formTemplateId: FormTemplateId) =
    auth.asyncGGAuth(formTemplateId) { implicit request: Request[AnyContent] => implicit l => cache =>
      if (cache.retrievals.getCredentialRole.get == Assistant) {
        Redirect(uk.gov.hmrc.gform.gform.routes.EnrolmentController.insufficientCredentialsPage(formTemplateId))
          .flashing("formTitle" -> cache.formTemplate.formName.value)
          .pure[Future]
      } else {
        cache.formTemplate.authConfig match {
          case HasEnrolmentSection((_, enrolmentSection, _, _, _)) =>
            Future.successful(
              Ok(
                renderEnrolmentSection(
                  cache.formTemplate,
                  cache.retrievals,
                  enrolmentSection,
                  FormModelOptics.fromEnrolmentSection(enrolmentSection, cache),
                  Nil,
                  ValidationResult.empty
                )
              )
            )
          case _ =>
            Redirect(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments(formTemplateId))
              .flashing("formTitle" -> cache.formTemplate.formName.value)
              .pure[Future]
        }
      }
    }

  private def renderEnrolmentSection(
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    enrolmentSection: EnrolmentSection,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    globalErrors: List[ErrorLink],
    validationResult: ValidationResult
  )(implicit request: Request[_], l: LangADT): Html = {
    implicit val sse = smartStringEvaluatorFactory(
      formModelOptics.formModelVisibilityOptics
    )

    val singleton = toSingleton(enrolmentSection)

    renderer
      .renderEnrolmentSection(
        formTemplate,
        singleton,
        retrievals,
        formModelOptics,
        globalErrors,
        validationResult
      )
  }

  def submitEnrolment(formTemplateId: FormTemplateId, action: Direction) =
    auth.asyncGGAuth(formTemplateId) { implicit request => implicit l => cache =>
      import cache._
      val checkEnrolment: ServiceId => List[Verifier] => NonEmptyList[Identifier] => EnrolM[CheckEnrolmentsResult] =
        serviceId =>
          verifiers =>
            identifiers => EitherT.liftF(Kleisli(_ => auth.checkEnrolment(serviceId, identifiers, verifiers)))

      formTemplate.authConfig match {
        case HasEnrolmentSection((serviceId, enrolmentSection, postCheck, lfcev, enrolmentOutcomes)) =>
          val genesisFormModel: FormModel[DataExpanded] = FormModel.fromEnrolmentSection(enrolmentSection)

          val formModelRenderPageOptics: FormModelRenderPageOptics[DataOrigin.Mongo] =
            FormModelRenderPageOptics(genesisFormModel, RecData.empty)

          processResponseDataFromBody(request, formModelRenderPageOptics) {
            requestRelatedData => variadicFormData => _ =>
              val formModelOpticsF = FormModelOptics
                .mkFormModelOptics[DataOrigin.Mongo, Future, SectionSelectorType.EnrolmentOnly](
                  variadicFormData,
                  cache,
                  cache.toCacheData,
                  recalculation,
                  None,
                  FormComponentIdToFileIdMapping.empty
                )
              def handleContinueWithData(formModelOptics: FormModelOptics[DataOrigin.Mongo]) = {
                val formModelVisibilityOptics = formModelOptics.formModelVisibilityOptics

                implicit val sse =
                  smartStringEvaluatorFactory(
                    formModelVisibilityOptics
                  )

                implicit val EC = enrolmentConnect
                implicit val GGC = ggConnect

                val formValidator: FormValidator = new FormValidator()
                val formHandlerResultF: Future[FormHandlerResult] =
                  formValidator.validatePageModelBySectionNumber[DataOrigin.Mongo](
                    formModelOptics,
                    formTemplate.sectionNumberZero,
                    cache.toCacheData,
                    EnvelopeWithMapping.empty,
                    validationService.validatePageModel
                  )

                val enrolmentResultProcessor = new EnrolmentResultProcessor(
                  renderEnrolmentSection,
                  formTemplate,
                  retrievals,
                  enrolmentSection,
                  formModelOptics,
                  frontendAppConfig
                )
                for {
                  formHandlerResult <- formHandlerResultF
                  res <- processValidation(
                           serviceId,
                           enrolmentSection,
                           postCheck,
                           checkEnrolment(serviceId),
                           formHandlerResult,
                           lfcev,
                           retrievals
                         )
                           .fold(
                             enrolmentResultProcessor
                               .recoverEnrolmentError(formHandlerResult.validationResult),
                             enrolmentResultProcessor.processEnrolmentResult(enrolmentOutcomes)
                           )
                           .run(Env(formTemplate, retrievals, formModelVisibilityOptics))
                } yield res
              }
              action match {
                case uk.gov.hmrc.gform.controllers.Continue =>
                  formModelOpticsF.flatMap(handleContinueWithData)
                case _ => Future.successful(BadRequest("Cannot determine action"))
              }

          }
        case _ =>
          Future.successful(
            Redirect(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments(formTemplateId))
              .flashing("formTitle" -> formTemplate.formName.value)
          )
      }
    }

  private def validateIdentifiers[F[_]: Applicative](
    identifiers: NonEmptyList[(IdentifierRecipe, Identifier)],
    postCheck: EnrolmentPostCheck
  )(implicit FR: Raise[F, SubmitEnrolmentError]): F[Unit] =
    postCheck match {
      case NoCheck => ().pure[F]
      case RegimeIdCheck(regimeId) =>
        val (identifierRecipe, identifier) = identifiers.head
        if (identifier.value.drop(2).startsWith(regimeId.value))
          ().pure[F]
        else
          FR.raise(RegimeIdNotMatch(identifierRecipe))
    }

  private def processValidation[F[_]: Monad: EnrolmentConnect: GGConnect](
    serviceId: ServiceId,
    enrolmentSection: EnrolmentSection,
    postCheck: EnrolmentPostCheck,
    checkEnrolment: List[Verifier] => NonEmptyList[Identifier] => F[CheckEnrolmentsResult],
    formHandlerResult: FormHandlerResult,
    enrolmentAction: EnrolmentAction,
    retrievals: MaterialisedRetrievals
  )(implicit messages: Messages, AA: Ask[F, Env], FR: Raise[F, SubmitEnrolmentError]): F[CheckEnrolmentsResult] = {

    def tryEnrolment(verifiers: List[Verifier], identifiers: NonEmptyList[Identifier]): F[CheckEnrolmentsResult] =
      for {
        enrolmentResponse <- enrolmentService.enrolUser[F](serviceId, identifiers, verifiers, retrievals)
        result <- enrolmentResponse match {
                    case ServiceResponse(TaxEnrolmentsResponse.Success) => checkEnrolment(verifiers)(identifiers)
                    case ServiceResponse(TaxEnrolmentsResponse.Conflict) =>
                      CheckEnrolmentsResult.Conflict(identifiers, verifiers).pure[F]
                    case ServiceResponse(TaxEnrolmentsResponse.InvalidIdentifiers) =>
                      CheckEnrolmentsResult.InvalidIdentifiers.pure[F]
                    case ServiceResponse(TaxEnrolmentsResponse.InvalidCredentials) =>
                      CheckEnrolmentsResult.InvalidCredentials.pure[F]
                    case _ =>
                      CheckEnrolmentsResult.Failed.pure[F]
                  }
      } yield result

    if (formHandlerResult.validationResult.isFormValid) {
      for {
        idenVer <- extractIdentifiersAndVerifiers[F](enrolmentSection)
        (identifierss, verifiers) = idenVer
        identifiers = identifierss.map(_._2)
        _             <- validateIdentifiers[F](identifierss, postCheck)
        initialResult <- tryEnrolment(verifiers, identifiers)
        reattemptResult <- (initialResult, enrolmentAction) match {
                             case (CheckEnrolmentsResult.InvalidIdentifiers, LegacyFcEnrolmentVerifier(value)) =>
                               tryEnrolment(List(Verifier(value, "FC")), identifiers)
                             case _ =>
                               initialResult.pure[F]
                           }
      } yield reattemptResult
    } else {
      FR.raise(EnrolmentFormNotValid)
    }
  }

  private def purgeEmpty[F[_]: Applicative](
    xs: NonEmptyList[(IdentifierRecipe, Identifier)]
  )(implicit
    FR: Raise[F, SubmitEnrolmentError]
  ): F[NonEmptyList[(IdentifierRecipe, Identifier)]] =
    xs.toList.filterNot(_._2.value.isEmpty) match {
      case Nil    => FR.raise(NoIdentifierProvided)
      case h :: t => NonEmptyList(h, t).pure[F]
    }

  private def extractIdentifiersAndVerifiers[F[_]: Monad](
    enrolmentSection: EnrolmentSection
  )(implicit
    messages: Messages,
    AA: Ask[F, Env],
    FR: Raise[F, SubmitEnrolmentError]
  ): F[(NonEmptyList[(IdentifierRecipe, Identifier)], List[Verifier])] = {
    def evaluate[A, B, G[_]: Traverse](xs: G[A])(g: A => FormCtx, f: A => String => B): F[G[B]] =
      for {
        env <- AA.ask
        res <- xs.traverse { x =>
                 val value = env.formModelVisibilityOptics
                   .evalAndApplyTypeInfoFirst(g(x))
                   .stringRepresentation
                 f(x)(value).pure[F]
               }
      } yield res

    val allIdentifiers: F[NonEmptyList[(IdentifierRecipe, Identifier)]] =
      evaluate(enrolmentSection.identifiers)(
        _.value,
        identifier => value => (identifier, Identifier(identifier.key, value))
      )

    val allVerifiers: F[List[Verifier]] =
      evaluate(enrolmentSection.verifiers)(
        _.value,
        verifier =>
          value =>
            if (value.nonEmpty)
              List(Verifier(verifier.key, value))
            else Nil
      ).map(_.flatten)

    for {
      identifiers       <- allIdentifiers
      verifiers         <- allVerifiers
      purgedIdentifiers <- purgeEmpty[F](identifiers)
    } yield (purgedIdentifiers, verifiers)
  }
}
