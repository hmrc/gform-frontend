/*
 * Copyright 2019 HM Revenue & Customs
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

import cats.{ Applicative, Monad, Traverse }
import cats.data.{ EitherT, Kleisli, NonEmptyList, ReaderT }
import cats.data.Validated.{ Invalid, Valid }
import cats.instances.future._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import cats.mtl.{ ApplicativeAsk, FunctorRaise }
import cats.mtl.implicits._
import java.net.URLEncoder

import play.api.i18n.I18nSupport
import play.api.mvc.{ AnyContent, Request, Result }
import play.twirl.api.Html
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.auth._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.{ get, processResponseDataFromBody }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.graph.{ Convertible, Evaluator, Recalculation }
import uk.gov.hmrc.gform.obligation.HmrcTaxPeriodIdentifier
import uk.gov.hmrc.gform.sharedmodel.TaxPeriods
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormDataRecalculated }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService }
import uk.gov.hmrc.gform.validation.ValidationUtil.{ GformError, ValidatedType }
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.ExecutionContext.Implicits.global

sealed trait SubmitEnrolmentError
private case object NoIdentifierProvided extends SubmitEnrolmentError
private case class RegimeIdNotMatch(identifier: IdentifierRecipe) extends SubmitEnrolmentError
private case class EnrolmentFormNotValid(errors: GformError) extends SubmitEnrolmentError

case class Env(formTemplate: FormTemplate, retrievals: MaterialisedRetrievals, data: FormDataRecalculated)

class EnrolmentController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
  renderer: SectionRenderingService,
  validationService: ValidationService,
  enrolmentService: EnrolmentService,
  appConfig: AppConfig,
  recalculation: Recalculation[Future, Throwable],
  taxEnrolmentConnector: TaxEnrolmentsConnector,
  ggConnector: GovernmentGatewayConnector
) extends FrontendController {

  type Ctx[A] = ReaderT[Future, Env, A]
  type EnrolM[A] = EitherT[Ctx, SubmitEnrolmentError, A]

  private def liftEM[A](a: Future[A]): EnrolM[A] = EitherT.liftF(Kleisli(Function.const(a)))

  private val evaluator: Evaluator[EnrolM] = {
    val eeittPrepop
      : (Eeitt, MaterialisedRetrievals, FormTemplate, HeaderCarrier) => EitherT[Ctx, SubmitEnrolmentError, String] =
      (e, mr, ft, hc) => liftEM(recalculation.booleanExprEval.evaluator.eeittPrepop(e, mr, ft, hc))
    new Evaluator(eeittPrepop)
  }

  private def enrolmentConnect(implicit hc: HeaderCarrier, ec: ExecutionContext): EnrolmentConnect[EnrolM] =
    new EnrolmentConnect[EnrolM] {
      def enrolGGUser(request: TaxEnrolment, service: ServiceId): EnrolM[HttpResponse] =
        liftEM(taxEnrolmentConnector.enrolGGUser(request, service))
    }

  private def ggConnect(implicit hc: HeaderCarrier, ec: ExecutionContext): GGConnect[EnrolM] =
    new GGConnect[EnrolM] {
      def enrolGGUser(request: GGEnrolmentRequest): EnrolM[HttpResponse] =
        liftEM(ggConnector.enrolGGUser(request))
    }

  import i18nSupport._

  def showEnrolment(formTemplateId: FormTemplateId, lang: Option[String]) = auth.asyncGGAuth(formTemplateId) {
    implicit request => cache =>
      cache.formTemplate.authConfig match {
        case HasEnrolmentSection((_, enrolmentSection, _)) =>
          Ok(
            renderer
              .renderEnrolmentSection(
                cache.formTemplate,
                cache.retrievals,
                enrolmentSection,
                FormDataRecalculated.empty,
                Nil,
                Nil,
                Valid(()),
                lang,
                cache.obligations)
          ).pure[Future]
        case _ =>
          Redirect(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments())
            .flashing("formTitle" -> cache.formTemplate.formName)
            .pure[Future]
      }
  }

  private def recoverEnrolmentError(
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    enrolmentSection: EnrolmentSection,
    data: FormDataRecalculated,
    lang: Option[String],
    obligations: Map[HmrcTaxPeriodIdentifier, TaxPeriods])(
    implicit request: Request[AnyContent]): SubmitEnrolmentError => Result = enrolmentError => {

    def convertEnrolmentError(see: SubmitEnrolmentError): (ValidatedType, List[Html]) = see match {
      case RegimeIdNotMatch(identifierRecipe) =>
        val regimeIdError = Map(identifierRecipe.value.toFieldId -> Set("RegimeId do not match"))
        (Invalid(regimeIdError), List.empty)
      case NoIdentifierProvided =>
        val globalError = html.form.errors.error_global("At least on identifier must be provided")
        (Valid(()), globalError :: Nil)
      case EnrolmentFormNotValid(invalid) => (Invalid(invalid), List.empty)
    }

    val (validationResult, globalErrors) = convertEnrolmentError(enrolmentError)
    val errorMap = getErrorMap(validationResult, data, enrolmentSection)

    Ok(
      renderer.renderEnrolmentSection(
        formTemplate,
        retrievals,
        enrolmentSection,
        data,
        errorMap,
        globalErrors,
        validationResult,
        lang,
        obligations
      )
    )
  }

  private def processEnrolmentResult(formTemplate: FormTemplate, lang: Option[String])(
    authRes: CheckEnrolmentsResult): Result =
    authRes match {
      case EnrolmentSuccessful => Redirect(routes.FormController.dashboard(formTemplate._id, lang).url)
      case EnrolmentFailed =>
        val newPageUrl = routes.FormController.dashboard(formTemplate._id, lang).url
        val continueUrl = URLEncoder.encode(appConfig.`gform-frontend-base-url` + newPageUrl, "UTF-8")
        val ggLoginUrl = appConfig.`government-gateway-sign-in-url`
        val redirectUrl = s"$ggLoginUrl?continue=$continueUrl"
        Redirect(redirectUrl)
    }

  def submitEnrolment(formTemplateId: FormTemplateId, lang: Option[String]) = auth.asyncGGAuth(formTemplateId) {
    implicit request => cache =>
      import cache._
      val checkEnrolment: ServiceId => NonEmptyList[Identifier] => EnrolM[CheckEnrolmentsResult] =
        serviceId => identifiers => EitherT.liftF(Kleisli(_ => auth.checkEnrolment(serviceId, identifiers)))

      processResponseDataFromBody(request) { (dataRaw: Map[FormComponentId, Seq[String]]) =>
        formTemplate.authConfig match {
          case HasEnrolmentSection((serviceId, enrolmentSection, postCheck)) =>
            def handleContinue = {

              implicit val EC = enrolmentConnect
              implicit val GGC = ggConnect
              implicit val evtor = evaluator

              val allFields = getAllEnrolmentFields(enrolmentSection.fields)
              for {
                data <- recalculation.recalculateFormData(dataRaw, formTemplate, retrievals)
                validationResult <- validationService
                                     .validateComponents(allFields, data, EnvelopeId(""), retrievals, formTemplate)
                res <- processValidation(
                        serviceId,
                        enrolmentSection,
                        postCheck,
                        checkEnrolment(serviceId),
                        validationResult)
                        .fold(
                          recoverEnrolmentError(
                            formTemplate,
                            retrievals,
                            enrolmentSection,
                            data,
                            lang,
                            cache.obligations),
                          processEnrolmentResult(formTemplate, lang))
                        .run(Env(formTemplate, retrievals, data))
                        .recoverWith(handleEnrolmentException(formTemplate, lang))
              } yield res

            }
            get(dataRaw, FormComponentId("save")) match {
              case "Continue" :: Nil => handleContinue
              case _                 => Future.successful(BadRequest("Cannot determine action"))
            }
          case _ =>
            Future.successful(
              Redirect(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments())
                .flashing("formTitle" -> formTemplate.formName)
            )
        }
      }
  }

  private def validateIdentifiers[F[_]: Applicative](
    identifiers: NonEmptyList[(IdentifierRecipe, Identifier)],
    postCheck: EnrolmentPostCheck
  )(implicit FR: FunctorRaise[F, SubmitEnrolmentError]): F[Unit] =
    postCheck match {
      case NoCheck => ().pure[F]
      case RegimeIdCheck(regimeId) =>
        val (identifierRecipe, identifier) = identifiers.head
        if (identifier.value.drop(2).startsWith(regimeId.value))
          ().pure[F]
        else
          FR.raise(RegimeIdNotMatch(identifierRecipe))
    }

  private def processValidation[F[_]: Monad: EnrolmentConnect: GGConnect: Evaluator](
    serviceId: ServiceId,
    enrolmentSection: EnrolmentSection,
    postCheck: EnrolmentPostCheck,
    checkEnrolment: NonEmptyList[Identifier] => F[CheckEnrolmentsResult],
    validationResult: ValidatedType
  )(
    implicit hc: HeaderCarrier,
    request: Request[AnyContent],
    AA: ApplicativeAsk[F, Env],
    FR: FunctorRaise[F, SubmitEnrolmentError]): F[CheckEnrolmentsResult] =
    validationResult match {
      case Invalid(errors) => FR.raise(EnrolmentFormNotValid(errors))
      case Valid(()) =>
        for {
          idenVer <- extractIdentifiersAndVerifiers[F](enrolmentSection)
          (identifierss, verifiers) = idenVer
          identifiers = identifierss.map(_._2)
          _       <- validateIdentifiers[F](identifierss, postCheck)
          _       <- enrolmentService.enrolUser(serviceId, identifiers, verifiers)
          authRes <- checkEnrolment(identifiers)
        } yield authRes
    }

  private def getErrorMap(
    validationResult: ValidatedType,
    data: FormDataRecalculated,
    enrolmentSection: EnrolmentSection
  ): List[(FormComponent, FormFieldValidationResult)] = {
    val enrolmentFields = getAllEnrolmentFields(enrolmentSection.fields)
    validationService.evaluateValidation(validationResult, enrolmentFields, data, Envelope(Nil))
  }

  private def getAllEnrolmentFields(fields: List[FormComponent]): List[FormComponent] =
    fields.flatMap { fieldValue =>
      fieldValue.`type` match {
        case grp: Group => getAllEnrolmentFields(grp.fields)
        case _          => List(fieldValue)
      }
    }

  private def purgeEmpty[F[_]: Applicative](
    xs: NonEmptyList[(IdentifierRecipe, Identifier)]
  )(
    implicit FR: FunctorRaise[F, SubmitEnrolmentError]
  ): F[NonEmptyList[(IdentifierRecipe, Identifier)]] =
    xs.toList.filterNot(_._2.value.isEmpty) match {
      case Nil    => FR.raise(NoIdentifierProvided)
      case h :: t => NonEmptyList(h, t).pure[F]
    }

  private def extractIdentifiersAndVerifiers[F[_]: Monad](
    enrolmentSection: EnrolmentSection
  )(
    implicit hc: HeaderCarrier,
    evaluator: Evaluator[F],
    AA: ApplicativeAsk[F, Env],
    FR: FunctorRaise[F, SubmitEnrolmentError]): F[(NonEmptyList[(IdentifierRecipe, Identifier)], List[Verifier])] = {
    def evaluate[A, B, G[_]: Traverse](xs: G[A])(g: A => Expr, f: A => String => B): F[G[B]] =
      for {
        env <- AA.ask
        res <- xs.traverse { x =>
                val fcId = FormComponentId("dummy")
                val convertible: Convertible[F] =
                  evaluator.eval(Set.empty, fcId, g(x), env.data.data, env.retrievals, env.formTemplate)
                Convertible
                  .asString(convertible, env.formTemplate)
                  .map(value => f(x)(value.getOrElse("")))
              }
      } yield res

    val allIdentifiers: F[NonEmptyList[(IdentifierRecipe, Identifier)]] =
      evaluate(enrolmentSection.identifiers)(
        _.value,
        identifier => value => (identifier, Identifier(identifier.key, value)))

    val allVerifiers: F[List[Verifier]] =
      evaluate(enrolmentSection.verifiers)(_.value, verifier => value => Verifier(verifier.key, value))

    for {
      identifiers       <- allIdentifiers
      verifiers         <- allVerifiers
      purgedIdentifiers <- purgeEmpty[F](identifiers)
    } yield (purgedIdentifiers, verifiers)
  }

  private def handleEnrolmentException(
    formTemplate: FormTemplate,
    lang: Option[String]
  )(implicit hc: HeaderCarrier, request: Request[_]): PartialFunction[Throwable, Future[Result]] = {
    case _ =>
      Future.successful(
        Redirect(routes.FormController.dashboard(formTemplate._id, lang)).withNewSession
      )
  }
}
