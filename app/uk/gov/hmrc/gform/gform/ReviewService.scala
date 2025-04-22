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

import cats.{ Monad, MonadError }
import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import play.api.i18n.Messages
import play.api.mvc.{ AnyContent, Request }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.objectStore.Attachments
import uk.gov.hmrc.gform.gformbackend.GformBackEndAlgebra
import uk.gov.hmrc.gform.graph.CustomerIdRecalculation
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.{ SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormIdData, FormModelOptics }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, BundledFormSubmissionData, LangADT, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form.{ Accepting, Form, FormStatus, Returning }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.summary.SubmissionDetails
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class ReviewService[F[_]: Monad](
  gformBackEnd: GformBackEndAlgebra[F],
  lookupRegistry: LookupRegistry
)(implicit
  me: MonadError[F, Throwable]
) {
  def forceUpdateFormStatus(
    cache: AuthCacheWithForm,
    status: FormStatus,
    reviewData: Map[String, String],
    maybeAccessCode: Option[AccessCode]
  )(implicit
    hc: HeaderCarrier
  ): F[Unit] =
    gformBackEnd.updateUserData(updateWithReviewData(cache, reviewData).form, maybeAccessCode) >>
      gformBackEnd.forceUpdateFormStatus(FormIdData.fromForm(cache.form, maybeAccessCode), status)

  def acceptForm[U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    reviewData: Map[String, String],
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[AnyContent],
    messages: Messages,
    headerCarrier: HeaderCarrier,
    l: LangADT,
    sse: SmartStringEvaluator
  ): F[HttpResponse] =
    submitReviewResults(updateWithReviewData(cache, reviewData), maybeAccessCode, Accepting, formModelOptics)

  def returnForm[U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    reviewData: Map[String, String],
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[AnyContent],
    messages: Messages,
    headerCarrier: HeaderCarrier,
    l: LangADT,
    sse: SmartStringEvaluator
  ): F[HttpResponse] =
    submitReviewResults(
      updateWithReviewData(cache, reviewData),
      maybeAccessCode,
      Returning,
      formModelOptics
    )

  def submitFormBundle(
    cache: AuthCacheWithForm,
    reviewData: Map[String, String],
    maybeAccessCode: Option[AccessCode]
  )(implicit
    headerCarrier: HeaderCarrier,
    l: LangADT,
    m: Messages
  ): F[Unit] =
    for {
      bundle           <- gformBackEnd.getFormBundle(FormIdData.fromForm(cache.form, maybeAccessCode))
      formDataToSubmit <- buildFormDataToSubmit(bundle)
      _                <- gformBackEnd.updateUserData(updateWithReviewData(cache, reviewData).form, maybeAccessCode)
      result           <- gformBackEnd.submitFormBundle(FormIdData.fromForm(cache.form, maybeAccessCode), formDataToSubmit)
    } yield result

  private def updateWithReviewData(cache: AuthCacheWithForm, reviewData: Map[String, String]) =
    cache.copy(form = cache.form.copy(thirdPartyData = cache.form.thirdPartyData.copy(reviewData = Some(reviewData))))

  private def submitReviewResults[U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    formStatus: FormStatus,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[AnyContent],
    messages: Messages,
    headerCarrier: HeaderCarrier,
    l: LangADT,
    sse: SmartStringEvaluator
  ): F[HttpResponse] =
    for {
      maybeSubmission <-
        gformBackEnd.submissionDetails(FormIdData.fromForm(cache.form, maybeAccessCode), cache.form.envelopeId)
      customerId = CustomerIdRecalculation.evaluateCustomerId(cache, formModelOptics.formModelVisibilityOptics)
      result <- maybeSubmission.fold(
                  throw new RuntimeException(s"Submission not found for ${cache.form.envelopeId.value}")
                )(submission =>
                  gformBackEnd.submitWithUpdatedFormStatus(
                    formStatus,
                    cache,
                    maybeAccessCode,
                    Some(SubmissionDetails(submission, "")),
                    customerId,
                    Attachments.empty,
                    formModelOptics
                  )
                )
    } yield result._1

  private def buildFormDataToSubmit(
    formIds: NonEmptyList[FormIdData]
  )(implicit
    hc: HeaderCarrier,
    l: LangADT,
    m: Messages
  ): F[NonEmptyList[BundledFormSubmissionData]] =
    for {
      forms         <- getForms(formIds)
      formTemplates <- getFormTemplates(forms)
      bundle        <- buildBundledFormSubmissionData(forms, formTemplates)
    } yield bundle

  private def buildBundledFormSubmissionData[D <: DataOrigin](
    forms: NonEmptyList[Form],
    formTemplates: Map[FormTemplateId, FormTemplate]
  )(implicit
    l: LangADT,
    m: Messages
  ): F[NonEmptyList[BundledFormSubmissionData]] =
    forms.traverse { form =>
      val formModelVisibilityOptics: FormModelVisibilityOptics[D] = null
      StructuredFormDataBuilder[D, F](
        formModelVisibilityOptics,
        formTemplates(form.formTemplateId).destinations,
        formTemplates(form.formTemplateId).expressionsOutput,
        lookupRegistry
      )
        .map { sfd =>
          BundledFormSubmissionData(
            FormIdData.fromForm(form, Some(AccessCode.fromSubmissionRef(SubmissionRef(form.envelopeId)))),
            sfd,
            DestinationEvaluator(formTemplates(form.formTemplateId), formModelVisibilityOptics)
          )
        }
    }

  private def getForms(formIds: NonEmptyList[FormIdData])(implicit hc: HeaderCarrier): F[NonEmptyList[Form]] =
    formIds.traverse(gformBackEnd.getForm)

  private def getFormTemplates(
    forms: NonEmptyList[Form]
  )(implicit hc: HeaderCarrier): F[Map[FormTemplateId, FormTemplate]] =
    forms
      .map(_.formTemplateId)
      .toList
      .distinct
      .traverse(gformBackEnd.getFormTemplate)
      .map(
        _.map(templateWithRedirects =>
          (templateWithRedirects.formTemplate._id, templateWithRedirects.formTemplate)
        ).toMap
      )
}
