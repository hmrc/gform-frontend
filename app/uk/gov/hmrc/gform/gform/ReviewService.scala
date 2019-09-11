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

import cats.MonadError
import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import play.api.mvc.{ AnyContent, Request }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.gformbackend.GformBackEndAlgebra
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.sharedmodel.form.FormIdData
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, BundledFormSubmissionData, LangADT }
import uk.gov.hmrc.gform.sharedmodel.form.{ Accepting, Form, FormId, FormStatus, Returning }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.summary.SubmissionDetails
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class ReviewService[F[_]](gformBackEnd: GformBackEndAlgebra[F], lookupRegistry: LookupRegistry)(
  implicit me: MonadError[F, Throwable]) {
  def forceUpdateFormStatus(
    cache: AuthCacheWithForm,
    status: FormStatus,
    reviewData: Map[String, String],
    maybeAccessCode: Option[AccessCode])(implicit hc: HeaderCarrier) =
    gformBackEnd.updateUserData(updateWithReviewData(cache, reviewData).form, maybeAccessCode) >>
      gformBackEnd.forceUpdateFormStatus(cache.form._id, status)

  def acceptForm(cache: AuthCacheWithForm, maybeAccessCode: Option[AccessCode], reviewData: Map[String, String])(
    implicit request: Request[AnyContent],
    headerCarrier: HeaderCarrier,
    l: LangADT): F[HttpResponse] =
    submitReviewResults(updateWithReviewData(cache, reviewData), maybeAccessCode, Accepting)

  def returnForm(cache: AuthCacheWithForm, maybeAccessCode: Option[AccessCode], reviewData: Map[String, String])(
    implicit request: Request[AnyContent],
    headerCarrier: HeaderCarrier,
    l: LangADT): F[HttpResponse] =
    submitReviewResults(
      updateWithReviewData(cache, reviewData),
      maybeAccessCode,
      Returning
    )

  def submitFormBundle(cache: AuthCacheWithForm, reviewData: Map[String, String], maybeAccessCode: Option[AccessCode])(
    implicit request: Request[AnyContent],
    headerCarrier: HeaderCarrier,
    l: LangADT): F[Unit] =
    for {
      bundle           <- gformBackEnd.getFormBundle(cache.form._id)
      formDataToSubmit <- buildFormDataToSubmit(bundle)
      _                <- gformBackEnd.updateUserData(updateWithReviewData(cache, reviewData).form, maybeAccessCode)
      result           <- gformBackEnd.submitFormBundle(cache.form._id, formDataToSubmit)
    } yield result

  private def updateWithReviewData(cache: AuthCacheWithForm, reviewData: Map[String, String]) =
    cache.copy(form = cache.form.copy(thirdPartyData = cache.form.thirdPartyData.copy(reviewData = Some(reviewData))))

  private def submitReviewResults(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    formStatus: FormStatus)(
    implicit request: Request[AnyContent],
    headerCarrier: HeaderCarrier,
    l: LangADT): F[HttpResponse] =
    for {
      submission <- gformBackEnd.submissionDetails(FormIdData.fromForm(cache.form, maybeAccessCode))
      result <- gformBackEnd.submitWithUpdatedFormStatus(
                 formStatus,
                 cache,
                 maybeAccessCode,
                 Some(SubmissionDetails(submission, "")))
    } yield result._1

  private def buildFormDataToSubmit(
    formIds: NonEmptyList[FormId])(implicit hc: HeaderCarrier, l: LangADT): F[NonEmptyList[BundledFormSubmissionData]] =
    for {
      forms         <- getForms(formIds)
      formTemplates <- getFormTemplates(forms)
      bundle        <- buildBundledFormSubmissionData(forms, formTemplates)
    } yield bundle

  private def buildBundledFormSubmissionData(
    forms: NonEmptyList[Form],
    formTemplates: Map[FormTemplateId, FormTemplate])(implicit l: LangADT): F[NonEmptyList[BundledFormSubmissionData]] =
    forms.traverse { form =>
      StructuredFormDataBuilder(form, formTemplates(form.formTemplateId), lookupRegistry)
        .map { sfd =>
          BundledFormSubmissionData(form._id, sfd)
        }
    }

  private def getForms(formIds: NonEmptyList[FormId])(implicit hc: HeaderCarrier): F[NonEmptyList[Form]] =
    formIds.traverse(gformBackEnd.getForm)

  private def getFormTemplates(forms: NonEmptyList[Form])(
    implicit hc: HeaderCarrier): F[Map[FormTemplateId, FormTemplate]] =
    forms
      .map(_.formTemplateId)
      .toList
      .distinct
      .traverse(gformBackEnd.getFormTemplate)
      .map(_.map(template => (template._id, template)).toMap)

}
