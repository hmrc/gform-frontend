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

package uk.gov.hmrc.gform.models

import cats.Monad
import cats.syntax.applicative._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.GraphSpec
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, MaterialisedRetrievals, Role }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.{ DbLookupChecker, DelegatedEnrolmentChecker, SeissEligibilityChecker }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.recalculation.Metadata
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateContext, FormTemplateId, IncludeIf, OptionData, Section, SectionNumber }
import uk.gov.hmrc.http.{ HeaderCarrier, SessionId }

import java.time.Instant

trait FormModelSupport extends GraphSpec {
  implicit val hc: HeaderCarrier = HeaderCarrier()

  val retrievals: MaterialisedRetrievals = AnonymousRetrievals(SessionId("dummy-sessionId"))
  val thirdPartyData: ThirdPartyData = ThirdPartyData.empty
  val envelopeId: EnvelopeId = EnvelopeId("dummy")

  protected def eligibilityStatusTrue[F[_]: Monad]: SeissEligibilityChecker[F] =
    new SeissEligibilityChecker[F]((_, _) => true.pure[F])

  protected def delegatedEnrolmentCheckStatus[F[_]: Monad]: DelegatedEnrolmentChecker[F] =
    new DelegatedEnrolmentChecker(delegatedEnrolmentCheckStatusTrue[F])

  protected def dbLookupCheckStatus[F[_]: Monad]: DbLookupChecker[F] =
    new DbLookupChecker(dbLookupStatusTrue[F])

  val maybeAccessCode: Option[AccessCode] = None

  def mkFormModelFromSections(sections: List[Section]): FormModelBuilder = {
    val formTemplate: FormTemplate = mkFormTemplate(sections)
    mkFormModelBuilder(formTemplate)
  }

  def mkForm(formTemplateId: FormTemplateId): Form = mkForm(formTemplateId, thirdPartyData)

  def mkForm(formTemplateId: FormTemplateId, thirdPartyData: ThirdPartyData): Form = Form(
    _id = FormId("form-id"),
    envelopeId = envelopeId,
    userId = UserId("user-id"),
    formTemplateId = formTemplateId,
    formTemplateVersion = None,
    formData = FormData(List.empty[FormField]),
    status = InProgress,
    visitsIndex = VisitIndex.Classic(Set.empty[SectionNumber.Classic]),
    thirdPartyData = thirdPartyData,
    envelopeExpiryDate = None,
    componentIdToFileId = FormComponentIdToFileIdMapping.empty,
    taskIdTaskStatus = TaskIdTaskStatusMapping.empty,
    startDate = Instant.now
  )

  def mkAuthCacheWithForm(formTemplate: FormTemplate): AuthCacheWithForm =
    mkAuthCacheWithForm(formTemplate, thirdPartyData)

  def mkAuthCacheWithForm(formTemplate: FormTemplate, thirdPartyData: ThirdPartyData): AuthCacheWithForm =
    AuthCacheWithForm(
      retrievals = retrievals,
      form = mkForm(formTemplate._id, thirdPartyData),
      formTemplateContext = FormTemplateContext.basicContext(formTemplate, None),
      role = Role.Customer,
      accessCode = maybeAccessCode,
      new LookupRegistry(Map())
    )

  def mkFormModelBuilder(formTemplate: FormTemplate): FormModelBuilder =
    new FormModelBuilder(
      retrievals,
      formTemplate,
      Metadata.from(formTemplate),
      thirdPartyData,
      envelopeId,
      maybeAccessCode,
      FormComponentIdToFileIdMapping.empty,
      new LookupRegistry(Map()),
      TaskIdTaskStatusMapping.empty
    )

  def mkFormModelOptics(
    formTemplate: FormTemplate,
    data: VariadicFormData
  )(implicit messages: Messages, lang: LangADT): FormModelOptics =
    mkFormModelOptics(formTemplate, data, ThirdPartyData.empty)

  def mkFormModelOptics(
    formTemplate: FormTemplate,
    data: VariadicFormData,
    thirdPartyData: ThirdPartyData
  )(implicit messages: Messages, lang: LangADT): FormModelOptics = {
    val authCache: AuthCacheWithForm = mkAuthCacheWithForm(formTemplate, thirdPartyData)

    FormModelOptics
      .mkFormModelOptics[SectionSelectorType.Normal](data, authCache)
  }

  def mkFormModelOpticsMongo(
    formTemplate: FormTemplate,
    data: VariadicFormData
  )(implicit messages: Messages, lang: LangADT): FormModelVisibilityOptics =
    FormModelOptics
      .mkFormModelOptics[SectionSelectorType.WithDeclaration](
        data,
        mkAuthCacheWithForm(formTemplate)
      )
      .formModelVisibilityOptics

  def mkProcessData(
    formTemplate: FormTemplate,
    formModelOptics: FormModelOptics
  ): ProcessData = {

    val visitsIndex: VisitIndex = VisitIndex.Classic(Set.empty[SectionNumber.Classic])
    val cache = mkAuthCacheWithForm(formTemplate)
    val obligations: Obligations = NotChecked

    ProcessData(formModelOptics, visitsIndex, obligations, cache, None)
  }

  def toOptionData(xs: List[String]): List[OptionData.IndexBased] =
    xs.map(l => OptionData.IndexBased(toSmartString(l), None, None, None, None))

  def toOptionData(s: String): OptionData.IndexBased = OptionData.IndexBased(toSmartString(s), None, None, None, None)

  def toOptionData(s: String, includeIf: IncludeIf): OptionData.IndexBased =
    OptionData.IndexBased(toSmartString(s), None, Some(includeIf), None, None)

}
