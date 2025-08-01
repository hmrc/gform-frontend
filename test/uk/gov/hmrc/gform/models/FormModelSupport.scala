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

import cats.data.NonEmptyList
import cats.syntax.applicative._
import cats.{ Id, Monad }
import play.api.i18n.Messages
import uk.gov.hmrc.gform.GraphSpec
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, MaterialisedRetrievals, Role }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.{ DbLookupChecker, DelegatedEnrolmentChecker, SeissEligibilityChecker }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.graph.{ GraphException, Recalculation }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateContext, FormTemplateId, IncludeIf, OptionData, Section, SectionNumber, SectionOrSummary }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.typeclasses.identityThrowableMonadError
import uk.gov.hmrc.http.{ HeaderCarrier, SessionId }

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

  val recalculation: Recalculation[Id, Throwable] =
    new Recalculation[Id, Throwable](
      eligibilityStatusTrue,
      delegatedEnrolmentCheckStatus,
      dbLookupCheckStatus,
      (s: GraphException) => new IllegalArgumentException(s.reportProblem)
    )

  val maybeAccessCode: Option[AccessCode] = None

  def mkFormModelFromSections(sections: List[Section]): FormModelBuilder[Throwable, Id] = {
    val formTemplate: FormTemplate = mkFormTemplate(sections)
    mkFormModelBuilder(formTemplate)
  }

  def mkForm(formTemplateId: FormTemplateId): Form = Form(
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
    taskIdTaskStatus = TaskIdTaskStatusMapping.empty
  )

  def mkAuthCacheWithForm(formTemplate: FormTemplate): AuthCacheWithForm = AuthCacheWithForm(
    retrievals = retrievals,
    form = mkForm(formTemplate._id),
    formTemplateContext = FormTemplateContext.basicContext(formTemplate, None),
    role = Role.Customer,
    accessCode = maybeAccessCode,
    new LookupRegistry(Map())
  )

  def mkFormModelBuilder(formTemplate: FormTemplate): FormModelBuilder[Throwable, Id] =
    new FormModelBuilder(
      retrievals,
      formTemplate,
      thirdPartyData,
      envelopeId,
      maybeAccessCode,
      recalculation,
      FormComponentIdToFileIdMapping.empty,
      new LookupRegistry(Map()),
      TaskIdTaskStatusMapping.empty
    )

  def mkFormModelOptics(
    formTemplate: FormTemplate,
    data: VariadicFormData[SourceOrigin.OutOfDate],
    currentSection: Option[SectionNumber] = None
  )(implicit messages: Messages, lang: LangADT): FormModelOptics[DataOrigin.Browser] = {
    val authCache: AuthCacheWithForm = mkAuthCacheWithForm(formTemplate)
    FormModelOptics
      .mkFormModelOptics[DataOrigin.Browser, Id, SectionSelectorType.Normal](
        data,
        authCache,
        recalculation,
        currentSection = currentSection.map(SectionOrSummary.Section.apply)
      )
  }

  def mkFormModelOpticsMongo(
    formTemplate: FormTemplate,
    data: VariadicFormData[SourceOrigin.OutOfDate]
  )(implicit messages: Messages, lang: LangADT): FormModelVisibilityOptics[DataOrigin.Mongo] = {
    val formModelOptics: FormModelOptics[DataOrigin.Mongo] =
      FormModelOptics.mkFormModelOptics[DataOrigin.Mongo, Id, SectionSelectorType.WithDeclaration](
        data,
        mkAuthCacheWithForm(formTemplate),
        recalculation
      )

    formModelOptics.formModelVisibilityOptics
  }

  def mkProcessData(
    formModelOptics: FormModelOptics[DataOrigin.Browser]
  ): ProcessData = {

    val visitsIndex: VisitIndex = VisitIndex.Classic(Set.empty[SectionNumber.Classic])
    val booleanExprCache: BooleanExprCache = BooleanExprCache.empty
    val obligations: Obligations = NotChecked

    ProcessData(formModelOptics, visitsIndex, obligations, booleanExprCache)
  }

  def toOptionData(xs: NonEmptyList[String]): NonEmptyList[OptionData.IndexBased] =
    xs.map(l => OptionData.IndexBased(toSmartString(l), None, None, None, None))

  def toOptionData(s: String): OptionData.IndexBased = OptionData.IndexBased(toSmartString(s), None, None, None, None)

  def toOptionData(s: String, includeIf: IncludeIf): OptionData.IndexBased =
    OptionData.IndexBased(toSmartString(s), None, Some(includeIf), None, None)

}
