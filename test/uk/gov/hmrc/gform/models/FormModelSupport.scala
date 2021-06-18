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

package uk.gov.hmrc.gform.models

import cats.{ Id, Monad }
import cats.syntax.applicative._
import scala.language.higherKinds
import uk.gov.hmrc.gform.GraphSpec
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, MaterialisedRetrievals, Role }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.{ DbLookupChecker, DelegatedEnrolmentChecker, SeissEligibilityChecker }
import uk.gov.hmrc.gform.graph.{ GraphException, Recalculation }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.form.FormComponentIdToFileIdMapping
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, NotChecked, Obligations }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormField, InProgress, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormId, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId, Section }
import uk.gov.hmrc.gform.typeclasses.identityThrowableMonadError
import uk.gov.hmrc.http.{ HeaderCarrier, SessionId }

trait FormModelSupport extends GraphSpec {
  implicit val hc: HeaderCarrier = HeaderCarrier()

  val retrievals: MaterialisedRetrievals = AnonymousRetrievals(SessionId("dummy-sessionId"))
  val thirdPartyData: ThirdPartyData = ThirdPartyData.empty
  val envelopeId: EnvelopeId = EnvelopeId("dummy")

  private def eligibilityStatusTrue[F[_]: Monad]: SeissEligibilityChecker[F] =
    new SeissEligibilityChecker[F]((_, _) => true.pure[F])

  private def delegatedEnrolmentCheckStatus[F[_]: Monad]: DelegatedEnrolmentChecker[F] =
    new DelegatedEnrolmentChecker(delegatedEnrolmentCheckStatusTrue[F])

  private def dbLookupCheckStatus[F[_]: Monad]: DbLookupChecker[F] =
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
    formData = FormData(List.empty[FormField]),
    status = InProgress,
    visitsIndex = VisitIndex(Set.empty[Int]),
    thirdPartyData = thirdPartyData,
    envelopeExpiryDate = None,
    componentIdToFileId = FormComponentIdToFileIdMapping.empty
  )

  def mkAuthCacheWithForm(formTemplate: FormTemplate): AuthCacheWithForm = AuthCacheWithForm(
    retrievals = retrievals,
    form = mkForm(formTemplate._id),
    formTemplate = formTemplate,
    role = Role.Customer,
    accessCode = maybeAccessCode
  )

  def mkFormModelBuilder(formTemplate: FormTemplate): FormModelBuilder[Throwable, Id] =
    new FormModelBuilder(
      retrievals,
      formTemplate,
      thirdPartyData,
      envelopeId,
      maybeAccessCode,
      recalculation,
      FormComponentIdToFileIdMapping.empty
    )

  def mkFormModelOptics(
    formTemplate: FormTemplate,
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ): FormModelOptics[DataOrigin.Browser] = {
    val authCache: AuthCacheWithForm = mkAuthCacheWithForm(formTemplate)
    FormModelOptics
      .mkFormModelOptics[DataOrigin.Browser, Id, SectionSelectorType.Normal](data, authCache, recalculation)
  }

  def mkProcessData(
    formModelOptics: FormModelOptics[DataOrigin.Browser]
  ): ProcessData = {

    val visitsIndex: VisitIndex = VisitIndex(Set.empty[Int])
    val booleanExprCache: BooleanExprCache = BooleanExprCache.empty
    val obligations: Obligations = NotChecked

    ProcessData(formModelOptics, visitsIndex, obligations, booleanExprCache)
  }

}
