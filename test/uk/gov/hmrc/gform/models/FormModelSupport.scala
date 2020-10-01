/*
 * Copyright 2020 HM Revenue & Customs
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

import cats.{ Id, Monad, MonadError, StackSafeMonad }
import cats.syntax.applicative._
import uk.gov.hmrc.gform.GraphSpec
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, MaterialisedRetrievals, Role }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.{ DbLookupChecker, DelegatedEnrolmentChecker, SeissEligibilityChecker }
import uk.gov.hmrc.gform.graph.{ GraphException, Recalculation }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormField, InProgress, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormId, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId, Section }
import uk.gov.hmrc.gform.typeclasses.identityThrowableMonadError
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

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
    formData = FormData(Seq.empty[FormField]),
    status = InProgress,
    visitsIndex = VisitIndex(Set.empty[Int]),
    thirdPartyData = thirdPartyData,
    envelopeExpiryDate = None
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
      recalculation
    )
}
/* import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, MaterialisedRetrievals }
 * import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
 * import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormDataRecalculated, ThirdPartyData }
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Basic, FormTemplate, FullyExpanded, GroupExpanded, Section }
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate.Section.NonRepeatingPage
 * import uk.gov.hmrc.http.HeaderCarrier
 * import uk.gov.hmrc.http.logging.SessionId
 *
 * trait FormModelSupport {
 *
 *   private implicit val hc: HeaderCarrier = HeaderCarrier()
 *
 *   private val retrievals: MaterialisedRetrievals = AnonymousRetrievals(SessionId("dummy-sessionId"))
 *   private val thirdPartyData: ThirdPartyData = ThirdPartyData.empty
 *   private val envelopeId: EnvelopeId = EnvelopeId("dummy")
 *
 *   private def mkFormModelBuilder(formTemplate: FormTemplate): FormModelBuilder = new FormModelBuilder(
 *     retrievals,
 *     formTemplate,
 *     thirdPartyData,
 *     envelopeId
 *   )
 *
 *   def getSingleton(section: Section.NonRepeatingPage): Singleton[FullyExpanded] =
 *     getSingleton(section, FormDataRecalculated.empty)
 *
 *   def getSingleton(section: Section.NonRepeatingPage, data: FormDataRecalculated): Singleton[FullyExpanded] = {
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(List(section), data)
 *     val singletons: Option[Singleton[FullyExpanded]] = formModel.pages.collectFirst {
 *       case s: Singleton[_] => s
 *     }
 *     singletons.getOrElse(throw new Exception("Wrong test data setup"))
 *   }
 *
 *   def mkFormModelExpandGroups(sections: List[Section]): FormModel[GroupExpanded] =
 *     mkFormModelExpandGroups(sections, FormDataRecalculated.empty)
 *
 *   def mkFormModelExpandGroups(sections: List[Section], data: FormDataRecalculated): FormModel[GroupExpanded] = {
 *     val formTemplate: FormTemplate = mkFormTemplate(sections)
 *     val formModelBuilder = mkFormModelBuilder(formTemplate)
 *     val basicModel = formModelBuilder.basic()
 *     formModelBuilder.expandGroups(basicModel, data)
 *   }
 *
 *   def mkFormModelBasic(sections: List[Section]): FormModel[Basic] = {
 *     val formTemplate: FormTemplate = mkFormTemplate(sections)
 *     mkFormModelBuilder(formTemplate).basic()
 *   }
 *
 *   def mkFormModel(sections: List[Section]): FormModel[FullyExpanded] =
 *     mkFormModel(sections, FormDataRecalculated.empty)
 *
 *   def mkFormModel(
 *     sections: List[Section],
 *     data: FormDataRecalculated): FormModel[FullyExpanded] = {
 *     val formTemplate: FormTemplate = mkFormTemplate(sections)
 *     mkFormModel(formTemplate, data)
 *   }
 *
 *   def mkFormModel(formTemplate: FormTemplate): FormModel[FullyExpanded] =
 *     mkFormModel(formTemplate, FormDataRecalculated.empty)
 *
 *   def mkFormModel(
 *     formTemplate: FormTemplate,
 *     data: FormDataRecalculated): FormModel[FullyExpanded] =
 *     mkFormModelBuilder(formTemplate).expand(data)
 *
 * } */
