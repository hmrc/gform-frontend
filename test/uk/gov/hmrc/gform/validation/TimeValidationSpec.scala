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

package uk.gov.hmrc.gform.validation

/* import java.time.LocalTime
 *
 * import cats.implicits._
 * import org.scalatest.mockito.MockitoSugar.mock
 * import play.api.i18n.{ Lang, Messages, MessagesApi, MessagesImpl }
 * import uk.gov.hmrc.gform.Helpers.toSmartString
 * import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
 * import uk.gov.hmrc.gform.fileupload.Envelope
 * import uk.gov.hmrc.gform.lookup.LookupRegistry
 * import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, ThirdPartyData }
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate._
 * import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT, VariadicFormData }
 * import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType */
import uk.gov.hmrc.gform.{ GraphSpec, Spec }
/* import uk.gov.hmrc.http.HeaderCarrier
 *
 * import scala.concurrent.Future */

class TimeValidationSpec extends Spec with GraphSpec {

  /* implicit val langADT: LangADT = LangADT.En
   * val lang = Lang(langADT.langADTToString)
   * val messagesApi = org.scalatest.mockito.MockitoSugar.mock[MessagesApi]
   * implicit val messages: Messages = MessagesImpl(lang, messagesApi)
   *
   * val retrievals = mock[MaterialisedRetrievals]
   *
   * private val lookupRegistry = new LookupRegistry(Map.empty)
   *
   * implicit lazy val hc = HeaderCarrier()
   *
   * private def mkComponentsValidator(data: FormDataRecalculated): ComponentsValidator =
   *   new ComponentsValidator(
   *     data,
   *     EnvelopeId("whatever"),
   *     Envelope.empty,
   *     retrievals,
   *     booleanExprEval,
   *     ThirdPartyData.empty,
   *     ExampleData.formTemplate,
   *     lookupRegistry,
   *     None)
   *
   * private def mkFormComponent(time: Time) =
   *   FormComponent(
   *     FormComponentId("timeOfCall"),
   *     time,
   *     toSmartString("timeLabel"),
   *     None,
   *     None,
   *     None,
   *     true,
   *     false,
   *     false,
   *     true,
   *     false,
   *     None)
   *
   * "Time" should "accepts value within range" in {
   *
   *   val time = Time(
   *     List(
   *       Range(StartTime(LocalTime.parse("10:00")), EndTime(LocalTime.parse("16:00")))
   *     ),
   *     IntervalMins(15)
   *   )
   *
   *   val iTime = mkFormComponent(time)
   *   val lstTime = List(iTime)
   *
   *   val data = mkFormDataRecalculated(
   *     VariadicFormData.ones(
   *       FormComponentId("timeOfCall") -> "10:15 AM"
   *     ))
   *
   *   val result: ValidatedType[Unit] =
   *     mkComponentsValidator(data).validate(iTime, lstTime, GetEmailCodeFieldMatcher.noop).futureValue
   *
   *   result.toEither should beRight(())
   * }
   *
   * it should "accepts value within ranges" in {
   *
   *   val time = Time(
   *     List(
   *       Range(StartTime(LocalTime.parse("10:00")), EndTime(LocalTime.parse("13:00"))),
   *       Range(StartTime(LocalTime.parse("16:00")), EndTime(LocalTime.parse("20:00")))
   *     ),
   *     IntervalMins(15)
   *   )
   *
   *   val iTime = mkFormComponent(time)
   *   val lstTime = List(iTime)
   *
   *   val data = mkFormDataRecalculated(
   *     VariadicFormData.ones(
   *       FormComponentId("timeOfCall") -> "07:15 PM"
   *     ))
   *
   *   val result: ValidatedType[Unit] =
   *     mkComponentsValidator(data).validate(iTime, lstTime, GetEmailCodeFieldMatcher.noop).futureValue
   *
   *   result.toEither should beRight(())
   * }
   *
   * it should "accepts an empty value when mandatory is false" in {
   *
   *   val time = Time(
   *     List(
   *       Range(StartTime(LocalTime.parse("10:00")), EndTime(LocalTime.parse("13:00"))),
   *       Range(StartTime(LocalTime.parse("16:00")), EndTime(LocalTime.parse("20:00")))
   *     ),
   *     IntervalMins(15)
   *   )
   *
   *   val iTime = mkFormComponent(time).copy(mandatory = false)
   *   val lstTime = List(iTime)
   *
   *   val data = mkFormDataRecalculated(
   *     VariadicFormData.ones(
   *       FormComponentId("timeOfCall") -> ""
   *     ))
   *
   *   val result: ValidatedType[Unit] =
   *     mkComponentsValidator(data).validate(iTime, lstTime, GetEmailCodeFieldMatcher.noop).futureValue
   *
   *   result.toEither should beRight(())
   * } */
}
