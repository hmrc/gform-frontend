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

import cats.scalatest.EitherMatchers
import org.scalatest._
import org.scalatest.concurrent.{ Eventually, ScalaFutures }
import org.scalatest.time.{ Millis, Span }
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT, SmartString }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator

import scala.concurrent.ExecutionContext

trait Spec
    extends FlatSpecLike with Matchers with EitherMatchers with DiagrammedAssertions with TryValues with EitherValues
    with OptionValues with AppendedClues with ScalaFutures with StreamlinedXml with JsResultMatcher with Inside
    with Eventually with ExampleData with ScalaCheckDrivenPropertyChecks with FormComponentMatcher {

  override implicit val patienceConfig =
    PatienceConfig(timeout = scaled(Span(1000, Millis)), interval = scaled(Span(15, Millis)))

  implicit lazy val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  implicit val smartStringEvaluator: SmartStringEvaluator = new SmartStringEvaluator {
    override def apply(s: SmartString, markDown: Boolean): String = s.rawValue(LangADT.En)
  }
}
