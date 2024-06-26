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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait FormComponentGen {
  def formComponentIdGen: Gen[FormComponentId] =
    for {
      first                <- Gen.oneOf(Gen.const("_"), Gen.alphaChar.map(_.toString))
      restBeforeUnderscore <- PrimitiveGen.nonEmptyAlphaNumStrGen
      restAfterUnderscore  <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen).map(_.map("_" + _).getOrElse(""))
    } yield FormComponentId(first + restBeforeUnderscore + restAfterUnderscore)

  def idBeginningWithNumberGen: Gen[FormComponentId] =
    for {
      precedingNumber     <- Gen.numChar.map(_.toString)
      followingCharacters <- Gen.alphaChar.map(_.toString)
    } yield FormComponentId(precedingNumber + followingCharacters)

  def labelGen: Gen[SmartString] = SmartStringGen.smartStringGen
  def helpTextGen: Gen[SmartString] = SmartStringGen.smartStringGen
  def shortNameGen: Gen[SmartString] =
    SmartStringGen.smartStringGen
  def errorMessageGen: Gen[SmartString] = SmartStringGen.smartStringGen

  def formComponentGen(maxDepth: Int = 3): Gen[FormComponent] =
    for {
      id                <- formComponentIdGen
      tpe               <- ComponentTypeGen.componentTypeGen(maxDepth)
      label             <- labelGen
      isPageHeading     <- PrimitiveGen.booleanGen
      helpText          <- Gen.option(helpTextGen)
      shortName         <- Gen.option(shortNameGen)
      validIf           <- Gen.option(ValidIfGen.validIfGen)
      includeIf         <- Gen.option(IncludeIfGen.includeIfGen)
      mandatory         <- PrimitiveGen.booleanGen
      editable          <- PrimitiveGen.booleanGen
      submissable       <- PrimitiveGen.booleanGen
      derived           <- PrimitiveGen.booleanGen
      onlyShowOnSummary <- PrimitiveGen.booleanGen
      errorMessage      <- Gen.option(errorMessageGen)
      presentationHint  <- Gen.option(PrimitiveGen.zeroOrMoreGen(PresentationHintGen.presentationHintGen))
      validators        <- PrimitiveGen.zeroOrMoreGen(FormComponentValidatorGen.formComponentValidatorGen)
    } yield FormComponent(
      id,
      tpe,
      label,
      isPageHeading,
      helpText,
      shortName,
      includeIf,
      validIf,
      mandatory,
      editable,
      submissable,
      derived,
      onlyShowOnSummary,
      errorMessage,
      presentationHint,
      validators
    )
}

object FormComponentGen extends FormComponentGen
