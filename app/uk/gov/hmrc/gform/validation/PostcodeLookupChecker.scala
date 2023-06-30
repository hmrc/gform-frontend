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

package uk.gov.hmrc.gform.validation

import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent
import uk.gov.hmrc.gform.sharedmodel.formtemplate.PostcodeLookup
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.errors

import ComponentChecker._

class PostcodeLookupChecker[D <: DataOrigin]() extends ComponentChecker[Unit, D] {

  override protected def checkProgram(context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    // val formComponent = context.formComponent
    val checker = new PostcodeLookupCheckerHelper(context.formModelVisibilityOptics)
    checker.validate(context.formComponent)
  }
}

class PostcodeLookupCheckerHelper[D <: DataOrigin](formModelVisibilityOptics: FormModelVisibilityOptics[D])(implicit
  messages: Messages,
  sse: SmartStringEvaluator
) {

  implicit val stringValueForReport = new ValueForReport[String] {
    def valueForReport(): String = ""
  }
  def validate(formComponent: FormComponent): CheckProgram[Unit] = {
    case class ModelComponentIdValue(modelComponentId: ModelComponentId, value: Option[String])
    val atomsWithValues: List[ModelComponentIdValue] = formComponent.multiValueId
      .atomsModelComponentIdsFilterByAtom(_.atom =!= PostcodeLookup.filter)
      .map(m => ModelComponentIdValue(m, formModelVisibilityOptics.data.one(m)))
    ifProgram(
      andCond = formComponent.mandatory,
      thenProgram = {
        val programs = atomsWithValues.map { mcv =>
          mcv.value
            .filter(_.nonEmpty)
            .toProgram(
              errorProgram = errorProgram[String](
                Map[ModelComponentId, Set[String]](
                  mcv.modelComponentId -> errors(formComponent, "postcode.error.required", None)
                )
              )
            )
            .andThen { postcode =>
              ifProgram(
                cond = PostcodeLookupValidation.checkPostcode(postcode),
                thenProgram = successProgram(()),
                elseProgram = errorProgram(
                  Map[ModelComponentId, Set[String]](
                    mcv.modelComponentId -> errors(formComponent, "postcode.error.real", None)
                  )
                )
              )
            }

        }
        programs.nonShortCircuitProgram
      },
      elseProgram = successProgram(())
    )
  }
}

object PostcodeLookupValidation {

  // Taken from https://github.com/hmrc/address-lookup/blob/d2409b5b3956b5d010135da86b75e401c4611f89/app/model/address/Postcode.scala#L36-L76
  // The basic syntax of a postcode (ignores the rules on valid letter ranges because they don't matter here).
  private val oPattern = "^[A-Z]{1,2}[0-9][0-9A-Z]?$".r
  private val iPattern = "^[0-9][A-Z]{2}$".r

  def checkPostcode(p: String): Boolean = checkPostcode0(p).isDefined

  private def checkPostcode0(p: String): Option[(String, String)] = {
    val norm = PostcodeLookupValidation.normalisePostcode0(p)
    if (norm.length < 5) None
    else {
      val incodeLength = norm.length - 3
      val out = norm.substring(0, incodeLength)
      val in = norm.substring(incodeLength, norm.length)
      checkSyntax(out, in)
    }
  }

  private def checkSyntax(out: String, in: String): Option[(String, String)] =
    (out, in) match {
      case (oPattern(), iPattern()) => Some(out -> in)
      case _                        => None
    }

  /** Removes all whitespace from a postcode string. */
  private def normalisePostcode0(postcode: String): String =
    postcode.trim.replaceAll("[ \\t]+", "").toUpperCase

  def normalisePostcode(postcode: String): String = {
    val containsNoWhiteSpace = normalisePostcode0(postcode).length === postcode.length

    checkPostcode0(postcode) match {
      case Some((out, in)) => if (containsNoWhiteSpace) out + in else out + " " + in
      case None            => postcode

    }
  }
}
