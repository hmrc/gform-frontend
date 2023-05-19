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
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, PostcodeLookup }
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.errors
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

class PostcodeLookupValidation[D <: DataOrigin](formModelVisibilityOptics: FormModelVisibilityOptics[D])(implicit
  messages: Messages,
  sse: SmartStringEvaluator
) {

  def validate(formComponent: FormComponent): ValidatedType[Unit] =
    validateRequired(formComponent)

  private def validateRequired(
    formComponent: FormComponent
  ): ValidatedType[Unit] = {

    case class ModelComponentIdValue(modelComponentId: ModelComponentId, value: Option[String])

    val atomsWithValues: List[ModelComponentIdValue] = formComponent.multiValueId
      .atomsModelComponentIdsFilterByAtom(_.atom =!= PostcodeLookup.filter)
      .map(m => ModelComponentIdValue(m, formModelVisibilityOptics.data.one(m)))

    if (formComponent.mandatory) {
      atomsWithValues.foldMap { mcv =>
        mcv.value
          .filter(_.nonEmpty)
          .fold(requiredError(formComponent, mcv.modelComponentId)) { postcode =>
            if (PostcodeLookupValidation.checkPostcode(postcode)) {
              validationSuccess
            } else {
              enterRealPostcode(formComponent, mcv.modelComponentId)
            }
          }
      }
    } else {
      validationSuccess
    }
  }

  private def requiredError(formComponent: FormComponent, modelComponentId: ModelComponentId): ValidatedType[Unit] =
    Map[ModelComponentId, Set[String]](
      modelComponentId -> errors(formComponent, "postcode.error.required", None)
    ).invalid

  private def enterRealPostcode(formComponent: FormComponent, modelComponentId: ModelComponentId): ValidatedType[Unit] =
    Map[ModelComponentId, Set[String]](
      modelComponentId -> errors(formComponent, "postcode.error.real", None)
    ).invalid
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
