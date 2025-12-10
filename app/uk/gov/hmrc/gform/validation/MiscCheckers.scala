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
import org.typelevel.ci._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.models.email.EmailFieldId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.LocalisedString
import uk.gov.hmrc.gform.sharedmodel.form.ThirdPartyData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.CheckerServiceHelper.validationFailure

import scala.collection.mutable.LinkedHashSet

import ComponentChecker._

class TimeChecker[D <: DataOrigin]() extends ComponentChecker[Unit, D] {

  private val genericErrorInvalid = "generic.error.invalid"

  override protected def checkProgram(context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val formComponent = context.formComponent
    formComponent match {
      case IsTime(t) =>
        validateTime(
          context.formComponent,
          t,
          context.formModelVisibilityOptics
        )
      case _ => throw new IllegalArgumentException("FormComponent is not a Time")
    }
  }

  private def validateTime(
    formComponent: FormComponent,
    time: Time,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val timeValue: Option[String] =
      formModelVisibilityOptics.data.one(formComponent.modelComponentId).filterNot(_.isEmpty)
    val timeErrorRequired = "time.error.required"

    val isMandatory = formComponent.mandatory.eval(formModelVisibilityOptics.booleanExprResolver)
    ifProgram(
      cond = timeValue.isDefined && !(Range.timeSlots(time) contains timeValue.get),
      thenProgram = validationFailure(
        formComponent,
        messages(
          genericErrorInvalid,
          formComponent.errorPlaceholder.getOrElse(formComponent.label).value()
        ),
        None
      ),
      elseProgram = ifProgram(
        cond = timeValue.isEmpty,
        andCond = isMandatory,
        thenProgram = validationFailure(
          formComponent,
          messages(
            timeErrorRequired,
            formComponent.errorPlaceholder.getOrElse(formComponent.label).value()
          ),
          None
        ),
        elseProgram = successProgram(())
      )
    )
  }
}

class ChoiceChecker[D <: DataOrigin]() extends ComponentChecker[Unit, D] {

  override protected def checkProgram(context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val formComponent = context.formComponent
    formComponent match {
      case IsChoice(c) =>
        val hasNonChoice = c.noneChoice.isDefined
        val hasNonChoiceError = c.noneChoiceError.isDefined
        ifProgram(
          andCond = hasNonChoice && hasNonChoiceError,
          thenProgram = List(
            validateChoiceNoneError(
              context.formComponent,
              c.noneChoice.getOrElse(NoneChoice.ValueBased("")),
              c.noneChoiceError
                .filter(_.m.values.filterNot(_.trim.isEmpty).nonEmpty)
                .getOrElse(
                  LocalisedString(
                    Map(
                      LangADT.En -> "To choose the final option, you need to deselect all other options",
                      LangADT.Cy -> "Er mwyn dewis yr opsiwn olaf, mae angen i chi ddad-ddewis pob opsiwn arall"
                    )
                  )
                )
            )(context.formModelVisibilityOptics),
            validateChoice(context.formComponent)(context.formModelVisibilityOptics)
          ).nonShortCircuitProgram,
          elseProgram = validateChoice(context.formComponent)(context.formModelVisibilityOptics)
        )
      case _ => validateChoice(context.formComponent)(context.formModelVisibilityOptics)
    }
  }

  def validateChoice(
    fieldValue: FormComponent
  )(
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {

    val choiceErrorRequired = "choice.error.required"
    val availableSelections: Set[String] = fieldValue.`type` match {
      case Choice(_, options, _, _, _, _, _, _, _, _, _, _) =>
        options.zipWithIndex.collect {
          case (OptionData.IndexBased(_, _, _, _, _), i)                                        => i.toString
          case (OptionData.ValueBased(_, _, _, _, OptionDataValue.StringBased(value), _, _), _) => value
          case (OptionData.ValueBased(_, _, _, _, OptionDataValue.ExprBased(expr), _, _), _) =>
            val prefix = expr match {
              case FormCtx(fcId) => fcId.value + "_"
              case _             => ""
            }
            prefix + formModelVisibilityOptics.evalAndApplyTypeInfoFirst(expr).stringRepresentation
        }.toSet
      case _ => Set.empty[String]
    }

    val choiceValues: Seq[String] = formModelVisibilityOptics.evaluationResults.recData.variadicFormData
      .get(fieldValue.modelComponentId)
      .toSeq
      .flatMap(_.toSeq)
      .filterNot(_.isEmpty)

    ifProgram(
      cond = fieldValue.mandatory.eval(
        formModelVisibilityOptics.booleanExprResolver
      ) && (choiceValues.isEmpty || (availableSelections.nonEmpty && !choiceValues.forall(
        availableSelections
      ))),
      thenProgram = validationFailure(fieldValue, choiceErrorRequired, None),
      elseProgram = successProgram(())
    )
  }

  def validateChoiceNoneError(
    fieldValue: FormComponent,
    noneChoice: NoneChoice,
    error: LocalisedString
  )(
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit l: LangADT): CheckProgram[Unit] = {
    val choiceValue = formModelVisibilityOptics.data
      .many(fieldValue.modelComponentId)
      .toSeq
      .flatten
      .filterNot(_.isEmpty)

    ifProgram(
      cond = choiceValue.contains(noneChoice.selection) && choiceValue.length > 1,
      thenProgram = errorProgram[Unit](Map(fieldValue.modelComponentId -> LinkedHashSet(error.value))),
      elseProgram = successProgram(())
    )
  }
}

class EmailFieldIdChecker[D <: DataOrigin]() extends ComponentChecker[Unit, D] {

  override protected def checkProgram(context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val formComponent = context.formComponent
    val emailCodeFieldMatcher: EmailCodeFieldMatcher = context.getEmailCodeFieldMatcher(formComponent)
    formComponent.`type` match {
      case emailCodeFieldMatcher.EmailCodeField(emailField) =>
        validateEmailCode(formComponent, emailField, context.formModelVisibilityOptics, context.cache.thirdPartyData)
      case _ => throw new IllegalArgumentException("FormComponent is not a EmailFieldId")
    }
  }

  def validateEmailCode(
    formComponent: FormComponent,
    emailFieldId: EmailFieldId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    thirdPartyData: ThirdPartyData
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val genericErrorEmail = "generic.error.email"
    val expectedCode = thirdPartyData.emailVerification.get(emailFieldId).map(_.code)
    val maybeCode: Option[CIString] =
      formModelVisibilityOptics.data.one(formComponent.modelComponentId).map(c => ci"$c")

    val emailError = CheckerServiceHelper.validationFailure(formComponent, genericErrorEmail, None)

    if (maybeCode === expectedCode.map(_.code)) CheckerServiceHelper.validationSuccess else emailError

  }
}
