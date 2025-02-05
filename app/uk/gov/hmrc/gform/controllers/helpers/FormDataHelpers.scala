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

package uk.gov.hmrc.gform.controllers.helpers

import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.show._
import com.softwaremill.quicklens._
import org.slf4j.LoggerFactory
import play.api.mvc.{ AnyContent, Request, Result }
import uk.gov.hmrc.gform.controllers.RequestRelatedData
import uk.gov.hmrc.gform.controllers.helpers.InvisibleCharsHelper._
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelRenderPageOptics }
import uk.gov.hmrc.gform.models.{ DataExpanded, EnteredVariadicFormData, ExpandUtils, FormModel, PageModel }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormField, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Address, Date, FormComponentId, Group, IsChoice, IsRevealingChoice, SectionNumber, TimeFormat }
import uk.gov.hmrc.gform.ops.FormComponentOps
import uk.gov.hmrc.gform.validation.{ PostcodeLookupValidation, TextChecker, TimeFormatter }

import scala.concurrent.Future

object FormDataHelpers {

  private val logger = LoggerFactory.getLogger(getClass)

  def processResponseDataFromBody(
    request: Request[AnyContent],
    formModelRenderPageOptics: FormModelRenderPageOptics[DataOrigin.Mongo],
    maybeSectionNumber: Option[SectionNumber] = None
  )(
    continuation: RequestRelatedData => VariadicFormData[SourceOrigin.OutOfDate] => EnteredVariadicFormData => Future[
      Result
    ]
  ): Future[Result] =
    request.body.asFormUrlEncoded
      .map(_.map { case (field, values) =>
        (
          field,
          values.map { value =>
            val matches = invisibleCharMatches(value)
            if (matches.isEmpty) {
              trimAndReplaceCRLFWithLF(value)
            } else {
              logger.info(
                s"Found invisible characters in field $field. " +
                  s"Matches are [${matches
                    .map { case (m, count) =>
                      s"${getUnicode(m)}:${getDesc(m)}($count)"
                    }
                    .mkString(", ")}]"
              )
              replaceInvisibleChars(value).trim
            }
          }
        )
      }) match {
      case Some(requestData) =>
        val (variadicFormData, requestRelatedData, enteredVariadicFormData) =
          buildVariadicFormDataFromBrowserPostData(formModelRenderPageOptics.formModel, requestData)
        val maybeSectionModelComponentIds = maybeSectionNumber.toSeq.flatMap(s =>
          unselectedChoiceElements(formModelRenderPageOptics.formModel(s), requestData)
        )
        continuation(requestRelatedData)(
          formModelRenderPageOptics.recData.variadicFormData ++
            variadicFormData --
            maybeSectionModelComponentIds
        )(
          EnteredVariadicFormData(
            formModelRenderPageOptics.recData.variadicFormData ++
              enteredVariadicFormData --
              maybeSectionModelComponentIds
          )
        )
      case None =>
        val variadicFormData = formModelRenderPageOptics.recData.variadicFormData ++
          VariadicFormData.empty[SourceOrigin.OutOfDate] --
          maybeSectionNumber.toSeq.flatMap(s =>
            unselectedChoiceElements(formModelRenderPageOptics.formModel(s), Map.empty)
          )
        continuation(RequestRelatedData.empty)(variadicFormData)(EnteredVariadicFormData(variadicFormData))
    }

  private def trimAndReplaceCRLFWithLF(value: String) = value.trim.replaceAll("\r\n", "\n")

  def get(data: Map[FormComponentId, Seq[String]], id: FormComponentId): List[String] =
    data.get(id).toList.flatten

  def anyFormId(data: Map[FormComponentId, Seq[String]]): Option[FormId] =
    data.get(FormComponentId("formId")).flatMap(_.filterNot(_.isEmpty()).headOption).map(FormId.apply)

  def dataEnteredInGroup[S <: SourceOrigin](group: Group, fieldData: VariadicFormData[S]): Boolean =
    group.fields
      .flatMap(_.multiValueId.toModelComponentIds)
      .exists(id => fieldData.get(id).exists(_.exists(_.nonEmpty)))

  def updateFormField(form: Form, updatedFormField: FormField): Form = {
    val updated: List[FormField] = form.formData.fields.filterNot(_.id === updatedFormField.id).+:(updatedFormField)
    form.modify(_.formData.fields).setTo(updated)
  }

  private def buildVariadicFormDataFromBrowserPostData(
    formModel: FormModel[DataExpanded],
    requestData: Map[String, Seq[String]]
  ): (VariadicFormData[SourceOrigin.OutOfDate], RequestRelatedData, VariadicFormData[SourceOrigin.OutOfDate]) = {

    val upperCaseIds: Set[ModelComponentId] = formModel.allUpperCaseIds
    val variadicFormComponentIds: Set[ModelComponentId] = formModel.allModelComponentIds
    val multiValueIds: Set[ModelComponentId] = formModel.allMultiSelectionIds

    val xs: List[
      (Option[(ModelComponentId, VariadicValue)], Option[RequestRelatedData], Option[(ModelComponentId, VariadicValue)])
    ] = requestData.toList.map { case (id, s) =>
      val modelComponentId = ExpandUtils.toModelComponentId(id)

      (variadicFormComponentIds(modelComponentId), multiValueIds(modelComponentId)) match {
        case (true, true) =>
          val value = Some(
            modelComponentId -> VariadicValue.Many(
              s.toList.mkString(",").split(",").map(_.trim).filterNot(_.isEmpty).toIndexedSeq
            )
          )
          (
            value,
            None,
            value
          )
        case (true, false) =>
          s.toList match {
            case first :: _ =>
              val isTimeFormat = formModel.staticTypeInfo
                .get(modelComponentId.baseComponentId)
                .flatMap(_.textConstraint)
                .contains(TimeFormat)
              val firstUpdated =
                if (upperCaseIds(modelComponentId)) {
                  first.toUpperCase()
                } else if (modelComponentId.isAtomic("month")) {
                  normalizeMonth(first)
                } else if (isTimeFormat) {
                  val localTime = TimeFormatter.maybeLocalTime(first)
                  localTime
                    .filterNot(t => TimeFormatter.isNoonConfusing(t, first))
                    .filterNot(t => TimeFormatter.isNoonRangeConfusing(t, first))
                    .map(TimeFormatter.normalizeLocalTime)
                    .getOrElse(first)
                } else first
              (
                Some(
                  modelComponentId -> VariadicValue.One(
                    cleanVariadicValues(firstUpdated, modelComponentId.toFormComponentId, formModel)
                  )
                ),
                None,
                Some(
                  modelComponentId -> VariadicValue.One(
                    firstUpdated
                  )
                )
              )
            case _ =>
              throw new IllegalArgumentException(
                show"""Got a single value form component ID "$id", with an empty list of values"""
              )
          }
        case (false, _) => (None, Some(RequestRelatedData(Map(id -> s))), None)
      }
    }

    xs.foldLeft(
      (
        VariadicFormData.empty[SourceOrigin.OutOfDate],
        RequestRelatedData.empty,
        VariadicFormData.empty[SourceOrigin.OutOfDate]
      )
    ) { case ((variadicFormDataAcc, requestRelatedDataAcc, entVariadicFormDataAcc), (maybeVar, maybeReq, maybeEnt)) =>
      (
        maybeVar.fold(variadicFormDataAcc)(variadicFormDataAcc.addValue),
        maybeReq.fold(requestRelatedDataAcc)(requestRelatedDataAcc + _),
        maybeEnt.fold(entVariadicFormDataAcc)(entVariadicFormDataAcc.addValue)
      )
    }
  }
  /*
   * Choice and RevealingChoice fields are not sent from the browser in the POST body, if value is not
   * selected. To ensure form data persisted in mongo is up-to-date, we need to
   * identify and remove them
   */
  private def unselectedChoiceElements(
    pageModel: PageModel[DataExpanded],
    requestData: Map[String, Seq[String]]
  ): Seq[ModelComponentId] =
    pageModel.allFormComponents.collect {
      case f @ IsChoice(_) if !requestData.contains(f.id.value) =>
        Set(f.modelComponentId)
      case f @ IsRevealingChoice(revealingChoice) if !requestData.contains(f.id.value) =>
        revealingChoice.options.flatMap(_.revealingFields).map(_.modelComponentId).toSet + f.modelComponentId
    }.flatten

  private def cleanVariadicValues(
    value: String,
    formComponentId: FormComponentId,
    formModel: FormModel[DataExpanded]
  ): String =
    formModel.fcLookup.get(formComponentId) match {
      case Some(formComponent) if formComponent.isNumeric                              => value.replace("£", "")
      case Some(formComponent) if formComponent.isUkSortCode && isValidSortCode(value) => value.replaceAll("[^0-9]", "")
      case Some(formComponent)
          if formComponent.isSterling || formComponent.isPositiveNumber || formComponent.isNumber =>
        val poundOrComma = "[£,]".r
        poundOrComma.replaceAllIn(value, "")
      case Some(formComponent) if formComponent.isReferenceNumber => value.replace(" ", "")
      case Some(formComponent) if formComponent.isPayeReference   => value.replace(" ", "")
      case Some(formComponent)
          if formComponent.isUTR || formComponent.isUkBankAccountNumber || formComponent.isCompanyRegistrationNumber =>
        value.replaceAll(" ", "")
      case Some(formComponent) if formComponent.isUkVrn =>
        value.toUpperCase.trim.replace(" ", "").replaceAll("^GB([0-9]{9}$)", "$1")
      case Some(formComponent) if formComponent.isNino   => value.toUpperCase.trim
      case Some(formComponent) if formComponent.isEORI   => value.toUpperCase.trim
      case Some(formComponent) if formComponent.isUkEORI => value.toUpperCase.trim
      case None if formComponentId.modelComponentId.fold(_ => false)({
            case ModelComponentId.Atomic(_, Date.day)   => true
            case ModelComponentId.Atomic(_, Date.month) => true
            case ModelComponentId.Atomic(_, Date.year)  => true
            case _                                      => false
          }) =>
        value.replaceAll(" ", "")
      case None
          if formModel
            .addressLookup(formComponentId.baseComponentId) && formComponentId.modelComponentId.fold(_ => false)({
            case ModelComponentId.Atomic(_, Address.postcode) => true
            case _                                            => false
          }) || formModel.postcodeLookup(formComponentId.baseComponentId) =>
        PostcodeLookupValidation.normalisePostcode(value)
      case _ => value
    }

  private def normalizeMonth(
    value: String
  ): String = value.replaceAll(" ", "").toLowerCase() match {
    case "jan" | "january"   => "1"
    case "feb" | "february"  => "2"
    case "mar" | "march"     => "3"
    case "apr" | "april"     => "4"
    case "may"               => "5"
    case "jun" | "june"      => "6"
    case "jul" | "july"      => "7"
    case "aug" | "august"    => "8"
    case "sep" | "september" => "9"
    case "oct" | "october"   => "10"
    case "nov" | "november"  => "11"
    case "dec" | "december"  => "12"
    case otherwise           => otherwise
  }

  private def isValidSortCode(value: String): Boolean =
    value match {
      case TextChecker.ukSortCodeFormat() => true
      case _                              => false
    }
}
