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

import cats.data.Validated
import cats.implicits._
import com.softwaremill.quicklens._
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.objectStore.File
import uk.gov.hmrc.gform.models.ids.IndexedComponentId
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel.form.ValidatorsResult
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.collection.mutable.LinkedHashSet
import uk.gov.hmrc.gform.eval.DataRetrieveEval
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.Attribute
import uk.gov.hmrc.gform.sharedmodel.DataRetrieveId

object ValidationUtil {

  type GformError = Map[ModelComponentId, LinkedHashSet[String]]
  type ValidatedNumeric = Validated[String, Int]

  type ValidatedType[A] = Validated[GformError, A]
  type EitherType[A] = Either[GformError, A]

  val printErrors: LinkedHashSet[String] => LinkedHashSet[String] = (map: LinkedHashSet[String]) => {
    map
  }

  def renderErrors(validationResult: FormFieldValidationResult): LinkedHashSet[String] =
    validationResult match {
      case FieldError(fv, _, errors)      => errors
      case FieldGlobalError(_, _, errors) => errors
      case ComponentField(_, compData) =>
        val orderedSet = LinkedHashSet[String]()
        orderedSet ++= compData.flatMap(kv => renderErrors(kv._2))
        orderedSet
      case _ => LinkedHashSet.empty[String]
    }

  private def evaluateWithSuffix(
    formComponent: FormComponent,
    gformErrors: Map[ModelComponentId, LinkedHashSet[String]]
  )(
    dGetter: ModelComponentId => Seq[String]
  ): List[(ModelComponentId, FormFieldValidationResult)] =
    formComponent.multiValueId.fold(_ => List.empty[(ModelComponentId, FormFieldValidationResult)]) { multiple =>
      multiple.atoms.toList.map { modelComponentId =>
        gformErrors.get(modelComponentId) match {
          case Some(errors) =>
            (modelComponentId, FieldError(formComponent, dGetter(modelComponentId).headOption.getOrElse(""), errors))
          case None => (modelComponentId, FieldOk(formComponent, dGetter(modelComponentId).headOption.getOrElse("")))
        }
      }
    }

  private def evaluateWithoutSuffix(
    formComponent: FormComponent,
    gformErrors: Map[ModelComponentId, LinkedHashSet[String]]
  )(
    dGetter: ModelComponentId => Seq[String]
  ): (ModelComponentId, FormFieldValidationResult) = {
    val modelComponentId = formComponent.modelComponentId
    val data = dGetter(modelComponentId).headOption.getOrElse("")
    gformErrors.get(modelComponentId) match {
      //without suffix
      case Some(errors) => (modelComponentId, FieldGlobalError(formComponent, data, errors))
      case None         => (modelComponentId, FieldGlobalOk(formComponent, data))
    }
  }

  def evaluateValidationResult[D <: DataOrigin](
    atomicFields: List[FormComponent],
    validationResult: ValidatedType[ValidatorsResult],
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    envelope: EnvelopeWithMapping
  ): ValidationResult = {

    // ToDo: This should be removed and replaced with data.data.one or data.data.many as appropriate
    val dataGetter: ModelComponentId => Seq[String] = fId =>
      formModelVisibilityOptics.data.get(fId).toList.flatMap(_.toSeq)

    val gFormErrors: Map[ModelComponentId, LinkedHashSet[String]] =
      validationResult.swap.getOrElse(Map.empty[ModelComponentId, LinkedHashSet[String]])

    def multiFieldValidationResult(
      formComponent: FormComponent,
      optics: FormModelVisibilityOptics[D]
    ): FormFieldValidationResult = {

      val getter: ModelComponentId => Seq[String] = fId => optics.data.get(fId).toList.flatMap(_.toSeq)
      val valSuffixResult: List[(ModelComponentId, FormFieldValidationResult)] =
        evaluateWithSuffix(formComponent, gFormErrors)(getter)

      val valWithoutSuffixResult: (ModelComponentId, FormFieldValidationResult) =
        evaluateWithoutSuffix(formComponent, gFormErrors)(getter)

      val dataMap = (valWithoutSuffixResult :: valSuffixResult).map { kv =>
        HtmlFieldId.pure(kv._1) -> kv._2
      }.toMap

      ComponentField(formComponent, dataMap)
    }

    def handleDataRetrieveAddress(
      formComponent: FormComponent,
      dataRetrieveId: DataRetrieveId,
      isUkAddress: Boolean
    ) = {
      val atomMap =
        formModelVisibilityOptics.recalculationResult.evaluationContext.thirdPartyData.dataRetrieve.fold(
          Map.empty[String, String]
        )(dr => DataRetrieveEval.getDataRetrieveAddressMap(dr, dataRetrieveId, isUkAddress))

      val syntheticOptics = formModelVisibilityOptics
        .modify(_.recData.variadicFormData)
        .using(_.withCopyFromAtom(formComponent.modelComponentId, atomMap))
      multiFieldValidationResult(formComponent, syntheticOptics)
    }

    def matchComponentType(formComponent: FormComponent): FormFieldValidationResult = formComponent match {
      case IsAddress(Address(_, _, _, Some(AuthCtx(AuthInfo.ItmpAddress)))) =>
        val itmpAddress = formModelVisibilityOptics.recalculationResult.evaluationContext.thirdPartyData.itmpRetrievals
          .flatMap(_.itmpAddress)
        val atomMap: Map[String, String] = Map(
          "street1"  -> itmpAddress.flatMap(_.line1).getOrElse(""),
          "street2"  -> itmpAddress.flatMap(_.line2).getOrElse(""),
          "street3"  -> itmpAddress.flatMap(_.line3).getOrElse(""),
          "street4"  -> itmpAddress.flatMap(address => address.line4.map(_ + " ") |+| address.line5).getOrElse(""),
          "postcode" -> itmpAddress.flatMap(_.postCode).getOrElse(""),
          "uk"       -> itmpAddress.flatMap(_.countryName).filter(_.trim.nonEmpty).fold(true)(_ => false).toString,
          "country"  -> itmpAddress.flatMap(_.countryName).filter(_.trim.nonEmpty).getOrElse("")
        )

        val syntheticOptics = formModelVisibilityOptics
          .modify(_.recData.variadicFormData)
          .using(_.withCopyFromAtom(formComponent.modelComponentId, atomMap))
        multiFieldValidationResult(formComponent, syntheticOptics)

      case IsAddress(Address(_, _, _, Some(DataRetrieveCtx(drId, Attribute("registeredOfficeAddress"))))) =>
        handleDataRetrieveAddress(formComponent, drId, isUkAddress = true)

      case IsAddress(Address(_, _, _, Some(DataRetrieveCtx(drId, Attribute("agencyAddress"))))) =>
        handleDataRetrieveAddress(formComponent, drId, isUkAddress = true)

      case IsAddress(Address(_, _, _, Some(FormCtx(fcId)))) =>
        def mapper: IndexedComponentId => IndexedComponentId = {
          case IndexedComponentId.Pure(_)           => IndexedComponentId.Pure(formComponent.baseComponentId)
          case IndexedComponentId.Indexed(_, index) => IndexedComponentId.Indexed(formComponent.baseComponentId, index)
        }
        val syntheticOptics = formModelVisibilityOptics
          .modify(_.recData.variadicFormData)
          .using(_.withSyntheticCopy(fcId.baseComponentId, mapper))
        multiFieldValidationResult(formComponent, syntheticOptics)

      case IsOverseasAddress(OverseasAddress(_, _, _, Some(DataRetrieveCtx(drId, Attribute("agencyAddress"))), _, _)) =>
        handleDataRetrieveAddress(formComponent, drId, isUkAddress = false)

      case IsOverseasAddress(OverseasAddress(_, _, _, Some(AuthCtx(AuthInfo.ItmpAddress)), _, _)) =>
        val itmpAddress = formModelVisibilityOptics.recalculationResult.evaluationContext.thirdPartyData.itmpRetrievals
          .flatMap(_.itmpAddress)
        val itmpLines = List(
          itmpAddress.flatMap(_.line1).getOrElse(""),
          itmpAddress.flatMap(_.line2).getOrElse(""),
          itmpAddress.flatMap(_.line3).getOrElse(""),
          itmpAddress.flatMap(_.line4).getOrElse(""),
          itmpAddress.flatMap(_.line5).getOrElse("")
        ).filter(_.trim.nonEmpty)
        val (ls, city) = itmpLines.splitAt(itmpLines.size - 1)
        val lines = if (ls.size <= 3) {
          ls
        } else {
          val lastLine = ls.drop(2).mkString(" ")
          ls.take(2) :+ lastLine
        }.padTo(3, "")
        val linesMap = List("line1", "line2", "line3").zip(lines).toMap
        val otherMap = Map(
          "city"     -> city.headOption.getOrElse(""),
          "postcode" -> itmpAddress.flatMap(_.postCode).getOrElse(""),
          "country"  -> itmpAddress.flatMap(_.countryName).filter(_.trim.nonEmpty).getOrElse("")
        )
        val atomMap: Map[String, String] = linesMap ++ otherMap

        val syntheticOptics = formModelVisibilityOptics
          .modify(_.recData.variadicFormData)
          .using(_.withCopyFromAtom(formComponent.modelComponentId, atomMap))
        multiFieldValidationResult(formComponent, syntheticOptics)

      case IsOverseasAddress(OverseasAddress(_, _, _, Some(FormCtx(fcId)), _, _)) =>
        def mapper: IndexedComponentId => IndexedComponentId = {
          case IndexedComponentId.Pure(_)           => IndexedComponentId.Pure(formComponent.baseComponentId)
          case IndexedComponentId.Indexed(_, index) => IndexedComponentId.Indexed(formComponent.baseComponentId, index)
        }
        val syntheticOptics = formModelVisibilityOptics
          .modify(_.recData.variadicFormData)
          .using(_.withSyntheticCopy(fcId.baseComponentId, mapper))
        multiFieldValidationResult(formComponent, syntheticOptics)

      case IsMultiField(_) => multiFieldValidationResult(formComponent, formModelVisibilityOptics)

      case IsTextOrTextArea(constraint) =>
        val atomicFcId = formComponent.modelComponentId
        val data = dataGetter(atomicFcId).headOption.getOrElse("")
        val okData = constraint match {
          case UkVrn | CompanyRegistrationNumber | EORI | UkEORI => data.replace(" ", "")
          case _                                                 => data
        }
        gFormErrors
          .get(atomicFcId)
          .fold[FormFieldValidationResult](
            FieldOk(formComponent, okData)
          )(errors => FieldError(formComponent, data, errors))

      case IsGroup(_) =>
        FieldOk(formComponent, "") //nothing to validate for group (TODO - review)

      case IsChoice(_) | IsRevealingChoice(_) =>
        val atomicFcId = formComponent.modelComponentId

        def ifNoData =
          gFormErrors
            .get(atomicFcId)
            .fold[FormFieldValidationResult](
              FieldOk(formComponent, "") // This is optional field, for which no data was entered
            ) { errors =>
              FieldError(formComponent, dataGetter(atomicFcId).headOption.getOrElse(""), errors)
            }

        formModelVisibilityOptics.data
          .many(atomicFcId)
          .filterNot(_.isEmpty)
          .fold[FormFieldValidationResult](ifNoData) { selectedValue =>
            val optionalData = selectedValue.map { index =>
              val currentValue = dataGetter(atomicFcId).headOption.getOrElse("")
              val formFieldValidationResult = gFormErrors.get(atomicFcId) match {
                case Some(errors) => FieldError(formComponent, currentValue, errors)
                case None         => FieldOk(formComponent, currentValue)
              }
              HtmlFieldId.indexed(formComponent.id, index) -> formFieldValidationResult
            }.toMap
            ComponentField(formComponent, optionalData)
          }

      case IsFileUpload(_) =>
        val modelComponentId = formComponent.modelComponentId
        val fileName = envelope.findSingle(modelComponentId).map(_.fileName).getOrElse("")
        gFormErrors.get(modelComponentId) match {
          case Some(errors) => FieldError(formComponent, fileName, errors)
          case None         => FieldOk(formComponent, fileName)
        }

      case IsMultiFileUpload(_) =>
        val modelComponentId = formComponent.modelComponentId
        val files: List[(FileComponentId.Multi, File)] = envelope.findMulti(modelComponentId)

        gFormErrors.get(modelComponentId) match {
          case Some(errors) =>
            FieldError(formComponent, files.headOption.map { case (_, file) => file.fileName }.getOrElse(""), errors)
          case None =>
            val validationResult = files.map { case (fileComponentId, file) =>
              HtmlFieldId.indexed(fileComponentId.formComponentId, fileComponentId.index.toString) -> FieldOk(
                formComponent,
                file.fileName
              )
            }.toMap
            ComponentField(formComponent, validationResult)
        }

      case IsInformationMessage(_) => FieldOk(formComponent, "")

      case IsTableComp(_) => FieldOk(formComponent, "")

      case IsMiniSummaryList(_) => FieldOk(formComponent, "")

      case IsHmrcTaxPeriod(_) =>
        val atomicFcId = formComponent.modelComponentId
        gFormErrors.get(atomicFcId) match {
          case Some(errors) =>
            FieldError(formComponent, dataGetter(atomicFcId).headOption.getOrElse(""), errors)
          case None =>
            val optionalData = formModelVisibilityOptics.data.one(atomicFcId).map { selectedValue =>
              selectedValue.map { _ =>
                HtmlFieldId
                  .pure(atomicFcId) -> FieldOk(formComponent, dataGetter(atomicFcId).headOption.getOrElse(""))
              }.toMap

            }
            ComponentField(formComponent, optionalData.getOrElse(Map.empty))
        }

      case IsTime(_) =>
        val atomicFcId = formComponent.modelComponentId
        val data = dataGetter(atomicFcId).headOption.getOrElse("")

        gFormErrors
          .get(atomicFcId)
          .fold[FormFieldValidationResult](
            FieldOk(formComponent, data)
          )(errors => FieldError(formComponent, dataGetter(atomicFcId).headOption.getOrElse(""), errors))

      case _ => FieldOk(formComponent, "")
    }

    val resultErrors: List[FormFieldValidationResult] = atomicFields.map { formComponent =>
      matchComponentType(formComponent)
    }

    val validatorsResult: Option[ValidatorsResult] = validationResult.toOption
    new ValidationResult(resultErrors.map(ffvr => ffvr.formComponent.id -> ffvr).toMap, validatorsResult)
  }

  private final object IsTextOrTextArea {
    def unapply(fc: FormComponent): Option[TextConstraint] = fc.`type` match {
      case Text(constraint, _, _, _, _, _, _)  => Some(constraint)
      case TextArea(constraint, _, _, _, _, _) => Some(constraint)
      case _                                   => None
    }
  }
}
