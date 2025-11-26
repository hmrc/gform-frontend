/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.gform

import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.{ AllowedValueType, AllowedValues, Attr, AttributeInstruction, ConstructAttribute, DataRetrieve, Fetch }

object DataRetrieveResponseValidator {

  sealed trait DataRetrieveValidationResult

  object DataRetrieveValidationResult {
    case object Success extends DataRetrieveValidationResult
    case class Failure(errors: List[String]) extends DataRetrieveValidationResult
  }

  private def validateResponse(
    json: JsValue,
    instructions: List[AttributeInstruction],
    endpointName: String
  ): DataRetrieveValidationResult =
    json match {
      case obj: JsObject =>
        val errors = scala.collection.mutable.ListBuffer[String]()
        val jsonFields = obj.fields.toMap

        val fieldValidationRules: Map[String, Option[AllowedValues]] = instructions.flatMap { instruction =>
          extractFieldPaths(instruction.from).map { fieldPath =>
            fieldPath -> instruction.allowedValues
          }
        }.toMap

        val expectedFieldNames = fieldValidationRules.keySet

        val unexpectedKeys = jsonFields.keySet -- expectedFieldNames.map(_.split("\\.").head)
        unexpectedKeys.foreach { key =>
          errors += s"unexpected key in JSON response '$key'"
        }

        fieldValidationRules.foreach { case (fieldPath, allowedValuesOpt) =>
          val pathParts = fieldPath.split("\\.")
          val value = navigateJsonPath(obj, pathParts)

          value match {
            case Some(jsValue) =>
              allowedValuesOpt.foreach { allowedValues =>
                validateFieldValue(jsValue, fieldPath, allowedValues, endpointName) match {
                  case Some(error) => errors += error
                  case None        => // Valid
                }
              }
            case None =>
              allowedValuesOpt.foreach { allowedValues =>
                if (allowedValues.isRequired) {
                  errors += s"missing required field '$fieldPath'"
                }
              }
          }
        }

        if (errors.isEmpty) DataRetrieveValidationResult.Success
        else DataRetrieveValidationResult.Failure(errors.toList)

      case _ =>
        DataRetrieveValidationResult.Failure(List("response must be a JSON object"))
    }

  private def extractFieldPaths(construct: ConstructAttribute): List[String] =
    construct match {
      case ConstructAttribute.AsIs(Fetch(path)) if path.nonEmpty =>
        List(path.mkString("."))
      case ConstructAttribute.Concat(fetches) =>
        fetches.flatMap {
          case Fetch(path) if path.nonEmpty => Some(path.mkString("."))
          case _                            => None
        }
      case ConstructAttribute.Combine(fetches) =>
        fetches.flatMap {
          case (_, Fetch(path)) if path.nonEmpty => Some(path.mkString("."))
          case _                                 => None
        }
      case ConstructAttribute.ExtractAtIndex(Fetch(path), _) if path.nonEmpty =>
        List(path.mkString("."))
      case _ => Nil
    }

  private def navigateJsonPath(json: JsValue, path: Array[String]): Option[JsValue] =
    if (path.isEmpty) {
      Some(json)
    } else {
      json match {
        case obj: JsObject =>
          obj.value.get(path.head).flatMap { value =>
            if (path.length == 1) Some(value)
            else navigateJsonPath(value, path.tail)
          }
        case _ => None
      }
    }

  private def getValueTypeAsString(valueType: AllowedValueType): String =
    valueType match {
      case AllowedValueType.JsStringType  => "string"
      case AllowedValueType.JsNumberType  => "number"
      case AllowedValueType.JsBooleanType => "boolean"
      case AllowedValueType.AnyValueType  => "any"
    }

  private def validateFieldValue(
    jsValue: JsValue,
    fieldName: String,
    allowedValues: AllowedValues,
    endpointName: String
  ): Option[String] =
    jsValue match {
      case JsArray(elements) =>
        val arrayErrors = elements.zipWithIndex.flatMap { case (element, index) =>
          validateFieldValue(element, s"$fieldName[$index]", allowedValues, endpointName)
            .map(error => s"array element at index $index: ${error.stripPrefix(s"field '$fieldName[$index]' ")}")
        }
        if (arrayErrors.nonEmpty) {
          Some(s"field '$fieldName' contains invalid elements: ${arrayErrors.mkString(", ")}")
        } else {
          None
        }
      case _ =>
        if (allowedValues.allowsAnyValue) {
          (jsValue, allowedValues.valueType) match {
            case (JsString(""), _) if allowedValues.isRequired =>
              Some(s"required field '$fieldName' cannot be empty")
            case (JsString(_), AllowedValueType.JsStringType)                              => None
            case (JsNumber(_), AllowedValueType.JsNumberType)                              => None
            case (JsBoolean(_), AllowedValueType.JsBooleanType)                            => None
            case (JsString(_) | JsNumber(_) | JsBoolean(_), AllowedValueType.AnyValueType) => None
            case _ =>
              val expectedType = getValueTypeAsString(allowedValues.valueType)
              Some(s"field '$fieldName' must be a $expectedType")
          }
        } else {
          (jsValue, allowedValues.valueType) match {
            case (JsString(value), AllowedValueType.JsStringType) =>
              if (value.isEmpty && !allowedValues.isRequired) {
                None // Empty optional field is OK
              } else if (!allowedValues.values.contains(value)) {
                Some(s"unexpected value for '$fieldName': '$value'")
              } else {
                None
              }
            case (JsNumber(value), AllowedValueType.JsNumberType) =>
              val valueAsString = value.toString()
              if (!allowedValues.values.contains(valueAsString)) {
                Some(s"unexpected value for '$fieldName': '$valueAsString'")
              } else {
                None
              }
            case (JsBoolean(value), AllowedValueType.JsBooleanType) =>
              val valueAsString = value.toString
              if (!allowedValues.values.contains(valueAsString)) {
                Some(s"unexpected value for '$fieldName': '$valueAsString'")
              } else {
                None
              }
            case (JsString(""), _) if !allowedValues.isRequired =>
              None // Empty optional field is OK
            case _ =>
              val expectedType = getValueTypeAsString(allowedValues.valueType)
              Some(s"field '$fieldName' must be a $expectedType")
          }
        }
    }

  def validateDataRetrieveResponse(json: JsValue, dataRetrieve: DataRetrieve): DataRetrieveValidationResult = {
    val endpointName = dataRetrieve.tpe.name

    dataRetrieve.attributes match {
      case Attr.FromObject(instructions) =>
        validateResponse(json, instructions, endpointName)
      case Attr.FromArray(instructions) =>
        json match {
          case JsArray(elements) =>
            val errors = scala.collection.mutable.ListBuffer[String]()
            elements.zipWithIndex.foreach { case (element, index) =>
              validateResponse(element, instructions, s"$endpointName[index $index]") match {
                case DataRetrieveValidationResult.Failure(elementErrors) =>
                  errors ++= elementErrors.map(err => s"[index $index] $err")
                case DataRetrieveValidationResult.Success => // Valid element
              }
            }
            if (errors.isEmpty) DataRetrieveValidationResult.Success
            else DataRetrieveValidationResult.Failure(errors.toList)
          case _ =>
            DataRetrieveValidationResult.Failure(List("response must be a JSON array"))
        }
    }
  }
}
