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
import org.slf4j.{ Logger, LoggerFactory }
import uk.gov.hmrc.gform.sharedmodel.{ AllowedValueType, AllowedValues, Attr, AttributeInstruction, ConstructAttribute, DataRetrieve, Fetch }

object DataRetrieveResponseValidator {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  sealed trait ValidationResult
  case object ValidationSuccess extends ValidationResult
  case class ValidationFailure(errors: List[String]) extends ValidationResult

  private def validateResponse(
    json: JsValue,
    instructions: List[AttributeInstruction],
    endpointName: String
  ): ValidationResult =
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
          val error = s"unexpected key in JSON response '$key'"
          logger.debug(s"Validation failed for $endpointName: $error")
          errors += error
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
                  val error = s"missing required field '$fieldPath'"
                  logger.debug(s"Validation failed for $endpointName: $error")
                  errors += error
                }
              }
          }
        }

        if (errors.isEmpty) ValidationSuccess else ValidationFailure(errors.toList)

      case _ =>
        val error = "response must be a JSON object"
        logger.debug(s"Validation failed for $endpointName: $error")
        ValidationFailure(List(error))
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

  private def validateFieldValue(
    jsValue: JsValue,
    fieldName: String,
    allowedValues: AllowedValues,
    endpointName: String
  ): Option[String] =
    // If wildcard is specified, any value is allowed (unless it's required and empty)
    if (allowedValues.allowsAnyValue) {
      jsValue match {
        case JsString("") if allowedValues.isRequired =>
          logger.debug(s"Validation failed for $endpointName: required field '$fieldName' is empty")
          Some(s"required field '$fieldName' cannot be empty")
        case JsString(_) | JsNumber(_) | JsBoolean(_) | JsArray(_) => None
        case _ =>
          logger.debug(s"Validation failed for $endpointName: field '$fieldName' has invalid type")
          Some(s"field '$fieldName' must be a string, number, boolean, or array")
      }
    } else {
      (jsValue, allowedValues.valueType) match {
        case (JsString(value), AllowedValueType.JsStringType) =>
          if (value.isEmpty && !allowedValues.isRequired) {
            None // Empty optional field is OK
          } else if (!allowedValues.value.contains(value)) {
            logger.debug(
              s"Validation failed for $endpointName: unexpected value for '$fieldName': '$value'"
            )
            Some(s"unexpected value for '$fieldName': '$value'")
          } else {
            None
          }
        case (JsNumber(_), AllowedValueType.JsNumberType) =>
          None // Valid number type
        case (JsBoolean(_), AllowedValueType.JsBooleanType) =>
          None // Valid boolean type
        case (JsString(""), _) if !allowedValues.isRequired =>
          None // Empty optional field is OK
        case (JsArray(_), _) =>
          // Arrays are allowed when we have specific allowed values (e.g., for ExtractAtIndex)
          // The actual extraction happens elsewhere, so we just validate presence
          None
        case _ =>
          val expectedType = allowedValues.valueType match {
            case AllowedValueType.JsStringType  => "string"
            case AllowedValueType.JsNumberType  => "number"
            case AllowedValueType.JsBooleanType => "boolean"
            case AllowedValueType.AnyValue      => "any"
          }
          logger.debug(
            s"Validation failed for $endpointName: field '$fieldName' must be a $expectedType"
          )
          Some(s"field '$fieldName' must be a $expectedType")
      }
    }

  def validateDataRetrieveResponse(json: JsValue, dataRetrieve: DataRetrieve): ValidationResult = {
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
                case ValidationFailure(elementErrors) =>
                  errors ++= elementErrors.map(err => s"[index $index] $err")
                case ValidationSuccess => // Valid element
              }
            }
            if (errors.isEmpty) ValidationSuccess else ValidationFailure(errors.toList)
          case _ =>
            val error = "response must be a JSON array"
            logger.debug(s"Validation failed for $endpointName: $error")
            ValidationFailure(List(error))
        }
    }
  }
}
