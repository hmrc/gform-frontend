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

package uk.gov.hmrc.gform.bars

import play.api.libs.json.{ JsObject, JsString, JsValue }
import org.slf4j.{ Logger, LoggerFactory }

object BankAccountSchemaValidation {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private object AllowedValues {
    val accountNumberIsWellFormatted: Set[String] = Set("yes", "no", "indeterminate")
    val nonStandardAccountDetailsRequiredForBacs: Set[String] = Set("yes", "no", "inapplicable")
    val sortCodeIsPresentOnEISCD: Set[String] = Set("yes", "no", "error")
    val accountExists: Set[String] = Set("yes", "no", "inapplicable", "indeterminate", "error")
    val nameMatches: Set[String] = Set("yes", "partial", "no", "inapplicable", "indeterminate", "error")
    val sortCodeSupportsDirectDebit: Set[String] = Set("yes", "no", "error")
    val sortCodeSupportsDirectCredit: Set[String] = Set("yes", "no", "error")
  }

  private case class SchemaField(name: String, isRequired: Boolean, allowedValues: Option[Set[String]] = None)

  private object Schemas {
    val validateBankDetails: Set[SchemaField] = Set(
      SchemaField("accountNumberIsWellFormatted", isRequired = true, Some(AllowedValues.accountNumberIsWellFormatted)),
      SchemaField(
        "nonStandardAccountDetailsRequiredForBacs",
        isRequired = true,
        Some(AllowedValues.nonStandardAccountDetailsRequiredForBacs)
      ),
      SchemaField("sortCodeIsPresentOnEISCD", isRequired = true, Some(AllowedValues.sortCodeIsPresentOnEISCD)),
      SchemaField("sortCodeBankName", isRequired = false),
      SchemaField("sortCodeSupportsDirectDebit", isRequired = false, Some(AllowedValues.sortCodeSupportsDirectDebit)),
      SchemaField("sortCodeSupportsDirectCredit", isRequired = false, Some(AllowedValues.sortCodeSupportsDirectCredit)),
      SchemaField("iban", isRequired = false)
    )

    val businessBankAccountExistence: Set[SchemaField] = Set(
      SchemaField("accountNumberIsWellFormatted", isRequired = true, Some(AllowedValues.accountNumberIsWellFormatted)),
      SchemaField("sortCodeIsPresentOnEISCD", isRequired = true, Some(AllowedValues.sortCodeIsPresentOnEISCD)),
      SchemaField("sortCodeBankName", isRequired = false),
      SchemaField(
        "nonStandardAccountDetailsRequiredForBacs",
        isRequired = true,
        Some(AllowedValues.nonStandardAccountDetailsRequiredForBacs)
      ),
      SchemaField("accountExists", isRequired = true, Some(AllowedValues.accountExists)),
      SchemaField("nameMatches", isRequired = true, Some(AllowedValues.nameMatches)),
      SchemaField("accountName", isRequired = false),
      SchemaField("sortCodeSupportsDirectDebit", isRequired = true, Some(AllowedValues.sortCodeSupportsDirectDebit)),
      SchemaField("sortCodeSupportsDirectCredit", isRequired = true, Some(AllowedValues.sortCodeSupportsDirectCredit)),
      SchemaField("iban", isRequired = false)
    )

    val personalBankAccountExistence: Set[SchemaField] = Set(
      SchemaField("accountNumberIsWellFormatted", isRequired = true, Some(AllowedValues.accountNumberIsWellFormatted)),
      SchemaField("accountExists", isRequired = true, Some(AllowedValues.accountExists)),
      SchemaField("nameMatches", isRequired = true, Some(AllowedValues.nameMatches)),
      SchemaField("accountName", isRequired = false),
      SchemaField(
        "nonStandardAccountDetailsRequiredForBacs",
        isRequired = true,
        Some(AllowedValues.nonStandardAccountDetailsRequiredForBacs)
      ),
      SchemaField("sortCodeIsPresentOnEISCD", isRequired = true, Some(AllowedValues.sortCodeIsPresentOnEISCD)),
      SchemaField("sortCodeSupportsDirectDebit", isRequired = true, Some(AllowedValues.sortCodeSupportsDirectDebit)),
      SchemaField("sortCodeSupportsDirectCredit", isRequired = true, Some(AllowedValues.sortCodeSupportsDirectCredit)),
      SchemaField("sortCodeBankName", isRequired = false),
      SchemaField("iban", isRequired = false)
    )
  }

  sealed trait ValidationResult
  case object ValidationSuccess extends ValidationResult
  case class ValidationFailure(errors: List[String]) extends ValidationResult

  private def validateResponse(json: JsValue, schema: Set[SchemaField], endpointName: String): ValidationResult =
    json match {
      case obj: JsObject =>
        val errors = scala.collection.mutable.ListBuffer[String]()
        val jsonFields = obj.fields.toMap

        val expectedFieldNames = schema.map(_.name)
        val unexpectedKeys = jsonFields.keySet -- expectedFieldNames
        unexpectedKeys.foreach { key =>
          val error = s"unexpected key in JSON response '$key'"
          logger.debug(s"Schema validation failed for $endpointName: $error")
          errors += error
        }

        schema.foreach { schemaField =>
          jsonFields.get(schemaField.name) match {
            case Some(JsString(value)) =>
              schemaField.allowedValues.foreach { allowedValues =>
                if (!allowedValues.contains(value) && !(value.isEmpty && !schemaField.isRequired)) {
                  val error = s"unexpected value for '${schemaField.name}': '$value'"
                  logger.debug(s"Schema validation failed for $endpointName: $error")
                  errors += error
                }
              }
            case Some(_) if !schemaField.isRequired =>
            // Optional field empty - this is fine
            case Some(_) =>
              val error = s"field '${schemaField.name}' must be a string"
              logger.debug(s"Schema validation failed for $endpointName: $error")
              errors += error
            case None if schemaField.isRequired =>
              val error = s"missing required field '${schemaField.name}'"
              logger.debug(s"Schema validation failed for $endpointName: $error")
              errors += error
            case None =>
            // Optional field is missing - this is fine
          }
        }

        if (errors.isEmpty) ValidationSuccess else ValidationFailure(errors.toList)

      case _ =>
        val error = "response must be a JSON object"
        logger.debug(s"Schema validation failed for $endpointName: $error")
        ValidationFailure(List(error))
    }

  def validateBankDetailsResponse(json: JsValue): ValidationResult =
    validateResponse(json, Schemas.validateBankDetails, "validateBankDetails")

  def validateBusinessBankAccountExistenceResponse(json: JsValue): ValidationResult =
    validateResponse(json, Schemas.businessBankAccountExistence, "businessBankAccountExistence")

  def validatePersonalBankAccountExistenceResponse(json: JsValue): ValidationResult =
    validateResponse(json, Schemas.personalBankAccountExistence, "personalBankAccountExistence")
}
