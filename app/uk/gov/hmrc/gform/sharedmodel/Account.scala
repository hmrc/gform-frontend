package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json.{Json, OFormat, OWrites, Reads}

case class Account(
                    sortCode: String,
                    accountNumber: String
                  )

object Account {
  implicit val format: OFormat[Account]= Json.format[Account]
}

