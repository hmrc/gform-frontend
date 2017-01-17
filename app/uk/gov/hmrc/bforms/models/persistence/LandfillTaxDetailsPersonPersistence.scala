/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.models.persistence

import java.time.LocalDate

import org.apache.commons.lang3.RandomStringUtils
import play.api.libs.json.{Format, JsError, JsResult, JsValue, Json, _}

case class LandfillTaxDetailsPersonPersistence(registrationNumber: GovernmentGatewayId = GovernmentGatewayId(RandomStringUtils.random(4)),
                                               firstName: FirstName = new FirstName(""),
                                               lastName: LastName = new LastName(""),
                                               telephoneNumber: TelephoneNumber = new TelephoneNumber(""),
                                               status: Status = new Status(""),
                                               datePersisted: LocalDate = LocalDate.now
                                              ) {
}


case class EitherLandfillTaxDetailsPersonPersistenceMapStringString(value: Either[LandfillTaxDetailsPersonPersistence, Map[String, String]])

object LandfillTaxDetailsPersonPersistence {

  def apply(governmentGateway: GovernmentGatewayId,
            firstName: FirstName,
            lastName: LastName,
            telephoneNumber: TelephoneNumber,
            status: Status) = {

    new LandfillTaxDetailsPersonPersistence(governmentGateway,
      firstName,
      lastName,
      telephoneNumber,
      status)
  }

  val mongoFormat = Json.format[LandfillTaxDetailsPersonPersistence]
}

object EitherLandfillTaxDetailsPersonPersistenceMapStringString {

  implicit val formatLandfill: Format[LandfillTaxDetailsPersonPersistence] = LandfillTaxDetailsPersonPersistence.mongoFormat

  def apply(value: LandfillTaxDetailsPersonPersistence) = new EitherLandfillTaxDetailsPersonPersistenceMapStringString(Left(value))

  def apply(value: Map[String, String]) = new EitherLandfillTaxDetailsPersistenceMapStringString(Right(value))

  implicit val format: Format[Either[LandfillTaxDetailsPersonPersistence, Map[String, String]]] = ValueClassFormatEitherLandfillTaxDetailsPersonPersistenceMapStringString.format[LandfillTaxDetailsPersonPersistence, Map[String, String]]
}

object ValueClassFormatEitherLandfillTaxDetailsPersonPersistenceMapStringString {

  def format[A <: LandfillTaxDetailsPersonPersistence, B <: Map[String, String]](implicit readL: Reads[A], readR: Reads[B], writeL: Writes[A], writeR: Writes[B]): Format[Either[A, B]] = {
    new Format[Either[A, B]] {
      def reads(json: JsValue): JsResult[Either[A, B]] = {
        json match {
          case o@JsObject(_) =>
            (o.value.get("object"), o.value.get("map")) match {
              case (Some(obj), None) => {
                println("object")
                readL.reads(obj).map(Left(_))
              }
              case (None, Some(obj)) => {
                println("map")
                readR.reads(obj).map(Right(_))
              }
              case (unknownL, unknownR) => JsError(
                s"""|Expected 'object' or 'map' for Either[L, R] type.
                    |UnknownL: $unknownL
                    |UnkownR : $unknownR
                   """.stripMargin)
            }
          case unknown => JsError(s"JsObject expected got $unknown")
        }
      }

      def writes(v: Either[A, B]): JsValue = {
        v match {
          case Left(left) => {
            println("Left(left)")
            Json.obj("object" -> writeL.writes(left))
          }
          case Right(right) => {
            println("Right(right)")
            Json.obj("map" -> writeR.writes(right))
          }
        }
      }
    }
  }
}

