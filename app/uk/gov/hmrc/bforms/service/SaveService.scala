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

package uk.gov.hmrc.bforms.service

import uk.gov.hmrc.bforms.connectors.{BformsConnector, VerificationResult}
import uk.gov.hmrc.bforms.models.LandfillTaxDetails
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._

import scala.collection.mutable.ListBuffer

object SaveService {

  def bformsConnector : BformsConnector = BformsConnector

  def saveToBackEndFormWithErrors(formDetails : Map[String, String], registrationNumber : String)(implicit hc : HeaderCarrier) : Future[VerificationResult] = {

    val environmentRegex = "environmentalBody1\\[(.*)\\]\\.(.*)".r
    val convertedDetails = formDetails.map {
      case (key, value) => key match {
        case environmentRegex(num, fieldName) =>
          (fieldName+"."+num, value)
        case _ => (key, value)
      }
    }

    val obj = implicitly[OWrites[Map[String, String]]].writes(convertedDetails).value.map {
      case (key, value) =>
        val json = value match {
          case JsString(str) => str
          case JsNumber(num) => num.toString
          case JsObject(obj) => obj.toString
        }
        Json.obj("id" -> key, "value" -> json)
    }.toList

    val (somelist, some) : (List[JsObject], List[JsObject])=
      obj.partition{o =>
        val string = o.value("id").as[String]
        string.startsWith("bodyName") || string.startsWith("amount")}
      val groupedList = somelist.groupBy(o => o.value("id").as[String].last)
      val finishedList = groupedList.values.map(bob => bob match {
        case y :: z :: Nil=>
          println(y.value("id"))
          val yId = y.value("id").as[String].split("\\.")(0)
          val yValue = y.value("value")
          val zId = z.value("id").as[String].split("\\.")(0)
          val zValue = z.value("value")
          if (yId == "bodyName") {
            Json.obj(yId -> yValue, zId -> zValue)
          } else {
            Json.obj(zId -> zValue, yId -> yValue)
          }
        case _ => throw new IllegalArgumentException
      })

    val filteredList = some.filter{ f => val stringId = f.value("id").as[String]
      stringId != "csrfToken" && stringId != "total"}
    val completeDetails = filteredList :+ Json.obj("id" -> "environmentalBodies", "value" -> Json.toJson(finishedList).toString)

    val jsonobject = Json.obj(
      "formTypeId" -> "LF100",
      "version" -> "0.1.0",
      "characterSet" -> "UTF-8",
      "fields" -> completeDetails
    )

    bformsConnector.saveForm(jsonobject, registrationNumber)
  }

  def saveToBackEnd[A](formDetails: LandfillTaxDetails, registrationNumber : String)(implicit hc : HeaderCarrier) : Future[VerificationResult] = {

    val formInfo = implicitly[OWrites[LandfillTaxDetails]].writes(formDetails).value.map {
      case(key, jsvalue) =>
      val json = jsvalue match {
        case JsString(str) => str
        case JsNumber(num) => num.toString
        case JsObject(obj) => obj.toString
        case others => others.toString
      }
      Json.obj("id" -> key, "value" -> json)}.toList

    val jsonobject : JsObject = Json.obj(
      "formTypeId" -> "LF100",
      "version" -> "0.1.0",
      "characterSet" -> "UTF-8",
      "fields" -> Json.toJson(formInfo)
    )

    bformsConnector.saveForm(jsonobject, registrationNumber)
  }
}