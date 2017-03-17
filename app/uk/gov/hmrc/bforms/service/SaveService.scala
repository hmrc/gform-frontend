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

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.bforms.connectors.BformsConnector
import uk.gov.hmrc.bforms.models.{ FormData, FormId, FormTypeId, SaveResult, VerificationResult }
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._

object SaveService {

  def bformsConnector : BformsConnector = BformsConnector

  def getFormById(formTypeId: FormTypeId, version: String, formId: FormId)(implicit hc : HeaderCarrier) = {
    bformsConnector.getById(formTypeId, version, formId)
  }

  def saveFormData(formData: FormData, tolerant: Boolean)(implicit hc : HeaderCarrier): Future[SaveResult] = {
    bformsConnector.save(formData, tolerant)
  }

  def updateFormData(formId: FormId, formData: FormData, tolerant: Boolean)(implicit hc : HeaderCarrier): Future[SaveResult] = {
    bformsConnector.update(formId, formData, tolerant)
  }

  def sendSubmission(formTypeId: FormTypeId, formId: FormId)(implicit hc : HeaderCarrier): Future[HttpResponse] = {
    bformsConnector.sendSubmission(formTypeId, formId)
  }

  def saveMap(formDetails : Map[String, String], registrationNumber : String)(implicit hc : HeaderCarrier) : Future[VerificationResult] = {
    val environmentRegex = "environmentalBody1\\[(.*)\\]\\.(.*)".r

    val convertedDetails = formDetails.map {
      case (key, value) => key match {
        case environmentRegex(num, fieldName) =>
          (fieldName+"."+num, value)
        case _ => (key, value)
      }
    }

    val objectList = implicitly[OWrites[Map[String, String]]].writes(convertedDetails).value.map {
      case (key, value) =>
        val json = value match {
          case JsString(str) => str
          case JsNumber(num) => num.toString
          case JsObject(obj) => obj.toString
        }
        Json.obj("id" -> key, "value" -> json)
    }.toList

    val (environmentalBodies, nonEnvironmentalBodies) : (List[JsObject], List[JsObject])=
      objectList.partition{ o =>
        val id = o.value("id").as[String]
        id.startsWith("bodyName") || id.startsWith("amount")}

    val groupedList = environmentalBodies.groupBy(o => o.value("id").as[String].last)

    val finishedList = groupedList.values.map {
      case f :: l :: Nil =>
        println(f.value("id"))
        val fId = f.value("id").as[String].split("\\.")(0)
        val fValue = f.value("value")
        val lId = l.value("id").as[String].split("\\.")(0)
        val lValue = l.value("value")
        if (fId == "bodyName") {
          Json.obj(fId -> fValue, lId -> lValue)
        } else {
          Json.obj(lId -> lValue, fId -> fValue)
        }
      case _ => throw new IllegalArgumentException
    }

    val filteredList = nonEnvironmentalBodies.filter{ f =>
      val stringId = f.value("id").as[String]
      stringId != "csrfToken" &&
        stringId != "total" &&
        stringId!= "totalCredits" &&
        stringId != "netDue"}

    val completeDetails = filteredList :+ Json.obj(
      "id" -> "environmentalBodies",
      "value" -> Json.toJson(finishedList).toString
    )

    val jsonobject = Json.obj(
      "formTypeId" -> "LF100",
      "version" -> "0.1.0",
      "characterSet" -> "UTF-8",
      "fields" -> completeDetails
    )

    bformsConnector.saveForm(jsonobject, registrationNumber)
  }
}
