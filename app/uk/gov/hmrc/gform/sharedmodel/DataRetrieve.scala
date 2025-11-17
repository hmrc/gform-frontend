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

package uk.gov.hmrc.gform.sharedmodel

import cats.Eq
import julienrf.json.derived
import play.api.i18n.Messages
import play.api.libs.json._
import uk.gov.hmrc.gform.eval.ExpressionResultWithTypeInfo
import uk.gov.hmrc.gform.models.ids.ModelDataRetrieveId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, IncludeIf, JsonUtils }
import uk.gov.hmrc.gform.typeclasses.Now
import uk.gov.hmrc.gform.views.summary.TextFormatter

import java.time.LocalDateTime
import scala.util.matching.Regex

final case class Fetch(path: List[String]) extends AnyVal

object Fetch {
  implicit val format: OFormat[Fetch] = derived.oformat()
}

sealed trait ConstructAttribute extends Product with Serializable

object ConstructAttribute {
  final case class AsIs(value: Fetch) extends ConstructAttribute
  final case class Concat(value: List[Fetch]) extends ConstructAttribute
  final case class Combine(value: List[(DataRetrieve.Attribute, Fetch)]) extends ConstructAttribute
  final case class ExtractAtIndex(value: Fetch, index: Int) extends ConstructAttribute

  implicit val format: OFormat[ConstructAttribute] = derived.oformat()
}

final case class AttributeInstruction(
  attribute: DataRetrieve.Attribute,
  from: ConstructAttribute
)

object AttributeInstruction {
  implicit val format: OFormat[AttributeInstruction] = derived.oformat()
}

sealed trait Attr extends Product with Serializable

object Attr {
  final case class FromObject(inst: List[AttributeInstruction]) extends Attr
  final case class FromArray(inst: List[AttributeInstruction]) extends Attr

  implicit val format: OFormat[Attr] = derived.oformat()
}

case class DataRetrieveId(value: String) {
  val modelPageId = ModelDataRetrieveId.fromId(this)
  def withIndex(index: Int): DataRetrieveId =
    DataRetrieveId(s"${index}_$value")
}

object DataRetrieveId {
  implicit val format: Format[DataRetrieveId] =
    JsonUtils.valueClassFormat[DataRetrieveId, String](DataRetrieveId.apply, _.value)
}

case class DataRetrieve(
  tpe: DataRetrieve.Type,
  id: DataRetrieveId,
  attributes: Attr,
  attrTypeMapping: Map[DataRetrieve.Attribute, DataRetrieve.AttrType],
  params: List[DataRetrieve.ParamExpr],
  `if`: Option[IncludeIf],
  maxFailedAttempts: Option[Int],
  failureCountResetMinutes: Option[Int]
) {

  def prepareRequest(
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser],
    maybePreviousResult: Option[DataRetrieveResult],
    maybeCorrelationId: Option[String]
  )(implicit messages: Messages): DataRetrieve.Request = {
    val evaluatedParams = params.map { case DataRetrieve.ParamExpr(DataRetrieve.Parameter(name, _, tpe), expr) =>
      name -> tpe.asString(formModelVisibilityOptics.evalAndApplyTypeInfoFirst(expr))
    }

    val jsonObjects: List[JsObject] = params.map {
      case DataRetrieve.ParamExpr(DataRetrieve.Parameter(name, path, tpe), expr) =>
        val value = tpe.asString(formModelVisibilityOptics.evalAndApplyTypeInfoFirst(expr))

        val leafValue = Json.obj(name -> JsString(value))
        path.foldLeft(leafValue) { case (acc, path) =>
          Json.obj(path -> acc)
        }

    }

    val json = jsonObjects.foldLeft(Json.obj())(_.deepMerge(_))

    DataRetrieve.Request(
      json,
      evaluatedParams,
      maybePreviousResult.flatMap(_.failureCount),
      maybePreviousResult.flatMap(_.failureCountResetTime),
      maybeCorrelationId
    )
  }

  def fromObject(json: JsValue, instructions: List[AttributeInstruction]): List[(DataRetrieve.Attribute, String)] =
    instructions.flatMap { x =>
      def extractJsPath(fetch: Fetch): JsPath =
        fetch.path.foldLeft(JsPath: JsPath)((acc, next) => acc \ next)

      def fromFetch(fetch: Fetch): String = {
        val jsPath = extractJsPath(fetch)
        jsPath.json.pick[JsValue].reads(json) match {
          case JsSuccess(JsString(attributeValue), _) => attributeValue
          case JsSuccess(JsNumber(attributeValue), _) =>
            TextFormatter.stripTrailingZeros(attributeValue.toString)
          case JsSuccess(JsBoolean(attributeValue), _) => attributeValue.toString
          case _                                       => ""
        }
      }
      def fetchArray(fetch: Fetch): List[String] = {
        val jsPath = extractJsPath(fetch)
        jsPath.json.pick[JsValue].reads(json) match {
          case JsSuccess(JsArray(attributeValues), _) => attributeValues.toList.map(_.as[String])
          case _                                      => List.empty
        }
      }
      x.from match {
        case ConstructAttribute.AsIs(fetch) => List(x.attribute -> fromFetch(fetch))
        case ConstructAttribute.ExtractAtIndex(fetch, index) =>
          List(x.attribute -> fetchArray(fetch).lift(index).getOrElse(""))
        case ConstructAttribute.Concat(fetches) =>
          val res = fetches
            .map { fetch =>
              val jsPath = fetch.path.foldLeft(JsPath: JsPath) { case (acc, next) =>
                acc \ next
              }
              jsPath.json.pick[JsString].reads(json) match {
                case JsSuccess(JsString(attributeValue), _) => attributeValue
                case _                                      => ""
              }
            }
            .mkString(" ")
            .trim()
          List(x.attribute -> res)
        case ConstructAttribute.Combine(fetches) => fetches.map { case (attr, fetch) => attr -> fromFetch(fetch) }
      }
    }

  def processResponse(json: JsValue): JsResult[DataRetrieve.Response] =
    attributes match {
      case Attr.FromArray(attributes) =>
        json match {
          case JsArray(objs) =>
            val attrValues: List[Map[DataRetrieve.Attribute, String]] =
              objs.toList.map(json => fromObject(json, attributes).toMap)

            JsSuccess(
              DataRetrieve.Response.Array(
                attrValues
              )
            )
          case unexpected => JsError(s"Expected array response for $tpe")
        }
      case Attr.FromObject(attributes) =>
        val attrValues = fromObject(json, attributes)
        JsSuccess(
          DataRetrieve.Response.Object(
            attrValues.toMap
          )
        )
    }

  def emptyValidResponse(): ServiceCallResponse[DataRetrieve.Response] =
    attributes match {
      case Attr.FromArray(_)    => ServiceResponse(DataRetrieve.Response.Array(List.empty))
      case Attr.FromObject(ins) => ServiceResponse(DataRetrieve.Response.Object(ins.map(_.attribute -> "").toMap))
    }
}

object DataRetrieve {

  final case class Request(
    json: JsValue,
    params: List[(String, String)],
    previousFailureCount: Option[Int],
    failureResetTime: Option[LocalDateTime],
    correlationId: Option[String]
  ) {
    def paramsAsJson(): JsValue = Json.toJson(params.toMap)

    def notReady(): Boolean = params.exists { case (_, value) => value.isEmpty }

    def fillPlaceholders(str: String): String =
      params.foldLeft(str) { case (acc, (param, value)) => acc.replaceAll("\\{\\{" + param + "\\}\\}", value) }
  }

  sealed trait Response extends Product with Serializable {
    def toRetrieveDataType(): RetrieveDataType = this match {
      case Response.Object(response) => RetrieveDataType.ObjectType(response)
      case Response.Array(response)  => RetrieveDataType.ListType(response)
    }
  }

  object Response {
    final case class Object(
      response: Map[DataRetrieve.Attribute, String]
    ) extends Response
    final case class Array(
      response: List[Map[DataRetrieve.Attribute, String]]
    ) extends Response
  }

  final case class ParamExpr(
    parameter: DataRetrieve.Parameter,
    expr: Expr
  )

  object ParamExpr {
    implicit val format: OFormat[ParamExpr] = derived.oformat()
  }

  sealed trait AttrType extends Product with Serializable

  object AttrType {
    case object String extends AttrType
    case object Number extends AttrType
    case object Date extends AttrType

    implicit val format: OFormat[AttrType] = derived.oformat()
    implicit val equal: Eq[AttrType] = Eq.fromUniversalEquals
  }

  sealed trait ParamType extends Product with Serializable {
    def asString(expressionResult: ExpressionResultWithTypeInfo)(implicit messages: Messages): String = this match {
      case ParamType.String  => expressionResult.stringRepresentation
      case ParamType.Integer => expressionResult.numberRepresentation.map(_.toInt.toString).getOrElse("")
      case ParamType.Date    => expressionResult.dateRepresentation.map(_.toString).getOrElse("")
    }
  }

  object ParamType {
    case object String extends ParamType
    case object Integer extends ParamType
    case object Date extends ParamType

    implicit val format: OFormat[ParamType] = derived.oformat()
  }

  final case class Type(name: String) extends AnyVal

  final case class Attribute(name: String)
  final case class Parameter(name: String, path: List[String], tpe: ParamType)

  object Type {
    implicit val format: OFormat[Type] = derived.oformat()
  }

  val failureCountAttribute: Attribute = Attribute("failedCount")
  val failureIsBlockedAttribute: Attribute = Attribute("isBlocked")
  val failureResetTimeAttribute: Attribute = Attribute("unblockTime")
  val failureResetDateAttribute: Attribute = Attribute("unblockDate")

  object Attribute {

    val idValidation: String = "[_a-zA-Z]\\w*"
    val unanchoredIdValidation: Regex = s"""$idValidation""".r

    implicit val format: OFormat[Attribute] = derived.oformat()
  }

  object Parameter {
    implicit val format: OFormat[Parameter] = derived.oformat()
  }

  def requestParamsFromCache(
    form: Form,
    dataRetrieveId: DataRetrieveId
  ): Option[JsValue] = form.thirdPartyData.dataRetrieve.flatMap(
    _.get(dataRetrieveId).map(_.requestParams)
  )

  implicit val format: OFormat[DataRetrieve] = {
    implicit val attrTypeMappingFormat: Format[Map[DataRetrieve.Attribute, DataRetrieve.AttrType]] =
      JsonUtils.formatMap[DataRetrieve.Attribute, DataRetrieve.AttrType](DataRetrieve.Attribute.apply, _.name)
    derived.oformat()
  }
}

sealed trait RetrieveDataType extends Product with Serializable {
  def size: Int = this match {
    case RetrieveDataType.ObjectType(_) => 1
    case RetrieveDataType.ListType(xs)  => xs.size
  }
}

object RetrieveDataType {
  case class ObjectType(data: Map[DataRetrieve.Attribute, String]) extends RetrieveDataType
  case class ListType(data: List[Map[DataRetrieve.Attribute, String]]) extends RetrieveDataType
}

case class DataRetrieveResult(
  id: DataRetrieveId,
  data: RetrieveDataType,
  requestParams: JsValue, // Request data used to decide if new call to the API is need when input data are changing
  failureCount: Option[Int],
  failureMaxAttempts: Option[Int],
  failureCountResetTime: Option[LocalDateTime]
) {
  def isBlocked(implicit now: Now[LocalDateTime]) =
    (failureMaxAttempts, failureCount, failureCountResetTime) match {
      case (Some(max), Some(current), Some(resetTime)) => current >= max && resetTime.isAfter(now.apply())
      case (_, _, _)                                   => false
    }
}

object DataRetrieveResult {

  implicit val dataRetrieveSuccessDataFormat: OFormat[Map[DataRetrieve.Attribute, String]] =
    implicitly[OFormat[Map[String, String]]]
      .bimap[Map[DataRetrieve.Attribute, String]](
        _.map { case (key, value) =>
          DataRetrieve.Attribute(key) -> value
        },
        _.map { case (key, value) =>
          key.name -> value
        }
      )
  implicit val retrieveDataTypeFormat: Format[RetrieveDataType] = {

    val reads: Reads[RetrieveDataType] = Reads {
      case a: JsArray =>
        implicitly[Reads[List[Map[DataRetrieve.Attribute, String]]]].reads(a).map(RetrieveDataType.ListType)
      case other => implicitly[Reads[Map[DataRetrieve.Attribute, String]]].reads(other).map(RetrieveDataType.ObjectType)
    }

    val writes: Writes[RetrieveDataType] = Writes[RetrieveDataType] {

      case RetrieveDataType.ObjectType(data) => Json.toJson(data)
      case RetrieveDataType.ListType(data)   => Json.toJson(data)
    }

    Format[RetrieveDataType](reads, writes)
  }
  implicit val format: Format[DataRetrieveResult] = derived.oformat()
}
