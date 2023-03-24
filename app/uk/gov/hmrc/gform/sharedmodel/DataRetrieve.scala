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
import scala.util.matching.Regex
import uk.gov.hmrc.gform.eval.ExpressionResultWithTypeInfo
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, IncludeIf, JsonUtils }

final case class Fetch(path: List[String]) extends AnyVal

object Fetch {
  implicit val format: OFormat[Fetch] = derived.oformat()
}

sealed trait ConstructAttribute extends Product with Serializable

object ConstructAttribute {
  final case class AsIs(value: Fetch) extends ConstructAttribute
  final case class Concat(value: List[Fetch]) extends ConstructAttribute

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

case class DataRetrieveId(value: String) extends AnyVal {
  def withIndex(index: Int): DataRetrieveId =
    DataRetrieveId(index + "_" + value)
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
  `if`: Option[IncludeIf]
) {

  def prepareRequest(
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser]
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
      evaluatedParams
    )
  }

  def fromObject(json: JsValue, instructions: List[AttributeInstruction]): List[(DataRetrieve.Attribute, String)] =
    instructions.map { x =>
      val res: String = x.from match {
        case ConstructAttribute.AsIs(fetch) =>
          val jsPath = fetch.path.foldLeft(JsPath: JsPath) { case (acc, next) =>
            acc \ next
          }
          jsPath.json.pick[JsValue].reads(json) match {
            case JsSuccess(JsString(attributeValue), _)  => attributeValue
            case JsSuccess(JsNumber(attributeValue), _)  => attributeValue.toString
            case JsSuccess(JsBoolean(attributeValue), _) => attributeValue.toString
            case _                                       => ""
          }
        case ConstructAttribute.Concat(fetches) =>
          fetches
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
      }
      x.attribute -> res
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

}

object DataRetrieve {

  final case class Request(
    json: JsValue,
    params: List[(String, String)]
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
    case object Integer extends AttrType

    implicit val format: OFormat[AttrType] = derived.oformat()
    implicit val equal: Eq[AttrType] = Eq.fromUniversalEquals
  }

  sealed trait ParamType extends Product with Serializable {
    def asString(expressionResult: ExpressionResultWithTypeInfo)(implicit messages: Messages): String = this match {
      case ParamType.String  => expressionResult.stringRepresentation
      case ParamType.Integer => expressionResult.numberRepresentation.map(_.toInt.toString).getOrElse("")
    }
  }

  object ParamType {
    case object String extends ParamType
    case object Integer extends ParamType

    implicit val format: OFormat[ParamType] = derived.oformat()
  }

  final case class Type(name: String) extends AnyVal

  final case class Attribute(name: String)
  final case class Parameter(name: String, path: List[String], tpe: ParamType)

  object Type {
    implicit val format: OFormat[Type] = derived.oformat()
  }

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
  requestParams: JsValue // Request data used to decide if new call to the API is need when input data are changing
)

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
