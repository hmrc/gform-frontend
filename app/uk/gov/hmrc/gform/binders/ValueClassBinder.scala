/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.binders

import cats.implicits._
import play.api.libs.json._
import play.api.mvc.{ JavascriptLiteral, PathBindable, QueryStringBindable }
import uk.gov.hmrc.gform.models.{ FastForward, LookupQuery }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormId, FormStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, Register, SectionNumber, SectionTitle4Ga, SuppressErrors }

import scala.util.Try
object ValueClassBinder {

  implicit val lookupQueryBinder: PathBindable[LookupQuery] =
    mkPathBindable(
      query => if (query.isEmpty) LookupQuery.Empty.asRight else LookupQuery.Value(query).asRight,
      _.asString)
  implicit val registerBinder: PathBindable[Register] = mkPathBindable(
    lookup => Register.fromString(lookup).fold[Either[String, Register]](Left(s"Unknown lookup: $lookup"))(Right.apply),
    _.asString)
  implicit val formTemplateIdBinder: PathBindable[FormTemplateId] = valueClassBinder(_.value)
  implicit val formIdBinder: PathBindable[FormId] = valueClassBinder(_.value)
  implicit val fileIdBinder: PathBindable[FileId] = valueClassBinder(_.value)

  implicit val accessCodeBinder: PathBindable[AccessCode] = valueClassBinder(_.value)
  implicit val submissionRefBinder: PathBindable[SubmissionRef] = valueClassBinder(_.value)
  implicit val destinationIdBinder: PathBindable[DestinationId] = valueClassBinder(_.id)
  implicit val sectionTitle4GaBinder: PathBindable[SectionTitle4Ga] = valueClassBinder(_.value)
  implicit val sectionNumberBinder: PathBindable[SectionNumber] = new PathBindable[SectionNumber] {
    override def bind(key: String, value: String): Either[String, SectionNumber] =
      Try { SectionNumber(value.toInt) }.map(_.asRight).getOrElse(s"No valid value in path $key: $value".asLeft)
    override def unbind(key: String, sectionNumber: SectionNumber): String = sectionNumber.value.toString
  }

  implicit val formStatusBinder: PathBindable[FormStatus] = new PathBindable[FormStatus] {
    override def bind(key: String, value: String): Either[String, FormStatus] =
      value match {
        case FormStatus(s) => s.asRight
        case _             => s"'$value' is not a valid FormStatus. Valid values are: ${FormStatus.all}".asLeft
      }

    override def unbind(key: String, value: FormStatus): String = value.toString
  }

  implicit val jLiteralAffinityGroup = new JavascriptLiteral[AccessCode] {
    def to(value: AccessCode): String = value.value
  }

  implicit def optionAccessCodeBinder(implicit stringBinder: PathBindable[String]): PathBindable[Option[AccessCode]] =
    new PathBindable[Option[AccessCode]] {
      override def bind(key: String, value: String): Either[String, Option[AccessCode]] =
        stringBinder.bind(key, value).right.flatMap(parseString[String]).right.map {
          case "-" => None
          case a   => Some(AccessCode(a))
        }

      override def unbind(key: String, maybeAccessCode: Option[AccessCode]): String =
        maybeAccessCode.fold("-")(a => a.value)
    }

  implicit val formIdQueryBinder: QueryStringBindable[FormId] = valueClassQueryBinder(_.value)

  implicit val sectionNumberQueryBinder: QueryStringBindable[SectionNumber] = new QueryStringBindable[SectionNumber] {

    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, SectionNumber]] =
      params.get(key).flatMap(_.headOption).map { value =>
        Try { SectionNumber(value.toInt) }
          .map(_.asRight)
          .getOrElse(s"No valid value in path $key: $value".asLeft)
      }

    override def unbind(key: String, sectionNumber: SectionNumber): String =
      s"""$key=${sectionNumber.value.toString}"""
  }

  implicit val suppressErrorsQueryBinder: QueryStringBindable[SuppressErrors] =
    new QueryStringBindable[SuppressErrors] {

      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, SuppressErrors]] =
        params.get(key).flatMap(_.headOption).map { value =>
          value match {
            case SuppressErrors.seYes => SuppressErrors.Yes.asRight
            case SuppressErrors.seNo  => SuppressErrors.No.asRight
            case _                    => s"No valid value in path $key: $value".asLeft
          }
        }

      override def unbind(key: String, suppressErrors: SuppressErrors): String =
        s"""$key=${suppressErrors.asString}"""
    }

  implicit val faseForwardQueryBinder: QueryStringBindable[FastForward] =
    new QueryStringBindable[FastForward] {

      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, FastForward]] =
        params.get(key).flatMap(_.headOption).map { value =>
          value match {
            case FastForward.ffYes => FastForward.Yes.asRight
            case maybeSectionNumber =>
              Try(maybeSectionNumber.toInt).toOption
                .fold[Either[String, FastForward]](s"No valid value in path $key: $value".asLeft)(sn =>
                  FastForward.StopAt(SectionNumber(sn)).asRight)
          }
        }

      override def unbind(key: String, fastForward: FastForward): String =
        s"""$key=${fastForward.asString}"""
    }

  implicit val optionAccessCodeBinder: QueryStringBindable[Option[AccessCode]] =
    new QueryStringBindable[Option[AccessCode]] {
      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, Option[AccessCode]]] =
        if (params.contains(key))
          params.get(key).flatMap(_.headOption).map(value => Right(Some(AccessCode(value))))
        else Some(Right(None))

      override def unbind(key: String, maybeAccessCode: Option[AccessCode]): String =
        maybeAccessCode match {
          case Some(a) => s"""$key=${a.value}"""
          case None    => ""
        }
    }

  private def valueClassQueryBinder[A: Reads](fromAtoString: A => String)(
    implicit stringBinder: QueryStringBindable[String]) =
    new QueryStringBindable[A] {
      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, A]] =
        stringBinder.bind(key, params).map(_.flatMap(parseString[A]))

      override def unbind(key: String, a: A): String =
        stringBinder.unbind(key, fromAtoString(a))
    }

  private def parseString[A: Reads](str: String) =
    JsString(str).validate[A] match {
      case JsSuccess(a, _) => Right(a)
      case JsError(_)      => Left("No valid value in url binding: " + str)
    }

  private def valueClassBinder[A: Reads](fromAtoString: A => String)(implicit stringBinder: PathBindable[String]) =
    mkPathBindable(parseString[A], fromAtoString)

  private def mkPathBindable[A](fromStringToA: String => Either[String, A], fromAtoString: A => String)(
    implicit stringBinder: PathBindable[String]) =
    new PathBindable[A] {
      override def bind(key: String, value: String): Either[String, A] =
        stringBinder.bind(key, value).flatMap(fromStringToA)

      override def unbind(key: String, a: A): String =
        stringBinder.unbind(key, fromAtoString(a))
    }

}
