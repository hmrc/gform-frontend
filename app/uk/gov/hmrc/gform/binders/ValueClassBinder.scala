/*
 * Copyright 2018 HM Revenue & Customs
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
import play.api.mvc.{ PathBindable, QueryStringBindable }
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, FormTemplateId4Ga, SectionNumber, SectionTitle4Ga }

import scala.util.Try
object ValueClassBinder {

  //You need to name it somethingBinder, or else play can't find them
  implicit val formTemplateIdBinder: PathBindable[FormTemplateId] = valueClassBinder(_.value)
  implicit val formTemplateId4GaBinder: PathBindable[FormTemplateId4Ga] = valueClassBinder(_.value)
  implicit val formIdBinder: PathBindable[FormId] = valueClassBinder(_.value)
  implicit val fileIdBinder: PathBindable[FileId] = valueClassBinder(_.value)
  implicit val sectionTitle4GaBinder: PathBindable[SectionTitle4Ga] = valueClassBinder(_.value)
  implicit val sectionNumberBinder: PathBindable[SectionNumber] = new PathBindable[SectionNumber] {
    override def bind(key: String, value: String): Either[String, SectionNumber] =
      Try {
        SectionNumber(value.toInt)
      }.map(_.asRight).getOrElse(s"No valid value in path $key: $value".asLeft)

    override def unbind(key: String, sectionNumber: SectionNumber): String = sectionNumber.value.toString
  }
  implicit val userIdBinder: PathBindable[UserId] = valueClassBinder(_.value)

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
  private def parseString[A: Reads](str: String) =
    JsString(str).validate[A] match {
      case JsSuccess(a, _) => Right(a)
      case JsError(_)      => Left("No valid value in url binding: " + str)
    }

  def valueClassBinder[A: Reads](fromAtoString: A => String)(implicit stringBinder: PathBindable[String]) =
    new PathBindable[A] {
      override def bind(key: String, value: String): Either[String, A] =
        stringBinder.bind(key, value).right.flatMap(parseString[A])

      override def unbind(key: String, a: A): String =
        stringBinder.unbind(key, fromAtoString(a))
    }

  def valueClassQueryBinder[A: Reads](fromAtoString: A => String)(implicit stringBinder: QueryStringBindable[String]) =
    new QueryStringBindable[A] {
      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, A]] =
        stringBinder.bind(key, params).map(_.right.flatMap(parseString[A]))

      override def unbind(key: String, a: A): String =
        stringBinder.unbind(key, fromAtoString(a))
    }
}
