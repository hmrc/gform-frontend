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

package uk.gov.hmrc.gform.binders

import cats.implicits._
import play.api.libs.json._
import play.api.mvc.{ JavascriptLiteral, PathBindable, QueryStringBindable }

import scala.util.{ Failure, Success }
import uk.gov.hmrc.gform.controllers.{ AddGroup, Back, Direction, EditAddToList, Exit, RemoveGroup, SaveAndContinue, SaveAndExit, SummaryContinue }
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.models.{ ExpandUtils, FastForward, LookupQuery }
import uk.gov.hmrc.gform.payment.PaymentReference
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FileComponentId
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormId, FormStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.util.Try
object ValueClassBinder {

  implicit val lookupQueryBinder: QueryStringBindable[LookupQuery] =
    new QueryStringBindable[LookupQuery] {
      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, LookupQuery]] =
        params.get(key).flatMap(_.headOption).map {
          case "" => LookupQuery.Empty.asRight
          case v  => LookupQuery.Value(v).asRight
        }

      override def unbind(key: String, lookupQuery: LookupQuery): String =
        s"""$key=${lookupQuery.asString}"""
    }
  implicit val registerBinder: PathBindable[Register] = mkPathBindable(
    lookup => Register.fromString(lookup).fold[Either[String, Register]](Left(s"Unknown lookup: $lookup"))(Right.apply),
    _.asString
  )

  implicit val formTemplateIdBinder: PathBindable[FormTemplateId] = caseInsensitive(FormTemplateId.apply, _.value)
  implicit val formIdBinder: PathBindable[FormId] = valueClassBinder(_.value)
  implicit val fileIdBinder: PathBindable[FileId] = valueClassBinder(_.value)

  implicit val accessCodeBinder: PathBindable[AccessCode] = valueClassBinder(_.value)
  implicit val envelopeIdBinder: PathBindable[EnvelopeId] = valueClassBinder(_.value)
  implicit val submissionRefBinder: PathBindable[SubmissionRef] = valueClassBinder(_.value)
  implicit val destinationIdBinder: PathBindable[DestinationId] = valueClassBinder(_.id)
  implicit val sectionTitle4GaBinder: PathBindable[SectionTitle4Ga] = valueClassBinder(_.value)
  implicit val sectionNumberBinder: PathBindable[SectionNumber] = new PathBindable[SectionNumber] {
    override def bind(key: String, value: String): Either[String, SectionNumber] = {
      val sectionNumber: Option[SectionNumber] = SectionNumber.parse(value)
      sectionNumber
        .map(_.asRight)
        .getOrElse(s"No valid value in path $key: $value".asLeft)
    }
    override def unbind(key: String, sectionNumber: SectionNumber): String = sectionNumber.value
  }

  implicit val coordinatesBinder: PathBindable[Coordinates] = new PathBindable[Coordinates] {
    override def bind(key: String, value: String): Either[String, Coordinates] = {
      val coordinates: Try[Coordinates] = Coordinates.parse(value)
      coordinates.map(_.asRight).getOrElse(s"No valid value in path $key: $value".asLeft)
    }
    override def unbind(key: String, coordinates: Coordinates): String = coordinates.value
  }
  implicit val taskNumberBinder: PathBindable[TaskNumber] = new PathBindable[TaskNumber] {
    override def bind(key: String, value: String): Either[String, TaskNumber] =
      Try(TaskNumber(value.toInt)).map(_.asRight).getOrElse(s"No valid value in path $key: $value".asLeft)
    override def unbind(key: String, taskNumber: TaskNumber): String = taskNumber.value.toString
  }
  implicit val taskSectionNumberBinder: PathBindable[TaskSectionNumber] = new PathBindable[TaskSectionNumber] {
    override def bind(key: String, value: String): Either[String, TaskSectionNumber] =
      Try(TaskSectionNumber(value.toInt)).map(_.asRight).getOrElse(s"No valid value in path $key: $value".asLeft)
    override def unbind(key: String, taskSectionNumber: TaskSectionNumber): String = taskSectionNumber.value.toString
  }

  implicit val addToListIdQueryBindable: QueryStringBindable[AddToListId] = new QueryStringBindable[AddToListId] {
    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, AddToListId]] =
      params.get(key).flatMap(_.headOption).map(fcId => Right(AddToListId(FormComponentId(fcId))))

    override def unbind(key: String, value: AddToListId): String = s"$key=${value.formComponentId.value}"
  }

  implicit val paymentReferenceQueryBindable: QueryStringBindable[PaymentReference] =
    new QueryStringBindable[PaymentReference] {
      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, PaymentReference]] =
        params.get(key).flatMap(_.headOption).map(reference => Right(PaymentReference(reference)))

      override def unbind(key: String, value: PaymentReference): String = s"$key=${value.value}"
    }

  implicit val formDirectionQueryBindable: QueryStringBindable[Direction] =
    new QueryStringBindable[Direction] {

      val AddGroupR = "AddGroup-(.*)".r.unanchored
      val RemoveGroupR = "RemoveGroup-(.*)".r.unanchored
      val EditAddToListR = "EditAddToList-(\\d*)-(.*)".r.unanchored

      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, Direction]] =
        params.get(key).flatMap(_.headOption).map {
          case "Save"            => Right(SaveAndExit)
          case "Back"            => Right(Back)
          case "SaveAndContinue" => Right(SaveAndContinue)
          case "Exit"            => Right(Exit)
          case "Continue"        => Right(uk.gov.hmrc.gform.controllers.Continue)
          case "SummaryContinue" => Right(SummaryContinue)
          case AddGroupR(x)      => Right(AddGroup(ExpandUtils.toModelComponentId(x)))
          case RemoveGroupR(x)   => Right(RemoveGroup(ExpandUtils.toModelComponentId(x)))
          case EditAddToListR(idx, fcId) =>
            Right(EditAddToList(idx.toInt, AddToListId(FormComponentId(fcId))): Direction)
          case unknown => throw new IllegalArgumentException(s"Query param $key has invalid value $unknown")
        }

      override def unbind(key: String, direction: Direction): String = {
        val value = direction match {
          case SaveAndExit                            => "Save"
          case Back                                   => "Back"
          case SaveAndContinue                        => "SaveAndContinue"
          case Exit                                   => "Exit"
          case uk.gov.hmrc.gform.controllers.Continue => "Continue"
          case SummaryContinue                        => "SummaryContinue"
          case AddGroup(modelComponentId)             => s"AddGroup-${modelComponentId.toMongoIdentifier}"
          case RemoveGroup(modelComponentId)          => s"RemoveGroup-${modelComponentId.toMongoIdentifier}"
          case EditAddToList(idx: Int, addToListId: AddToListId) =>
            s"EditAddToList-$idx-${addToListId.formComponentId.value}"
        }
        s"$key=$value"
      }
    }

  implicit val formStatusBinder: PathBindable[FormStatus] = new PathBindable[FormStatus] {
    override def bind(key: String, value: String): Either[String, FormStatus] =
      value match {
        case FormStatus(s) => s.asRight
        case _             => s"'$value' is not a valid FormStatus. Valid values are: ${FormStatus.all}".asLeft
      }

    override def unbind(key: String, value: FormStatus): String = value.toString
  }

  implicit val jLiteralAffinityGroup: JavascriptLiteral[AccessCode] = new JavascriptLiteral[AccessCode] {
    def to(value: AccessCode): String = value.value
  }

  implicit def optionAccessCodeBinder(implicit stringBinder: PathBindable[String]): PathBindable[Option[AccessCode]] =
    new PathBindable[Option[AccessCode]] {
      override def bind(key: String, value: String): Either[String, Option[AccessCode]] =
        stringBinder.bind(key, value).flatMap(parseString[String]).map {
          case "-" => None
          case a   => Some(AccessCode(a))
        }

      override def unbind(key: String, maybeAccessCode: Option[AccessCode]): String =
        maybeAccessCode.fold("-")(a => a.value)
    }

  implicit def optionSectionNumberBinder(implicit
    stringBinder: PathBindable[String]
  ): PathBindable[Option[SectionNumber]] = new PathBindable[Option[SectionNumber]] {
    override def bind(key: String, value: String): Either[String, Option[SectionNumber]] =
      stringBinder.bind(key, value).flatMap(parseString[String]).flatMap {
        case "-" => Right(None)
        case a =>
          Try(value.toInt) match {
            case Success(a)     => Right(Some(SectionNumber.Classic.NormalPage(TemplateSectionIndex(a))))
            case Failure(error) => Left(("No valid value in url binding: " + value + ". Error: " + error))
          }
      }

    override def unbind(key: String, maybeAccessCode: Option[SectionNumber]): String =
      maybeAccessCode.fold("-")(a => a.value)
  }

  implicit val formIdQueryBinder: QueryStringBindable[FormId] = valueClassQueryBinder(_.value)

  implicit val sectionNumberQueryBinder: QueryStringBindable[SectionNumber] = new QueryStringBindable[SectionNumber] {

    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, SectionNumber]] =
      params.get(key).flatMap(_.headOption).map { value =>
        SectionNumber
          .parse(value)
          .map(_.asRight)
          .getOrElse(s"No valid value in path $key: $value".asLeft)
      }

    override def unbind(key: String, sectionNumber: SectionNumber): String =
      s"""$key=${sectionNumber.value}"""
  }

  implicit val coordinatesQueryBinder: QueryStringBindable[Coordinates] = new QueryStringBindable[Coordinates] {

    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, Coordinates]] =
      params.get(key).flatMap(_.headOption).map { value =>
        Coordinates
          .parse(value)
          .map(_.asRight)
          .getOrElse(s"No valid value in path $key: $value".asLeft)
      }

    override def unbind(key: String, coordinates: Coordinates): String =
      s"""$key=${coordinates.value}"""
  }

  implicit val suppressErrorsQueryBinder: QueryStringBindable[SuppressErrors] =
    new QueryStringBindable[SuppressErrors] {

      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, SuppressErrors]] =
        params.get(key).flatMap(_.headOption).map {
          case SuppressErrors.seYes => SuppressErrors.Yes.asRight
          case SuppressErrors.seNo  => SuppressErrors.No.asRight
          case value                => s"No valid value in path $key: $value".asLeft
        }

      override def unbind(key: String, suppressErrors: SuppressErrors): String =
        s"""$key=${suppressErrors.asString}"""
    }

  implicit val fastForwardQueryBinder: QueryStringBindable[FastForward] =
    new QueryStringBindable[FastForward] {

      private def toSectionNumber(key: String, value: String): Either[String, SectionNumber] =
        SectionNumber
          .parse(value)
          .fold[Either[String, SectionNumber]](s"No valid value in path $key: $value".asLeft)(sn => sn.asRight)

      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, FastForward]] =
        params.get(key).flatMap(_.headOption).map {
          case FastForward.ffYes            => FastForward.Yes.asRight
          case FastForward.ffCYAFormSummary => FastForward.CYA(SectionOrSummary.FormSummary).asRight
          case FastForward.ffCYATaskSummary => FastForward.CYA(SectionOrSummary.TaskSummary).asRight
          case value if value.startsWith("cya:") =>
            for {
              sn2 <- toSectionNumber(key, value.drop(4))
            } yield FastForward.CYA(SectionOrSummary.Section(sn2))
          case value if value.startsWith("back:") =>
            toSectionNumber(key, value.replace("back:", "")).map(FastForward.BackUntil)
          case value => toSectionNumber(key, value).map(FastForward.StopAt)
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

  implicit val formComponentIdBinder: PathBindable[FormComponentId] = {
    implicit val reads: Reads[FormComponentId] = FormComponentId.oformat
    valueClassBinder(_.value)
  }

  implicit val fileComponentIdBinder: PathBindable[FileComponentId] = {
    implicit val reads: Reads[FileComponentId] = FileComponentId.vformat
    valueClassBinder(_.value())
  }

  implicit val baseComponentIdBinder: PathBindable[BaseComponentId] =
    mkPathBindable(
      id => BaseComponentId(id).asRight,
      _.value
    )

  private def valueClassQueryBinder[A: Reads](
    fromAtoString: A => String
  )(implicit stringBinder: QueryStringBindable[String]) =
    new QueryStringBindable[A] {
      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, A]] =
        stringBinder.bind(key, params).map(_.flatMap(parseString[A]))

      override def unbind(key: String, a: A): String =
        stringBinder.unbind(key, fromAtoString(a))
    }

  private def parseString[A: Reads](str: String) =
    JsString(str).validate[A] match {
      case JsSuccess(a, _) => Right(a)
      case JsError(error)  => Left("No valid value in url binding: " + str + ". Error: " + error)
    }

  private def valueClassBinder[A: Reads](fromAtoString: A => String)(implicit stringBinder: PathBindable[String]) =
    mkPathBindable(parseString[A], fromAtoString)

  private def mkPathBindable[A](fromStringToA: String => Either[String, A], fromAtoString: A => String)(implicit
    stringBinder: PathBindable[String]
  ) =
    new PathBindable[A] {
      override def bind(key: String, value: String): Either[String, A] =
        stringBinder.bind(key, value).flatMap(fromStringToA)

      override def unbind(key: String, a: A): String =
        stringBinder.unbind(key, fromAtoString(a))
    }

  private def caseInsensitive[A: Reads](f: String => A, g: A => String) =
    implicitly[PathBindable[String]].transform[A](s => f(s.toLowerCase), a => g(a).toLowerCase)
}
