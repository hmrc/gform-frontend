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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.Eq
import cats.implicits._
import play.api.libs.json._
import scala.util.Try
import uk.gov.hmrc.gform.models.{ FormModel, Visibility }
import SectionNumber.Classic.AddToListPage.TerminalPageKind

sealed trait SectionNumber extends Ordered[SectionNumber] with Product with Serializable {

  def isAddToList: Boolean = this match {
    case _: SectionNumber.Classic.FixedPage                  => false
    case _: SectionNumber.Classic.NormalPage                 => false
    case _: SectionNumber.Classic.RepeatedPage               => false
    case _: SectionNumber.Classic.AddToListPage.DefaultPage  => true
    case _: SectionNumber.Classic.AddToListPage.Page         => true
    case _: SectionNumber.Classic.AddToListPage.TerminalPage => true
    case SectionNumber.TaskList(_, sectionNumber)            => sectionNumber.isAddToList
  }

  def isAddToListTerminalSpecific(pageKind: TerminalPageKind): Boolean = this match {
    case SectionNumber.Classic.AddToListPage.TerminalPage(_, _, `pageKind`) => true
    case SectionNumber.TaskList(_, sectionNumber)                           => sectionNumber.isAddToListTerminalSpecific(pageKind)
    case _                                                                  => false
  }

  def isAddToListRepeaterPage: Boolean = isAddToListTerminalSpecific(TerminalPageKind.RepeaterPage)

  def isAddToListDeclarationPage: Boolean = isAddToListTerminalSpecific(TerminalPageKind.DeclarationPage)

  def isAddToListTerminalPage: Boolean = this match {
    case SectionNumber.Classic.AddToListPage.TerminalPage(_, _, _) => true
    case SectionNumber.TaskList(_, sectionNumber)                  => sectionNumber.isAddToListTerminalPage
    case _                                                         => false
  }

  def templateSectionIndex: TemplateSectionIndex =
    fold(_.sectionIndex)(_.sectionNumber.sectionIndex)

  def fold[B](f: SectionNumber.Classic => B)(g: SectionNumber.TaskList => B): B =
    this match {
      case n: SectionNumber.Classic  => f(n)
      case r: SectionNumber.TaskList => g(r)
    }

  def isTaskList: Boolean = fold(_ => false)(_ => true)

  override def compare(that: SectionNumber): Int = (this, that) match {
    case (a: SectionNumber.Classic, b: SectionNumber.Classic) => a.compareClassic(b)
    case (SectionNumber.TaskList(Coordinates(tsn0, tn0), sn0), SectionNumber.TaskList(Coordinates(tsn1, tn1), sn1)) =>
      if (tsn0 === tsn1 && tn0 === tn1) sn0.compare(sn1)
      else if (tsn0 === tsn1) tn0.compare(tn1)
      else tsn0.compare(tsn1)
    case (l, r) => throw new Exception(s"Cannot compare SectionNumber: $l with $r")
  }

  def maybeCoordinates: Option[Coordinates] =
    fold(_ => Option.empty[Coordinates])(tl => Some(tl.coordinates))

  def toCoordinatesUnsafe: Coordinates =
    fold[Coordinates](_ => throw new Exception(s"Cannot convert $this to coordinates"))(_.coordinates)

  def contains(coordinates: Coordinates): Boolean = maybeCoordinates.fold(false)(_ === coordinates)

  def increment(formModel: FormModel[Visibility]): SectionNumber = {
    val as = formModel.availableSectionNumbers
    val indexOfThis: Int = as.indexOf(this)
    val nextIndex = indexOfThis + 1
    if (as.size === nextIndex) {
      this
    } else {
      formModel.availableSectionNumbers(nextIndex)
    }
  }

  def unsafeToClassic: SectionNumber.Classic =
    fold(identity)(taskList => throw new Exception("unsafeToClassic invoked on TaskList type: " + taskList))
  def unsafeToTaskList: SectionNumber.TaskList =
    fold(classic => throw new Exception("unsafeToTaskList invoked on Classic type: " + classic))(identity)

  def value: String = this match {
    case SectionNumber.Classic.FixedPage(TemplateSectionIndex(sectionIndex))  => "fixed-page"
    case SectionNumber.Classic.NormalPage(TemplateSectionIndex(sectionIndex)) => "n" + sectionIndex.toString
    case SectionNumber.Classic.AddToListPage.DefaultPage(TemplateSectionIndex(sectionIndex)) =>
      "ad" + sectionIndex.toString
    case SectionNumber.Classic.AddToListPage.Page(TemplateSectionIndex(sectionIndex), iterationNumber, pageNumber) =>
      "ap" + sectionIndex.toString + "." + iterationNumber.toString + "." + pageNumber.toString
    case SectionNumber.Classic.AddToListPage
          .TerminalPage(TemplateSectionIndex(sectionIndex), iterationNumber, terminalPage) =>
      (terminalPage match {
        case TerminalPageKind.CyaPage         => "ac"
        case TerminalPageKind.DeclarationPage => "as"
        case TerminalPageKind.RepeaterPage    => "ar"
      }) + sectionIndex.toString + "." + iterationNumber.toString
    case SectionNumber.Classic.RepeatedPage(TemplateSectionIndex(sectionIndex), pageNumber) =>
      "r" + sectionIndex.toString + "." + pageNumber.toString
    case SectionNumber.TaskList(Coordinates(taskSectionNumber, taskNumber), sectionNumber) =>
      List(taskSectionNumber.value.toString, taskNumber.value.toString, sectionNumber.value).mkString(",")
  }
}

object SectionNumber {

  sealed trait Classic extends SectionNumber with Product with Serializable {
    def sectionIndex: TemplateSectionIndex

    def compareClassic(that: Classic): Int = (this, that) match {
      case (Classic.NormalPage(sn0), Classic.NormalPage(sn1)) => sn0.index.compare(sn1.index)
      case (Classic.NormalPage(sn0), Classic.AddToListPage.DefaultPage(sn1)) if sn0 =!= sn1 =>
        sn0.index.compare(sn1.index)
      case (Classic.NormalPage(sn0), Classic.AddToListPage.Page(sn1, _, _)) if sn0 =!= sn1 =>
        sn0.index.compare(sn1.index)
      case (Classic.NormalPage(sn0), Classic.AddToListPage.TerminalPage(sn1, _, _)) if sn0 =!= sn1 =>
        sn0.index.compare(sn1.index)
      case (Classic.NormalPage(sn0), Classic.RepeatedPage(sn1, _)) if sn0 =!= sn1 => sn0.index.compare(sn1.index)
      case (Classic.AddToListPage.DefaultPage(sn0), Classic.NormalPage(sn1)) if sn0 =!= sn1 =>
        sn0.index.compare(sn1.index)
      case (Classic.AddToListPage.Page(sn0, _, _), Classic.NormalPage(sn1)) if sn0 =!= sn1 =>
        sn0.index.compare(sn1.index)
      case (Classic.AddToListPage.TerminalPage(sn0, _, _), Classic.NormalPage(sn1)) if sn0 =!= sn1 =>
        sn0.index.compare(sn1.index)
      case (Classic.RepeatedPage(sn0, _), Classic.NormalPage(sn1)) if sn0 =!= sn1 => sn0.index.compare(sn1.index)
      case (Classic.AddToListPage.DefaultPage(sn0), Classic.RepeatedPage(sn1, _)) if sn0 =!= sn1 =>
        sn0.index.compare(sn1.index)
      case (Classic.AddToListPage.Page(sn0, _, _), Classic.RepeatedPage(sn1, _)) if sn0 =!= sn1 =>
        sn0.index.compare(sn1.index)
      case (Classic.AddToListPage.TerminalPage(sn0, _, _), Classic.RepeatedPage(sn1, _)) if sn0 =!= sn1 =>
        sn0.index.compare(sn1.index)
      case (Classic.RepeatedPage(sn0, _), Classic.AddToListPage.DefaultPage(sn1)) if sn0 =!= sn1 =>
        sn0.index.compare(sn1.index)
      case (Classic.RepeatedPage(sn0, _), Classic.AddToListPage.Page(sn1, _, _)) if sn0 =!= sn1 =>
        sn0.index.compare(sn1.index)
      case (Classic.RepeatedPage(sn0, _), Classic.AddToListPage.TerminalPage(sn1, _, _)) if sn0 =!= sn1 =>
        sn0.index.compare(sn1.index)
      case (a: Classic.AddToListPage, b: Classic.AddToListPage) => a.compareAddToListPage(b)
      case (Classic.RepeatedPage(sn0, pn0), Classic.RepeatedPage(sn1, pn1)) =>
        if (sn0 === sn1) pn0.compare(pn1)
        else sn0.index.compare(sn1.index)
      case (l, r) => throw new Exception(s"SectionNumber $l cannot have same sectionIndex as $r")
    }
  }
  object Classic {
    case class FixedPage(sectionIndex: TemplateSectionIndex) extends Classic
    case class NormalPage(sectionIndex: TemplateSectionIndex) extends Classic
    case class RepeatedPage(sectionIndex: TemplateSectionIndex, pageNumber: Int) extends Classic
    sealed trait AddToListPage extends Classic {
      def compareAddToListPage(that: AddToListPage): Int = (this, that) match {
        // format: off
        case (AddToListPage.DefaultPage(sn0), AddToListPage.DefaultPage(sn1))                     => compDef(0, sn0, sn1)
        case (AddToListPage.DefaultPage(sn0), AddToListPage.Page(sn1, _, _))                      => compDef(-1, sn0, sn1)
        case (AddToListPage.Page(sn0, _, _), AddToListPage.DefaultPage(sn1))                      => compDef(1, sn0, sn1)
        case (AddToListPage.DefaultPage(sn0), AddToListPage.TerminalPage(sn1, _, _))              => compDef(-1, sn0, sn1)
        case (AddToListPage.TerminalPage(sn0, _, _), AddToListPage.DefaultPage(sn1))              => compDef(1, sn0, sn1)
        case (AddToListPage.TerminalPage(sn0, in0, _), AddToListPage.Page(sn1, in1, _))           => comp(1, sn0, in0, sn1, in1)
        case (AddToListPage.Page(sn0, in0, _), AddToListPage.TerminalPage(sn1, in1, _))           => comp(-1, sn0, in0, sn1, in1)
        case (AddToListPage.TerminalPage(sn0, in0, t0), AddToListPage.TerminalPage(sn1, in1, t1)) => comp(t0.compare(t1), sn0, in0, sn1, in1)
        case (AddToListPage.Page(sn0, in0, pn0), AddToListPage.Page(sn1, in1, pn1))               => comp(pn0.compare(pn1), sn0, in0, sn1, in1)
        // format: on
      }

      private def compDef(default: Int, sn0: TemplateSectionIndex, sn1: TemplateSectionIndex): Int =
        if (sn0 === sn1) default
        else sn0.index.compare(sn1.index)

      private def comp(default: Int, sn0: TemplateSectionIndex, in0: Int, sn1: TemplateSectionIndex, in1: Int): Int =
        if (sn0 === sn1 && in0 === in1) default
        else if (sn0 === sn1) in0.compare(in1)
        else sn0.index.compare(sn1.index)
    }

    object AddToListPage {
      sealed trait TerminalPageKind extends Ordered[TerminalPageKind] {
        val sortOrder: Int
        def compare(other: TerminalPageKind): Int = this.sortOrder.compare(other.sortOrder)
      }

      case object TerminalPageKind {
        case object CyaPage extends TerminalPageKind {
          override val sortOrder: Int = 0
        }
        case object DeclarationPage extends TerminalPageKind {
          override val sortOrder: Int = 1
        }
        case object RepeaterPage extends TerminalPageKind {
          override val sortOrder: Int = 2
        }
      }
      case class DefaultPage(sectionIndex: TemplateSectionIndex) extends AddToListPage
      case class Page(sectionIndex: TemplateSectionIndex, iterationNumber: Int, pageNumber: Int) extends AddToListPage
      case class TerminalPage(
        sectionIndex: TemplateSectionIndex,
        iterationNumber: Int,
        terminalKind: TerminalPageKind
      ) extends AddToListPage
    }
    implicit val equal: Eq[SectionNumber.Classic] = Eq.fromUniversalEquals
  }

  final case class TaskList(
    coordinates: Coordinates,
    sectionNumber: Classic
  ) extends SectionNumber

  object TaskList {
    implicit val equal: Eq[SectionNumber.TaskList] = Eq.fromUniversalEquals
  }

  val classicZero = Classic.NormalPage(TemplateSectionIndex(0))
  val classicFixed = Classic.FixedPage(TemplateSectionIndex(0))

  val taskListZero = TaskList(Coordinates(TaskSectionNumber(0), TaskNumber(0)), classicZero)

  implicit val equal: Eq[SectionNumber] = Eq.fromUniversalEquals
  implicit val format: Format[SectionNumber.Classic] = Format[SectionNumber.Classic](
    Reads[SectionNumber.Classic] {
      case JsString(value) =>
        parseClassic(value) match {
          case Some(sectionNumber) => JsSuccess(sectionNumber)
          case None                => JsError(s"Invalid section number: $value")
        }
      case unknown => JsError(s"JsString value expected, got: $unknown")
    },
    Writes[SectionNumber.Classic](a => JsString(a.value))
  )

  implicit val writer: Writes[SectionNumber] = Writes[SectionNumber](a => JsString(a.value))

  // format: off
  private val NormalPageRegex                  = "^n(\\d+)$".r
  private val AddToListDefaultPageRegex        = "^ad(\\d+)$".r
  private val AddToListPageRegex               = "^ap(\\d+)\\.(\\d+)\\.(\\d+)$".r
  private val AddToListCyaPageRegex            = "^ac(\\d+)\\.(\\d+)$".r
  private val AddToListRepeaterPageRegex       = "^ar(\\d+)\\.(\\d+)$".r
  private val AddToListDeclarationSectionRegex = "^as(\\d+)\\.(\\d+)$".r
  private val RepeatedPageRegex                = "^r(\\d+)\\.(\\d+)$".r
  // format: on

  def parseClassic(string: String): Option[SectionNumber.Classic] =
    string match {
      case NormalPageRegex(sectionIndex) =>
        Some(SectionNumber.Classic.NormalPage(TemplateSectionIndex(sectionIndex.toInt)))
      case AddToListDefaultPageRegex(sectionIndex) =>
        Some(SectionNumber.Classic.AddToListPage.DefaultPage(TemplateSectionIndex(sectionIndex.toInt)))
      case AddToListPageRegex(sectionIndex, iterationNumber, pageNumber) =>
        Some(
          SectionNumber.Classic.AddToListPage
            .Page(TemplateSectionIndex(sectionIndex.toInt), iterationNumber.toInt, pageNumber.toInt)
        )
      case AddToListCyaPageRegex(sectionIndex, iterationNumber) =>
        Some(mkAtlTerminalPage(sectionIndex, iterationNumber, TerminalPageKind.CyaPage))
      case AddToListDeclarationSectionRegex(sectionIndex, iterationNumber) =>
        Some(mkAtlTerminalPage(sectionIndex, iterationNumber, TerminalPageKind.DeclarationPage))
      case AddToListRepeaterPageRegex(sectionIndex, iterationNumber) =>
        Some(mkAtlTerminalPage(sectionIndex, iterationNumber, TerminalPageKind.RepeaterPage))
      case RepeatedPageRegex(sectionIndex, pageNumber) =>
        Some(SectionNumber.Classic.RepeatedPage(TemplateSectionIndex(sectionIndex.toInt), pageNumber.toInt))
      case _ => None
    }

  private def mkAtlTerminalPage(
    sectionIndex: String,
    iterationNumber: String,
    kind: TerminalPageKind
  ): SectionNumber.Classic =
    SectionNumber.Classic.AddToListPage
      .TerminalPage(TemplateSectionIndex(sectionIndex.toInt), iterationNumber.toInt, kind)

  def parse(string: String): Option[SectionNumber] =
    if (string.contains(",")) {
      string.split(",").toList match {
        case maybeTaskSectionNumber :: maybeTaskNumber :: sectionNumber :: Nil =>
          (
            Try(maybeTaskSectionNumber.toInt).toOption,
            Try(maybeTaskNumber.toInt).toOption,
            parseClassic(sectionNumber)
          ) match {
            case (Some(taskSectionNumber), Some(taskNumber), Some(sn: SectionNumber.Classic)) =>
              Some(
                SectionNumber
                  .TaskList(
                    Coordinates(TaskSectionNumber(taskSectionNumber), TaskNumber(taskNumber)),
                    sn
                  )
              )
            case _ => None
          }
        case _ => None
      }
    } else {
      parseClassic(string)
    }
}
