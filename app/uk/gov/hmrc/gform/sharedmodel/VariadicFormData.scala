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

import cats.instances.list._
import cats.syntax.eq._
import cats.syntax.foldable._
import cats.{ Monoid, Show }
import cats.syntax.show._
import uk.gov.hmrc.gform.models.{ DependencyGraphVerification, FormModel, PageMode, PageModel }
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormField }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.models.ids.IndexedComponentId
import uk.gov.hmrc.gform.models.Atom

import scala.collection.{ immutable, mutable }

sealed trait VariadicValue extends Product with Serializable {
  def toSeq: Seq[String] = this match {
    case VariadicValue.One(v)   => Seq(v)
    case VariadicValue.Many(vs) => vs
  }

  def isEmpty: Boolean = this match {
    case VariadicValue.One(v)   => v.isEmpty
    case VariadicValue.Many(vs) => vs.forall(_.isEmpty)
  }

  def toSet: Set[String] = toSeq.toSet

  def exists(pred: String => Boolean): Boolean = toSeq.exists(pred)

  def contains(s: String): Boolean = toSeq.contains(s)

  def map(f: String => String): VariadicValue = this match {
    case VariadicValue.One(v)   => VariadicValue.One(f(v))
    case VariadicValue.Many(vs) => VariadicValue.Many(vs.map(f))
  }

  def fold[B](f: VariadicValue.One => B)(g: VariadicValue.Many => B): B = this match {
    case v: VariadicValue.One  => f(v)
    case v: VariadicValue.Many => g(v)
  }
}

object VariadicValue {
  case class One(value: String) extends VariadicValue
  case class Many(value: Seq[String]) extends VariadicValue

  implicit val show: Show[VariadicValue] = new Show[VariadicValue] {
    private def quote(s: String) = s""""$s""""
    override def show(t: VariadicValue): String = t match {
      case One(v)   => quote(v)
      case Many(vs) => "[" + vs.map(quote).mkString(", ") + "]"
    }
  }

  def isVariadic(componentType: ComponentType): Boolean = componentType match {
    case _: Choice          => true
    case _: RevealingChoice => true
    case _                  => false
  }
}

sealed trait SourceOrigin extends Product with Serializable

object SourceOrigin {
  trait OutOfDate extends SourceOrigin
  trait Current extends SourceOrigin

  def changeSource[M[_ <: SourceOrigin]](in: M[OutOfDate]): M[Current] = in.asInstanceOf[M[Current]]
  def changeSourceToOutOfDate[M[_ <: SourceOrigin]](in: M[Current]): M[OutOfDate] = in.asInstanceOf[M[OutOfDate]]
}

case class VariadicFormData[S <: SourceOrigin](data: collection.Map[ModelComponentId, VariadicValue]) {

  def get(id: ModelComponentId): Option[VariadicValue] = data.get(id)

  def filesOfMultiFileComponent(id: ModelComponentId): List[(FileComponentId.Multi, VariadicValue.One)] =
    data
      .filter { case (modelComponentId, _) =>
        val fileComponentId = FileComponentId.fromString(modelComponentId.toMongoIdentifier)
        fileComponentId.isMultiFor(id.toFormComponentId)
      }
      .collect { case (modelComponentId, one @ VariadicValue.One(_)) =>
        val fileComponentId = FileComponentId.fromString(modelComponentId.toMongoIdentifier)
        fileComponentId -> one
      }
      .collect { case (m @ FileComponentId.Multi(_, _), variadicValue) => m -> variadicValue }
      .toList
      .sortBy { case (FileComponentId.Multi(_, index), _) => index }

  def by(formComponent: FormComponent): VariadicFormData[S] = {
    val dataList: List[(ModelComponentId, VariadicValue)] =
      formComponent.multiValueId.toModelComponentIds
        .flatMap { modelComponentId =>
          data
            .get(modelComponentId)
            .map(modelComponentId -> _)
        }
    val childrenData = formComponent.childrenFormComponents.foldMap(by)
    VariadicFormData[S](dataList.toMap) ++ childrenData
  }

  def forBaseComponentId(baseComponentId: BaseComponentId): Iterable[(ModelComponentId, VariadicValue)] =
    collect {
      case (modelComponentId, value) if modelComponentId.baseComponentId === baseComponentId =>
        modelComponentId -> value
    }

  def withSyntheticCopy(
    baseComponentId: BaseComponentId,
    mapper: IndexedComponentId => IndexedComponentId
  ): VariadicFormData[S] = {
    val copy = forBaseComponentId(baseComponentId).map { case (modelComponentId, variadicValue) =>
      modelComponentId.map(mapper) -> variadicValue
    }
    VariadicFormData[S](data ++ copy)
  }

  def withCopyFromAtom(
    modelComponentId: ModelComponentId,
    atomMap: Map[String, String]
  ): VariadicFormData[S] = {
    val copy =
      (modelComponentId, VariadicValue.One("")) ::
        (modelComponentId match {
          case ModelComponentId.Pure(indexedCompId) =>
            atomMap.map { case (atom, value) =>
              (ModelComponentId.Atomic(indexedCompId, Atom(atom)), VariadicValue.One(value))
            }
          case _ => List()
        }).toList
    VariadicFormData[S](data ++ copy)
  }

  def forBaseComponentIdLessThenEqual(modelComponentId: ModelComponentId): Iterable[(ModelComponentId, VariadicValue)] =
    modelComponentId.indexedComponentId.fold(pure => forBaseComponentId(pure.baseComponentId)) { indexed =>
      collect {
        case (mcId, value)
            if mcId.baseComponentId === indexed.baseComponentId &&
              mcId.maybeIndex.exists(_ <= indexed.index) =>
          mcId -> value
      }
    }

  def forBaseComponentIdLessThen(modelComponentId: ModelComponentId): Iterable[(ModelComponentId, VariadicValue)] =
    modelComponentId.indexedComponentId.fold { pure =>
      forBaseComponentId(pure.baseComponentId)
    } { indexed =>
      collect {
        case (mcId, value)
            if mcId.baseComponentId === indexed.baseComponentId &&
              mcId.maybeIndex.exists(_ < indexed.index) =>
          mcId -> value
      }
    }

  def distinctIndexedComponentIds(modelComponentId: ModelComponentId): List[IndexedComponentId] =
    forBaseComponentId(modelComponentId.indexedComponentId.baseComponentId)
      .map { case (modelComponentId, _) =>
        modelComponentId
          .fold(p => p.indexedComponentId)(i => i.indexedComponentId)
      }
      .toList
      .distinct
      .sortBy(_.maybeIndex)

  def distinctIndexedModelIds(modelComponentId: ModelComponentId): List[ModelComponentId] =
    forBaseComponentId(modelComponentId.indexedComponentId.baseComponentId)
      .map(_._1)
      .filter(_.indexedComponentId.isIndexed)
      .toList
      .distinct
      .sortBy(_.indexedComponentId.maybeIndex)

  def keySet(): collection.Set[ModelComponentId] = data.keySet

  def ++[R <: SourceOrigin](addend: VariadicFormData[R]): VariadicFormData[R] = VariadicFormData[R](data ++ addend.data)
  def addValue(entry: (ModelComponentId, VariadicValue)): VariadicFormData[S] = fold { mutableMap =>
    mutableMap.addOne(entry)
  } { immutableMap =>
    immutableMap + entry
  }
  def addOne(entry: (ModelComponentId, String)): VariadicFormData[S] =
    this addValue (entry._1 -> VariadicValue.One(entry._2))
  def addMany(entry: (ModelComponentId, Seq[String])): VariadicFormData[S] =
    this addValue (entry._1 -> VariadicValue.Many(entry._2))

  def -(remove: ModelComponentId): VariadicFormData[S] = --(Set(remove))

  def --(remove: VariadicFormData[S]): VariadicFormData[S] = --(remove.keySet())

  def --(formComponents: IterableOnce[ModelComponentId]): VariadicFormData[S] =
    fold { mutableMap =>
      formComponents.iterator.foreach(x => mutableMap.remove(x))
    } { immutableMap =>
      immutableMap -- formComponents
    }
  private def fold(f: mutable.Map[ModelComponentId, VariadicValue] => Unit)(
    g: immutable.Map[ModelComponentId, VariadicValue] => immutable.Map[ModelComponentId, VariadicValue]
  ): VariadicFormData[S] = data match {
    case map: mutable.Map[ModelComponentId, VariadicValue] =>
      f(map)
      this
    case map: immutable.Map[ModelComponentId, VariadicValue] => VariadicFormData[S](g(map))
    case _                                                   => throw new RuntimeException("Unknown map type")
  }

  def subset(ids: Set[ModelComponentId]): VariadicFormData[S] =
    VariadicFormData[S](data.filter { case (k, _) => ids.contains(k) })

  def collect[B](pf: PartialFunction[(ModelComponentId, VariadicValue), B]): Iterable[B] = data.collect(pf)

  def contains(id: ModelComponentId): Boolean = data.contains(id)

  def mapKeys(f: ModelComponentId => ModelComponentId): VariadicFormData[S] =
    VariadicFormData[S](data.map { case (k, v) =>
      (f(k), v)
    })

  def mapValues(f: (ModelComponentId, VariadicValue) => VariadicValue): VariadicFormData[S] =
    VariadicFormData[S](data.map { case (k, v) =>
      (k, f(k, v))
    })

  def one(id: ModelComponentId): Option[String] =
    get(id)
      .map {
        case VariadicValue.One(v) => v
        case notOne =>
          throw new IllegalArgumentException(
            show"""Expected VariadicValue.One for form component ID "$id". Got $notOne"""
          )
      }

  def oneOrElse(id: ModelComponentId, dflt: => String): String = one(id).getOrElse(dflt)

  def many(id: ModelComponentId): Option[Seq[String]] =
    get(id)
      .map {
        case VariadicValue.Many(vs) => vs
        case notMany =>
          throw new IllegalArgumentException(
            show"""Expected VariadicValue.Many for form component ID "$id". Got $notMany"""
          )
      }

  def toFormField(modelComponentId: ModelComponentId): FormField = {
    val value = data.get(modelComponentId).fold("") {
      case VariadicValue.One(one)   => one
      case VariadicValue.Many(many) => many.mkString(",")
    }
    FormField(modelComponentId, value)
  }

  def toFormData: FormData = FormData {
    data.toList.map {
      case (variadicValueId, VariadicValue.One(one))   => FormField(variadicValueId, one)
      case (variadicValueId, VariadicValue.Many(many)) => FormField(variadicValueId, many.mkString(","))
    }
  }
}

object VariadicFormData {
  def empty[S <: SourceOrigin]: VariadicFormData[S] = VariadicFormData(Map.empty)

  def create[S <: SourceOrigin](idAndValue: (ModelComponentId, VariadicValue)*): VariadicFormData[S] =
    VariadicFormData[S](idAndValue.toMap)

  def one[S <: SourceOrigin](formComponentId: ModelComponentId, value: String): VariadicFormData[S] =
    VariadicFormData[S](Map(formComponentId -> VariadicValue.One(value)))

  def ones[S <: SourceOrigin](idAndValue: (ModelComponentId, String)*): VariadicFormData[S] =
    idAndValue.toList.foldMap { case (id, value) => one(id, value) }

  def many[S <: SourceOrigin](formComponentId: ModelComponentId, value: Seq[String]): VariadicFormData[S] =
    VariadicFormData[S](Map(formComponentId -> VariadicValue.Many(value)))

  def manys[S <: SourceOrigin](idAndValue: (ModelComponentId, Seq[String])*): VariadicFormData[S] =
    idAndValue.toList.foldMap { case (id, value) => many(id, value) }

  implicit def monoid[S <: SourceOrigin]: Monoid[VariadicFormData[S]] = new Monoid[VariadicFormData[S]] {
    override def empty: VariadicFormData[S] = VariadicFormData.empty

    override def combine(x: VariadicFormData[S], y: VariadicFormData[S]): VariadicFormData[S] = x ++ y
  }

  // The VariadicFormData[S] instance returned contains ALL fields in the data map, even if
  // there is no corresponding ModelComponentId in the given set of form components Ids.
  // The only use of formComponentsIds set is to determine which branch of VariadicValue each ModelComponentId should use,
  // with the assumption that a value of any ModelComponentId found in the data map that is not
  // in the formComponentIds set should be represented by a VariadicValue.One value.
  def buildFromMongoData[S <: SourceOrigin](
    formModel: FormModel[DependencyGraphVerification],
    data: Map[ModelComponentId, String]
  ): VariadicFormData[S] = {
    // We do not know fully expanded form model, so we must descent to VariadicValueId level to
    // properly classify data to VariadicValues
    val multiValueIds: Set[BaseComponentId] = formModel.allMultiSelectionIds.map(_.baseComponentId)

    VariadicFormData[S](
      data.map { case (id, s) =>
        if (multiValueIds(id.baseComponentId))
          (id, VariadicValue.Many(s.split(",").map(_.trim).filterNot(_.isEmpty).toSeq))
        else (id, VariadicValue.One(s))
      }
    )
  }

  def listVariadicFormComponentIds[A <: PageMode](page: Page[A]): Set[ModelComponentId] =
    page.allFields.flatMap(listVariadicFormComponentIds).toSet

  def listVariadicFormComponentIds[A <: PageMode](pageModel: PageModel[A]): Set[ModelComponentId] =
    pageModel.allFormComponents.flatMap(listVariadicFormComponentIds).toSet

  def listVariadicFormComponentIds(component: FormComponent): Set[ModelComponentId] =
    component.`type` match {
      case g: Group  => listVariadicFormComponentIds(g.fields)
      case _: Choice => Set(component.modelComponentId)
      case r: RevealingChoice =>
        listVariadicFormComponentIds(r.options.flatMap(_.revealingFields)) + component.modelComponentId
      case _: Text | _: TextArea | _: Date | _: CalendarDate.type | _: TaxPeriodDate.type | _: Address |
          _: OverseasAddress | _: HmrcTaxPeriod | _: InformationMessage | _: FileUpload | _: MultiFileUpload | _: Time |
          _: MiniSummaryList | _: PostcodeLookup | _: TableComp | _: Button =>
        Set.empty
    }

  def listVariadicFormComponentIds(components: List[FormComponent]): Set[ModelComponentId] =
    components.flatMap(listVariadicFormComponentIds).toSet
}
