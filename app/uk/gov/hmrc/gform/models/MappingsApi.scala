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

package uk.gov.hmrc.gform.models

import play.api.data.validation.Constraint
import play.api.data.{ FormError, Mapping }

object MappingsApi {
  implicit class MappingOps[T](val m: Mapping[T]) {
    def is(value: T) = new ConditionIs[T](m, value)
    def onlyWhen(condition: Condition) = OnlyWhen(m, condition)
  }

  implicit class MappingWithKeyOps[T](val v: (String, Mapping[T])) {
    def is(value: T) = new ConditionIs[T](v._2 withPrefix v._1, value)
  }

}

trait Condition {
  def eval(data: Map[String, String]): Boolean
  def withPrefix(prefix: String): Condition
}

class ConditionIs[T](mapping: Mapping[T], value: T) extends Condition {
  override def eval(data: Map[String, String]): Boolean =
    mapping
      .bind(data)
      .fold(
        _ ⇒ false,
        _ == value
      )

  override def withPrefix(prefix: String): Condition = new ConditionIs(mapping withPrefix prefix, value)
}

case class OnlyWhen[T](wrapped: Mapping[T], condition: Condition, val constraints: Seq[Constraint[Option[T]]] = Nil)
    extends Mapping[Option[T]] {

  override val format: Option[(String, Seq[Any])] = wrapped.format

  override val key: String = wrapped.key

  override val mappings: Seq[Mapping[_]] = wrapped.mappings :+ this

  override def bind(data: Map[String, String]): Either[Seq[FormError], Option[T]] = {
    val required = condition eval data
    if (!required)
      Right(None)
    else
      wrapped bind data fold (
        errors ⇒ Left(errors),
        valid ⇒ Right(Some(valid))
      )
  }

  override def unbind(value: Option[T]): Map[String, String] = value.map(wrapped.unbind).getOrElse(Map.empty)

  override def unbindAndValidate(value: Option[T]): (Map[String, String], Seq[FormError]) = {
    val errors = collectErrors(value)
    value.map(wrapped.unbindAndValidate).map(r => r._1 -> (r._2 ++ errors)).getOrElse(Map.empty -> errors)
  }

  override def withPrefix(prefix: String): Mapping[Option[T]] =
    copy(wrapped = wrapped withPrefix prefix)

  def verifying(addConstraints: Constraint[Option[T]]*): Mapping[Option[T]] =
    this.copy(constraints = constraints ++ addConstraints.toSeq)

}
