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

package uk.gov.hmrc.gform.testonly.snapshot

import cats.implicits._
import uk.gov.hmrc.govukfrontend.views.Aliases.{ ErrorMessage, Fieldset, HtmlContent, InputItem, Legend }
import uk.gov.hmrc.govukfrontend.views.viewmodels.dateinput.DateInput
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import play.api.mvc.QueryStringBindable

case class DateTimeUserInput(
  day: Option[String],
  month: Option[String],
  year: Option[String],
  hour: Option[String],
  minute: Option[String]
)

object DateTimeUserInput {

  def apply(): DateTimeUserInput = DateTimeUserInput(None, None, None, None, None)

  def apply(prefix: String, answers: Map[String, String]): DateTimeUserInput = DateTimeUserInput(
    answers.get(s"$prefix-day"),
    answers.get(s"$prefix-month"),
    answers.get(s"$prefix-year"),
    answers.get(s"$prefix-hour"),
    answers.get(s"$prefix-minute")
  )

  def createDateInput(
    prefix: String,
    legend: String,
    dateTimeUserInput: DateTimeUserInput,
    errors: Map[String, String]
  ): DateInput = {
    val dayInput = createInputItem(s"$prefix-day", "Day", 2, dateTimeUserInput.day, errors)
    val monthInput = createInputItem(s"$prefix-month", "Month", 2, dateTimeUserInput.month, errors)
    val yearInput = createInputItem(s"$prefix-year", "Year", 4, dateTimeUserInput.year, errors)
    val hourInput = createInputItem(s"$prefix-hour", "Hour", 2, dateTimeUserInput.hour, errors)
    val minuteInput = createInputItem(s"$prefix-minute", "Minute", 2, dateTimeUserInput.minute, errors)

    val items = List(dayInput, monthInput, yearInput, hourInput, minuteInput)
    val fieldSet = Fieldset(legend = Some(Legend(content = content.Text(legend))))
    val errorMessage = createErrorMessage(errors)
    DateInput(id = prefix, items = items, errorMessage = errorMessage, fieldset = Some(fieldSet))
  }

  private def createInputItem(
    id: String,
    label: String,
    width: Int,
    userValue: Option[String],
    errors: Map[String, String]
  ): InputItem =
    InputItem(
      id = id,
      name = id,
      label = Some(label),
      value = userValue,
      classes = s"${errorClass(id, errors)} govuk-input--width-$width"
    )

  private def errorClass(id: String, errors: Map[String, String]): String =
    if (errors.contains(id)) "govuk-input--error" else ""

  private def createErrorMessage(errors: Map[String, String]): Option[ErrorMessage] =
    if (errors.isEmpty) None else Some(ErrorMessage(content = HtmlContent(errors.values.mkString("<br>"))))

  implicit def queryStringBindableDateTimeUserInput(implicit
    stringBinder: QueryStringBindable[String]
  ): QueryStringBindable[DateTimeUserInput] = new QueryStringBindable[DateTimeUserInput] {
    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, DateTimeUserInput]] =
      (for {
        d  <- stringBinder.bind(s"$key.day", params).traverse(identity)
        m  <- stringBinder.bind(s"$key.month", params).traverse(identity)
        y  <- stringBinder.bind(s"$key.year", params).traverse(identity)
        h  <- stringBinder.bind(s"$key.hour", params).traverse(identity)
        mi <- stringBinder.bind(s"$key.minute", params).traverse(identity)
      } yield (d, m, y, h, mi) match {
        case (None, None, None, None, None) => Option.empty[DateTimeUserInput]
        case _                              => Option(DateTimeUserInput(d, m, y, h, mi))
      }).traverse(identity)

    override def unbind(key: String, dateTimeUserInput: DateTimeUserInput): String =
      List(
        dateTimeUserInput.day.map(dValue => stringBinder.unbind(s"$key.day", dValue)),
        dateTimeUserInput.month.map(mValue => stringBinder.unbind(s"$key.month", mValue)),
        dateTimeUserInput.year.map(yValue => stringBinder.unbind(s"$key.year", yValue)),
        dateTimeUserInput.hour.map(hValue => stringBinder.unbind(s"$key.hour", hValue)),
        dateTimeUserInput.minute.map(miValue => stringBinder.unbind(s"$key.minute", miValue))
      ).flatten.mkString("&")
  }

}

case class UserInputs(
  from: Option[DateTimeUserInput],
  to: Option[DateTimeUserInput],
  snapshotIdFilter: Option[String],
  descriptionFilter: Option[String],
  templateIdFilter: Option[String]
)
object UserInputs {
  def apply(): UserInputs = UserInputs(None, None, None, None, None)
  def fromDateInput(userInput: UserInputs, errors: Map[String, String]): DateInput =
    DateTimeUserInput.createDateInput(
      "from",
      "Snapshot created after date",
      userInput.from.getOrElse(DateTimeUserInput()),
      errors
    )

  def toDateInput(userInput: UserInputs, errors: Map[String, String]): DateInput =
    DateTimeUserInput.createDateInput(
      "to",
      "Snapshot created before date",
      userInput.to.getOrElse(DateTimeUserInput()),
      errors
    )

  implicit def queryStringBindableUserInputs(implicit
    dateTimeUserInputBinder: QueryStringBindable[DateTimeUserInput],
    stringBinder: QueryStringBindable[String]
  ): QueryStringBindable[UserInputs] = new QueryStringBindable[UserInputs] {
    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, UserInputs]] =
      (for {
        from  <- dateTimeUserInputBinder.bind("from", params).traverse(identity)
        to    <- dateTimeUserInputBinder.bind("to", params).traverse(identity)
        snId  <- stringBinder.bind("snapshotIdFilter", params).traverse(identity)
        desc  <- stringBinder.bind("descriptionFilter", params).traverse(identity)
        tmpId <- stringBinder.bind("templateIdFilter", params).traverse(identity)
      } yield (from, to, snId, desc, tmpId) match {
        case (None, None, None, None, None) => Option(UserInputs())
        case _                              => Option(UserInputs(from, to, snId, desc, tmpId))
      }).traverse(identity)

    override def unbind(key: String, userInputs: UserInputs): String =
      List(
        Some(dateTimeUserInputBinder.unbind("from", userInputs.from.getOrElse(DateTimeUserInput())))
          .filter(_.trim.nonEmpty),
        Some(dateTimeUserInputBinder.unbind("to", userInputs.to.getOrElse(DateTimeUserInput())))
          .filter(_.trim.nonEmpty),
        userInputs.snapshotIdFilter.filter(_.trim.nonEmpty).map(s => stringBinder.unbind(s"snapshotIdFilter", s)),
        userInputs.descriptionFilter.filter(_.trim.nonEmpty).map(d => stringBinder.unbind(s"descriptionFilter", d)),
        userInputs.templateIdFilter.filter(_.trim.nonEmpty).map(d => stringBinder.unbind(s"templateIdFilter", d))
      ).flatten.mkString("&")
  }

}
