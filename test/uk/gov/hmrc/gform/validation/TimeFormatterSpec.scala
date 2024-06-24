package uk.gov.hmrc.gform.validation

import uk.gov.hmrc.gform.Spec

import java.time.LocalTime

class TimeFormatterSpec extends Spec {
  ".normalizeLocalTime" should "remove the leading 0 if there is only one digit for the hour" in {
    val normalisedTime: String = TimeFormatter.normalizeLocalTime(LocalTime.parse("09:00"))
    val expectedTime: String = "9:00am"

    normalisedTime shouldBe expectedTime
  }

  it should "return the time as a 12-hour clock followed by am when appropriate" in {
    val normalisedTime: String = TimeFormatter.normalizeLocalTime(LocalTime.parse("05:00"))
    val expectedTime: String = "5:00am"

    normalisedTime shouldBe expectedTime
  }

  it should "return the time as a 12-hour clock followed by pm when appropriate" in {
    val normalisedTime: String = TimeFormatter.normalizeLocalTime(LocalTime.parse("15:00"))
    val expectedTime: String = "3:00pm"

    normalisedTime shouldBe expectedTime
  }

  ".maybeLocalTime" should "accept a number from 1-9 as a valid time" in {
    val maybeLocalTime: Option[LocalTime] = TimeFormatter.maybeLocalTime("3")
    val expectedTime: Option[LocalTime] = Some(LocalTime.parse("03:00"))

    maybeLocalTime shouldBe expectedTime
  }

  ".maybeLocalTime" should "not accept the number 0 as a valid time" in {
    val maybeLocalTime: Option[LocalTime] = TimeFormatter.maybeLocalTime("0")
    val expectedTime: Option[LocalTime] = None

    maybeLocalTime shouldBe expectedTime
  }
}
