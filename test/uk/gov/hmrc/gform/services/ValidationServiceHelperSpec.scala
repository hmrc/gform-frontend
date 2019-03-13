package uk.gov.hmrc.gform.services
import org.scalatest.{FlatSpec, Matchers}
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationServiceHelper
import uk.gov.hmrc.gform.views.html.localisation

class ValidationServiceHelperSpec extends FlatSpec with Matchers {

  "ValidationServiceHelper.incorrectDate with precisely yyyy-04-dd" should "return message: must be in April" in {
    println(ValidationServiceHelper
      .incorrectDateMessage(Precisely, ConcreteDate(AnyYear, ExactMonth(4), AnyDay), OffsetDate(0)))

    localisation(ValidationServiceHelper
      .incorrectDateMessage(Precisely, ConcreteDate(AnyYear, ExactMonth(4), AnyDay), OffsetDate(0))) shouldBe
      "must be in April"
  }

  "ValidationServiceHelper.incorrectDate with precisely next-mm-dd" should "return message must be in April" in {
    ValidationServiceHelper
      .incorrectDateMessage(Precisely, ConcreteDate(Next, AnyMonth, AnyDay), OffsetDate(0)) shouldBe
      s"must be in ${ValidationServiceHelper.getNextYear}"
  }

  "ValidationServiceHelper.incorrectDate with precisely previous-mm-dd" should "return message must be in April" in {
    ValidationServiceHelper
      .incorrectDateMessage(Precisely, ConcreteDate(Previous, AnyMonth, AnyDay), OffsetDate(0)) shouldBe
      s"must be in ${ValidationServiceHelper.getPreviousYear}"
  }

  "ValidationServiceHelper.incorrectDate with precisely yyyy-mm-firstDay" should "return message: must be the first day of the month" in {
    ValidationServiceHelper
      .incorrectDateMessage(Precisely, ConcreteDate(AnyYear, AnyMonth, FirstDay), OffsetDate(0)) shouldBe
      s"must be the first day of the month"
  }

  "ValidationServiceHelper.incorrectDate with precisely yyyy-mm-lastDay" should "return message: must be the last day of the month" in {
    ValidationServiceHelper
      .incorrectDateMessage(Precisely, ConcreteDate(AnyYear, AnyMonth, LastDay), OffsetDate(0)) shouldBe
      s"must be the last day of the month"
  }

  "ValidationServiceHelper.incorrectDate with precisely 2018-mm-dd" should "return message: must be in 2018" in {
    ValidationServiceHelper
      .incorrectDateMessage(Precisely, ConcreteDate(ExactYear(2018), AnyMonth, AnyDay), OffsetDate(0)) shouldBe
      s"must be in 2018"
  }

  "ValidationServiceHelper.incorrectDate with precisely yyyy-mm-3" should "return message: must be the third day of the month" in {
    ValidationServiceHelper
      .incorrectDateMessage(Precisely, ConcreteDate(AnyYear, AnyMonth, ExactDay(3)), OffsetDate(0)) shouldBe
      s"must be the 3rd of any month"
  }

}
