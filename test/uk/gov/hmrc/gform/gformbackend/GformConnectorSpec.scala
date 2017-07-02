package uk.gov.hmrc.gform.gformbackend

import org.scalatest.time.{Millis, Span}
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.gformbackend.model.{FormTemplate, FormTypeId, Version}
import uk.gov.hmrc.gform.models.components.{Constant, FieldId, FieldValue, Text}
import uk.gov.hmrc.gform.models.{DmsSubmission, Section}
import uk.gov.hmrc.gform.wshttp.StubbedWSHttp
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse}

import scala.collection.immutable.List

class GformConnectorSpec extends Spec {

  behavior of "GformConnector.formTemplate - happy path"

  it should "return form template for given type and version" in new Fixture {
    val status = 200
    connector
      .formTemplate(formTypeId, version)
      .futureValue shouldBe formTemplate
  }

  behavior of "GformConnector.formTemplate - unhappy scenario"

  it should "it fails when formTemplate doesn't exist" in new Fixture {
    val status = 404
    connector
      .formTemplate(formTypeId, version)
      .failed
      .futureValue shouldBe an[uk.gov.hmrc.play.http.NotFoundException]
  }

  it should "it fails when gform returns 5xx" in new Fixture {
    val status = 500
    connector
      .formTemplate(formTypeId, version)
      .failed
      .futureValue shouldBe an[uk.gov.hmrc.play.http.Upstream5xxResponse]
  }

  it should "it fails when gform returns BadRequest" in new Fixture {
    val status = 400
    connector
      .formTemplate(formTypeId, version)
      .failed
      .futureValue shouldBe an[uk.gov.hmrc.play.http.BadRequestException]
  }

  it should "it fails when gform returns other 4xx code" in new Fixture {
    val status = 401
    connector
      .formTemplate(formTypeId, version)
      .failed
      .futureValue shouldBe an[uk.gov.hmrc.play.http.Upstream4xxResponse]
  }

  trait Fixture extends ExampleData {

    def status: Int

    lazy val responseJson: Option[JsValue] = Some(Json.toJson(formTemplate))

    lazy val r = HttpResponse(
      responseStatus = status,
      responseJson = responseJson
    )

    lazy val wSHttp = new StubbedWSHttp(r)

    lazy val connector = new GformConnector(wSHttp, "baseUrl")
    implicit lazy val hc: HeaderCarrier = HeaderCarrier()

  }

}


trait ExampleData {

  lazy val dmsSubmission = DmsSubmission("nino", "some-classification-type", "some-business-area")
  lazy val section0 = Section("Your details", None, None, List(FieldValue(FieldId("iptRegNum"), Text(Constant(""), total = false), "Insurance Premium Tax (IPT) number", None, None, true, true, true)))
  lazy val section1 = Section("About you", None, None, List(FieldValue(FieldId("firstName"), Text(Constant(""), total = false), "First Name", None, None, true, true, true)))
  lazy val section2 = Section("Business details", None, None, List(FieldValue(FieldId("nameOfBusiness"), Text(Constant(""), total = false), "Name of business", None, None, true, true, true)))

  lazy val formTypeId = FormTypeId("FormId-13-2-3-1233-3")
  lazy val version = Version("1.2.3")

  lazy val formTemplate =  FormTemplate(
    formTypeId = formTypeId,
    formName = "IPT100",
    version = version,
    description = "abc",
    characterSet = "UTF-8",
    dmsSubmission = dmsSubmission,
    submitSuccessUrl = "success-url",
    submitErrorUrl = "error-url",
    sections = List(section0, section1, section2)
  )
}