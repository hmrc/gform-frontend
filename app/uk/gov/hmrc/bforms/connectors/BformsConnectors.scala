package uk.gov.hmrc.bforms.connectors

import uk.gov.hmrc.bforms.WSHttp
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http.HttpPost

trait BformsConnector {

  def httpPost: HttpPost

  def bformsUrl: String

  def saveForm[A](formDetails : A) = {
    httpPost.POST[A, String](bformsUrl + "/saveForm", formDetails)
  }

  def retrieveForm[A](registrationNumber: String) = {
    httpPost.POST[String, A](bformsUrl + s"/retrieveForm/", registrationNumber)
  }

}

object BformsConnector extends BformsConnector with ServicesConfig {

  lazy val httpPost = WSHttp

  def bformsUrl: String = s"${baseUrl("bforms")}/bforms"
}
