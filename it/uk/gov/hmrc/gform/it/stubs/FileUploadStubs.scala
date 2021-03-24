package uk.gov.hmrc.gform.it.stubs

import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock.{ ok, stubFor }

trait FileUploadStubs {
  def getFileUploadEnvelopeStub() =
    stubFor(
      WireMock
        .get(s"/file-upload/envelopes/some-envelope-id")
        .willReturn(ok("""{ "files": [] }""".stripMargin))
    )

}
