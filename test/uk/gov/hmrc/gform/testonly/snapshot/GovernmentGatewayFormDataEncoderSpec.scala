/*
 * Copyright 2024 HM Revenue & Customs
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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.domain.Nino

class GovernmentGatewayFormDataEncoderSpec extends AnyFlatSpec with Matchers {
  "GovernmentGatewayFormData encoder" should "correctly encode data" in new TestFixtures {

    val result = UrlFormEncoder[GovernmentGatewayFormData].encode(formData)
    result shouldBe List(
      ("redirectionUrl", "http://example.com"),
      ("credentialStrength", "strong"),
      ("confidenceLevel", "high"),
      ("authorityId", "cred-123"),
      ("affinityGroup", "Individual"),
      ("credentialRole", "User"),
      ("gatewayToken", "gateway-123"),
      ("nino", "AB123456C"),
      ("usersName", "John Doe"),
      ("email", "text@foo.com"),
      ("enrolment[0].name", "HMCE-VATDEC-ORG"),
      ("enrolment[0].state", "Activated"),
      ("enrolment[0].taxIdentifier[0].name", "VATRegNo"),
      ("enrolment[0].taxIdentifier[0].value", "12341234"),
      ("enrolment[0].taxIdentifier[1].name", "UTR"),
      ("enrolment[0].taxIdentifier[1].value", "12341234"),
      ("enrolment[1].name", "IR-SA"),
      ("enrolment[1].state", "Activated"),
      ("enrolment[1].taxIdentifier[0].name", "VATRegNo"),
      ("enrolment[1].taxIdentifier[0].value", "12341234"),
      ("enrolment[1].taxIdentifier[1].name", "UTR"),
      ("enrolment[1].taxIdentifier[1].value", "12341234"),
      ("itmp.givenName", "John"),
      ("itmp.middleName", "Smith"),
      ("itmp.familyName", "Doe"),
      ("itmp.dateOfBirth", "1980-01-01"),
      ("itmp.address.line1", "1 Fake Street"),
      ("itmp.address.line2", "Fake Town"),
      ("itmp.address.line3", "Fake City"),
      ("itmp.address.line4", "Fake County"),
      ("itmp.address.line5", "Fake Line 5"),
      ("itmp.address.postCode", "AB12 3CD"),
      ("itmp.address.countryCode", "GB"),
      ("itmp.address.countryName", "United Kingdom"),
      ("agent.agentId", "agent-123"),
      ("agent.agentCode", "agent-456"),
      ("agent.agentFriendlyName", "agent-789")
    )

  }

  it should "correctly url encode data" in new TestFixtures {
    val expectedEncodedString = """
                                  |redirectionUrl=http%3A%2F%2Fexample.com
                                  |&credentialStrength=strong
                                  |&confidenceLevel=high
                                  |&authorityId=cred-123
                                  |&affinityGroup=Individual
                                  |&credentialRole=User
                                  |&gatewayToken=gateway-123
                                  |&nino=AB123456C
                                  |&usersName=John+Doe
                                  |&email=text%40foo.com
                                  |&enrolment%5B0%5D.name=HMCE-VATDEC-ORG
                                  |&enrolment%5B0%5D.state=Activated
                                  |&enrolment%5B0%5D.taxIdentifier%5B0%5D.name=VATRegNo
                                  |&enrolment%5B0%5D.taxIdentifier%5B0%5D.value=12341234
                                  |&enrolment%5B0%5D.taxIdentifier%5B1%5D.name=UTR
                                  |&enrolment%5B0%5D.taxIdentifier%5B1%5D.value=12341234
                                  |&enrolment%5B1%5D.name=IR-SA
                                  |&enrolment%5B1%5D.state=Activated
                                  |&enrolment%5B1%5D.taxIdentifier%5B0%5D.name=VATRegNo
                                  |&enrolment%5B1%5D.taxIdentifier%5B0%5D.value=12341234
                                  |&enrolment%5B1%5D.taxIdentifier%5B1%5D.name=UTR
                                  |&enrolment%5B1%5D.taxIdentifier%5B1%5D.value=12341234
                                  |&itmp.givenName=John
                                  |&itmp.middleName=Smith
                                  |&itmp.familyName=Doe
                                  |&itmp.dateOfBirth=1980-01-01
                                  |&itmp.address.line1=1+Fake+Street
                                  |&itmp.address.line2=Fake+Town
                                  |&itmp.address.line3=Fake+City
                                  |&itmp.address.line4=Fake+County
                                  |&itmp.address.line5=Fake+Line+5
                                  |&itmp.address.postCode=AB12+3CD
                                  |&itmp.address.countryCode=GB
                                  |&itmp.address.countryName=United+Kingdom
                                  |&agent.agentId=agent-123
                                  |&agent.agentCode=agent-456
                                  |&agent.agentFriendlyName=agent-789
  """.stripMargin.trim.replaceAll("\n", "")

    GovernmentGatewayFormData.toUrlEncoded(
      formData
    ) shouldBe expectedEncodedString
  }

  trait TestFixtures {
    val taxIdentifierVAT = TaxIdentifierData("VATRegNo", "12341234")
    val taxIdentifierUTR = TaxIdentifierData("UTR", "12341234")
    val enrolment1 = EnrolmentData("HMCE-VATDEC-ORG", "Activated", List(taxIdentifierVAT, taxIdentifierUTR))
    val enrolment2 = EnrolmentData("IR-SA", "Activated", List(taxIdentifierVAT, taxIdentifierUTR))
    val itmpData = ItmpData(
      givenName = Some("John"),
      middleName = Some("Smith"),
      familyName = Some("Doe"),
      birthdate = Some("1980-01-01"),
      address = ItmpAddress(
        line1 = Some("1 Fake Street"),
        line2 = Some("Fake Town"),
        line3 = Some("Fake City"),
        line4 = Some("Fake County"),
        line5 = Some("Fake Line 5"),
        postCode = Some("AB12 3CD"),
        countryCode = Some("GB"),
        countryName = Some("United Kingdom")
      )
    )
    val nino = Nino("AB123456C")
    val agent = AgentData(Some("agent-123"), Some("agent-456"), Some("agent-789"))
    val formData = GovernmentGatewayFormData(
      redirectionUrl = "http://example.com",
      credentialStrength = "strong",
      confidenceLevel = "high",
      credId = "cred-123",
      nino = Some(nino),
      enrolments = List(enrolment1, enrolment2),
      affinityGroup = "Individual",
      credentialRole = "User",
      usersName = Some("John Doe"),
      email = Some("text@foo.com"),
      gatewayToken = Some("gateway-123"),
      groupIdentifier = None,
      itmpData = Some(itmpData),
      agent = Some(agent)
    )
  }
}
