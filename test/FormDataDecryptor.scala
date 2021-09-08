/*
 * Copyright 2021 HM Revenue & Customs
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

import com.typesafe.config.ConfigFactory
import uk.gov.hmrc.crypto.{ Crypted, CryptoWithKeysFromConfig }

object FormDataDecryptor extends App {

  val jsonCrypto =
    new CryptoWithKeysFromConfig(
      baseConfigKey = "json.encryption",
      ConfigFactory.parseString("""
                                  | json.encryption.key = "fqpLDZ4sumDsekHkeEBlCA=="
                                  | json.encryption.previousKeys = ""
                                  |""".stripMargin)
    )

  println(
    jsonCrypto
      .decrypt(
        Crypted(
          "YqXxFaIpftfoe19tBuAW3DYpGJWOgZfRvD2Yl350dLyF5e66eaDwem5WLxPIICbFdON/rYkpTWFIt/L4Ax8uEgF43DbkcX+rJ54FLknH9UFXXVLkAMbXtBxGJECefNpFoTpEno+1DyQDeA7M/2Smxssb75GGyXpz3+5Ob2TGl7LQEKr/iP+zgQSgbdxVq8rHIFRX8lvlTgwkDfoOLat7ifFdZK+m7Qry624igGix5jghrdYFuEWgVcIWye4PQiqoTTslpJQDCXi1EUvaCymKmJWxMxXcR0YVmOhIHeRGoDRUrfcFtUJzFdn/fa1ZT+4ZiX+3zHJby7xBT6vQRJjnK7f4OwmKnlEJkciyo7NruYrPmFACQGt5gehqlSq7ikpOYRhaYWhIHkSHBAKz61edzqV+Fso3OMfmBinZ2oWZWcuz+uAAws+xXGnWz+95u3DWIoEYCcIEGkLuesjvIpytK+SZ027Nueee/jxrzF3iS6Ys9W+ZsKYWfDeFD7iX7+eti0C8HM8sYu853bgdHf9ELDPOriNpsF/Jjw/SVw4t0QaQLgFCtr/bJSZn8jwOdUNPleBOXcbDXAbtXLEjnN8EELp7Yejtv9EfGCBa7SqrdpGLXgKJ+vIfIpD16pb8UC3U"
        )
      )
      .value
  )
}
