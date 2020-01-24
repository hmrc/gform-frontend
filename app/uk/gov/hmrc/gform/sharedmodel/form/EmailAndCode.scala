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

package uk.gov.hmrc.gform.sharedmodel.form

import cats.data.State
import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.gform.sharedmodel.email.EmailConfirmationCode
import uk.gov.hmrc.gform.typeclasses.Rnd

case class EmailAndCode(email: String, code: EmailConfirmationCode)

object EmailAndCode {

  private def random(implicit rnd: Rnd[Int]): String = {

    val vowels = Set('A', 'E', 'I', 'O', 'U', 'Y')
    val consonants = ('A' to 'Z').filterNot(vowels.contains)

    def mkStream(chars: IndexedSeq[Char]) = {
      def next: Char = chars.charAt(rnd.random(chars.length))
      Stream continually next
    }

    def mkState(stream: Stream[Char], n: Int) = State[Rnd[Int], String](r => (r, stream.take(n).toList.mkString))

    def consonantStream = mkStream(consonants)

    def getConsonants(i: Int) = mkState(consonantStream, i)

    val refGen = for {
      code <- getConsonants(4)
    } yield code

    refGen.runA(rnd).value
  }

  def emailVerificationCode(email: String): EmailAndCode =
    EmailAndCode(email, EmailConfirmationCode(random))

  implicit val format: OFormat[EmailAndCode] = derived.oformat
}
