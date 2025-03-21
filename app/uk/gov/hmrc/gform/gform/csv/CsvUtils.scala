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

package uk.gov.hmrc.gform.gform.csv

import kantan.csv.ops.toCsvInputOps
import kantan.csv.{ HeaderDecoder, rfc }

import scala.util.Using

object CsvUtils {
  def readCsv[A](filename: String, headerDecoder: HeaderDecoder[A]): List[A] =
    Using.resource(getClass.getClassLoader.getResourceAsStream("lookup/" + filename)) { lookup =>
      lookup.unsafeReadCsv[List, A](rfc.withHeader)(headerDecoder, implicitly, implicitly)
    }

  def readCsvWithColumns(filename: String): List[Map[String, String]] =
    Using.resource(getClass.getClassLoader.getResourceAsStream("lookup/" + filename)) { lookup =>
      val lines: List[List[String]] = lookup.asUnsafeCsvReader[List[String]](rfc.withHeader(false)).toList
      lines.tail.map(lines.head.zip(_).toMap)
    }

}
