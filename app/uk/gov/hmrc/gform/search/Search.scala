/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.search

import cats.syntax.all._
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.{ BooleanQuery, PrefixQuery }
import org.apache.lucene.search.ConstantScoreQuery
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.lookup.LookupLabel
import org.apache.lucene.search.{ IndexSearcher, Query }
import org.apache.lucene.index.StoredFields

case class SicCode(code: String, desc: String)

object Search {

  val FIELD_SEARCH_TERMS_EN = "searchTermsEn"
  val FIELD_SEARCH_TERMS_CY = "searchTermsCy"

  def search(
    searcher: IndexSearcher,
    query: String
  )(implicit l: LangADT): List[LookupLabel] = {

    val splitSearchParams = query.toLowerCase.split(" ").map(_.trim)
    val queryBuilder = new BooleanQuery.Builder()

    val fieldName = if (l === LangADT.En) FIELD_SEARCH_TERMS_EN else FIELD_SEARCH_TERMS_CY

    splitSearchParams.foreach { value =>
      queryBuilder.add(new PrefixQuery(new Term(fieldName, value)), Occur.MUST)
    }

    val parsedQuery: Query = new ConstantScoreQuery(queryBuilder.build())

    val results: Array[org.apache.lucene.search.ScoreDoc] = searcher.search(parsedQuery, Integer.MAX_VALUE).scoreDocs

    val storedFields: StoredFields = searcher.storedFields()

    results.map { sf =>
      val doc = storedFields.document(sf.doc)
      LookupLabel(
        if (l === LangADT.En)
          doc.get(FIELD_SEARCH_TERMS_EN)
        else
          doc.get(FIELD_SEARCH_TERMS_CY)
      )
    }.toList
  }
}
