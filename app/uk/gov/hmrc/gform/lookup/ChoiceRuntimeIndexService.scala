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

package uk.gov.hmrc.gform.lookup

import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.{ Document, Field, StringField, TextField }
import org.apache.lucene.index.{ DirectoryReader, IndexWriter, IndexWriterConfig, Term }
import org.apache.lucene.search.{ BooleanQuery, ConstantScoreQuery, IndexSearcher, PrefixQuery, TopDocs }
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.store.ByteBuffersDirectory
import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

import scala.util.{ Failure, Success, Try }

case class ChoiceOption(value: String, label: String)
case class ChoiceSearchResult(value: String, label: String)

object ChoiceSearchResult {
  implicit val format: OFormat[ChoiceSearchResult] = Json.format[ChoiceSearchResult]
}

class ChoiceRuntimeIndexService {

  private val analyzer = new StandardAnalyzer()
  private var indexMap: Map[String, (ByteBuffersDirectory, IndexSearcher)] = Map.empty

  def createIndexForChoiceOptions(formComponentId: FormComponentId, options: List[ChoiceOption]): Unit = {
    val directory = new ByteBuffersDirectory()
    val config = new IndexWriterConfig(analyzer)
    val writer = new IndexWriter(directory, config)

    try options.foreach { choiceOption =>
      val doc = new Document()
      doc.add(new StringField("value", choiceOption.value, Field.Store.YES))
      doc.add(new TextField("label", choiceOption.label, Field.Store.YES))
      doc.add(new TextField("searchTerms", choiceOption.label.toLowerCase, Field.Store.NO))
      writer.addDocument(doc)
    } finally writer.close()

    val reader = DirectoryReader.open(directory)
    val searcher = new IndexSearcher(reader)
    indexMap = indexMap + (formComponentId.value -> (directory, searcher))
  }

  def search(formComponentId: String, query: String, maxResults: Int = 10): List[ChoiceSearchResult] =
    indexMap.get(formComponentId) match {
      case Some((_, searcher)) =>
        Try {
          val splitSearchParams = query.toLowerCase.split(" ").map(_.trim)
          val queryBuilder = new BooleanQuery.Builder()

          splitSearchParams.foreach { value =>
            queryBuilder.add(new PrefixQuery(new Term("searchTerms", value)), Occur.MUST)
          }

          val parsedQuery = new ConstantScoreQuery(queryBuilder.build())
          val topDocs: TopDocs = searcher.search(parsedQuery, maxResults)

          topDocs.scoreDocs.toList.map { scoreDoc =>
            val doc = searcher.storedFields().document(scoreDoc.doc)
            ChoiceSearchResult(
              value = doc.get("value"),
              label = doc.get("label")
            )
          }
        } match {
          case Success(results) => results
          case Failure(_)       => List.empty
        }
      case None => List.empty
    }
}
