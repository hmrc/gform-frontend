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
import org.apache.lucene.search.{ BooleanQuery, IndexSearcher, PrefixQuery, TermQuery }
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.store.ByteBuffersDirectory
import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

import scala.util.{ Failure, Success, Try }

case class ChoiceOption(value: String, label: String, keyWord: Option[String] = None)
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

      // Index label for prefix searching
      doc.add(new TextField("searchable", choiceOption.label.toLowerCase, Field.Store.NO))

      // Index exact keyword for abbreviation/synonym matching
      choiceOption.keyWord.foreach { keyword =>
        val normalizedKeyword = keyword.toLowerCase.trim
        if (normalizedKeyword.nonEmpty) {
          doc.add(new StringField("exactKeyword", normalizedKeyword, Field.Store.NO))
          // Also add keyword to searchable terms for prefix matching
          doc.add(new TextField("searchable", normalizedKeyword, Field.Store.NO))
        }
      }

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
          val searchQuery = query.toLowerCase.trim
          if (searchQuery.isEmpty) {
            List.empty
          } else {
            // 1. Try exact keyword match first (for abbreviations like "ltd")
            val exactKeywordQuery = new TermQuery(new Term("exactKeyword", searchQuery))
            val exactResults = searcher.search(exactKeywordQuery, maxResults)

            if (exactResults.totalHits.value > 0) {
              exactResults.scoreDocs.toList.map { scoreDoc =>
                val doc = searcher.storedFields().document(scoreDoc.doc)
                ChoiceSearchResult(doc.get("value"), doc.get("label"))
              }
            } else {
              // 2. Fall back to prefix search for labels and keywords
              searchByTerms(searcher, searchQuery, maxResults)
            }
          }
        } match {
          case Success(results) => results
          case Failure(_)       => List.empty
        }
      case None => List.empty
    }

  private def searchByTerms(searcher: IndexSearcher, searchQuery: String, maxResults: Int): List[ChoiceSearchResult] = {
    val terms = searchQuery.split("\\s+").filter(_.nonEmpty)

    if (terms.length == 1) {
      // Single term - use prefix query
      val prefixQuery = new PrefixQuery(new Term("searchable", terms.head))
      val results = searcher.search(prefixQuery, maxResults)
      results.scoreDocs.toList.map { scoreDoc =>
        val doc = searcher.storedFields().document(scoreDoc.doc)
        ChoiceSearchResult(doc.get("value"), doc.get("label"))
      }
    } else {
      // Multiple terms - match any term with prefix
      val booleanQuery = new BooleanQuery.Builder()
      terms.foreach { term =>
        booleanQuery.add(new PrefixQuery(new Term("searchable", term)), Occur.SHOULD)
      }

      val results = searcher.search(booleanQuery.build(), maxResults)
      results.scoreDocs.toList.map { scoreDoc =>
        val doc = searcher.storedFields().document(scoreDoc.doc)
        ChoiceSearchResult(doc.get("value"), doc.get("label"))
      }
    }
  }
}
