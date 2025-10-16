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

import org.apache.lucene.analysis.{ Analyzer, TokenStream }
import org.apache.lucene.analysis.core.{ LowerCaseFilter, WhitespaceTokenizer }
import org.apache.lucene.document.{ Document, Field, StringField, TextField }
import org.apache.lucene.index.{ DirectoryReader, IndexWriter, IndexWriterConfig, Term }
import org.apache.lucene.search.{ BooleanQuery, IndexSearcher, PrefixQuery }
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.store.ByteBuffersDirectory
import play.api.libs.json.{ Json, OFormat }

import scala.util.{ Failure, Success, Try }

case class ChoiceOption(value: String, label: String, keyWord: Option[String] = None)
case class ChoiceSearchResult(value: String, label: String)

object ChoiceSearchResult {
  implicit val format: OFormat[ChoiceSearchResult] = Json.format[ChoiceSearchResult]
}

class WhiteSpaceOnlyAnalyzer() extends Analyzer {
  override def createComponents(fieldName: String): Analyzer.TokenStreamComponents = {
    val tokenizer = new WhitespaceTokenizer() // keep -, €, $ etc.
    val lowercased: TokenStream = new LowerCaseFilter(tokenizer)
    new Analyzer.TokenStreamComponents(tokenizer, lowercased)
  }
}

class ChoiceRuntimeIndexService {

  private val analyzer = new WhiteSpaceOnlyAnalyzer()
  private var indexMap: Map[String, (ByteBuffersDirectory, IndexSearcher, Long)] = Map.empty
  private val INDEX_TTL_MS = 30 * 60 * 1000 // 30 minutes TTL

  def createIndexForChoiceOptions(indexKey: String, options: List[ChoiceOption]): Unit =
    if (!indexMap.contains(indexKey)) {

      val directory = new ByteBuffersDirectory()
      val config = new IndexWriterConfig(analyzer)
      val writer = new IndexWriter(directory, config)

      try options.foreach { choiceOption =>
        val doc = new Document()
        doc.add(new StringField("value", choiceOption.value, Field.Store.YES))
        doc.add(new TextField("label", choiceOption.label, Field.Store.YES))

        // Index label for prefix searching
        doc.add(new TextField("searchable", choiceOption.label.toLowerCase, Field.Store.NO))

        // Index keyword for searching
        choiceOption.keyWord.foreach { keyword =>
          val normalizedKeyword = keyword.toLowerCase.trim
          if (normalizedKeyword.nonEmpty) {
            doc.add(new TextField("searchable", normalizedKeyword, Field.Store.NO))
          }
        }

        writer.addDocument(doc)
      } finally writer.close()

      val reader = DirectoryReader.open(directory)
      val searcher = new IndexSearcher(reader)
      val timestamp = System.currentTimeMillis()
      indexMap = indexMap + (indexKey -> (directory, searcher, timestamp))

      cleanupExpiredIndexes()
    }

  def search(indexKey: String, query: String): List[ChoiceSearchResult] =
    indexMap.get(indexKey) match {
      case Some((_, searcher, _)) =>
        Try {
          val searchQuery = query.toLowerCase.trim
          if (searchQuery.isEmpty) {
            List.empty
          } else {
            searchByTerms(searcher, searchQuery)
          }
        } match {
          case Success(results) => results
          case Failure(_)       => List.empty
        }
      case None => List.empty
    }

  private def searchByTerms(searcher: IndexSearcher, searchQuery: String): List[ChoiceSearchResult] = {
    val terms = searchQuery.split("\\s+").filter(_.nonEmpty)

    val booleanQuery = new BooleanQuery.Builder()
    terms.foreach { term =>
      booleanQuery.add(new PrefixQuery(new Term("searchable", term)), Occur.MUST)
    }

    val results = searcher.search(booleanQuery.build(), Integer.MAX_VALUE)
    results.scoreDocs.toList.map { scoreDoc =>
      val doc = searcher.storedFields().document(scoreDoc.doc)
      ChoiceSearchResult(doc.get("value"), doc.get("label"))
    }
  }

  private def cleanupExpiredIndexes(): Unit = {
    val currentTime = System.currentTimeMillis()
    val expiredKeys = indexMap
      .filter { case (_, (_, _, timestamp)) =>
        currentTime - timestamp > INDEX_TTL_MS
      }
      .keys
      .toList

    expiredKeys.foreach { key =>
      indexMap.get(key).foreach { case (directory, _, _) =>
        try directory.close()
        catch {
          case _: Exception =>
        }
      }
      indexMap = indexMap - key
    }
  }
}
