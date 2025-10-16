/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.eval

import cats.data.NonEmptyList
import cats.syntax.all._
import play.api.i18n.Messages
import play.api.libs.json._
import uk.gov.hmrc.gform.api.NinoInsightsConnector
import uk.gov.hmrc.gform.auth.models.ItmpRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithoutForm
import uk.gov.hmrc.gform.gform.{ AuthContextPrepop, DataRetrieveService }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.processor.UserCtxEvaluatorProcessor
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.binders.RedirectUrl

import java.time.LocalDate
import scala.concurrent.{ ExecutionContext, Future }

// This is used to evaluate if expressions from exitPages before user's form data are retrieved,
// mainly to exit form for agents before they are asked for access code  find a class name for this
case class InitFormEvaluator(
  cache: AuthCacheWithoutForm,
  authConfig: AuthConfig,
  itmpRetrievals: Option[ItmpRetrievals]
) {
  def evalIncludeIf(includeIf: IncludeIf)(implicit
    messages: Messages
  ): Boolean =
    evalBooleanExpr(includeIf.booleanExpr)

  val now: LocalDate = LocalDate.now()

  private def evalBooleanExpr(be: BooleanExpr)(implicit
    messages: Messages
  ): Boolean =
    be match {
      case Equals(l, r)                                   => evalExpr(l) === evalExpr(r)
      case And(l, r)                                      => evalBooleanExpr(l) && evalBooleanExpr(r)
      case Or(l, r)                                       => evalBooleanExpr(l) || evalBooleanExpr(r)
      case Not(be)                                        => !evalBooleanExpr(be)
      case IsLogin(value)                                 => BooleanExprEval.evalIsLoginExpr(value, cache.retrievals)
      case DateAfter(DateValueExpr(l), DateValueExpr(r))  => l.toLocalDate(now).isAfter(r.toLocalDate(now))
      case DateBefore(DateValueExpr(l), DateValueExpr(r)) => l.toLocalDate(now).isBefore(r.toLocalDate(now))
      case IsTrue                                         => true
      case _                                              => false
    }

  def evalExpr(e: Expr)(implicit
    messages: Messages
  ): String = e match {
    case UserCtx(userField) =>
      UserCtxEvaluatorProcessor.processEvaluation(
        cache.retrievals,
        userField,
        authConfig
      )
    case AuthCtx(value: AuthInfo) =>
      AuthContextPrepop
        .values(value, cache.retrievals, itmpRetrievals)
        .stringRepresentation(TypeInfo(e, StaticTypeData(ExprType.string, None)), messages)
    case d @ DataRetrieveCtx(_, _) =>
      cache.dataRetrieve.fold("") { dr =>
        DataRetrieveEval.getDataRetrieveAttribute(dr, d) match {
          case Some(h :: Nil) => h
          case Some(xs)       => xs.mkString(" ")
          case None           => ""
        }
      }
    case Constant(c) => c
    case LinkCtx(internalLink) =>
      internalLink match {
        case InternalLink.NewFormForTemplate(formTemplateId) =>
          uk.gov.hmrc.gform.gform.routes.NewFormController
            .dashboardClean(formTemplateId)
            .url
        case InternalLink.NewForm =>
          uk.gov.hmrc.gform.gform.routes.NewFormController
            .dashboardNewFormLink(cache.formTemplate._id)
            .url
        case InternalLink.NewSession =>
          uk.gov.hmrc.gform.gform.routes.NewFormController
            .dashboardWithNewSession(cache.formTemplate._id)
            .url
        case InternalLink.SignOut =>
          uk.gov.hmrc.gform.gform.routes.SignOutController
            .signOut(cache.formTemplate._id)
            .url
        case InternalLink.UrlLink(url) =>
          uk.gov.hmrc.gform.gform.routes.RedirectController.redirect(RedirectUrl(url)).url
        case _ => ""
      }
    case _ => ""
  }

  private def prepareRequest(dataRetrieve: DataRetrieve)(implicit
    messages: Messages
  ): DataRetrieve.Request = {
    val evaluatedParams = dataRetrieve.params.map {
      case DataRetrieve.ParamExpr(DataRetrieve.Parameter(name, _, _), expr) =>
        name -> evalExpr(expr)
    }

    val jsonObjects: List[JsObject] = dataRetrieve.params.map {
      case DataRetrieve.ParamExpr(DataRetrieve.Parameter(name, path, tpe), expr) =>
        val value = evalExpr(expr)

        val leafValue = Json.obj(name -> JsString(value))
        path.foldLeft(leafValue) { case (acc, path) =>
          Json.obj(path -> acc)
        }

    }

    val json = jsonObjects.foldLeft(Json.obj())(_.deepMerge(_))

    DataRetrieve.Request(
      json,
      evaluatedParams,
      None,
      None
    )
  }
}

object InitFormEvaluator {
  def makeCacheWithDataRetrieve(
    cache: AuthCacheWithoutForm,
    authConfig: AuthConfig,
    itmpRetrievals: Option[ItmpRetrievals],
    dataRetrieve: Option[NonEmptyList[DataRetrieve]],
    gformConnector: GformConnector,
    ninoInsightsConnector: NinoInsightsConnector[Future]
  )(implicit hc: HeaderCarrier, ex: ExecutionContext, messages: Messages): Future[AuthCacheWithoutForm] =
    for {
      results <- dataRetrieve.fold(Future.successful(List.empty[DataRetrieveResult])) { r =>
                   r.toList.foldLeft(Future.successful(List.empty[DataRetrieveResult])) { case (acc, r) =>
                     val initFormEvaluator = InitFormEvaluator(cache, authConfig, itmpRetrievals)
                     acc.flatMap {
                       case results if r.`if`.forall(initFormEvaluator.evalIncludeIf) =>
                         val request = initFormEvaluator.prepareRequest(r)
                         DataRetrieveService
                           .retrieveDataResult(
                             r,
                             None,
                             request,
                             None,
                             None,
                             Some(ninoInsightsConnector),
                             None,
                             Some(gformConnector),
                             None,
                             None
                           )
                           .map {
                             case Some(result) => results :+ result
                             case None         => results
                           }
                       case results =>
                         Future.successful(results)
                     }
                   }
                 }
    } yield
      if (results.isEmpty) cache
      else cache.copy(dataRetrieve = Some(results.map(r => r.id -> r).toMap))
}
