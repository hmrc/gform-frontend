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
import cats.instances.option._
import cats.instances.future._
import play.api.i18n.Messages
import play.api.libs.json._
import uk.gov.hmrc.gform.api.NinoInsightsConnector
import uk.gov.hmrc.gform.auth.models.ItmpRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithoutForm
import uk.gov.hmrc.gform.gform.AuthContextPrepop
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.processor.UserCtxEvaluatorProcessor
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveResult, ServiceCallResponse }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

// This is used to evaluate if expressions from exitPages before user's form data are retrieved,
// mainly to exit form for agents before they are asked for access code
class NoFormEvaluation(
  cache: AuthCacheWithoutForm,
  authConfig: AuthConfig,
  itmpRetrievals: Option[ItmpRetrievals],
  gformConnector: GformConnector,
  ninoInsightsConnector: NinoInsightsConnector[Future]
) {

  def toCache: AuthCacheWithoutForm = cache

  def evalIncludeIf(includeIf: IncludeIf)(implicit
    messages: Messages
  ): Boolean =
    evalBooleanExpr(includeIf.booleanExpr)

  private def evalBooleanExpr(be: BooleanExpr)(implicit
    messages: Messages
  ): Boolean =
    be match {
      case Equals(l, r)                                   => evalExpr(l) === evalExpr(r)
      case And(l, r)                                      => evalBooleanExpr(l) && evalBooleanExpr(r)
      case Or(l, r)                                       => evalBooleanExpr(l) || evalBooleanExpr(r)
      case Not(be)                                        => !evalBooleanExpr(be)
      case IsLogin(value)                                 => BooleanExprEval.evalIsLoginExpr(value, cache.retrievals)
      case DateAfter(DateValueExpr(l), DateValueExpr(r))  => l.toLocalDate.isAfter(r.toLocalDate)
      case DateBefore(DateValueExpr(l), DateValueExpr(r)) => l.toLocalDate.isBefore(r.toLocalDate)
      case IsTrue                                         => true
      case _                                              => false
    }

  private def evalExpr(e: Expr)(implicit
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
    case _           => ""
  }

  private def retrieveWithState(
    dataRetrieve: DataRetrieve
  )(implicit hc: HeaderCarrier, ex: ExecutionContext, messages: Messages): Future[Option[DataRetrieveResult]] = {
    val maybeExecutor
      : Option[(DataRetrieve, DataRetrieve.Request) => Future[ServiceCallResponse[DataRetrieve.Response]]] =
      dataRetrieve.tpe match {
        case DataRetrieve.Type("ninoInsights") => Some(ninoInsightsConnector.insights)
        case DataRetrieve.Type("employments")  => Some(gformConnector.getEmployments)
        case DataRetrieve.Type("agentDetails") => Some(gformConnector.getDesAgentDetails)
        case _                                 => Option.empty
      }
    maybeExecutor.flatTraverse { executor =>
      retrieveData(dataRetrieve, executor)
    }
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
      evaluatedParams
    )
  }

  private def retrieveData(
    dataRetrieve: DataRetrieve,
    executor: (DataRetrieve, DataRetrieve.Request) => Future[ServiceCallResponse[DataRetrieve.Response]]
  )(implicit ex: ExecutionContext, messages: Messages): Future[Option[DataRetrieveResult]] = {
    val request: DataRetrieve.Request = prepareRequest(dataRetrieve)
    val requestParams = request.paramsAsJson()

    if (request.notReady()) {
      Future.successful(None)
    } else {
      executor(dataRetrieve, request).map {
        case ServiceResponse(result) =>
          Some(
            DataRetrieveResult(
              dataRetrieve.id,
              result.toRetrieveDataType(),
              requestParams
            )
          )
        case CannotRetrieveResponse | NotFound => throw new Exception("Cannot retrieve data")
      }
    }
  }

  def withDataRetrieve(
    dataRetrieve: Option[NonEmptyList[DataRetrieve]]
  )(implicit hc: HeaderCarrier, ex: ExecutionContext, messages: Messages): Future[NoFormEvaluation] =
    for {
      results <- dataRetrieve.fold(Future.successful(List.empty[DataRetrieveResult])) { r =>
                   r.toList.foldLeft(Future.successful(List.empty[DataRetrieveResult])) { case (acc, r) =>
                     acc.flatMap {
                       case results if r.`if`.forall(evalIncludeIf) =>
                         retrieveWithState(r).map {
                           case Some(result) => results :+ result
                           case None         => results
                         }
                       case results =>
                         Future.successful(results)
                     }
                   }
                 }
    } yield new NoFormEvaluation(
      cache.copy(dataRetrieve = Some(results.map(r => r.id -> r).toMap)),
      authConfig,
      itmpRetrievals,
      gformConnector,
      ninoInsightsConnector
    )

}
