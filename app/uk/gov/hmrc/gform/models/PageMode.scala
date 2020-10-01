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

package uk.gov.hmrc.gform.models

sealed trait PageMode extends Product with Serializable

trait Basic extends PageMode
trait DataExpanded extends PageMode

/*
 * Marks a FormModel used to verify cyclic dependencies in dependency graph
 */
trait DependencyGraphVerification extends PageMode

/*
 * Marks a FormModel with following properties:
 *   - it doesn't include invisible sections
 *   - it doesn't include fields on revealing choices under options which are not selected
 */
trait Visibility extends PageMode
trait Interim extends PageMode
