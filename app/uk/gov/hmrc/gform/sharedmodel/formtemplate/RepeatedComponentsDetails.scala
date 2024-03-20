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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

/** Manages mappings between repeated components and their parent components, providing functionalities such as:
  * - Identifying if a component is repeated based on its ID.
  * - Retrieving the parent ID of a given component.
  * - Checking if a component is a parent of another component.
  *
  * Usage scenarios include:
  * - ATL components: The key is the ATL field ID, and the value is the ID of `addAnotherQuestion`.
  * - Group components: The key is the child component ID within the group, and the value is the group's ID.
  * - RepeatingPage components: The key and value are the same, representing the RepeatingPage's field ID.
  */
case class RepeatedComponentsDetails(
  private val componentToParentMapping: Map[FormComponentId, FormComponentId]
) {

  // Checks if a given form component ID corresponds to a repeated component.
  def isRepeated(componentId: FormComponentId): Boolean =
    componentToParentMapping.keys.exists(_.baseComponentId == componentId.baseComponentId)

  // Gets the parent ID of a specific component, if it exists.
  def getParentIdOf(componentId: FormComponentId): Option[FormComponentId] =
    componentToParentMapping.find { case (key, _) => key.baseComponentId == componentId.baseComponentId }.map(_._2)

  // Retrieves the parent IDs for a list of component IDs.
  def getParentIdsFor(componentIds: List[FormComponentId]): List[FormComponentId] =
    componentIds.flatMap(getParentIdOf)

  // Determines if any of the specified potential parents is a parent of any of the given potential child components.
  def hasParent(potentialParents: List[FormComponentId], potentialChilds: List[FormComponentId]): Boolean = {
    val parentIds = getParentIdsFor(potentialChilds).map(_.baseComponentId).toSet
    potentialParents.exists(parent => parentIds.contains(parent.baseComponentId))
  }
}

object RepeatedComponentsDetails {
  def empty: RepeatedComponentsDetails = RepeatedComponentsDetails(Map.empty)
}
