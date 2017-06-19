;(function(global){'use strict'
var $=global.jQuery
var GOVUK=global.GOVUK||{}
function ShowHideContent(){var self=this
var selectors={namespace:'ShowHideContent',radio:'[data-target] > input[type="radio"]',checkbox:'[data-target] > input[type="checkbox"]'}
function escapeElementName(str){var result=str.replace('[','\\[').replace(']','\\]')
return result}
function initToggledContent(){var $control=$(this)
var $content=getToggledContent($control)
if($content.length){$control.attr('aria-controls',$content.attr('id'))
$control.attr('aria-expanded','false')
$content.attr('aria-hidden','true')}}
function getToggledContent($control){var id=$control.attr('aria-controls')
if(!id){id=$control.closest('[data-target]').data('target')}
return $('#'+id)}
function showToggledContent($control,$content){if($content.hasClass('js-hidden')){$content.removeClass('js-hidden')
$content.attr('aria-hidden','false')
if($control.attr('aria-controls')){$control.attr('aria-expanded','true')}}}
function hideToggledContent($control,$content){$content=$content||getToggledContent($control)
if(!$content.hasClass('js-hidden')){$content.addClass('js-hidden')
$content.attr('aria-hidden','true')
if($control.attr('aria-controls')){$control.attr('aria-expanded','false')}}}
function handleRadioContent($control,$content){var selector=selectors.radio+'[name='+escapeElementName($control.attr('name'))+'][aria-controls]'
var $form=$control.closest('form')
var $radios=$form.length?$form.find(selector):$(selector)
$radios.each(function(){hideToggledContent($(this))})
if($control.is('[aria-controls]')){showToggledContent($control,$content)}}
function handleCheckboxContent($control,$content){if($control.is(':checked')){showToggledContent($control,$content)}else{hideToggledContent($control,$content)}}
function init($container,elementSelector,eventSelectors,handler){$container=$container||$(document.body)
function deferred(){var $control=$(this)
handler($control,getToggledContent($control))}
var $controls=$(elementSelector)
$controls.each(initToggledContent)
$.each(eventSelectors,function(idx,eventSelector){$container.on('click.'+selectors.namespace,eventSelector,deferred)})
if($controls.is(':checked')){$controls.filter(':checked').each(deferred)}}
function getEventSelectorsForRadioGroups(){var radioGroups=[]
return $(selectors.radio).map(function(){var groupName=$(this).attr('name')
if($.inArray(groupName,radioGroups)===-1){radioGroups.push(groupName)
return'input[type="radio"][name="'+$(this).attr('name')+'"]'}
return null})}
self.showHideRadioToggledContent=function($container){init($container,selectors.radio,getEventSelectorsForRadioGroups(),handleRadioContent)}
self.showHideCheckboxToggledContent=function($container){init($container,selectors.checkbox,[selectors.checkbox],handleCheckboxContent)}
self.destroy=function($container){$container=$container||$(document.body)
$container.off('.'+selectors.namespace)}}
ShowHideContent.prototype.init=function($container){this.showHideRadioToggledContent($container)
this.showHideCheckboxToggledContent($container)}
GOVUK.ShowHideContent=ShowHideContent
global.GOVUK=GOVUK})(window)



var showHideContent = new GOVUK.ShowHideContent();
showHideContent.init();
