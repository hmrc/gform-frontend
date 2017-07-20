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
if($control.is('[aria-controls]')){showToggledContent($control,$content)
getToggledContent($radios.not($control)).find('input[type=text]').val('')}}
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

var uploader = function(el) {
  // constants
  var DEFAULT_UPLOAD_TEXT = 'Browse';
  var DEFAULT_CHANGE_TEXT = 'Change';
  var DEFAULT_MAX_FILE_SIZE = 1048576;
  var FORM_ERROR_CLASS = 'form-field-group--error';
  var FILE_URL = '/file-upload/upload/envelopes/{{envelopeId}}/files/{{fileId}}';
  var DEFAULT_LABEL = 'Your uploaded file will appear here';
  var DEFAULT_FILE_SIZE_ERROR = 'File exceeds max size allowed';

  // variables
  var formId = el.data('form-id');
  var fileId = el.data('file-id');
  var uploadText = el.data('uploadText') || DEFAULT_UPLOAD_TEXT;
  var changeText = el.data('changeText') || DEFAULT_CHANGE_TEXT;
  var initialText = el.data('initialText') || DEFAULT_UPLOAD_TEXT;
  var maxFileSize = parseInt(el.data('maxFileSize'), 10) || DEFAULT_MAX_FILE_SIZE;
  var uploaderLabel = el.data('label') || DEFAULT_LABEL;
  var fileSizeError = el.data('fileSizeError') || DEFAULT_FILE_SIZE_ERROR;

  // DOM elements
  var uploadedFileEl = el.find('.file-upload__file-list-item').eq(0);
  var fileLinks = uploadedFileEl.find('.file-upload__file-list-item-link');
  var uploadErrorsEl = el.find('.file-upload__errors').eq(0);
  var uploaderEl = $('<input id="' + fileId + '" type="file" class="file-upload__file" />');
  var uploaderBtn = $('<label for="' + fileId + '" class="file-upload__file-label">' + initialText + '</label>');

  var handleError = function(text) {
    var errorEl = '<span class="error-notification" role="alert">' + text + '</span>';

    uploaderBtn.html(uploadText);
    uploadedFileEl.empty().html(uploaderLabel);
    uploadErrorsEl.empty().append(errorEl);
    el.addClass(FORM_ERROR_CLASS);
    uploaderEl.prop('disabled', false);
  };

  // Upload the file when a new one is selected
  uploaderEl.on('change', function(evt) {
<<<<<<< HEAD
    console.log('CHANGE');
    // Get the file
    var file = evt.target.files[0];
    console.log('FILE', file)
=======
    // Get the file
    var file = evt.target.files[0];
>>>>>>> master
    var formData = new FormData();
    var fileUrl = FILE_URL
      .replace('{{envelopeId}}', window.gform.envelopeId)
      .replace('{{fileId}}', fileId);

<<<<<<< HEAD
    // Handle file upload cancel
    if (!file) return;

=======
>>>>>>> master
    // Show loading text and disable upload
    uploaderEl.prop('disabled', true);
    uploadedFileEl.html('Loading...');

    // Display error if file size is too big and don't upload it
<<<<<<< HEAD
    if (file.size > maxFileSize) {
=======
    if (file && file.size > maxFileSize) {
>>>>>>> master
      handleError(fileSizeError);
      return;
    }

    // Create a form data object with the file to upload
    formData.append(fileId, file);

    // Perform POST request
    $.ajax({
      url: fileUrl,
      type: 'POST',
      data: formData,
      processData: false,
      contentType: false,
      success: function(response) {
        uploaderBtn.html('Change document');
        uploadErrorsEl.empty();
        uploadedFileEl.html(file.name);
        el.removeClass(FORM_ERROR_CLASS);
        uploaderEl.prop('disabled', false);
      },
      error: function(err) {
        handleError(err.responseJSON.message);
      }
    });
  });

  // Append the upload input and button to the DOM
  el.append(uploaderEl).append(uploaderBtn);

  // Convert uploaded file links to plain text
  if (fileLinks.length) {
    fileLinks.contents().unwrap();
  }

  // Template changes the label if an error on load (need to keep for non-js version)
  // so we are going to have to update the label in this situation
  if (uploadedFileEl.contents().length && uploadErrorsEl.html().trim() !== '') {
    uploadedFileEl.empty().html(uploaderLabel);
  }
}

// Only use file uploader if browser supports it
if (window.File && window.FileList && window.FormData) {
  $('.file-uploader').each(function () {
    uploader($(this));
  });
}
