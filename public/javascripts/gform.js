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
global.GOVUK=GOVUK})(window);

var ERROR_CODES = {
  FILE_TOO_LARGE: 413
};
var FORM_ERROR_CLASS = 'form-field-group--error';
var FILE_URL = '/file-upload/upload/envelopes/{{envelopeId}}/files/{{fileId}}';
var FILE_DELETE_URL = '/submissions/api/forms/{{formId}}/deleteFile/{{fileId}}';
var gfForm = $('#gf-form');
var gfFormAction = $('#gform-action');

var gform = window.gform || {};
var formMaxAttachmentSizeMB = parseInt(window.gform.formMaxAttachmentSizeMB || 1, 10);

var uploaderDefaults = {
  uploadText: 'Browse',
  changeText: 'Change',
  maxFileSize: formMaxAttachmentSizeMB * 1024 * 1024,
  uploaderLabel: 'Your uploaded file will appear here',
  maxFileSizeError: 'File exceeds max size allowed',
  contentTypes: window.gform.contentTypes
};

var details = $('details');

var showHideContent = new GOVUK.ShowHideContent();

var uploader = function(el) {
  // variables
  var formId = el.data('form-id');
  var fileId = el.data('file-id');

  var config = $.extend({}, uploaderDefaults, {
    uploadText: el.data('uploadText'),
    changeText: el.data('changeText'),
    maxFileSize: parseInt(el.data('maxFileSize'), 10),
    fileSizeError: el.data('fileSizeError'),
    initialText: el.data('initialText'),
    defaultUploaderLabel: el.data('default-label')
  });

  // DOM elements
  var uploadedFileEl = el.find('.file-upload__file-list-item').eq(0);
  var fileLinks = uploadedFileEl.find('.file-upload__file-list-item-link');
  var uploadErrorsEl = el.find('.file-upload__errors').eq(0);
  var uploaderBtn = $('<label for="' + fileId + '" class="file-upload__file-label form-label">' + config.initialText + '</label>');
  var uploaderEl = $('<input id="' + fileId + '" type="file" class="file-upload__file" accept="'+config.contentTypes+'"/>');
  var deleteBtnEl = $('<a href="#" class="gf-delete" data-file-id="' + fileId + '">Delete</a>');

  var handleError = function(text) {
    var errorEl = '<span class="error-notification error-message" role="alert">' + text + '</span>';

    uploaderBtn.html(config.uploadText);
    uploadedFileEl.empty();
    uploadErrorsEl.empty().append(errorEl);
    el.addClass(FORM_ERROR_CLASS);
    uploaderEl.prop('disabled', false);
  };

  var isEmptyEl = function(el) {
    return el.html().trim() === '';
  }

  el.on('click', '.gf-delete', function(evt) {
    evt.preventDefault();

    var currentFile = uploadedFileEl.clone();
    var deleteFileUrl = FILE_DELETE_URL
      .replace('{{formId}}', formId)
      .replace('{{fileId}}', fileId);

    if (!fileId) {
      handleError('Could not delete file, file is invalid');
      return;
    }

    // Show loading text and disable upload
    uploaderEl.prop('disabled', true);
    uploadedFileEl.html('Deleting file...');

    // Perform DELETE request
    $.ajax({
      url: deleteFileUrl,
      type: 'DELETE',
      success: function(response) {
        uploaderBtn.html(config.uploadText);
        uploadErrorsEl.empty();
        uploadedFileEl.empty();
        el.removeClass(FORM_ERROR_CLASS);
        uploaderEl.prop('disabled', false);
      },
      error: function(err) {
        var errorMsg = err.responseJSON && err.responseJSON.message
          ? err.responseJSON.message
          : 'An unexpected error occurred';

        handleError(errorMsg);

        // Revert DOM to display previous file, as it was not deleted
        uploadedFileEl.html(currentFile);
      }
    });
  });

  // Upload the file when a new one is selected
  uploaderEl.on('change', function(evt) {
    // Get the file
    var file = evt.target.files[0];
    var formData = new FormData();
    var fileUrl = FILE_URL
      .replace('{{envelopeId}}', window.gform.envelopeId)
      .replace('{{fileId}}', fileId);

    // Handle file upload cancel
    if (!file) return;

    // Show loading text and disable upload
    uploaderEl.prop('disabled', true);
    uploadedFileEl.html('Loading...');

    // Display error if file size is too big and don't upload it
    if (file.size > config.maxFileSize) {
      handleError(config.fileSizeError);
      return;
    }

    // Create a form data object with the file to upload
    formData.append(fileId, file, file.name.replace(/\\/g,'/').replace(/.*\//, ''));

    // Perform POST request
    $.ajax({
      url: fileUrl,
      type: 'POST',
      data: formData,
      processData: false,
      contentType: false,
      success: function(response) {
        uploaderBtn.html(config.changeText);
        uploadErrorsEl.empty();
        uploadedFileEl.html('<span>' + file.name + '</span>');
        uploadedFileEl.append(deleteBtnEl);
        el.removeClass(FORM_ERROR_CLASS);
        uploaderEl.prop('disabled', false);
      },
      error: function(err) {
        if (err.responseJSON && err.responseJSON.message)  {
          handleError(err.responseJSON.message);
          return;
        }

        switch(err.status) {
          case ERROR_CODES.FILE_TOO_LARGE:
            handleError(config.fileSizeError);
            break;
          default:
            handleError('An unexpected error occurred, your file could not be uploaded');
            break;
        }

      }
    });
  });

  // Append the upload input and button to the DOM
  el.append(uploaderBtn).append(uploaderEl);

  // Convert uploaded file links to plain text
  if (fileLinks.length) {
    fileLinks.text(function() {
      return $.trim($(this).text());
    }).contents().unwrap().wrap('<span>');
  }

  // Template changes the label if an error on load (need to keep for non-js version)
  // so we are going to have to update the label in this situation
  if (uploadedFileEl.text().trim() === config.defaultUploaderLabel) {
    uploadedFileEl.empty();
  } else {
    uploadedFileEl.append(deleteBtnEl);
  }
}

// Only use file uploader if browser supports it
if (window.File && window.FileList && window.FormData) {
  $('.file-uploader').each(function () {
    uploader($(this));
  });
}

// Add `aria-hidden` attribute to hidden content to ensure screen readers can 'see' the content
details.on('click', function(evt) {
  var summaryEl = $(evt.currentTarget).find('summary');
  var isExpanded = summaryEl.attr('aria-expanded') === "true";
  var summaryControls = summaryEl.attr('aria-controls');
  var toggleTarget = summaryEl.next('#' + summaryControls);

  if (isExpanded) {
    toggleTarget.attr('aria-hidden', false);
  } else {
    toggleTarget.attr('aria-hidden', true);
  }
});

// Fix to POST the submit type. In browsers other than Safari, you can post the value
// of the submit button. This workaround sets the value on a hidden input instead as
// gforms works by using different submit values
gfForm.on('click', '[type="submit"]', function(evt) {
  var type = $(evt.target).val();
  gfFormAction.val(type);
});

// Manually submit the form with the action saving the form and taking the user back one screen
$('#backButton').on("click",function(e){
    e.preventDefault();
    gfFormAction.attr('value', 'Back');
    gfForm.submit();
});

// Manually submit the form with the action taking the user back to the summary page
$('#BackToSummary').on('click', function(e) {
    e.preventDefault();
    gfFormAction.attr('value', 'BackToSummary');
    gfForm.submit();
})

$('#saveComeBackLater').on('click', function(e) {
  e.preventDefault();
  gfFormAction.attr('value', 'Save');
  gfForm.submit();
});

$('#saveComeBackLaterExit').on('click', function(e) {
  e.preventDefault();
  gfFormAction.attr('value', 'Exit');
  gfForm.submit();
});

// Add focus class to file upload label on input focus for outline in firefox
$('.file-upload__file').focus(function(){
    $(this).siblings('.file-upload__file-label').addClass('focus');
})

$('.file-upload__file').blur(function(){
    $(this).siblings('.file-upload__file-label').removeClass('focus');
});

// Avoid the add/remove repeating group from bering triggered on submit, unless in focus
$('.removeRepeatingSection, #addRepeatingGroup').attr('type', 'button');

$('.removeRepeatingSection, #addRepeatingGroup').focus(function(){
  $(this).attr('type', 'submit');
});

$('.removeRepeatingSection, #addRepeatingGroup').blur(function(){
  $(this).attr('type', 'button');
});

showHideContent.init();

function setUpErrorSummary($el) {
  var links = [];
  // remove multiple links to same context
  $el.find('a').each(function (i, link) {
    var $link = $(link);
    var context = $link.attr('data-context');
    if (links.indexOf(context) === -1) {
      links.push(context);
      $link.removeClass('js-hidden').on('click', function () {
        var focuses = $(this).attr('data-focuses');
        $('#' + focuses).trigger('focus')
      })
    } else {
      $link.parents('li').remove()
    }
  });
  // focus on the error summary
  $el.trigger('focus');
  // remove subsequent inline error messages for same field
  $('.error-message + .error-message').remove()
}

setUpErrorSummary($('.error-summary'));

function showLabel($label) {
  $label.removeClass('js-hidden').attr('aria-hidden', false)
}

function hideLabel($label) {
  $label.addClass('js-hidden').attr('aria-hidden', true)
}

function toggleAddressLabels(e) {
  var choice = $(e.target).attr('data-address-choice');
  $('span[data-address-label]').each(function (i, label) {
    $(label).attr('data-address-label') === choice ? showLabel($(label)) : hideLabel($(label))
  })
}

$('input[data-address-choice]').on('click', toggleAddressLabels);

