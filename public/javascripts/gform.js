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

var gfForm = $('#gf-form');
var gfFormAction = $('#gform-action');

var gform = window.gform || {};
var formMaxAttachmentSizeMB = parseInt(window.gform.formMaxAttachmentSizeMB || 1, 10);

var details = $('details');

var showHideContent = new GOVUK.ShowHideContent();




// ERROR HANDLING
var handleError = function($input, msg) {
  var errorEl = '<span class="error-message file-upload-error" role="alert">' + msg + '</span>';
  $(errorEl).insertBefore($input)

};


// UPLOAD
function handleFileUpload(e) {
  $('.file-upload-error').remove();
  
  var file = e.target.files[0];
  var $input = $(e.currentTarget);
  var formTemplateId = $input[0].dataset.formTemplateId;
  accessCode = $input[0].dataset.accessCode;
  var maxFileSize = parseInt($input.data('maxFileSizeMB') || window.gform.formMaxAttachmentSizeMB, 10);

  $input.attr('aria-busy', true);

  if (window.gform.contentTypes.indexOf(file.type) === -1) {
    return handleFileUploadError($input, 'The file type ' + file.type + ' is not permitted. You can only upload ' + window.gform.contentTypes);
  }

  if (file.size > (maxFileSize * 1024 * 1024)) {
    return handleFileUploadError($input, 'This file is larger than the maximum file size of ' + maxFileSize + 'MB');
  }
  
  return uploadFile(file, $input.attr('id'))
    .then(function (response) {
      fileUploadSuccess(response, $input.attr('id'), file.name, formTemplateId, $input);
    }, function (err) {
      $input.removeAttr('aria-busy')
      handleFileUploadError($input, err.statusText)
    })
}

function fileUploadSuccess(response, fileId, name, formTemplateId, input) {
  input.removeAttr('aria-busy')

  $('#' + fileId + '-files')
    .addClass('subsection')
    .empty()
    .append(makeFileEntry(name, fileId, formTemplateId, accessCode))
    .attr('tabIndex', '-1')
    .trigger('focus');
}

function makeFileEntry(name, fileId, formTemplateId, accessCode) {
  return $('<span>' + name + '</span> <a href="#" class="delete-file" data-file-id="' + fileId + '" data-form-id="' + formTemplateId + '" data-access-code="' + accessCode + '"><span aria-hidden="true">Delete</span><span class="visuallyhidden">Delete ' + name + '</span></a>')
}

function uploadFile(file, fileId) {
  var formData = new FormData();
  formData.append(fileId, file, file.name.replace(/\\/g,'/').replace(/.*\//, ''));
  return $.ajax({
    url: '/file-upload/upload/envelopes/' + window.gform.envelopeId + '/files/' + fileId,
    type: 'POST',
    data: formData,
    processData: false,
    contentType: false
  });
}



// DELETE
function handleFileDelete(e) {
  e.preventDefault();

  var t = $(e.currentTarget);
  var d = e.currentTarget.dataset;

  t.attr('aria-busy', 'true');

  if (!d.fileId) {
    handleError($('#' + d.formId), 'Could not delete file, file is invalid');
  }

  var deleteUrl = '/submissions/api/forms/' + d.formId + '/' + d.accessCode + '/deleteFile/' + d.fileId + '';

  return fileDelete(deleteUrl)
  .then(function(response) {
    fileDeleteSuccess(d.fileId, t)
  }, function (err) {
    t.removeAttr('aria-busy')
    handleError($('#' + d.fileId), err.responseJSON && err.responseJSON.message ? err.responseJSON.message : 'An unexpected error occurred');
  })
}

function fileDelete(deleteUrl) {
  return $.ajax({
    url: deleteUrl,
    type: 'DELETE',
  });
}

function fileDeleteSuccess(fileId, deleteLink) {  
  $('#' + fileId + '-files').empty();
  $('#' + fileId).val('');
}

$('.file-upload').on('change', handleFileUpload);
$('.uploaded-files').on('click', '.delete-file', handleFileDelete);




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

// wrapper function to ensure ga is
// available in the environment
function sendToGA() {
  if (typeof ga === 'function') {
    ga.apply(null, arguments)
  }
}

function sendErrorToGA($errorLink) {
  // Google Analytics event reporting, using template:
  // ga('send', 'event', [eventCategory], [eventAction], [eventLabel], [eventValue], [fieldsObject])
  sendToGA('send', 'event', 'error - field', labelText($errorLink), $errorLink.text().trim())
}

function labelText($errorLink) {
  var fieldId = $errorLink.attr('data-focuses');
  var $label = $('label[for="' + fieldId + '"]');
  if ($label.length) {
    return $label.text().trim();
  }
  var $legend = $('fieldset#' + fieldId).find('legend.form-label').first();
  if ($legend.length) {
    return $legend.text().trim()
  }

  return $('h1').text().trim();

}

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
        $('[name="' + focuses + '"]').first().trigger('focus')
      });
      sendErrorToGA($link);
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
