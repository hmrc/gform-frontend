;(function (global) {
  'use strict'

  var $ = global.jQuery
  var GOVUK = global.GOVUK || {}

  function GformFileUpload () {
    var self = this

    // Set up event handlers etc
    function init () {
      $('.file-upload').on('change', handleFileUpload);
      $('.uploaded-files').on('click', '.delete-file', handleFileDelete);
    }


    // Error handling
    function handleError ($input, msg) {
      var errorEl = '<span class="error-message file-upload-error" role="alert">' + msg + '</span>';
      $(errorEl).insertBefore($input)
    }


    // Setup file upload
    function handleFileUpload (e) {
      $('.file-upload-error').remove();

      var file = e.target.files[0];
      var $input = $(e.currentTarget);
      var formTemplateId = $input[0].dataset.formTemplateId;
      var accessCode = $input[0].dataset.accessCode;
      var maxFileSize = parseInt($input.data('maxFileSizeMB') || window.gform.formMaxAttachmentSizeMB, 10);

      $input.attr('aria-busy', true);

      if (window.gform.contentTypes.indexOf(file.type) === -1) {
        return handleError($input, 'The file type ' + file.type + ' is not permitted. You can only upload ' + window.gform.contentTypes);
      }

      if (file.size > (maxFileSize * 1024 * 1024)) {
        return handleError($input, 'This file is larger than the maximum file size of ' + maxFileSize + 'MB');
      }

      return uploadFile(file, $input.attr('id'))
        .then(function (response) {
          fileUploadSuccess(response, $input.attr('id'), file.name, formTemplateId, $input, accessCode);
        }, function (err) {
          $input.removeAttr('aria-busy')
          handleError($input, err.statusText)
        }) 
    }


    // Handle file upload request
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

    // Handle successful file upload
    function fileUploadSuccess (response, fileId, name, formTemplateId, input, accessCode) {
      input.removeAttr('aria-busy')

      $('#' + fileId + '-files')
        .addClass('subsection')
        .empty()
        .append(makeFileEntry(name, fileId, formTemplateId, accessCode))
        .attr('tabIndex', '-1')
        .trigger('focus');
    }
    
    // Display the uploaded file name and delete button
    function makeFileEntry(name, fileId, formTemplateId, accessCode) {
      return $('<span>' + name + '</span> <a href="#" class="delete-file" data-file-id="' + fileId + '" data-form-id="' + formTemplateId + '" data-access-code="' + accessCode + '"><span aria-hidden="true">Delete</span><span class="visuallyhidden">Delete ' + name + '</span></a>')
    }

    // Handle file deletion
    function handleFileDelete (e) {
      e.preventDefault();

      var t = $(e.currentTarget);
      var d = e.currentTarget.dataset;

      t.attr('aria-busy', 'true');

      if (!d.fileId) {
        handleError($('#' + d.formId), 'Could not delete file, file is invalid');
      }

      var deleteUrl = '/submissions/api/forms/' + d.formId + '/' + d.accessCode + '/deleteFile/' + d.fileId + '';

      return fileDelete (deleteUrl)
      .then(function(response) {
        fileDeleteSuccess(d.fileId, t)
      }, function (err) {
        t.removeAttr('aria-busy')
        handleError($('#' + d.fileId), err.responseJSON && err.responseJSON.message ? err.responseJSON.message : 'An unexpected error occurred');
      })
    }

    // Delete file
    function fileDelete (deleteUrl) {
      return $.ajax({
        url: deleteUrl,
        type: 'DELETE',
      });
    }

    // File deletion succeeded
    function fileDeleteSuccess (fileId) {
      $('#' + fileId + '-files').empty();
      $('#' + fileId).val('');
    }
    
    // Set up file upload
    self.initFileUpload = function () {
      init()
    }

  }

  GformFileUpload.prototype.init = function () {
    this.initFileUpload()
  }

  GOVUK.GformFileUpload = GformFileUpload
  global.GOVUK = GOVUK
})(window)