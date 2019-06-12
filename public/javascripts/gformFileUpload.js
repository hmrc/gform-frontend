;(function (global) {
  'use strict';

  var $ = global.jQuery;
  var GOVUK = global.GOVUK || {};
  var lang = global.gform && global.gform.lang || "en";
  var strings = {
    maxSizeError: {
      en: "This file is larger than the maximum file size of {0}MB",
      cy: "Mae’r ffeil hon yn fwy na maint y ffeil fwyaf a ganiateir sef {0}MB"
    },
    fileTypeError: {
      en: "The file type {0} is not permitted. You can only upload {1}",
      cy: "Ni chaniateir y math o ffeil {0}. Gallwch ond uwchlwytho {1}"
    },
    invalidFileToDelete: {
      en: "Could not delete file, file is invalid",
      cy: "Doedd dim modd dileu ffeil, mae’r ffeil yn annilys"
    },
    unexpectedError: {
      en: "An unexpected error occurred",
      cy: "Mae gwall annisgwyl wedi digwydd"
    },
    deleteLabel: {
      en: "Delete",
      cy: "Dileu"
    }
  };

  function interpolate(string, vars) {
    vars.forEach(function (v, index) {
      var token = '{' + index + '}';
      string = string.replace(token, v)
    });
    return string
  }

  var humanReadableMimeTypes = {

    'application/pdf': 'PDF',
    'image/jpeg': 'JPEG',
    'application/zip': 'ZIP',

    // Microsoft Office Mime Types
    'application/msword': 'DOC',
    'application/vnd.openxmlformats-officedocument.wordprocessingml.document': 'DOCX',
    'application/vnd.openxmlformats-officedocument.wordprocessingml.template': 'DOTX',
    'application/vnd.ms-word.document.macroEnabled.12': 'DOCM',
    'application/vnd.ms-word.template.macroEnabled.12': 'DOTM',
    'application/vnd.ms-excel': 'XLS',
    'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet': 'XLSX',
    'application/vnd.openxmlformats-officedocument.spreadsheetml.template': 'XLTX',
    'application/vnd.ms-excel.sheet.macroEnabled.12': 'XLSM',
    'application/vnd.ms-excel.template.macroEnabled.12': 'XLTM',
    'application/vnd.ms-excel.addin.macroEnabled.12': 'XLAM',
    'application/vnd.ms-excel.sheet.binary.macroEnabled.12': 'XLSB',
    'application/vnd.ms-powerpoint': 'PPT',
    'application/vnd.openxmlformats-officedocument.presentationml.presentation': 'PPTX',
    'application/vnd.openxmlformats-officedocument.presentationml.template': 'POTX',
    'application/vnd.openxmlformats-officedocument.presentationml.slideshow': 'PPSX',
    'application/vnd.ms-powerpoint.addin.macroEnabled.12': 'PPAM',
    'application/vnd.ms-powerpoint.presentation.macroEnabled.12': 'PPTM',
    'application/vnd.ms-powerpoint.template.macroEnabled.12': 'potm',
    'application/vnd.ms-powerpoint.slideshow.macroEnabled.12': 'ppsm',
    'application/vnd.ms-access': 'MDB',

    // Open Office Mime Types
    'application/vnd.oasis.opendocument.text': 'ODT',
    'application/vnd.oasis.opendocument.text-template': 'OTT',
    'application/vnd.oasis.opendocument.text-web': 'OTH',
    'application/vnd.oasis.opendocument.text-master': 'ODM',
    'application/vnd.oasis.opendocument.graphics': 'ODG',
    'application/vnd.oasis.opendocument.graphics-template': 'OTG',
    'application/vnd.oasis.opendocument.presentation': 'ODP',
    'application/vnd.oasis.opendocument.presentation-template': 'OTP',
    'application/vnd.oasis.opendocument.spreadsheet': 'ODS',
    'application/vnd.oasis.opendocument.spreadsheet-template': 'OTS',
    'application/vnd.oasis.opendocument.chart': 'ODC',
    'application/vnd.oasis.opendocument.formula': 'ODF',
    'application/vnd.oasis.opendocument.database': 'ODB',
    'application/vnd.oasis.opendocument.image': 'ODI',
    'application/vnd.openofficeorg.extension': 'OXT',

    // Document Extensions
    '.doc': 'DOC',
    '.docx': 'DOCX',
    '.dotx': 'DOTX',
    '.docm': 'DOCM',
    '.dotm': 'DOTM',
    '.xls': 'XLS',
    '.xlsx': 'XLSX',
    '.xltx': 'XLTX',
    '.xlsm': 'XLSM',
    '.xltm': 'XLTM',
    '.xlam': 'XLAM',
    '.xlsb': 'XLSB',
    '.ppt': 'PPT',
    '.pptx': 'PPTX',
    '.potx': 'POTX',
    '.ppsx': 'PPSX',
    '.ppam': 'PPAM',
    '.pptm': 'PPTM',
    '.potm': 'potm',
    '.ppsm': 'ppsm',
    '.mdb': 'MDB',

    '.odt': 'ODT',
    '.ott': 'OTT',
    '.oth': 'OTH',
    '.odm': 'ODM',
    '.odg': 'ODG',
    '.otg': 'OTG',
    '.odp': 'ODP',
    '.otp': 'OTP',
    '.ods': 'ODS',
    '.ots': 'OTS',
    '.odc': 'ODC',
    '.odf': 'ODF',
    '.odb': 'ODB',
    '.odi': 'ODI',
    '.oxt': 'OXT',

    // lower to upper
    'doc': 'DOC',
    'docx': 'DOCX',
    'dotx': 'DOTX',
    'docm': 'DOCM',
    'dotm': 'DOTM',
    'xls': 'XLS',
    'xlsx': 'XLSX',
    'xltx': 'XLTX',
    'xlsm': 'XLSM',
    'xltm': 'XLTM',
    'xlam': 'XLAM',
    'xlsb': 'XLSB',
    'ppt': 'PPT',
    'pptx': 'PPTX',
    'potx': 'POTX',
    'ppsx': 'PPSX',
    'ppam': 'PPAM',
    'pptm': 'PPTM',
    'potm': 'potm',
    'ppsm': 'ppsm',
    'mdb': 'MDB',

    'odt': 'ODT',
    'ott': 'OTT',
    'oth': 'OTH',
    'odm': 'ODM',
    'odg': 'ODG',
    'otg': 'OTG',
    'odp': 'ODP',
    'otp': 'OTP',
    'ods': 'ODS',
    'ots': 'OTS',
    'odc': 'ODC',
    'odf': 'ODF',
    'odb': 'ODB',
    'odi': 'ODI',
    'oxt': 'OXT'
  }

  var submitButton = $('input[type=submit], button[type=submit]')

  function disableSubmitButton() {
    submitButton.attr('disabled', true)
  }

  function enableSubmitButton() {
    submitButton.removeAttr('disabled')
  }

  function mapUserFriendlyFileTypes(array) {
    return array
        .split(', ')
        .map(function(a) {
          if (humanReadableMimeTypes.hasOwnProperty(a)) return humanReadableMimeTypes[a]
          return a // default to mime type if no friendly value is found
        })
  }

  function removeDuplicatesFromArray(array) {
    return array.filter(function(item, index) {
      return array.indexOf(item) >= index
    })
  }

  function transformMimeTypes(mimetypes) {
    return removeDuplicatesFromArray(mapUserFriendlyFileTypes(mimetypes)).join(', ')
  }



  function GformFileUpload () {
    var self = this

    // Set up event handlers etc
    function init () {
      $('.file-upload').on('change', handleFileUpload);
      $('.uploaded-files').on('click', '.delete-file', handleFileDelete);
    }


    // Error handling
    function handleError ($input, msg) {
      enableSubmitButton()
      var errorEl = '<span class="error-message file-upload-error" role="alert">' + msg + '</span>';
      $(errorEl).insertBefore($input)
    }


    // Setup file upload
    function handleFileUpload (e) {
      var file = e.target.files[0];
      if (!file) {
        return false;
      }
      disableSubmitButton();
      $('.file-upload-error').remove();
      var $input = $(e.currentTarget);
      var formTemplateId = $input[0].dataset.formTemplateId;
      var accessCode = $input[0].dataset.accessCode;
      var maxFileSize = parseInt($input.data('maxFileSizeMB') || window.gform.formMaxAttachmentSizeMB, 10);

      $input.attr('aria-busy', true);

      if (window.gform.contentTypes.indexOf(file.type) === -1) {
        return handleError($input, interpolate(strings.fileTypeError[lang], [transformMimeTypes(file.type), transformMimeTypes(window.gform.contentTypes)]));
      }

      if (file.size > (maxFileSize * 1024 * 1024)) {
        return handleError($input, interpolate(strings.maxSizeError[lang], [maxFileSize]));
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
      enableSubmitButton()
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
      return $('<span>' + name + '</span> <a href="#" class="delete-file" data-file-id="' + fileId + '" data-form-id="' + formTemplateId + '" data-access-code="' + accessCode + '"><span aria-hidden="true">' + strings.deleteLabel[lang] + '</span><span class="visuallyhidden">' + strings.deleteLabel[lang] + ' ' + name + '</span></a>')
    }

    // Handle file deletion
    function handleFileDelete (e) {
      e.preventDefault();

      disableSubmitButton()

      var t = $(e.currentTarget);
      var d = e.currentTarget.dataset;

      t.attr('aria-busy', 'true');

      if (!d.fileId) {
        handleError($('#' + d.formId), strings.invalidFileToDelete[lang]);
      }

      var deleteUrl = '/submissions/api/forms/' + d.formId + '/' + d.accessCode + '/deleteFile/' + d.fileId + '';

      return fileDelete (deleteUrl)
      .then(function(response) {
        fileDeleteSuccess(d.fileId, t)
      }, function (err) {
        t.removeAttr('aria-busy')
        handleError($('#' + d.fileId), err.responseJSON && err.responseJSON.message ? err.responseJSON.message : strings.unexpectedError[lang]);
      })
    }

    // Delete file
    function fileDelete (deleteUrl) {
      return $.ajax({
        url: deleteUrl,
        type: 'DELETE'
      });
    }

    // File deletion succeeded
    function fileDeleteSuccess (fileId) {
      enableSubmitButton()
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