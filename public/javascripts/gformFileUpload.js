(function(global) {
  "use strict";

  var $ = global.jQuery;
  var GOVUK = global.GOVUK || {};
  var lang = (global.gform && global.gform.lang) || "en";
  var strings = {
    uploadingFile: {
      en: "Uploading file",
      cy: "Wrthi’n uwchlwytho’r ffeil"
    },
    emptyFileSizeError: {
      en: "This file is empty",
      cy: "Mae'r ffeil hon yn wag"
    },
    maxSizeError: {
      en: "This file is larger than the maximum file size of {0}MB",
      cy: "Mae’r ffeil hon yn fwy na maint y ffeil fwyaf a ganiateir sef {0}MB"
    }
  };

  function interpolate(string, vars) {
    vars.forEach(function(v, index) {
      const token = "{" + index + "}";
      string = string.replace(token, v);
    });
    return string;
  }

 function GformFileUpload() {
  var self = this;

  // Set up event handlers etc
  function init() {
    $('button[name$="-uploadButton"]').css("display", "none")
    $(".govuk-file-upload").on("change", handleFileUpload);
    $('button[name$="singleFile"]').on("click", handleEmptyFileUpload);
  }

   function handleEmptyFileUpload(e) {
     var inputFile = $('.govuk-file-upload')[0];
     if (inputFile.files.length === 0) {
       const form = $(e.target).closest("form");
       const errorRedirect = form.find('input[name="error_action_redirect"]').val();
       const params = {
           errorMessage: "Missing file",
           errorCode: "MissingFile",
       };
       const queryString = $.param(params);
       const errorUrl = errorRedirect + "?" + queryString;
       window.location.href = errorUrl;
       e.preventDefault();
     } else {
       singleFileUploadProgress(e);
     }
   }

  // Error handling
  function handleError($input, msg, submitButton) {
    submitButton.css("display", "none");
    const errorEl = '<span class="govuk-error-message" role="alert">' + msg + "</span>";
    $(errorEl).insertBefore($input);
  }

  function fileSubmit(form, button) {
    const formGroup = form.find(".govuk-form-group");
    const input = formGroup.find(".govuk-file-upload");
    const formComponentId = input.attr("id");
    const uploadedFiles = $("#" + formComponentId + "-files");

    button.css("display", "none");
    input.hide();
    uploadedFiles.empty().append(startProgressBar());

    return true;
  }

  function dataSubmit(form, dataForm, button) {
    $.ajax({
       type: dataForm.attr("method"),
       url: dataForm.attr("action"),
       data: dataForm.serialize()
    }).then(function (){
       button.unbind("click")
       button.on("click", function(e) {
         fileSubmit(form, button);
       });
       button.click();
    });
  }

  function handleFileUpload(e) {
    const form = $(e.target).closest("form");
    const id = e.target.getAttribute('id');
    const submitButton = $('button[name=' + id + "-uploadButton" + ']');
    const dataForm = $("#gf-form");

    const file = e.target.files[0];
    if (!file) {
      return false;
    }
    $(".govuk-error-message").remove();
    const $input = $(e.currentTarget);
    const maxFileSize = parseInt(
      window.gform.fileUploadMaxSize.get(id),
      10
    );

    if (file.size == 0) {
      return handleError($input, strings.emptyFileSizeError[lang], submitButton);
    }

    if (file.size > maxFileSize * 1024 * 1024) {
      return handleError(
        $input,
        interpolate(strings.maxSizeError[lang], [maxFileSize]),
        submitButton
      );
    }
    submitButton.css("display", "")
    submitButton.on("click", function(e) {
       dataSubmit(form, dataForm, submitButton)
    });
  }

   function singleFileUploadProgress(e) {
     const form = $(e.target).closest("form");
     const formGroup = form.find(".govuk-form-group");
     const input = formGroup.find(".govuk-file-upload");
     const formComponentId = input.attr("id");
     const uploadedFiles = $("#" + formComponentId + "-files");

     input.hide();
     uploadedFiles.empty().append(startProgressBar());
   }
  // Display uploading file message
  function startProgressBar() {
    return progressBarWrapper("<span class='app-progress-spinner'></span><span id='fileupload' role='alert'>" + strings.uploadingFile[lang] + "</span>", "");
  }

  function progressBarWrapper(messageContent, buttonContent) {
    return $(
      "<dl class='govuk-summary-list pp-file-upload-spinner__list'>" +
        "<div class='govuk-summary-list__row'>" +
          "<dd class='app-summary-list__spinner'>" +
            messageContent +
          "</dd>" +
          "<dd class='app-summary-list__spinner'>" +
            buttonContent +
          "</dd>" +
        "</div>" +
      "</dl>"
    );
  }

  // Set up file upload
  self.initFileUpload = function() {
    init();
    };
  }

  GformFileUpload.prototype.init = function() {
    this.initFileUpload();
  };

  GOVUK.GformFileUpload = GformFileUpload;
  global.GOVUK = GOVUK;
})(window);
