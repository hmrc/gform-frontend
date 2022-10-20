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
  }

  // Error handling
  function handleError($input, msg, submitButton) {
    submitButton.css("display", "none");
    const errorEl = '<span class="govuk-error-message" role="alert">' + msg + "</span>";
    $(errorEl).insertBefore($input);
  }

  function submitFile(file, fileId, formAction) {
     const fileName = fileId + "_" + file.name.replace(/\\/g, "/").replace(/.*\//, "")
     const newFile = new File([file], fileName, {type: file.type});

     const formData = new FormData();
     formData.append(
         fileId,
         newFile,
         fileName
     );

     return $.ajax({
         type: "POST",
         url: formAction,
         data: formData,
         processData: false,
         contentType: false
     });
  }

  function submitForm(dataForm) {
     return $.ajax({
         type: dataForm.attr("method"),
         url: dataForm.attr("action"),
         data: dataForm.serialize()
     });
  }

  function handleFileUpload(e) {
    const form = $(e.target).closest("form");
    const submitButton = $('button[name=' + e.target.getAttribute('id') + "-uploadButton" + ']');
    const dataForm = $("#gf-form");

    const file = e.target.files[0];
    if (!file) {
      return false;
    }
    $(".govuk-error-message").remove();
    const $input = $(e.currentTarget);
    const maxFileSize = parseInt(
      $input.data("maxFileSizeMB") || window.gform.formMaxAttachmentSizeMB,
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
    submitButton.css("display", "");
    submitButton.on("click", function(e) {
        const formGroup = form.find(".govuk-form-group");
        const input = formGroup.find(".govuk-file-upload");
        const fileId = input.attr("id");
        const uploadedFiles = $("#" + fileId + "-files");

        submitButton.css("display", "none");
        formGroup.hide();
        uploadedFiles.empty().append(startProgressBar());

        submitForm(dataForm).done(function (){
            if (e.target.hasAttribute("upscan")) {
                submitButton.unbind("click")
                submitButton.on("click", function() { return true; });
                submitButton.click();
            } else {
                e.preventDefault();
                const formAction = submitButton.attr("formAction");
                submitFile(file, fileId, formAction).done(function () {
                    location.reload();
                });
            }
        });
    });
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
