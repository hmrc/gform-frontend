(function(global) {
  "use strict";

  var $ = global.jQuery;
  var GOVUK = global.GOVUK || {};
  var lang = (global.gform && global.gform.lang) || "en";
  var strings = {
    uploadingFile: {
      en: "Uploading file",
      cy: "Wrthi’n uwchlwytho’r ffeil"
    }
  };

 function GformFileUpload() {
  var self = this;

  // Set up event handlers etc
  function init() {
    $('button[name$="-uploadButton"]').css("display", "none")
    $(".govuk-file-upload").on("change", handleFileUpload);
  }

  var fileSubmit = function(form, button) {
    var formGroup = form.find(".govuk-form-group");
    var input = formGroup.find(".govuk-file-upload");
    var formComponentId = input.attr("id");
    var uploadedFiles = $("#" + formComponentId + "-files");

    button.css("display", "none");
    formGroup.hide();
    uploadedFiles.empty().append(startProgressBar());

    return true;
  }

  var dataSubmit = function(form, dataForm, button) {
    $.ajax({
       type: dataForm.attr("method"),
       url: dataForm.attr("action"),
       data: dataForm.serialize()
    }).then(function (){
       button.on("click", function(e) {
         fileSubmit(form, button);
       });
       button.click();
    });
  }

  function handleFileUpload(e) {
    var form = $(e.target).closest("form")
    var submitButton = form.find(".govuk-button--secondary")
    var dataForm = $("#gf-form");
    submitButton.css("display", "")
    submitButton.on("click", function(e) {
       dataSubmit(form, dataForm, submitButton)
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
