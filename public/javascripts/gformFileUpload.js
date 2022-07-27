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

  function handleFileUpload(e) {
    var form = $(e.target).closest("form")
    var formGroup = form.find(".govuk-form-group")
    var input = formGroup.find(".govuk-file-upload")
    var submitButton = form.find(".govuk-button--secondary")
    var formComponentId = input.attr("id");
    var uploadedFiles = $("#" + formComponentId + "-files")
    var frm = $("#gf-form");
    submitButton.css("display", "")
    submitButton.on("click", function(evt) {
      formGroup.hide()
      uploadedFiles.empty().append(startProgressBar());
      submitButton.css("display", "none")
      evt.preventDefault();

      $.ajax({
          type: frm.attr('method'),
          url: frm.attr('action'),
          data: frm.serialize()
      }).then(function (){
          submitButton.submit();
      });
      return false;
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
