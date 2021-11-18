(function(global) {
  "use strict";

  var $ = global.jQuery;
  var GOVUK = global.GOVUK || {};
  var lang = (global.gform && global.gform.lang) || "en";
  var strings = {
    emptyFileSizeError: {
      en: "This file is empty",
      cy: "Mae'r ffeil hon yn wag"
    },
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
    upscanError: {
      en: "You can only upload {0}",
      cy: "Gallwch ond uwchlwytho {0}"
    },
    unexpectedError: {
      en: "An unexpected error occurred",
      cy: "Mae gwall annisgwyl wedi digwydd"
    },
    deleteLabel: {
      en: "Delete",
      cy: "Dileu"
    },
    uploadingFile: {
      en: "Uploading file",
      cy: "Wrthi’n uwchlwytho’r ffeil"
    },
    hasBeenUploaded: {
      en: "{0} has been uploaded",
      cy: "Uwchlwythwyd {0}"
    }
  };

  function interpolate(string, vars) {
    vars.forEach(function(v, index) {
      var token = "{" + index + "}";
      string = string.replace(token, v);
    });
    return string;
  }

  var humanReadableMimeTypes = {
    "application/pdf": "PDF",
    "image/jpeg": "JPEG",
    "application/zip": "ZIP",

    // Microsoft Office Mime Types
    "application/msword": "DOC",
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document":
      "DOCX",
    "application/vnd.openxmlformats-officedocument.wordprocessingml.template":
      "DOTX",
    "application/vnd.ms-word.document.macroEnabled.12": "DOCM",
    "application/vnd.ms-word.template.macroEnabled.12": "DOTM",
    "application/vnd.ms-excel": "XLS",
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet": "XLSX",
    "application/vnd.openxmlformats-officedocument.spreadsheetml.template":
      "XLTX",
    "application/vnd.ms-excel.sheet.macroEnabled.12": "XLSM",
    "application/vnd.ms-excel.template.macroEnabled.12": "XLTM",
    "application/vnd.ms-excel.addin.macroEnabled.12": "XLAM",
    "application/vnd.ms-excel.sheet.binary.macroEnabled.12": "XLSB",
    "application/vnd.ms-powerpoint": "PPT",
    "application/vnd.openxmlformats-officedocument.presentationml.presentation":
      "PPTX",
    "application/vnd.openxmlformats-officedocument.presentationml.template":
      "POTX",
    "application/vnd.openxmlformats-officedocument.presentationml.slideshow":
      "PPSX",
    "application/vnd.ms-powerpoint.addin.macroEnabled.12": "PPAM",
    "application/vnd.ms-powerpoint.presentation.macroEnabled.12": "PPTM",
    "application/vnd.ms-powerpoint.template.macroEnabled.12": "potm",
    "application/vnd.ms-powerpoint.slideshow.macroEnabled.12": "ppsm",
    "application/vnd.ms-access": "MDB",

    // Open Office Mime Types
    "application/vnd.oasis.opendocument.text": "ODT",
    "application/vnd.oasis.opendocument.text-template": "OTT",
    "application/vnd.oasis.opendocument.text-web": "OTH",
    "application/vnd.oasis.opendocument.text-master": "ODM",
    "application/vnd.oasis.opendocument.graphics": "ODG",
    "application/vnd.oasis.opendocument.graphics-template": "OTG",
    "application/vnd.oasis.opendocument.presentation": "ODP",
    "application/vnd.oasis.opendocument.presentation-template": "OTP",
    "application/vnd.oasis.opendocument.spreadsheet": "ODS",
    "application/vnd.oasis.opendocument.spreadsheet-template": "OTS",
    "application/vnd.oasis.opendocument.chart": "ODC",
    "application/vnd.oasis.opendocument.formula": "ODF",
    "application/vnd.oasis.opendocument.database": "ODB",
    "application/vnd.oasis.opendocument.image": "ODI",
    "application/vnd.openofficeorg.extension": "OXT",

    // Document Extensions
    ".doc": "DOC",
    ".docx": "DOCX",
    ".dotx": "DOTX",
    ".docm": "DOCM",
    ".dotm": "DOTM",
    ".xls": "XLS",
    ".xlsx": "XLSX",
    ".xltx": "XLTX",
    ".xlsm": "XLSM",
    ".xltm": "XLTM",
    ".xlam": "XLAM",
    ".xlsb": "XLSB",
    ".ppt": "PPT",
    ".pptx": "PPTX",
    ".potx": "POTX",
    ".ppsx": "PPSX",
    ".ppam": "PPAM",
    ".pptm": "PPTM",
    ".potm": "potm",
    ".ppsm": "ppsm",
    ".mdb": "MDB",

    ".odt": "ODT",
    ".ott": "OTT",
    ".oth": "OTH",
    ".odm": "ODM",
    ".odg": "ODG",
    ".otg": "OTG",
    ".odp": "ODP",
    ".otp": "OTP",
    ".ods": "ODS",
    ".ots": "OTS",
    ".odc": "ODC",
    ".odf": "ODF",
    ".odb": "ODB",
    ".odi": "ODI",
    ".oxt": "OXT",

    // lower to upper
    doc: "DOC",
    docx: "DOCX",
    dotx: "DOTX",
    docm: "DOCM",
    dotm: "DOTM",
    xls: "XLS",
    xlsx: "XLSX",
    xltx: "XLTX",
    xlsm: "XLSM",
    xltm: "XLTM",
    xlam: "XLAM",
    xlsb: "XLSB",
    ppt: "PPT",
    pptx: "PPTX",
    potx: "POTX",
    ppsx: "PPSX",
    ppam: "PPAM",
    pptm: "PPTM",
    potm: "potm",
    ppsm: "ppsm",
    mdb: "MDB",

    odt: "ODT",
    ott: "OTT",
    oth: "OTH",
    odm: "ODM",
    odg: "ODG",
    otg: "OTG",
    odp: "ODP",
    otp: "OTP",
    ods: "ODS",
    ots: "OTS",
    odc: "ODC",
    odf: "ODF",
    odb: "ODB",
    odi: "ODI",
    oxt: "OXT"
  };

  var submitButton = $("button.govuk-button");

  function disableSubmitButton() {
    submitButton.attr("disabled", true);
  }

  function enableSubmitButton() {
    submitButton.removeAttr("disabled");
  }

  function mapUserFriendlyFileTypes(array) {
    return array.split(", ").map(function(a) {
      if (humanReadableMimeTypes.hasOwnProperty(a))
        return humanReadableMimeTypes[a];
      return a; // default to mime type if no friendly value is found
    });
  }

  function removeDuplicatesFromArray(array) {
    return array.filter(function(item, index) {
      return array.indexOf(item) >= index;
    });
  }

  function transformMimeTypes(mimetypes) {
    return removeDuplicatesFromArray(mapUserFriendlyFileTypes(mimetypes)).join(
      ", "
    );
  }

  function getFileExtension(fileName) {
    var fa = fileName.split(".");
    if (fa.length > 1) {
      return fa.pop();
    } else {
      return "unknown";
    }
  }

  function GformFileUpload() {
    var self = this;

    // Set up event handlers etc
    function init() {
      $(".govuk-file-upload").on("change", handleFileUpload);
      $(".uploaded-files").on("click", ".govuk-link", handleFileDelete);
    }

    // Error handling
    function handleError($input, msg) {
      enableSubmitButton();
      var errorEl =
        '<span class="govuk-error-message" role="alert">' + msg + "</span>";
      $(errorEl).insertBefore($input);
    }

    // Setup file upload
    function handleFileUpload(e) {
      var file = e.target.files[0];
      if (!file) {
        return false;
      }
      disableSubmitButton();
      $(".govuk-error-message").remove();
      var $input = $(e.currentTarget);
      var dataset = $input[0].dataset;
      var formTemplateId = dataset.formTemplateId;
      var accessCode = dataset.accessCode;
      var fileId = dataset.fileId;
      var sectionNumber = dataset.sectionNumber;
      var maxFileSize = parseInt(
        $input.data("maxFileSizeMB") || window.gform.formMaxAttachmentSizeMB,
        10
      );
      var fileExtension = getFileExtension(file.name);
      var fileExtensionCheck = file.type === "" || window.gform.restrictedFileExtensions.indexOf(fileExtension.toLowerCase()) >= 0;
      var fileTypeCheck = window.gform.contentTypes.indexOf(file.type) === -1;

      $input.attr("aria-busy", true);

      if ( fileExtensionCheck || fileTypeCheck ) {
        var fileTypeInError = (fileExtensionCheck) ? fileExtension : transformMimeTypes(file.type);
        return handleError(
          $input,
          interpolate(strings.fileTypeError[lang], [
            fileTypeInError,
            transformMimeTypes(window.gform.contentTypes)
          ])
        );
      }

      if (file.size == 0) {
        return handleError($input, strings.emptyFileSizeError[lang]);
      }

      if (file.size > maxFileSize * 1024 * 1024) {
        return handleError(
          $input,
          interpolate(strings.maxSizeError[lang], [maxFileSize])
        );
      }

      function onError(err) {
        $input.removeAttr("aria-busy");
        handleError($input, err.statusText);
      }

      var formComponentId = $input.attr("id");

      $("#" + formComponentId + "-files")
        .empty()
        .append(startProgressBar());

      if (e.target.hasAttribute("upscan")) {
        uploadFileUpscan()
          .then(function(response) {
            var key = document.getElementsByName("key")[0].value
            return checkConfirmation(formTemplateId, key);
          }, onError)
          .then(function(response) {
            console.log("response:",response)
            if(response == "") {
              fileUploadSuccess(
                  formComponentId,
                  file.name,
                  formTemplateId,
                  $input,
                  accessCode,
                  sectionNumber
              );
            } else {
              switch (response) {
                case "InvalidFileType":
                case "REJECTED":
                  onError({
                    statusText: interpolate(strings.upscanError[lang], [transformMimeTypes(window.gform.contentTypes)])
                  });
                  break;
                case "EntityTooSmall":
                  onError({
                    statusText: interpolate(strings.emptyFileSizeError[lang])
                  });
                  break;
                case "EntityTooLarge":
                  onError({
                    statusText: interpolate(strings.maxSizeError[lang], [maxFileSize])
                  });
                  break;
                default:
                  onError({
                    statusText: interpolate(strings.unexpectedError[lang])
                  });
              }
              hideFileStatus(formComponentId);
            }
          }, onError);
      } else {
        uploadFile(file, fileId)
          .then(function(response) {
            return updateMapping(
              formComponentId,
              fileId,
              formTemplateId,
              accessCode
            );
          }, onError)
          .then(function(response) {
            fileUploadSuccess(
              formComponentId,
              file.name,
              formTemplateId,
              $input,
              accessCode,
              sectionNumber
            );
          }, onError);
      }
    }

    function checkConfirmation(formTemplateId, key) {
      return $.ajax({
        url: "/submissions/upscan/check/" + formTemplateId +"/" + key,
        type: "GET"
      });
    }

    // Handle upscan file upload request
    function uploadFileUpscan() {
      var upscanForm = document.getElementById("gf-upscan")
      var formData = new FormData(upscanForm);

      return $.ajax({
        url: upscanForm.action,
        type: "POST",
        data: formData,
        processData: false,
        contentType: false,
        xhrFields: {
          withCredentials: true
        }
      })
    }

    // Handle file upload request
    function uploadFile(file, fileId) {
      var formData = new FormData();
      formData.append(
        fileId,
        file,
        fileId + "_" + file.name.replace(/\\/g, "/").replace(/.*\//, "")
      );

      return $.ajax({
        url:
          "/file-upload/upload/envelopes/" +
          window.gform.envelopeId +
          "/files/" +
          fileId,
        type: "POST",
        data: formData,
        processData: false,
        contentType: false
      });
    }

    // Associate formComponentId with fileId
    function updateMapping(
      formComponentId,
      fileId,
      formTemplateId,
      accessCode
    ) {
      return $.ajax({
        url:
          "/submissions/api/add-file/" +
          formTemplateId +
          "/" +
          accessCode +
          "/" +
          formComponentId +
          "/" +
          fileId,
        type: "GET"
      });
    }

    // Handle successful file upload
    function fileUploadSuccess(
      formComponentId,
      name,
      formTemplateId,
      input,
      accessCode,
      sectionNumber
    ) {
      enableSubmitButton();
      input.removeAttr("aria-busy");

      $("#" + formComponentId + "-files")
        .empty()
        .append(
          makeFileEntry(name, formComponentId, formTemplateId, accessCode, sectionNumber)
        )
        .trigger("focus");
    }

    // Display uploading file message
    function startProgressBar() {
      return progressBarWrapper("<span class='app-progress-spinner'></span><span id='fileupload' role='status' aria-live='polite'>" + strings.uploadingFile[lang] + "</span>", "");
    }

    // Display the uploaded file name and delete button
    function makeFileEntry(name, formComponentId, formTemplateId, accessCode, sectionNumber) {
      var deleteUrl = "/submissions/form/delete-file/" + formTemplateId + "/" + accessCode + "/" + sectionNumber + "/" + formComponentId
      var ariaLabel = name + " " + strings.deleteLabel[lang]

      var hasBeenUploadedMessage = interpolate(strings.hasBeenUploaded[lang], [
        " <strong>" + name + "</strong> "
      ])
      return progressBarWrapper(
        "<span id='fileupload' role='status' aria-live='polite'>" + hasBeenUploadedMessage +  "</span>",
        "<button type='submit' class='govuk-button govuk-button--secondary govuk-!-margin-bottom-0' data-module='govuk-button' id='fileDelete' aria-label='" + ariaLabel + "' formaction='" + deleteUrl + "'>" +
          strings.deleteLabel[lang] +
        "</button>"
      );
    }

    function progressBarWrapper(messageContent, buttonContent) {
      return $(
        "<dl class='govuk-summary-list app-file-upload__list'>" +
          "<div class='govuk-summary-list__row'>" +
            "<dd class='govuk-summary-list__value'>" +
              messageContent +
            "</dd>" +
            "<dd class='govuk-summary-list__actions app-file-upload__actions'>" +
              buttonContent +
            "</dd>" +
          "</div>" +
        "</dl>"
      );
    }

    // Handle file deletion
    function handleFileDelete(e) {
      var t = $(e.currentTarget);

      if (t.attr("aria-busy") == "true") {
        // to prevent duplicate requests via double click
        return false;
      }
      t.attr("aria-busy", "true");

      e.preventDefault();

      disableSubmitButton();

      var d = e.currentTarget.dataset;

      if (!d.formComponentId) {
        handleError($("#" + d.formId), strings.invalidFileToDelete[lang]);
      }

      var deleteUrl =
        "/submissions/api/forms/" +
        d.formId +
        "/" +
        d.accessCode +
        "/" +
        d.formComponentId +
        "";

      return fileDelete(deleteUrl).then(
        function(response) {
          hideFileStatus(d.formComponentId);
        },
        function(err) {
          t.removeAttr("aria-busy");
          handleError(
            $("#" + d.formComponentId),
            err.responseJSON && err.responseJSON.message
              ? err.responseJSON.message
              : strings.unexpectedError[lang]
          );
        }
      );
    }

    // Delete file
    function fileDelete(deleteUrl) {
      return $.ajax({
        url: deleteUrl,
        type: "GET"
      });
    }

    // File deletion succeeded
    function hideFileStatus(formComponentId) {
      enableSubmitButton();
      $("#" + formComponentId + "-files").empty();
      $("#" + formComponentId).val("");
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
