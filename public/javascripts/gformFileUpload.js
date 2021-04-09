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
    return fileName.split('.').pop().toLowerCase();
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
      var maxFileSize = parseInt(
        $input.data("maxFileSizeMB") || window.gform.formMaxAttachmentSizeMB,
        10
      );
      var fileExtension = getFileExtension(file.name);

      $input.attr("aria-busy", true);

      if (
        file.type === "" ||
        window.gform.contentTypes.indexOf(file.type) === -1 ||
        window.gform.restrictedFileExtensions.includes(fileExtension)
      ) {
        return handleError(
          $input,
          interpolate(strings.fileTypeError[lang], [
            fileExtension.toUpperCase(),
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
      uploadFile(file, fileId)
        .then(function(response) {
          return updateMapping(
            $input.attr("id"),
            fileId,
            formTemplateId,
            accessCode
          );
        }, onError)
        .then(function(response) {
          fileUploadSuccess(
            $input.attr("id"),
            file.name,
            formTemplateId,
            $input,
            accessCode
          );
        }, onError);
    }

    function onError(err) {
      $input.removeAttr("aria-busy");
      handleError($input, err.statusText);
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
      accessCode
    ) {
      enableSubmitButton();
      input.removeAttr("aria-busy");

      $("#" + formComponentId + "-files")
        .empty()
        .append(
          makeFileEntry(name, formComponentId, formTemplateId, accessCode)
        )
        .attr("tabIndex", "-1")
        .trigger("focus");
    }

    // Display the uploaded file name and delete button
    function makeFileEntry(name, formComponentId, formTemplateId, accessCode) {
      var deleteUrl =
        "/submissions/api/forms/" +
        formTemplateId +
        "/" +
        accessCode +
        "/" +
        formComponentId;

      return $(
        "<span>" +
          name +
          '</span> <a href="' +
          deleteUrl +
          '" class="govuk-link" data-form-component-id="' +
          formComponentId +
          '" data-form-id="' +
          formTemplateId +
          '" data-access-code="' +
          accessCode +
          '"><span aria-hidden="true">' +
          strings.deleteLabel[lang] +
          '</span><span class="govuk-visually-hidden">' +
          strings.deleteLabel[lang] +
          " " +
          name +
          "</span></a>"
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
          fileDeleteSuccess(d.formComponentId);
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
    function fileDeleteSuccess(formComponentId) {
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
