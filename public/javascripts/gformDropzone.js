;(function (global) {
  'use strict'

  var $ = global.jQuery
  var GOVUK = global.GOVUK || {}

  function GformDropzone () {
    var self = this;

    function init () {

      const csvUrlTemplate = "/submissions/test-only/proxy-to-gform/gform/translation/"
      const xlsxUrlTemplate = "/submissions/test-only/proxy-to-gform/gform/translation-excel/"

      const translationPageRegex = /^\/submissions\/test-only\/translation\/(.*)$/;

      const formTemplateId = translationPageRegex.exec(window.location.pathname)[1];

      if (formTemplateId) {
        const dropzone = $("#dropzone")
        dropzone
          .on("dragleave", function(e) {
            e.preventDefault();

            dropzone.removeClass("dropzone-active");
            dropzone.addClass("dropzone-inactive");
          })
          .on("dragover", function(e) {
            e.preventDefault();

            dropzone.removeClass("dropzone-inactive");
            dropzone.addClass("dropzone-active");

          })
        .on("drop", function(e) {
          e.preventDefault();

          const ev = e.originalEvent;

          dropzone.removeClass("dropzone-active");
          dropzone.addClass("dropzone-inactive");

          if (ev.dataTransfer.items) {
            // Use DataTransferItemList interface to access the file(s)
            const item = ev.dataTransfer.items[0];


            if (item.kind === "file") {
              const file = item.getAsFile();
              if(file.type === "text/csv") {
                file.text().then(function(body) {
                const url = window.location.origin + csvUrlTemplate + formTemplateId;
                fetch(url, {
                  headers: {
                    "Content-Type": "text/plain;charset=utf-8",
                    "Origin": "null"
                  },
                  method: "POST",
                  body: body
                }).then(function(response) {
                  response.json().then(function(responseBody) {
                    $("#dropzone-result").text(responseBody.error)
                  });
                })
              }) ;
              } else if (file.type === "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
                $("#dropzone-result").text("Processing " + file.name + ", please wait...")
                file.arrayBuffer().then(function(body) {
                  const url = window.location.origin + xlsxUrlTemplate + formTemplateId;
                  fetch(url, {
                    headers: {
                      "Content-Type": file.type,
                      "Origin": "null"
                    },
                    method: "POST",
                    body: body
                  }).then(function(response) {
                    response.json().then(function(responseBody) {
                      var list = "";
                      for (var i = 0; i < responseBody.untranslatedRows.length; i++) {
                        var row = responseBody.untranslatedRows[i];
                        list += "<li><b>" + row.path + "</b>: <span id='item-" + i + "'></span></li>";
                      }

                      $("#dropzone-result").html(
                        "<p>Translated: " + responseBody.translatedCount  + "</p>" +
                          "<p>Untranslated: " + responseBody.untranslatedCount  + "</p>" +
                          "<ol>" + list + "<ol>"
                      )

                      for (var i = 0; i < responseBody.untranslatedRows.length; i++) {
                        var row = responseBody.untranslatedRows[i];
                        $("#item-" + i).text(row.en)
                      }
                    });
                  })
                })
              } else {
                $("#dropzone-result").text("File " + file.name + " of type " + file.type + " is not .csv nor .xlsx.")
              }
            } else {
              $("#dropzone-result").text("Not a file");
            }
          } else {
            // Use DataTransfer interface to access the file(s)
            console.error("Gform - DataTransfer API not supported", ev.dataTransfer.files);
          }
        });
      } else {
        console.error("No templateId found in url.")
      }
    }

    self.GformDropzone = function () {
      init()
    }
  }

  GformDropzone.prototype.init = function () {
    this.GformDropzone()
  }

  GOVUK.GformDropzone = GformDropzone
  global.GOVUK = GOVUK
})(window);
