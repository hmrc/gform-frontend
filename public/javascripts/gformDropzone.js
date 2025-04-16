;(function (global) {
  'use strict'

  var $ = global.jQuery
  var GOVUK = global.GOVUK || {}

  function GformDropzone () {
    var self = this;

    function init () {

      const xlsxUrlTemplate = "/submissions/test-only/proxy-to-gform/gform/translation-excel/"

      const translationPageRegex = /^\/submissions\/test-only\/(translation|translation-quick)\/(.*)$/;

      const translationPage = translationPageRegex.exec(window.location.pathname)

      if (!translationPage) {
        return undefined;
      }

      const formTemplateId = translationPage[2];

      const errorDiv = "<div id='error' class='govuk-error-message'></div>"

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
              if (file.type === "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
                $("#dropzone-result").text("Processing " + file.name + ", please wait...")
                $("#dropzone-audit").remove();
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
                    if(response.ok) {
                      response.json().then(function(responseBody) {
                        if(responseBody.error) {
                          $("#dropzone-result").html("<strong>Something went wrong:</strong>" + errorDiv);
                          $("#error").text(responseBody.error);
                        } else {
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
                        }
                      });
                    } else {
                      if(response.status == 504) {
                        $("#dropzone-result").html("<strong>Connection Timeout</strong>" + errorDiv);
                        $("#error").text("It looks like your spreadsheet takes a lot of time to process. Please wait a couple of minutes and then refresh this page. Latest result will be displayed here.");
                      } else {
                        response.text().then(function(responseBody) {
                          $("#dropzone-result").html("<strong>Error</strong>" + errorDiv);
                          $("#error").text("Status: " + response.status + ", body: " + responseBody);
                        })
                      }
                    }
                  })
                })
              } else {
                $("#dropzone-result").html(errorDiv)
                $("#error").text("File " + file.name + " of type " + file.type + " is not .csv nor .xlsx.");
              }
            } else {
              $("#dropzone-result").html(errorDiv)
              $("#error").text("Not a file. Kind: " + item.kind + ". Type: " + item.type);
            }
          } else {
            // Use DataTransfer interface to access the file(s)
            $("#dropzone-result").html(errorDiv)
            $("#error").text("Gform - DataTransfer API not supported " + ev.dataTransfer.files);
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
