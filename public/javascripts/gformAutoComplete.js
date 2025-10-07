;(function (global) {
  'use strict';

  var $ = global.jQuery;
  var GOVUK = global.GOVUK || {};

  function GformAutoComplete () {
    // this url should come from appConfig, if it's missing the fallback
    // is used
    var baseLookupUrl = global.gform.baseLookupUrl || "/submissions/lookup/";

    function generateSourceFn (lookup, formTemplateId, id, maybeAccessCode) {
      var URL;
      
      if (lookup === "choice") {
        URL = baseLookupUrl.replace("/lookup/", "/lookup-choice/") + formTemplateId + "/" + id;
        // Initialize tracking for this field's last valid selection
        if (!window['lastValidSelection_' + id]) {
          window['lastValidSelection_' + id] = null;
        }
      } else {
        URL = baseLookupUrl + formTemplateId + "/" + id + "/" + lookup;
      }
      return function (query, populateResults) {
        var endpoint = URL + '/' + maybeAccessCode + "?query=" + encodeURIComponent(query)
        return $.get(endpoint)
          .then(function(results) {
            if (lookup === "choice") {
              populateResults(results);
            } else {
              populateResults(results);
            }
          })
      }
    }

    function setUpAutoComplete ($container) {
      var lookup = $container.attr('data-lookup');
      var formTemplateId = $container.attr('data-formTemplateId');
      var id = $container.attr('data-field-id');
      var formComponentId = $container.attr('data-component-id');
      var value = $container.attr('data-value');
      var showAll = $container.attr('data-show-all');
      var maybeAccessCode = $container.attr('data-accessCode');
      var displayWidth = $container.attr('data-displayWidth');
      var lang = $container.attr('data-language');

      var configurationOptions = {
              selectElement: $container[0],
              id: id,
              name: id,
              source: generateSourceFn(lookup, formTemplateId, formComponentId, maybeAccessCode),
              showNoOptionsFound: true,
              autoselect: false,
              defaultValue: value,
              showAllValues: showAll === "Enabled",
              dropdownArrow: function(config) {
                 return ''
               },
              onConfirm: function(selectedOption) {
                if (lookup === "choice" && selectedOption && selectedOption.value) {
                  // Store this as the last valid selection
                  window['lastValidSelection_' + id] = selectedOption;
                  
                  // Get the hidden input from global storage
                  var $hiddenInput = window['hiddenInput_' + id];
                  if ($hiddenInput && $hiddenInput.length) {
                    $hiddenInput.val(selectedOption.value);
                  } else {
                    // Fallback: try to find it by ID
                    var $fallbackHiddenInput = $('#' + id + '__value');
                    if ($fallbackHiddenInput.length) {
                      $fallbackHiddenInput.val(selectedOption.value);
                    }
                  }
                } else if (lookup === "choice" && (!selectedOption || !selectedOption.value)) {
                  // Don't clear the value if onConfirm is called with undefined/empty
                  // Instead, preserve the last valid selection if we have one
                  var lastValid = window['lastValidSelection_' + id];
                  if (lastValid && lastValid.value) {
                    var $hiddenInput = window['hiddenInput_' + id];
                    if ($hiddenInput && $hiddenInput.length && !$hiddenInput.val()) {
                      $hiddenInput.val(lastValid.value);
                    }
                  }
                }
              },
              templates: lookup === "choice" ? {
                inputValue: function(result) {
                  // Display the label in the input field (what user sees)
                  return result && result.label ? result.label : result;
                },
                suggestion: function(result) {
                  // Display the label in the dropdown
                  return result && result.label ? result.label : result;
                }
              } : undefined
      };

     if (lang === 'cy') {
       //copied from https://github.com/hmrc/hmrc-frontend/blob/main/src/components/accessible-autocomplete/accessible-autocomplete.js
       configurationOptions.tAssistiveHint = function(){ return'Pan fydd canlyniadau awtogwblhau ar gael, defnyddiwch y saethau i fyny ac i lawr i’w hadolygu a phwyswch y fysell ’enter’ i’w dewis.'
           + ' Gall defnyddwyr dyfeisiau cyffwrdd, archwilio drwy gyffwrdd â’r sgrin neu drwy sweipio.' };
       configurationOptions.tStatusQueryTooShort = function(minQueryLength){ return 'Ysgrifennwch '+ minQueryLength + ' neu fwy o gymeriadau am ganlyniadau' };
       configurationOptions.tNoResults = function(){ return 'Dim canlyniadau wedi’u darganfod' };
       configurationOptions.tStatusNoResults = function(){ return 'Dim canlyniadau chwilio' };
       configurationOptions.tStatusSelectedOption = function(selectedOption, length, index) { return 'Mae ' + selectedOption + (index + 1) + ' o '+ length +' wedi’i amlygu' };
       configurationOptions.tStatusResults = function(length, contentSelectedOption) {
         var resultOrResults = (length === 1) ? 'canlyniad' : 'o ganlyniadau';
         return length + ' ' + resultOrResults +' ar gael. '+ contentSelectedOption;
       };
     }
      // this is the method provided by accessible-autocomplete.min.js
     window.accessibleAutocomplete.enhanceSelectElement(configurationOptions);

      function getMaxWidth(displayWidth) {
        var maxWidth = null;
        switch(displayWidth) {
            case "XS": maxWidth = "9ex"; break;
            case "S": maxWidth = "10.8ex"; break;
            case "M": maxWidth = "23ex"; break;
            case "L": maxWidth = "41ex"; break;
            case "XL": maxWidth = "59ex"; break;
            case "XXL": break;
            case "DEFAULT": maxWidth = "41ex"; break;
        }
        if(maxWidth) {
           return {"max-width": maxWidth }
        } else {
           return {}
        }
      }

      $container.css(getMaxWidth(displayWidth))

      var checkInput = window.setInterval(function () {
        var $input = $('input#' + id);
        if ($input.length) {
          $input
            .attr('autocomplete', 'off');
          
          // For choice TypeAhead, set up the hidden input for form submission
          if (lookup === "choice") {
            // Find and remove/disable the original select element that accessible-autocomplete hides
            var $originalSelect = $('#' + id + '-select, select[name="' + id + '"]');
            if ($originalSelect.length) {
              $originalSelect.removeAttr('name'); // Remove name so it doesn't submit
            }
            
            // Also check for any other inputs with this name that might conflict
            $('input[name="' + id + '"], select[name="' + id + '"]').not($input).each(function() {
              $(this).removeAttr('name');
            });
            
            // Create a hidden input that will hold the actual value for form submission
            var hiddenInputId = id + '__value';
            var $hiddenInput = $('<input type="hidden" name="' + id + '" id="' + hiddenInputId + '">');
            
            // Set initial value if there's a prepopulated value
            if (value) {
              $hiddenInput.val(value);
              // Also store this as the last valid selection for consistency
              window['lastValidSelection_' + id] = { value: value, label: value };
            }
            
            // Insert into the form directly to ensure it stays in the DOM
            var $form = $input.closest('form');
            $form.append($hiddenInput);
            
            // Store the hidden input globally so onConfirm can find it
            window['hiddenInput_' + id] = $hiddenInput;
            
            // Remove the name attribute from the visible input so it doesn't conflict
            $input.removeAttr('name');
            
            // Add form submission safety check
            $input.closest('form').on('submit', function(e) {
              // Ensure our hidden input has the name attribute
              if (!$hiddenInput.attr('name')) {
                $hiddenInput.attr('name', id);
              }
              
              // Final safety check: if hidden input is empty but we have a last valid selection, restore it
              if (!$hiddenInput.val()) {
                var lastValid = window['lastValidSelection_' + id];
                if (lastValid && lastValid.value) {
                  $hiddenInput.val(lastValid.value);
                }
              }
            });
          }
          
          window.clearInterval(checkInput);
        }
      }, 200);
    }

    function init () {
      var $els = $('.lookup');
      if ($els.length && typeof window.accessibleAutocomplete === 'function') {
        $els.each(function (i, el) {
          setUpAutoComplete($(el))
        })
      }
    }

    self.GformAutoComplete = function () {
      init()
    }
  }

  GformAutoComplete.prototype.init = function () {
    self.GformAutoComplete()
  };

  GOVUK.GformAutoComplete = GformAutoComplete;
  global.GOVUK = GOVUK
})(window);
