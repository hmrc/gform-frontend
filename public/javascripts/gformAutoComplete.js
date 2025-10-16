;(function (global) {
  'use strict';

  var $ = global.jQuery;
  var GOVUK = global.GOVUK || {};
  var STATE = new WeakMap(); // selectEl -> { labelValueMap: Map, lastValid: {label,value} }

  function baseLookupUrl() {
    return (global.gform && global.gform.baseLookupUrl) || '/submissions/lookup/';
  }

  function extractMeta($el) {
    return {
      fieldId: $el.attr('data-field-id'),
      componentId: $el.attr('data-component-id'),
      formTemplateId: $el.attr('data-formTemplateId'),
      lookup: $el.attr('data-lookup'),
      initialValue: $el.attr('data-value') || '',
      showAll: $el.attr('data-show-all'),
      accessCode: $el.attr('data-accessCode') || '-',
      language: $el.attr('data-language') || 'en',
      displayWidth: $el.attr('data-displayWidth'),
      indexKey: $el.attr('data-index-key')
    };
  }

  function buildEndpoint(meta, query) {
    var base = baseLookupUrl();
    var path;
    if (meta.lookup === 'choice') {
      path = base.replace('/lookup/', '/lookup-choice/') + meta.formTemplateId;
    } else {
      path = base + meta.formTemplateId + '/' + meta.componentId + '/' + meta.lookup;
    }
    var url = path + '/' + meta.accessCode + '?query=' + encodeURIComponent(query || '');
    if (meta.indexKey && meta.lookup === 'choice') {
      url += '&indexKey=' + encodeURIComponent(meta.indexKey);
    }
    return url;
  }

  function fetchResults(meta, query) {
    return $.get(buildEndpoint(meta, query))
      .then(function (results) {
        if (!Array.isArray(results)) return [];
        return results.map(function (r) {
            if (r && typeof r === 'object' && 'label' in r && 'value' in r) {
              return { label: String(r.label), value: String(r.value) };
            }
            return { label: String(r), value: String(r) };
        });
      })
      .catch(function () { return []; });
  }

  function ensureSelectElement($origin, meta) {
    // If original element is already a select (e.g. choice path), reuse it.
    if ($origin.is('select')) {
      return $origin[0];
    }
    // Otherwise create a synthetic select for enhancement with the correct id/name.
    var $select = $('<select/>');
    $select.attr({ id: meta.fieldId, name: meta.fieldId, 'data-language': meta.language });
    // Insert after placeholder to preserve layout.
    $origin.after($select);
    return $select[0];
  }

  function setSelected(selectEl, label, value) {
    // Clear existing options – we only keep the selected one to avoid accumulation.
    while (selectEl.options.length) selectEl.remove(0);
    var opt = document.createElement('option');
    opt.value = value;
    opt.textContent = label;
    opt.selected = true;
    selectEl.appendChild(opt);
    var state = STATE.get(selectEl) || {};
    state.lastValid = { label: label, value: value };
    STATE.set(selectEl, state);
  }

  function buildSource(meta, selectEl) {
    return function (query, populate) {
      fetchResults(meta, query).then(function (items) {
        var map = new Map();
        items.forEach(function (it) { map.set(it.label, it.value); });
        var st = STATE.get(selectEl) || {};
        st.labelValueMap = map;
        STATE.set(selectEl, st);
        populate(items.map(function (it) { return it.label; }));
      });
    };
  }

  function applyWidth($el, displayWidth) {
    var widths = { XS: '9ex', S: '10.8ex', M: '23ex', L: '41ex', XL: '59ex', DEFAULT: '41ex' };
    if (displayWidth && displayWidth !== 'XXL') {
      var w = widths[displayWidth] || widths.DEFAULT;
      if (w) $el.css({ 'max-width': w });
    }
  }

  function buildConfig(selectEl, meta, initialLabel) {
    return {
      selectElement: selectEl,
      showNoOptionsFound: true,
      autoselect: false,
      // defaultValue must be the user-facing label, not the underlying value
      defaultValue: initialLabel || meta.initialValue,
      showAllValues: meta.showAll === 'Enabled',
      source: buildSource(meta, selectEl),
      dropdownArrow: function () { return ''; },
      onConfirm: function (chosenLabel) {
        var state = STATE.get(selectEl) || {};
        var label = typeof chosenLabel !== 'undefined' ? chosenLabel : (state.lastValid && state.lastValid.label) || '';
        if (!label) return;
        var value = (state.labelValueMap && state.labelValueMap.get(label)) || (state.lastValid && state.lastValid.value) || label;
        setSelected(selectEl, label, value);
      }
    };
  }

  function addWelshTranslations(config, meta) {
    if (meta.language !== 'cy') return config;
    config.tAssistiveHint = function () { return 'Pan fydd canlyniadau awtogwblhau ar gael, defnyddiwch y saethau i fyny ac i lawr i’w hadolygu a phwyswch y fysell ’enter’ i’w dewis. Gall defnyddwyr dyfeisiau cyffwrdd, archwilio drwy gyffwrdd â’r sgrin neu drwy sweipio.'; };
    config.tStatusQueryTooShort = function (n) { return 'Ysgrifennwch ' + n + ' neu fwy o gymeriadau am ganlyniadau'; };
    config.tNoResults = function () { return 'Dim canlyniadau wedi’u darganfod'; };
    config.tStatusNoResults = function () { return 'Dim canlyniadau chwilio'; };
    config.tStatusSelectedOption = function (selectedOption, length, index) { return 'Mae ' + selectedOption + ' ' + (index + 1) + ' o ' + length + ' wedi’i amlygu'; };
    config.tStatusResults = function (length, contentSelectedOption) {
      var r = (length === 1) ? 'canlyniad' : 'o ganlyniadau';
      return length + ' ' + r + ' ar gael. ' + contentSelectedOption;
    };
    return config;
  }

  function deriveInitialLabel(selectEl, meta) {
    // For a prepopulated choice select, the selected <option>'s text is the label
    if (selectEl && selectEl.options && selectEl.options.length) {
      var selectedOpt = Array.prototype.find.call(selectEl.options, function (o) { return o.selected; });
      if (!selectedOpt && meta.initialValue) {
        selectedOpt = Array.prototype.find.call(selectEl.options, function (o) { return o.value === meta.initialValue; });
      }
      if (selectedOpt) {
        return selectedOpt.textContent || selectedOpt.innerText || selectedOpt.value;
      }
    }
    return meta.initialValue; // fallback
  }

  function initStateFromDom(selectEl) {
    if (!selectEl || !selectEl.options) return;
    var map = new Map();
    var lastValid = null;
    Array.prototype.forEach.call(selectEl.options, function (o) {
      var label = o.textContent || o.innerText || o.value;
      map.set(label, o.value);
      if (o.selected) {
        lastValid = { label: label, value: o.value };
      }
    });
    var st = STATE.get(selectEl) || {};
    if (map.size) st.labelValueMap = map;
    if (lastValid) st.lastValid = lastValid;
    STATE.set(selectEl, st);
  }

  function initElement(el) {
  var $origin = $(el);
  var meta = extractMeta($origin);
  applyWidth($origin, meta.displayWidth);
  var selectEl = ensureSelectElement($origin, meta);
  initStateFromDom(selectEl);
  var initialLabel = deriveInitialLabel(selectEl, meta);
  if (!STATE.get(selectEl) || !STATE.get(selectEl).lastValid) {
    if (meta.initialValue) {
      setSelected(selectEl, initialLabel, meta.initialValue);
    }
  }

  if (!global.accessibleAutocomplete || typeof global.accessibleAutocomplete.enhanceSelectElement !== 'function') {
    return;
  }

  var config = addWelshTranslations(buildConfig(selectEl, meta, initialLabel), meta);
  global.accessibleAutocomplete.enhanceSelectElement(config);

  requestAnimationFrame(function () {
    var input = document.getElementById(meta.fieldId);
    if (input) {
      input.setAttribute('autocomplete', 'off');
      
      // Add blur handler for auto-selection
      $(input).on('blur', function() {
        var typed = input.value.trim();
        if (!typed) return;
        
        var state = STATE.get(selectEl);
        if (!state || !state.labelValueMap) return;
        
        // If user typed exactly what's in our label map, auto-select it
        if (state.labelValueMap.has(typed)) {
          var value = state.labelValueMap.get(typed);
          setSelected(selectEl, typed, value);
          console.log('Auto-selected exact match:', typed, '->', value);
        }
      });
    }
    
    var describedBy = selectEl.getAttribute('aria-describedby');
    if (describedBy && input && !input.getAttribute('aria-describedby')) {
      input.setAttribute('aria-describedby', describedBy);
      selectEl.setAttribute('aria-describedby', '');
    }
  });
}

  function initAll() {
    var $els = $('.lookup');
    if (!$els.length) return;
    $els.each(function (_, el) { initElement(el); });
  }

  function GformAutoComplete() {}
  GformAutoComplete.prototype.init = initAll;

  GOVUK.GformAutoComplete = GformAutoComplete;
  global.GOVUK = GOVUK;

})(window);
