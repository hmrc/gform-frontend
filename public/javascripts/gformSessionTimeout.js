;(function (global) {
  'use strict';
  var $ = global.jQuery;
  var GOVUK = global.GOVUK || {};

  GOVUK.gformSessionTimeout = function(options) {

    var settings = {
      timeout: 30,
      countdown: 15,
      time: 'minutes',
      title: 'You’re about to be signed out',
      message: 'For security reasons, you will be signed out of this service in',
      keep_alive_url: '/submissions/keep-alive',
      logout_url: '/loggedout',
      restart_on_yes: true,
      dialog_width: 340,
      close_on_escape: true,
      background_no_scroll: true,
      keep_alive_button_text: 'Stay signed in',
      sign_out_button_text: 'Sign out'
    };

    $.extend(settings, options);

    var timeoutInterval, startTime, currentMin;
    var dialogOpen = false;

    function getDateNow() {
        return Date.now() || +new Date()
    }

    function secondsToTime (secs) {
      var hours = Math.floor(secs / (60 * 60));
      var divisorForMinutes = secs % (60 * 60);
      var minutes = Math.floor(divisorForMinutes / 60);
      var divisorForSeconds = divisorForMinutes % 60;
      var seconds = Math.ceil(divisorForSeconds);
      return {
        'h': hours,
        'm': minutes,
        's': seconds
      };
    }

    function expired() {
      return getDateNow() >= settings.signout_time
    }

    function withinCountdown() {
      return !expired() && getDateNow() >=  settings.signout_time - (settings.countdown * 1000)
    }

    function init() {
      setupDialogTimer()
    }

    function setupDialogTimer() {
      dialogOpen = false
      settings.signout_time = getDateNow() + settings.timeout * 1000;
      timeoutInterval = global.setInterval(checkTimeElapsed, 1000)
    }

    function checkTimeElapsed() {
      if (expired()) {
        signOut()
      } else if (withinCountdown()) {
        global.clearInterval(timeoutInterval);
        setupDialog()
      }
    }

    function setupDialog() {
      dialogOpen = true;
      startTime = Math.round(getDateNow() / 1000);
      currentMin = Math.ceil(settings.timeout / 60);
      destroyDialog();
      if (settings.background_no_scroll) {
        $('html').addClass('noScroll')
      }
      var time = secondsToTime(settings.countdown);
      dialogOpen = true;
      if (time.m === 1) {
        settings.time = ' minute'
      }
      $('<div id="timeout-dialog" class="timeout-dialog" role="dialog" aria-labelledby="timeout-message" tabindex=-1 aria-live="polite">' +
        '<h1 class="heading-medium push--top">' + settings.title + '</h1>' +
        '<p id="timeout-message" role="text">' + settings.message + ' <span id="timeout-countdown" class="countdown">' + time.m + ' ' + settings.time + '</span>' + '.</p>' +
        '<button id="timeout-keep-signin-btn" class="button">' + settings.keep_alive_button_text + '</button>' +
        '<button id="timeout-sign-out-btn" class="button button--link">' + settings.sign_out_button_text + '</button>' +
        '</div>' +
        '<div id="timeout-overlay" class="timeout-overlay"></div>')
        .appendTo('body');

      // AL: disable the non-dialog page to prevent confusion for VoiceOver users
      $('#skiplink-container, body>header, #global-cookie-message, body>main, body>footer').attr('aria-hidden', 'true');

      var activeElement = document.activeElement;
      var modalFocus = document.getElementById('timeout-dialog');
      modalFocus.focus();
      addEvents();
      startCountdown(settings.countdown);
      var escPress = function (event) {
        if (event.keyCode === 27) {
          keepAlive();
          activeElement.focus()
        }
      };

      var closeDialog = function () {
        if (dialogOpen) {
          keepAlive();
          activeElement.focus()
        }
      };

      var handleTouch = function (e) {
        var touches = e.originalEvent.touches || e.originalEvent.changedTouches;
        if ($('#timeout-dialog').length) {
          if (touches.length === 1) {
            e.preventDefault()
          }
        }
      };

      $(document)
        .on('touchmove', handleTouch)
        .on('keydown', escPress);
      $('#timeout-keep-signin-btn').on('click', closeDialog);
      $('#timeout-sign-out-btn').on('click', signOut)
    }

    function destroyDialog() {
      if ($('#timeout-dialog').length) {
        dialogOpen = false;
        $('.timeout-overlay').remove();
        $('#timeout-dialog').remove();
        if (settings.background_no_scroll) {
          $('html').removeClass('noScroll')
        }
      }
      $('#skiplink-container, body>header, #global-cookie-message, body>main, body>footer').removeAttr('aria-hidden')
    }

    function updateUI(counter) {
      if (counter < 60) {
        $('.timeout-dialog').removeAttr('aria-live');
        $('#timeout-countdown').html(counter + ' seconds')
      } else {
        var newCounter = Math.ceil(counter / 60);
        var minutesMessage = ' minutes';
        if (newCounter === 1) {
          minutesMessage = ' minute'
        }
        if (newCounter < currentMin) {
          currentMin = newCounter;
          $('#timeout-countdown').html(newCounter + minutesMessage)
        }
      }
    }

    function addEvents() {
      $('a, input, textarea, button, [tabindex]').not('[tabindex="-1"]').on('focus', function (event) {
        var modalFocus = document.getElementById('timeout-dialog');
        if (modalFocus && dialogOpen) {
          if (!modalFocus.contains(event.target)) {
            event.stopPropagation();
            modalFocus.focus()
          }
        }
      });

      function handleFocus () {
        if (dialogOpen) {
          global.clearInterval(countdown);
          var expiredSeconds = (Math.round(Date.now() / 1000)) - startTime;
          var currentCounter = settings.countdown - expiredSeconds;
          updateUI(currentCounter);
          startCountdown(currentCounter)
        }
      }

      if (navigator.userAgent.match(/MSIE 8/) == null) {
        $(global)
          .off('focus', handleFocus)
          .on('focus', handleFocus)
      }
    }

    function startCountdown(counter) {
      global.countdown = global.setInterval(function () {
        counter -= 1;
        updateUI(counter);
        if (counter <= 0) {
          signOut()
        }
      }, 1000)
    }

    function keepAlive() {
      destroyDialog();
      global.clearInterval(global.countdown);
      $.get(settings.keep_alive_url, function () {
        if (settings.restart_on_yes) {
          setupDialogTimer()
        } else {
          signOut()
        }
      })
    }

    function signOut() {
      global.location = settings.logout_url
    }

    init()
  };
})(window);
