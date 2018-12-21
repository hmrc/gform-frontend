;(function (global) {
  'use strict'
  var $ = global.jQuery
  var GOVUK = global.GOVUK || {}

  GOVUK.gformSessionTimeout = function(options) {

    var settings = {
      timeout: 900,
      countdown: 120,
      time: 'minutes',
      title: 'You’re about to be signed out',
      message: 'For security reasons, you will be signed out of this service in',
      keep_alive_url: document.location.href,
      logout_url: document.location.href,
      restart_on_yes: true,
      dialog_width: 340,
      close_on_escape: true,
      background_no_scroll: true,
      keep_alive_button_text: 'Stay signed in',
      sign_out_button_text: 'Sign out'
    };

    $.extend(settings, options);

    var timeoutInterval;

    function getDateNow() {
        return Date.now() || +new Date()
    }

    function secondsToTime (secs) {
      var hours = Math.floor(secs / (60 * 60));

      var divisorForMinutes = secs % (60 * 60);
      var minutes = Math.floor(divisorForMinutes / 60);

      var divisorForSeconds = divisorForMinutes % 60;
      var seconds = Math.ceil(divisorForSeconds);

      var obj = {
        'h': hours,
        'm': minutes,
        's': seconds
      };
      return obj
    }

    function expired() {
      return getDateNow() >= settings.signout_time
    }

    function withinCountdown() {
      return !expired() && getDateNow() >=  settings.signout_time - (settings.countdown * 1000)
    }

    var TimeoutDialog = {
      init: function () {
        this.setupDialogTimer()
      },

      setupDialogTimer: function () {
        TimeoutDialog.dialogOpen = false
        settings.signout_time = getDateNow() + settings.timeout * 1000;
        timeoutInterval = window.setInterval(TimeoutDialog.checkTimeElapsed, 1000)
      },

      checkTimeElapsed: function() {
        if (expired()) {
          TimeoutDialog.signOut()
        } else if (withinCountdown()) {
          window.clearInterval(timeoutInterval);
          TimeoutDialog.setupDialog()
        }
      },

      setupDialog: function () {
        var self = this;
        window.dialogOpen = true;
        TimeoutDialog.startTime = Math.round(getDateNow() / 1000, 0);
        TimeoutDialog.currentMin = Math.ceil(settings.timeout / 60);
        self.destroyDialog();
        if (settings.background_no_scroll) {
          $('html').addClass('noScroll')
        }
        var time = secondsToTime(settings.countdown);
        TimeoutDialog.dialogOpen = true;
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
        TimeoutDialog.addEvents();
        TimeoutDialog.startCountdown(settings.countdown);
        TimeoutDialog.escPress = function (event) {
          if (event.keyCode === 27) {
            TimeoutDialog.keepAlive();
            activeElement.focus()
          }
        };

        TimeoutDialog.closeDialog = function () {
          if (window.dialogOpen) {
            TimeoutDialog.keepAlive();
            activeElement.focus()
          }
        };

        TimeoutDialog.handleTouch = function (e) {
          var touches = e.originalEvent.touches || e.originalEvent.changedTouches
          if ($('#timeout-dialog').length) {
            if (touches.length === 1) {
              e.preventDefault()
            }
          }
        };

        $(document)
          .on('touchmove', self.handleTouch)
          .on('keydown', self.escPress);
        $('#timeout-keep-signin-btn').on('click', self.closeDialog);
        $('#timeout-sign-out-btn').on('click', self.signOut)
      },

      destroyDialog: function () {
        if ($('#timeout-dialog').length) {
          window.dialogOpen = false;
          $('.timeout-overlay').remove();
          $('#timeout-dialog').remove();
          if (settings.background_no_scroll) {
            $('html').removeClass('noScroll')
          }
        }
        $('#skiplink-container, body>header, #global-cookie-message, body>main, body>footer').removeAttr('aria-hidden')
      },

      updateUI: function (counter) {
        if (counter < 60) {
          $('.timeout-dialog').removeAttr('aria-live');
          $('#timeout-countdown').html(counter + ' seconds')
        } else {
          var newCounter = Math.ceil(counter / 60);
          var minutesMessage = ' minutes';
          if (newCounter === 1) {
            minutesMessage = ' minute'
          }
          if (newCounter < TimeoutDialog.currentMin) {
            TimeoutDialog.currentMin = newCounter;
            $('#timeout-countdown').html(newCounter + minutesMessage)
          }
        }
      },

      addEvents: function () {
        $('a, input, textarea, button, [tabindex]').not('[tabindex="-1"]').on('focus', function (event) {
          var modalFocus = document.getElementById('timeout-dialog');
          if (modalFocus && TimeoutDialog.dialogOpen) {
            if (!modalFocus.contains(event.target)) {
              event.stopPropagation();
              modalFocus.focus()
            }
          }
        });

        function handleFocus () {
          if (TimeoutDialog.dialogOpen) {
            window.clearInterval(TimeoutDialog.countdown);
            var expiredSeconds = (Math.round(Date.now() / 1000, 0)) - TimeoutDialog.startTime;
            var currentCounter = settings.countdown - expiredSeconds;
            TimeoutDialog.updateUI(currentCounter);
            TimeoutDialog.startCountdown(currentCounter)
          }
        }

        if (navigator.userAgent.match(/MSIE 8/) == null) {
          $(window).off('focus', handleFocus);
          $(window).on('focus', handleFocus)
        }
      },

      startCountdown: function (counter) {
        TimeoutDialog.countdown = window.setInterval(function () {
          counter -= 1;
          TimeoutDialog.updateUI(counter);
          if (counter <= 0) {
            TimeoutDialog.signOut()
          }
        }, 1000)
      },

      keepAlive: function () {
        this.destroyDialog();
        window.clearInterval(this.countdown);
        $.get(settings.keep_alive_url, function () {
          if (settings.restart_on_yes) {
            TimeoutDialog.setupDialogTimer()
          } else {
            TimeoutDialog.signOut()
          }
        })
      },

      signOut: function () {
        window.location = settings.logout_url
      }
    };

    TimeoutDialog.init()
  };
})(window);
