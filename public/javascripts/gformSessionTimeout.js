(function(global) {
  "use strict";

  function GformSessionTimeout() {
    var self = this;

    var enableBroadcastTimestamp = false;
    var refreshSessionUrl = "";
    var signOutUrl = "";
    function init() {
      $("input").keypress(broadcastTimestamp);
      $("textarea").keypress(broadcastTimestamp);
      refreshSessionUrl = $('meta[name="gform-refresh-session"]').attr('data-refresh-session-url');
      signOutUrl = $('meta[name="hmrc-timeout-dialog"]').attr('data-sign-out-url');
      $.ajax({
        url: refreshSessionUrl,
        type: "GET",
        success: function(data, textStatus, xhr) {
            enableBroadcastTimestamp = $('meta[name="gform-refresh-session"]').length !== 0
        }
      });
    }

    function broadcastTimestamp() {
      if (!enableBroadcastTimestamp) return;
      enableBroadcastTimestamp = false;
      setTimeout(function () { enableBroadcastTimestamp = true; }, 60000)
      if (window.BroadcastChannel) {
        const channel = new window.BroadcastChannel("session-activity");
        channel.postMessage({
          timestamp: Date.now()
        });
        $.ajax({
            url: refreshSessionUrl,
            type: "GET",
            error: function(xhr, textStatus, errorThrown) {
              if (xhr.status == 403) {
                  window.location.href = signOutUrl
              }
            }
        });
      }
      return true;
    }

    self.GformSessionTimeout = function() {
      init()
    }
  }

  GformSessionTimeout.prototype.init = function() {
    this.GformSessionTimeout();
  };

  GOVUK.GformSessionTimeout = GformSessionTimeout;
  global.GOVUK = GOVUK;
})(window);
