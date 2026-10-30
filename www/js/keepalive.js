/* Keep Shiny session alive for long idle / scrolling sessions (30 min). */
(function () {
  var IDLE_MS = 30 * 60 * 1000;
  var PING_MS = 90 * 1000;
  var lastActive = Date.now();
  var lastPing = 0;

  function touch() { lastActive = Date.now(); }

  ["pointerdown", "keydown", "touchstart", "scroll", "mousemove", "wheel"].forEach(function (ev) {
    window.addEventListener(ev, touch, { passive: true, capture: true });
  });
  document.addEventListener("visibilitychange", function () {
    if (!document.hidden) touch();
  });

  function ping() {
    if (!window.Shiny || !Shiny.setInputValue) return;
    if (Date.now() - lastActive > IDLE_MS) return;
    lastPing = Date.now();
    try {
      Shiny.setInputValue("umuganura_keepalive", lastPing, { priority: "event" });
    } catch (e) {}
  }

  setInterval(ping, PING_MS);
  document.addEventListener("shiny:connected", function () {
    touch();
    ping();
  });
  document.addEventListener("shiny:disconnected", function () {
    // Soft reconnect without full page reload when possible.
    setTimeout(function () {
      if (window.Shiny && Shiny.shinyapp && typeof Shiny.shinyapp.reconnect === "function") {
        try { Shiny.shinyapp.reconnect(); } catch (e) {}
      }
    }, 1500);
  });

  // Clear select-lock on deliberate page wheel so scroll never fights the user.
  window.addEventListener("wheel", function (e) {
    if (!window.UmuganuraSelectLock || !UmuganuraSelectLock.clear) return;
    var t = e.target;
    if (t && t.closest && t.closest(".selectize-dropdown, .selectize-input, select")) return;
    UmuganuraSelectLock.clear();
  }, { passive: true, capture: true });
})();
