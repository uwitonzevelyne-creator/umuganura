/* Scoped loading: spinner on the viz/map/output that is busy — not a full-page veil. */
(function () {
  var SHOW_DELAY = 100;
  var HIDE_DELAY = 160;
  var timers = new WeakMap();

  function mark(el, on, title) {
    if (!el || !el.classList) return;
    if (on) {
      el.classList.add("um-loading-target");
      el.setAttribute("data-um-loading", title || "Loading");
      el.setAttribute("aria-busy", "true");
    } else {
      el.classList.remove("um-loading-target");
      el.removeAttribute("data-um-loading");
      el.setAttribute("aria-busy", "false");
    }
  }

  function schedule(el, on, title) {
    if (!el) return;
    var prev = timers.get(el);
    if (prev) clearTimeout(prev);
    var delay = on ? SHOW_DELAY : HIDE_DELAY;
    var t = setTimeout(function () {
      timers.delete(el);
      mark(el, on, title);
    }, delay);
    timers.set(el, t);
  }

  function targetsFromEvent(e) {
    var out = [];
    var binding = e && e.target;
    if (binding && binding.length !== undefined && !binding.nodeType) {
      // jQuery-ish
      for (var i = 0; i < binding.length; i++) out.push(binding[i]);
      return out;
    }
    if (binding && binding.nodeType === 1) {
      out.push(binding);
      return out;
    }
    // Fallback: any visible recalculating / busy output host
    document.querySelectorAll(
      ".shiny-plot-output.recalculating, .html-widget-output.recalculating, .shiny-html-output.recalculating, .leaflet.html-widget.recalculating, .js-plotly-plot.recalculating"
    ).forEach(function (el) { out.push(el); });
    return out;
  }

  function nearestHost(el) {
    if (!el || !el.closest) return el;
    return (
      el.closest(".map-card, .chart-card, .map-frame, .ap-map-frame, .ej-inspect-card, .shiny-plot-output, .html-widget-output, .shiny-html-output, .js-plotly-plot, .leaflet-container, .hv-photo, .bridge-frame") ||
      el
    );
  }

  document.addEventListener("shiny:recalculating", function (e) {
    targetsFromEvent(e).forEach(function (el) {
      schedule(nearestHost(el), true, "Loading");
    });
  });

  document.addEventListener("shiny:value", function (e) {
    targetsFromEvent(e).forEach(function (el) {
      schedule(nearestHost(el), false);
    });
  });

  document.addEventListener("shiny:error", function (e) {
    targetsFromEvent(e).forEach(function (el) {
      schedule(nearestHost(el), false);
    });
  });

  document.addEventListener("shiny:idle", function () {
    document.querySelectorAll(".um-loading-target").forEach(function (el) {
      schedule(el, false);
    });
  });

  // Observe class flips on outputs (Shiny adds .recalculating)
  if (window.MutationObserver) {
    var mo = new MutationObserver(function (mutations) {
      mutations.forEach(function (m) {
        var el = m.target;
        if (!el || !el.classList) return;
        if (!el.classList.contains("shiny-plot-output") &&
            !el.classList.contains("html-widget-output") &&
            !el.classList.contains("shiny-html-output") &&
            !el.classList.contains("js-plotly-plot") &&
            !(el.classList.contains("html-widget") && el.classList.contains("leaflet"))) {
          return;
        }
        var host = nearestHost(el);
        if (el.classList.contains("recalculating")) schedule(host, true, "Loading");
        else schedule(host, false);
      });
    });
    function watch() {
      mo.observe(document.body, { attributes: true, attributeFilter: ["class"], subtree: true });
    }
    if (document.body) watch();
    else document.addEventListener("DOMContentLoaded", watch);
  }

  // Tiny API for Action plan / manual scopes
  window.UmuganuraLoading = {
    show: function (title, sub, el) {
      var host = el ? nearestHost(el) : null;
      if (!host) {
        // Prefer the active panel's first chart card if caller gave no element
        var panel = document.querySelector(".panel.is-active");
        host = panel && panel.querySelector(".chart-card, .shiny-plot-output, .js-plotly-plot, .leaflet-container, .ap-map-frame");
      }
      if (host) schedule(host, true, title || "Loading");
    },
    hide: function (el) {
      if (el) schedule(nearestHost(el), false);
      else document.querySelectorAll(".um-loading-target").forEach(function (n) { schedule(n, false); });
    },
    forceHide: function () {
      document.querySelectorAll(".um-loading-target").forEach(function (n) { mark(n, false); });
    },
    setCopy: function () {}
  };
})();
