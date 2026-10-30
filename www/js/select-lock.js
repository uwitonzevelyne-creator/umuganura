/* Keep window scroll put while any select / selectize is opened or updated. */
(function () {
  var pinY = null;
  var until = 0;
  var raf = 0;
  var armed = false;

  function yNow() {
    return window.scrollY || window.pageYOffset || document.documentElement.scrollTop || 0;
  }

  function hold(ms) {
    var y = yNow();
    if (pinY == null) pinY = y;
    else if (y > 40) pinY = y;
    until = Date.now() + (ms || 4000);
    armed = true;
    if (!raf) raf = requestAnimationFrame(tick);
  }

  function tick() {
    if (!armed || pinY == null || Date.now() > until) {
      armed = false;
      pinY = null;
      raf = 0;
      return;
    }
    if (Math.abs(yNow() - pinY) > 1) {
      rawScrollTo(pinY);
    }
    raf = requestAnimationFrame(tick);
  }

  function rawScrollTo(y) {
    if (_scrollTo) {
      _scrollTo.call(window, 0, y);
    } else {
      window.scrollTo(0, y);
    }
    document.documentElement.scrollTop = y;
    if (document.body) document.body.scrollTop = y;
  }

  var _scrollTo = window.scrollTo ? window.scrollTo.bind(window) : null;
  if (_scrollTo) {
    window.scrollTo = function (a, b) {
      if (armed && pinY != null && Date.now() <= until) {
        var top = typeof a === "number" ? b : (a && a.top);
        if (typeof top === "number" && Math.abs(top - pinY) > 1) {
          if (typeof a === "number") return _scrollTo(0, pinY);
          return _scrollTo({ left: (a && a.left) || 0, top: pinY, behavior: "auto" });
        }
      }
      return _scrollTo.apply(window, arguments);
    };
  }

  if (Element.prototype.scrollIntoView) {
    var _siv = Element.prototype.scrollIntoView;
    Element.prototype.scrollIntoView = function () {
      if (armed && pinY != null && Date.now() <= until) return;
      return _siv.apply(this, arguments);
    };
  }

  function isSelectTarget(el) {
    return !!(el && el.closest && el.closest(
      "select, .selectize-input, .selectize-dropdown, .selectize-control, .selectize-dropdown-content, .shiny-input-container, .form-group, .price-filters, .hv-filters, .ej-inst"
    ));
  }

  ["pointerdown", "mousedown", "touchstart", "focusin", "keydown"].forEach(function (ev) {
    document.addEventListener(ev, function (e) {
      if (isSelectTarget(e.target)) hold(2200);
    }, true);
  });

  document.addEventListener("change", function (e) {
    if (isSelectTarget(e.target)) hold(1800);
  }, true);

  document.addEventListener("shiny:idle", function () {
    if (armed) until = Date.now() + 200;
  }, true);

  // A wheel or a drag of the page ends the pin. Do not listen to scroll:
  // correcting scroll from inside a scroll event loops and freezes the tab.
  window.addEventListener("wheel", function (e) {
    if (!armed) return;
    if (e.target && e.target.closest && e.target.closest(".selectize-dropdown, .selectize-input, select")) return;
    armed = false;
    pinY = null;
    until = 0;
  }, { passive: true, capture: true });

  function wireSelectize($) {
    $(document).on("mousedown click focus", ".selectize-input, .selectize-dropdown, .selectize-dropdown-content", function () {
      hold(4500);
    });
    $(document).on("dropdown_open", function () { hold(4500); });
    $(document).on("dropdown_close item_add item_remove", function () {
      if (armed) hold(2500);
    });
    // After Shiny builds a selectize, re-pin if user was interacting.
    $(document).on("shiny:inputchanged", function (e) {
      if (!armed || !e || !e.name) return;
      if (/^ej_|^iso_/.test(e.name) && document.querySelector(".selectize-control.dropdown-active")) hold(800);
    });
  }

  if (window.jQuery) wireSelectize(window.jQuery);
  else document.addEventListener("DOMContentLoaded", function () {
    if (window.jQuery) wireSelectize(window.jQuery);
  });

  // Watch selectize opening (class dropdown-active) anywhere in the app.
  if (window.MutationObserver) {
    var mo = new MutationObserver(function (mutations) {
      for (var i = 0; i < mutations.length; i++) {
        var t = mutations[i].target;
        if (!t || !t.classList) continue;
        if (t.classList.contains("dropdown-active")) {
          hold(2200);
          break;
        }
      }
    });
    function observe() {
      mo.observe(document.body, { attributes: true, attributeFilter: ["class"], subtree: true, childList: true });
    }
    if (document.body) observe();
    else document.addEventListener("DOMContentLoaded", observe);
  }

  function clear() {
    armed = false;
    pinY = null;
    until = 0;
  }

  window.UmuganuraSelectLock = { hold: hold, clear: clear };
})();
