(function () {
  function tellShiny(cause) {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue("ej_inspect_open", cause || "", { priority: "event" });
    }
  }

  function resizeMaps(root) {
    if (!root) return;
    window.dispatchEvent(new Event("resize"));
    if (window.UmuganuraMaps) UmuganuraMaps.invalidate(root);
  }

  function openInspect(cause) {
    document.querySelectorAll(".ej-inspect-panel.is-open").forEach(function (p) {
      p.classList.remove("is-open");
      p.setAttribute("aria-hidden", "true");
    });
    var panel = document.getElementById("ej-inspect-" + cause);
    if (!panel) return;
    // Keep the cause room visible — never bounce to Ejo land.
    if (typeof window.ejoShowStep === "function") {
      var step = panel.closest(".hv-step");
      var key = step && step.getAttribute("data-step");
      if (key) window.ejoShowStep(key);
    } else if (typeof window.ejoOpenRoom === "function") {
      window.ejoOpenRoom();
    }
    panel.classList.add("is-open");
    panel.setAttribute("aria-hidden", "false");
    document.body.classList.add("ej-inspect-open");
    tellShiny(cause);
    window.scrollTo(0, 0);
    setTimeout(function () {
      resizeMaps(panel);
      if (window.jQuery) window.jQuery(panel).trigger("shown");
      var back = panel.querySelector(".ej-inspect-close");
      if (back && back.focus) back.focus();
    }, 80);
    setTimeout(function () { resizeMaps(panel); }, 320);
  }

  function closeInspect(cause) {
    var panel = cause ? document.getElementById("ej-inspect-" + cause) : null;
    if (panel) {
      panel.classList.remove("is-open");
      panel.setAttribute("aria-hidden", "true");
    } else {
      document.querySelectorAll(".ej-inspect-panel.is-open").forEach(function (p) {
        p.classList.remove("is-open");
        p.setAttribute("aria-hidden", "true");
      });
    }
    if (!document.querySelector(".ej-inspect-panel.is-open")) {
      document.body.classList.remove("ej-inspect-open");
      tellShiny("");
    }
    // Stay on the same cause step inside the room — never openLand.
    window.scrollTo(0, 0);
  }

  document.addEventListener("click", function (e) {
    // Exact open control only — never body / ancestors with a data-* flag.
    var open = e.target.closest("button.ej-inspect-open[data-ej-inspect]");
    if (open) {
      e.preventDefault();
      e.stopPropagation();
      if (typeof e.stopImmediatePropagation === "function") e.stopImmediatePropagation();
      openInspect(open.getAttribute("data-ej-inspect"));
      return;
    }
    var close = e.target.closest("button.ej-inspect-close[data-ej-inspect-close]");
    if (close) {
      e.preventDefault();
      e.stopPropagation();
      if (typeof e.stopImmediatePropagation === "function") e.stopImmediatePropagation();
      closeInspect(close.getAttribute("data-ej-inspect-close"));
      return;
    }
    var printBtn = e.target.closest(".ej-inspect-print");
    if (printBtn) {
      e.preventDefault();
      var panel = printBtn.closest(".ej-inspect-panel");
      if (!panel) return;
      panel.classList.add("is-printing");
      window.print();
      setTimeout(function () { panel.classList.remove("is-printing"); }, 400);
    }
  }, true);

  // Bubble-phase only: let the select receive the click, then block nav land-reset.
  document.addEventListener("click", function (e) {
    if (!e.target || !e.target.closest) return;
    if (!e.target.closest(".ej-inspect-panel.is-open")) return;
    if (e.target.closest("a[data-panel], button[data-panel]")) return;
    e.stopPropagation();
  }, false);

  // Native selects inside Inspect must not arm the page scroll lock.
  document.addEventListener("mousedown", function (e) {
    if (e.target && e.target.closest && e.target.closest(".ej-inspect-panel")) {
      if (window.UmuganuraSelectLock && UmuganuraSelectLock.clear) UmuganuraSelectLock.clear();
    }
  }, true);

  function pushStep(key) {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue("ej_active_step", String(key || "0"), { priority: "event" });
    }
    closeInspect();
  }

  document.addEventListener("click", function (e) {
    var btn = e.target.closest("#ejo-room .hv-step-btn, #ejo-room [data-step-go]");
    if (!btn) return;
    if (document.body.classList.contains("ej-inspect-open")) {
      e.preventDefault();
      e.stopPropagation();
    }
    pushStep(btn.getAttribute("data-step") || btn.getAttribute("data-step-go") || "0");
  }, true);

  window.ejoInspectOpen = openInspect;
  window.ejoInspectClose = closeInspect;
})();
