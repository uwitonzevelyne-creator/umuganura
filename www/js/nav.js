(function () {
  // Scroll pin: keep page put during select/Shiny redraws WITHOUT listening to
  // "scroll" (correcting inside a scroll event loops and freezes the tab).
  var pinY = null;
  var pinUntil = 0;
  var pinRaf = 0;
  var pinArmed = false;
  var _scrollTo = window.scrollTo ? window.scrollTo.bind(window) : null;

  function yNow() { return window.scrollY || window.pageYOffset || document.documentElement.scrollTop || 0; }
  function clearPin() {
    pinArmed = false;
    pinY = null;
    pinUntil = 0;
    if (pinRaf) { cancelAnimationFrame(pinRaf); pinRaf = 0; }
  }
  function rawScrollTo(y) {
    if (_scrollTo) _scrollTo(0, y);
    else window.scrollTo(0, y);
    document.documentElement.scrollTop = y;
    if (document.body) document.body.scrollTop = y;
  }
  function enforcePin() {
    if (!pinArmed || pinY == null || Date.now() > pinUntil) { clearPin(); return; }
    if (Math.abs(yNow() - pinY) > 1) rawScrollTo(pinY);
    pinRaf = requestAnimationFrame(enforcePin);
  }
  function pinScroll(ms) {
    var y = yNow();
    // Never overwrite a good pin with a collapsed (near-top) position mid-update.
    if (pinY == null) pinY = y;
    else if (y > 40) pinY = y;
    pinUntil = Date.now() + (ms || 3200);
    pinArmed = true;
    if (!pinRaf) pinRaf = requestAnimationFrame(enforcePin);
  }
  function extendPin(ms) {
    if (!pinArmed || pinY == null) return;
    pinUntil = Date.now() + (ms || 2200);
    if (!pinRaf) pinRaf = requestAnimationFrame(enforcePin);
  }
  // Redirect programmatic jumps to top while pinned; allow intentional pin restores.
  if (_scrollTo) {
    window.scrollTo = function (a, b) {
      if (pinArmed && pinY != null && Date.now() <= pinUntil) {
        var top = typeof a === "number" ? b : (a && a.top);
        if (typeof top === "number" && Math.abs(top - pinY) > 1) {
          if (typeof a === "number") return _scrollTo(0, pinY);
          return _scrollTo({ left: (a && a.left) || 0, top: pinY, behavior: "auto" });
        }
      }
      return _scrollTo.apply(window, arguments);
    };
  }
  // Stop focus/selectize from yanking the page while a pin is active.
  if (typeof Element !== "undefined" && Element.prototype.scrollIntoView) {
    var _scrollIntoView = Element.prototype.scrollIntoView;
    Element.prototype.scrollIntoView = function () {
      if (pinArmed && pinY != null && Date.now() <= pinUntil) return;
      return _scrollIntoView.apply(this, arguments);
    };
  }
  if (typeof Element !== "undefined" && Element.prototype.scrollIntoViewIfNeeded) {
    var _scrollIntoViewIfNeeded = Element.prototype.scrollIntoViewIfNeeded;
    Element.prototype.scrollIntoViewIfNeeded = function () {
      if (pinArmed && pinY != null && Date.now() <= pinUntil) return;
      return _scrollIntoViewIfNeeded.apply(this, arguments);
    };
  }
  window.UmuganuraSelectLock = { hold: pinScroll, clear: clearPin };

  function showPanel(id) {
    clearPin();
    var panelEl = document.getElementById(id);
    if (window.UmuganuraLoading && panelEl) {
      UmuganuraLoading.show("Loading", "Opening…", panelEl.querySelector(".chart-card, .shiny-plot-output, .js-plotly-plot, .leaflet-container") || panelEl);
    }
    // Ejo Hazaza: open land overview when entering the tab, but never yank the user
    // back to land if they are already inside the cause / action-plan room.
    if (id === "ejo-hazaza" && typeof window.ejoOpenLand === "function") {
      var stayInRoom = typeof window.ejoRoomOpen === "function" && window.ejoRoomOpen();
      if (stayInRoom) {
        document.querySelectorAll(".panel").forEach(function (p) {
          p.classList.toggle("is-active", p.id === "ejo-hazaza");
        });
        document.querySelectorAll("[data-panel]").forEach(function (a) {
          a.classList.toggle("is-active", a.getAttribute("data-panel") === "ejo-hazaza");
        });
        document.body.setAttribute("data-panel", "ejo-hazaza");
      } else {
        window.ejoOpenLand();
      }
      announceShown(document.getElementById("ejo-hazaza"));
      setTimeout(function () {
        window.dispatchEvent(new Event("resize"));
        if (window.UmuganuraMaps) UmuganuraMaps.invalidate(document.getElementById("ejo-hazaza"));
        if (window.UmuganuraLoading) UmuganuraLoading.hide();
      }, 120);
      return;
    }
    document.querySelectorAll(".panel").forEach(function (p) {
      p.classList.toggle("is-active", p.id === id);
    });
    document.querySelectorAll("[data-panel]").forEach(function (a) {
      a.classList.toggle("is-active", a.getAttribute("data-panel") === id);
    });
    // Leave Ejo idle so overview charts do not keep recomputing off-panel.
    if (id !== "ejo-hazaza" && window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue("ej_active_step", "", { priority: "event" });
    }
    var panel = document.getElementById(id);
    if (panel) panel.scrollTop = 0;
    window.scrollTo(0, 0);
    announceShown(panel);
    setTimeout(function () {
      window.dispatchEvent(new Event("resize"));
      if (window.Plotly) {
        document.querySelectorAll(".js-plotly-plot").forEach(function (el) {
          window.Plotly.Plots.resize(el);
        });
      }
      if (window.UmuganuraMaps && panel) UmuganuraMaps.invalidate(panel);
      if (window.UmuganuraLoading) UmuganuraLoading.hide();
    }, 120);
  }

  function isReport(item) {
    return !!{ "inflation-overview": 1, "interest-overview": 1, "gdp-overview": 1, "fx-overview": 1, "jobs-overview": 1 }[item];
  }

  function showReport(item) {
    document.querySelectorAll(".iso-rep-pane").forEach(function (el) {
      el.classList.toggle("is-on", el.getAttribute("data-report") === item);
    });
  }

  function setRoomMode(item) {
    var room = document.getElementById("factor-room");
    if (!room) return;
    room.classList.remove("mode-classic", "mode-price", "mode-overview", "mode-input", "mode-food", "mode-live", "mode-force", "mode-report");
    room.setAttribute("data-item", item || "");
    var reportItem = isReport(item);
    if (reportItem) {
      room.classList.add("mode-report");
      showReport(item);
    } else if (item === "inflation-input") room.classList.add("mode-input");
    else if (item === "inflation-food") room.classList.add("mode-food");
    else if (item === "inflation-livestock") room.classList.add("mode-live");
    else if (/^(interest|jobs|gdp|fx)-/.test(item || "")) room.classList.add("mode-force");
    else room.classList.add("mode-classic");
    var tab = "idle";
    if (item === "inflation-input") tab = "input";
    else if (item === "inflation-food") tab = "food";
    else if (item === "inflation-livestock") tab = "live";
    showIsokoTab(room, tab);
    var shown = room.querySelector("#factor-classic");
    if (reportItem) shown = room.querySelector("#iso-report");
    else if (tab === "input" || tab === "food" || tab === "live") shown = room.querySelector("#price-deck");
    else if (/^(interest|jobs|gdp|fx)-/.test(item || "")) shown = room.querySelector("#force-deck");
    announceShown(shown || room);
  }

  function showIsokoTab(room, tab) {
    var list = room.querySelector("#isoko_tabs");
    if (!list) return;
    var chosen = null;
    list.querySelectorAll("a[data-value]").forEach(function (a) {
      var on = a.getAttribute("data-value") === tab;
      a.parentElement.classList.toggle("active", on);
      if (on) chosen = a.getAttribute("href");
    });
    var content = room.querySelector(".tab-content[data-tabsetid='" + list.getAttribute("data-tabsetid") + "']");
    if (content) {
      content.querySelectorAll(".tab-pane").forEach(function (pane) {
        var on = !!(chosen && ("#" + pane.id) === chosen);
        pane.classList.toggle("active", on);
        pane.style.display = on ? "block" : "none";
      });
    }
    if (window.Shiny) window.Shiny.setInputValue("isoko_tabs", tab, { priority: "event" });
    setTimeout(function () {
      window.dispatchEvent(new Event("resize"));
      if (window.UmuganuraMaps) UmuganuraMaps.invalidate(room);
    }, 100);
  }

  // Outputs suspend while hidden; tell Shiny when CSS reveals them.
  function announceShown(el) {
    if (window.jQuery) window.jQuery(el || document).trigger("shown");
  }

  document.addEventListener("toggle", function (e) {
    if (e.target && e.target.tagName === "DETAILS" && e.target.open) announceShown(e.target);
  }, true);

  document.addEventListener("click", function (e) {
    var zoom = e.target.closest("[data-zoom]");
    if (zoom) {
      var box = zoom.closest(".map-zoom");
      if (box) {
        var now = parseFloat(box.getAttribute("data-scale") || "1");
        now = zoom.getAttribute("data-zoom") === "in" ? Math.min(2.4, now + 0.25) : Math.max(1, now - 0.25);
        box.setAttribute("data-scale", String(now));
        var svg = box.querySelector("svg");
        if (svg) svg.style.transform = "scale(" + now + ")";
      }
      return;
    }
    var grow = e.target.closest("[data-grow]");
    if (grow) {
      var list = document.getElementById("force-list");
      if (list) list.classList.toggle("is-tall");
      return;
    }
    var back = e.target.closest("[data-back]");
    if (back) {
      var land = document.getElementById("factor-land");
      var room = document.getElementById("factor-room");
      if (land) land.classList.remove("is-hidden");
      if (room) room.classList.add("is-hidden");
      return;
    }
    var opener = e.target.closest("[data-open]");
    if (opener) {
      pinScroll(3500);
      var openId = opener.getAttribute("data-open");
      var openItem = opener.getAttribute("data-item");
      var landEl = document.getElementById("factor-land");
      var roomEl = document.getElementById("factor-room");
      if (landEl) landEl.classList.add("is-hidden");
      if (roomEl) roomEl.classList.remove("is-hidden");
      var pill = document.getElementById("room-pill");
      if (pill) pill.textContent = opener.getAttribute("data-label") || openId;
      document.querySelectorAll(".factor-group").forEach(function (el) {
        el.classList.toggle("is-hidden", el.getAttribute("data-group") !== openId);
      });
      document.querySelectorAll(".factor-link").forEach(function (el) {
        el.classList.toggle("is-on", el.getAttribute("data-item") === openItem);
      });
      setRoomMode(openItem);
      if (window.Shiny) {
        window.Shiny.setInputValue("factor_pick", { force: openId, item: openItem }, { priority: "event" });
        window.Shiny.setInputValue("factor_ready", Date.now(), { priority: "event" });
      }
      if (isReport(openItem)) return;
      setTimeout(function () {
        window.dispatchEvent(new Event("resize"));
        if (window.UmuganuraMaps && roomEl) UmuganuraMaps.invalidate(roomEl);
      }, 60);
      return;
    }
    // Pin scroll for room switches / selects so Shiny redraws do not yank to top.
    if (e.target.closest(".factor-link, .hv-step-btn, .ej-act, .dock-btn, [data-force], [data-item]")) {
      pinScroll(3500);
    }
    var force = e.target.closest("[data-force]");
    if (!force) {
      var flink = e.target.closest(".factor-link[data-item]");
      if (flink) {
        force = flink;
      }
    }
    if (force) {
      var id = force.getAttribute("data-force");
      var item = force.getAttribute("data-item");
      if (!id && item) {
        var grp = force.closest(".factor-group");
        id = grp ? grp.getAttribute("data-group") : null;
        if (!id) {
          var onDock = document.querySelector(".dock-btn.is-on");
          id = onDock ? onDock.getAttribute("data-force") : null;
        }
      }
      if (item) {
        document.querySelectorAll(".factor-card, .factor-link").forEach(function (el) {
          el.classList.toggle("is-on", el.getAttribute("data-item") === item);
        });
        if (id) {
          document.querySelectorAll(".dock-btn").forEach(function (el) {
            el.classList.toggle("is-on", el.getAttribute("data-force") === id);
          });
          document.querySelectorAll(".factor-group").forEach(function (el) {
            el.classList.toggle("is-hidden", el.getAttribute("data-group") !== id);
          });
        }
        setRoomMode(item);
        if (window.Shiny && id) {
          window.Shiny.setInputValue("factor_pick", { force: id, item: item }, { priority: "event" });
          window.Shiny.setInputValue("factor_ready", Date.now(), { priority: "event" });
        }
        if (isReport(item)) return;
        setTimeout(function () {
          window.dispatchEvent(new Event("resize"));
          if (window.Plotly) {
            document.querySelectorAll("#factor-room .js-plotly-plot").forEach(function (el) {
              window.Plotly.Plots.resize(el);
            });
          }
          var fr = document.getElementById("factor-room");
          if (window.UmuganuraMaps && fr) UmuganuraMaps.invalidate(fr);
        }, 120);
        return;
      }
    }
    // Only real nav / CTA controls — never body[data-panel] (set on every panel visit).
    // Without this filter, clicks on Inspect selects matched body and kicked users to land.
    var link = e.target.closest("a[data-panel], button[data-panel]");
    if (!link) return;
    if (e.target.closest("#ejo-room .ej-inspect, #ejo-room .hv-step, #ejo-room .ej-desk, .ej-sol-shell")) {
      return;
    }
    e.preventDefault();
    var pid = link.getAttribute("data-panel");
    // Top-nav EJO tab: land overview. Stay in room if already working a cause.
    if (pid === "ejo-hazaza" && typeof window.ejoOpenLand === "function") {
      var inRoom = typeof window.ejoRoomOpen === "function" && window.ejoRoomOpen();
      var inInspect = document.body.classList.contains("ej-inspect-open");
      if (inRoom || inInspect) {
        showPanel("ejo-hazaza");
        return;
      }
      window.ejoOpenLand();
      announceShown(document.getElementById("ejo-hazaza"));
      return;
    }
    showPanel(pid);
  });

  function openHash() {
    var id = (location.hash || "").replace("#", "");
    var panel = id && document.getElementById(id);
    if (!panel || !panel.classList.contains("panel")) return;
    showPanel(id);
    document.body.setAttribute("data-panel", id);
  }
  openHash();
  window.addEventListener("hashchange", openHash);

  // Freeze window scroll while selects / Shiny outputs refresh (Isoko filters etc.).
  function isSelecty(el) {
    return !!(el && el.closest && el.closest(
      "select, .selectize-control, .selectize-dropdown, .selectize-input, .shiny-input-container, .form-group, .selectize-dropdown-content, .price-filters, .hv-filters, .ej-inst"
    ));
  }
  document.addEventListener("pointerdown", function (e) { if (isSelecty(e.target)) pinScroll(3500); }, true);
  document.addEventListener("mousedown", function (e) { if (isSelecty(e.target)) pinScroll(3500); }, true);
  document.addEventListener("touchstart", function (e) { if (isSelecty(e.target)) pinScroll(3500); }, true);
  document.addEventListener("focusin", function (e) { if (isSelecty(e.target)) pinScroll(3500); }, true);
  document.addEventListener("change", function (e) { if (isSelecty(e.target)) pinScroll(3500); }, true);
  // Only EXTEND an existing pin during Shiny redraws — never capture a jumped-to-top Y.
  document.addEventListener("shiny:recalculating", function () { extendPin(2800); }, true);
  document.addEventListener("shiny:visualchange", function () { extendPin(2800); }, true);
  document.addEventListener("shiny:value", function () { extendPin(1600); }, true);
  document.addEventListener("shiny:idle", function () {
    if (!pinArmed || pinY == null) return;
    pinUntil = Date.now() + 400;
  }, true);
  // Intentional page scroll ends the pin. Do NOT listen to "scroll" and call
  // scrollTo from inside it — that loops and freezes the tab.
  window.addEventListener("wheel", function (e) {
    if (!pinArmed) return;
    if (e.target && e.target.closest && e.target.closest(".selectize-dropdown, .selectize-input, select")) return;
    clearPin();
  }, { passive: true, capture: true });
  window.addEventListener("touchmove", function (e) {
    if (!pinArmed) return;
    if (e.target && e.target.closest && e.target.closest(".selectize-dropdown, .selectize-input, select")) return;
    clearPin();
  }, { passive: true, capture: true });
  // Lightweight: only re-pin when selectize opens (class), not every DOM text mutate.
  if (window.MutationObserver) {
    var mo = new MutationObserver(function (mutations) {
      for (var i = 0; i < mutations.length; i++) {
        var t = mutations[i].target;
        if (t && t.classList && t.classList.contains("dropdown-active")) {
          pinScroll(2200);
          break;
        }
      }
    });
    function observeSelectize() {
      if (!document.body) return;
      mo.observe(document.body, { attributes: true, attributeFilter: ["class"], subtree: true });
    }
    if (document.body) observeSelectize();
    else document.addEventListener("DOMContentLoaded", observeSelectize);
  }
  if (window.jQuery) {
    window.jQuery(document).on("mousedown", ".selectize-dropdown .option, .selectize-input", function () { pinScroll(2200); });
    window.jQuery(document).on("change", "select", function () { pinScroll(2200); });
  }

  var isoWords = ["inflation", "interest rate", "exchange rate", "GDP", "unemployment"];
  var isoSlot = document.getElementById("iso-word");
  var isoI = 0;
  if (isoSlot) {
    setInterval(function () {
      isoSlot.classList.add("is-out");
      setTimeout(function () {
        isoI = (isoI + 1) % isoWords.length;
        isoSlot.textContent = isoWords[isoI];
        isoSlot.classList.remove("is-out");
      }, 280);
    }, 2200);
  }

  function syncIsoGap() {
    var left = document.querySelector("#factor-land .iso-col:not(.iso-right)");
    var right = document.querySelector("#factor-land .iso-col.iso-right");
    if (!left || !right || !left.offsetParent) return;
    var cards = left.querySelectorAll(".iso-card");
    if (cards.length < 2) return;
    var gap = cards[1].getBoundingClientRect().top - cards[0].getBoundingClientRect().bottom;
    if (gap > 12) right.style.gap = Math.round(gap) + "px";
  }
  var kaliza = document.querySelector(".iso-scene img");
  if (kaliza) {
    if (kaliza.complete) syncIsoGap();
    else kaliza.addEventListener("load", syncIsoGap);
  }
  window.addEventListener("resize", syncIsoGap);
  setTimeout(syncIsoGap, 120);

  var flash = document.createElement("div");
  flash.className = "click-flash";
  document.body.appendChild(flash);
  document.addEventListener("mousemove", function (e) {
    var near = null;
    var best = 48;
    document.querySelectorAll(".iso-card, .ik-node").forEach(function (card) {
      if (!card.offsetParent) return;
      var r = card.getBoundingClientRect();
      var cx = Math.min(Math.max(e.clientX, r.left), r.right);
      var cy = Math.min(Math.max(e.clientY, r.top), r.bottom);
      var dist = Math.hypot(e.clientX - cx, e.clientY - cy);
      if (dist < best) { best = dist; near = r; }
    });
    if (!near) { flash.style.display = "none"; return; }
    flash.style.display = "block";
    flash.style.left = (near.left - 52) + "px";
    flash.style.top = (near.top + near.height / 2 - 23) + "px";
  });
})();
