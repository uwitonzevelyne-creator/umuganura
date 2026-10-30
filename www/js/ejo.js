(function () {
  var scenes = [
    {
      title: "Overview of post-harvest agricultural loss in Rwanda every year?",
      body: "We use <span class=\"ej-or\">operations research</span> to follow the harvest after it leaves the field, name the causes, and price the solutions.",
      tag: "1 \u00b7 Overview (Post Harvest Loss)",
      hero: "ejo/land/overview.png",
      thumb: "ejo/land/rain.png",
      money: "overview",
      hold: 3000
    },
    {
      title: "Rain at harvest",
      body: "Rain in the harvest weeks leaves maize and beans wet in the field. Aflatoxin and sprouting follow. The rain is weather. The loss grows when that week has nowhere to dry the crop.",
      tag: "2 \u00b7 Rain at harvest (only a natural event), and its effect can be managed",
      hero: "ejo/land/rain.png",
      thumb: "ejo/land/drying.png",
      money: "weather",
      hold: 3000
    },
    {
      title: "Drying",
      body: "Grain bagged while it is still wet heats up in the store. Mould, insects and sprouting then take the part of the harvest that a clean drying floor or a dryer would have kept.",
      tag: "3 \u00b7 Poor drying of yield",
      hero: "ejo/land/drying.png",
      thumb: "ejo/land/handling.png",
      money: "storage",
      hold: 3000
    },
    {
      title: "Handling",
      body: "Sacks that are overfilled, thrown and dragged split and bruise the crop. That damage is loss, even when the produce was sound in the field.",
      tag: "4 \u00b7 Mishandling of agricultural products leads to spoilage",
      hero: "ejo/land/handling.png",
      thumb: "ejo/land/truck.png",
      money: "handling",
      hold: 3000
    },
    {
      title: "Transport routes",
      body: "A long haul such as Musanze\u2013Nyabugogo\u2013Rusizi keeps produce on the road. Extra hours, extra loading and extra kilometres crush the crop and spend the margin before a nearer market is tried.",
      tag: "5 \u00b7 Farmers use the same market place without navigating other options",
      hero: "ejo/land/truck.png",
      thumb: "ejo/land/cooling.png",
      money: "transport",
      hold: 3000
    },
    {
      title: "Lack of cooling",
      body: "Avocado, tomato and other fresh crops keep respiring after harvest. Without a cold room they soften and spoil before they can be sold.",
      tag: "6 \u00b7 Spoilage of agriculture products due to an uncooled environment",
      hero: "ejo/land/cooling.png",
      thumb: "ejo/land/supply.png",
      money: "cold",
      hold: 3000
    },
    {
      title: "Too much supply",
      body: "When growers send the same crop to one market, Nyabugogo fills and the price falls, while other markets stay short. What cannot be sold in time is lost.",
      tag: "7 \u00b7 Too much supply on one market",
      hero: "ejo/land/supply.png",
      thumb: "ejo/land/solutions.png",
      money: "glut",
      hold: 3000
    },
    {
      title: "Action plan",
      body: "Each solution has a lead, a start counted from January 2026, a place already on its page, and a progress report the lead files on this platform.",
      tag: "8 \u00b7 Solutions: who does the work, when, where, and where the report goes",
      hero: "ejo/land/solutions.png",
      thumb: "ejo/land/overview.png",
      money: "gain",
      hold: 6000
    }
  ];

  var i = 0;
  var timer = null;
  var paused = false;
  var land = document.getElementById("ejo-land");

  function box(cls, strong, small) {
    return '<article class="' + cls + '"><strong>' + strong + '</strong><span>' + small + '</span></article>';
  }

  function show(n) {
    if (!land || !window.EJO_LAND) return;
    i = (n + scenes.length) % scenes.length;
    var s = scenes[i];
    var pack = s.money === "gain" ? null : window.EJO_LAND[s.money];
    document.getElementById("ej-title").textContent = s.title;
    document.getElementById("ej-body").innerHTML = s.body;
    document.getElementById("ej-tag").textContent = s.tag;
    var hero = document.getElementById("ej-hero");
    var thumb = document.getElementById("ej-thumb");
    hero.src = s.hero + "?v=5";
    hero.alt = s.title;
    thumb.src = s.thumb + "?v=5";
    thumb.alt = "Next";
    var prices = document.getElementById("ej-prices");
    var predict = document.getElementById("ej-predict");
    if (s.money === "gain") {
      var ap = window.EJO_LAND.action || window.EJO_LAND.overview;
      predict.classList.remove("is-off");
      prices.innerHTML =
        box("ej-money", ap.farmer, "Money lost by farmers, " + ap.span) +
        box("ej-money", ap.gov, "Money lost by government, " + ap.span);
      predict.innerHTML = box(
        "ej-next5",
        ap.ahead,
        "Predictive model, " + ap.aheadSpan + ". What will continue to be lost"
      );
    } else {
      predict.classList.remove("is-off");
      prices.innerHTML =
        box("ej-money", pack.farmer, "Money lost by farmers, " + pack.span) +
        box("ej-money", pack.gov, "Money lost by government, " + pack.span);
      predict.innerHTML = box("ej-next5", pack.ahead, "Predictive model, " + pack.aheadSpan + ". What will continue to be lost");
    }
    clearTimeout(timer);
    if (!paused) timer = setTimeout(function () { show(i + 1); }, s.hold);
  }

  function resetRoomOverview() {
    var room = document.getElementById("ejo-room");
    if (!room) return;
    room.querySelectorAll(".hv-step").forEach(function (s) {
      s.classList.toggle("is-on", s.getAttribute("data-step") === "0");
      s.classList.remove("is-detail");
    });
    room.querySelectorAll(".hv-step-btn").forEach(function (b) {
      b.classList.toggle("is-on", b.getAttribute("data-step") === "0");
    });
  }

  function openRoom() {
    var room = document.getElementById("ejo-room");
    var landEl = document.getElementById("ejo-land") || land;
    document.querySelectorAll(".panel").forEach(function (p) {
      p.classList.toggle("is-active", p.id === "ejo-hazaza");
    });
    document.body.setAttribute("data-panel", "ejo-hazaza");
    if (landEl) landEl.classList.add("is-hidden");
    if (room) room.classList.remove("is-hidden");
    resetRoomOverview();
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue("ej_active_step", "0", { priority: "event" });
    }
    clearTimeout(timer);
    window.scrollTo(0, 0);
    setTimeout(function () {
      if (window.jQuery && room) window.jQuery(room).trigger("shown");
      window.dispatchEvent(new Event("resize"));
      if (window.Plotly && room) {
        room.querySelectorAll(".hv-step.is-on .js-plotly-plot").forEach(function (el) {
          try { window.Plotly.Plots.resize(el); } catch (err) {}
        });
      }
    }, 80);
  }

  function openPlan() {
    if (window.ejoOpenActionPlan) {
      window.ejoOpenActionPlan();
      return;
    }
    openRoom();
    var room = document.getElementById("ejo-room");
    var btn = room && room.querySelector(".hv-step-btn[data-step='7']");
    if (btn) btn.click();
  }

  function openLand() {
    document.querySelectorAll(".panel").forEach(function (p) {
      p.classList.toggle("is-active", p.id === "ejo-hazaza");
    });
    document.querySelectorAll("[data-panel]").forEach(function (a) {
      a.classList.toggle("is-active", a.getAttribute("data-panel") === "ejo-hazaza");
    });
    document.body.setAttribute("data-panel", "ejo-hazaza");
    var room = document.getElementById("ejo-room");
    var landEl = document.getElementById("ejo-land") || land;
    if (room) room.classList.add("is-hidden");
    if (landEl) landEl.classList.remove("is-hidden");
    resetRoomOverview();
    // Land carousel only — do not compute overview plotlys until Navigate opens the room.
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue("ej_active_step", "", { priority: "event" });
    }
    show(0);
    window.scrollTo(0, 0);
  }

  function showEjoStep(key) {
    var room = document.getElementById("ejo-room");
    var landEl = document.getElementById("ejo-land") || land;
    var scope = room && room.querySelector(".hv-room");
    if (!scope) return;
    key = String(key);
    if (landEl) landEl.classList.add("is-hidden");
    if (room) room.classList.remove("is-hidden");
    document.querySelectorAll(".panel").forEach(function (p) {
      p.classList.toggle("is-active", p.id === "ejo-hazaza");
    });
    document.body.setAttribute("data-panel", "ejo-hazaza");
    scope.querySelectorAll(".hv-step").forEach(function (s) {
      s.classList.toggle("is-on", s.getAttribute("data-step") === key);
      s.classList.remove("is-detail");
    });
    scope.querySelectorAll(".hv-step-btn").forEach(function (b) {
      b.classList.toggle("is-on", b.getAttribute("data-step") === key);
    });
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue("ej_active_step", key, { priority: "event" });
    }
    setTimeout(function () {
      if (window.jQuery) window.jQuery(scope).trigger("shown");
      window.dispatchEvent(new Event("resize"));
      if (window.UmuganuraMaps) UmuganuraMaps.invalidate(scope);
    }, 80);
  }

  window.ejoOpenLand = openLand;
  window.ejoOpenRoom = openRoom;
  window.ejoShowStep = showEjoStep;
  window.ejoRoomOpen = function () {
    var room = document.getElementById("ejo-room");
    return !!(room && !room.classList.contains("is-hidden"));
  };

  if (land) {
    var pauseBtn = document.getElementById("ej-pause");
    if (pauseBtn) {
      pauseBtn.addEventListener("click", function () {
        paused = !paused;
        pauseBtn.classList.toggle("is-paused", paused);
        pauseBtn.setAttribute("aria-label", paused ? "Resume" : "Pause");
        if (paused) clearTimeout(timer);
        else timer = setTimeout(function () { show(i + 1); }, scenes[i].hold);
      });
    }
    var prev = document.querySelector(".ej-prev");
    var next = document.querySelector(".ej-next");
    if (prev) prev.addEventListener("click", function () { show(i - 1); });
    if (next) next.addEventListener("click", function () { show(i + 1); });
    function openEjoPortalLogin() {
      if (window.ejoOpenActionPlan) {
        window.ejoOpenActionPlan();
        return;
      }
      document.querySelectorAll(".panel").forEach(function (p) {
        p.classList.toggle("is-active", p.id === "ejo-solution");
      });
      document.body.setAttribute("data-panel", "ejo-solution");
      window.scrollTo(0, 0);
    }
    window.ejoOpenPortal = openEjoPortalLogin;

    // Capture phase: cause steps must win over any land-reset handlers.
    document.addEventListener("click", function (e) {
      var stepBtn = e.target.closest("#ejo-room .hv-step-btn, #ejo-room [data-step-go]");
      if (stepBtn) {
        e.preventDefault();
        e.stopPropagation();
        if (typeof e.stopImmediatePropagation === "function") e.stopImmediatePropagation();
        var key = stepBtn.getAttribute("data-step") || stepBtn.getAttribute("data-step-go") || "0";
        // Action plan opens the One Acre–style solutions landing (no login desk).
        if (String(key) === "7") {
          openEjoPortalLogin();
          return;
        }
        showEjoStep(key);
        return;
      }
      var navBtn = e.target.closest("[data-ejo-open]");
      if (navBtn) {
        e.preventDefault();
        e.stopPropagation();
        if (scenes[i] && scenes[i].money === "gain") openPlan();
        else openRoom();
        return;
      }
      // Top nav / home CTA only — never from inside the room.
      if (e.target.closest("#ejo-room, .ej-sol-shell")) return;
      var go = e.target.closest("a[data-panel='ejo-hazaza'], button[data-panel='ejo-hazaza']");
      if (go) openLand();
    }, true);
    show(0);
    if ((location.hash || "") === "#ejo-report") {
      document.querySelectorAll(".panel").forEach(function (p) {
        p.classList.toggle("is-active", p.id === "ejo-hazaza");
      });
      document.body.setAttribute("data-panel", "ejo-hazaza");
      openPlan();
    } else if ((location.hash || "") === "#ejo-hazaza") {
      openLand();
    }
  }

  function relayout(scope) {
    setTimeout(function () {
      $(scope).find(".js-plotly-plot").each(function () {
        if (window.Plotly) Plotly.Plots.resize(this);
      });
      $(scope).trigger("shown");
      $(window).trigger("resize");
    }, 120);
  }
  function openEjoSolution(scope, id) {
    if (window.Shiny) {
      Shiny.setInputValue("ej_act", { scope: scope, id: id, nonce: Date.now() }, { priority: "event" });
    }
    document.querySelectorAll(".panel").forEach(function (p) {
      p.classList.toggle("is-active", p.id === "ejo-solution");
    });
    document.querySelectorAll("[data-panel]").forEach(function (a) {
      a.classList.toggle("is-active", a.getAttribute("data-panel") === "ejo-hazaza");
    });
    document.body.setAttribute("data-panel", "ejo-solution");
    window.scrollTo(0, 0);
    setTimeout(function () {
      var app = document.getElementById("gsas-app");
      var logged = app && !app.classList.contains("is-hidden");
      if (logged && window.gsasShowView) window.gsasShowView("desk");
      if (window.jQuery) window.jQuery("#ejo-solution").trigger("shown");
      window.dispatchEvent(new Event("resize"));
    }, 80);
  }

  function backToEjoRoom() {
    document.querySelectorAll(".panel").forEach(function (p) {
      p.classList.toggle("is-active", p.id === "ejo-hazaza");
    });
    document.querySelectorAll("[data-panel]").forEach(function (a) {
      a.classList.toggle("is-active", a.getAttribute("data-panel") === "ejo-hazaza");
    });
    document.body.setAttribute("data-panel", "ejo-hazaza");
    var land = document.getElementById("ejo-land");
    var room = document.getElementById("ejo-room");
    if (land) land.classList.add("is-hidden");
    if (room) room.classList.remove("is-hidden");
    document.querySelectorAll(".ej-room .hv-step").forEach(function (s) {
      s.classList.remove("is-detail");
    });
    window.scrollTo(0, 0);
    setTimeout(function () {
      if (window.jQuery) window.jQuery("#ejo-room").trigger("shown");
      window.dispatchEvent(new Event("resize"));
    }, 80);
  }

  $(document).on("click", ".ej-act", function () {
    var scope = this.dataset.scope;
    var id = this.dataset.act;
    var kind = this.dataset.kind || "";
    // Solutions (cause list + action plan) open their own full page.
    if (kind === "solution" || scope === "ap" || /^[A-Z]\d+$/.test(id || "")) {
      openEjoSolution(scope, id);
      return;
    }
    var step = $(this).closest(".hv-step");
    step.addClass("is-detail");
    step.find(".ej-detail [id$='_detail']").html('<div class="ej-loading">Opening who does the work, and the report the team lead receives\u2026</div>');
    if (window.Shiny) {
      Shiny.setInputValue("ej_act", { scope: scope, id: id, nonce: Date.now() }, { priority: "event" });
    }
    relayout(step.find(".ej-detail"));
  });

  $(document).on("click", "[data-ej-back]", function () {
    var step = $(this).closest(".hv-step");
    step.removeClass("is-detail");
    relayout(step.find(".ej-main"));
  });

  window.ejoBackToRoom = backToEjoRoom;
  window.backToEjoRoom = backToEjoRoom;

  $(document).on("click", "[data-ej-sol-back]", function () {
    backToEjoRoom();
  });

  $(document).on("click", ".ej-room .hv-step-btn, .ej-room [data-step-go]", function () {
    $(".ej-room .hv-step").removeClass("is-detail");
  });

  $(document).on("shiny:value", function (e) {
    if (/^ej_.*_detail$/.test(e.name)) relayout(document.getElementById(e.name));
    if (e.name === "ej_sol_page") {
      relayout(document.getElementById("ejo-solution"));
      window.scrollTo(0, 0);
    }
  });

  if (window.Shiny) {
    Shiny.addCustomMessageHandler("ejo_mailto", function (msg) {
      if (!msg || !msg.url) return;
      window.location.href = msg.url;
    });
  }

})();
