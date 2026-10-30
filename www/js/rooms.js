(function () {
  function announce(el) {
    if (window.jQuery) window.jQuery(el || document).trigger("shown");
  }

  function resizeSoon(scope) {
    setTimeout(function () {
      window.dispatchEvent(new Event("resize"));
      var root = scope || document;
      if (window.Plotly) {
        root.querySelectorAll(".js-plotly-plot").forEach(function (el) {
          try { window.Plotly.Plots.resize(el); } catch (err) {}
        });
      }
      if (window.UmuganuraMaps) UmuganuraMaps.invalidate(root);
    }, 90);
  }

  function showStep(scope, n) {
    if (!scope) return;
    var key = String(n);
    scope.querySelectorAll(".hv-step").forEach(function (s) {
      s.classList.toggle("is-on", s.getAttribute("data-step") === key);
    });
    scope.querySelectorAll(".hv-step-btn").forEach(function (b) {
      b.classList.toggle("is-on", b.getAttribute("data-step") === key);
    });
    announce(scope);
    resizeSoon(scope);
  }

  function openCrop(id, label) {
    var land = document.getElementById("harvest-land");
    var room = document.getElementById("harvest-room");
    if (!room || !id) return;
    if (!label) {
      var card = document.querySelector('.hv-card[data-crop="' + id + '"]');
      label = card ? card.getAttribute("data-label") : id;
    }
    if (land) land.classList.add("is-hidden");
    room.classList.remove("is-hidden");
    var pill = document.getElementById("hv-pill");
    if (pill) pill.textContent = label;
    showStep(room, 1);
    if (window.Shiny && window.Shiny.setInputValue) {
      window.Shiny.setInputValue("hv_crop", { id: id, nonce: Date.now() }, { priority: "event" });
    }
  }
  window.umuOpenCrop = openCrop;

  function filterCards() {
    var q = (document.getElementById("hv-search") || { value: "" }).value.trim().toLowerCase();
    var on = document.querySelector(".hv-chip.is-on");
    var cat = on ? on.getAttribute("data-cat") : "all";
    document.querySelectorAll(".hv-cat").forEach(function (box) {
      var catOk = cat === "all" || box.getAttribute("data-cat") === cat;
      var shown = 0;
      box.querySelectorAll(".hv-card").forEach(function (c) {
        var ok = catOk && (!q || c.getAttribute("data-name").indexOf(q) >= 0);
        c.style.display = ok ? "" : "none";
        if (ok) shown++;
      });
      box.style.display = shown ? "" : "none";
    });
  }

  document.addEventListener("input", function (e) {
    if (e.target && e.target.id === "hv-search") filterCards();
  });

  document.addEventListener("click", function (e) {
    var panelLink = e.target.closest("a[data-panel], button[data-panel]");
    if (panelLink) document.body.setAttribute("data-panel", panelLink.getAttribute("data-panel"));

    var card = e.target.closest(".hv-card");
    if (card) {
      openCrop(card.getAttribute("data-crop"), card.getAttribute("data-label"));
      return;
    }
    if (e.target.closest("[data-hv-back]")) {
      var land = document.getElementById("harvest-land");
      var room = document.getElementById("harvest-room");
      if (room) room.classList.add("is-hidden");
      if (land) land.classList.remove("is-hidden");
      return;
    }
    var ikGo = e.target.closest("[data-ik-go]");
    if (ikGo) {
      var ikLand = document.getElementById("ik-land");
      var ikRoom = document.getElementById("ik-room");
      if (ikLand) ikLand.classList.add("is-hidden");
      if (ikRoom) {
        ikRoom.classList.remove("is-hidden");
        ikRoom.classList.add("ik-solo");
      }
      showStep(ikRoom, ikGo.getAttribute("data-ik-go"));
      return;
    }
    if (e.target.closest("[data-ik-back]")) {
      var ikLandBack = document.getElementById("ik-land");
      var ikRoomBack = document.getElementById("ik-room");
      if (ikRoomBack) {
        ikRoomBack.classList.add("is-hidden");
        ikRoomBack.classList.remove("ik-solo");
      }
      if (ikLandBack) ikLandBack.classList.remove("is-hidden");
      return;
    }
    var step = e.target.closest(".hv-step-btn, [data-step-go]");
    if (step) {
      // Ejo room steps are handled in capture by ejo.js (keeps land from resetting).
      if (step.closest("#ejo-room")) return;
      showStep(step.closest(".hv-room"), step.getAttribute("data-step") || step.getAttribute("data-step-go"));
      return;
    }
    var chip = e.target.closest(".hv-chip");
    if (chip) {
      document.querySelectorAll(".hv-chip").forEach(function (c) { c.classList.toggle("is-on", c === chip); });
      filterCards();
    }
  });

  var hashId = (location.hash || "").replace("#", "");
  var hashPanel = hashId && document.getElementById(hashId);
  if (hashPanel && hashPanel.classList.contains("panel")) document.body.setAttribute("data-panel", hashId);
  else document.body.setAttribute("data-panel", "home");
})();
