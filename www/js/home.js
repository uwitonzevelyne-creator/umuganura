(function () {
  var reduce = window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  function typeLoop(el, phrase, pace, hold) {
    if (!el) return;
    if (reduce) {
      el.textContent = phrase;
      return;
    }
    var n = 0;
    function tick() {
      n += 1;
      el.textContent = phrase.slice(0, n);
      if (n >= phrase.length) {
        setTimeout(function () {
          n = 0;
          el.textContent = "";
          setTimeout(tick, 360);
        }, hold || 1700);
      } else {
        setTimeout(tick, pace || 48);
      }
    }
    tick();
  }

  typeLoop(document.getElementById("hx-type"), "What We Lose in Agriculture EVERY YEAR", 48, 1700);
  typeLoop(document.getElementById("hx-float"), "Know the harvest first. Then stop losing it.", 42, 1900);

  var rail = document.getElementById("hx-rail");
  var detail = document.getElementById("hx-detail");
  var stage = document.getElementById("hx-bike");
  if (!rail || !detail) return;

  var kicker = detail.querySelector(".hx-detail-kicker");
  var title = detail.querySelector("h3");
  var copy = detail.querySelector(".hx-detail-copy");
  var photo = detail.querySelector("img");
  var openBtn = detail.querySelector(".hx-open");
  var paused = false;
  var offset = 0;
  var moving = false;

  function cardWidth() {
    var card = rail.querySelector(".hx-card");
    if (!card) return 280;
    var gap = 16;
    return card.getBoundingClientRect().width + gap;
  }

  function showCard(card) {
    rail.querySelectorAll(".hx-card").forEach(function (c) {
      c.classList.toggle("is-on", c === card);
    });
    kicker.textContent = card.getAttribute("data-kicker") || "";
    title.textContent = card.getAttribute("data-title") || "";
    copy.textContent = card.getAttribute("data-copy") || "";
    photo.src = card.getAttribute("data-img") || "";
    photo.alt = "";
    openBtn.textContent = card.getAttribute("data-open") || "Open";
    openBtn.setAttribute("data-panel", card.getAttribute("data-room") || "");
    detail.hidden = false;
    paused = true;
  }

  rail.addEventListener("click", function (e) {
    var card = e.target.closest(".hx-card");
    if (!card) return;
    if (card.classList.contains("is-on") && !detail.hidden) {
      card.classList.remove("is-on");
      detail.hidden = true;
      paused = false;
      return;
    }
    showCard(card);
  });

  if (stage) {
    stage.addEventListener("mouseenter", function () { paused = true; });
    stage.addEventListener("mouseleave", function () {
      if (detail.hidden) paused = false;
    });
  }

  function loop() {
    if (!paused && moving) {
      offset += 0.55;
      var w = cardWidth();
      if (offset >= w) {
        offset -= w;
        var first = rail.querySelector(".hx-card");
        if (first) rail.appendChild(first);
      }
      rail.style.transform = "translateX(" + (-offset) + "px)";
    }
    requestAnimationFrame(loop);
  }

  var reduce = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  if (!reduce) {
    setTimeout(function () {
      rail.querySelectorAll(".hx-card").forEach(function (c) { c.style.animation = "none"; });
      moving = true;
    }, 1900);
    requestAnimationFrame(loop);
  }
})();
