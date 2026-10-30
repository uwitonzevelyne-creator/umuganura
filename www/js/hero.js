(function () {
  const CROPS = [
    { id: "coffee", label: "Coffee" },
    { id: "avocado", label: "Avocado" },
    { id: "banana", label: "Banana" },
    { id: "rice", label: "Rice" },
    { id: "sorghum", label: "Sorghum" },
    { id: "maize", label: "Maize" },
    { id: "bean", label: "Bean" }
  ];

  const HOLD_MS = 4000;
  const WALK_MS = 32000;

  const navLinks = document.querySelectorAll("[data-panel]");
  const panels = document.querySelectorAll(".panel");
  const cropImgs = document.querySelectorAll(".center-crop img");
  const cropName = document.querySelector(".crop-name");
  const walker = document.querySelector(".walker");
  const basket = document.querySelector(".basket-fill");
  const path = document.getElementById("walk-path");
  const svg = document.querySelector(".imigongo");
  const mapWrap = document.querySelector(".map-wrap");
  const farmer = document.querySelector(".farmer");
  const btnExplore = document.querySelector(".btn-explore");
  const btnPlay = document.querySelector(".btn-play");

  let cropIndex = 0;
  let playing = true;
  let walkStart = performance.now();
  let cropTimer = null;

  function showPanel(id) {
    panels.forEach(function (p) {
      p.classList.toggle("is-active", p.id === id);
    });
    navLinks.forEach(function (a) {
      a.classList.toggle("is-active", a.getAttribute("data-panel") === id);
    });
  }

  navLinks.forEach(function (a) {
    a.addEventListener("click", function (e) {
      e.preventDefault();
      goPanel(a.getAttribute("data-panel"));
    });
  });

  function goPanel(id) {
    showPanel(id);
    const home = document.getElementById("home");
    if (home && id === "home") home.scrollTop = 0;
  }

  if (btnExplore) {
    btnExplore.addEventListener("click", function () {
      goPanel("home");
      const el = document.getElementById("worth");
      if (el) el.scrollIntoView({ behavior: "smooth", block: "start" });
    });
  }

  document.querySelectorAll("[data-scroll]").forEach(function (btn) {
    btn.addEventListener("click", function () {
      const el = document.getElementById(btn.getAttribute("data-scroll"));
      if (el) el.scrollIntoView({ behavior: "smooth", block: "start" });
    });
  });

  if (btnPlay) {
    btnPlay.addEventListener("click", function () {
      playing = !playing;
      btnPlay.textContent = playing ? "▶" : "❚❚";
      if (walker) walker.classList.toggle("is-present", !playing);
      if (playing) {
        walkStart = performance.now() - ((performance.now() - walkStart) % WALK_MS);
        tickCrops();
      }
    });
  }

  function setCrop(i) {
    cropIndex = i;
    const crop = CROPS[i];
    cropImgs.forEach(function (img) {
      img.classList.toggle("is-on", img.getAttribute("data-crop") === crop.id);
    });
    if (cropName) cropName.textContent = crop.label;
  }

  function basketTarget() {
    const box = walker.getBoundingClientRect();
    return {
      x: box.left + box.width * 0.18,
      y: box.top + box.height * 0.48
    };
  }

  function dropIntoBasket(cropId) {
    const srcImg = document.querySelector('.center-crop img[data-crop="' + cropId + '"]');
    if (!srcImg || !srcImg.classList.contains("is-on")) return;
    const from = srcImg.getBoundingClientRect();
    const piece = document.createElement("img");
    piece.src = srcImg.src;
    piece.alt = "";
    piece.className = "fly-piece";
    piece.style.left = from.left + from.width / 2 - 28 + "px";
    piece.style.top = from.top + from.height / 2 - 28 + "px";
    document.body.appendChild(piece);

    requestAnimationFrame(function () {
      const to = basketTarget();
      piece.style.left = to.x + "px";
      piece.style.top = to.y + "px";
      piece.style.transform = "scale(0.28)";
    });

    setTimeout(function () {
      const kept = document.createElement("img");
      kept.src = srcImg.src;
      kept.alt = cropId;
      basket.appendChild(kept);
      piece.remove();
    }, 920);
  }

  function presentAndEmpty() {
    walker.classList.add("is-present");
    const gifts = Array.prototype.slice.call(basket.querySelectorAll("img"));
    const dest = farmer ? farmer.getBoundingClientRect() : { left: 80, top: window.innerHeight - 80, width: 80, height: 80 };

    gifts.forEach(function (g, n) {
      const box = g.getBoundingClientRect();
      const fly = document.createElement("img");
      fly.src = g.src;
      fly.className = "fly-piece";
      fly.style.left = box.left + "px";
      fly.style.top = box.top + "px";
      fly.style.width = box.width + "px";
      fly.style.height = box.height + "px";
      document.body.appendChild(fly);
      setTimeout(function () {
        fly.style.left = dest.left + dest.width / 2 + "px";
        fly.style.top = dest.top + 20 + "px";
        fly.style.transform = "scale(0.2)";
        fly.style.opacity = "0";
      }, 60 + n * 80);
      setTimeout(function () { fly.remove(); }, 1100 + n * 80);
    });

    setTimeout(function () {
      basket.innerHTML = "";
      walker.classList.remove("is-present");
      setCrop(0);
    }, 1400);
  }

  function tickCrops() {
    clearTimeout(cropTimer);
    if (!playing) return;
    cropTimer = setTimeout(function () {
      if (!playing) return;
      dropIntoBasket(CROPS[cropIndex].id);
      const next = cropIndex + 1;
      if (next >= CROPS.length) {
        setTimeout(presentAndEmpty, 1000);
        cropTimer = setTimeout(function () {
          walkStart = performance.now();
          tickCrops();
        }, HOLD_MS);
      } else {
        setTimeout(function () { setCrop(next); }, 500);
        tickCrops();
      }
    }, HOLD_MS);
  }

  function placeWalker(now) {
    if (!path || !svg || !mapWrap || !walker) return;
    const t = ((now - walkStart) % WALK_MS) / WALK_MS;
    const len = path.getTotalLength();
    const p = path.getPointAtLength(t * len);
    const ctm = path.getScreenCTM();
    if (!ctm) return;
    const pt = svg.createSVGPoint();
    pt.x = p.x;
    pt.y = p.y;
    const scr = pt.matrixTransform(ctm);
    const wrap = mapWrap.getBoundingClientRect();
    walker.style.left = scr.x - wrap.left + "px";
    walker.style.top = scr.y - wrap.top + "px";
  }

  function loop(now) {
    if (playing && !walker.classList.contains("is-present")) {
      placeWalker(now);
    }
    requestAnimationFrame(loop);
  }

  setCrop(0);
  walker.classList.add("is-bob");
  requestAnimationFrame(loop);
  setTimeout(tickCrops, 600);
})();
