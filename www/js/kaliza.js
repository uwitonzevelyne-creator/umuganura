(function () {
  var voiceOn = false;
  var lastText = "";
  var chart = null;
  var voices = [];
  var heygen = null;
  var hello = false;
  var speakTimer = null;
  var frameTimer = null;
  var frameI = 0;
  var restPose = "screenshoots/ejo%20hazaza/female-teacher-pointing-blackboard-holding-book-classroom-flat-vector-illustration-isolated-white-background-453465287.webp";
  var lastChart = { labels: [], values: [], unit: "" };
  var lastPayload = null;
  var talkPoses = [
    "amakuru/kaliza-talk-a.png?v=1",
    "amakuru/kaliza-talk-b.png?v=1",
    "amakuru/kaliza-talk-c.png?v=1"
  ];

  function el(id) { return document.getElementById(id); }

  if (window.speechSynthesis) {
    voices = window.speechSynthesis.getVoices();
    window.speechSynthesis.onvoiceschanged = function () { voices = window.speechSynthesis.getVoices(); };
  }

  // Warm East African English first, then any female English voice.
  function pickVoice() {
    var prefs = [/Asilia/i, /en-KE/i, /Zuri/i, /Imani/i, /en-TZ/i, /Ezinne/i, /en-NG/i, /Leah/i, /en-ZA/i,
      /Libby|Sonia|Hazel|Susan/i, /Aria|Jenny|Zira/i, /Google UK English Female/i, /female/i, /^en/i];
    for (var i = 0; i < prefs.length; i++) {
      for (var j = 0; j < voices.length; j++) {
        var v = voices[j];
        if (prefs[i].test(v.name) || prefs[i].test(v.lang)) {
          if (/male/i.test(v.name) && !/female/i.test(v.name) && i > 8) continue;
          return v;
        }
      }
    }
    return null;
  }

  function setSpeaking(on) {
    var a = el("kaliza-avatar");
    if (a) a.classList.toggle("is-speaking", on);
    if (!on) clearInterval(frameTimer);
  }

  function endGesture() {
    var a = el("kaliza-avatar");
    var right = document.querySelector(".ak-right");
    setSpeaking(false);
    setTimeout(function () {
      if (a) a.classList.remove("is-pointing");
      if (right) right.classList.remove("is-pointed");
    }, 1200);
  }

  function speak(p) {
    lastText = p.subtitle_text || "";
    clearTimeout(speakTimer);
    if (window.speechSynthesis) window.speechSynthesis.cancel();
    setSpeaking(true);
    var text = lastText;
    var guess = Math.max(4200, text.length * 55);
    if (!voiceOn) {
      speakTimer = setTimeout(endGesture, Math.min(guess, 14000));
      return;
    }
    if (heygen) {
      try {
        heygen.speak({ text: text, task_type: "repeat", taskType: "repeat" });
        speakTimer = setTimeout(endGesture, guess);
        return;
      } catch (err) { heygen = null; }
    }
    if (p.audio_stream_url) {
      var audio = new Audio(p.audio_stream_url);
      audio.onended = endGesture;
      audio.onerror = function () { browserSpeak(text, guess); };
      audio.play().catch(function () { browserSpeak(text, guess); });
      return;
    }
    browserSpeak(text, guess);
  }

  function browserSpeak(text, guess) {
    if (!window.speechSynthesis || !window.SpeechSynthesisUtterance) {
      speakTimer = setTimeout(endGesture, guess);
      return;
    }
    var u = new SpeechSynthesisUtterance(text);
    var v = pickVoice();
    if (v) { u.voice = v; u.lang = v.lang; } else { u.lang = "en-GB"; }
    u.rate = 0.96;
    u.pitch = 1.08;
    u.onend = endGesture;
    u.onerror = endGesture;
    u.onboundary = function () {
      var a = el("kaliza-avatar");
      if (a) a.classList.add("is-speaking");
    };
    window.speechSynthesis.speak(u);
    speakTimer = setTimeout(endGesture, guess + 4000);
  }

  function line(text, who) {
    var box = el("kaliza-subs");
    if (!box) return null;
    var p = document.createElement("p");
    p.className = "ak-line " + who;
    if (who === "user") p.textContent = text;
    box.appendChild(p);
    while (box.children.length > 14) box.removeChild(box.firstChild);
    box.scrollTop = box.scrollHeight;
    return p;
  }

  function typewrite(node, text) {
    if (!node) return;
    var i = 0;
    var box = el("kaliza-subs");
    var t = setInterval(function () {
      i += 2;
      node.textContent = text.slice(0, i);
      if (box) box.scrollTop = box.scrollHeight;
      if (i >= text.length) clearInterval(t);
    }, 28);
  }

  function fmt(v, unit) {
    if (v === null || v === undefined || isNaN(v)) return "\u2014";
    var abs = Math.abs(v);
    var d = abs >= 100 ? 0 : abs >= 10 ? 1 : 2;
    var s = Number(v).toLocaleString("en-US", { maximumFractionDigits: d, minimumFractionDigits: 0 });
    if (unit === "%") return s + "%";
    if (unit === "US$ m") return "$" + s + "m";
    return unit ? s + " " + unit : s;
  }

  function wrapLabel(s) {
    var words = String(s).split(" ");
    var out = [], cur = "";
    words.forEach(function (w) {
      if ((cur + " " + w).trim().length > 16) { if (cur) out.push(cur); cur = w; } else { cur = (cur + " " + w).trim(); }
    });
    if (cur) out.push(cur);
    return out;
  }

  function drawHeat(labels, values) {
    var box = el("kaliza-heat");
    if (!box) return;
    var nums = (values || []).map(function (v) { return Math.abs(Number(v) || 0); });
    var max = Math.max.apply(null, nums.concat([1]));
    box.innerHTML = "";
    (labels || []).forEach(function (lab, i) {
      var cell = document.createElement("i");
      var t = Math.min(1, (nums[i] || 0) / max);
      cell.style.background = "rgba(31, 95, 209, " + (0.18 + t * 0.75) + ")";
      cell.textContent = lab + " · " + (values[i] == null ? "" : values[i]);
      box.appendChild(cell);
    });
  }

  function renderChart(cd) {
    cd = cd || {};
    var labels = [].concat(cd.labels || []);
    var raw = cd.values || [];
    if (!Array.isArray(raw)) raw = typeof raw === "object" ? Object.keys(raw).map(function (k) { return raw[k]; }) : [raw];
    var values = raw.map(function (v) { return v === null ? null : Number(v); });
    var unit = cd.unit || "";
    lastPayload = cd;
    lastChart = { labels: labels, values: values, unit: unit };
    drawHeat(labels, values);
    if (cd.title) el("kaliza-chart-title").textContent = cd.title;
    var card = document.querySelector(".ak-chart-card");
    var kind = cd.type === "pie" ? "pie" : "bar";
    var wide = kind === "bar" && labels.length > 8;
    if (card) {
      card.classList.toggle("is-wait", !labels.length);
      card.style.height = kind === "pie" ? "380px" : (wide ? Math.max(460, labels.length * 22) + "px" : "420px");
    }
    var stats = el("kaliza-stats");
    if (stats) {
      stats.innerHTML = "";
      var shown = labels.length > 8 ? labels.slice(0, 6) : labels;
      shown.forEach(function (lab) {
        var i = labels.indexOf(lab);
        var d = document.createElement("div");
        d.className = "ak-stat ak-glass";
        d.innerHTML = "<span></span><b></b>";
        d.firstChild.textContent = lab;
        d.lastChild.textContent = fmt(values[i], unit);
        stats.appendChild(d);
      });
    }
    if (!labels.length || !window.Chart) return;
    var canvas = el("kaliza-chart");
    var ctx = canvas.getContext("2d");
    var grad = ctx.createLinearGradient(0, 0, 0, canvas.clientHeight || 320);
    grad.addColorStop(0, "rgba(183, 228, 199, 0.95)");
    grad.addColorStop(1, "rgba(45, 106, 79, 0.85)");
    var pieColors = ["#14804a", "#1b4332", "#2d6a4f", "#40916c", "#52b788", "#74c69d", "#1f5fd1"];
    if (chart) chart.destroy();
    var dataset = kind === "pie"
      ? { data: values, backgroundColor: labels.map(function (_, i) { return pieColors[i % pieColors.length]; }), borderColor: "#fff", borderWidth: 2 }
      : {
          data: values,
          backgroundColor: values.map(function (v) { return v < 0 ? "rgba(231, 111, 81, 0.85)" : grad; }),
          borderColor: "rgba(255, 255, 255, 0.75)",
          borderWidth: 1,
          borderRadius: 8,
          maxBarThickness: 22
        };
    chart = new window.Chart(ctx, {
      type: kind,
      data: { labels: (kind === "pie" || wide) ? labels : labels.map(wrapLabel), datasets: [dataset] },
      options: {
        indexAxis: wide ? "y" : "x",
        responsive: true,
        maintainAspectRatio: false,
        animation: { duration: 900, easing: "easeOutQuart" },
        plugins: {
          legend: { display: kind === "pie", position: "right", labels: { color: "#10243a", boxWidth: 12 } },
          tooltip: { callbacks: { label: function (c) { return (c.label || "") + ": " + fmt(kind === "pie" ? c.parsed : (wide ? c.parsed.x : c.parsed.y), unit); } } }
        },
        scales: kind === "pie" ? {} : {
          x: {
            beginAtZero: true,
            ticks: { color: "#1a3d2c", font: { size: 11 }, callback: wide ? function (v) { return fmt(v, ""); } : function (v) { return v; } },
            grid: { display: wide, color: "rgba(16,36,58,0.08)" }
          },
          y: {
            beginAtZero: !wide,
            ticks: {
              autoSkip: false,
              color: "#1a3d2c",
              font: { size: 11 },
              callback: wide ? function (v) { return this.getLabelForValue(v); } : function (v) { return fmt(v, unit === "%" ? "%" : ""); }
            },
            grid: { display: !wide, color: "rgba(16,36,58,0.08)" }
          }
        }
      }
    });
  }

  function renderMenu(items) {
    var box = el("kaliza-menu");
    if (!box) return;
    box.innerHTML = "";
    (items || []).forEach(function (t) {
      var b = document.createElement("button");
      b.type = "button";
      b.className = "ak-chip";
      b.textContent = t;
      b.setAttribute("data-say", t);
      box.appendChild(b);
    });
  }

  function navigate(nav) {
    if (!nav || !nav.panel) return;
    setTimeout(function () {
      var link = document.querySelector('.nav-links [data-panel="' + nav.panel + '"]');
      if (link) link.click();
      if (nav.crop && window.umuOpenCrop) setTimeout(function () { window.umuOpenCrop(nav.crop); }, 250);
    }, 1800);
  }

  function handle(raw) {
    var p = typeof raw === "string" ? JSON.parse(raw) : raw;
    try { renderChart(p.chart_data); } catch (err) { if (window.console) console.error("kaliza chart", err); }
    var a = el("kaliza-avatar");
    if (a) a.setAttribute("data-expr", p.avatar_expression_trigger || "guiding");
    if (p.avatar_gesture_command === "point_right_hand") {
      if (a) a.classList.add("is-pointing");
      var right = document.querySelector(".ak-right");
      if (right) right.classList.add("is-pointed");
    }
    typewrite(line("", "kaliza"), p.subtitle_text || "");
    var report = el("kaliza-report");
    if (report) report.textContent = p.subtitle_text || "";
    renderMenu(p.menu);
    speak(p);
    navigate(p.navigate);
  }

  function send(text) {
    text = (text || "").trim();
    if (!text || !window.Shiny) return;
    line(text, "user");
    window.Shiny.setInputValue("kaliza_msg", { text: text, nonce: Date.now() }, { priority: "event" });
    fetch("http://127.0.0.1:7452/api/deepseek", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ question: text, mode: "kaliza", figures: lastChart })
    }).then(function (r) { return r.json(); }).then(function (j) {
      if (!j || !j.ok || !j.text) return;
      typewrite(line("", "kaliza"), j.text);
      lastText = j.text;
      if (voiceOn) browserSpeak(j.text, Math.max(2500, j.text.length * 62));
    }).catch(function () {});
  }

  function greet() {
    if (hello || !window.Shiny || !window.Shiny.setInputValue) return;
    hello = true;
    window.Shiny.setInputValue("kaliza_hello", Date.now(), { priority: "event" });
  }

  document.addEventListener("submit", function (e) {
    if (e.target && e.target.id === "kaliza-form") {
      e.preventDefault();
      var inp = el("kaliza-input");
      send(inp.value);
      inp.value = "";
    }
  });

  document.addEventListener("click", function (e) {
    var chartBtn = e.target.closest("#amakuru [data-chart]");
    if (chartBtn && lastPayload) {
      var kind = chartBtn.getAttribute("data-chart");
      var card = document.querySelector(".ak-chart-card");
      if (kind === "heat") {
        if (card) card.style.display = "none";
        drawHeat(lastChart.labels, lastChart.values);
        return;
      }
      if (card) card.style.display = "";
      lastPayload.type = kind;
      renderChart(lastPayload);
      return;
    }
    var say = e.target.closest("#amakuru [data-say]");
    if (say) { send(say.getAttribute("data-say")); return; }
    var go = e.target.closest('[data-panel="amakuru"]');
    if (go) setTimeout(greet, 350);
    if (e.target.closest("#kaliza-mic")) {
      var mic = el("kaliza-mic");
      voiceOn = !voiceOn;
      if (mic) {
        mic.classList.toggle("is-on", voiceOn);
        mic.setAttribute("aria-pressed", voiceOn ? "true" : "false");
        mic.title = voiceOn ? "Turn Kaliza's voice off" : "Turn Kaliza's voice on";
      }
      if (!voiceOn) {
        if (window.speechSynthesis) window.speechSynthesis.cancel();
        setSpeaking(false);
        return;
      }
      if (lastText) browserSpeak(lastText, Math.max(2500, lastText.length * 62));
      return;
    }
  });

  function listen() {
    var R = window.SpeechRecognition || window.webkitSpeechRecognition;
    if (!R) return;
    var r = new R();
    r.lang = "en-US";
    r.interimResults = false;
    var mic = el("kaliza-mic");
    if (mic) mic.classList.add("is-on");
    r.onresult = function (ev) { send(ev.results[0][0].transcript); };
    r.onend = function () { if (mic) mic.classList.remove("is-on"); };
    r.start();
  }

  // Rotating placeholder hints in the chat bar.
  var hints = ["am a farmer\u2026", "Show maize yields\u2026", "Irrigation in Bugesera\u2026", "Beans in Musanze\u2026", "Open Ikigega\u2026", "Soil conservation\u2026"];
  var hi = 0;
  setInterval(function () {
    var inp = el("kaliza-input");
    if (inp && document.activeElement !== inp && !inp.value) inp.placeholder = hints[hi++ % hints.length];
  }, 2600);

  function boot() {
    if (!window.Shiny || !window.Shiny.addCustomMessageHandler) { setTimeout(boot, 200); return; }
    window.Shiny.addCustomMessageHandler("kaliza", handle);
    window.Shiny.addCustomMessageHandler("kaliza_heygen", function (msg) {
      import("https://esm.sh/@heygen/streaming-avatar@2").then(function (mod) {
        var Avatar = mod.default || mod.StreamingAvatar;
        var av = new Avatar({ token: msg.token });
        av.on(mod.StreamingEvents.STREAM_READY, function (ev) {
          var box = el("kaliza-video");
          if (!box) return;
          var v = document.createElement("video");
          v.autoplay = true; v.playsInline = true; v.srcObject = ev.detail;
          box.innerHTML = ""; box.appendChild(v);
          el("kaliza-avatar").classList.add("has-video");
          heygen = av;
        });
        return av.createStartAvatar({ quality: "low", avatarName: msg.avatar, language: "en" });
      }).catch(function () { heygen = null; });
    });
    var onAmakuru = document.body.getAttribute("data-panel") === "amakuru" || location.hash === "#amakuru";
    var panel = document.getElementById("amakuru");
    if (onAmakuru || (panel && panel.classList.contains("is-active"))) greet();
  }
  boot();
})();
