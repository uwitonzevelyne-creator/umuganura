(function () {
  var state = { cause: null, sol: null, tab: "overview" };
  var DATA = window.AP_DATA || null;
  var chartSeq = 0;

  function $(sel, root) {
    return (root || document).querySelector(sel);
  }
  function $all(sel, root) {
    return Array.prototype.slice.call((root || document).querySelectorAll(sel));
  }
  function esc(s) {
    return String(s == null ? "" : s)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }
  function fmtBn(n) {
    if (n == null || !isFinite(n)) return "—";
    return Number(n).toFixed(1) + " bn RWF";
  }
  function fmtM(n) {
    if (n == null || !isFinite(n)) return "—";
    return Number(n).toLocaleString() + " m RWF";
  }
  function fmtPct(n) {
    if (n == null || !isFinite(n)) return "—";
    return (100 * Number(n)).toFixed(0) + "%";
  }
  function libByCause(cause) {
    var lib = window.AP_LIB || [];
    for (var i = 0; i < lib.length; i++) if (lib[i].cause === cause) return lib[i];
    return null;
  }
  function findSol(cause, id) {
    var pack = libByCause(cause);
    if (!pack) return null;
    for (var i = 0; i < pack.solutions.length; i++) {
      if (pack.solutions[i].id === id) return pack.solutions[i];
    }
    return null;
  }
  function optRow(cause, id) {
    var rows = (DATA && DATA.options) || [];
    for (var i = 0; i < rows.length; i++) {
      if (rows[i].cause === cause && rows[i].id === id) return rows[i];
    }
    return null;
  }
  function planRows(cause) {
    return ((DATA && DATA.plans) || []).filter(function (r) {
      return r.cause === cause;
    });
  }
  function optsFor(cause) {
    return ((DATA && DATA.options) || []).filter(function (r) {
      return r.cause === cause;
    });
  }
  function mediaImg(cause, n) {
    var m = (window.AP_MEDIA || {})[cause] || {};
    var imgs = m.images || [];
    if (imgs[n]) return imgs[n];
    if (imgs[0]) return imgs[0];
    var pack = libByCause(cause);
    return (pack && pack.img) || "ejo/land/overview.png";
  }

  function showLanding(on) {
    var land = $("#ap-landing");
    if (land) land.classList.toggle("is-hidden", !on);
  }
  function setTab(tab, keepLanding) {
    state.tab = tab;
    $all(".ap-tab").forEach(function (b) {
      b.classList.toggle("is-on", b.getAttribute("data-ap-tab") === tab);
    });
    $all(".ap-view").forEach(function (v) {
      v.classList.toggle("is-on", v.getAttribute("data-ap-view") === tab);
    });
    if (!keepLanding) showLanding(false);
    if (window.Shiny) Shiny.setInputValue("ap_tab", tab, { priority: "event" });
    if (tab === "roadmap") refreshMap();
    setTimeout(drawPendingCharts, 80);
  }
  function setDashLayer(name) {
    ["ap-dash-causes", "ap-dash-sols"].forEach(function (id) {
      var el = document.getElementById(id);
      if (el) el.classList.toggle("is-on", id === name);
    });
  }
  function refreshMap() {
    setTimeout(function () {
      window.dispatchEvent(new Event("resize"));
      if (window.jQuery) {
        var el = document.getElementById("ap_deploy_map");
        if (el) window.jQuery(el).trigger("shown");
      }
    }, 220);
  }

  /* —— L→R header card (~6 cm) —— */
  function hdrHtml(kicker, title, body, img) {
    return (
      '<div class="ap-tab-intro-copy">' +
      '<p class="ap-tab-intro-kicker">' +
      esc(kicker) +
      "</p><h2>" +
      esc(title) +
      "</h2><p>" +
      esc(body) +
      "</p></div>" +
      '<div class="ap-tab-intro-media" style="background-image:url(\'' +
      esc(img) +
      "')\"></div>"
    );
  }

  function section(title, lead, inner) {
    return (
      '<section class="ap-scroll-block"><h2>' +
      esc(title) +
      "</h2>" +
      (lead ? '<p class="ap-scroll-lead">' + lead + "</p>" : "") +
      (inner || "") +
      "</section>"
    );
  }

  function kpiRow(items) {
    return (
      '<div class="ap-kpi-row">' +
      items
        .map(function (it) {
          return (
            "<article><span>" +
            esc(it[0]) +
            "</span><strong>" +
            esc(it[1]) +
            "</strong><p>" +
            esc(it[2] || "") +
            "</p></article>"
          );
        })
        .join("") +
      "</div>"
    );
  }

  function photoRail(cause, n, caption) {
    var m = (window.AP_MEDIA || {})[cause] || {};
    var imgs = (m.images || []).slice(0, n || 6);
    if (!imgs.length) return "";
    return (
      '<div class="ap-media-rail">' +
      imgs
        .map(function (src, i) {
          return (
            '<article class="ap-media-card"><div class="ap-media-shot" style="background-image:url(\'' +
            src +
            "')\"></div><h4>" +
            esc(caption || "Field") +
            " · " +
            (i + 1) +
            "</h4></article>"
          );
        })
        .join("") +
      "</div>"
    );
  }

  function chartBox(title, spec) {
    var id = "ap-chart-" + ++chartSeq;
    return (
      '<article class="ap-chart-card" data-ap-chart="' +
      id +
      '" data-ap-spec=\'' +
      esc(JSON.stringify(spec)) +
      "'><h4>" +
      esc(title) +
      '</h4><div class="ap-chart-host" id="' +
      id +
      '"></div></article>'
    );
  }

  function drawPendingCharts() {
    $all("[data-ap-chart]").forEach(function (card) {
      var id = card.getAttribute("data-ap-chart");
      var host = document.getElementById(id);
      if (!host || host.getAttribute("data-drawn") === "1") return;
      var raw = card.getAttribute("data-ap-spec");
      var spec;
      try {
        spec = JSON.parse(raw);
      } catch (e) {
        return;
      }
      host.setAttribute("data-drawn", "1");
      renderChart(host, spec);
    });
  }

  function renderChart(host, spec) {
    var type = spec.type || "bar";
    var x = spec.x || [];
    var y = spec.y || [];
    var y2 = spec.y2 || null;
    if (window.Plotly) {
      var traces = [
        {
          type: type === "line" ? "scatter" : "bar",
          mode: type === "line" ? "lines+markers" : undefined,
          x: x,
          y: y,
          name: spec.name || "",
          marker: { color: "#14804a" },
          line: { color: "#14804a", width: 2 }
        }
      ];
      if (y2) {
        traces.push({
          type: type === "line" ? "scatter" : "bar",
          mode: type === "line" ? "lines+markers" : undefined,
          x: x,
          y: y2,
          name: spec.name2 || "",
          marker: { color: "#0f2f4a" },
          line: { color: "#0f2f4a", width: 2 }
        });
      }
      window.Plotly.newPlot(
        host,
        traces,
        {
          margin: { t: 12, r: 12, b: 48, l: 48 },
          paper_bgcolor: "rgba(0,0,0,0)",
          plot_bgcolor: "#EBF5FB",
          font: { family: "Source Sans 3, sans-serif", size: 11, color: "#0f2f4a" },
          yaxis: { title: spec.ylab || "", gridcolor: "#c5dff0" },
          xaxis: { title: spec.xlab || "", tickangle: x.length > 6 ? -30 : 0 },
          legend: { orientation: "h" },
          height: 280
        },
        { displayModeBar: false, responsive: true }
      );
      return;
    }
    // CSS fallback bars
    var max = Math.max.apply(null, y.concat([0.0001]));
    host.innerHTML =
      '<div class="ap-bar-fallback">' +
      x
        .map(function (lab, i) {
          var h = Math.max(4, Math.round((100 * y[i]) / max));
          return (
            '<div class="ap-bar-col"><div class="ap-bar" style="height:' +
            h +
            '%"></div><span>' +
            esc(lab) +
            "</span><b>" +
            esc(String(y[i])) +
            "</b></div>"
          );
        })
        .join("") +
      "</div>";
  }

  function tinyChecks(activity) {
    var a = String(activity || "").toLowerCase();
    var base = [
      "Write the success metric and who stores the file before day one.",
      "Name a deputy lead — the primary lead will travel.",
      "Budget a 10% contingency for fuel, airtime and wet-season access."
    ];
    if (/survey|map|rank|monte/.test(a)) {
      return base.concat([
        "Fix the sample frame and replacement rule before enumerators leave.",
        "Train on consent language in Kinyarwanda and English.",
        "Pilot 5 interviews; adjust the form; then scale."
      ]);
    }
    if (/sms|app|warning|calendar/.test(a)) {
      return base.concat([
        "Opt-in / opt-out path; no night sends unless urgent.",
        "False-alarm log: who reviews and how often.",
        "Fallback if the tower is down (board + radio)."
      ]);
    }
    if (/install|build|rehabilitate|distribute|lease|shed|dryer|floor|tarpaulin|truck|cool|pack/.test(a)) {
      return base.concat([
        "Drainage, power meter, spare-parts SKU list before handover.",
        "Fee board posted in local language on day one.",
        "Operator training + signed SOP; moisture / temperature logbook.",
        "Women’s and youth access hours written into the fee schedule."
      ]);
    }
    if (/measure|review|loss|monitor/.test(a)) {
      return base.concat([
        "Baseline weigh / sample protocol locked with NISR or RAB.",
        "Calibrate meters; keep a calibration sticker with date.",
        "Publish a one-page season note — even if the result is poor."
      ]);
    }
    if (/design|site|route|model|publish/.test(a)) {
      return base.concat([
        "Document assumptions in one sheet; freeze version before build.",
        "Check land / right-of-way / buyer window before CAPEX.",
        "Peer review the OR model with a second institution."
      ]);
    }
    return base.concat(["Confirm crop fit, peak volume and buyer window before committing CAPEX."]);
  }

  /* —— Headers —— */
  function idleHeaders() {
    var img = "ejo/land/overview.png";
    var map = {
      overview: [
        "SOLUTION OVERVIEW",
        "Analytics across six loss causes",
        "Loss shares (APHLIS), option effect, capital and activity budgets from ejo_options.csv and ejo_action_plans.csv. Open Dashboard to lock a solution — then every tab fills for that pick.",
        img
      ],
      dashboard: [
        "CAUSES DASHBOARD",
        "Pick a cause, then a costed solution",
        "Cards only for navigation. Header updates with the active cause and solution. Deep steps, budgets and leads live on Roadmap.",
        "ejo/land/solutions.png"
      ],
      roadmap: [
        "IMPLEMENTATION ROADMAP",
        "Steps, budget, leads, map",
        "Select a solution to load PERT durations, step cost (m RWF), institutional leads, forgotten checks, the six-step framework and the deployment map.",
        "ejo/land/drying.png"
      ],
      performance: [
        "PERFORMANCE FRAMEWORK",
        "Effect, utilization, cost discipline",
        "Indicators and OR maths tied to the selected option’s effect, capex and opex from the options file.",
        "ejo/land/cooling.png"
      ],
      decision: [
        "DECISION INTELLIGENCE",
        "Scale, retarget or stop",
        "Rules keyed to utilization, loss movement and cost — re-open the cause OR model when constraints change.",
        "ejo/land/truck.png"
      ]
    };
    Object.keys(map).forEach(function (k) {
      var el = document.getElementById("ap-hdr-" + k);
      if (el) el.innerHTML = hdrHtml(map[k][0], map[k][1], map[k][2], map[k][3]);
    });
    renderOverviewNational();
  }

  function contextualHeaders(pack, sol, opt) {
    var hero = mediaImg(state.cause, 0);
    var tag = sol.id + " · " + sol.title;
    var lead = (opt && opt.lead) || "Lead in ejo_options.csv";
    var capex = opt ? fmtBn(opt.capex_bn) : "—";
    var effect = opt ? fmtPct(opt.effect) : "—";
    var set = function (id, kicker, title, body, img) {
      var el = document.getElementById(id);
      if (el) el.innerHTML = hdrHtml(kicker, title, body, img || hero);
    };
    set(
      "ap-hdr-overview",
      "OVERVIEW · " + sol.id,
      sol.title,
      pack.label + " · planned loss cut " + effect + " · capital " + capex + " · lead " + lead + ". Scroll for solution analytics backed by Umuganura tables.",
      hero
    );
    set(
      "ap-hdr-dashboard",
      "DASHBOARD · ACTIVE",
      pack.label + " → " + sol.title,
      "Navigate causes and solutions here. Analytics, roadmap, performance and decisions use this selection.",
      mediaImg(state.cause, 1)
    );
    set(
      "ap-hdr-roadmap",
      "ROADMAP · " + sol.id,
      "Implement " + sol.title,
      "PERT steps, step budgets, leads, forgotten checks, six-step framework and deployment map for " + pack.label + ".",
      mediaImg(state.cause, 2)
    );
    set(
      "ap-hdr-performance",
      "PERFORMANCE · " + sol.id,
      "Measure " + sol.title,
      "Effect " + effect + " · capex " + capex + " · opex " + (opt ? fmtBn(opt.opex_bn) + "/yr" : "—") + ". Indicators below stay linked to this option.",
      mediaImg(state.cause, 3)
    );
    set(
      "ap-hdr-decision",
      "DECISION · " + sol.id,
      "Scale, retarget or stop",
      "Rules for " + tag + ". Evidence note: " + ((opt && opt.basis) || sol.evidence || "see options file") + ".",
      mediaImg(state.cause, 4)
    );
  }

  /* —— Overview (national + solution) —— */
  function renderOverviewNational() {
    var sc = document.getElementById("ap-scroll-overview");
    if (!sc || state.sol) return;
    chartSeq = 0;
    var opts = (DATA && DATA.options) || [];
    var plans = (DATA && DATA.plans) || [];
    var aph = (DATA && DATA.aphlis) || [];
    var causes = ["weather", "storage", "handling", "transport", "cold", "glut"];
    var labels = causes.map(function (c) {
      var p = libByCause(c);
      return p ? p.label : c;
    });
    var capex = causes.map(function (c) {
      return opts.filter(function (o) {
        return o.cause === c;
      }).reduce(function (s, o) {
        return s + (o.capex_bn || 0);
      }, 0);
    });
    var effect = causes.map(function (c) {
      var rows = opts.filter(function (o) {
        return o.cause === c;
      });
      if (!rows.length) return 0;
      return (
        rows.reduce(function (s, o) {
          return s + (o.effect || 0);
        }, 0) / rows.length
      );
    });
    var stepCost = causes.map(function (c) {
      return plans.filter(function (p) {
        return p.cause === c;
      }).reduce(function (s, p) {
        return s + (p.cost_m || 0);
      }, 0);
    });
    var years = [];
    aph.forEach(function (r) {
      if (years.indexOf(r.year) < 0) years.push(r.year);
    });
    years.sort();
    var aphSeries = {};
    causes.forEach(function (c) {
      aphSeries[c] = years.map(function (y) {
        var hit = aph.filter(function (r) {
          return r.year === y && r.cause === c;
        })[0];
        return hit ? hit.pct : null;
      });
    });

    sc.innerHTML =
      section(
        "National snapshot",
        "Figures below come from ejo_options.csv, ejo_action_plans.csv and APHLIS value-chain % loss (mapped to causes).",
        kpiRow([
          ["Options on file", String(opts.length), "Five costed interventions per cause"],
          ["Plan activities", String(plans.length), "Seven steps × six causes"],
          ["Capital (all options)", opts.reduce(function (s, o) { return s + (o.capex_bn || 0); }, 0).toFixed(1) + " bn", "Sum of capex_bn"],
          ["Step budgets", plans.reduce(function (s, p) { return s + (p.cost_m || 0); }, 0).toLocaleString() + " m", "Sum of cost_m"]
        ])
      ) +
      '<div class="ap-chart-grid">' +
      chartBox("Capital by cause (bn RWF)", { type: "bar", x: labels, y: capex.map(function (v) { return +v.toFixed(1); }), ylab: "bn RWF" }) +
      chartBox("Mean option effect by cause", { type: "bar", x: labels, y: effect.map(function (v) { return +(100 * v).toFixed(1); }), ylab: "% loss cut" }) +
      chartBox("Implementation step cost by cause (m RWF)", { type: "bar", x: labels, y: stepCost, ylab: "m RWF" }) +
      chartBox("APHLIS % loss — Harvesting/field drying (weather)", {
        type: "line",
        x: years,
        y: aphSeries.weather,
        ylab: "% dry-weight loss"
      }) +
      chartBox("APHLIS % loss — Storage steps", {
        type: "line",
        x: years,
        y: aphSeries.storage,
        ylab: "% dry-weight loss"
      }) +
      chartBox("APHLIS % loss — Transport steps", {
        type: "line",
        x: years,
        y: aphSeries.transport,
        ylab: "% dry-weight loss"
      }) +
      "</div>" +
      section(
        "How to use this Action plan",
        "Dashboard picks the cause and solution. Overview fills with that option’s analytics. Roadmap carries deep steps, budgets, leads, the deployment map and the six-step institutional framework.",
        '<p class="ap-scroll-cta"><button type="button" class="ap-btn ap-btn-primary" data-ap-tab="dashboard">Open causes dashboard →</button></p>'
      );
    setTimeout(drawPendingCharts, 60);
  }

  function renderOverviewSolution(pack, sol, opt) {
    var sc = document.getElementById("ap-scroll-overview");
    if (!sc) return;
    chartSeq = 0;
    var peers = optsFor(state.cause);
    var plans = planRows(state.cause);
    var aph = ((DATA && DATA.aphlis) || []).filter(function (r) {
      return r.cause === state.cause;
    });
    var years = [];
    aph.forEach(function (r) {
      if (years.indexOf(r.year) < 0) years.push(r.year);
    });
    years.sort();
    var aphY = years.map(function (y) {
      var hit = aph.filter(function (r) {
        return r.year === y;
      })[0];
      return hit ? hit.pct : null;
    });

    sc.innerHTML =
      section(
        "Solution at a glance",
        esc(sol.why || ""),
        kpiRow([
          ["Effect", opt ? fmtPct(opt.effect) : "—", "From ejo_options.csv"],
          ["Capital", opt ? fmtBn(opt.capex_bn) : "—", "capex_bn"],
          ["Running", opt ? fmtBn(opt.opex_bn) + "/yr" : "—", "opex_bn"],
          ["Lead", (opt && opt.lead) || "—", "Institutional lead"]
        ])
      ) +
      section(
        "Problem → intervention → outcome",
        "",
        '<div class="ap-or-strip"><article><h3>Problem</h3><p>' +
          esc(sol.problem) +
          "</p></article><article><h3>Intervention</h3><p>" +
          esc(sol.intervention) +
          "</p></article><article><h3>Outcome</h3><p>" +
          esc(sol.outcome) +
          "</p></article></div>"
      ) +
      '<div class="ap-chart-grid">' +
      chartBox("Peer options — effect (%)", {
        type: "bar",
        x: peers.map(function (o) { return o.id; }),
        y: peers.map(function (o) { return +(100 * (o.effect || 0)).toFixed(1); }),
        ylab: "%"
      }) +
      chartBox("Peer options — capital (bn RWF)", {
        type: "bar",
        x: peers.map(function (o) { return o.id; }),
        y: peers.map(function (o) { return o.capex_bn || 0; }),
        ylab: "bn"
      }) +
      chartBox("Peer options — opex (bn RWF / yr)", {
        type: "bar",
        x: peers.map(function (o) { return o.id; }),
        y: peers.map(function (o) { return o.opex_bn || 0; }),
        ylab: "bn/yr"
      }) +
      chartBox("Cause plan — step cost (m RWF)", {
        type: "bar",
        x: plans.map(function (p) { return p.id; }),
        y: plans.map(function (p) { return p.cost_m || 0; }),
        ylab: "m RWF"
      }) +
      chartBox("Cause plan — PERT months", {
        type: "bar",
        x: plans.map(function (p) { return p.id; }),
        y: plans.map(function (p) { return p.pert_m || 0; }),
        ylab: "months"
      }) +
      (years.length
        ? chartBox("APHLIS % loss for this cause", { type: "line", x: years, y: aphY, ylab: "%" })
        : "") +
      "</div>" +
      section("Field context", "Cause-folder photos — no videos.", photoRail(state.cause, 8, pack.label)) +
      section(
        "Evidence on file",
        esc((opt && opt.basis) || sol.evidence || ""),
        '<p class="ap-scroll-cta"><button type="button" class="ap-btn ap-btn-primary" data-ap-tab="roadmap">Open implementation roadmap →</button></p>'
      );
    setTimeout(drawPendingCharts, 60);
  }

  /* —— Roadmap deep steps + map —— */
  function renderRoadmap(pack, sol, opt) {
    var sc = document.getElementById("ap-scroll-roadmap");
    var tail = document.getElementById("ap-scroll-roadmap-tail");
    if (!sc) return;
    chartSeq = 0;
    var plans = planRows(state.cause);
    var fw = (DATA && DATA.framework) || [];
    var budget = (DATA && DATA.budgets && DATA.budgets[state.cause]) || null;
    var totalStep = plans.reduce(function (s, p) {
      return s + (p.cost_m || 0);
    }, 0);

    var stepsHtml = plans
      .map(function (p, idx) {
        var checks = tinyChecks(p.activity)
          .map(function (c) {
            return "<li>" + esc(c) + "</li>";
          })
          .join("");
        return (
          '<article class="ap-deep-step">' +
          '<div class="ap-deep-step-top"><span class="ap-step-id">Step ' +
          esc(p.id) +
          '</span><span class="ap-step-meta">PERT ' +
          (p.pert_m != null ? p.pert_m.toFixed(1) : "—") +
          " mo · " +
          fmtM(p.cost_m) +
          "</span></div>" +
          "<h3>" +
          esc(p.activity) +
          "</h3>" +
          '<p class="ap-step-lead"><strong>Lead:</strong> ' +
          esc(p.lead || "—") +
          (p.after ? " · <strong>After:</strong> " + esc(p.after) : "") +
          "</p>" +
          '<p class="ap-step-pert">Optimistic ' +
          esc(p.opt) +
          " · Likely " +
          esc(p.likely) +
          " · Pessimistic " +
          esc(p.pess) +
          " months</p>" +
          '<h4>Tiny considerations people forget</h4><ul class="ap-tiny">' +
          checks +
          "</ul>" +
          (idx % 2 === 0 ? photoRail(state.cause, 2, "Step visual") : "") +
          "</article>"
        );
      })
      .join("");

    sc.innerHTML =
      section(
        "Delivery spine for " + pack.label,
        "Activities from ejo_action_plans.csv. Option " +
          esc(sol.id) +
          " (" +
          esc(sol.title) +
          ") sits inside this cause package. Cause capital budget band: " +
          (budget != null ? budget + " bn RWF" : "—") +
          ".",
        kpiRow([
          ["Steps", String(plans.length), "Cause plan activities"],
          ["Step budget sum", totalStep.toLocaleString() + " m", "cost_m total"],
          ["Option capital", opt ? fmtBn(opt.capex_bn) : "—", sol.id],
          ["Option lead", (opt && opt.lead) || "—", "ejo_options.csv"]
        ])
      ) +
      '<div class="ap-chart-grid">' +
      chartBox("Step cost (m RWF)", {
        type: "bar",
        x: plans.map(function (p) { return p.id; }),
        y: plans.map(function (p) { return p.cost_m || 0; }),
        ylab: "m RWF"
      }) +
      chartBox("PERT duration (months)", {
        type: "bar",
        x: plans.map(function (p) { return p.id; }),
        y: plans.map(function (p) { return p.pert_m || 0; }),
        ylab: "months"
      }) +
      "</div>" +
      section("Deep implementation steps", "Every activity with budget, lead, PERT and forgotten checks.", stepsHtml) +
      section(
        "Six-step institutional framework",
        "Same framework used on the Action desk — Alignment through Co-implementation.",
        '<div class="ap-fw-row">' +
          fw
            .map(function (f) {
              return (
                "<article><span>Step " +
                esc(f.id) +
                "</span><strong>" +
                esc(f.title) +
                "</strong><p>Gate for funding clearance and partner reports on " +
                esc(sol.id) +
                ".</p></article>"
              );
            })
            .join("") +
          "</div>"
      ) +
      section(
        "Deployment map",
        "Districts marked for Action plan geography. Scroll to the map below, then the site checklist.",
        ""
      );

    if (tail) {
      tail.innerHTML =
        section(
          "Site & roll-out checklist",
          "After the map — land the intervention without skipping the boring parts.",
          '<div class="ap-map-guides">' +
            "<article class=\"ap-map-guide\"><h3>Site screen</h3><p>Volumes, access, power/water, flood risk, buyer window — match: " +
            esc(sol.success) +
            ".</p></article>" +
            "<article class=\"ap-map-guide\"><h3>Lead & partners</h3><p>" +
            esc((opt && opt.lead) || pack.label) +
            " owns the option; district and coop deputies named in each step above.</p></article>" +
            "<article class=\"ap-map-guide\"><h3>Pilot footprint</h3><p>Start on one high-loss corridor; measure a season before expanding markers.</p></article>" +
            "<article class=\"ap-map-guide\"><h3>Feedback to OR</h3><p>" +
            esc(pack.or || "Re-run the cause OR model") +
            " when utilization or arrivals change.</p></article></div>"
        ) +
        section("Briefing photos", "", photoRail(state.cause, 6, pack.label));
    }
    setTimeout(drawPendingCharts, 60);
  }

  function renderPerformance(pack, sol, opt) {
    var sc = document.getElementById("ap-scroll-performance");
    if (!sc) return;
    chartSeq = 0;
    var peers = optsFor(state.cause);
    var effect = opt ? opt.effect : 0.2;
    var seasonT = 1200;
    var days = 40;
    var daily = seasonT / days;
    var util = 0.72;
    var capacity = daily / Math.max(util, 0.01);
    var reduced = 100 * (1 - effect);

    sc.innerHTML =
      section(
        "OR planning maths",
        "Labelled assumptions — not invented district targets. Effect from ejo_options.csv.",
        '<div class="ap-or-math">' +
          '<article class="ap-math-card"><h4>Required daily capacity</h4><p class="ap-math-f">C<sub>day</sub> = V<sub>season</sub> / D<sub>ops</sub></p><p>Example V=' +
          seasonT +
          " t, D=" +
          days +
          " → <strong>" +
          daily.toFixed(1) +
          " t/day</strong>.</p></article>" +
          '<article class="ap-math-card"><h4>Loss reduction</h4><p class="ap-math-f">L<sub>after</sub> = L<sub>base</sub> · (1 − e)</p><p>e = ' +
          fmtPct(effect) +
          " for " +
          esc(sol.id) +
          " → index <strong>" +
          reduced.toFixed(1) +
          "</strong>.</p></article>" +
          '<article class="ap-math-card"><h4>Utilization</h4><p class="ap-math-f">ρ = λ / (c · μ)</p><p>At ' +
          (util * 100).toFixed(0) +
          "% illustrative utilization, service need ≈ <strong>" +
          capacity.toFixed(1) +
          " t/day</strong>.</p></article>" +
          '<article class="ap-math-card"><h4>Cost band</h4><p>Capex <strong>' +
          (opt ? fmtBn(opt.capex_bn) : "—") +
          "</strong> · Opex <strong>" +
          (opt ? fmtBn(opt.opex_bn) + "/yr" : "—") +
          "</strong>.</p></article></div>"
      ) +
      '<div class="ap-chart-grid">' +
      chartBox("Cause options — effect vs capital", {
        type: "bar",
        x: peers.map(function (o) { return o.id; }),
        y: peers.map(function (o) { return +(100 * (o.effect || 0)).toFixed(1); }),
        y2: peers.map(function (o) { return o.capex_bn || 0; }),
        name: "Effect %",
        name2: "Capex bn"
      }) +
      chartBox("Cause options — opex", {
        type: "bar",
        x: peers.map(function (o) { return o.id; }),
        y: peers.map(function (o) { return o.opex_bn || 0; }),
        ylab: "bn/yr"
      }) +
      "</div>" +
      section(
        "Indicator set",
        "Measure what this intervention is meant to change.",
        [
          ["Physical loss rate", "Lost quantity ÷ initial quantity. Unit %. Source: weighbridge / sample. Frequency: season."],
          ["Cost per kg handled", "Relevant opex ÷ kg. Unit RWF/kg. Baseline first."],
          ["Utilization / coverage", "Throughput or users ÷ available capacity."],
          ["Quality / reject rate", "Rejected ÷ handled. Link to buyer specs."],
          ["Time-to-service", "Hours from harvest to protected stage."],
          ["Outcome check", sol.outcome],
          ["Success conditions", sol.success],
          ["Evidence", (opt && opt.basis) || sol.evidence]
        ]
          .map(function (r) {
            return '<article class="ap-ind"><b>' + esc(r[0]) + "</b><p>" + esc(r[1]) + "</p></article>";
          })
          .join("")
      ) +
      section("Context photos", "", photoRail(state.cause, 6, sol.title));
    setTimeout(drawPendingCharts, 60);
  }

  function renderDecision(pack, sol, opt) {
    var sc = document.getElementById("ap-scroll-decision");
    if (!sc) return;
    chartSeq = 0;
    var peers = optsFor(state.cause);
    var rules = [
      ["Losses fall and utilization is high", "Test more capacity or coverage; re-run the cause OR model."],
      ["Facility or package underused", "Check access, fees, hours, crop fit and awareness before building more."],
      ["Losses stay high", "Revisit moisture, pests, handling, routing or SOPs in the roadmap steps."],
      ["Costs exceed benefits", "Reassess scale, location, mix — or stop the pilot; update the evidence note."],
      ["Weather / peak shock", "Tighten advisories and scheduling before adding hardware."],
      ["Data missing", "Store the required dataset before setting numeric targets."],
      ["Success conditions met", sol.success],
      ["Why this intervention", sol.why]
    ];
    sc.innerHTML =
      section(
        "Decision rules for " + sol.id,
        "Option lead: " + esc((opt && opt.lead) || "—") + ". Evidence: " + esc((opt && opt.basis) || sol.evidence || ""),
        rules
          .map(function (r) {
            return '<article class="ap-rule"><b>' + esc(r[0]) + "</b><p>" + esc(r[1]) + "</p></article>";
          })
          .join("")
      ) +
      '<div class="ap-chart-grid">' +
      chartBox("Compare effect (%) across peers", {
        type: "bar",
        x: peers.map(function (o) { return o.id; }),
        y: peers.map(function (o) { return +(100 * (o.effect || 0)).toFixed(1); }),
        ylab: "%"
      }) +
      chartBox("Compare capital (bn)", {
        type: "bar",
        x: peers.map(function (o) { return o.id; }),
        y: peers.map(function (o) { return o.capex_bn || 0; }),
        ylab: "bn"
      }) +
      "</div>" +
      section("Scenes", "", photoRail(state.cause, 6, sol.title));
    setTimeout(drawPendingCharts, 60);
  }

  function renderAll() {
    if (!state.cause || !state.sol) {
      idleHeaders();
      return;
    }
    var pack = libByCause(state.cause);
    var sol = findSol(state.cause, state.sol);
    if (!pack || !sol) return;
    var opt = optRow(state.cause, state.sol);
    contextualHeaders(pack, sol, opt);
    renderOverviewSolution(pack, sol, opt);
    renderRoadmap(pack, sol, opt);
    renderPerformance(pack, sol, opt);
    renderDecision(pack, sol, opt);
  }

  function renderSolutions(cause) {
    var pack = libByCause(cause);
    if (!pack) return;
    state.cause = cause;
    state.sol = null;
    var title = $("#ap-sol-cause-title");
    if (title) title.textContent = pack.label + " · solutions";
    var grid = $("#ap-sol-grid");
    if (!grid) return;
    var hdr = document.getElementById("ap-hdr-dashboard");
    if (hdr) {
      hdr.innerHTML = hdrHtml(
        "DASHBOARD · " + pack.label.toUpperCase(),
        pack.label + " — choose a solution",
        (pack.blurb || "") + " Options below carry capex, opex, effect and lead from ejo_options.csv.",
        mediaImg(cause, 0)
      );
    }
    grid.innerHTML = pack.solutions
      .map(function (sol, idx) {
        var opt = optRow(cause, sol.id);
        var shot = mediaImg(cause, idx);
        return (
          '<button type="button" class="ap-sol-card' +
          (sol.primary ? " is-primary" : "") +
          '" data-ap-sol="' +
          esc(sol.id) +
          '" data-ap-cause="' +
          esc(cause) +
          '">' +
          '<div class="ap-sol-card-photo" style="background-image:url(\'' +
          shot +
          "')\"></div>" +
          '<div class="ap-sol-card-pad">' +
          '<div class="ap-sol-card-top"><span class="ap-sol-kicker">' +
          (sol.primary ? "Primary" : "Supporting") +
          '</span><span class="ap-sol-id">' +
          esc(sol.id) +
          "</span></div>" +
          "<h3>" +
          esc(sol.title) +
          "</h3>" +
          '<p class="ap-sol-line"><strong>Problem: </strong>' +
          esc(sol.problem) +
          "</p>" +
          '<p class="ap-sol-line"><strong>Intervention: </strong>' +
          esc(sol.intervention) +
          "</p>" +
          '<div class="ap-sol-meta"><span>Effect · ' +
          (opt ? fmtPct(opt.effect) : esc(sol.complexity)) +
          "</span><span>Capex · " +
          (opt ? fmtBn(opt.capex_bn) : esc(sol.cost)) +
          "</span><span>Lead · " +
          esc((opt && opt.lead) || "—") +
          "</span></div>" +
          '<span class="ap-sol-cta">Open analytics &amp; roadmap →</span></div></button>'
        );
      })
      .join("");
    setDashLayer("ap-dash-sols");
    setTab("dashboard", false);
    if (window.Shiny) Shiny.setInputValue("ap_cause", cause, { priority: "event" });
  }

  function selectSolution(cause, solId) {
    var pack = libByCause(cause);
    var sol = findSol(cause, solId);
    if (!pack || !sol) return;
    var ov = document.getElementById("ap-scroll-overview");
    if (window.UmuganuraLoading) UmuganuraLoading.show("Loading", "Building solution analytics…", ov);
    state.cause = cause;
    state.sol = solId;
    renderAll();
    showLanding(false);
    setTab("overview", false);
    window.scrollTo({ top: 0, behavior: "smooth" });
    if (window.Shiny) {
      Shiny.setInputValue("ap_cause", cause, { priority: "event" });
      Shiny.setInputValue("ap_solution", solId, { priority: "event" });
    }
    setTimeout(function () {
      if (window.UmuganuraLoading) UmuganuraLoading.hide(ov);
    }, 450);
  }

  function startHero() {
    var slides = $all(".ap-hero-slide");
    if (slides.length < 2) return;
    var i = 0;
    setInterval(function () {
      slides[i].classList.remove("is-on");
      i = (i + 1) % slides.length;
      slides[i].classList.add("is-on");
    }, 4500);
  }

  function dataReady() {
    return !!(DATA && DATA.options && DATA.options.length);
  }

  function loadAssets(done) {
    var left = 2;
    function tick() {
      left -= 1;
      if (left <= 0 && done) done();
    }
    if (dataReady()) tick();
    else {
      fetch("ejo/action/ap_data.json?v=3")
        .then(function (r) {
          return r.ok ? r.json() : null;
        })
        .then(function (d) {
          DATA = d || DATA || {};
          window.AP_DATA = DATA;
          tick();
        })
        .catch(function () {
          if (!DATA) DATA = {};
          tick();
        });
    }
    if (window.AP_MEDIA && Object.keys(window.AP_MEDIA).length) tick();
    else {
      fetch("ejo/action/media_index.json?v=3")
        .then(function (r) {
          return r.ok ? r.json() : {};
        })
        .then(function (d) {
          window.AP_MEDIA = d || {};
          tick();
        })
        .catch(function () {
          window.AP_MEDIA = window.AP_MEDIA || {};
          tick();
        });
    }
  }

  function bind() {
    var root = $("#ejo-solution");
    if (!root || root.getAttribute("data-ap-bound") === "1") return;
    root.setAttribute("data-ap-bound", "1");

    document.addEventListener("click", function (e) {
      var tabBtn = e.target.closest("[data-ap-tab]");
      if (tabBtn && root.contains(tabBtn)) {
        e.preventDefault();
        var tab = tabBtn.getAttribute("data-ap-tab");
        if (tab === "overview" && !state.sol) {
          showLanding(true);
          setTab("overview", true);
          window.scrollTo({ top: 0, behavior: "smooth" });
        } else if (tab === "dashboard") {
          showLanding(false);
          if (!state.cause) setDashLayer("ap-dash-causes");
          else if (!state.sol) renderSolutions(state.cause);
          else setDashLayer("ap-dash-sols");
          setTab("dashboard");
        } else {
          showLanding(false);
          setTab(tab);
        }
        return;
      }

      var openCause = e.target.closest("[data-ap-open-cause]");
      if (openCause && root.contains(openCause)) {
        e.preventDefault();
        renderSolutions(openCause.getAttribute("data-ap-open-cause"));
        return;
      }

      var solBtn = e.target.closest("button.ap-sol-card[data-ap-sol]");
      if (solBtn && root.contains(solBtn)) {
        e.preventDefault();
        selectSolution(solBtn.getAttribute("data-ap-cause"), solBtn.getAttribute("data-ap-sol"));
        return;
      }

      var back = e.target.closest("[data-ap-back]");
      if (back && root.contains(back)) {
        e.preventDefault();
        if (back.getAttribute("data-ap-back") === "causes") {
          state.sol = null;
          state.cause = null;
          setDashLayer("ap-dash-causes");
          idleHeaders();
        }
        return;
      }

      if (e.target.closest("#ap-exit") || e.target.closest("[data-ap-home]")) {
        e.preventDefault();
        if (window.ejoBackToRoom) window.ejoBackToRoom();
        else if (window.backToEjoRoom) window.backToEjoRoom();
        else {
          document.querySelectorAll(".panel").forEach(function (p) {
            p.classList.toggle("is-active", p.id === "ejo-hazaza");
          });
          document.body.setAttribute("data-panel", "ejo-hazaza");
        }
      }
    });

    startHero();
  }

  function openActionPlan() {
    if (window.UmuganuraLoading) UmuganuraLoading.show("Loading", "Opening Action plan…", document.getElementById("ejo-solution"));
    document.querySelectorAll(".panel").forEach(function (p) {
      p.classList.toggle("is-active", p.id === "ejo-solution");
    });
    document.querySelectorAll("[data-panel]").forEach(function (a) {
      a.classList.toggle("is-active", a.getAttribute("data-panel") === "ejo-hazaza");
    });
    document.body.setAttribute("data-panel", "ejo-solution");
    window.scrollTo(0, 0);
    bind();
    showLanding(true);
    setDashLayer("ap-dash-causes");
    loadAssets(function () {
      idleHeaders();
      if (state.cause && state.sol) renderAll();
      if (window.UmuganuraLoading) UmuganuraLoading.hide();
    });
    $all(".ap-tab").forEach(function (b) {
      b.classList.toggle("is-on", b.getAttribute("data-ap-tab") === "overview");
    });
    $all(".ap-view").forEach(function (v) {
      v.classList.toggle("is-on", v.getAttribute("data-ap-view") === "overview");
    });
    if (window.jQuery) window.jQuery("#ejo-solution").trigger("shown");
    window.dispatchEvent(new Event("resize"));
  }

  window.ejoOpenActionPlan = openActionPlan;
  window.ejoOpenPortalLogin = openActionPlan;

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () {
      bind();
      loadAssets(idleHeaders);
    });
  } else {
    bind();
    loadAssets(idleHeaders);
  }
})();
