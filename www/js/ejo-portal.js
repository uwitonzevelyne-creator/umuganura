(function () {
  var i18n = {
    en: {
      brand_sub: "Ejo Hazaza · Government delivery desk",
      login: "Login",
      login_hint: "Please enter your credentials!",
      workplace: "Where you work",
      username: "Username",
      password: "Password",
      creds_title: "Demo credentials — copy into the boxes",
      creds_pass: "Password for every account:",
      fill: "Fill",
      powered: "Powered by Umuganura · Republic of Rwanda",
      nav_home: "Dashboard",
      nav_inbox: "Solution inbox",
      nav_pms: "Tasks & schedule",
      nav_desk: "Open solution desk",
      nav_alerts: "Alerts",
      nav_back: "← Back to Ejo Hazaza",
      dash_title: "Delivery cockpit",
      dash_sub: "Solutions led by your institution, schedule position, and clearance status.",
      inbox_title: "Solution inbox",
      pms_title: "Tasks & schedule",
      desk_title: "Solution desk",
      desk_sub: "Funding, six-step framework, milestones, and partner reports for the opened solution.",
      alerts_title: "Alerts",
      kpi_sols: "Solutions on you",
      kpi_role: "Role",
      kpi_pass: "Shared password",
      kpi_demo: "Demo desk only",
      no_sols: "No solutions currently name your institution as lead or partner.",
      col_sol: "Solution",
      col_cause: "Cause",
      col_lead: "Lead",
      col_capex: "Capital",
      open: "Open desk"
    },
    fr: {
      brand_sub: "Ejo Hazaza · Bureau de livraison gouvernemental",
      login: "Connexion",
      login_hint: "Veuillez saisir vos identifiants !",
      workplace: "Où vous travaillez",
      username: "Nom d'utilisateur",
      password: "Mot de passe",
      creds_title: "Identifiants de démonstration — copiez-les",
      creds_pass: "Mot de passe pour chaque compte :",
      fill: "Remplir",
      powered: "Propulsé par Umuganura · République du Rwanda",
      nav_home: "Tableau de bord",
      nav_inbox: "Boîte solutions",
      nav_pms: "Tâches & calendrier",
      nav_desk: "Bureau solution",
      nav_alerts: "Alertes",
      nav_back: "← Retour à Ejo Hazaza",
      dash_title: "Cockpit de livraison",
      dash_sub: "Solutions de votre institution, calendrier et statut de clearance.",
      inbox_title: "Boîte solutions",
      pms_title: "Tâches & calendrier",
      desk_title: "Bureau solution",
      desk_sub: "Financement, cadre en six étapes, jalons et rapports partenaires.",
      alerts_title: "Alertes",
      kpi_sols: "Solutions vous concernant",
      kpi_role: "Rôle",
      kpi_pass: "Mot de passe partagé",
      kpi_demo: "Bureau démo uniquement",
      no_sols: "Aucune solution ne nomme encore votre institution.",
      col_sol: "Solution",
      col_cause: "Cause",
      col_lead: "Chef de file",
      col_capex: "Capital",
      open: "Ouvrir"
    },
    rw: {
      brand_sub: "Ejo Hazaza · Ibiro by'ubuyobozi",
      login: "Injira",
      login_hint: "Andika izina n'ijambo ry'ibanga!",
      workplace: "Aho ukorera",
      username: "Izina ry'ukoresha",
      password: "Ijambo ry'ibanga",
      creds_title: "Amakuru yo kugerageza — kopi",
      creds_pass: "Ijambo ry'ibanga kuri konti zose:",
      fill: "Uzuza",
      powered: "Bikozwe na Umuganura · Repubulika y'u Rwanda",
      nav_home: "Ikibaho",
      nav_inbox: "Ibaruwa z'ibisubizo",
      nav_pms: "Imirimo n'igenekerezo",
      nav_desk: "Ibiro by'igisubizo",
      nav_alerts: "Imenyesha",
      nav_back: "← Subira kuri Ejo Hazaza",
      dash_title: "Ikibaho cyo gutanga",
      dash_sub: "Ibisubizo by'ikigo cyawe, igenekerezo n'ukwemera.",
      inbox_title: "Ibaruwa z'ibisubizo",
      pms_title: "Imirimo n'igenekerezo",
      desk_title: "Ibiro by'igisubizo",
      desk_sub: "Inkunga, intangiriro esheshatu, n' raporo.",
      alerts_title: "Imenyesha",
      kpi_sols: "Ibisubizo kuri wowe",
      kpi_role: "Uruhare",
      kpi_pass: "Ijambo ry'ibanga",
      kpi_demo: "Gerageza gusa",
      no_sols: "Nta gisubizo gikubiyemo ikigo cyawe.",
      col_sol: "Igisubizo",
      col_cause: "Impamvu",
      col_lead: "Uyoboye",
      col_capex: "Imari",
      open: "Fungura"
    }
  };

  function applyLang(lang) {
    var dict = i18n[lang] || i18n.en;
    document.querySelectorAll("[data-i18n]").forEach(function (el) {
      var key = el.getAttribute("data-i18n");
      if (dict[key]) el.textContent = dict[key];
    });
    document.querySelectorAll(".gsas-lang-btn").forEach(function (b) {
      b.classList.toggle("is-on", b.getAttribute("data-lang") === lang);
    });
    try { localStorage.setItem("umuganura_gsas_lang", lang); } catch (e) {}
  }

  function setAuthed(on) {
    var login = document.getElementById("gsas-login");
    var app = document.getElementById("gsas-app");
    if (login) login.classList.toggle("is-hidden", !!on);
    if (app) app.classList.toggle("is-hidden", !on);
  }

  function showView(id) {
    document.querySelectorAll(".gsas-view").forEach(function (v) {
      v.classList.toggle("is-on", v.id === "gsas-view-" + id);
    });
    document.querySelectorAll(".gsas-nav").forEach(function (n) {
      n.classList.toggle("is-on", n.getAttribute("data-gsas-view") === id);
    });
  }

  window.gsasSetAuthed = setAuthed;
  window.gsasShowView = showView;
  window.gsasApplyLang = applyLang;

  document.addEventListener("click", function (e) {
    var lang = e.target.closest(".gsas-lang-btn");
    if (lang) {
      applyLang(lang.getAttribute("data-lang") || "en");
      return;
    }
    var fill = e.target.closest(".gsas-fill");
    if (fill) {
      var user = fill.getAttribute("data-user") || "";
      var pass = fill.getAttribute("data-pass") || "123456";
      var inst = fill.getAttribute("data-inst") || "";
      var u = document.getElementById("gsas_user");
      var p = document.querySelector("#gsas_pass");
      if (u) {
        u.value = user;
        u.dispatchEvent(new Event("input", { bubbles: true }));
        if (window.jQuery) window.jQuery(u).trigger("change");
      }
      if (p) {
        p.value = pass;
        p.dispatchEvent(new Event("input", { bubbles: true }));
        if (window.jQuery) window.jQuery(p).trigger("change");
      }
      if (window.Shiny) {
        Shiny.setInputValue("gsas_user", user, { priority: "event" });
        Shiny.setInputValue("gsas_pass", pass, { priority: "event" });
        if (inst) Shiny.setInputValue("gsas_inst", inst, { priority: "event" });
      }
      var sel = document.querySelector("#gsas_inst");
      if (sel && inst && window.jQuery) {
        var $s = window.jQuery(sel);
        if ($s[0] && $s[0].selectize) $s[0].selectize.setValue(inst, true);
        else $s.val(inst).trigger("change");
      }
      return;
    }
    var eye = e.target.closest("#gsas-eye");
    if (eye) {
      var inp = document.querySelector("#gsas_pass");
      if (!inp) return;
      var show = inp.type === "password";
      inp.type = show ? "text" : "password";
      eye.textContent = show ? "Hide" : "Show";
      return;
    }
    var nav = e.target.closest(".gsas-nav");
    if (nav) {
      showView(nav.getAttribute("data-gsas-view") || "home");
      return;
    }
    var openSol = e.target.closest(".gsas-open-sol");
    if (openSol) {
      var scope = openSol.getAttribute("data-scope");
      var id = openSol.getAttribute("data-act");
      if (window.Shiny) {
        Shiny.setInputValue("ej_act", { scope: scope, id: id, nonce: Date.now() }, { priority: "event" });
      }
      showView("desk");
      return;
    }
    var copy = e.target.closest(".gsas-copy-prev");
    if (copy) {
      var pre = copy.parentElement.querySelector(".gsas-lock");
      if (pre && navigator.clipboard) navigator.clipboard.writeText(pre.innerText);
      copy.textContent = "Copied";
      return;
    }
    var deep = e.target.closest(".gsas-deepseek");
    if (deep) {
      postDeep(deep.getAttribute("data-text") || "", deep.getAttribute("data-mode") || "summary", deep.getAttribute("data-id"), "gsas-deepseek-out");
      return;
    }
    if (e.target.closest(".gsas-brief")) {
      var brief = e.target.closest(".gsas-brief");
      var notes = [].map.call(document.querySelectorAll(".gsas-task, .gsas-lock"), function (n) { return n.innerText; }).slice(0, 8).join("\n\n");
      var slot = brief.parentElement.querySelector(".gsas-brief-slot");
      postDeep(notes, "brief", null, slot ? slot.id : "gsas-brief-out");
      return;
    }
    if (e.target.closest("#gsas-bell")) {
      showView("alerts");
      return;
    }
    if (e.target.closest("#gsas-field-go")) {
      var bits = [
        ["Crop status", "gsas_f_crop"],
        ["Rot or bruising", "gsas_f_rot"],
        ["Transport delays", "gsas_f_transport"],
        ["Weather", "gsas_f_weather"]
      ].map(function (pair) {
        var el = document.getElementById(pair[1]);
        return pair[0] + ": " + (el && el.value ? el.value.trim() : "—");
      });
      var local = "Locked field diagnostic\n" + bits.join("\n");
      var box = document.getElementById("gsas-field-out");
      if (box) box.textContent = local;
      var mail = document.getElementById("gsas-field-mail");
      if (mail) mail.href = "https://mail.google.com/mail/?view=cm&fs=1&su=" + encodeURIComponent("Field diagnostic") + "&body=" + encodeURIComponent(local);
      postDeep(local, "summary", null, "gsas-field-out");
      return;
    }
    if (e.target.closest("#gsas-field-copy")) {
      var report = document.getElementById("gsas-field-out");
      var note = document.getElementById("gsas-field-note");
      if (report && navigator.clipboard) navigator.clipboard.writeText(report.innerText || "");
      if (note) note.textContent = "Report copied! Open our chatwoot and user will see the message to send to your project team.";
      return;
    }
    var mail = e.target.closest(".gsas-mail-link");
    if (mail) {
      var to = document.getElementById("gsas_mail_to");
      var body = document.getElementById("gsas_mail_body");
      mail.setAttribute("href", "mailto:?subject=" + encodeURIComponent("Umuganura note for " + (to ? to.value : "")) + "&body=" + encodeURIComponent(body ? body.value : ""));
    }
  });

  if (window.Shiny) {
    Shiny.addCustomMessageHandler("gsas_auth", function (msg) {
      setAuthed(!!(msg && msg.ok));
      if (!msg || !msg.ok) {
        document.body.removeAttribute("data-tier");
        if (typeof window.ejoOpenLand === "function") window.ejoOpenLand();
        else if (window.jQuery) {
          document.querySelectorAll(".panel").forEach(function (p) {
            p.classList.toggle("is-active", p.id === "ejo-hazaza");
          });
          document.body.setAttribute("data-panel", "ejo-hazaza");
        }
        return;
      }
      if (msg && msg.ok) {
        document.body.setAttribute("data-tier", msg.tier || "base");
        var agency = document.getElementById("gsas-agency");
        var role = document.getElementById("gsas-role-tag");
        var disp = document.getElementById("gsas-display");
        var av = document.getElementById("gsas-avatar");
        if (agency) agency.textContent = msg.institution || "";
        if (role) role.textContent = msg.role || "";
        if (disp) disp.textContent = msg.display_name || msg.username || "";
        if (av) av.textContent = ((msg.display_name || msg.username || "U").charAt(0) || "U").toUpperCase();
        showView(msg.view || "home");
      }
    });
    Shiny.addCustomMessageHandler("gsas_badge", function (msg) {
      var n = msg && msg.n ? Number(msg.n) : 0;
      ["alerts", "home"].forEach(function (view) {
        var btn = document.querySelector('.gsas-nav[data-gsas-view="' + view + '"]');
        if (!btn) return;
        var b = btn.querySelector(".gsas-badge");
        if (!b) { b = document.createElement("span"); b.className = "gsas-badge"; btn.appendChild(b); }
        b.textContent = n > 0 ? String(n) : "";
        b.style.display = n > 0 ? "inline-block" : "none";
      });
      var hint = document.getElementById("gsas-tier-hint");
      if (hint && msg && msg.hint) hint.textContent = msg.hint;
      var nav = document.querySelector('a[data-panel="ejo-hazaza"]');
      if (nav) nav.classList.toggle("has-badge", n > 0);
      var bell = document.getElementById("gsas-bell-n");
      if (bell) {
        bell.textContent = n > 0 ? String(n) : "";
        bell.style.display = n > 0 ? "inline-block" : "none";
      }
    });
  }

  function postDeep(text, mode, id, outId) {
    var out = document.getElementById(outId);
    var prior = out ? out.textContent : "";
    if (out) out.textContent = "Reading the figures…";
    fetch("http://127.0.0.1:7452/api/deepseek", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ question: text, mode: mode, figures: {} })
    }).then(function (r) { return r.json(); }).then(function (j) {
      if (j && j.ok) {
        if (out) out.textContent = j.text;
        if (id && window.Shiny) window.Shiny.setInputValue("gsas_summary_save", { id: Number(id), text: j.text, nonce: Date.now() }, { priority: "event" });
        return;
      }
      if (out && prior.indexOf("Locked field") === 0) out.textContent = prior + "\n\n" + ((j && j.error) || "The wording service did not add a summary.");
      else if (out) out.textContent = (j && j.error) || "No summary.";
    }).catch(function () {
      if (out && prior.indexOf("Locked field") === 0) out.textContent = prior;
      else if (out) out.textContent = "The wording service is not running.";
    });
  }

  var saved = "en";
  try { saved = localStorage.getItem("umuganura_gsas_lang") || "en"; } catch (e) {}
  applyLang(saved);
})();
