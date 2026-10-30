# Institutional workflow for Ejo Hazaza solutions (funding, 6-step framework, email reports).
# Sourced after ejo.R / ejo_detail.R. Safe defaults — never assume inputs exist at init.

ejo_fund_bodies <- c(
  Government = "Government",
  NGO = "NGO",
  Investor = "Institutional Investor"
)
ejo_fund_statuses <- c(
  process = "In process",
  pending = "Pending",
  approved = "Approved",
  cleared = "Cleared"
)

ejo_inst_mail <- c(
  "MINAGRI" = "postharvest@minagri.gov.rw",
  "MININFRA" = "projects@mininfra.gov.rw",
  "MINECOFIN" = "ppp@minecofin.gov.rw",
  "MINICOM" = "trade@minicom.gov.rw",
  "RDB" = "investment@rdb.rw",
  "RPPA" = "tenders@rppa.gov.rw",
  "NCST" = "challenges@ncst.gov.rw",
  "RAB" = "extension@rab.gov.rw",
  "NAEB" = "operations@naeb.gov.rw",
  "NISR" = "data@statistics.gov.rw",
  "RSB" = "standards@rsb.gov.rw",
  "RTDA" = "roads@rtda.gov.rw",
  "Meteo Rwanda" = "forecast@meteorwanda.gov.rw",
  "districts" = "districts@gov.rw",
  "cooperatives" = "coop@rca.gov.rw",
  "Private sector" = "partners@private.rw"
)

ejo_nst2_map <- list(
  weather = list(
    nst2 = "NST2: climate-resilient production and reduced post-harvest loss under the agriculture pillar.",
    ministries = "MINAGRI (lead), Meteo Rwanda, RAB, districts.",
    instruments = "Seasonal advisory budgets; cooperative dryer CAPEX windows."
  ),
  storage = list(
    nst2 = "NST2: modernised storage and warehouse receipts to stabilise farm-gate prices.",
    ministries = "MINAGRI / MINICOM (lead), RAB, RSB, cooperatives.",
    instruments = "p-median warehouse siting; moisture and aflatoxin compliance."
  ),
  handling = list(
    nst2 = "NST2: quality and packaging for domestic and export markets.",
    ministries = "MINAGRI, RSB, RAB extension, private packers.",
    instruments = "Crate break-even and handling training envelopes."
  ),
  transport = list(
    nst2 = "NST2: productive infrastructure and efficient produce corridors.",
    ministries = "MININFRA / RTDA (roads), NAEB / MINICOM (market links).",
    instruments = "OSRM route optimisation; feeder-road and truck pool PPPs."
  ),
  cold = list(
    nst2 = "NST2: horticulture and dairy value chains with cold-chain capacity.",
    ministries = "MINAGRI, NAEB, private cold-store operators.",
    instruments = "M/M/c cold-room sizing; solar cold rooms at collection."
  ),
  glut = list(
    nst2 = "NST2: market stabilisation, processing and institutional demand.",
    ministries = "MINICOM, MINAGRI, MINEDUC (school feeding), NAEB.",
    instruments = "Multi-period store/sell plan; processing and calendar advice."
  )
)

ejo_inst_steps <- list(
  list(
    id = "1", title = "National Priority Alignment",
    body = function(op, nst) paste0(
      "Map this solution to NST2 and the line ministries that own the outcome.\n\n",
      "NST2 link: ", nst$nst2, "\n",
      "Line ministries: ", nst$ministries, "\n",
      "Solution: ", op$id, " — ", op$option, "\n",
      "Team lead on file: ", op$lead, "."
    )
  ),
  list(
    id = "2", title = "Institutional Arrangement Selection",
    body = function(op, nst) paste0(
      "Choose the legal track that fits the funder and the works.\n\n",
      "Tracks available:\n",
      "• PPP / BOT — when private capital builds and operates (typical for cold rooms, trucks, warehouses).\n",
      "• Umucyo e-Procurement / RPPA tender — when public CAPEX buys goods or works.\n",
      "• NCST innovation challenge — when the solution is a pilot technology or advisory system.\n\n",
      "Suggested for ", op$id, ": ",
      if (grepl("Private|NAEB|truck|cold|warehouse|Processing", op$option, ignore.case = TRUE))
        "PPP/BOT or mixed public–private with RPPA works packages."
      else if (grepl("SMS|advice|calendar|training|early-warning", op$option, ignore.case = TRUE))
        "NCST / RAB extension challenge or framework contract."
      else "Umucyo / RPPA tender under MINAGRI or the named lead.",
      "\nInstruments: ", nst$instruments
    )
  ),
  list(
    id = "3", title = "Feasibility & Risk Assessment",
    body = function(op, nst) {
      ap <- tryCatch(ejo_appraisals[[op$cause]], error = function(e) NULL)
      row <- if (!is.null(ap) && nrow(ap)) ap[ap$id == op$id, , drop = FALSE] else NULL
      paste0(
        "Technical viability, value for money (VFM) and fiscal risk for MINECOFIN / RDB.\n\n",
        "Operations research on this cause: ", ejo_or[[op$cause]], ".\n",
        if (!is.null(row) && nrow(row)) paste0(
          "Capital: ", fmt_num(row$capex_bn[1], 1), " bn RWF · Running / yr: ", fmt_num(row$opex_bn[1], 1), " bn RWF\n",
          "Effect on this cause's loss: ", fmt_num(100 * row$effect[1], 0), "%\n",
          "Gain 2026–2031: ", fmt_bn(row$gain[1]), " · NPV: ", fmt_bn(row$npv[1]),
          " · BCR: ", if (is.finite(row$bcr[1])) paste0(fmt_num(row$bcr[1], 1), "×") else "—", "\n"
        ) else "Appraisal row not loaded.\n",
        "Fiscal note: CAPEX sits with the selected funding body; opex must be ring-fenced in the lead institution's budget."
      )
    }
  ),
  list(
    id = "4", title = "Procurement & SPV Selection",
    body = function(op, nst) paste0(
      "RFQ → RFP → evaluation → award. Where private finance is used, incorporate a Special Purpose Vehicle (SPV).\n\n",
      "RFQ: shortlist suppliers against ", op$id, " technical specs derived from the live OR model.\n",
      "RFP: priced bids in RWF; evaluation weights technical / financial as set by RPPA guidance.\n",
      "SPV: local company (or JV) named on the contract; lead institution ", ejo_lead_parts(op$lead)[1],
      " chairs the contracting authority board seat."
    )
  ),
  list(
    id = "5", title = "Contract Negotiation & Signing",
    body = function(op, nst) paste0(
      "Lock RWF-denominated terms, SLAs and risk-sharing.\n\n",
      "Currency: Rwandan Franc (RWF) for CAPEX, opex and penalties.\n",
      "SLA examples: dryer uptime, cold-room temperature band, route on-time %, loss-cut versus baseline.\n",
      "Risk-sharing: construction delay → contractor; demand/volume → shared; force majeure → public contingency.\n",
      "Contracting authority: ", ejo_lead_parts(op$lead)[1], " with co-signatories from the funding body."
    )
  ),
  list(
    id = "6", title = "Co-Implementation, Monitoring & Handover",
    body = function(op, nst) {
      reps <- ejo_inst_report_targets(op)
      paste0(
        "Joint Steering Committee, monitoring lines and institutional handover.\n\n",
        "Steering Committee: funding body + ", ejo_lead_parts(op$lead)[1],
        " + technical partners on the option.\n",
        "Monitoring uses the Report-goes-to column already on the action plan: ",
        paste(unique(reps), collapse = "; "), ".\n",
        "Handover: after the CPM finish month, assets and data rooms transfer to the lead; Umuganura keeps the progress log."
      )
    }
  )
)

ejo_inst_report_targets <- function(op) {
  if (is.null(op) || !nrow(as.data.frame(op))) return(character())
  parts <- unique(unlist(lapply(ejo_lead_parts(op$lead), ejo_lead_parts)))
  acts <- tryCatch(ejo_pm[ejo_pm$cause == op$cause, , drop = FALSE], error = function(e) NULL)
  if (!is.null(acts) && nrow(acts)) {
    parts <- unique(c(parts, acts$report_to, unlist(lapply(acts$lead, ejo_lead_parts))))
  }
  parts <- parts[nzchar(parts)]
  parts
}

ejo_inst_email_for <- function(name) {
  if (!nzchar(name)) return("reports@umuganura.rw")
  hit <- ejo_inst_mail[names(ejo_inst_mail) == name]
  if (length(hit)) return(unname(hit[[1]]))
  soft <- ejo_inst_mail[vapply(names(ejo_inst_mail), function(n) grepl(n, name, ignore.case = TRUE), logical(1))]
  if (length(soft)) return(unname(soft[[1]]))
  paste0(gsub("[^A-Za-z0-9]+", ".", tolower(name)), "@gov.rw")
}

ejo_inst_active_op <- function(act) {
  if (is.null(act) || is.null(act$id)) return(NULL)
  op <- ejo_options[ejo_options$id == act$id, , drop = FALSE]
  if (!nrow(op)) return(NULL)
  if (!is.null(act$scope) && act$scope != "ap" && act$scope %in% names(ejo_causes)) {
    if (!identical(op$cause[1], act$scope)) return(NULL)
  }
  op[1, ]
}

ejo_inst_is_option_id <- function(scope, id) {
  if (is.null(id) || !nzchar(id)) return(FALSE)
  if (identical(scope, "ap")) return(id %in% ejo_options$id)
  if (!scope %in% names(ejo_causes)) return(FALSE)
  id %in% ejo_options$id[ejo_options$cause == scope]
}

ejo_inst_proposal_text <- function(op, fund, status, step_id = "1") {
  if (is.null(op) || !nrow(as.data.frame(op))) return("No solution selected.")
  nst <- ejo_nst2_map[[op$cause]]
  if (is.null(nst)) nst <- list(nst2 = "NST2 agriculture pillar.", ministries = op$lead, instruments = ejo_or[[op$cause]])
  ap <- tryCatch(ejo_appraisals[[op$cause]], error = function(e) NULL)
  row <- if (!is.null(ap) && nrow(ap)) ap[ap$id == op$id, , drop = FALSE] else NULL
  step <- ejo_inst_steps[[max(1, min(6, suppressWarnings(as.integer(step_id))))]]
  paste0(
    "UMUGANURA — EJO HAZAZA FUNDING PROPOSAL PLAN\n",
    "============================================\n",
    "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n",
    "Cause: ", ejo_causes[[op$cause]], "\n",
    "Solution: ", op$id, " — ", op$option, "\n",
    "Team lead: ", op$lead, "\n\n",
    "1. FUNDING BODY\n",
    "   Source: ", fund, "\n",
    "   Status: ", status, "\n\n",
    "2. OPERATIONS RESEARCH BASIS\n",
    "   Model: ", ejo_or[[op$cause]], "\n",
    "   Site/context: ", unname(ejo_where[[op$cause]]), "\n",
    "   Data already loaded: ", unname(ejo_data_src[[op$cause]]), "\n\n",
    "3. COST–BENEFIT (RWF)\n",
    if (!is.null(row) && nrow(row)) paste0(
      "   Capital (bn RWF): ", fmt_num(row$capex_bn[1], 2), "\n",
      "   Running / year (bn RWF): ", fmt_num(row$opex_bn[1], 2), "\n",
      "   Loss cut: ", fmt_num(100 * row$effect[1], 0), "%\n",
      "   Gain 2026–2031: ", fmt_bn(row$gain[1]), "\n",
      "   NPV: ", fmt_bn(row$npv[1]), "\n",
      "   Benefit / cost: ", if (is.finite(row$bcr[1])) paste0(fmt_num(row$bcr[1], 2), "×") else "—", "\n",
      "   Payback: ", if (is.finite(row$payback[1])) row$payback[1] else "after 2031", "\n"
    ) else "   Appraisal not available.\n",
    "\n4. NST2 / INSTITUTIONAL ALIGNMENT\n",
    "   ", nst$nst2, "\n",
    "   Ministries: ", nst$ministries, "\n\n",
    "5. CURRENT FRAMEWORK STEP\n",
    "   Step ", step$id, ": ", step$title, "\n",
    step$body(op, nst), "\n\n",
    "6. REPORTING ENDPOINTS\n",
    paste(vapply(ejo_inst_report_targets(op), function(x) paste0("   - ", x, " <", ejo_inst_email_for(x), ">"), character(1)), collapse = "\n"),
    "\n\nThis brief is assembled from Umuganura Ejo Hazaza planning files (ejo_options.csv, appraisals, action plans).\n"
  )
}

ejo_inst_email_body <- function(op, fund, status, step_id, note = "") {
  nst <- ejo_nst2_map[[op$cause]]
  if (is.null(nst)) nst <- list(nst2 = "", ministries = "", instruments = "")
  step <- ejo_inst_steps[[max(1, min(6, suppressWarnings(as.integer(step_id))))]]
  paste0(
    "Institutional progress report — Umuganura / Ejo Hazaza\n\n",
    "Solution: ", op$id, " — ", op$option, "\n",
    "Cause: ", ejo_causes[[op$cause]], "\n",
    "Funding: ", fund, " (", status, ")\n",
    "Lifecycle step: ", step$id, " — ", step$title, "\n\n",
    "Where the project is: ", status, " under ", fund, " financing.\n",
    "Where it is going: advancing ", step$title, " toward co-implementation and handover.\n\n",
    step$body(op, nst), "\n\n",
    if (nzchar(note)) paste0("Officer note:\n", note, "\n\n") else "",
    "Generated ", format(Sys.time(), "%Y-%m-%d %H:%M"), " from Umuganura.\n"
  )
}

ejo_pm_statuses <- c(
  on_track = "On track",
  at_risk = "At risk",
  delayed = "Delayed",
  completed = "Completed"
)

ejo_inst_link_banner <- function(op, state = NULL) {
  sid <- if (!is.null(state$framework_step)) as.character(state$framework_step) else "1"
  step_n <- max(1, min(6, suppressWarnings(as.integer(sid))))
  step <- ejo_inst_steps[[step_n]]
  fund <- if (!is.null(state$fund_source)) state$fund_source else "—"
  fst <- if (!is.null(state$fund_status) && state$fund_status %in% names(ejo_fund_statuses)) {
    ejo_fund_statuses[[state$fund_status]]
  } else if (!is.null(state$fund_status)) state$fund_status else "—"
  pm <- if (!is.null(state$pm_status) && state$pm_status %in% names(ejo_pm_statuses)) {
    ejo_pm_statuses[[state$pm_status]]
  } else if (!is.null(state$pm_status) && nzchar(state$pm_status)) state$pm_status else "Not filed yet"
  tags$div(
    class = "ej-inst-link",
    tags$p(class = "kicker", "CONNECTED COLLECTION"),
    tags$p(class = "note", paste0(
      "Every partner report on ", op$id, " is stored against framework step ", step$id,
      " (", step$title, "), funding ", fund, " · ", fst, ", and PM status ", pm,
      ". Login required so the institution of record is clear."
    ))
  )
}

ejo_inst_eval_ui <- function(fund, status, step_id) {
  st <- match(status, names(ejo_fund_statuses), nomatch = 1)
  step_n <- max(1, min(6, suppressWarnings(as.integer(step_id))))
  pct <- round(100 * ((st - 1) / 3 * 0.45 + (step_n / 6) * 0.55))
  tags$div(
    class = "ej-inst-eval",
    tags$p(tags$strong("Evaluation snapshot")),
    tags$p(class = "note", paste0(
      "Funding body: ", fund, " · Clearance: ", ejo_fund_statuses[[names(ejo_fund_statuses)[st]]],
      " · Framework step ", step_n, " of 6 (", ejo_inst_steps[[step_n]]$title, ")."
    )),
    tags$div(class = "ej-inst-meter", tags$span(style = paste0("width:", pct, "%"))),
    tags$p(class = "note", paste0("Composite readiness ≈ ", pct, "%. Cleared funding + step 6 implies handover-ready."))
  )
}

ejo_inst_workflow_ui <- function(op, state = NULL) {
  if (is.null(op) || !nrow(as.data.frame(op))) return(NULL)
  nst <- ejo_nst2_map[[op$cause]]
  if (is.null(nst)) nst <- list(nst2 = "NST2 agriculture pillar.", ministries = op$lead, instruments = ejo_or[[op$cause]])
  targets <- ejo_inst_report_targets(op)
  if (!length(targets)) targets <- ejo_lead_parts(op$lead)
  mail_choices <- stats::setNames(
    vapply(targets, ejo_inst_email_for, character(1)),
    paste0(targets, " · ", vapply(targets, ejo_inst_email_for, character(1)))
  )
  step_choices <- stats::setNames(
    vapply(ejo_inst_steps, `[[`, character(1), "id"),
    paste0("Step ", vapply(ejo_inst_steps, `[[`, character(1), "id"), " · ", vapply(ejo_inst_steps, `[[`, character(1), "title"))
  )
  fund0 <- if (!is.null(state$fund_source) && state$fund_source %in% unname(ejo_fund_bodies)) state$fund_source else "Government"
  st0 <- if (!is.null(state$fund_status) && state$fund_status %in% names(ejo_fund_statuses)) state$fund_status else "process"
  step0 <- if (!is.null(state$framework_step) && state$framework_step %in% unname(step_choices)) state$framework_step else "1"
  tags$div(
    class = "ej-inst",
    ejo_inst_link_banner(op, state),
    tags$div(
      class = "ej-inst-banner",
      tags$p(class = "kicker", "INSTITUTIONAL WORKFLOW"),
      tags$h3("Funding, six-step government framework, and automated reports"),
      tags$p(class = "note", paste0(
        "Context: ", ejo_causes[[op$cause]], " · OR: ", ejo_or[[op$cause]],
        ". Partner reports below lock to the selected framework step so collection stays one chain."
      ))
    ),
    fluidRow(
      column(
        12,
        tags$div(
          class = "ej-inst-fund chart-card",
          tags$h3("Funding source"),
          tags$p(class = "note", "Choose who finances this solution. Status moves from in process → pending → approved → cleared."),
          radioButtons(
            "ej_fund_source", "Funding body",
            choices = ejo_fund_bodies,
            selected = fund0,
            inline = TRUE
          ),
          radioButtons(
            "ej_fund_status", "Clearance status",
            choices = ejo_fund_statuses,
            selected = st0,
            inline = TRUE
          ),
          actionButton("ej_fund_save", "Save funding state to database", class = "btn-go"),
          uiOutput("ej_fund_saved"),
          tags$div(
            class = "ej-inst-actions",
            downloadButton("ej_fund_proposal", "Download Funding Proposal Plan", class = "btn-go ej-inst-dl"),
            tags$span(class = "note", "Assembles funder, OR math, and RWF cost–benefit into one brief.")
          )
        )
      )
    ),
    fluidRow(
      column(
        4,
        tags$div(
          class = "ej-inst-side chart-card",
          tags$h3("Six-step framework"),
          radioButtons("ej_inst_step", NULL, choices = step_choices, selected = step0),
          tags$p(class = "note", "NST2 → arrangement → feasibility → procurement → contract → co-implementation. Tick milestones; partner reports inherit this step.")
        )
      ),
      column(
        8,
        tags$div(
          class = "ej-inst-step-body chart-card",
          uiOutput("ej_inst_step_panel"),
          uiOutput("ej_inst_milestones"),
          actionButton("ej_mile_save", "Save step milestones", class = "btn-go"),
          uiOutput("ej_mile_saved")
        )
      )
    ),
    fluidRow(
      column(
        12,
        tags$div(
          class = "ej-inst-mail chart-card",
          tags$h3("Automated reporting & compliance"),
          tags$p(class = "note", paste0(
            "Task list from Report-goes-to and leads on this solution: ",
            paste(targets, collapse = "; "), "."
          )),
          tags$ul(class = "ej-inst-tasks", lapply(targets, function(t) {
            tags$li(tags$b(t), " — oversight endpoint ", tags$code(ejo_inst_email_for(t)))
          })),
          selectInput("ej_email_to", "SEND REPORT TO", choices = mail_choices, width = "100%"),
          textAreaInput(
            "ej_email_preview", "Report preview",
            value = ejo_inst_email_body(
              op, fund0,
              if (st0 %in% names(ejo_fund_statuses)) ejo_fund_statuses[[st0]] else st0,
              step0
            ),
            rows = 10, width = "100%"
          ),
          uiOutput("ej_inst_eval"),
          actionButton("ej_email_send", "Generate & Email Report", class = "btn-go"),
          uiOutput("ej_email_done")
        )
      )
    )
  )
}
