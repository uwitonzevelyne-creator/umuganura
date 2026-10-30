# Ejo hazaza: inside one activity or one chosen option. Six views on how it gets done:
# dashboard, machine-learning forecast, data pipeline, automated insights, data API, decision optimisation.

ej_dsec <- function(n, title, sub, ...) {
  tags$section(
    class = "ej-dsec",
    tags$div(class = "ej-sec", tags$b(n), tags$div(tags$h3(title), tags$p(sub))),
    ...
  )
}
ej_card <- function(title, ...) tags$div(class = "chart-card", if (!is.null(title)) tags$h3(class = "card-title", title), ...)
ej_kpis <- function(...) tags$div(class = "kpi-row hv-kpis", ...)

# ---- machine learning: cause loss forecast with rolling-origin model selection ----
ejo_ml_cache <- new.env()
ejo_ml <- function(k) {
  if (!is.null(ejo_ml_cache[[k]])) return(ejo_ml_cache[[k]])
  cy <- ejo_cy[ejo_cy$cause == k & ejo_cy$year <= 2023 & is.finite(ejo_cy$total), c("year", "total")]
  cy <- cy[order(cy$year), , drop = FALSE]
  d <- data.frame(year = cy$year, v = cy$total / 1e9)
  fit_pred <- function(m, tr, yrs, level = 0.8) {
    if (m == "Linear trend") {
      f <- stats::lm(v ~ year, tr)
      p <- stats::predict(f, data.frame(year = yrs), interval = "prediction", level = level)
      return(data.frame(fit = p[, 1], lo = p[, 2], hi = p[, 3]))
    }
    if (m == "Quadratic trend") {
      f <- stats::lm(v ~ year + I((year - 2010)^2), tr)
      p <- stats::predict(f, data.frame(year = yrs), interval = "prediction", level = level)
      return(data.frame(fit = p[, 1], lo = p[, 2], hi = p[, 3]))
    }
    if (m == "Constant growth (log-linear)") {
      f <- stats::lm(log(v) ~ year, tr[tr$v > 0, ])
      p <- exp(stats::predict(f, data.frame(year = yrs), interval = "prediction", level = level))
      return(data.frame(fit = p[, 1], lo = p[, 2], hi = p[, 3]))
    }
    if (m == "Recent trend (2010+)") {
      t2 <- tr[tr$year >= 2010, ]
      if (nrow(t2) < 4) t2 <- tr
      f <- stats::lm(v ~ year, t2)
      p <- stats::predict(f, data.frame(year = yrs), interval = "prediction", level = level)
      return(data.frame(fit = p[, 1], lo = p[, 2], hi = p[, 3]))
    }
    h <- tryCatch(stats::HoltWinters(stats::ts(tr$v, start = min(tr$year)), gamma = FALSE), error = function(e) NULL)
    if (is.null(h)) return(data.frame(fit = rep(NA_real_, length(yrs)), lo = NA_real_, hi = NA_real_))
    n <- max(yrs) - max(tr$year)
    p <- stats::predict(h, n.ahead = n, prediction.interval = TRUE, level = level)
    p <- p[(nrow(p) - length(yrs) + 1):nrow(p), , drop = FALSE]
    data.frame(fit = p[, "fit"], lo = p[, "lwr"], hi = p[, "upr"])
  }
  models <- c("Linear trend", "Quadratic trend", "Constant growth (log-linear)", "Recent trend (2010+)", "Holt exponential smoothing")
  origins <- utils::tail(d$year, 6)
  cv <- do.call(rbind, lapply(models, function(m) {
    e <- vapply(origins, function(t) {
      tr <- d[d$year < t, ]
      if (nrow(tr) < 6) return(NA_real_)
      p <- tryCatch(fit_pred(m, tr, t)$fit, error = function(e) NA_real_)
      p - d$v[d$year == t]
    }, numeric(1))
    a <- d$v[match(origins, d$year)]
    data.frame(model = m, rmse = sqrt(mean(e^2, na.rm = TRUE)), mape = 100 * mean(abs(e) / a, na.rm = TRUE), n = sum(is.finite(e)))
  }))
  cv <- cv[order(cv$rmse), , drop = FALSE]
  best <- cv$model[which(is.finite(cv$rmse))[1]]
  fy <- 2024:2031
  fc <- cbind(year = fy, fit_pred(best, d, fy))
  fc$lo <- pmax(fc$lo, 0)
  out <- list(hist = d, cv = cv, best = best, fc = fc)
  ejo_ml_cache[[k]] <- out
  out
}

# ---- Monte Carlo of the cause's CPM network (beta-PERT durations) ----
ejo_mc_cache <- new.env()
ejo_mc <- function(k, n = 4000) {
  if (!is.null(ejo_mc_cache[[k]])) return(ejo_mc_cache[[k]])
  cp <- ejo_cpm(k)
  pl <- cp$plan
  set.seed(2026)
  m <- nrow(pl)
  D <- sapply(seq_len(m), function(i) {
    a <- pl$opt[i]; b <- pl$pess[i]; md <- pl$likely[i]
    if (b <= a) return(rep(md, n))
    al <- 1 + 4 * (md - a) / (b - a)
    be <- 1 + 4 * (b - md) / (b - a)
    a + (b - a) * stats::rbeta(n, al, be)
  })
  pre <- lapply(ifelse(is.na(pl$after), "", pl$after), function(s) match(setdiff(strsplit(s, ";", fixed = TRUE)[[1]], ""), pl$id))
  ES <- EF <- matrix(0, n, m)
  for (i in seq_len(m)) {
    ES[, i] <- if (length(pre[[i]])) apply(EF[, pre[[i]], drop = FALSE], 1, max) else 0
    EF[, i] <- ES[, i] + D[, i]
  }
  E <- apply(EF, 1, max)
  LF <- LS <- matrix(0, n, m)
  for (i in rev(seq_len(m))) {
    succ <- which(vapply(pre, function(z) i %in% z, logical(1)))
    LF[, i] <- if (length(succ)) apply(LS[, succ, drop = FALSE], 1, min) else E
    LS[, i] <- LF[, i] - D[, i]
  }
  crit <- colMeans((LS - ES) < 1e-6)
  out <- list(plan = pl, D = D, ES = ES, EF = EF, E = E, crit = setNames(crit, pl$id))
  ejo_mc_cache[[k]] <- out
  out
}

# ---- decision optimisation: spread a budget over districts (LP with diminishing returns) ----
ejo_cause_weights <- function(k) {
  if (k == "cold" && length(ejo_hort_t)) {
    w <- setNames(rep(0, length(ejo_districts)), ejo_districts)
    h <- ejo_hort_t[names(ejo_hort_t) %in% ejo_districts]
    w[names(h)] <- h
    if (sum(w) > 0) return(w / sum(w))
  }
  crops <- if (k %in% c("storage", "weather")) c("Maize", "Bean", "Paddy rice", "Sorghum", "Wheat") else unique(sas_prod$crop)
  ejo_district_share(crops)
}
ejo_access <- local({
  if (is.null(ejo_net)) return(setNames(rep(20, length(ejo_districts)), ejo_districts))
  mk <- ejo_net$nodes$id[ejo_net$nodes$type == "market"]
  vapply(ejo_districts, function(d) {
    r <- ejo_net$km[paste0("d:", d), mk]
    mean(utils::head(sort(r), 3))
  }, numeric(1))
})
ejo_alloc_lp <- function(w, budget, ret) {
  n <- length(w)
  eff <- 1 / (1 + ejo_access[names(w)] / 30)
  r1 <- ret * eff
  obj <- c(r1, 0.5 * r1)
  A <- rbind(c(rep(1, n), rep(1, n)), cbind(diag(n), matrix(0, n, n)), cbind(matrix(0, n, n), diag(n)), cbind(diag(n), matrix(0, n, n)))
  dir <- c("=", rep("<=", n), rep("<=", n), rep(">=", n))
  rhs <- c(budget, budget * w, budget * w, 0.25 * budget * w)
  if (!requireNamespace("lpSolve", quietly = TRUE)) return(NULL)
  s <- lpSolve::lp("max", obj, A, dir, rhs, compute.sens = TRUE)
  if (s$status != 0) return(NULL)
  x1 <- s$solution[1:n]
  x2 <- s$solution[(n + 1):(2 * n)]
  d <- data.frame(district = names(w), share = as.numeric(w), km = as.numeric(ejo_access[names(w)]), eff = as.numeric(eff),
                  alloc = x1 + x2, avoided = x1 * r1 + x2 * 0.5 * r1, stringsAsFactors = FALSE)
  list(d = d[order(-d$alloc), ], dual = s$duals[1], total = s$objval)
}

# ---- data pipeline: what feeds each cause ----
ejo_nrow <- function(x) if (is.null(x)) 0 else nrow(x)
ejo_sources <- function(k, text = "") {
  base <- list(
    c("FAO food balances 2010\u20132023", "CLEANED/master_rwanda_ag/fao_rwanda_food_balances.csv", ejo_nrow(ejo_fbs), "yearly", "live"),
    c("FAO food balances 1961\u20132013 (old method)", "EXTERNAL/ejo_hazaza/fao_fbs_historic_rwanda.csv", ejo_nrow(ejo_fbs_old), "archive", "live"),
    c("FAOSTAT producer prices", "CLEANED/master_rwanda_ag/fao_rwanda_producer_prices.csv", ejo_nrow(crop_prices), "yearly", "live")
  )
  by <- list(
    transport = list(c("NAEB / Esoko market prices", "RWANDA_INSTITUTIONS/NAEB/market prices", ejo_nrow(naeb_prices), "weekly", "live"),
                     c("OSRM road matrix 87 \u00d7 87", "EXTERNAL/ejo_hazaza/osrm_road_matrix.csv", ejo_nrow(ejo_matrix), "quarterly", "live"),
                     c("OSRM road geometry", "EXTERNAL/ejo_hazaza/osrm_routes.csv", ejo_nrow(ejo_routes), "quarterly", "live"),
                     c("NISR SAS district production", "CLEANED/nisr_sas_tables", ejo_nrow(sas_prod), "per season", "live")),
    storage = list(c("APHLIS cereal losses by stage", "EXTERNAL/ejo_hazaza/aphlis_rwanda_losses.csv", ejo_nrow(ejo_aphlis), "yearly", "live"),
                   c("NISR SAS district production", "CLEANED/nisr_sas_tables", ejo_nrow(sas_prod), "per season", "live"),
                   c("NISR SAS use of harvest (sold, stored, lost)", "CLEANED/nisr_sas_tables", ejo_nrow(sas_use), "per season", "live")),
    weather = list(c("NASA POWER district weather", "EXTERNAL/ejo_hazaza/nasa_power_district_monthly.csv", ejo_nrow(ejo_power), "monthly", "live"),
                   c("NISR SAS district production", "CLEANED/nisr_sas_tables", ejo_nrow(sas_prod), "per season", "live")),
    cold = list(c("NASA POWER max temperature", "EXTERNAL/ejo_hazaza/nasa_power_district_monthly.csv", ejo_nrow(ejo_power), "monthly", "live"),
                c("NAEB horticulture exports", "RWANDA_INSTITUTIONS/NAEB/cleaned", ejo_nrow(ejo_hort_series), "monthly", "live"),
                c("NAEB / Esoko market prices", "RWANDA_INSTITUTIONS/NAEB/market prices", ejo_nrow(naeb_prices), "weekly", "live")),
    glut = list(c("NAEB / Esoko market prices", "RWANDA_INSTITUTIONS/NAEB/market prices", ejo_nrow(naeb_prices), "weekly", "live"),
                c("NISR SAS district production", "CLEANED/nisr_sas_tables", ejo_nrow(sas_prod), "per season", "live")),
    handling = list(c("NISR SAS district production", "CLEANED/nisr_sas_tables", ejo_nrow(sas_prod), "per season", "live"))
  )
  t <- tolower(text)
  plan <- list(
    c("survey|map|flow", "Trader and flow survey (KoboToolbox forms)", "new: field survey"),
    c("sms|app|alert|link", "SMS gateway and app usage logs", "new: telecom feed"),
    c("site|build|centre|warehouse|shed|floor", "District site register and build progress", "new: MINAGRI/district reports"),
    c("road", "RTDA feeder-road condition and works", "new: RTDA feed"),
    c("truck|lease|contract", "GPS fleet telemetry (trips, hours, temperature)", "new: operator API"),
    c("cold|chiller|logger|refrigerat|48-hour", "Cold-room and logger temperatures (IoT)", "new: sensor stream"),
    c("dryer|tarpaulin|dry", "Dryer moisture-meter readings", "new: cooperative reports"),
    c("crate|packag", "Crate rental and packaging ledger", "new: cooperative ledger"),
    c("train", "Extension training register", "new: RAB register"),
    c("bag|hermetic|aflatoxin|test", "Moisture and aflatoxin test results (RSB)", "new: lab results"),
    c("receipt|bank", "Warehouse receipts and loans (EAX, banks)", "new: EAX API"),
    c("price|calendar|market", "Monthly price calendar per product", "derived from NAEB prices"),
    c("contract|school|institution", "Institutional supply contracts", "new: MINEDUC/MINAGRI"),
    c("process", "Processing-unit intake volumes", "new: MINICOM reports"),
    c("measure|track|audit|monitor|review", "Season loss monitoring panel (NISR)", "new: NISR survey module"),
    c("rank|risk|model|queue|p-median|pareto|size|choose", "Umuganura model outputs (this platform)", "derived")
  )
  extra <- lapply(plan, function(p) if (grepl(p[1], t)) c(p[2], p[3], NA, "per activity cycle", "planned") else NULL)
  extra <- Filter(Negate(is.null), extra)
  rows <- c(base, by[[k]], extra)
  data.frame(source = vapply(rows, `[`, "", 1), where = vapply(rows, `[`, "", 2), rows = suppressWarnings(as.numeric(vapply(rows, `[`, "", 3))),
             refresh = vapply(rows, `[`, "", 4), status = vapply(rows, `[`, "", 5), stringsAsFactors = FALSE)
}
ejo_transforms <- list(
  transport = c("Clean and geocode 57 markets", "Join prices to the road matrix", "Net price = price \u2212 spoilage \u2212 transport", "Solve the transportation LP"),
  storage = c("Average APHLIS losses by stage", "Join district production", "Distance matrix between districts", "Solve the p-median model"),
  weather = c("Harvest-month rain per district", "Rain anomaly against the 24-year mean", "Bootstrap 5,000 seasons", "Expected monetary value of each defence"),
  cold = c("Hourly spoilage from max temperature", "Lot arrivals per district", "Erlang C queue per room count", "Pick the cheapest room count"),
  glut = c("Monthly median price per product", "Harvest calendar", "Store-or-sell LP over 12 months", "Gain per tonne \u00d7 marketed volume"),
  handling = c("Handling share of loss per product", "Pareto ranking", "Crate cost per tonne", "Benefit / cost per product")
)

# ---- shared builders ----
ejo_api_block <- function(path, payload, consumes) {
  tags$div(
    class = "split",
    ej_card("Endpoints this work exposes",
            tags$table(class = "hv-table ej-table", tags$thead(tags$tr(tags$th("Method"), tags$th("Endpoint"), tags$th("Returns"))),
                       tags$tbody(lapply(path, function(p) tags$tr(tags$td(tags$b(p[1])), tags$td(tags$code(p[2])), tags$td(class = "ej-small", p[3])))))),
    ej_card("APIs it pulls from",
            tags$table(class = "hv-table ej-table", tags$thead(tags$tr(tags$th("Provider"), tags$th("Call"))),
                       tags$tbody(lapply(consumes, function(p) tags$tr(tags$td(tags$b(p[1])), tags$td(class = "ej-small", tags$code(p[2])))))))
  )
}
ejo_consumes <- function(k) {
  common <- list(c("FAOSTAT", "bulks-faostat.fao.org/production/FoodBalanceSheets*_E_Africa.zip"),
                 c("World Bank", "api.worldbank.org/v2/country/RWA/indicator/FP.CPI.TOTL"))
  by <- list(
    transport = list(c("OSRM", "router.project-osrm.org/table/v1/driving/{lon,lat;...}"), c("OSRM", "router.project-osrm.org/route/v1/driving/{from};{to}?geometries=geojson"), c("NAEB / Esoko", "market price export (weekly CSV)")),
    storage = list(c("APHLIS", "aphlis.net/en/data/tables/value-chain/RW/{crop}/{year}?download=csv"), c("NISR", "SAS tables (seasonal)")),
    weather = list(c("NASA POWER", "power.larc.nasa.gov/api/temporal/monthly/point?parameters=PRECTOTCORR,T2M_MAX,RH2M"), c("Meteo Rwanda", "seasonal forecast bulletin")),
    cold = list(c("NASA POWER", "power.larc.nasa.gov/api/temporal/monthly/point?parameters=T2M_MAX"), c("NAEB", "horticulture export performance")),
    glut = list(c("NAEB / Esoko", "market price export (weekly CSV)"), c("NISR", "SAS tables (seasonal)")),
    handling = list(c("NISR", "SAS tables (seasonal)"))
  )
  c(by[[k]], common)
}
ejo_pipeline_ui <- function(k, src, title) {
  tr <- ejo_transforms[[k]]
  outs <- c("Money lost by cause and year (ejo_cy)", "Activity and option KPIs", "Alerts and summaries", "API cache (JSON)")
  rows <- ifelse(is.finite(src$rows) & src$rows > 0, src$rows, 50)
  lab <- c(src$source, tr, outs)
  ns <- nrow(src)
  nt <- length(tr)
  s1 <- seq_len(ns) - 1
  t1 <- ns + ((seq_len(ns) - 1) %% nt)
  s2 <- ns + seq_len(nt) - 1
  inflow <- tapply(rows, t1, sum)
  link <- data.frame(s = c(s1, rep(s2, each = length(outs))), t = c(t1, rep(ns + nt + seq_along(outs) - 1, nt)),
                     v = c(log10(rows + 10), rep(log10(as.numeric(inflow[as.character(s2)]) + 10) / length(outs), each = length(outs))))
  link$v[!is.finite(link$v)] <- 1
  col <- c(ifelse(src$status == "live", "#2d6a4f", "#c6a336"), rep(ejo_blue, nt), rep("#1b4332", length(outs)))
  p <- plotly::plot_ly(type = "sankey", orientation = "h",
                       node = list(label = lab, color = col, pad = 12, thickness = 16),
                       link = list(source = link$s, target = link$t, value = link$v, color = "rgba(45,106,79,0.18)")) |>
    plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", font = list(size = 11), margin = list(l = 4, r = 4, t = 10, b = 10))
  now <- Sys.time()
  steps <- c(paste("Extract:", src$source), paste("Transform:", tr), paste("Load:", outs))
  rin <- c(src$rows, rep(sum(src$rows, na.rm = TRUE), nt), rep(NA, length(outs)))
  tagList(
    ej_card(title, p, tags$p(class = "note", "Green sources are live in the platform; gold sources are planned and needed to track this work in the field.")),
    tags$div(
      class = "split",
      ej_card("Sources",
              tags$table(class = "hv-table ej-table", tags$thead(tags$tr(tags$th("Source"), tags$th("Where"), tags$th("Rows"), tags$th("Refresh"), tags$th("Status"))),
                         tags$tbody(lapply(seq_len(nrow(src)), function(i) tags$tr(
                           tags$td(src$source[i]), tags$td(class = "ej-small", src$where[i]), tags$td(if (is.finite(src$rows[i])) fmt_num(src$rows[i], 0) else "\u2014"),
                           tags$td(src$refresh[i]), tags$td(tags$span(class = paste("ej-badge", if (src$status[i] == "live") "is-live" else "is-plan"), src$status[i]))
                         ))))),
      ej_card("Run log (ELT: load raw, transform in R)",
              tags$table(class = "hv-table ej-table", tags$thead(tags$tr(tags$th("Step"), tags$th("Rows in"), tags$th("Ran"))),
                         tags$tbody(lapply(seq_along(steps), function(i) tags$tr(tags$td(class = "ej-small", steps[i]),
                                                                                  tags$td(if (is.finite(rin[i])) fmt_num(rin[i], 0) else "\u2014"),
                                                                                  tags$td(format(now - (length(steps) - i) * 7, "%H:%M:%S")))))))
    )
  )
}
ejo_alert <- function(tone, title, text) tags$div(class = paste("ej-alert", tone), tags$b(title), tags$p(text))

ejo_ml_ui <- function(k, ml, plan_gain = NULL, title_extra = "") {
  h <- ml$hist
  fc <- ml$fc
  p <- plotly::plot_ly() |>
    plotly::add_trace(x = c(fc$year, rev(fc$year)), y = c(fc$hi, rev(fc$lo)), type = "scatter", mode = "lines", fill = "toself",
                      fillcolor = "rgba(155,34,38,0.15)", line = list(color = "rgba(0,0,0,0)"), name = "80% prediction band", hoverinfo = "skip") |>
    plotly::add_trace(x = h$year, y = h$v, type = "scatter", mode = "markers+lines", name = "FAO-based loss (history)", marker = list(color = "#6b7280"), line = list(color = "#9ca3af")) |>
    plotly::add_trace(x = fc$year, y = fc$fit, type = "scatter", mode = "lines+markers", name = paste("Forecast:", ml$best), line = list(color = ejo_red, width = 3), marker = list(color = ejo_red))
  if (!is.null(plan_gain)) {
    pg <- plan_gain[plan_gain$year %in% fc$year, ]
    still <- fc$fit[match(pg$year, fc$year)] * (1 - pg$effect)
    p <- plotly::add_trace(p, x = pg$year, y = still, type = "scatter", mode = "lines+markers", name = "With the plan", line = list(color = ejo_blue, width = 3, dash = "dash"), marker = list(color = ejo_blue))
  }
  p <- p |> plotly::layout(legend = list(orientation = "h", y = -0.2)) |> ejo_plot()
  cv <- ml$cv
  tagList(
    tags$div(
      class = "split",
      ej_card(paste0("Loss forecast for ", ejo_causes[[k]], title_extra), p),
      ej_card("Model selection (rolling-origin cross-validation, last 6 years)",
              tags$table(class = "hv-table ej-table", tags$thead(tags$tr(tags$th("Model"), tags$th("RMSE (bn RWF)"), tags$th("MAPE"), tags$th("Tests"))),
                         tags$tbody(lapply(seq_len(nrow(cv)), function(i) tags$tr(class = if (cv$model[i] == ml$best) "is-on" else NULL,
                                                                                   tags$td(if (cv$model[i] == ml$best) tags$b(cv$model[i]) else cv$model[i]),
                                                                                   tags$td(fmt_num(cv$rmse[i], 2)), tags$td(paste0(fmt_num(cv$mape[i], 1), "%")), tags$td(cv$n[i]))))),
              tags$p(class = "note", "Each model is trained on the years before a test year and scored on that year, six times. The lowest error wins and is refitted on 1996\u20132023."))
    )
  )
}

# ---- one activity of a cause ----
ejo_activity_detail <- function(k, id, fy, ids) {
  cp <- ejo_cpm(k)
  pl <- cp$plan
  a <- pl[pl$id == id, , drop = FALSE]
  if (!nrow(a)) return(hv_empty("Activity not found."))
  mc <- ejo_mc(k)
  i <- match(id, pl$id)
  g <- ejo_gain(k, ids)
  ml <- ejo_ml(k)
  fy <- as.integer(fy)
  gf <- g[g$year == fy, ]
  cost <- a$cost_m * 1e6
  share_cost <- a$cost_m / sum(pl$cost_m)
  gain_m <- sum(g$gain) / 72
  ef_sim <- mc$EF[, i]
  p_on <- mean(ef_sim <= a$lf + 1e-9)
  p_e <- mean(mc$E <= cp$E)
  crit_idx <- mc$crit[[id]]
  stake_m <- if (a$critical) gain_m else 0

  months <- seq(0, ceiling(max(mc$E)), by = 0.5)
  pdone <- vapply(months, function(t) mean(ef_sim <= t), numeric(1))
  spend <- pmin(1, pmax(0, (months - a$es) / max(a$dur, 0.1))) * cost
  dash <- plotly::plot_ly() |>
    plotly::add_trace(x = months, y = 100 * pdone, type = "scatter", mode = "lines", name = "Chance it is finished (%)", line = list(color = ejo_blue, width = 3)) |>
    plotly::add_trace(x = months, y = spend / 1e6, type = "scatter", mode = "lines", name = "Planned spend (m RWF)", yaxis = "y2", line = list(color = ejo_green, width = 3, dash = "dot")) |>
    plotly::layout(shapes = list(list(type = "line", x0 = a$ef, x1 = a$ef, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#111827", dash = "dash")),
                                 list(type = "line", x0 = a$lf, x1 = a$lf, y0 = 0, y1 = 1, yref = "paper", line = list(color = ejo_red, dash = "dot"))),
                   yaxis2 = list(overlaying = "y", side = "right", title = "m RWF", showgrid = FALSE),
                   legend = list(orientation = "h", y = -0.22)) |>
    hv_plot_layout("Chance finished (%)", "Months from plan start")
  money <- plotly::plot_ly() |>
    plotly::add_bars(x = g$year, y = g$loss / 1e9, name = "Lost if nothing changes", marker = list(color = ejo_red)) |>
    plotly::add_bars(x = g$year, y = g$gain / 1e9, name = "Gained with the plan", marker = list(color = ejo_blue)) |>
    plotly::add_bars(x = g$year, y = g$cost / 1e9, name = "Plan cost", marker = list(color = ejo_green)) |>
    plotly::layout(barmode = "group", legend = list(orientation = "h", y = -0.22)) |>
    ejo_plot()
  hist <- plotly::plot_ly(x = mc$E, type = "histogram", nbinsx = 40, marker = list(color = "rgba(31,95,209,0.75)"), name = "Plan finish") |>
    plotly::layout(shapes = list(list(type = "line", x0 = cp$E, x1 = cp$E, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#111827", dash = "dash")))) |>
    hv_plot_layout("Simulated plans", "Months until the whole plan is done")
  crit_p <- plotly::plot_ly(x = names(mc$crit), y = 100 * mc$crit, type = "bar", marker = list(color = ifelse(names(mc$crit) == id, ejo_blue, "#9fb7e6")),
                            text = paste0(fmt_num(100 * mc$crit, 0), "%"), textposition = "outside") |>
    hv_plot_layout("Criticality index (%)", "Activity")

  w <- ejo_cause_weights(k)
  ret <- sum(g$gain) / max(cp$cost * 1e6, 1)
  al <- ejo_alloc_lp(w, a$cost_m, ret)

  src <- ejo_sources(k, a$activity)
  pred <- pl$id[vapply(seq_len(nrow(pl)), function(j) id %in% strsplit(ifelse(is.na(pl$after[j]), "", pl$after[j]), ";")[[1]], logical(1))]
  growth <- if (nrow(gf)) 100 * (gf$loss / max(ejo_cy$total[ejo_cy$cause == k & ejo_cy$year == 2026], 1) - 1) else NA

  alerts <- list(
    if (a$critical) ejo_alert("is-red", "On the critical path",
                              paste0("Every month this slips pushes the whole ", ejo_causes[[k]], " plan back a month and leaves about ", fmt_bn(gain_m * 1e9), " of gain unrealised.")) else
      ejo_alert("is-blue", paste(fmt_num(a$slack, 1), "months of slack"), paste0("It can start as late as month ", fmt_num(a$ls, 1), " without delaying the plan.")),
    ejo_alert(if (p_on >= 0.8) "is-blue" else "is-red", paste0(fmt_num(100 * p_on, 0), "% chance of finishing on time"),
              paste0("Monte Carlo of ", format(length(ef_sim), big.mark = ","), " plans: it finishes by month ", fmt_num(stats::quantile(ef_sim, 0.8), 1), " in 4 of 5 cases (latest allowed: ", fmt_num(a$lf, 1), ").")),
    ejo_alert("is-green", paste0(fmt_num(100 * share_cost, 0), "% of the plan budget"), paste0(fmt_bn(cost), " of ", fmt_bn(cp$cost * 1e6), ". Spend starts in month ", fmt_num(a$es, 1), ".")),
    if (is.finite(growth)) ejo_alert("is-red", paste0("Loss grows ", fmt_num(growth, 0), "% by ", fy), paste0("Without action, ", ejo_causes[[k]], " costs ", fmt_bn(gf$loss), " in ", fy, " (", ml$best, " forecast).")),
    if (length(pred)) ejo_alert("is-amber", paste("Unblocks", paste(pred, collapse = ", ")), "These activities cannot start until this one is done.")
  )
  alerts <- Filter(Negate(is.null), alerts)
  summ <- paste0(
    a$activity, " (", a$lead, ") takes about ", fmt_num(a$dur, 1), " months, from month ", fmt_num(a$es, 1), " to ", fmt_num(a$ef, 1), ", and costs ", fmt_bn(cost), ". ",
    if (a$critical) "It sits on the critical path. " else paste0("It has ", fmt_num(a$slack, 1), " months of slack. "),
    "In ", fmt_num(100 * crit_idx, 0), "% of simulated plans it was critical. ",
    "The plan it belongs to brings back ", fmt_bn(sum(g$gain)), " over 2026\u20132031 for ", fmt_bn(sum(g$cost)), " of cost."
  )
  payload <- list(
    cause = k, cause_label = ejo_causes[[k]], activity = list(id = id, name = a$activity, lead = a$lead, after = if (is.na(a$after)) list() else strsplit(a$after, ";")[[1]]),
    schedule_months = list(expected = round(a$dur, 2), start = round(a$es, 2), finish = round(a$ef, 2), slack = round(a$slack, 2), critical = a$critical,
                           p_on_time = round(p_on, 3), criticality_index = round(crit_idx, 3)),
    cost_rwf = cost,
    money_rwf = list(year = fy, lost_if_nothing = round(gf$loss), gained_with_plan = round(gf$gain), plan_cost = round(gf$cost)),
    forecast = list(model = ml$best, years = ml$fc$year, loss_bn = round(ml$fc$fit, 3)),
    allocation_top5 = if (!is.null(al)) lapply(seq_len(min(5, nrow(al$d))), function(j) list(district = al$d$district[j], rwf_m = round(al$d$alloc[j], 1))) else list()
  )
  slug <- paste0("/api/v1/ejo/causes/", k, "/activities/", id)

  tagList(
    tags$div(
      class = "ej-detail-head",
      tags$p(class = "kicker", paste(toupper(ejo_causes[[k]]), "\u00b7 ACTIVITY", id)),
      tags$h2(a$activity),
      tags$p(class = "ej-lead", "Who leads: ", tags$b(a$lead), if (!is.na(a$after) && nzchar(a$after)) paste0(" \u00b7 starts after ", gsub(";", ", ", a$after)) else " \u00b7 starts first")
    ),
    ej_dsec("1", "Data visualisation: dashboard and report", "Where the activity stands, what it costs (green) and what the plan it serves wins back (blue) against what keeps being lost (red).",
            ej_kpis(
              hv_kpi("Expected duration", paste(fmt_num(a$dur, 1), "months"), paste0("PERT from ", a$opt, " / ", a$likely, " / ", a$pess, " months")),
              hv_kpi("Window", paste0("Month ", fmt_num(a$es, 1), "\u2013", fmt_num(a$ef, 1)), if (a$critical) "Critical path" else paste("Latest finish month", fmt_num(a$lf, 1))),
              hv_kpi("Activity cost", fmt_bn(cost), paste0(fmt_num(100 * share_cost, 0), "% of the plan budget"), "is-green"),
              hv_kpi(paste("Lost in", fy, "if nothing changes"), fmt_bn(gf$loss), ejo_causes[[k]], "is-red"),
              hv_kpi(paste("Gained in", fy), fmt_bn(gf$gain), "With the options ticked for this cause", "is-blue"),
              hv_kpi("Gain at risk per month of delay", fmt_bn(stake_m * 1e9), if (a$critical) "Critical: delay moves the whole plan" else "Zero while slack lasts", if (a$critical) "is-red" else NULL)
            ),
            tags$div(class = "split", ej_card("Progress: chance finished and planned spend", dash), ej_card(paste(ejo_causes[[k]], "2026\u20132031: lost, gained, cost"), money)),
            ej_card("Report: month-by-month plan",
                    tags$table(class = "hv-table ej-table",
                               tags$thead(tags$tr(tags$th("Month"), tags$th("Chance finished"), tags$th("Planned spend"), tags$th("Status"))),
                               tags$tbody(lapply(which(months %% 1 == 0 & months <= ceiling(a$lf) + 1), function(j) tags$tr(
                                 tags$td(months[j]), tags$td(paste0(fmt_num(100 * pdone[j], 0), "%")), tags$td(class = "ej-green", fmt_bn(spend[j])),
                                 tags$td(if (months[j] < a$es) "waiting for predecessors" else if (months[j] <= a$ef) "in progress" else if (months[j] <= a$lf) "should be done (slack)" else "late if not done")
                               )))))
    ),
    ej_dsec("2", "Predictive models (machine learning): will the plan deliver?", "A model chosen by cross-validation forecasts this cause's loss; a Monte Carlo of the whole action plan predicts when it is done and how often this activity decides the finish date.",
            ejo_ml_ui(k, ml, g),
            ej_kpis(
              hv_kpi("Chance on time", paste0(fmt_num(100 * p_on, 0), "%"), "Finishes before its latest finish", if (p_on >= 0.8) "is-blue" else "is-red"),
              hv_kpi("Criticality index", paste0(fmt_num(100 * crit_idx, 0), "%"), "Share of simulated plans where it is critical"),
              hv_kpi("Plan done (80% sure)", paste(fmt_num(stats::quantile(mc$E, 0.8), 1), "months"), paste0("Median ", fmt_num(stats::median(mc$E), 1), " months")),
              hv_kpi(paste("Loss forecast", fy), paste0(fmt_num(ml$fc$fit[ml$fc$year == fy], 1), " bn RWF"), paste0("80% band ", fmt_num(ml$fc$lo[ml$fc$year == fy], 1), "\u2013", fmt_num(ml$fc$hi[ml$fc$year == fy], 1)), "is-red")
            ),
            tags$div(class = "split", ej_card("Monte Carlo: when the whole plan finishes", hist), ej_card("How often each activity is critical", crit_p))
    ),
    ej_dsec("3", "Data pipelines (ETL / ELT)", "What feeds this activity, how it is transformed and where the results land. Planned feeds are what field teams must start sending.",
            ejo_pipeline_ui(k, src, paste("Data flow for", a$activity))
    ),
    ej_dsec("4", "Automated insights: alerts and summary", "Rules run on every refresh and raise these alerts; the summary is written from the same numbers.",
            tags$div(class = "ej-alerts", alerts),
            ej_card("Auto-summary", tags$p(class = "ej-summary", summ))
    ),
    ej_dsec("5", "Data APIs (integration)", "How ministries, districts and partners read and write this activity's data.",
            ejo_api_block(list(c("GET", slug, "Schedule, cost, risk and money for this activity"),
                               c("GET", paste0("/api/v1/ejo/causes/", k, "/loss?from=1996&to=2031"), "Money lost by year (farmers, government)"),
                               c("POST", paste0(slug, "/progress"), "Field team reports % done, spend and date"),
                               c("POST", "/api/v1/ejo/alerts/subscribe", "Webhook or SMS when an alert fires")),
                          payload, ejo_consumes(k))
    ),
    ej_dsec("6", "Decision optimisation (prescriptive analytics)", "A linear program spreads this activity's budget over the 30 districts to avoid the most loss: districts near markets get more per franc, each district gets at least a quarter of its fair share, and returns fall after the first tranche.",
            if (is.null(al)) hv_empty("The allocation model could not be solved.") else tagList(
              ej_kpis(
                hv_kpi("Budget spread", fmt_bn(cost), "Over 30 districts", "is-green"),
                hv_kpi("Loss avoided by this spend", fmt_bn(al$total * 1e6), "Attributed share of the plan's gain", "is-blue"),
                hv_kpi("Extra 1 m RWF would avoid", fmt_bn(al$dual * 1e6), "Shadow price of the budget", "is-blue"),
                hv_kpi("First districts", paste(utils::head(al$d$district, 3), collapse = ", "), "Largest allocations")
              ),
              tags$div(
                class = "split",
                ej_card("Allocation by district (m RWF)",
                        plotly::plot_ly(y = rev(utils::head(al$d$district, 12)), x = rev(utils::head(al$d$alloc, 12)), type = "bar", orientation = "h", marker = list(color = ejo_green),
                                        text = paste0(fmt_num(rev(utils::head(al$d$alloc, 12)), 1), " m"), textposition = "auto") |>
                          plotly::layout(yaxis = list(categoryorder = "array", categoryarray = rev(utils::head(al$d$district, 12)), automargin = TRUE)) |>
                          hv_plot_layout(NULL, "m RWF")),
                ej_card("Where the money goes",
                        if (has_leaflet) {
                          v <- setNames(al$d$alloc, al$d$district)
                          pal <- leaflet::colorNumeric(c("#eef4ff", "#7aa7f0", "#1f5fd1"), domain = c(0, max(v)))
                          hv_fill(hv_base_map(), v, pal, function(d, x) paste0("<b>", d, "</b><br>", fmt_num(x, 1), " m RWF"))
                        })
              ),
              ej_card("Allocation table",
                      tags$table(class = "hv-table ej-table", tags$thead(tags$tr(tags$th("District"), tags$th("Share of the problem"), tags$th("Road km to markets"), tags$th("Allocation"), tags$th("Loss avoided"))),
                                 tags$tbody(lapply(seq_len(nrow(al$d)), function(j) tags$tr(
                                   tags$td(al$d$district[j]), tags$td(paste0(fmt_num(100 * al$d$share[j], 1), "%")), tags$td(fmt_num(al$d$km[j], 0)),
                                   tags$td(class = "ej-green", paste0(fmt_num(al$d$alloc[j], 1), " m RWF")), tags$td(class = "ej-blue", fmt_bn(al$d$avoided[j] * 1e6))
                                 )))))
            )
    )
  )
}

# ---- one option chosen in the national action plan ----
ejo_option_detail <- function(id, fy, chosen) {
  ap <- do.call(rbind, ejo_appraisals)
  o <- ap[ap$id == id, , drop = FALSE]
  if (!nrow(o)) return(hv_empty("Option not found."))
  k <- o$cause
  fy <- as.integer(fy)
  g <- ejo_gain(k, id)
  gf <- g[g$year == fy, ]
  ml <- ejo_ml(k)
  set.seed(31)
  n <- 4000
  ef <- o$effect * stats::runif(n, 0.7, 1.3)
  cx <- o$capex_bn * stats::runif(n, 0.85, 1.25)
  ox <- o$opex_bn * stats::runif(n, 0.85, 1.25)
  cy <- ejo_cy[ejo_cy$cause == k & ejo_cy$year %in% ejo_years, ]
  cy <- cy[order(cy$year), ]
  ad <- unname(ejo_adopt)
  dsc <- ejo_disc(cy$year)
  lf <- stats::rnorm(n, 1, 0.12)
  npv_sim <- vapply(seq_len(n), function(j) sum(cy$total * ef[j] * ad * dsc) * lf[j] - cx[j] * 1e9 - sum(ox[j] * 1e9 * ad * dsc), numeric(1))
  p_pos <- mean(npv_sim > 0)

  budgets <- seq(5, 150, by = 5)
  sweep <- do.call(rbind, lapply(budgets, function(b) {
    ids <- ejo_knapsack(ap, b)
    data.frame(budget = b, npv = sum(ap$npv[ap$id %in% ids]) / 1e9, inside = id %in% ids)
  }))
  without <- ap[ap$id != id, ]
  bud <- if (length(chosen)) max(60, sum(ap$capex_bn[ap$id %in% chosen])) else 60
  with_ids <- ejo_knapsack(ap, bud)
  wo_ids <- ejo_knapsack(without, bud)
  marg <- sum(ap$npv[ap$id %in% with_ids]) - sum(without$npv[without$id %in% wo_ids])

  money <- plotly::plot_ly() |>
    plotly::add_bars(x = g$year, y = g$loss / 1e9, name = paste("Lost:", ejo_causes[[k]]), marker = list(color = ejo_red)) |>
    plotly::add_bars(x = g$year, y = g$gain / 1e9, name = "Gained by this option", marker = list(color = ejo_blue)) |>
    plotly::add_bars(x = g$year, y = g$cost / 1e9, name = "Its cost", marker = list(color = ejo_green)) |>
    plotly::layout(barmode = "group", legend = list(orientation = "h", y = -0.22)) |>
    ejo_plot()
  cum <- plotly::plot_ly() |>
    plotly::add_trace(x = g$year, y = cumsum(g$gain) / 1e9, type = "scatter", mode = "lines+markers", name = "Cumulative gain", line = list(color = ejo_blue, width = 3)) |>
    plotly::add_trace(x = g$year, y = cumsum(g$cost) / 1e9, type = "scatter", mode = "lines+markers", name = "Cumulative cost", line = list(color = ejo_green, width = 3)) |>
    plotly::layout(legend = list(orientation = "h", y = -0.22)) |>
    ejo_plot()
  hist <- plotly::plot_ly(x = npv_sim / 1e9, type = "histogram", nbinsx = 50, marker = list(color = "rgba(31,95,209,0.75)")) |>
    plotly::layout(shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, yref = "paper", line = list(color = ejo_red, dash = "dash")))) |>
    hv_plot_layout("Simulations", "Net present value, bn RWF")
  sw <- plotly::plot_ly() |>
    plotly::add_trace(x = sweep$budget, y = sweep$npv, type = "scatter", mode = "lines", name = "Best portfolio NPV", line = list(color = "#1b4332", width = 2)) |>
    plotly::add_trace(x = sweep$budget[sweep$inside], y = sweep$npv[sweep$inside], type = "scatter", mode = "markers", name = paste(id, "chosen"), marker = list(color = ejo_blue, size = 9)) |>
    plotly::add_trace(x = sweep$budget[!sweep$inside], y = sweep$npv[!sweep$inside], type = "scatter", mode = "markers", name = paste(id, "left out"), marker = list(color = "#d1d5db", size = 9)) |>
    plotly::layout(legend = list(orientation = "h", y = -0.22)) |>
    hv_plot_layout("Portfolio NPV, bn RWF", "Capital budget, bn RWF")

  src <- ejo_sources(k, o$option)
  enters <- if (any(sweep$inside)) min(sweep$budget[sweep$inside]) else NA
  alerts <- Filter(Negate(is.null), list(
    ejo_alert(if (o$npv > 0) "is-blue" else "is-red", paste0("Net present value ", fmt_bn(o$npv)), paste0("Benefit / cost ", fmt_num(o$bcr, 1), "\u00d7 at ", fmt_num(100 * ejo_p("discount_rate"), 0), "% a year.")),
    ejo_alert(if (p_pos >= 0.9) "is-blue" else "is-red", paste0(fmt_num(100 * p_pos, 0), "% chance it pays"), "Monte Carlo with effect \u00b130%, costs \u221215% to +25% and loss \u00b112%."),
    if (is.finite(enters)) ejo_alert("is-green", paste("Enters the plan from", enters, "bn RWF"), "Below that budget the knapsack prefers other options."),
    ejo_alert("is-blue", paste0("Worth ", fmt_bn(marg), " to the portfolio"), "Best plan NPV with this option minus the best plan without it, same budget."),
    if (is.finite(o$payback)) ejo_alert("is-blue", paste("Pays back in", o$payback), "Cumulative gain passes cumulative cost.") else ejo_alert("is-red", "Does not pay back by 2031", "Consider a smaller roll-out or a cheaper design.")
  ))
  summ <- paste0(o$option, " (", o$lead, ") cuts ", ejo_causes[[k]], " loss by ", fmt_num(100 * o$effect, 0), "% at full roll-out. ",
                 "It costs ", fmt_num(o$capex_bn, 1), " bn RWF up front and ", fmt_num(o$opex_bn, 1), " bn RWF a year, and brings back ", fmt_bn(sum(g$gain)), " over 2026\u20132031 (", fmt_bn(gf$gain), " in ", fy, "). ",
                 "In ", fmt_num(100 * p_pos, 0), "% of simulated futures it pays for itself.")
  payload <- list(option = list(id = id, name = o$option, cause = k, lead = o$lead), capex_bn_rwf = o$capex_bn, opex_bn_rwf_per_year = o$opex_bn, effect = o$effect,
                  appraisal = list(gain_rwf = round(o$gain), npv_rwf = round(o$npv), bcr = round(o$bcr, 2), payback = o$payback, p_npv_positive = round(p_pos, 3)),
                  by_year = lapply(seq_len(nrow(g)), function(j) list(year = g$year[j], lost = round(g$loss[j]), gained = round(g$gain[j]), cost = round(g$cost[j]))))
  slug <- paste0("/api/v1/ejo/options/", id)

  tagList(
    tags$div(
      class = "ej-detail-head",
      tags$p(class = "kicker", paste("ACTION PLAN \u00b7", toupper(ejo_causes[[k]]), "\u00b7 OPTION", id)),
      tags$h2(o$option),
      tags$p(class = "ej-lead", "Who leads: ", tags$b(o$lead), paste0(" \u00b7 ", o$basis))
    ),
    ej_dsec("1", "Data visualisation: dashboard and report", "What the option costs (green), what it wins back (blue) and what the cause keeps losing (red).",
            ej_kpis(
              hv_kpi("Capital", paste0(fmt_num(o$capex_bn, 1), " bn RWF"), "Spent in 2026", "is-green"),
              hv_kpi("Running cost", paste0(fmt_num(o$opex_bn, 1), " bn RWF a year"), "Scaled by roll-out", "is-green"),
              hv_kpi(paste("Lost in", fy, "if nothing changes"), fmt_bn(gf$loss), ejo_causes[[k]], "is-red"),
              hv_kpi(paste("Gained in", fy), fmt_bn(gf$gain), paste0(fmt_num(100 * o$effect, 0), "% effect \u00d7 ", fmt_num(100 * ejo_adopt[[as.character(fy)]], 0), "% roll-out"), "is-blue"),
              hv_kpi("Gained 2026\u20132031", fmt_bn(sum(g$gain)), paste("Benefit / cost", fmt_num(o$bcr, 1), "\u00d7"), "is-blue"),
              hv_kpi("Net present value", fmt_bn(o$npv), if (is.finite(o$payback)) paste("Pays back in", o$payback) else "After 2031", if (o$npv >= 0) "is-blue" else "is-red")
            ),
            tags$div(class = "split", ej_card("Each year: lost, gained, cost", money), ej_card("Cumulative gain against cumulative cost", cum)),
            ejo_year_table(g)
    ),
    ej_dsec("2", "Predictive models (machine learning): will it deliver?", "The cause's loss is forecast by the model that won cross-validation; the option's path is drawn on top, and a Monte Carlo tests the investment under uncertain effect, cost and loss.",
            ejo_ml_ui(k, ml, g, paste(" with", id)),
            ej_kpis(
              hv_kpi("Chance it pays", paste0(fmt_num(100 * p_pos, 0), "%"), "NPV above zero", if (p_pos >= 0.9) "is-blue" else "is-red"),
              hv_kpi("NPV, bad case (10%)", fmt_bn(stats::quantile(npv_sim, 0.1)), "1 in 10 futures are worse", if (stats::quantile(npv_sim, 0.1) >= 0) "is-blue" else "is-red"),
              hv_kpi("NPV, median", fmt_bn(stats::median(npv_sim)), "Half the futures are better", "is-blue")
            ),
            ej_card("Monte Carlo: net present value of the option", hist)
    ),
    ej_dsec("3", "Data pipelines (ETL / ELT)", "Feeds that measure this option and the transformations that turn them into money lost and won.",
            ejo_pipeline_ui(k, src, paste("Data flow for", o$option))
    ),
    ej_dsec("4", "Automated insights: alerts and summary", "Rules run on every refresh; the summary is written from the same numbers.",
            tags$div(class = "ej-alerts", alerts),
            ej_card("Auto-summary", tags$p(class = "ej-summary", summ))
    ),
    ej_dsec("5", "Data APIs (integration)", "How partners read the appraisal and report delivery.",
            ejo_api_block(list(c("GET", slug, "Costs, effect, appraisal and money by year"),
                               c("GET", "/api/v1/ejo/plan?budget_bn=60", "The knapsack plan for a budget"),
                               c("POST", paste0(slug, "/delivery"), "Units delivered, spend and districts reached"),
                               c("POST", "/api/v1/ejo/alerts/subscribe", "Webhook or SMS when an alert fires")),
                          payload, ejo_consumes(k))
    ),
    ej_dsec("6", "Decision optimisation (prescriptive analytics)", "The 0-1 knapsack is solved again for every budget from 5 to 150 bn RWF: blue dots are budgets where this option makes the best plan.",
            ej_kpis(
              hv_kpi("Enters the plan from", if (is.finite(enters)) paste(enters, "bn RWF") else "never", "Smallest budget that picks it", "is-green"),
              hv_kpi("Value to the portfolio", fmt_bn(marg), paste("At a", bud, "bn RWF budget"), if (marg >= 0) "is-blue" else "is-red"),
              hv_kpi("Rank by NPV", paste(match(id, ap$id[order(-ap$npv)]), "of", nrow(ap)), "Among all 30 options")
            ),
            ej_card("Best plan for each budget", sw)
    )
  )
}
