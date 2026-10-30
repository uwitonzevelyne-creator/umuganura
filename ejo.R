# Ejo hazaza: post-harvest loss, the money lost, and operations-research answers, 2026-2031

ejo_dir <- file.path("dataset hachthon", "EXTERNAL", "ejo_hazaza")
ejo_read <- function(f) {
  p <- file.path(ejo_dir, f)
  if (file.exists(p)) utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8") else NULL
}
ejo_par_tbl <- ejo_read("ejo_model_parameters.csv")
ejo_par <- setNames(as.numeric(ejo_par_tbl$value), ejo_par_tbl$key)
ejo_p <- function(k) unname(ejo_par[[k]])
ejo_shares <- ejo_read("ejo_cause_shares.csv")
ejo_options <- ejo_read("ejo_options.csv")
ejo_plans <- ejo_read("ejo_action_plans.csv")
ejo_nodes <- ejo_read("ejo_nodes.csv")
ejo_matrix <- ejo_read("osrm_road_matrix.csv")
ejo_routes <- ejo_read("osrm_routes.csv")
ejo_power <- ejo_read("nasa_power_district_monthly.csv")
ejo_aphlis <- ejo_read("aphlis_rwanda_losses.csv")
ejo_diesel <- ejo_read("wb_rwanda_diesel_price.csv")

ejo_causes <- c(transport = "Transport routes", storage = "Drying and storage", weather = "Rain at harvest",
                cold = "Lack of Cooling", glut = "Too Much Supply", handling = "Handling and packaging")
ejo_or <- c(
  transport = "Transportation problem (linear programming) on real road distances",
  storage = "p-median facility location (integer programming)",
  weather = "Monte Carlo simulation and expected monetary value",
  cold = "M/M/c queueing model (Erlang C)",
  glut = "Multi-period inventory plan (linear programming)",
  handling = "Pareto analysis, break-even and PERT"
)
ejo_budget0 <- c(transport = 10, storage = 20, weather = 15, cold = 20, glut = 20, handling = 10)
ejo_years <- 2026:2031
ejo_adopt <- setNames(vapply(ejo_years, function(y) ejo_p(paste0("adoption_", y)), numeric(1)), ejo_years)
ejo_red <- "#9b2226"
ejo_red2 <- "#e85d04"
ejo_green <- "#1b4332"
ejo_blue <- "#1f5fd1"
ejo_blue2 <- "#8fb3f0"
ejo_reds <- c("#7f1d1d", "#b91c1c", "#dc2626", "#ef4444", "#f87171", "#fca5a5")
ejo_blues <- c("#1e3a8a", "#1d4ed8", "#2563eb", "#3b82f6", "#60a5fa", "#93c5fd")
names(ejo_reds) <- names(ejo_causes)
names(ejo_blues) <- names(ejo_causes)

fmt_bn <- function(x, unit = TRUE) {
  if (!length(x) || !is.finite(x)) return("\u2014")
  if (unit && abs(x) < 1e9) return(paste0(fmt_num(x / 1e6, if (abs(x) >= 1e7) 0 else 1), " m RWF"))
  v <- x / 1e9
  s <- fmt_num(v, if (abs(v) >= 100) 0 else if (abs(v) >= 10) 1 else 2)
  if (unit) paste0(s, " bn RWF") else s
}

# ---- products: FAO food balance items, their group and producer-price item ----
ejo_items <- data.frame(
  fbs = c("Maize and products", "Rice and products", "Sorghum and products", "Wheat and products",
          "Beans", "Peas", "Soyabeans", "Groundnuts",
          "Cassava and products", "Sweet potatoes", "Potatoes and products", "Roots, Other", "Yams",
          "Bananas", "Plantains",
          "Tomatoes and products", "Onions", "Vegetables, other",
          "Fruits, other", "Pineapples and products", "Lemons, Limes and products", "Oranges, Mandarines",
          "Milk - Excluding Butter", "Eggs",
          "Sugar cane", "Coffee and products", "Tea (including mate)"),
  label = c("Maize", "Rice", "Sorghum", "Wheat", "Beans", "Peas", "Soya bean", "Groundnut",
            "Cassava", "Sweet potato", "Irish potato", "Taro and other roots", "Yam",
            "Bananas", "Cooking banana", "Tomato", "Onion", "Other vegetables",
            "Other fruit", "Pineapple", "Lemon and lime", "Orange", "Milk", "Eggs",
            "Sugarcane", "Coffee", "Tea"),
  group = c(rep("cereals", 4), rep("pulses", 4), rep("roots", 5), rep("bananas", 2), rep("vegetables", 3),
            rep("fruits", 4), rep("milk", 2), rep("cash", 3)),
  price = c("Maize (corn)", "Rice", "Sorghum", "Wheat", "Beans, dry", "Peas, dry", "Soya beans", "Groundnuts, excluding shelled",
            "Cassava, fresh", "Sweet potatoes", "Potatoes", "Taro", "Yams", "Bananas", "Cooking bananas",
            "Tomatoes", "Onions and shallots, dry (excluding dehydrated)",
            "Cabbages|Carrots and turnips|Spinach|Other beans, green|Pumpkins, squash and gourds|Eggplants (aubergines)",
            "Avocados|Mangoes, guavas and mangosteens|Papayas", "Pineapples", "Lemons and limes", "Oranges",
            "Raw milk of cattle", "Hen eggs in shell, fresh", "Sugar cane", "Coffee, green", "Tea leaves"),
  stringsAsFactors = FALSE
)
ejo_group_label <- c(cereals = "Cereals", pulses = "Pulses and nuts", roots = "Roots and tubers", bananas = "Bananas",
                     vegetables = "Vegetables", fruits = "Fruits", milk = "Milk and eggs", cash = "Cash crops")
ejo_perish <- c(cereals = "durable", pulses = "durable", roots = "semi", bananas = "semi",
                vegetables = "perishable", fruits = "perishable", milk = "perishable", cash = "semi")

ejo_cpi <- local({
  d <- factor_data[factor_data$id == "cpi", c("year", "value"), drop = FALSE]
  d[order(d$year), , drop = FALSE]
})
ejo_infl <- local({
  d <- factor_data[factor_data$id == "inflation", c("year", "value"), drop = FALSE]
  d <- d[order(d$year), , drop = FALSE]
  v <- mean(utils::tail(d$value, 5), na.rm = TRUE) / 100
  if (is.finite(v)) v else 0.05
})
ejo_cpi_at <- function(y) {
  d <- ejo_cpi
  if (nrow(d) < 2) return((1 + ejo_infl)^(y - 2025))
  vapply(y, function(t) {
    if (t %in% d$year) return(d$value[d$year == t][1])
    if (t > max(d$year)) return(d$value[which.max(d$year)] * (1 + ejo_infl)^(t - max(d$year)))
    d$value[which.min(d$year)] / (1 + ejo_infl)^(min(d$year) - t)
  }, numeric(1))
}

ejo_price_at <- function(spec, years) {
  its <- strsplit(spec, "|", fixed = TRUE)[[1]]
  d <- crop_prices[crop_prices$item %in% its, , drop = FALSE]
  if (!nrow(d)) return(rep(NA_real_, length(years)))
  d <- stats::aggregate(value ~ year, d, stats::median)
  vapply(years, function(y) {
    v <- d$value[d$year == y]
    if (length(v)) return(v[1])
    ny <- d$year[which.min(abs(d$year - y))]
    d$value[d$year == ny][1] * ejo_cpi_at(y) / ejo_cpi_at(ny)
  }, numeric(1))
}

ejo_fbs <- local({
  p <- file.path(data_dir, "fao_rwanda_food_balances.csv")
  if (!file.exists(p)) return(data.frame(Item = character(), Element = character(), Year = integer(), Value = numeric()))
  fb <- utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
  fb <- fb[fb$`Item Code (FBS)` < 2900 & fb$Item %in% ejo_items$fbs & fb$Element %in% c("Production", "Losses") & is.finite(fb$Value),
           c("Item", "Element", "Year", "Value")]
  fb[!duplicated(fb[c("Item", "Element", "Year")]), , drop = FALSE]
})
# FAO food balances, old methodology (to 2013), spliced onto the new series by the 2010-2013 overlap ratio.
ejo_fbs_old <- local({
  d <- ejo_read("fao_fbs_historic_rwanda.csv")
  if (is.null(d)) return(NULL)
  ren <- c("Rice (Milled Equivalent)" = "Rice and products", "Groundnuts (Shelled Eq)" = "Groundnuts")
  d$Item <- ifelse(d$Item %in% names(ren), ren[d$Item], d$Item)
  d$Item <- ejo_items$fbs[match(tolower(d$Item), tolower(ejo_items$fbs))]
  d[!is.na(d$Item) & is.finite(d$Value), c("Item", "Element", "Year", "Value")]
})
ejo_hist_years <- 1996:2026
ejo_fc_years <- 2027:2031
ejo_all_years <- 1996:2031

# Loss by product and year: FAO food balance losses (old series spliced before 2010), then production trend x the 2019-2023 loss rate.
ejo_loss_items <- local({
  yrs <- ejo_all_years
  out <- list()
  splice <- function(new, old) {
    if (is.null(old) || !nrow(old)) return(new)
    ov <- merge(new, old, by = "Year")
    ov <- ov[ov$Value.y > 0, , drop = FALSE]
    r <- if (nrow(ov)) stats::median(ov$Value.x / ov$Value.y) else 1
    if (!is.finite(r) || r < 0.2 || r > 5) r <- 1
    early <- old[old$Year < min(new$Year), , drop = FALSE]
    early$Value <- early$Value * r
    rbind(early, new)
  }
  for (i in seq_len(nrow(ejo_items))) {
    it <- ejo_items[i, ]
    pr <- ejo_fbs[ejo_fbs$Item == it$fbs & ejo_fbs$Element == "Production", c("Year", "Value")]
    ls <- ejo_fbs[ejo_fbs$Item == it$fbs & ejo_fbs$Element == "Losses", c("Year", "Value")]
    if (nrow(pr) < 4 || !nrow(ls)) next
    rr <- merge(pr, ls, by = "Year")
    rate_early <- local({
      if (it$fbs == "Bananas") {
        b <- ejo_fbs[ejo_fbs$Item %in% c("Bananas", "Plantains") & ejo_fbs$Year >= 2014 & ejo_fbs$Year <= 2018, , drop = FALSE]
        pp <- sum(b$Value[b$Element == "Production"])
        if (pp > 0) return(sum(b$Value[b$Element == "Losses"]) / pp)
      }
      e <- rr[rr$Year >= 2014 & rr$Year <= 2018 & rr$Value.x > 0, , drop = FALSE]
      if (!nrow(e)) e <- rr[rr$Value.x > 0, , drop = FALSE]
      if (nrow(e)) stats::median(e$Value.y / e$Value.x) else NA_real_
    })
    rr <- rr[rr$Year >= 2019 & rr$Value.x > 0, , drop = FALSE]
    rate <- if (nrow(rr)) mean(rr$Value.y / rr$Value.x) else 0
    if (!is.finite(rate) || rate <= 0) next
    fit <- stats::lm(Value ~ Year, data = pr[pr$Year >= 2010, , drop = FALSE])
    lastp <- pr$Value[which.max(pr$Year)]
    old_pr <- if (!is.null(ejo_fbs_old)) ejo_fbs_old[ejo_fbs_old$Item == it$fbs & ejo_fbs_old$Element == "Production", c("Year", "Value")] else NULL
    old_ls <- if (!is.null(ejo_fbs_old)) ejo_fbs_old[ejo_fbs_old$Item == it$fbs & ejo_fbs_old$Element == "Losses", c("Year", "Value")] else NULL
    pr_all <- splice(pr, old_pr)
    ls_all <- splice(ls, old_ls)
    pf <- pmax(stats::predict(fit, newdata = data.frame(Year = yrs)), 0.5 * lastp)
    prod_t <- ifelse(yrs %in% pr_all$Year, pr_all$Value[match(yrs, pr_all$Year)], ifelse(yrs > max(pr$Year), pf, NA_real_)) * 1000
    from_rate <- !(yrs %in% ls_all$Year) & yrs < min(ls$Year) & is.finite(prod_t) & is.finite(rate_early)
    loss_t <- ifelse(yrs %in% ls_all$Year, ls_all$Value[match(yrs, ls_all$Year)] * 1000,
                     ifelse(yrs > max(ls$Year), prod_t * rate, ifelse(from_rate, prod_t * rate_early, NA_real_)))
    kind <- ifelse(yrs %in% ls$Year, "FAO food balance",
                   ifelse(yrs %in% ls_all$Year, "FAO food balance (old method)",
                          ifelse(from_rate, "Estimate: production \u00d7 2014\u201318 loss rate",
                                 ifelse(yrs >= 2027, "Forecast", "Estimate: production trend \u00d7 loss rate"))))
    brk <- yrs < 2014 & is.finite(prod_t) & prod_t > 0 & is.finite(loss_t) & is.finite(rate_early) & rate_early > 0 &
      (loss_t / prod_t > 2 * rate_early | loss_t / prod_t < rate_early / 2)
    loss_t[brk] <- prod_t[brk] * rate_early
    kind[brk] <- "Harmonised: production \u00d7 2014\u201318 loss rate (FAO method break)"
    pt <- ejo_price_at(it$price, yrs)
    if (it$fbs == "Bananas") {
      b <- ejo_fbs[ejo_fbs$Item %in% c("Bananas", "Plantains") & ejo_fbs$Element == "Production" & ejo_fbs$Year >= 2014 & ejo_fbs$Year <= 2018, , drop = FALSE]
      wb <- sum(b$Value[b$Item == "Bananas"]) / max(sum(b$Value), 1)
      pc <- ejo_price_at("Cooking bananas", yrs)
      early <- yrs < 2014 & is.finite(pc)
      pt[early] <- wb * pt[early] + (1 - wb) * pc[early]
    }
    out[[length(out) + 1]] <- data.frame(
      item = it$label, fbs = it$fbs, group = it$group, year = yrs, kind = kind,
      prod_t = prod_t, loss_t = loss_t, rate = rate, price_t = pt,
      stringsAsFactors = FALSE
    )
  }
  d <- do.call(rbind, out)
  d <- d[is.finite(d$loss_t), , drop = FALSE]
  ok <- is.finite(d$price_t)
  gm <- stats::aggregate(price_t ~ group + year, d[ok, , drop = FALSE], stats::median)
  miss <- !ok
  d$price_t[miss] <- gm$price_t[match(paste(d$group[miss], d$year[miss]), paste(gm$group, gm$year))]
  d$price_t[!is.finite(d$price_t)] <- stats::median(d$price_t, na.rm = TRUE)
  d$farmer <- d$loss_t * d$price_t
  d
})

# Cause shares: CSV assumptions, with cereals replaced by APHLIS value-chain steps for Rwanda.
ejo_aphlis_steps <- local({
  if (is.null(ejo_aphlis)) return(NULL)
  v <- ejo_aphlis[ejo_aphlis$table == "value-chain", , drop = FALSE]
  if (!nrow(v)) return(NULL)
  v
})
ejo_share_mat <- local({
  m <- as.matrix(ejo_shares[, names(ejo_causes)])
  rownames(m) <- ejo_shares$group
  if (!is.null(ejo_aphlis_steps)) {
    st <- tapply(ejo_aphlis_steps$value, ejo_aphlis_steps$column, mean, na.rm = TRUE)
    g <- function(p) sum(st[grepl(p, names(st), ignore.case = TRUE)], na.rm = TRUE)
    w <- c(transport = g("transport"), storage = g("storage"), weather = g("harvesting|drying"),
           cold = 0, glut = 0, handling = g("thresh|winnow"))
    if (sum(w) > 0) m["cereals", ] <- w / sum(w)
  }
  m / rowSums(m)
})

# Government share: public spending on agriculture per franc of harvest, plus export earnings lost to cold-chain breaks.
ejo_gov_ratio <- local({
  r <- NA_real_
  fx15 <- fx_rwf$value[fx_rwf$year == 2015]
  if (nrow(ag_spend) && length(fx15)) {
    pv <- stats::aggregate(v ~ year, data.frame(year = ejo_loss_items$year, v = ejo_loss_items$prod_t * ejo_loss_items$price_t), sum)
    sp <- ag_spend[ag_spend$year %in% pv$year & ag_spend$year <= 2023, , drop = FALSE]
    if (nrow(sp)) {
      rwf <- sp$value * 1e6 * fx15[1] * ejo_cpi_at(sp$year) / ejo_cpi_at(2015)
      r <- stats::median(utils::tail(rwf / pv$v[match(sp$year, pv$year)], 5))
    }
  }
  if (is.finite(r) && r > 0) r else 0.04
})
ejo_hort_series <- local({
  h <- naeb_export_year("Horticulture")
  h <- h[is.finite(h$value), c("year", "value"), drop = FALSE]
  if (nrow(h) && max(h$value) < 1e5) h$value <- h$value * 1e6
  h
})
ejo_hort_usd <- if (nrow(ejo_hort_series)) list(usd = ejo_hort_series$value[which.max(ejo_hort_series$year)], year = max(ejo_hort_series$year)) else list(usd = NA_real_, year = NA_integer_)
ejo_fx_now <- if (nrow(fx_rwf)) fx_rwf$value[which.max(fx_rwf$year)] else NA_real_
ejo_fx_loss <- function(years) {
  if (!is.finite(ejo_hort_usd$usd) || !is.finite(ejo_fx_now)) return(rep(0, length(years)))
  vapply(years, function(y) {
    if (y < min(ejo_hort_series$year)) return(0)
    fx <- fx_rwf$value[fx_rwf$year == y]
    fx <- if (length(fx)) fx[1] else ejo_fx_now
    if (y %in% ejo_hort_series$year) return(ejo_hort_series$value[ejo_hort_series$year == y][1] * fx * ejo_p("hort_export_loss_share"))
    ejo_hort_usd$usd * ejo_fx_now * ejo_p("hort_export_loss_share") * ejo_cpi_at(y) / ejo_cpi_at(ejo_hort_usd$year)
  }, numeric(1))
}

ejo_cy <- local({
  d <- ejo_loss_items
  out <- lapply(names(ejo_causes), function(k) {
    sh <- ejo_share_mat[d$group, k]
    a <- stats::aggregate(cbind(farmer, tonnes) ~ year, data.frame(year = d$year, farmer = d$farmer * sh, tonnes = d$loss_t * sh), sum)
    a$gov <- a$farmer * ejo_gov_ratio + if (k == "cold") ejo_fx_loss(a$year) else 0
    a$total <- a$farmer + a$gov
    a$cause <- k
    a
  })
  do.call(rbind, out)
})

# ---- options, gains, appraisal and knapsack ----
ejo_effect <- function(ids, full = FALSE) {
  e <- ejo_options$effect[match(ids, ejo_options$id)]
  e <- e[is.finite(e)]
  if (full) return(if (length(e)) 1 - prod(1 - e) else 0)
  vapply(ejo_years, function(y) if (length(e)) 1 - prod(1 - e * ejo_adopt[[as.character(y)]]) else 0, numeric(1))
}
ejo_gain <- function(k, ids) {
  cy <- ejo_cy[ejo_cy$cause == k & ejo_cy$year %in% ejo_years, , drop = FALSE]
  cy <- cy[order(cy$year), , drop = FALSE]
  E <- ejo_effect(ids)
  op <- ejo_options[ejo_options$id %in% ids, , drop = FALSE]
  cost <- vapply(seq_along(ejo_years), function(i) (if (i == 1) sum(op$capex_bn) else 0) * 1e9 + sum(op$opex_bn) * 1e9 * ejo_adopt[[i]], numeric(1))
  data.frame(year = cy$year, farmer = cy$farmer, gov = cy$gov, loss = cy$total, tonnes = cy$tonnes,
             effect = E, gain = cy$total * E, gain_farmer = cy$farmer * E, gain_gov = cy$gov * E, cost = cost)
}
ejo_disc <- function(y) (1 + ejo_p("discount_rate"))^-(y - 2026)
ejo_appraise <- function(k) {
  op <- ejo_options[ejo_options$cause == k, , drop = FALSE]
  cy <- ejo_cy[ejo_cy$cause == k & ejo_cy$year %in% ejo_years, , drop = FALSE]
  cy <- cy[order(cy$year), , drop = FALSE]
  a <- unname(ejo_adopt)
  res <- lapply(seq_len(nrow(op)), function(i) {
    g <- cy$total * op$effect[i] * a
    opx <- op$opex_bn[i] * 1e9 * a
    net <- g - opx - c(op$capex_bn[i] * 1e9, rep(0, length(a) - 1))
    pv_g <- sum(g * ejo_disc(cy$year))
    pv_c <- op$capex_bn[i] * 1e9 + sum(opx * ejo_disc(cy$year))
    cum <- cumsum(net)
    data.frame(gain = sum(g), npv = pv_g - pv_c, bcr = if (pv_c > 0) pv_g / pv_c else NA_real_,
               payback = if (any(cum >= 0)) cy$year[which(cum >= 0)[1]] else NA_integer_)
  })
  cbind(op, do.call(rbind, res))
}
ejo_knapsack <- function(ap, budget) {
  ok <- ap$npv > 0
  if (!any(ok) || !requireNamespace("lpSolve", quietly = TRUE)) return(character())
  a <- ap[ok, , drop = FALSE]
  s <- lpSolve::lp("max", a$npv / 1e9, matrix(a$capex_bn, nrow = 1), "<=", budget, all.bin = TRUE)
  if (s$status != 0) return(character())
  a$id[s$solution > 0.5]
}
ejo_appraisals <- setNames(lapply(names(ejo_causes), ejo_appraise), names(ejo_causes))
ejo_default_ids <- setNames(lapply(names(ejo_causes), function(k) ejo_knapsack(ejo_appraisals[[k]], ejo_budget0[[k]])), names(ejo_causes))
# Precomputed plan gains for overview (default knapsack) — no reactive wait on cause pages.
ejo_gain_default <- do.call(rbind, lapply(names(ejo_causes), function(k) {
  cbind(cause = k, ejo_gain(k, ejo_default_ids[[k]]))
}))

ejo_to_usd <- function(rwf, year) {
  fx <- fx_rwf$value[match(year, fx_rwf$year)]
  fx[!is.finite(fx)] <- ejo_fx_now
  ifelse(is.finite(fx) & fx > 0, rwf / fx, NA_real_)
}
ejo_usd_txt <- function(x) {
  if (!length(x) || !is.finite(x)) return("\u2014")
  if (abs(x) >= 1e9) paste0("$", formatC(x / 1e9, format = "f", digits = 2), " bn")
  else if (abs(x) >= 1e6) paste0("$", formatC(x / 1e6, format = "f", digits = 1), " m")
  else paste0("$", formatC(x, format = "f", digits = 0, big.mark = ","))
}
ejo_window_usd <- function(cause = NULL, y0, y1) {
  d <- ejo_cy
  if (!is.null(cause)) d <- d[d$cause == cause, , drop = FALSE]
  d <- d[d$year >= y0 & d$year <= y1, , drop = FALSE]
  if (!nrow(d)) return(list(farmer = NA_real_, gov = NA_real_, y0 = NA_integer_, y1 = NA_integer_))
  list(
    farmer = sum(ejo_to_usd(d$farmer, d$year), na.rm = TRUE),
    gov = sum(ejo_to_usd(d$gov, d$year), na.rm = TRUE),
    y0 = min(d$year), y1 = max(d$year)
  )
}
ejo_land <- local({
  yrs <- sort(unique(ejo_cy$year))
  hist <- yrs[yrs <= 2025]
  if (length(hist) > 30) hist <- utils::tail(hist, 30)
  y0 <- if (length(hist)) min(hist) else NA_integer_
  y1 <- if (length(hist)) max(hist) else NA_integer_
  fwd <- yrs[yrs >= 2026 & yrs <= 2030]
  f0 <- if (length(fwd)) min(fwd) else NA_integer_
  f1 <- if (length(fwd)) max(fwd) else NA_integer_
  pack <- function(cause) {
    h <- ejo_window_usd(cause, y0, y1)
    f <- ejo_window_usd(cause, f0, f1)
    list(
      farmer = ejo_usd_txt(h$farmer), gov = ejo_usd_txt(h$gov),
      ahead = ejo_usd_txt(f$farmer + f$gov),
      span = if (is.finite(h$y0)) paste0(h$y0, "\u2013", h$y1) else "",
      aheadSpan = if (is.finite(f$y0)) paste0(f$y0, "\u2013", f$y1) else ""
    )
  }
  gparts <- lapply(names(ejo_causes), function(k) {
    g <- ejo_gain(k, ejo_default_ids[[k]])
    g <- g[g$year >= 2026 & g$year <= 2030, , drop = FALSE]
    if (!nrow(g)) return(0)
    sum(ejo_to_usd(g$gain, g$year), na.rm = TRUE)
  })
  fwd_span <- if (length(fwd)) paste0(min(fwd), "\u2013", max(fwd)) else ""
  fwd_loss <- if (length(fwd)) ejo_window_usd(NULL, min(fwd), max(fwd)) else list(farmer = NA_real_, gov = NA_real_)
  list(
    overview = pack(NULL),
    transport = pack("transport"),
    storage = pack("storage"),
    weather = pack("weather"),
    cold = pack("cold"),
    glut = pack("glut"),
    handling = pack("handling"),
    gain = ejo_usd_txt(sum(unlist(gparts))),
    gainSpan = fwd_span,
    action = list(
      farmer = ejo_usd_txt(fwd_loss$farmer),
      gov = ejo_usd_txt(fwd_loss$gov),
      ahead = ejo_usd_txt(fwd_loss$farmer + fwd_loss$gov),
      span = fwd_span,
      aheadSpan = fwd_span
    )
  )
})

# ---- CPM / PERT ----
ejo_cpm <- function(k) {
  pl <- ejo_plans[ejo_plans$cause == k, , drop = FALSE]
  if (!nrow(pl)) return(NULL)
  pl$dur <- (pl$opt + 4 * pl$likely + pl$pess) / 6
  pl$var <- ((pl$pess - pl$opt) / 6)^2
  pre <- lapply(ifelse(is.na(pl$after), "", pl$after), function(s) setdiff(strsplit(s, ";", fixed = TRUE)[[1]], ""))
  n <- nrow(pl)
  es <- ef <- numeric(n)
  for (i in seq_len(n)) {
    p <- match(pre[[i]], pl$id)
    es[i] <- if (length(p)) max(ef[p]) else 0
    ef[i] <- es[i] + pl$dur[i]
  }
  E <- max(ef)
  lf <- rep(E, n)
  ls <- numeric(n)
  for (i in rev(seq_len(n))) {
    succ <- which(vapply(pre, function(z) pl$id[i] %in% z, logical(1)))
    lf[i] <- if (length(succ)) min(ls[succ]) else E
    ls[i] <- lf[i] - pl$dur[i]
  }
  pl$es <- es
  pl$ef <- ef
  pl$ls <- ls
  pl$lf <- lf
  pl$slack <- pmax(0, ls - es)
  pl$critical <- pl$slack < 1e-6
  list(plan = pl, E = E, sd = sqrt(sum(pl$var[pl$critical])), cost = sum(pl$cost_m))
}

# Implementation order matches the landing loop. Months are counted from January 2026.
ejo_plan_order <- c(
  weather = "Rain at harvest", storage = "Drying", handling = "Handling",
  transport = "Transport routes", cold = "Lack of cooling", glut = "Too much supply"
)
ejo_where <- c(
  weather = "Districts on the Rain at harvest page, ranked by NASA POWER harvest-month rain",
  storage = "Districts on the Drying page, and the warehouse sites the p-median chooses",
  handling = "Collection points and markets used in the crate and sorting trials",
  transport = "District-to-market roads already in the transport model",
  cold = "Districts with a vegetable or fruit harvest on the Lack of cooling page",
  glut = "Markets in the monthly price calendar"
)
ejo_data_src <- c(
  weather = "NASA POWER rain and NISR SAS cereal and pulse production are already loaded",
  storage = "NISR SAS storage loss and APHLIS cereal loss by stage are already loaded",
  handling = "FAO food-balance loss valued at FAO producer prices is already loaded",
  transport = "OSRM road times and market prices are already loaded",
  cold = "NISR SAS vegetables and fruits, NAEB horticulture exports and NASA POWER temperature are already loaded",
  glut = "Monthly market prices and FAO production are already loaded"
)
ejo_month_name <- function(m) {
  origin <- as.Date("2026-01-01")
  vapply(pmax(0, floor(as.numeric(m))), function(k) {
    format(seq(origin, by = "month", length.out = k + 1)[k + 1], "%b %Y")
  }, character(1))
}
ejo_pm <- do.call(rbind, lapply(names(ejo_plan_order), function(k) {
  cp <- ejo_cpm(k)
  if (is.null(cp)) return(NULL)
  pl <- cp$plan
  track <- grepl("Measure|Audit|Review|Monitor|Track", pl$activity)
  report_to <- if (any(track)) pl$lead[which(track)[1]] else pl$lead[1]
  data.frame(
    cause = k,
    cause_label = unname(ejo_plan_order[[k]]),
    id = pl$id,
    activity = pl$activity,
    lead = pl$lead,
    where = unname(ejo_where[[k]]),
    start = ejo_month_name(pl$es),
    finish = ejo_month_name(pl$ef - 1e-9),
    likely = pl$likely,
    critical = pl$critical,
    after = ifelse(is.na(pl$after) | !nzchar(pl$after), "Starts January 2026", paste("After", gsub(";", " and ", pl$after, fixed = TRUE))),
    cost = pl$cost_m * 1e6,
    data_note = ifelse(track, "This activity creates the progress figure. It is not in the published files yet.", unname(ejo_data_src[[k]])),
    report_to = report_to,
    track = track,
    stringsAsFactors = FALSE
  )
}))
rownames(ejo_pm) <- NULL
ejo_report_path <- file.path("reports", "ejo_progress.csv")

# ---- network: OSRM road matrix between district centres and markets ----
ejo_net <- local({
  if (is.null(ejo_nodes) || is.null(ejo_matrix)) return(NULL)
  n <- nrow(ejo_nodes)
  if (nrow(ejo_matrix) != n * n) return(NULL)
  id <- paste0(ifelse(ejo_nodes$type == "district", "d:", "m:"), ejo_nodes$node)
  km <- matrix(ejo_matrix$km, n, n, byrow = TRUE, dimnames = list(id, id))
  hr <- matrix(ejo_matrix$hours, n, n, byrow = TRUE, dimnames = list(id, id))
  hav <- function(i, j) {
    r <- pi / 180
    a <- sin((ejo_nodes$lat[j] - ejo_nodes$lat[i]) * r / 2)^2 + cos(ejo_nodes$lat[i] * r) * cos(ejo_nodes$lat[j] * r) * sin((ejo_nodes$lon[j] - ejo_nodes$lon[i]) * r / 2)^2
    6371 * 2 * asin(sqrt(a))
  }
  bad <- which(!is.finite(km), arr.ind = TRUE)
  if (nrow(bad)) for (b in seq_len(nrow(bad))) {
    d <- 1.4 * hav(bad[b, 1], bad[b, 2])
    km[bad[b, 1], bad[b, 2]] <- d
    hr[bad[b, 1], bad[b, 2]] <- d / 40
  }
  nodes <- ejo_nodes
  nodes$id <- id
  list(nodes = nodes, km = km, hr = hr)
})
ejo_xy <- function(id) {
  i <- match(id, ejo_net$nodes$id)
  data.frame(lng = ejo_net$nodes$lon[i], lat = ejo_net$nodes$lat[i])
}
ejo_route_path <- function(from, to) {
  if (is.null(ejo_routes)) return(NULL)
  r <- ejo_routes[ejo_routes$from == from & ejo_routes$to == to, , drop = FALSE]
  rev_it <- FALSE
  if (!nrow(r)) {
    r <- ejo_routes[ejo_routes$from == to & ejo_routes$to == from, , drop = FALSE]
    rev_it <- TRUE
  }
  if (!nrow(r)) return(NULL)
  pts <- strsplit(strsplit(r$path[1], "|", fixed = TRUE)[[1]], " ", fixed = TRUE)
  lng <- as.numeric(vapply(pts, `[`, "", 1))
  lat <- as.numeric(vapply(pts, `[`, "", 2))
  if (rev_it) {
    lng <- rev(lng)
    lat <- rev(lat)
  }
  list(lng = lng, lat = lat)
}

# ---- products and crops ----
ejo_sas_crop <- function(p) {
  x <- tolower(p)
  if (grepl("maize", x)) return("Maize")
  if (grepl("rice", x)) return("Paddy rice")
  if (grepl("sorghum", x)) return("Sorghum")
  if (grepl("wheat", x)) return("Wheat")
  if (grepl("soya", x)) return("Soybean")
  if (grepl("groundnut", x)) return("Groundnuts")
  if (grepl("sweet", x) && grepl("potato", x)) return("Sweet potato")
  if (grepl("irish|kinigi|kirundo|kuruseke|^potato", x)) return("Irish potato")
  if (grepl("cassava", x)) return("Cassava")
  if (grepl("taro|yam", x)) return("Yams & Taro")
  if (grepl("avocado|mango|orange|lemon|passion|pineapple|plum|banan-fruit|banana fruit|apple|papaya|tree tomato|strawberr|melon|guava", x)) return("Fruits")
  if (grepl("banana|banan", x)) return("Cooking banana")
  if (grepl("green bean|fresh peas|tomat|onion|carrot|cabbage|pepper|chilli|egg ?plant|aubergine|courgette|cucumber|spinach|celery|leek|garlic|betterave|beet|cauliflower|lettuce|amaranth|dodo", x)) return("Vegetables")
  if (grepl("pea", x)) return("Pea")
  if (grepl("bean", x)) return("Bean")
  "Vegetables"
}
ejo_crop_perish <- function(crop) {
  if (crop %in% c("Maize", "Paddy rice", "Sorghum", "Wheat", "Bean", "Pea", "Soybean", "Groundnuts")) "durable"
  else if (crop %in% c("Irish potato", "Sweet potato", "Cassava", "Yams & Taro", "Cooking banana")) "semi"
  else "perishable"
}
ejo_districts <- names(district_lnglat)
ejo_district_share <- function(crops) {
  out <- setNames(rep(0, length(ejo_districts)), ejo_districts)
  p <- sas_prod[sas_prod$crop %in% crops & sas_prod$district %in% ejo_districts, , drop = FALSE]
  if (nrow(p)) {
    p <- p[p$year == max(p$year), , drop = FALSE]
    v <- tapply(p$value, p$district, sum, na.rm = TRUE)
    out[names(v)] <- v
  }
  if (sum(out, na.rm = TRUE) <= 0) out[] <- 1
  out[!is.finite(out)] <- 0
  out / sum(out)
}
ejo_fao_map <- list(c("tree tomato", "^Other fruits"), c("tomat", "^Tomatoes"), c("onion", "^Onions and shallots, dry"), c("carrot", "^Carrots"),
                    c("cabbage", "^Cabbages"), c("avocado", "^Avocados"), c("pineapple", "^Pineapples"), c("mango", "^Mangoes"),
                    c("orange", "^Oranges"), c("lemon", "^Lemons"), c("green bean", "^Other beans, green"), c("pepper|chilli", "^Chillies"),
                    c("egg ?plant|aubergine", "^Eggplants"), c("courgette|pumpkin", "^Pumpkins"), c("spinach", "^Spinach"),
                    c("cucumber", "^Cucumbers"), c("taro", "^Taro"), c("yam", "^Yams"), c("cassava", "^Cassava"),
                    c("sweet", "^Sweet potatoes"), c("potato|kinigi|kirundo|kuruseke", "^Potatoes"), c("maize", "^Maize"),
                    c("rice", "^Rice"), c("sorghum", "^Sorghum"), c("wheat", "^Wheat"), c("soya", "^Soya"),
                    c("groundnut", "^Groundnuts"), c("fresh peas", "^Peas, green"), c("pea", "^Peas, dry"), c("bean", "^Beans, dry"),
                    c("banan-fruit|banana fruit", "^Bananas"), c("banana", "^Plantains|^Cooking bananas"), c("plum", "^Plums"),
                    c("papaya", "^Papayas"), c("leek", "^Leeks"))
ejo_volume <- function(product, crop) {
  x <- tolower(product)
  tonnes <- NA_real_
  src <- ""
  for (m in ejo_fao_map) if (grepl(m[1], x)) {
    its <- unique(fao_prod$item[grepl(m[2], fao_prod$item)])
    if (length(its)) {
      d <- fao_prod[fao_prod$item == its[1] & is.finite(fao_prod$value), , drop = FALSE]
      if (nrow(d)) {
        tonnes <- d$value[which.max(d$year)]
        src <- paste0("FAOSTAT ", its[1], ", ", max(d$year))
      }
    }
    break
  }
  if (!is.finite(tonnes)) {
    n <- sas_prod_nat[sas_prod_nat$crop == crop, , drop = FALSE]
    if (nrow(n)) {
      y <- max(n$year)
      tonnes <- sum(n$value[n$year == y])
      src <- paste0("NISR SAS ", crop, " ", y)
    } else {
      tonnes <- 10000
      src <- "placeholder volume"
    }
  }
  uc <- if (crop == "Bean") "Bush bean" else crop
  u <- sas_use[sas_use$crop == uc, , drop = FALSE]
  sold <- if (nrow(u)) {
    u <- u[order(u$year, u$season), , drop = FALSE]
    u$sold[nrow(u)] / 100
  } else ejo_p("marketed_default")
  if (!is.finite(sold) || sold <= 0) sold <- ejo_p("marketed_default")
  list(tonnes = tonnes, sold = sold, marketed = tonnes * sold, source = src)
}
ejo_products <- local({
  if (!nrow(naeb_prices) || is.null(ejo_net)) return(character())
  p <- naeb_prices[naeb_prices$tab == "food" & tolower(naeb_prices$unit) == "kg", , drop = FALSE]
  p <- p[p$date > max(p$date) - 365, , drop = FALSE]
  n <- tapply(p$market, p$product, function(z) length(unique(z)))
  out <- names(n)[n >= 15]
  out <- out[!grepl("flour|minimex|oil|sugar|fish|meat|meet|beef|porc|egg|milk|salt|charcoal", out, ignore.case = TRUE)]
  sort(out)
})
ejo_product_choices <- setNames(ejo_products, prod_label(ejo_products))

# ---- 1. transport: transportation LP ----
ejo_transport <- function(product, tc, hub, slack, via = "Nyabugogo") {
  if (is.null(ejo_net) || !length(product) || !nzchar(product)) return(NULL)
  pr <- naeb_prices[naeb_prices$product == product, , drop = FALSE]
  if (!nrow(pr)) return(NULL)
  pr <- pr[pr$date > max(pr$date) - 365, , drop = FALSE]
  mp <- tapply(pr$price, pr$market, stats::median)
  mk <- intersect(names(mp), ejo_net$nodes$node[ejo_net$nodes$type == "market"])
  if (!length(via) || !paste0("m:", via) %in% ejo_net$nodes$id) via <- "Nyabugogo"
  if (length(mk) < 3 || !paste0("m:", via) %in% ejo_net$nodes$id) return(NULL)
  vid <- paste0("m:", via)
  crop <- ejo_sas_crop(product)
  grp <- ejo_crop_perish(crop)
  sp <- ejo_p(paste0("spoil_h_", grp))
  tr <- ejo_p(paste0("transfer_", grp))
  vol <- ejo_volume(product, crop)
  sh <- ejo_district_share(crop)
  S <- sh * vol$marketed
  D <- rep(sum(S) / length(mk), length(mk))
  dn <- paste0("d:", names(S))
  mn <- paste0("m:", mk)
  km <- ejo_net$km[dn, mn, drop = FALSE]
  hr <- ejo_net$hr[dn, mn, drop = FALSE]
  dimnames(km) <- dimnames(hr) <- list(names(S), mk)
  price <- as.numeric(mp[mk]) * 1000
  cap1 <- function(m) {
    m[] <- pmin(1, m)
    m
  }
  lossf <- cap1(2 * tr + sp * hr)
  net <- sweep(1 - lossf, 2, price, "*") - tc * km
  k1 <- ejo_net$km[dn, vid]
  h1 <- ejo_net$hr[dn, vid]
  k2 <- ejo_net$km[vid, mn]
  h2 <- ejo_net$hr[vid, mn]
  lossh <- cap1(4 * tr + sp * outer(h1, h2, "+"))
  kmh <- outer(k1, k2, "+")
  neth <- sweep(1 - lossh, 2, price, "*") - tc * kmh
  if (!requireNamespace("lpSolve", quietly = TRUE)) return(NULL)
  sol <- lpSolve::lp.transport(net, "max", rep("=", length(S)), S, rep("<=", length(mk)), D * slack, integers = NULL)
  X <- sol$solution
  dimnames(X) <- list(names(S), mk)
  near <- apply(km, 1, which.min)
  Xd <- matrix(0, length(S), length(mk))
  Xd[cbind(seq_along(S), near)] <- S * (1 - hub)
  Xh <- outer(S * hub, D / sum(D))
  money <- function(F, lf, kmm) sum(F * sweep(lf, 2, price, "*")) + tc * sum(F * kmm)
  cur_lost <- money(Xd, lossf, km) + money(Xh, lossh, kmh)
  opt_lost <- money(X, lossf, km)
  cur_val <- sum(Xd * net) + sum(Xh * neth)
  opt_val <- sum(X * net)
  list(product = product, crop = crop, grp = grp, sp = sp, tr = tr, vol = vol, S = S, D = D, mk = mk,
       price = setNames(price, mk), km = km, hr = hr, net = net, neth = neth, X = X, Xd = Xd, Xh = Xh,
       cur_val = cur_val, opt_val = opt_val, cur_lost = cur_lost, opt_lost = opt_lost,
       cur_tlost = sum(Xd * lossf) + sum(Xh * lossh), opt_tlost = sum(X * lossf),
       cur_km = sum(Xd * km) + sum(Xh * kmh), opt_km = sum(X * km), tc = tc, hub = hub, via = via)
}

# ---- 2. storage: p-median warehouse sites ----
ejo_pmedian <- function(w, k) {
  if (is.null(ejo_net) || !requireNamespace("lpSolve", quietly = TRUE)) return(NULL)
  ds <- names(w)
  n <- length(ds)
  id <- paste0("d:", ds)
  D <- ejo_net$km[id, id]
  D <- (D + t(D)) / 2
  obj <- c(as.vector(t(w * D)), rep(0, n))
  nv <- n * n + n
  A1 <- matrix(0, n, nv)
  for (i in seq_len(n)) A1[i, (i - 1) * n + seq_len(n)] <- 1
  A2 <- matrix(0, n * n, nv)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    r <- (i - 1) * n + j
    A2[r, r] <- 1
    A2[r, n * n + j] <- -1
  }
  A3 <- matrix(c(rep(0, n * n), rep(1, n)), 1)
  s <- lpSolve::lp("min", obj, rbind(A1, A2, A3), c(rep("=", n), rep("<=", n * n), "="), c(rep(1, n), rep(0, n * n), k),
                   binary.vec = n * n + seq_len(n))
  if (s$status != 0) return(NULL)
  x <- matrix(s$solution[seq_len(n * n)], n, n, byrow = TRUE)
  site <- ds[s$solution[n * n + seq_len(n)] > 0.5]
  to <- ds[apply(x, 1, which.max)]
  dist <- D[cbind(seq_len(n), match(to, ds))]
  list(sites = site, assign = data.frame(district = ds, site = to, km = dist, w = w), avg_km = sum(w * dist) / sum(w), max_km = max(dist[w > 0]))
}

# ---- 3. weather: harvest-month rain from NASA POWER ----
ejo_rain <- function(season) {
  if (is.null(ejo_power)) return(NULL)
  m <- if (season == "A") c(1, 2) else c(6, 7)
  p <- ejo_power[ejo_power$month %in% m & is.finite(ejo_power$rain_mm), , drop = FALSE]
  a <- stats::aggregate(rain_mm ~ district + year, p, sum)
  med <- tapply(a$rain_mm, a$district, stats::median)
  a$excess <- pmax(0, a$rain_mm / med[a$district] - 1)
  a
}
ejo_group_crops <- list(cereals = c("Maize", "Sorghum", "Paddy rice", "Wheat"), pulses = c("Bean", "Pea", "Soybean", "Groundnuts"),
                        roots = c("Cassava", "Sweet potato", "Irish potato", "Yams & Taro"), bananas = "Cooking banana",
                        vegetables = "Vegetables", fruits = "Fruits")

# ---- 4. cold chain: M/M/c ----
ejo_erlang <- function(c, a) {
  if (a >= c) return(1)
  k <- 0:(c - 1)
  ls <- exp(k * log(a) - lfactorial(k))
  top <- exp(c * log(a) - lfactorial(c)) * c / (c - a)
  top / (sum(ls) + top)
}
ejo_tmax <- local({
  if (is.null(ejo_power)) return(setNames(rep(25, length(ejo_districts)), ejo_districts))
  p <- ejo_power[ejo_power$year >= max(ejo_power$year) - 9 & is.finite(ejo_power$tmax_c), , drop = FALSE]
  v <- tapply(p$tmax_c, p$district, mean)
  out <- setNames(rep(stats::median(v), length(ejo_districts)), ejo_districts)
  out[names(v)] <- v
  out
})
ejo_perish_price <- local({
  p <- naeb_prices[naeb_prices$category %in% c("vegetables and fruits") & tolower(naeb_prices$unit) == "kg", , drop = FALSE]
  if (!nrow(p)) p <- naeb_prices[grepl("tomat", naeb_prices$product, ignore.case = TRUE), , drop = FALSE]
  v <- if (nrow(p)) stats::median(p$price[p$date > max(p$date) - 365]) else 500
  if (is.finite(v)) v else 500
})
ejo_hort_t <- local({
  p <- sas_prod[sas_prod$crop %in% c("Vegetables", "Fruits") & sas_prod$district %in% ejo_districts, , drop = FALSE]
  out <- setNames(rep(0, length(ejo_districts)), ejo_districts)
  if (nrow(p)) {
    yrs <- sort(unique(p$year), decreasing = TRUE)
    y <- yrs[1]
    v <- tapply(p$value[p$year == y], p$district[p$year == y], sum)
    ns <- length(unique(p$season[p$year == y]))
    out[names(v)] <- v * if (ns == 1) 2 else 1
  }
  out
})
ejo_hort_sold <- local({
  u <- sas_use[sas_use$crop %in% c("Vegetables", "Fruits"), , drop = FALSE]
  v <- if (nrow(u)) mean(u$sold[u$year == max(u$year)], na.rm = TRUE) / 100 else 0.6
  if (is.finite(v) && v > 0) v else 0.6
})
ejo_queue <- function(d, lot, cap, cost_m, wait0 = 24) {
  t_yr <- ejo_hort_t[[d]] * ejo_hort_sold
  if (!is.finite(t_yr) || t_yr <= 0) return(NULL)
  lam <- t_yr / 8760 / lot
  mu <- cap / lot / 24
  a <- lam / mu
  spo <- ejo_p("spoil_h_perishable") * ejo_p("q10")^((ejo_tmax[[d]] - 25) / 10)
  val_lot <- lot * 1000 * ejo_perish_price
  base <- lam * 8760 * min(1, spo * wait0) * val_lot
  cmin <- max(1, floor(a) + 1)
  cs <- cmin:(cmin + 14)
  rows <- lapply(cs, function(c) {
    C <- ejo_erlang(c, a)
    wq <- C / (c * mu - lam)
    data.frame(c = c, rho = a / c, wq = wq, p48 = C * exp(-(c * mu - lam) * 48), p12 = C * exp(-(c * mu - lam) * 12),
               spoil = lam * 8760 * min(1, spo * wq) * val_lot, rooms = c * cost_m * 1e6)
  })
  tb <- do.call(rbind, rows)
  tb$total <- tb$spoil + tb$rooms
  best <- tb[which.min(tb$total), ]
  list(d = d, t_yr = t_yr, lam = lam, mu = mu, a = a, spo = spo, base = base, tb = tb, best = best, tmax = ejo_tmax[[d]])
}

# ---- 5. glut: multi-period sales plan ----
ejo_monthly_price <- function(product) {
  pr <- naeb_prices[naeb_prices$product == product, , drop = FALSE]
  if (!nrow(pr)) return(NULL)
  mm <- stats::aggregate(price ~ year + month, pr, stats::median)
  idx <- stats::aggregate(price ~ month, mm, mean)
  p <- idx$price[match(1:12, idx$month)]
  if (sum(is.finite(p)) < 6) return(NULL)
  ok <- which(is.finite(p))
  p <- stats::approx(c(ok - 12, ok, ok + 12), rep(p[ok], 3), xout = 1:12)$y
  list(p = p, n = nrow(mm), months = sort(unique(paste(mm$year, mm$month))))
}
ejo_glut <- function(product, harvest, cost_store, delta) {
  mp <- ejo_monthly_price(product)
  if (is.null(mp) || !length(harvest) || !requireNamespace("lpSolve", quietly = TRUE)) return(NULL)
  p <- mp$p * 1000
  H <- rep(0, 12)
  H[harvest] <- 1 / length(harvest)
  A <- matrix(0, 13, 24)
  for (t in 1:12) {
    A[t, t] <- 1
    A[t, 12 + t] <- 1
    if (t > 1) A[t, 12 + t - 1] <- -(1 - delta)
  }
  A[13, 24] <- 1
  s <- lpSolve::lp("max", c(p, rep(-cost_store, 12)), A, rep("=", 13), c(H, 0))
  if (s$status != 0) return(NULL)
  sell <- s$solution[1:12]
  inv <- s$solution[13:24]
  list(p = p, H = H, sell = sell, inv = inv, cur = sum(p * H), opt = s$objval, n = mp$n)
}
ejo_harvest_default <- function(crop) {
  if (crop %in% c("Vegetables", "Fruits", "Cooking banana", "Cassava")) c(1, 2, 6, 7) else c(1, 2, 6, 7)
}

# ---- UI ----
ej_steps <- c("0" = "Overview", "1" = "Rain at harvest", "2" = "Drying", "3" = "Handling",
              "4" = "Transport routes", "5" = "Lack of cooling", "6" = "Too much supply", "7" = "Action plan")
ej_step_bar <- function(n) {
  tags$div(
    class = "hv-step-foot",
    if (n > 0) tags$button(type = "button", class = "btn-ghost", `data-step-go` = n - 1, paste("\u2190", ej_steps[[as.character(n - 1)]])),
    tags$span(class = "hv-step-count", paste(n + 1, "of", length(ej_steps))),
    if (n < 7) tags$button(type = "button", class = "btn-go", `data-step-go` = n + 1, paste(ej_steps[[as.character(n + 1)]], "\u2192"))
  )
}
ej_sec <- function(n, title, sub = NULL) tags$div(class = "ej-sec", tags$b(n), tags$div(tags$h3(title), if (!is.null(sub)) tags$p(sub)))

ej_dash_icon <- function(kind = "globe") {
  paths <- list(
    globe = "M12 2a10 10 0 1 0 0 20 10 10 0 0 0 0-20Zm0 2c1.6 0 3.3 2.6 3.8 6H8.2C8.7 6.6 10.4 4 12 4Z",
    farmer = "M12 12a4 4 0 1 0-4-4 4 4 0 0 0 4 4Zm0 2c-4 0-8 2-8 4v2h16v-2c0-2-4-4-8-4Z",
    gov = "M3 21h18v-2H3v2Zm2-4h14V9L12 4 5 9v8Zm4-2V11h2v4H9Zm4 0V11h2v4h-2Z",
    coin = "M12 2a10 10 0 1 0 10 10A10 10 0 0 0 12 2Zm1 15h-2v-1.1A3.5 3.5 0 0 1 8 13h2a1.5 1.5 0 0 0 1.5 1.5H12A1.5 1.5 0 1 0 12 11h-1a3.5 3.5 0 0 1 0-7V2.9h2V4a3.5 3.5 0 0 1 3 2.9h-2A1.5 1.5 0 0 0 12 5.5h-.5A1.5 1.5 0 1 0 12 8.5h1a3.5 3.5 0 0 1 0 7V17Z",
    tonne = "M4 18h16v2H4v-2Zm2-2h12l1-8H5l1 8Zm3-10h6l1 2H8l1-2Z",
    gain = "M4 18h16v2H4zm2-3 4-5 3 3 5-7 1.5 1.2-6.5 9-3-3-2.5 3.1L6 15Z",
    rain = "M12 3a6 6 0 0 0-6 6c0 4 6 10 6 10s6-6 6-10a6 6 0 0 0-6-6Z"
  )
  d <- if (!is.null(paths[[kind]])) paths[[kind]] else paths$globe
  HTML(sprintf(
    '<svg class="ej-dash-icon" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true" focusable="false"><path d="%s"></path></svg>',
    d
  ))
}
ej_dash_kpi <- function(value, label, count = NULL, icon = "globe") {
  tags$article(
    class = "ej-dash-card",
    ej_dash_icon(icon),
    tags$p(class = "ej-dash-value", value),
    if (!is.null(count) && nzchar(as.character(count))) tags$p(class = "ej-dash-count", count),
    tags$p(class = "ej-dash-label", label)
  )
}
ej_dash_hero <- function(...) {
  tags$div(
    class = "ej-dash-hero",
    style = "background-image:url('ejo/dash/hero-land.webp');",
    tags$div(class = "ej-dash-hero-overlay", `aria-hidden` = "true"),
    tags$div(class = "ej-dash-row", ...)
  )
}
ej_top <- function(scope, ...) {
  ctl <- list(...)
  is_over <- identical(as.character(scope), "over")
  tags$div(
    class = paste("ej-top ej-dash-filters ej-dash-filters-mid", if (is_over) "is-over"),
    tags$div(class = "ej-filter-field", selectInput(paste0("ej_", scope, "_y"), "YEAR", choices = rev(ejo_hist_years), selected = 2026, selectize = FALSE, width = "100%")),
    tags$div(class = "ej-filter-field", selectInput(paste0("ej_", scope, "_f"), "FORECAST", choices = ejo_fc_years, selected = 2031, selectize = FALSE, width = "100%")),
    tags$div(
      class = "ej-filter-field",
      selectInput(
        paste0("ej_", scope, "_who"), "WHO LOSES",
        choices = c("Farmers + government" = "both", "Farmers" = "farmer", "Government" = "gov"),
        selected = "both", selectize = FALSE, width = "100%"
      )
    ),
    if (length(ctl)) lapply(ctl, function(x) tags$div(class = "ej-filter-field", x))
  )
}
ej_detail_box <- function(scope, back) {
  tags$div(
    class = "ej-detail",
    tags$button(type = "button", class = "btn-ghost ej-back", `data-ej-back` = "1", paste("\u2190 Back to", back)),
    uiOutput(paste0("ej_", scope, "_detail"))
  )
}

ejo_cause_ui <- function(k, i, head, lede, controls, or_block) {
  op <- ejo_options[ejo_options$cause == k, , drop = FALSE]
  tags$div(
    class = "hv-step", `data-step` = as.character(i),
    tags$div(
      class = "hv-step-head",
      tags$p(class = "kicker", sprintf("CAUSE %d OF 6 \u00b7 %s", i, toupper(ejo_causes[[k]]))),
      tags$h2(head),
      tags$p(lede)
    ),
    ejo_inspect_ui(k),
    uiOutput(paste0("ej_", k, "_loss_cards")),
    do.call(ej_top, c(list(k), controls)),
    tags$div(
      class = "ej-main ej-dash-stack",
      tags$div(class = "chart-card", tags$h3(class = "card-title", "Money lost on this cause, farmers and government"), plotlyOutput(paste0("ej_", k, "_loss_plot"), height = "380px")),
      tags$div(class = "chart-card", tags$h3(class = "card-title", "Share of national post-harvest loss on this cause"), plotlyOutput(paste0("ej_", k, "_share_plot"), height = "320px")),
      ej_sec("1", paste("Operations research:", ejo_or[[k]]), "One chart or map at a time. Change the filters above and the model solves again."),
      or_block,
      ej_sec("2", "Options we formulated, ranked", "Each option is costed and tested on this cause's loss, 2026\u20132031. A 0-1 knapsack model picks the best set for the budget."),
      tags$div(
        class = "price-filters hv-filters ej-opt-filters",
        sliderInput(paste0("ej_", k, "_budget"), "BUDGET FOR THIS CAUSE (RWF BN, CAPITAL)", 0, 60, ejo_budget0[[k]], step = 0.5),
        checkboxGroupInput(paste0("ej_", k, "_opts"), "OPTIONS IN THE PLAN", choices = setNames(op$id, paste0(op$id, " \u00b7 ", op$option)),
                           selected = ejo_default_ids[[k]])
      ),
      uiOutput(paste0("ej_", k, "_opts_table")),
      ej_sec("3", "What we gain, 2026\u20132031", "Blue is money saved with the options in the plan, as they roll out year by year."),
      uiOutput(paste0("ej_", k, "_gain_cards")),
      tags$div(class = "chart-card", plotlyOutput(paste0("ej_", k, "_gain_plot"), height = "400px")),
      uiOutput(paste0("ej_", k, "_year_table")),
      ej_sec("4", "Action plan: critical path (CPM/PERT)", "Who does what, in which order. Dark blue bars are the critical path."),
      tags$div(class = "price-filters viz-only", sliderInput(paste0("ej_", k, "_deadline"), "TARGET (MONTHS FROM START)", 6, 36, 18, step = 1)),
      uiOutput(paste0("ej_", k, "_cpm_cards")),
      tags$div(class = "chart-card", plotlyOutput(paste0("ej_", k, "_gantt"), height = "380px"))
    ),
    ej_detail_box(k, ejo_causes[[k]]),
    ej_step_bar(i)
  )
}

ejo_look_ui <- function() {
  m <- ejo_land
  tags$div(
    id = "ejo-land", class = "ej-look",
    tags$button(type = "button", class = "ej-arrow ej-prev", "\u2039"),
    tags$button(type = "button", class = "ej-arrow ej-next", "\u203a"),
    tags$div(
      class = "ej-copy",
      tags$button(
        type = "button", id = "ej-pause", class = "ej-pause", "aria-label" = "Pause",
        tags$img(src = "ejo/land/pause.png?v=5", alt = "")
      ),
      tags$h2(id = "ej-title"),
      tags$p(id = "ej-body"),
      tags$button(type = "button", class = "ej-go", `data-ejo-open` = "room", "Navigate  >")
    ),
    tags$div(
      class = "ej-stage",
      tags$img(id = "ej-hero", alt = ""),
      tags$p(id = "ej-tag")
    ),
    tags$div(
      class = "ej-rail",
      tags$div(id = "ej-prices"),
      tags$div(id = "ej-predict"),
      tags$img(id = "ej-thumb", alt = "")
    ),
    tags$script(HTML(paste0("window.EJO_LAND=", jsonlite::toJSON(m, auto_unbox = TRUE, null = "null"), ";")))
  )
}

ejo_ui <- function() {
  dist_ch <- sort(ejo_districts)
  mk_ch <- if (!is.null(ejo_net)) sort(ejo_net$nodes$node[ejo_net$nodes$type == "market"]) else character()
  sel_prod <- if ("Tomatos" %in% ejo_products) "Tomatos" else ejo_products[1]
  glut_prod <- if ("Irish- Potato- Kirundo" %in% ejo_products) "Irish- Potato- Kirundo" else sel_prod
  hort_rank <- names(sort(ejo_hort_t, decreasing = TRUE))
  tagList(
  tags$section(
    id = "ejo-hazaza", class = "panel",
    ejo_look_ui(),
    tags$div(
      id = "ejo-room", class = "is-hidden",
    hero_banner(
      if (file.exists(file.path("www", "umusaruro", "ejo-hero.jpg"))) "umusaruro/ejo-hero.jpg" else "umusaruro/hero.jpg",
      "Ejo hazaza (Tomorrow)",
      "What Rwanda loses after every harvest, who pays for it, what it will cost to 2031, and the operations-research plan that wins it back.",
      "EJO HAZAZA"
    ),
    tags$div(
      class = "hv-room step-scope ik-room ej-room",
      tags$div(
      class = "ej-desk",
      tags$nav(
        class = "ej-side",
        lapply(names(ej_steps), function(n) tags$button(type = "button", class = paste("hv-step-btn", if (n == "0") "is-on"), `data-step` = n, tags$b(n), ej_steps[[n]]))
      ),
      tags$div(
      class = "ej-desk-main",

      tags$div(
        class = "hv-step is-on", `data-step` = "0",
        tags$div(
          class = "hv-step-head",
          tags$p(class = "kicker", "OVERVIEW \u00b7 THE HARVEST THAT NEVER REACHES A PLATE"),
          tags$h2("Money left on the table every year"),
          tags$p("Umusaruro says how much Rwanda harvests. Isoko gives the price. Ikigega shows what is stored and lost. Ejo hazaza multiplies them: tonnes lost \u00d7 price = francs lost, for every product in the FAO food balance, split into six causes, from 1996 and forecast to 2031.")
        ),
        ejo_inspect_ui("over"),
        uiOutput("ej_over_cards"),
        ej_top("over"),
        tags$div(
          class = "ej-dash-stack",
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Money lost by cause, 1996\u20132031 (red) and gained with the plan (blue)"), plotlyOutput("ej_over_plot", height = "360px")),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Who pays: farmers against government"), plotlyOutput("ej_over_who_plot", height = "300px")),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Where the loss happens"), plotlyOutput("ej_over_causes", height = "320px")),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Tonnes of food lost each year"), plotlyOutput("ej_over_tonnes", height = "300px")),
          tags$div(class = "chart-card", uiOutput("ej_over_items_title"), plotlyOutput("ej_over_items", height = "360px")),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Cause mix over time (% of money lost)"), plotlyOutput("ej_over_share", height = "320px")),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Top products by farm-gate loss (share of year)"), plotlyOutput("ej_over_items_share", height = "320px")),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Money gained with the plan by cause"), plotlyOutput("ej_over_gain", height = "360px")),
          uiOutput("ej_over_table"),
          uiOutput("ej_over_data")
        ),
        ej_step_bar(0)
      ),

      ejo_cause_ui(
        "transport", 4, "Produce that travels to Kigali and back",
        "Tomatoes grown in Huye, Nyaruguru and Gisagara go up to Nyabugogo; Huye traders then buy them back and carry them home. Every extra kilometre, hour and loading costs francs and crushed fruit. The transportation model finds the cheapest district-to-market plan for any product.",
        list(
          selectInput("ej_tr_product", "PRODUCT", choices = ejo_product_choices, selected = sel_prod, selectize = FALSE),
          selectInput("ej_tr_origin", "GROWN IN (DISTRICT)", choices = dist_ch, selected = "Huye", selectize = FALSE),
          selectInput("ej_tr_via", "FIRST SOLD AT", choices = mk_ch, selected = if ("Nyabugogo" %in% mk_ch) "Nyabugogo" else mk_ch[1], selectize = FALSE),
          selectInput("ej_tr_home", "LAST SOLD AT", choices = mk_ch, selected = if ("Butare" %in% mk_ch) "Butare" else mk_ch[1], selectize = FALSE)
        ),
        tagList(
          uiOutput("ej_tr_cards"),
          tags$div(class = "chart-card map-card", uiOutput("ej_tr_map_title"), if (has_leaflet) leaflet::leafletOutput("ej_tr_map", height = "560px")),
          tags$div(class = "chart-card", uiOutput("ej_tr_side")),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Where the grower earns most, after transport and spoilage"), plotlyOutput("ej_tr_best", height = "420px")),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Optimal flows: district to market (tonnes a year)"), plotlyOutput("ej_tr_sankey", height = "420px")),
          uiOutput("ej_tr_flows")
        )
      ),

      ejo_cause_ui(
        "storage", 2, "Grain that rots, moulds and feeds weevils",
        "Most stored-grain loss happens in the house and the market store: moisture, aflatoxin and insects. APHLIS measures it by stage. The p-median model places cooperative warehouses and dryers where they serve the most harvest over the shortest roads.",
        list(
          selectInput("ej_st_crop", "CROP", choices = c("Maize", "Bean", "Paddy rice", "Sorghum", "Wheat", "Soybean", "Groundnuts", "Pea"), selected = "Maize"),
          sliderInput("ej_st_k", "NUMBER OF WAREHOUSES", 2, 12, 5, step = 1)
        ),
        tagList(
          uiOutput("ej_st_cards"),
          tags$div(class = "chart-card map-card", tags$h3(class = "card-title", "Red: storage loss by district. Blue: optimal warehouse sites and the districts they serve"), if (has_leaflet) leaflet::leafletOutput("ej_st_map", height = "560px")),
          tags$div(class = "chart-card", uiOutput("ej_st_side")),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "APHLIS: where cereal is lost, by stage (%)"), plotlyOutput("ej_st_aphlis", height = "400px")),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "APHLIS: total dry-weight loss by year (%)"), plotlyOutput("ej_st_aphlis_trend", height = "400px"))
        )
      ),

      ejo_cause_ui(
        "weather", 1, "Rain at harvest and nowhere to dry",
        "When it rains in the harvest months, maize and beans stay wet on the field and in the yard; mould and sprouting follow. NASA POWER gives 24 years of harvest-month rain for every district. The Monte Carlo model draws thousands of seasons to price the risk, and the decision table picks the best defence.",
        list(
          selectInput("ej_we_season", "HARVEST SEASON", choices = c("Season A (harvest Jan\u2013Feb)" = "A", "Season B (harvest Jun\u2013Jul)" = "B"), selected = "A"),
          selectInput("ej_we_group", "CROPS", choices = c("Cereals" = "cereals", "Pulses" = "pulses", "Roots and tubers" = "roots"), selected = "cereals"),
          sliderInput("ej_we_n", "SIMULATED SEASONS", 500, 8000, 1000, step = 500),
          selectInput("ej_we_district", "DISTRICT RAIN LINE", choices = dist_ch, selected = "Musanze")
        ),
        tagList(
          uiOutput("ej_we_cards"),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Simulated weather loss: do nothing (red) and with the plan (blue)"), plotlyOutput("ej_we_hist", height = "420px")),
          tags$div(class = "chart-card map-card", tags$h3(class = "card-title", "Chance of a wet harvest (rain 25% above normal)"), if (has_leaflet) leaflet::leafletOutput("ej_we_map", height = "480px")),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Harvest-month rain by year"), plotlyOutput("ej_we_line", height = "360px")),
          uiOutput("ej_we_emv")
        )
      ),

      ejo_cause_ui(
        "cold", 5, "Fresh produce waiting in the heat",
        "Tomatoes, avocados, French beans and milk lose value every hour they wait in the sun. Export lots that miss a cooled dispatch within 48 hours are rejected. The M/M/c queue model sizes cold rooms per district: enough that produce rarely waits, not so many that rooms sit empty.",
        list(
          selectInput("ej_co_district", "DISTRICT", choices = dist_ch, selected = hort_rank[1]),
          sliderInput("ej_co_lot", "LOT SIZE (TONNES)", 0.5, 5, ejo_p("lot_t"), step = 0.5),
          sliderInput("ej_co_cap", "COLD ROOM THROUGHPUT (TONNES A DAY)", 2, 20, ejo_p("room_t_day"), step = 1),
          sliderInput("ej_co_cost", "COLD ROOM COST (RWF M A YEAR)", 3, 30, ejo_p("room_cost_rwf_m"), step = 1)
        ),
        tagList(
          uiOutput("ej_co_cards"),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Cost of waiting (red) against cost of rooms (blue)"), plotlyOutput("ej_co_curve", height = "420px")),
          tags$div(class = "chart-card map-card", tags$h3(class = "card-title", "Optimal cold rooms per district"), if (has_leaflet) leaflet::leafletOutput("ej_co_map", height = "480px")),
          uiOutput("ej_co_table")
        )
      ),

      ejo_cause_ui(
        "glut", 6, "Everyone harvests at once and prices crash",
        "When a whole province harvests in the same weeks, markets flood, prices fall and unsold produce rots. NAEB/Esoko prices show the monthly swing for every product. The multi-period model decides how much to sell now and how much to store and sell later.",
        list(
          selectizeInput("ej_gl_product", "PRODUCT", choices = ejo_product_choices, selected = glut_prod),
          selectizeInput("ej_gl_harvest", "HARVEST MONTHS", choices = setNames(1:12, month.abb), selected = c(1, 2, 6, 7), multiple = TRUE),
          sliderInput("ej_gl_store", "STORAGE COST (RWF PER TONNE A MONTH)", 0, 20000, ejo_p("store_rwf_t_month"), step = 500),
          sliderInput("ej_gl_delta", "LOSS IN STORE PER MONTH (%)", 0, 40, 5, step = 1)
        ),
        tagList(
          uiOutput("ej_gl_cards"),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Price by month, sales at harvest today (red) and the optimal plan (blue)"), plotlyOutput("ej_gl_plot", height = "420px"))
        )
      ),

      ejo_cause_ui(
        "handling", 3, "Crushed in sacks, bruised in baskets",
        "Overloaded sacks, rough loading and no sorting crush the produce long before it spoils. Pareto analysis shows the few products that carry most of the handling loss; break-even analysis shows where plastic crates pay for themselves.",
        list(
          sliderInput("ej_ha_crate", "CRATE PRICE (RWF)", 2000, 15000, ejo_p("crate_rwf"), step = 500),
          sliderInput("ej_ha_sack", "DAMAGE IN SACKS (%)", 2, 30, 100 * ejo_p("damage_sack"), step = 1),
          sliderInput("ej_ha_crt", "DAMAGE IN CRATES (%)", 0, 15, 100 * ejo_p("damage_crate"), step = 1)
        ),
        tagList(
          uiOutput("ej_ha_cards"),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Pareto: handling loss by product, 2026"), plotlyOutput("ej_ha_pareto", height = "440px")),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Crates: francs saved per franc spent"), plotlyOutput("ej_ha_bcr", height = "440px"))
        )
      ),

      tags$div(
        class = "hv-step", `data-step` = "7",
        tags$div(
          class = "hv-step-head",
          tags$p(class = "kicker", "8 \u00b7 ACTION PLAN \u00b7 SOLUTIONS"),
          tags$h2("How each solution is carried out"),
          tags$p("Open an option to see how that solution is carried out. The first institution named on it is the team lead. The others work with the lead, do the activities the plan already assigns them, and send the report the lead receives.")
        ),
        ej_top("ap", sliderInput("ej_ap_budget", "NATIONAL BUDGET (RWF BN, CAPITAL)", 5, 150, 60, step = 5)),
        tags$div(
          class = "ej-main",
          uiOutput("ej_ap_cards"),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Each year: lost if nothing changes (red), gained with the plan (blue)"), plotlyOutput("ej_ap_plot", height = "420px")),
          ej_sec("1", "Implementation schedule", "Dark rows are the critical path. A delay there delays that cause. Months use the PERT duration in ejo_action_plans.csv, counted from January 2026."),
          uiOutput("ej_ap_schedule"),
          tags$div(
            id = "ejo-report",
            ej_sec("2", "Solutions", "Open one for the partner reports, funding clearance (Government / NGO / Institutional Investor), the six-step institutional framework, proposal download, and automated email to Report-goes-to institutions."),
            uiOutput("ej_ap_table")
          )
        ),
        ej_detail_box("ap", "the action plan"),
        ej_step_bar(7)
      )
      )
      )
    ),
    site_footer()
    )
  ),
  if (exists("ejo_action_ui", mode = "function")) ejo_action_ui()
  else if (exists("ejo_portal_ui", mode = "function")) ejo_portal_ui()
  else tags$section(id = "ejo-solution", class = "panel")
  )
}

# ---- server helpers ----
ejo_plot <- function(p, ytitle = "RWF billion") {
  hv_plot_layout(p, ytitle) |>
    plotly::layout(
      legend = list(orientation = "h", y = -0.28, x = 0, xanchor = "left", font = list(size = 10),
                    bgcolor = "rgba(255,255,255,0.85)", borderwidth = 0),
      margin = list(l = 56, r = 18, t = 36, b = 88),
      autosize = TRUE
    ) |>
    plotly::config(responsive = TRUE, displayModeBar = FALSE)
}
ejo_kind_year <- local({
  k <- tapply(ejo_loss_items$kind, ejo_loss_items$year, function(z) names(sort(table(z), decreasing = TRUE))[1])
  setNames(as.character(k), names(k))
})
ejo_who_val <- function(who) {
  w <- if (is.null(who) || !nzchar(as.character(who)[1])) "both" else as.character(who)[1]
  if (!w %in% c("both", "farmer", "gov")) "both" else w
}
ejo_loss_plot <- function(cy, y = 2026, f = 2031, who = "both") {
  cy <- cy[order(cy$year), , drop = FALSE]
  who <- ejo_who_val(who)
  est <- cy$year > 2023
  pat <- list(shape = ifelse(est, "/", ""), fgcolor = "rgba(255,255,255,0.55)", size = 6)
  edge <- list(color = ifelse(cy$year %in% c(as.numeric(y), as.numeric(f)), "#111827", "rgba(0,0,0,0)"), width = ifelse(cy$year %in% c(as.numeric(y), as.numeric(f)), 2.5, 0))
  src <- ejo_kind_year[as.character(cy$year)]
  p <- plotly::plot_ly()
  if (who %in% c("both", "farmer")) {
    p <- plotly::add_bars(p, x = cy$year, y = cy$farmer / 1e9, name = "Farmers lose", marker = list(color = ejo_red, pattern = pat, line = edge), customdata = src,
                          hovertemplate = "%{x}: %{y:.2f} bn RWF<br>%{customdata}<extra>Farmers lose</extra>")
  }
  if (who %in% c("both", "gov")) {
    p <- plotly::add_bars(p, x = cy$year, y = cy$gov / 1e9, name = "Government loses", marker = list(color = ejo_red2, pattern = pat, line = edge), customdata = src,
                          hovertemplate = "%{x}: %{y:.2f} bn RWF<br>%{customdata}<extra>Government loses</extra>")
  }
  p |>
    plotly::layout(barmode = "stack", legend = list(orientation = "h", y = -0.18),
                   shapes = list(list(type = "line", x0 = 2023.5, x1 = 2023.5, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#7a867e", dash = "dot")),
                                 list(type = "line", x0 = 2026.5, x1 = 2026.5, y0 = 0, y1 = 1, yref = "paper", line = list(color = ejo_red, dash = "dash"))),
                   annotations = list(list(x = 2009.5, y = 1.07, yref = "paper", text = "FAO FOOD BALANCES 1996\u20132023", showarrow = FALSE, font = list(color = "#7a867e", size = 11)),
                                      list(x = 2025, y = 1.07, yref = "paper", text = "ESTIMATE", showarrow = FALSE, font = list(color = "#7a867e", size = 10)),
                                      list(x = 2029, y = 1.07, yref = "paper", text = "FORECAST 2027\u20132031", showarrow = FALSE, font = list(color = ejo_red, size = 11)))) |>
    ejo_plot()
}
ejo_lead_parts <- function(s) {
  parts <- trimws(unlist(strsplit(as.character(s), ";", fixed = TRUE)))
  parts[nzchar(parts)]
}
ejo_option_work <- function(op) {
  parts <- ejo_lead_parts(op$lead)
  acts <- ejo_pm[ejo_pm$cause == op$cause, , drop = FALSE]
  if (!nrow(acts) || !length(parts)) return(acts)
  keep <- vapply(acts$lead, function(ld) any(tolower(ejo_lead_parts(ld)) %in% tolower(parts)), logical(1))
  acts[keep, , drop = FALSE]
}
ejo_option_why <- function(op, ap_row = NULL) {
  cause <- op$cause
  why_cause <- c(
    weather = "Rain at harvest leaves grain wet on the field and in the yard; mould and sprouting follow. This solution cuts that weather loss where cooperatives can dry the crop before it spoils.",
    storage = "Grain and pulses spoil when moisture and pests stay in store. This solution places drying and storage where the loss is highest so the harvest keeps its value.",
    handling = "Rough packing and unloading bruise and spill produce before it reaches the buyer. This solution hardens handling so less of the harvest is lost on the way.",
    transport = "Long detours and weak corridors raise time, cost and spoilage on the road. This solution shortens or protects the route so more of the harvest arrives.",
    cold = "Heat after harvest ruins milk, vegetables and export horticulture. This solution adds cold capacity sized to the waiting crop.",
    glut = "When everyone harvests together, prices fall and unsold stock rots. This solution spreads sales, processes surplus or finds buyers so the glut does not become loss."
  )
  head <- unname(why_cause[[cause]])
  if (is.null(head) || !nzchar(head)) head <- paste("This solution addresses", ejo_causes[[cause]], ".")
  basis <- if (!is.null(op$basis) && nzchar(as.character(op$basis)[1])) as.character(op$basis)[1] else ""
  or_line <- paste0("Operations research on this cause: ", ejo_or[[cause]], ".")
  effect_line <- if (!is.null(ap_row) && nrow(ap_row) && is.finite(ap_row$effect[1])) {
    paste0("At full roll-out it is planned to cut this cause's loss by ", fmt_num(100 * ap_row$effect[1], 0), "%.")
  } else ""
  paste(c(head, or_line, if (nzchar(basis)) paste0("Evidence on file: ", basis, ".") else NULL, effect_line), collapse = " ")
}

ejo_login_card <- function(user = NULL) {
  if (!is.null(user)) {
    return(tags$div(
      class = "chart-card ej-login-bar",
      tags$p(class = "note", paste0(
        "Signed in as ", user$display_name, " (", user$institution, " · ", user$role, "). ",
        "Reports and milestones write to the SQLite project database under this account."
      ))
    ))
  }
  tags$div(
    class = "chart-card ej-login",
    tags$p(class = "kicker", "INSTITUTION DESK"),
    tags$h3("Sign in on the green government portal"),
    tags$p(class = "note", "Use the left/right login screen (username + password 123456). Then this desk unlocks funding, the six-step framework, and partner reports.")
  )
}

ejo_option_portal <- function(id, log, user = NULL, state = NULL, db_reports = NULL) {
  op <- ejo_options[ejo_options$id == id, , drop = FALSE]
  if (!nrow(op)) return(hv_empty("That option is not in the plan file."))
  op <- op[1, ]
  parts <- ejo_lead_parts(op$lead)
  if (!length(parts)) return(hv_empty("This option has no lead in the plan file."))
  lead <- parts[1]
  partners <- if (length(parts) > 1) parts[-1] else character()
  work <- ejo_option_work(op)
  where <- unname(ejo_where[[op$cause]])
  months <- format(seq(as.Date("2026-01-01"), as.Date("2031-12-01"), by = "month"), "%b %Y")
  ap <- tryCatch(ejo_appraisals[[op$cause]], error = function(e) NULL)
  ap_row <- if (!is.null(ap) && nrow(ap)) ap[ap$id == op$id, , drop = FALSE] else NULL
  why <- ejo_option_why(op, ap_row)
  glance <- tags$div(
    class = "mega-row four glance ej-sol-glance",
    tags$article(
      class = "lux-card",
      tags$span("Cuts this loss by"),
      tags$strong(if (!is.null(ap_row) && nrow(ap_row) && is.finite(ap_row$effect[1])) paste0(fmt_num(100 * ap_row$effect[1], 0), "%") else "\u2014"),
      tags$p(ejo_causes[[op$cause]])
    ),
    tags$article(
      class = "lux-card",
      tags$span("Gain 2026\u20132031"),
      tags$strong(if (!is.null(ap_row) && nrow(ap_row)) fmt_bn(ap_row$gain[1]) else "\u2014"),
      tags$p("Money saved with this solution")
    ),
    tags$article(
      class = "lux-card",
      tags$span("Net present value"),
      tags$strong(if (!is.null(ap_row) && nrow(ap_row)) fmt_bn(ap_row$npv[1]) else "\u2014"),
      tags$p(paste0("Capital ", fmt_num(op$capex_bn, 1), " bn \u00b7 running ", fmt_num(op$opex_bn, 1), " bn / yr"))
    ),
    tags$article(
      class = "lux-card",
      tags$span("Who leads"),
      tags$strong(lead),
      tags$p(if (length(partners)) paste("With", paste(partners, collapse = ", ")) else "Sole lead on this solution")
    )
  )
  partner_block <- if (!length(partners)) {
    tags$p(class = "note", paste0(lead, " is the only institution named on this option, so the file has no separate partner to report to them."))
  } else {
    lapply(partners, function(p) {
      mine <- work[vapply(work$lead, function(ld) tolower(p) %in% tolower(ejo_lead_parts(ld)), logical(1)), , drop = FALSE]
      tags$div(
        class = "chart-card",
        tags$h3(p),
        tags$p(class = "note", paste("Works with the team lead,", lead, ".")),
        if (!nrow(mine)) tags$p(class = "note", "The plan names this partner on the option and does not assign them a separate activity. They report on the option as a whole.")
        else tags$table(
          class = "hv-table",
          tags$thead(tags$tr(tags$th("What they perform"), tags$th("When"), tags$th("Where"), tags$th("Report goes to"))),
          tags$tbody(lapply(seq_len(nrow(mine)), function(i) tags$tr(
            tags$td(tags$b(mine$id[i]), " ", mine$activity[i]),
            tags$td(paste0(mine$start[i], " \u2013 ", mine$finish[i])),
            tags$td(where),
            tags$td(lead)
          )))
        )
      )
    })
  }
  inbox_csv <- log[log$option_id == id, , drop = FALSE]
  inbox_db <- if (!is.null(db_reports) && nrow(db_reports)) db_reports else data.frame()
  step_choices <- stats::setNames(
    vapply(ejo_inst_steps, `[[`, character(1), "id"),
    paste0("Step ", vapply(ejo_inst_steps, `[[`, character(1), "id"), " · ", vapply(ejo_inst_steps, `[[`, character(1), "title"))
  )
  step0 <- if (!is.null(state$framework_step) && state$framework_step %in% unname(step_choices)) state$framework_step else "1"
  pm0 <- if (!is.null(state$pm_status) && state$pm_status %in% names(ejo_pm_statuses)) state$pm_status else "on_track"
  who_choices <- unique(c(lead, partners))
  who0 <- if (!is.null(user) && user$institution %in% who_choices) user$institution else who_choices[1]
  mine0 <- if (nrow(work)) work[vapply(work$lead, function(ld) tolower(who0) %in% tolower(ejo_lead_parts(ld)), logical(1)), , drop = FALSE] else work
  work_ch <- if (nrow(mine0)) {
    stats::setNames(paste(mine0$id, mine0$activity), paste(mine0$id, mine0$activity))
  } else {
    stats::setNames(op$option, op$option)
  }
  report_block <- if (is.null(user)) {
    tags$div(
      class = "chart-card",
      tags$h3(paste("Report to", lead)),
      tags$p(class = "note", "Sign in above to file a progress report. Each report locks to the active six-step framework stage and funding clearance.")
    )
  } else {
    tags$div(
      class = "chart-card",
      tags$h3(paste("Report to", lead, "· linked to framework")),
      tags$p(class = "note", "This form writes to SQLite and inherits the framework step, funding body, and clearance you set in the institutional workflow below."),
      tags$div(
        class = "price-filters hv-filters",
        selectInput("ej_opt_who", "WHO IS REPORTING", choices = who_choices, selected = who0, width = "100%"),
        selectInput("ej_opt_work", "WHAT THEY PERFORMED", choices = work_ch, selected = unname(work_ch)[1], width = "100%"),
        selectInput("ej_opt_where", "WHERE", choices = c("National", sort(ejo_districts)), selected = "National", width = "100%"),
        selectInput("ej_opt_when", "MONTH", choices = months, selected = format(Sys.Date(), "%b %Y"), width = "100%")
      ),
      selectInput("ej_opt_step", "FRAMEWORK STEP THIS REPORT COVERS", choices = step_choices, selected = step0, width = "100%"),
      selectInput("ej_opt_pm", "PM STATUS", choices = ejo_pm_statuses, selected = pm0, width = "100%"),
      textInput("ej_opt_mile", "MILESTONE HIT (OPTIONAL)", value = "", width = "100%", placeholder = "e.g. RFQ issued · Steering Committee met"),
      textInput("ej_opt_next", "NEXT ACTION", value = "", width = "100%", placeholder = "What must happen next"),
      textInput("ej_opt_next_owner", "NEXT OWNER", value = lead, width = "100%"),
      textInput("ej_opt_next_when", "NEXT DUE", value = "", width = "100%", placeholder = "e.g. Apr 2026"),
      textAreaInput("ej_opt_note", "WHAT WAS DONE", rows = 3, width = "100%", placeholder = "What this partner finished"),
      textInput("ej_opt_tonnes", "MEASURED TONNES, IF THERE IS A FIGURE", value = "", width = "100%"),
      actionButton("ej_opt_send", paste("Send the report to", lead), class = "btn-go"),
      uiOutput("ej_opt_done")
    )
  }
  db_inbox_ui <- if (!is.null(user) && nrow(inbox_db)) {
    tags$div(
      class = "chart-card",
      tags$h3(paste("Database trail for", lead)),
      tags$table(
        class = "hv-table",
        tags$thead(tags$tr(
          tags$th("Ref"), tags$th("Step"), tags$th("PM"), tags$th("From"), tags$th("Work"),
          tags$th("Next action"), tags$th("Note")
        )),
        tags$tbody(lapply(seq_len(min(nrow(inbox_db), 20)), function(i) tags$tr(
          tags$td(inbox_db$ref[i]),
          tags$td(paste0(inbox_db$framework_step[i], " · ", inbox_db$framework_title[i])),
          tags$td(inbox_db$pm_status[i]),
          tags$td(paste0(inbox_db$filed_by[i], " (", inbox_db$institution[i], ")")),
          tags$td(inbox_db$activity[i]),
          tags$td(if (nzchar(inbox_db$next_action[i])) inbox_db$next_action[i] else "\u2014"),
          tags$td(inbox_db$note[i])
        )))
      )
    )
  } else NULL
  tags$div(
    class = "ej-sol-page",
    tags$div(
      class = "ej-detail-head",
      tags$p(class = "kicker", paste("SOLUTION", id, "\u00b7", toupper(ejo_causes[[op$cause]]))),
      tags$h2(op$option),
      tags$p(class = "ej-lead", "Why this solution: ", tags$span(why))
    ),
    ejo_login_card(user),
    tags$h3(class = "ej-sol-glance-title", "At a glance"),
    glance,
    tags$p(class = "note ej-sol-where", paste0("Where it runs: ", where, ".")),
    partner_block,
    report_block,
    db_inbox_ui,
    tags$div(
      class = "chart-card",
      tags$h3(paste("CSV inbox ·", lead)),
      if (!nrow(inbox_csv)) tags$p(class = "note", paste(lead, "has not received a CSV report on this option yet."))
      else tags$table(
        class = "hv-table",
        tags$thead(tags$tr(tags$th("Reference"), tags$th("From"), tags$th("Work"), tags$th("Where"), tags$th("Month"), tags$th("Tonnes"), tags$th("What was done"))),
        tags$tbody(lapply(seq_len(nrow(inbox_csv)), function(i) tags$tr(
          tags$td(inbox_csv$ref[i]), tags$td(inbox_csv$filed_by[i]), tags$td(inbox_csv$activity[i]), tags$td(inbox_csv$where[i]),
          tags$td(inbox_csv$when[i]), tags$td(if (nzchar(inbox_csv$tonnes[i]) && inbox_csv$tonnes[i] != "NA") inbox_csv$tonnes[i] else "\u2014"), tags$td(inbox_csv$note[i])
        )))
      )
    ),
    if (!is.null(user) && exists("ejo_inst_workflow_ui", mode = "function")) ejo_inst_workflow_ui(op, state)
    else if (is.null(user)) tags$div(
      class = "chart-card",
      tags$h3("Institutional workflow"),
      tags$p(class = "note", "Sign in to open funding clearance, the six-step framework, milestones, and automated ministry reports for this solution.")
    ) else NULL
  )
}

ej_act_pill <- function(scope, id, badge, title, lead, meta, cost, critical = FALSE) {
  is_sol <- id %in% ejo_options$id
  tags$button(
    type = "button",
    class = paste("ej-act", if (isTRUE(critical)) "is-crit"),
    `data-scope` = scope,
    `data-act` = id,
    `data-kind` = if (is_sol) "solution" else "activity",
    tags$b(class = "ej-act-id", badge),
    tags$span(class = "ej-act-body", tags$strong(title), tags$em(paste("Who leads:", lead)), tags$small(paste(meta, collapse = " \u00b7 "))),
    tags$span(class = "ej-act-cost", cost),
    tags$span(class = "force-go", "\u2192")
  )
}
ejo_year_row <- function(cy, yr) {
  r <- cy[cy$year == as.numeric(yr), , drop = FALSE]
  if (!nrow(r)) return(NULL)
  r
}
ejo_gain_plot <- function(g) {
  plotly::plot_ly() |>
    plotly::add_bars(x = g$year, y = g$loss / 1e9, name = "Lost if nothing changes", marker = list(color = ejo_red),
                     text = fmt_num(g$loss / 1e9, 2), textposition = "outside", hovertemplate = "%{x}: RWF %{y:.2f} bn<extra>Lost</extra>") |>
    plotly::add_bars(x = g$year, y = g$gain / 1e9, name = "Gained with the plan", marker = list(color = ejo_blue),
                     text = fmt_num(g$gain / 1e9, 2), textposition = "outside", hovertemplate = "%{x}: RWF %{y:.2f} bn<extra>Gained</extra>") |>
    plotly::add_trace(x = g$year, y = (g$loss - g$gain) / 1e9, type = "scatter", mode = "lines+markers", name = "Still lost",
                      line = list(color = "#7f1d1d", dash = "dash"), marker = list(color = "#7f1d1d")) |>
    plotly::layout(barmode = "group") |>
    ejo_plot()
}
ejo_year_table <- function(g) {
  tags$div(
    class = "chart-card",
    tags$table(
      class = "hv-table ej-table",
      tags$thead(tags$tr(tags$th("Year"), tags$th("Farmers lose"), tags$th("Government loses"), tags$th("Total lost"),
                         tags$th("Gained with plan"), tags$th("Plan cost"), tags$th("Still lost"), tags$th("Plan running"))),
      tags$tbody(lapply(seq_len(nrow(g)), function(i) tags$tr(
        tags$td(g$year[i]),
        tags$td(class = "ej-red", fmt_bn(g$farmer[i])), tags$td(class = "ej-red2", fmt_bn(g$gov[i])), tags$td(class = "ej-red", tags$b(fmt_bn(g$loss[i]))),
        tags$td(class = "ej-blue", tags$b(fmt_bn(g$gain[i]))), tags$td(class = "ej-green", fmt_bn(g$cost[i])), tags$td(fmt_bn(g$loss[i] - g$gain[i])), tags$td(paste0(fmt_num(100 * ejo_adopt[[i]], 0), "%"))
      ))),
      tags$tfoot(tags$tr(tags$td(tags$b("2026\u20132031")), tags$td(class = "ej-red", fmt_bn(sum(g$farmer))), tags$td(class = "ej-red2", fmt_bn(sum(g$gov))),
                         tags$td(class = "ej-red", tags$b(fmt_bn(sum(g$loss)))), tags$td(class = "ej-blue", tags$b(fmt_bn(sum(g$gain)))),
                         tags$td(class = "ej-green", fmt_bn(sum(g$cost))), tags$td(fmt_bn(sum(g$loss - g$gain))), tags$td("")))
    )
  )
}
ejo_gantt <- function(cp) {
  pl <- cp$plan
  pl <- pl[rev(seq_len(nrow(pl))), , drop = FALSE]
  lab <- paste0(pl$id, " \u00b7 ", pl$activity)
  plotly::plot_ly() |>
    plotly::add_bars(y = lab, x = pl$dur, base = pl$es, orientation = "h", name = "Activity",
                     marker = list(color = ifelse(pl$critical, "#1e3a8a", "#93c5fd")),
                     text = paste0(fmt_num(pl$dur, 1), " mo"), textposition = "auto", textangle = 0, constraintext = "none",
                     hovertemplate = paste0("%{y}<br>Start month ", fmt_num(pl$es, 1), ", end ", fmt_num(pl$ef, 1), "<br>Slack ", fmt_num(pl$slack, 1), " months<br>", pl$lead, "<extra></extra>")) |>
    plotly::add_bars(y = lab, x = pl$slack, base = pl$ef, orientation = "h", name = "Slack", marker = list(color = "rgba(150,150,150,0.25)"), hoverinfo = "skip") |>
    plotly::layout(barmode = "overlay", showlegend = FALSE, yaxis = list(title = NULL, automargin = TRUE), xaxis = list(title = "Months from start")) |>
    hv_plot_layout("", "Months from start")
}

ejo_server <- function(input, output, session) {
  ejo_inspect_server(input, output, session)
  if (exists("ejo_action_server", mode = "function")) ejo_action_server(input, output, session)
  ejo_step_num <- c(weather = "1", storage = "2", handling = "3", transport = "4", cold = "5", glut = "6")
  ejo_step_on <- function(cause) {
    s <- input$ej_active_step
    if (is.null(s) || !nzchar(as.character(s)[1])) return(FALSE)
    identical(as.character(s)[1], as.character(ejo_step_num[[cause]]))
  }
  # Only compute overview when the user is actually on Ejo step 0.
  # (null step used to mean "on", which ran every overview chart on Home load.)
  ejo_over_on <- function() {
    s <- input$ej_active_step
    !is.null(s) && identical(as.character(s)[1], "0")
  }
  yr_h <- function(s) {
    v <- input[[paste0("ej_", s, "_y")]]
    if (is.null(v) || !length(v)) return(2026L)
    y <- suppressWarnings(as.integer(as.character(v)[1]))
    if (!length(y) || !is.finite(y[1])) 2026L else as.integer(y[1])
  }
  yr_f <- function(s) {
    v <- input[[paste0("ej_", s, "_f")]]
    if (is.null(v) || !length(v)) return(2031L)
    y <- suppressWarnings(as.integer(as.character(v)[1]))
    if (!length(y) || !is.finite(y[1])) 2031L else as.integer(y[1])
  }
  sel <- lapply(setNames(names(ejo_causes), names(ejo_causes)), function(k) reactive({
    v <- input[[paste0("ej_", k, "_opts")]]
    if (is.null(v)) character() else v
  }))
  gains <- lapply(setNames(names(ejo_causes), names(ejo_causes)), function(k) reactive(ejo_gain(k, sel[[k]]())))

  for (k in names(ejo_causes)) local({
    kk <- k
    observeEvent(input[[paste0("ej_", kk, "_budget")]], {
      b <- isolate(input[[paste0("ej_", kk, "_budget")]])
      req(isTruthy(b), is.finite(b))
      ids <- ejo_knapsack(ejo_appraisals[[kk]], b)
      cur <- isolate(input[[paste0("ej_", kk, "_opts")]])
      if (is.null(cur)) cur <- character()
      if (!identical(sort(as.character(ids)), sort(as.character(cur)))) {
        isolate(updateCheckboxGroupInput(session, paste0("ej_", kk, "_opts"), selected = ids))
      }
    }, ignoreInit = TRUE)

    output[[paste0("ej_", kk, "_loss_cards")]] <- renderUI({
      req(ejo_step_on(kk))
      y <- yr_h(kk)
      fy <- yr_f(kk)
      who <- ejo_who_val(input[[paste0("ej_", kk, "_who")]])
      cy <- ejo_cy[ejo_cy$cause == kk, , drop = FALSE]
      a <- ejo_year_row(cy, y)
      b <- ejo_year_row(cy, fy)
      f <- cy[cy$year %in% ejo_years, ]
      ally <- sum(ejo_cy$total[ejo_cy$year == y])
      if (is.null(a)) a <- data.frame(total = NA, farmer = NA, gov = NA, tonnes = NA)
      pick <- function(farmer, gov, total) {
        if (identical(who, "farmer")) farmer else if (identical(who, "gov")) gov else total
      }
      share <- if (is.finite(a$total) && ally > 0) paste0(fmt_num(100 * a$total / ally, 0), "% of national loss") else ejo_kind_year[[as.character(y)]]
      ej_dash_hero(
        ej_dash_kpi(fmt_bn(pick(a$farmer, a$gov, a$total)), paste("Lost in", y, "on this cause"), share, "coin"),
        ej_dash_kpi(fmt_bn(a$farmer), paste("Farmers lost in", y), "Farm-gate value of the lost harvest", "farmer"),
        ej_dash_kpi(fmt_bn(a$gov), paste("Government lost in", y), if (kk == "cold") "Public spend + export earnings" else "Public spending on lost harvest", "gov"),
        ej_dash_kpi(fmt_bn(pick(b$farmer, b$gov, b$total)), paste("Lost in", fy, "if nothing changes"), paste0("Forecast \u00b7 farmers ", fmt_bn(b$farmer)), "rain"),
        ej_dash_kpi(fmt_bn(pick(sum(f$farmer), sum(f$gov), sum(f$total))), "Lost 2026\u20132031 if nothing changes", "Six years, prices rising with inflation", "globe"),
        ej_dash_kpi(if (is.finite(a$tonnes)) paste(fmt_num(a$tonnes, 0), "t") else "\u2014", paste("Produce lost in", y), "Tonnes that never reach a plate", "tonne")
      )
    })
    output[[paste0("ej_", kk, "_loss_plot")]] <- plotly::renderPlotly({
      req(ejo_step_on(kk))
      ejo_loss_plot(ejo_cy[ejo_cy$cause == kk, , drop = FALSE], yr_h(kk), yr_f(kk), input[[paste0("ej_", kk, "_who")]])
    }) |> bindCache(kk, input[[paste0("ej_", kk, "_y")]], input[[paste0("ej_", kk, "_f")]], input[[paste0("ej_", kk, "_who")]], input$ej_active_step, cache = "app")

    output[[paste0("ej_", kk, "_share_plot")]] <- plotly::renderPlotly({
      req(ejo_step_on(kk))
      y <- yr_h(kk)
      who <- ejo_who_val(input[[paste0("ej_", kk, "_who")]])
      d <- ejo_cy[ejo_cy$year == y, , drop = FALSE]
      req(nrow(d) > 0)
      val <- if (identical(who, "farmer")) d$farmer else if (identical(who, "gov")) d$gov else d$total
      d$lab <- unname(ejo_causes[d$cause])
      d$col <- ifelse(d$cause == kk, ejo_red, "#c8d5cc")
      plotly::plot_ly(
        x = d$lab, y = val / 1e9, type = "bar",
        marker = list(color = d$col),
        hovertemplate = "%{x}: RWF %{y:.2f} bn<extra></extra>"
      ) |>
        plotly::layout(xaxis = list(title = "", tickangle = -25), showlegend = FALSE) |>
        hv_plot_layout(NULL, "RWF billion")
    }) |> bindCache(kk, input[[paste0("ej_", kk, "_y")]], input[[paste0("ej_", kk, "_who")]], input$ej_active_step, cache = "app")

    output[[paste0("ej_", kk, "_opts_table")]] <- renderUI({
      ap <- ejo_appraisals[[kk]]
      on <- ap$id %in% sel[[kk]]()
      ap <- ap[order(-ap$npv), , drop = FALSE]
      on <- ap$id %in% sel[[kk]]()
      tags$div(
        class = "chart-card",
        tags$table(
          class = "hv-table ej-table",
          tags$thead(tags$tr(tags$th(""), tags$th("Option"), tags$th("Who leads"), tags$th("Capital"), tags$th("Running / yr"),
                             tags$th("Cuts this loss by"), tags$th("Gain 2026\u20132031"), tags$th("Net present value"), tags$th("Benefit / cost"), tags$th("Pays back"))),
          tags$tbody(lapply(seq_len(nrow(ap)), function(i) tags$tr(
            class = if (on[i]) "is-on" else NULL,
            tags$td(if (on[i]) tags$span(class = "ej-tick", "\u2713") else ""),
            tags$td(tags$b(ap$id[i]), " ", ap$option[i]),
            tags$td(ap$lead[i]),
            tags$td(class = "ej-green", paste0(fmt_num(ap$capex_bn[i], 1), " bn RWF")),
            tags$td(class = "ej-green", paste0(fmt_num(ap$opex_bn[i], 1), " bn RWF")),
            tags$td(paste0(fmt_num(100 * ap$effect[i], 0), "%")),
            tags$td(class = "ej-blue", fmt_bn(ap$gain[i])),
            tags$td(class = if (ap$npv[i] >= 0) "ej-blue" else "ej-red", fmt_bn(ap$npv[i])),
            tags$td(if (is.finite(ap$bcr[i])) paste0(fmt_num(ap$bcr[i], 1), "\u00d7") else "\u2014"),
            tags$td(if (is.finite(ap$payback[i])) ap$payback[i] else "after 2031")
          )))
        ),
        tags$p(class = "note", "Gain = this cause's loss \u00d7 the option's effect \u00d7 the share rolled out that year. Net present value at ", fmt_num(100 * ejo_p("discount_rate"), 0),
               "% a year, after capital and running costs. Effects and costs are planning assumptions (ejo_options.csv) you can edit.")
      )
    })

    output[[paste0("ej_", kk, "_sols")]] <- renderUI({
      ap <- ejo_appraisals[[kk]]
      if (is.null(ap) || !nrow(ap)) return(hv_empty("No solutions loaded for this cause."))
      ap <- ap[order(-ap$npv), , drop = FALSE]
      on <- ap$id %in% sel[[kk]]()
      tags$div(
        class = "ej-acts",
        lapply(seq_len(nrow(ap)), function(i) ej_act_pill(
          kk, ap$id[i], ap$id[i], ap$option[i], ap$lead[i],
          c(
            if (on[i]) "In the knapsack plan" else "Not selected in budget",
            paste0("gain ", fmt_bn(ap$gain[i])),
            paste0("NPV ", fmt_bn(ap$npv[i]))
          ),
          paste0(fmt_num(ap$capex_bn[i], 1), " bn RWF"),
          on[i]
        )),
        tags$p(class = "note", "Each solution opens funding clearance, the six institutional steps, proposal download, and automated reports.")
      )
    })

    output[[paste0("ej_", kk, "_gain_cards")]] <- renderUI({
      g <- gains[[kk]]()
      fy <- yr_f(kk)
      gf <- g[g$year == fy, , drop = FALSE]
      npv <- sum((g$gain - g$cost) * ejo_disc(g$year))
      cum <- cumsum(g$gain - g$cost)
      tags$div(
        class = "kpi-row hv-kpis",
        hv_kpi(paste("Gained in", fy), fmt_bn(gf$gain), paste0(fmt_num(100 * gf$effect, 0), "% of that year's loss avoided"), "is-blue"),
        hv_kpi("Gained 2026\u20132031", fmt_bn(sum(g$gain)), paste0("Farmers ", fmt_bn(sum(g$gain_farmer)), " \u00b7 Government ", fmt_bn(sum(g$gain_gov))), "is-blue"),
        hv_kpi("Cost of the plan", fmt_bn(sum(g$cost)), "Capital plus running costs, 2026\u20132031", "is-green"),
        hv_kpi("Net present value", fmt_bn(npv), if (any(cum >= 0)) paste("Pays back in", g$year[which(cum >= 0)[1]]) else "Does not pay back by 2031", if (npv >= 0) "is-blue" else "is-red")
      )
    })
    output[[paste0("ej_", kk, "_gain_plot")]] <- plotly::renderPlotly({
      g <- gains[[kk]]()
      ejo_gain_plot(g)
    }) |> bindCache(kk, sel[[kk]](), input[[paste0("ej_", kk, "_f")]], cache = "app")
    output[[paste0("ej_", kk, "_year_table")]] <- renderUI(ejo_year_table(gains[[kk]]()))

    output[[paste0("ej_", kk, "_cpm_cards")]] <- renderUI({
      cp <- ejo_cpm(kk)
      if (is.null(cp)) return(hv_empty("No action plan rows for this cause."))
      T <- input[[paste0("ej_", kk, "_deadline")]]
      pz <- stats::pnorm((T - cp$E) / max(cp$sd, 0.01))
      crit <- cp$plan$id[cp$plan$critical]
      tags$div(
        class = "kpi-row hv-kpis",
        hv_kpi("Expected duration", paste(fmt_num(cp$E, 1), "months"), paste("Critical path:", paste(crit, collapse = " \u2192 "))),
        hv_kpi("Spread (\u03c3)", paste(fmt_num(cp$sd, 1), "months"), "From optimistic and pessimistic estimates"),
        hv_kpi(paste("Done within", T, "months"), paste0(fmt_num(100 * pz, 0), "%"), "PERT probability", if (pz >= 0.5) "is-blue" else "is-red"),
        hv_kpi("Budget of the plan", fmt_bn(cp$cost * 1e6), "Sum of all activities", "is-green")
      )
    })
    output[[paste0("ej_", kk, "_gantt")]] <- plotly::renderPlotly({
      cp <- ejo_cpm(kk)
      if (is.null(cp)) return(NULL)
      ejo_gantt(cp)
    }) |> bindCache(kk, cache = "app")
    output[[paste0("ej_", kk, "_acts")]] <- renderUI({
      cp <- ejo_cpm(kk)
      if (is.null(cp)) return(NULL)
      pl <- cp$plan
      tags$div(
        class = "ej-acts",
        lapply(seq_len(nrow(pl)), function(i) ej_act_pill(
          kk, pl$id[i], pl$id[i], pl$activity[i], pl$lead[i],
          c(paste(fmt_num(pl$dur[i], 1), "months"), paste0("month ", fmt_num(pl$es[i], 1), "\u2013", fmt_num(pl$ef[i], 1)),
            if (pl$critical[i]) "critical path" else paste(fmt_num(pl$slack[i], 1), "months slack"),
            if (is.na(pl$after[i]) || !nzchar(pl$after[i])) "starts first" else paste("after", gsub(";", ", ", pl$after[i]))),
          fmt_bn(pl$cost_m[i] * 1e6), pl$critical[i]
        ))
      )
    })
  })

  # ---- overview ----
  # Overview uses precomputed default-plan gains (instant). Cause pages still use live sel().
  all_gain <- reactive({
    req(ejo_over_on())
    ejo_gain_default
  })
  output$ej_over_cards <- renderUI({
    req(ejo_over_on())
    y <- yr_h("over")
    fy <- yr_f("over")
    who <- ejo_who_val(input$ej_over_who)
    a <- ejo_cy[ejo_cy$year == y, ]
    b <- ejo_cy[ejo_cy$year == fy, ]
    f <- ejo_cy[ejo_cy$year %in% ejo_years, ]
    ty <- sum(ejo_loss_items$loss_t[ejo_loss_items$year == y], na.rm = TRUE)
    na_or <- function(x) if (length(x) && sum(is.finite(x))) sum(x, na.rm = TRUE) else NA_real_
    pick <- function(farmer, gov, total) {
      if (identical(who, "farmer")) farmer else if (identical(who, "gov")) gov else total
    }
    lost_y <- pick(na_or(a$farmer), na_or(a$gov), na_or(a$total))
    lost_f <- pick(na_or(b$farmer), na_or(b$gov), na_or(b$total))
    lost_span <- pick(sum(f$farmer), sum(f$gov), sum(f$total))
    tot_y <- na_or(a$total)
    farm_pct <- if (is.finite(tot_y) && tot_y > 0) paste0(fmt_num(100 * na_or(a$farmer) / tot_y, 1), "%") else "\u2014"
    gov_pct <- if (is.finite(tot_y) && tot_y > 0) paste0(fmt_num(100 * na_or(a$gov) / tot_y, 1), "%") else "\u2014"
    ej_dash_hero(
      ej_dash_kpi(fmt_bn(lost_y), paste("Lost in", y), if (ty > 0) paste(fmt_num(ty, 0), "t of food") else "No tonnes for this year", "coin"),
      ej_dash_kpi(farm_pct, "Farmers' share of the loss", fmt_bn(na_or(a$farmer)), "farmer"),
      ej_dash_kpi(gov_pct, "Government's share of the loss", fmt_bn(na_or(a$gov)), "gov"),
      ej_dash_kpi(fmt_bn(lost_f), paste("Lost in", fy, "if nothing changes"), "Forecast year from the filters", "rain"),
      ej_dash_kpi(fmt_bn(lost_span), "Lost 2026\u20132031", "If nothing changes", "globe"),
      ej_dash_kpi(if (ty > 0) paste(fmt_num(ty, 0), "t") else "\u2014", paste("Produce lost in", y), "Tonnes that never reach a plate", "tonne")
    )
  })
  output$ej_over_plot <- plotly::renderPlotly({
    req(ejo_over_on())
    y <- yr_h("over")
    fy <- yr_f("over")
    who <- ejo_who_val(input$ej_over_who)
    p <- plotly::plot_ly()
    for (k in names(ejo_causes)) {
      cy <- ejo_cy[ejo_cy$cause == k, , drop = FALSE]
      cy <- cy[order(cy$year), ]
      yy <- if (identical(who, "farmer")) cy$farmer else if (identical(who, "gov")) cy$gov else cy$total
      p <- plotly::add_bars(
        p, x = cy$year, y = yy / 1e9, name = paste0(ejo_causes[[k]], " · lost"),
        legendgroup = k, offsetgroup = "lost",
        marker = list(color = ejo_reds[[k]], pattern = list(shape = ifelse(cy$year > 2023, "/", ""), fgcolor = "rgba(255,255,255,0.5)", size = 6)),
        hovertemplate = paste0("%{x}: %{y:.2f} bn RWF lost<extra>", ejo_causes[[k]], "</extra>")
      )
    }
    for (k in names(ejo_causes)) {
      gk <- ejo_gain_default[ejo_gain_default$cause == k, , drop = FALSE]
      gk <- gk[order(gk$year), , drop = FALSE]
      gy <- if (identical(who, "farmer")) gk$gain_farmer else if (identical(who, "gov")) gk$gain_gov else gk$gain
      p <- plotly::add_bars(
        p, x = gk$year, y = gy / 1e9, name = paste0(ejo_causes[[k]], " · gained"),
        legendgroup = k, offsetgroup = "gained",
        marker = list(color = ejo_blues[[k]]),
        hovertemplate = paste0("%{x}: %{y:.2f} bn RWF gained<extra>", ejo_causes[[k]], "</extra>")
      )
    }
    hl <- lapply(c(y, fy), function(v) list(type = "rect", x0 = v - 0.5, x1 = v + 0.5, y0 = 0, y1 = 1, yref = "paper", fillcolor = "rgba(17,24,39,0.06)", line = list(color = "#111827", width = 1)))
    p |>
      plotly::layout(
        barmode = "group", legend = list(orientation = "h", y = -0.28),
        shapes = c(list(list(type = "line", x0 = 2026.5, x1 = 2026.5, y0 = 0, y1 = 1, yref = "paper", line = list(color = ejo_red, dash = "dash"))), hl),
        annotations = list(list(x = 2029, y = 1.06, yref = "paper", text = "FORECAST", showarrow = FALSE, font = list(color = ejo_red, size = 11)))
      ) |>
      ejo_plot()
  }) |> bindCache(input$ej_over_y, input$ej_over_f, input$ej_over_who, cache = "app")
  output$ej_over_who_plot <- plotly::renderPlotly({
    req(ejo_over_on())
    d <- stats::aggregate(cbind(farmer, gov) ~ year, ejo_cy, sum)
    d <- d[order(d$year), ]
    plotly::plot_ly() |>
      plotly::add_bars(x = d$year, y = d$farmer / 1e9, name = "Farmers", marker = list(color = ejo_red)) |>
      plotly::add_bars(x = d$year, y = d$gov / 1e9, name = "Government", marker = list(color = ejo_red2)) |>
      plotly::layout(barmode = "stack", legend = list(orientation = "h", y = -0.18)) |>
      ejo_plot()
  })
  output$ej_over_gain <- plotly::renderPlotly({
    req(ejo_over_on())
    who <- ejo_who_val(input$ej_over_who)
    g <- ejo_gain_default
    p <- plotly::plot_ly()
    for (k in names(ejo_causes)) {
      gk <- g[g$cause == k, , drop = FALSE]
      gy <- if (identical(who, "farmer")) gk$gain_farmer else if (identical(who, "gov")) gk$gain_gov else gk$gain
      p <- plotly::add_bars(p, x = gk$year, y = gy / 1e9, name = ejo_causes[[k]], marker = list(color = ejo_blues[[k]]),
                            hovertemplate = paste0("%{x}: %{y:.2f} bn RWF<extra>", ejo_causes[[k]], "</extra>"))
    }
    p |> plotly::layout(barmode = "stack", legend = list(orientation = "h", y = -0.22)) |> ejo_plot()
  }) |> bindCache(input$ej_over_who, cache = "app")
  output$ej_over_tonnes <- plotly::renderPlotly({
    req(ejo_over_on())
    d <- stats::aggregate(tonnes ~ year, ejo_cy, sum)
    d <- d[order(d$year), ]
    plotly::plot_ly(x = d$year, y = d$tonnes / 1000, type = "scatter", mode = "lines+markers",
                    line = list(color = "#14804a", width = 3), marker = list(color = "#f8d308", size = 7),
                    hovertemplate = "%{x}: %{y:.1f} thousand tonnes<extra></extra>") |>
      hv_plot_layout(NULL, "Thousand tonnes lost")
  })
  output$ej_over_items_title <- renderUI({
    req(ejo_over_on())
    tags$h3(class = "card-title", paste("Products that lose the most money,", yr_h("over")))
  })
  output$ej_over_items <- plotly::renderPlotly({
    req(ejo_over_on())
    d <- ejo_loss_items[ejo_loss_items$year == yr_h("over"), , drop = FALSE]
    req(nrow(d) > 0)
    d <- d[order(-d$farmer), , drop = FALSE][1:min(15, nrow(d)), ]
    d <- d[rev(seq_len(nrow(d))), ]
    plotly::plot_ly(y = d$item, x = d$farmer / 1e9, type = "bar", orientation = "h", marker = list(color = ejo_red),
                    text = paste0(fmt_num(d$loss_t, 0), " t \u00b7 ", fmt_num(100 * d$rate, 1), "% lost"), textposition = "auto",
                    hovertemplate = "%{y}: RWF %{x:.2f} bn<extra></extra>") |>
      plotly::layout(yaxis = list(categoryorder = "array", categoryarray = d$item, automargin = TRUE)) |>
      hv_plot_layout(NULL, "RWF billion, farm-gate value")
  }) |> bindCache(input$ej_over_y, cache = "app")
  output$ej_over_items_share <- plotly::renderPlotly({
    req(ejo_over_on())
    d <- ejo_loss_items[ejo_loss_items$year == yr_h("over"), , drop = FALSE]
    req(nrow(d) > 0)
    d <- d[order(-d$farmer), , drop = FALSE][1:min(12, nrow(d)), ]
    tot <- sum(d$farmer, na.rm = TRUE)
    d$share <- if (tot > 0) 100 * d$farmer / tot else 0
    d <- d[rev(seq_len(nrow(d))), ]
    plotly::plot_ly(y = d$item, x = d$share, type = "bar", orientation = "h", marker = list(color = "#17a0db"),
                    text = paste0(fmt_num(d$share, 1), "%"), textposition = "auto",
                    hovertemplate = "%{y}: %{x:.1f}% of listed farm-gate loss<extra></extra>") |>
      plotly::layout(yaxis = list(categoryorder = "array", categoryarray = d$item, automargin = TRUE)) |>
      hv_plot_layout(NULL, "% of top products")
  }) |> bindCache(input$ej_over_y, cache = "app")
  output$ej_over_causes <- plotly::renderPlotly({
    req(ejo_over_on())
    who <- ejo_who_val(input$ej_over_who)
    c26 <- ejo_cy[ejo_cy$year == yr_h("over"), ]
    req(nrow(c26) > 0)
    vals <- if (identical(who, "farmer")) c26$farmer else if (identical(who, "gov")) c26$gov else c26$total
    plotly::plot_ly(labels = unname(ejo_causes[c26$cause]), values = vals / 1e9, type = "pie", hole = 0.55,
                    marker = list(colors = unname(ejo_reds[c26$cause])), textinfo = "label+percent",
                    hovertemplate = "%{label}: RWF %{value:.2f} bn<extra></extra>") |>
      plotly::layout(showlegend = FALSE, paper_bgcolor = "rgba(0,0,0,0)")
  }) |> bindCache(input$ej_over_y, input$ej_over_who, cache = "app")
  output$ej_over_share <- plotly::renderPlotly({
    req(ejo_over_on())
    who <- ejo_who_val(input$ej_over_who)
    d <- ejo_cy
    d$val <- if (identical(who, "farmer")) d$farmer else if (identical(who, "gov")) d$gov else d$total
    tot <- stats::aggregate(val ~ year, d, sum)
    names(tot)[2] <- "all"
    d <- merge(d, tot, by = "year")
    d$share <- ifelse(d$all > 0, 100 * d$val / d$all, 0)
    p <- plotly::plot_ly()
    for (k in names(ejo_causes)) {
      dk <- d[d$cause == k, ]
      dk <- dk[order(dk$year), ]
      p <- plotly::add_trace(p, x = dk$year, y = dk$share, name = ejo_causes[[k]], type = "scatter", mode = "lines",
                             stackgroup = "one", line = list(width = 0.5, color = ejo_reds[[k]]),
                             fillcolor = ejo_reds[[k]], hovertemplate = paste0("%{x}: %{y:.1f}%<extra>", ejo_causes[[k]], "</extra>"))
    }
    p |> plotly::layout(yaxis = list(title = "% of money lost", range = c(0, 100)), legend = list(orientation = "h", y = -0.2)) |>
      hv_plot_layout(NULL, "% of money lost")
  }) |> bindCache(input$ej_over_who, cache = "app")
  output$ej_over_table <- renderUI({
    req(ejo_over_on())
    g <- ejo_gain_default
    tags$div(
      class = "chart-card",
      tags$h3(class = "card-title", "Six causes, 2026\u20132031"),
      tags$table(
        class = "hv-table ej-table",
        tags$thead(tags$tr(tags$th("Cause"), tags$th("Model"), lapply(ejo_years, tags$th), tags$th("Lost 2026\u20132031"), tags$th("Gained"))),
        tags$tbody(lapply(names(ejo_causes), function(k) {
          gk <- g[g$cause == k, ]
          tags$tr(tags$td(tags$b(ejo_causes[[k]])), tags$td(class = "ej-small", ejo_or[[k]]),
                  lapply(seq_len(nrow(gk)), function(i) tags$td(tags$span(class = "ej-red", fmt_bn(gk$loss[i], FALSE)), tags$br(), tags$span(class = "ej-blue", fmt_bn(gk$gain[i], FALSE)))),
                  tags$td(class = "ej-red", tags$b(fmt_bn(sum(gk$loss)))), tags$td(class = "ej-blue", tags$b(fmt_bn(sum(gk$gain)))))
        }))
      ),
      tags$p(class = "note", "Each year cell: red = lost if nothing changes, blue = gained with the plan, RWF billion.")
    )
  })
  output$ej_over_data <- renderUI({
    req(ejo_over_on())
    n <- function(x) if (is.null(x)) "not downloaded" else paste(format(nrow(x), big.mark = ","), "rows")
    rows <- list(
      c("FAO food balances: production and losses by product, 2010\u20132023", paste(format(nrow(ejo_fbs), big.mark = ","), "rows")),
      c("FAO food balances, old methodology, 1961\u20132013 (spliced in for 1996\u20132009)", n(ejo_fbs_old)),
      c("FAOSTAT producer prices (RWF per tonne)", paste(format(nrow(crop_prices), big.mark = ","), "rows")),
      c("NISR SAS district production and use (sold, stored, lost)", paste(format(nrow(sas_prod), big.mark = ","), "rows")),
      c("OSRM road distances and drive times, 87 \u00d7 87", n(ejo_matrix)),
      c("OSRM road geometry for produce corridors", n(ejo_routes)),
      c("NASA POWER monthly rain, max temperature, humidity per district", n(ejo_power)),
      c("APHLIS cereal losses by value-chain stage, Rwanda", n(ejo_aphlis)),
      c("World Bank diesel pump price", n(ejo_diesel)),
      c("FAO government expenditure on agriculture; World Bank CPI and exchange rate", paste0("public spend = ", fmt_num(100 * ejo_gov_ratio, 1), "% of harvest value")),
      c("Planning assumptions: cause shares, options, parameters, action plans", "editable")
    )
    tags$div(
      class = "chart-card",
      tags$h3(class = "card-title", "Data behind Ejo hazaza"),
      tags$table(class = "hv-table", tags$thead(tags$tr(tags$th("Dataset"), tags$th("Status"))),
                 tags$tbody(lapply(rows, function(r) tags$tr(tags$td(r[1]), tags$td(r[2]))))),
      tags$p(class = "note", "Money lost = tonnes lost (FAO food balances 1996\u20132023, old series scaled to the new one by their 2010\u20132013 overlap; products with production but no loss figure before 2010 use their 2010\u20132013 loss rate; 2024\u20132031 = production trend \u00d7 the 2019\u20132023 loss rate) \u00d7 producer price (moved with the consumer price index outside FAOSTAT years). ",
             "Government loss = the public agricultural spending tied to every franc of harvest that is lost, plus horticulture export earnings lost to cold-chain breaks. ",
             "Cause shares for cereals come from APHLIS stages; for other products they are planning assumptions.")
    )
  })

  # ---- transport ----
  tr <- reactive({
    req(input$ej_tr_product)
    ejo_transport(input$ej_tr_product, ejo_p("transport_rwf_tkm"), ejo_p("hub_share"), ejo_p("market_slack"), input$ej_tr_via)
  })
  tr_example <- reactive({
    r <- tr()
    req(r)
    o <- input$ej_tr_origin
    h <- input$ej_tr_home
    v <- r$via
    req(o %in% names(r$S), h %in% r$mk)
    p <- r$price[[h]]
    lp <- function(k, hrs, n) list(km = k, hours = hrs, lossf = min(1, n * r$tr + r$sp * hrs),
                                   lost = p * min(1, n * r$tr + r$sp * hrs) + r$tc * k)
    vid <- paste0("m:", v)
    k1 <- ejo_net$km[paste0("d:", o), vid]
    k2 <- ejo_net$km[vid, paste0("m:", h)]
    h1 <- ejo_net$hr[paste0("d:", o), vid]
    h2 <- ejo_net$hr[vid, paste0("m:", h)]
    loop <- lp(k1 + k2, h1 + h2, 4)
    direct <- lp(ejo_net$km[paste0("d:", o), paste0("m:", h)], ejo_net$hr[paste0("d:", o), paste0("m:", h)], 2)
    best <- sort(r$net[o, ], decreasing = TRUE)
    list(o = o, h = h, v = v, p = p, loop = loop, direct = direct, best = best, tonnes = r$S[[o]])
  })
  output$ej_tr_map_title <- renderUI(tags$h3(class = "card-title", paste0("Red: ", input$ej_tr_origin, " \u2192 ", input$ej_tr_via, " \u2192 ", input$ej_tr_home, " today. Blue: the direct and optimal routes")))
  output$ej_tr_cards <- renderUI({
    r <- tr()
    if (is.null(r)) return(hv_empty("No market prices or road matrix for this product."))
    e <- tr_example()
    extra <- e$loop$lost - e$direct$lost
    yr <- extra * e$tonnes * r$hub
    tags$div(
      class = "kpi-row hv-kpis",
      hv_kpi(paste(e$o, "\u2192", e$v, "\u2192", e$h), paste(fmt_num(e$loop$km, 0), "km"), paste0(fmt_num(e$loop$hours, 1), " h on the road, 4 loadings, ", fmt_num(100 * e$loop$lossf, 1), "% spoiled")),
      hv_kpi(paste(e$o, "\u2192", e$h, "direct"), paste(fmt_num(e$direct$km, 0), "km"), paste0(fmt_num(e$direct$hours, 1), " h, 2 loadings, ", fmt_num(100 * e$direct$lossf, 1), "% spoiled")),
      hv_kpi(paste("Extra lost per tonne via", e$v), paste(fmt_num(extra, 0), "RWF"), paste0("Spoilage + transport. Price at ", e$h, ": ", fmt_num(e$p / 1000, 0), " RWF/kg"), "is-red"),
      hv_kpi(paste("Loop cost for", e$o, "a year"), fmt_bn(yr), paste0(fmt_num(e$tonnes * r$hub, 0), " t of ", fmt_num(e$tonnes, 0), " t marketed go round"), "is-red"),
      hv_kpi("National: lost on the road today", fmt_bn(r$cur_lost), paste0(fmt_num(r$cur_tlost, 0), " t spoiled \u00b7 ", fmt_num(r$cur_km / max(sum(r$S), 1), 0), " km per tonne"), "is-red"),
      hv_kpi("Optimal plan saves", fmt_bn(r$opt_val - r$cur_val), paste0("Road loss falls to ", fmt_bn(r$opt_lost), " (", fmt_num(r$opt_km / max(sum(r$S), 1), 0), " km per tonne)"), "is-blue")
    )
  })
  output$ej_tr_map <- leaflet::renderLeaflet({
    r <- tr()
    req(r)
    e <- tr_example()
    m <- hv_base_map()
    pal <- leaflet::colorNumeric(c("#f4f7f2", "#9fd3b5", "#2d6a4f"), domain = c(0, max(r$S)))
    m <- hv_fill(m, r$S, pal, function(d, v) paste0("<b>", d, "</b><br>", if (is.finite(v)) paste0(fmt_num(v, 0), " t marketed a year") else "No data"))
    line <- function(m, from, to, col, w, dash = NULL, lab = NULL) {
      g <- ejo_route_path(from, to)
      if (is.null(g)) {
        a <- if (from %in% ejo_districts) district_lnglat[[from]] else NULL
        xa <- if (!is.null(a)) c(a$cx, a$cy) else unlist(ejo_xy(paste0("m:", from)))
        xb <- unlist(ejo_xy(paste0("m:", to)))
        g <- list(lng = c(xa[1], xb[1]), lat = c(xa[2], xb[2]))
      }
      leaflet::addPolylines(m, lng = g$lng, lat = g$lat, color = col, weight = w, opacity = 0.9, dashArray = dash, label = lab)
    }
    X <- r$X
    fl <- which(X > 0, arr.ind = TRUE)
    if (nrow(fl)) {
      fl <- fl[order(-X[fl]), , drop = FALSE][1:min(30, nrow(fl)), , drop = FALSE]
      mx <- max(X[fl])
      for (i in seq_len(nrow(fl))) {
        d <- rownames(X)[fl[i, 1]]
        mk <- colnames(X)[fl[i, 2]]
        m <- line(m, d, mk, ejo_blue2, 1.5 + 5 * X[fl[i, 1], fl[i, 2]] / mx, NULL, paste0(d, " \u2192 ", mk, ": ", fmt_num(X[fl[i, 1], fl[i, 2]], 0), " t"))
      }
    }
    m <- line(m, e$o, e$v, ejo_red, 5, NULL, paste(e$o, "\u2192", e$v, "(first sold)"))
    m <- line(m, e$v, e$h, ejo_red, 5, "8 6", paste(e$v, "\u2192", e$h, "(bought back, last sold)"))
    m <- line(m, e$o, e$h, ejo_blue, 6, NULL, paste(e$o, "\u2192", e$h, "direct"))
    xy <- ejo_xy(paste0("m:", r$mk))
    pr <- r$price / 1000
    m <- leaflet::addCircleMarkers(m, lng = xy$lng, lat = xy$lat, radius = 4 + 8 * (pr - min(pr)) / max(1, diff(range(pr))),
                                   color = "#ffffff", weight = 1.5, fillColor = ifelse(r$mk %in% c(e$h, e$v), "#111827", "#6b7280"), fillOpacity = 0.9,
                                   label = paste0(r$mk, ": ", fmt_num(pr, 0), " RWF/kg"))
    leaflet::addLegend(m, "bottomleft", colors = c(ejo_red, ejo_blue, ejo_blue2), labels = c(paste("Loop through", e$v), "Direct route", "Optimal flows (LP)"), opacity = 0.9)
  })
  output$ej_tr_side <- renderUI({
    r <- tr()
    req(r)
    e <- tr_example()
    row <- function(lab, a, b) tags$div(class = "ej-cmp", tags$span(lab), tags$b(class = "ej-red", a), tags$b(class = "ej-blue", b))
    tags$div(
      tags$div(
        class = "hv-side-card",
        tags$h4("Loop against direct"),
        tags$div(class = "ej-cmp ej-cmp-head", tags$span(""), tags$b(class = "ej-red", paste("Via", e$v)), tags$b(class = "ej-blue", "Direct")),
        row("Road km", fmt_num(e$loop$km, 0), fmt_num(e$direct$km, 0)),
        row("Hours", fmt_num(e$loop$hours, 1), fmt_num(e$direct$hours, 1)),
        row("Loadings", "4", "2"),
        row("Spoiled", paste0(fmt_num(100 * e$loop$lossf, 1), "%"), paste0(fmt_num(100 * e$direct$lossf, 1), "%")),
        row("Lost / tonne", fmt_num(e$loop$lost, 0), fmt_num(e$direct$lost, 0))
      ),
      tags$div(
        class = "hv-side-card",
        tags$h4("Product"),
        tags$p(prod_label(r$product), " \u00b7 ", r$crop, " \u00b7 ", r$grp),
        tags$p(paste0(fmt_num(r$vol$tonnes, 0), " t a year (", r$vol$source, "), ", fmt_num(100 * r$vol$sold, 0), "% sold (NISR SAS)."))
      ),
      tags$p(class = "note", "Road km and hours: OSRM on OpenStreetMap. Prices: NAEB/Esoko, last 12 months median per market. Spoilage per hour and per loading are planning assumptions for ", r$grp, " produce.")
    )
  })
  output$ej_tr_best <- plotly::renderPlotly({
    e <- tr_example()
    b <- utils::head(e$best, 10)
    lp_net <- e$p - e$loop$lost
    nm <- c(paste0(e$h, " via ", e$v), names(b))
    v <- c(lp_net, unname(b)) / 1000
    col <- c(ejo_red, ifelse(names(b) == names(b)[1], ejo_blue, ejo_blue2))
    plotly::plot_ly(x = v, y = nm, type = "bar", orientation = "h", marker = list(color = col),
                    text = paste0("RWF ", fmt_num(v, 0), "/kg"), textposition = "auto", hovertemplate = "%{y}: RWF %{x:.0f} per kg kept<extra></extra>") |>
      plotly::layout(yaxis = list(categoryorder = "array", categoryarray = rev(nm), automargin = TRUE)) |>
      hv_plot_layout(NULL, paste("RWF per kg the", e$o, "grower keeps"))
  })
  output$ej_tr_sankey <- plotly::renderPlotly({
    r <- tr()
    req(r)
    X <- r$X
    fl <- which(X > 0, arr.ind = TRUE)
    req(nrow(fl) > 0)
    fl <- fl[order(-X[fl]), , drop = FALSE][1:min(30, nrow(fl)), , drop = FALSE]
    src <- rownames(X)[fl[, 1]]
    dst <- colnames(X)[fl[, 2]]
    labs <- c(unique(src), paste0(unique(dst), " "))
    plotly::plot_ly(type = "sankey", orientation = "h",
                    node = list(label = labs, color = c(rep("#2d6a4f", length(unique(src))), rep(ejo_blue, length(unique(dst)))), pad = 10, thickness = 14),
                    link = list(source = match(src, labs) - 1, target = match(paste0(dst, " "), labs) - 1, value = X[fl], color = "rgba(31,95,209,0.25)")) |>
      plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", font = list(size = 11))
  })
  output$ej_tr_flows <- renderUI({
    r <- tr()
    req(r)
    X <- r$X
    fl <- which(X > 0, arr.ind = TRUE)
    if (!nrow(fl)) return(NULL)
    fl <- fl[order(-X[fl]), , drop = FALSE][1:min(20, nrow(fl)), , drop = FALSE]
    tags$div(
      class = "chart-card",
      tags$h3(class = "card-title", "Optimal plan: the 20 largest flows"),
      tags$table(
        class = "hv-table",
        tags$thead(tags$tr(tags$th("From district"), tags$th("To market"), tags$th("Tonnes a year"), tags$th("Road km"), tags$th("Hours"), tags$th("Price"), tags$th("Grower keeps"))),
        tags$tbody(lapply(seq_len(nrow(fl)), function(i) {
          a <- fl[i, 1]
          b <- fl[i, 2]
          tags$tr(tags$td(rownames(X)[a]), tags$td(colnames(X)[b]), tags$td(fmt_num(X[a, b], 0)), tags$td(fmt_num(r$km[a, b], 0)), tags$td(fmt_num(r$hr[a, b], 1)),
                  tags$td(paste0("RWF ", fmt_num(r$price[b] / 1000, 0), "/kg")), tags$td(class = "ej-blue", paste0("RWF ", fmt_num(r$net[a, b] / 1000, 0), "/kg")))
        }))
      ),
      tags$p(class = "note", "Linear program: maximise what growers keep (price \u2212 spoilage \u2212 transport) so every district's marketed harvest is sold and no market takes more than ",
             fmt_num(100 * (ejo_p("market_slack") - 1), 0), "% above its present share. Solved with lpSolve.")
    )
  })

  # ---- storage ----
  st <- reactive({
    w <- ejo_district_share(input$ej_st_crop)
    n <- sas_prod_nat[sas_prod_nat$crop == input$ej_st_crop, , drop = FALSE]
    tot <- if (nrow(n)) sum(n$value[n$year == max(n$year)]) else NA_real_
    list(w = w, tot = tot, pm = ejo_pmedian(w, input$ej_st_k))
  })
  st_rate <- reactive({
    grp <- if (input$ej_st_crop %in% c("Bean", "Pea", "Soybean", "Groundnuts")) "pulses" else "cereals"
    d <- ejo_loss_items[ejo_loss_items$group == grp & ejo_loss_items$year == 2026, , drop = FALSE]
    list(grp = grp, rate = sum(d$loss_t) / sum(d$prod_t) * ejo_share_mat[grp, "storage"])
  })
  output$ej_st_cards <- renderUI({
    s <- st()
    if (is.null(s$pm)) return(hv_empty("The road matrix or lpSolve is missing."))
    rr <- st_rate()
    lost <- s$tot * rr$rate
    tags$div(
      class = "kpi-row hv-kpis",
      hv_kpi("Lost in storage", paste(fmt_num(lost, 0), "t"), paste0(fmt_num(100 * rr$rate, 1), "% of the ", input$ej_st_crop, " harvest (", fmt_num(s$tot, 0), " t, NISR SAS)"), "is-red"),
      hv_kpi("Warehouse sites", paste(length(s$pm$sites)), paste(s$pm$sites, collapse = ", "), "is-blue"),
      hv_kpi("Average road to a warehouse", paste(fmt_num(s$pm$avg_km, 1), "km"), "Weighted by each district's harvest", "is-blue"),
      hv_kpi("Farthest district", paste(fmt_num(s$pm$max_km, 0), "km"), "Add a site to bring it closer")
    )
  })
  output$ej_st_map <- leaflet::renderLeaflet({
    s <- st()
    req(s$pm)
    rr <- st_rate()
    lost <- s$w * s$tot * rr$rate
    pal <- leaflet::colorNumeric(c("#fff1ee", "#f4a28c", "#b91c1c"), domain = c(0, max(lost, 1)))
    m <- hv_fill(hv_base_map(), lost, pal, function(d, v) paste0("<b>", d, "</b><br>", fmt_num(v, 0), " t lost in storage"))
    a <- s$pm$assign
    for (i in seq_len(nrow(a))) if (a$district[i] != a$site[i] && a$w[i] > 0) {
      p1 <- district_lnglat[[a$district[i]]]
      p2 <- district_lnglat[[a$site[i]]]
      m <- leaflet::addPolylines(m, lng = c(p1$cx, p2$cx), lat = c(p1$cy, p2$cy), color = ejo_blue, weight = 2, opacity = 0.8,
                                 label = paste0(a$district[i], " \u2192 ", a$site[i], ": ", fmt_num(a$km[i], 0), " km"))
    }
    cx <- vapply(s$pm$sites, function(d) district_lnglat[[d]]$cx, numeric(1))
    cy <- vapply(s$pm$sites, function(d) district_lnglat[[d]]$cy, numeric(1))
    m |>
      leaflet::addMarkers(lng = cx, lat = cy, icon = leaflet::makeIcon("umusaruro/coop.svg", iconWidth = 36, iconHeight = 36), label = paste("Warehouse:", s$pm$sites)) |>
      leaflet::addLabelOnlyMarkers(lng = cx, lat = cy, label = s$pm$sites, labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "bottom", offset = c(0, 16), className = "pin-tag ej-pin-blue"))
  })
  output$ej_st_side <- renderUI({
    s <- st()
    req(s$pm)
    a <- s$pm$assign
    share <- tapply(a$w, a$site, sum)
    tags$div(
      tags$div(
        class = "hv-side-card",
        tags$h4(tags$img(src = "umusaruro/coop.svg", alt = ""), "Optimal sites"),
        lapply(names(sort(share, decreasing = TRUE)), function(sname) tags$div(
          class = "hv-bar-row", tags$span(sname),
          tags$div(class = "hv-bar ej-bar-blue", tags$i(style = sprintf("width:%.0f%%", 100 * share[[sname]] / max(share)))),
          tags$b(paste0(fmt_num(100 * share[[sname]], 0), "%"))
        ))
      ),
      tags$p(class = "note", "p-median: choose k district sites to minimise tonnes \u00d7 road km from every district to its nearest warehouse. Road km from OSRM; harvest shares from NISR SAS.")
    )
  })
  output$ej_st_aphlis <- plotly::renderPlotly({
    v <- ejo_aphlis_steps
    if (is.null(v)) return(NULL)
    crop <- switch(input$ej_st_crop, "Paddy rice" = "rice", "Sorghum" = "sorghum", "Wheat" = "wheat", "maize")
    v <- v[v$crop == crop, , drop = FALSE]
    if (!nrow(v)) v <- ejo_aphlis_steps[ejo_aphlis_steps$crop == "maize", , drop = FALSE]
    y <- max(v$year)
    s <- tapply(v$value[v$year == y], v$column[v$year == y], mean)
    s <- s[is.finite(s)]
    col <- ifelse(grepl("storage", names(s), ignore.case = TRUE), ejo_red, ifelse(grepl("drying|harvest", names(s), ignore.case = TRUE), "#e8836f", "#f4b4a5"))
    plotly::plot_ly(x = names(s), y = unname(s), type = "bar", marker = list(color = col), text = paste0(fmt_num(s, 1), "%"), textposition = "outside") |>
      plotly::layout(xaxis = list(categoryorder = "array", categoryarray = names(s))) |>
      hv_plot_layout("% lost at this stage", paste("APHLIS", tools::toTitleCase(crop), y))
  })
  output$ej_st_aphlis_trend <- plotly::renderPlotly({
    if (is.null(ejo_aphlis)) return(NULL)
    v <- ejo_aphlis[ejo_aphlis$table == "dry-weight-losses", , drop = FALSE]
    if (!nrow(v)) return(NULL)
    a <- stats::aggregate(value ~ crop + year, v, mean)
    p <- plotly::plot_ly()
    cols <- c(maize = ejo_red, rice = "#e8836f", sorghum = "#7f1d1d", wheat = "#f59e0b", millets = "#a16207")
    for (cr in unique(a$crop)) {
      z <- a[a$crop == cr, ]
      p <- plotly::add_trace(p, x = z$year, y = z$value, type = "scatter", mode = "lines+markers", name = tools::toTitleCase(cr), line = list(color = cols[[cr]]))
    }
    hv_plot_layout(p, "% of harvest lost (dry weight)")
  })

  # ---- weather ----
  we <- reactive({
    tryCatch({
      season <- input$ej_we_season
      grp <- input$ej_we_group
      n_raw <- input$ej_we_n
      req(isTruthy(season), isTruthy(grp), isTruthy(n_raw))
      rn <- ejo_rain(season)
      req(!is.null(rn), nrow(rn) > 0)
      crops <- ejo_group_crops[[grp]]
      req(length(crops) > 0)
      w <- ejo_district_share(crops)
      req(length(w) > 0)
      excess <- rn$excess * unname(w[as.character(rn$district)])
      excess[!is.finite(excess)] <- 0
      nat <- stats::aggregate(excess ~ year, data.frame(year = rn$year, excess = excess), sum)
      m <- 1 + ejo_p("rain_beta") * nat$excess
      mn <- m / mean(m)
      mn[!is.finite(mn)] <- 1
      d <- ejo_loss_items[ejo_loss_items$group == grp & ejo_loss_items$year == 2026, , drop = FALSE]
      base <- sum(d$farmer, na.rm = TRUE) * ejo_share_mat[grp, "weather"] * (1 + ejo_gov_ratio)
      if (!is.finite(base) || base <= 0) base <- 1
      set.seed(2026)
      n <- min(4000L, max(500L, as.integer(n_raw)[1]))
      sim <- base * sample(mn, n, replace = TRUE) * exp(stats::rnorm(n, -0.005, 0.1))
      E <- ejo_effect(isolate(sel$weather()), full = TRUE)
      risk <- tapply(rn$excess > 0.25, rn$district, mean)
      list(rn = rn, nat = nat, sim = sim, simp = sim * (1 - E), E = E, base = base, risk = risk, w = w, grp = grp)
    }, error = function(e) {
      warning("Weather model failed: ", conditionMessage(e))
      NULL
    })
  }) |> bindCache(input$ej_we_season, input$ej_we_group, input$ej_we_n, {
    v <- tryCatch(sel$weather(), error = function(e) character())
    if (is.null(v)) "" else paste(v, collapse = ",")
  }, cache = "session")
  output$ej_we_cards <- renderUI({
    s <- we()
    if (is.null(s)) return(hv_empty("Weather simulation could not run for these inputs."))
    q <- stats::quantile(s$sim, c(0.5, 0.9))
    qp <- stats::quantile(s$simp, 0.9)
    tags$div(
      class = "kpi-row hv-kpis",
      hv_kpi("Expected weather loss a year", fmt_bn(mean(s$sim)), paste(ejo_group_label[[s$grp]], "\u00b7 Season", input$ej_we_season), "is-red"),
      hv_kpi("Bad year (1 in 10)", fmt_bn(q[[2]]), "90th percentile of the simulation", "is-red"),
      hv_kpi("Chance of a loss 25% above normal", paste0(fmt_num(100 * mean(s$sim > 1.25 * s$base), 0), "%"), paste(length(s$sim), "simulated seasons"), "is-red"),
      hv_kpi("With the plan, bad year", fmt_bn(qp), paste0(fmt_num(100 * s$E, 0), "% of weather loss avoided at full roll-out"), "is-blue")
    )
  })
  output$ej_we_hist <- plotly::renderPlotly({
    s <- we()
    req(!is.null(s))
    plotly::plot_ly(alpha = 0.6) |>
      plotly::add_histogram(x = s$sim / 1e9, name = "Do nothing", marker = list(color = ejo_red), nbinsx = 40) |>
      plotly::add_histogram(x = s$simp / 1e9, name = "With the plan", marker = list(color = ejo_blue), nbinsx = 40) |>
      plotly::layout(barmode = "overlay") |>
      hv_plot_layout("Simulated seasons", "Weather loss, RWF billion")
  })
  output$ej_we_map <- leaflet::renderLeaflet({
    s <- we()
    req(!is.null(s))
    pal <- leaflet::colorNumeric(c("#fff1ee", "#f4a28c", "#b91c1c"), domain = c(0, max(s$risk, 0.01, na.rm = TRUE)))
    hv_fill(hv_base_map(), s$risk, pal, function(d, v) paste0("<b>", d, "</b><br>", if (is.finite(v)) paste0(fmt_num(100 * v, 0), "% of harvests since 2001 were wet") else "No data"))
  })
  output$ej_we_line <- plotly::renderPlotly({
    s <- we()
    req(!is.null(s))
    d <- s$rn[s$rn$district == input$ej_we_district, , drop = FALSE]
    req(nrow(d))
    med <- stats::median(d$rain_mm)
    plotly::plot_ly(x = d$year, y = d$rain_mm, type = "bar", name = input$ej_we_district,
                    marker = list(color = ifelse(d$rain_mm > 1.25 * med, ejo_red, "#9fb8a8"))) |>
      plotly::add_lines(x = d$year, y = rep(med, nrow(d)), name = "Median", line = list(color = "#1b4332", dash = "dash")) |>
      hv_plot_layout("Harvest-month rain (mm)", paste("NASA POWER,", input$ej_we_district, "\u00b7 red = wet harvest"))
  })
  output$ej_we_emv <- renderUI({
    s <- we()
    if (is.null(s)) return(NULL)
    ap <- ejo_appraisals$weather
    ex <- mean(s$sim)
    rows <- lapply(seq_len(nrow(ap)), function(i) {
      save <- ex * ap$effect[i]
      cost <- ap$capex_bn[i] * 1e9 / 5 + ap$opex_bn[i] * 1e9
      data.frame(alt = paste(ap$id[i], ap$option[i]), save = save, cost = cost, emv = save - cost,
                 p90 = stats::quantile(s$sim * (1 - ap$effect[i]), 0.9))
    })
    pk <- sel$weather()
    if (length(pk)) {
      op <- ejo_options[ejo_options$id %in% pk, ]
      rows[[length(rows) + 1]] <- data.frame(alt = paste("Plan:", paste(pk, collapse = " + ")), save = ex * s$E, cost = sum(op$capex_bn) * 1e9 / 5 + sum(op$opex_bn) * 1e9,
                                             emv = ex * s$E - (sum(op$capex_bn) * 1e9 / 5 + sum(op$opex_bn) * 1e9), p90 = stats::quantile(s$simp, 0.9))
    }
    tb <- rbind(data.frame(alt = "Do nothing", save = 0, cost = 0, emv = 0, p90 = stats::quantile(s$sim, 0.9)), do.call(rbind, rows))
    best <- which.max(tb$emv)
    tags$div(
      class = "chart-card",
      tags$h3(class = "card-title", "Decision table: expected monetary value per year"),
      tags$table(
        class = "hv-table ej-table",
        tags$thead(tags$tr(tags$th("Decision"), tags$th("Expected saving"), tags$th("Cost a year"), tags$th("Net value (EMV)"), tags$th("Bad-year loss"))),
        tags$tbody(lapply(seq_len(nrow(tb)), function(i) tags$tr(
          class = if (i == best) "is-on" else NULL,
          tags$td(if (i == best) tags$b(tb$alt[i], " \u2190 best") else tb$alt[i]),
          tags$td(class = "ej-blue", fmt_bn(tb$save[i])), tags$td(fmt_bn(tb$cost[i])),
          tags$td(class = if (tb$emv[i] >= 0) "ej-blue" else "ej-red", fmt_bn(tb$emv[i])), tags$td(class = "ej-red", fmt_bn(tb$p90[i]))
        )))
      ),
      tags$p(class = "note", "Each simulated season draws one year of harvest-month rain (2001\u20132024, all districts together) and a random shock. Loss grows ",
             fmt_num(100 * ejo_p("rain_beta"), 0), "% for every 100% of rain above the district median, weighted by where the crop grows. Capital is spread over five years.")
    )
  })

  # ---- cold chain ----
  co <- reactive({
    q <- ejo_queue(input$ej_co_district, input$ej_co_lot, input$ej_co_cap, input$ej_co_cost)
    nat <- lapply(ejo_districts, function(d) {
      z <- ejo_queue(d, input$ej_co_lot, input$ej_co_cap, input$ej_co_cost)
      if (is.null(z)) NULL else data.frame(district = d, c = z$best$c, t = z$t_yr, save = z$base - z$best$total, tmax = z$tmax)
    })
    list(q = q, nat = do.call(rbind, nat))
  })
  output$ej_co_cards <- renderUI({
    s <- co()
    q <- s$q
    if (is.null(q)) return(hv_empty("No vegetable or fruit harvest reported for this district."))
    b <- q$best
    fx <- ejo_fx_loss(2026)
    tags$div(
      class = "kpi-row hv-kpis",
      hv_kpi("Fresh produce to cool", paste(fmt_num(q$t_yr / 365, 1), "t a day"), paste0(fmt_num(q$lam * 24, 1), " lots a day \u00b7 max ", fmt_num(q$tmax, 1), " \u00b0C (NASA POWER)")),
      hv_kpi("Lost waiting in the heat today", fmt_bn(q$base), paste0("A day without cooling: ", fmt_num(100 * min(1, q$spo * 24), 0), "% spoiled"), "is-red"),
      hv_kpi("Optimal cold rooms", paste(b$c), paste0("Use ", fmt_num(100 * b$rho, 0), "% \u00b7 mean wait ", fmt_num(b$wq, 1), " h"), "is-blue"),
      hv_kpi("Lots waiting over 48 h", paste0(fmt_num(100 * b$p48, 2), "%"), "The export rule: dispatch cooled within 48 hours", if (b$p48 < 0.01) "is-blue" else "is-red"),
      hv_kpi("Saved a year in this district", fmt_bn(q$base - b$total), "After paying for the rooms", "is-blue"),
      hv_kpi("Export earnings at risk", fmt_bn(fx), paste0(fmt_num(100 * ejo_p("hort_export_loss_share"), 0), "% of NAEB horticulture exports"), "is-red")
    )
  })
  output$ej_co_curve <- plotly::renderPlotly({
    q <- co()$q
    req(q)
    tb <- q$tb
    plotly::plot_ly(x = tb$c) |>
      plotly::add_trace(y = tb$spoil / 1e6, type = "scatter", mode = "lines+markers", name = "Spoilage while waiting", line = list(color = ejo_red)) |>
      plotly::add_trace(y = tb$rooms / 1e6, type = "scatter", mode = "lines+markers", name = "Cold rooms", line = list(color = ejo_blue)) |>
      plotly::add_trace(y = tb$total / 1e6, type = "scatter", mode = "lines+markers", name = "Total", line = list(color = "#111827", width = 3)) |>
      plotly::layout(shapes = list(list(type = "line", x0 = q$best$c, x1 = q$best$c, y0 = 0, y1 = 1, yref = "paper", line = list(color = ejo_blue, dash = "dot")))) |>
      hv_plot_layout("RWF million a year", "Number of cold rooms (c)")
  })
  output$ej_co_map <- leaflet::renderLeaflet({
    n <- co()$nat
    req(n)
    v <- setNames(n$c, n$district)
    pal <- leaflet::colorNumeric(c("#eef4ff", "#8fb3f0", "#1e3a8a"), domain = c(0, max(v)))
    m <- hv_fill(hv_base_map(), v, pal, function(d, x) paste0("<b>", d, "</b><br>", if (is.finite(x)) paste(x, "cold rooms") else "No horticulture reported"))
    leaflet::addLabelOnlyMarkers(m, lng = vapply(n$district, function(d) district_lnglat[[d]]$cx, numeric(1)), lat = vapply(n$district, function(d) district_lnglat[[d]]$cy, numeric(1)),
                                 label = as.character(n$c), labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "center", className = "pin-tag ej-pin-blue"))
  })
  output$ej_co_table <- renderUI({
    s <- co()
    q <- s$q
    req(q)
    n <- s$nat
    tags$div(
      class = "split",
      tags$div(
        class = "chart-card",
        tags$h3(class = "card-title", paste("Queue table,", q$d)),
        tags$table(
          class = "hv-table ej-table",
          tags$thead(tags$tr(tags$th("Rooms"), tags$th("Utilisation"), tags$th("Mean wait"), tags$th("Wait > 12 h"), tags$th("Wait > 48 h"), tags$th("Spoilage"), tags$th("Rooms cost"), tags$th("Total"))),
          tags$tbody(lapply(seq_len(min(10, nrow(q$tb))), function(i) {
            z <- q$tb[i, ]
            tags$tr(class = if (z$c == q$best$c) "is-on" else NULL, tags$td(tags$b(z$c)), tags$td(paste0(fmt_num(100 * z$rho, 0), "%")), tags$td(paste(fmt_num(z$wq, 2), "h")),
                    tags$td(paste0(fmt_num(100 * z$p12, 1), "%")), tags$td(paste0(fmt_num(100 * z$p48, 2), "%")),
                    tags$td(class = "ej-red", paste0("RWF ", fmt_num(z$spoil / 1e6, 0), " m")), tags$td(class = "ej-blue", paste0("RWF ", fmt_num(z$rooms / 1e6, 0), " m")), tags$td(tags$b(paste0("RWF ", fmt_num(z$total / 1e6, 0), " m"))))
          }))
        ),
        tags$p(class = "note", "M/M/c: lots arrive at random (Poisson) and each room cools a lot in random time. Erlang C gives the chance a lot must wait. Spoilage per hour rises with the district's maximum temperature (Q10 = ",
               fmt_num(ejo_p("q10"), 0), ").")
      ),
      tags$div(
        class = "chart-card",
        tags$h3(class = "card-title", "National cold-room plan"),
        tags$table(
          class = "hv-table",
          tags$thead(tags$tr(tags$th("District"), tags$th("Fresh t a year"), tags$th("Rooms"), tags$th("Saved a year"))),
          tags$tbody(lapply(order(-n$save)[1:min(12, nrow(n))], function(i) tags$tr(tags$td(n$district[i]), tags$td(fmt_num(n$t[i], 0)), tags$td(tags$b(n$c[i])), tags$td(class = "ej-blue", fmt_bn(n$save[i])))))
        ),
        tags$p(class = "note", paste0("All districts: ", sum(n$c), " cold rooms, saving ", fmt_bn(sum(n$save)), " a year after costs."))
      )
    )
  })

  # ---- glut ----
  gl <- reactive({
    req(input$ej_gl_product)
    h <- as.integer(input$ej_gl_harvest)
    r <- ejo_glut(input$ej_gl_product, h, input$ej_gl_store, input$ej_gl_delta / 100)
    req(r)
    crop <- ejo_sas_crop(input$ej_gl_product)
    r$vol <- ejo_volume(input$ej_gl_product, crop)
    r
  })
  output$ej_gl_cards <- renderUI({
    r <- gl()
    gain_t <- r$opt - r$cur
    tags$div(
      class = "kpi-row hv-kpis",
      hv_kpi("Price swing in the year", paste0(fmt_num(100 * (max(r$p) / min(r$p) - 1), 0), "%"), paste0("RWF ", fmt_num(min(r$p) / 1000, 0), " to ", fmt_num(max(r$p) / 1000, 0), " per kg"), "is-red"),
      hv_kpi("Selling at harvest today", paste0("RWF ", fmt_num(r$cur / 1000, 0), "/kg"), "Average price received", "is-red"),
      hv_kpi("Optimal sales plan", paste0("RWF ", fmt_num(r$opt / 1000, 0), "/kg"), "After storage cost and losses", "is-blue"),
      hv_kpi("Gain per tonne", paste0("RWF ", fmt_num(gain_t, 0)), paste0(fmt_num(100 * gain_t / r$cur, 1), "% more for the farmer"), "is-blue"),
      hv_kpi("National gain a year", fmt_bn(gain_t * r$vol$marketed), paste0(fmt_num(r$vol$marketed, 0), " t marketed (", r$vol$source, ")"), "is-blue")
    )
  })
  output$ej_gl_plot <- plotly::renderPlotly({
    r <- gl()
    m <- month.abb
    plotly::plot_ly() |>
      plotly::add_bars(x = m, y = 100 * r$H, name = "Sold at harvest today (%)", marker = list(color = ejo_red)) |>
      plotly::add_bars(x = m, y = 100 * r$sell, name = "Optimal sales (%)", marker = list(color = ejo_blue)) |>
      plotly::add_trace(x = m, y = 100 * r$inv, type = "scatter", mode = "lines", name = "In store (%)", line = list(color = ejo_blue2, dash = "dot"), inherit = FALSE) |>
      plotly::add_trace(x = m, y = r$p / 1000, type = "scatter", mode = "lines+markers", name = "Price (RWF/kg)", yaxis = "y2", line = list(color = "#111827", width = 3)) |>
      plotly::layout(barmode = "group", xaxis = list(categoryorder = "array", categoryarray = m),
                     yaxis2 = list(overlaying = "y", side = "right", title = "RWF per kg", showgrid = FALSE)) |>
      hv_plot_layout("% of the harvest", paste("NAEB/Esoko monthly median,", r$n, "market-months"))
  })

  # ---- handling ----
  ha <- reactive({
    d <- ejo_loss_items[ejo_loss_items$year == 2026, , drop = FALSE]
    d$hand <- d$farmer * ejo_share_mat[d$group, "handling"]
    d <- d[order(-d$hand), , drop = FALSE]
    d$cum <- cumsum(d$hand) / sum(d$hand)
    cost_t <- input$ej_ha_crate / (ejo_p("crate_trips") * ejo_p("crate_life") * 0.02)
    b <- d[ejo_perish[d$group] != "durable", , drop = FALSE]
    b$benefit <- (input$ej_ha_sack - input$ej_ha_crt) / 100 * b$price_t
    b$bcr <- b$benefit / cost_t
    list(d = d, b = b, cost_t = cost_t)
  })
  output$ej_ha_cards <- renderUI({
    s <- ha()
    n80 <- which(s$d$cum >= 0.8)[1]
    good <- sum(s$b$bcr >= 1)
    tags$div(
      class = "kpi-row hv-kpis",
      hv_kpi("Handling loss, 2026", fmt_bn(sum(s$d$hand)), "Farm-gate value of crushed and bruised produce", "is-red"),
      hv_kpi("80% of it sits in", paste(n80, "products"), paste(utils::head(s$d$item, min(n80, 4)), collapse = ", "), "is-red"),
      hv_kpi("Crate cost per tonne moved", paste0("RWF ", fmt_num(s$cost_t, 0)), paste0(ejo_p("crate_trips"), " trips a year for ", ejo_p("crate_life"), " years"), "is-green"),
      hv_kpi("Products where crates pay", paste(good, "of", nrow(s$b)), "Benefit / cost above 1", "is-blue")
    )
  })
  output$ej_ha_pareto <- plotly::renderPlotly({
    d <- utils::head(ha()$d, 15)
    plotly::plot_ly() |>
      plotly::add_bars(x = d$item, y = d$hand / 1e9, name = "Handling loss", marker = list(color = ejo_red)) |>
      plotly::add_trace(x = d$item, y = 100 * d$cum, type = "scatter", mode = "lines+markers", yaxis = "y2", name = "Cumulative %", line = list(color = "#111827")) |>
      plotly::layout(xaxis = list(categoryorder = "array", categoryarray = d$item, tickangle = -35),
                     yaxis2 = list(overlaying = "y", side = "right", range = c(0, 105), title = "Cumulative %", showgrid = FALSE),
                     shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 80, y1 = 80, yref = "y2", line = list(color = "#7a867e", dash = "dot")))) |>
      hv_plot_layout("RWF billion")
  })
  output$ej_ha_bcr <- plotly::renderPlotly({
    b <- ha()$b
    b <- b[order(-b$bcr), , drop = FALSE]
    plotly::plot_ly(x = b$item, y = b$bcr, type = "bar", marker = list(color = ifelse(b$bcr >= 1, ejo_blue, ejo_red)),
                    text = paste0(fmt_num(b$bcr, 1), "\u00d7"), textposition = "outside") |>
      plotly::layout(xaxis = list(categoryorder = "array", categoryarray = b$item, tickangle = -35),
                     shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 1, y1 = 1, line = list(color = "#111827", dash = "dash")))) |>
      hv_plot_layout("Francs saved per franc on crates")
  })

  # ---- national action plan ----
  ap_all <- reactive({
    ap <- do.call(rbind, ejo_appraisals)
    ids <- ejo_knapsack(ap, input$ej_ap_budget)
    list(ap = ap, ids = ids)
  })
  ap_gain <- reactive({
    a <- ap_all()
    do.call(rbind, lapply(names(ejo_causes), function(k) cbind(cause = k, ejo_gain(k, intersect(a$ids, ejo_options$id[ejo_options$cause == k])))))
  })
  output$ej_ap_cards <- renderUI({
    a <- ap_all()
    g <- ap_gain()
    op <- ejo_options[ejo_options$id %in% a$ids, ]
    npv <- sum((g$gain - g$cost) * ejo_disc(g$year))
    y <- yr_h("ap")
    fy <- yr_f("ap")
    ly <- sum(ejo_cy$total[ejo_cy$year == y])
    tags$div(
      class = "kpi-row hv-kpis",
      hv_kpi(paste("Lost in", y), if (ly > 0) fmt_bn(ly) else "\u2014", ejo_kind_year[[as.character(y)]], "is-red"),
      hv_kpi(paste("Lost in", fy, "if nothing changes"), fmt_bn(sum(g$loss[g$year == fy])), "Forecast, all six causes", "is-red"),
      hv_kpi(paste("Gained in", fy), fmt_bn(sum(g$gain[g$year == fy])), "With the chosen options", "is-blue"),
      hv_kpi("Lost 2026\u20132031 if nothing changes", fmt_bn(sum(g$loss)), "All six causes", "is-red"),
      hv_kpi("Gained with the plan", fmt_bn(sum(g$gain)), paste(length(a$ids), "options chosen"), "is-blue"),
      hv_kpi("Capital used", paste0(fmt_num(sum(op$capex_bn), 1), " bn RWF"), paste("of", input$ej_ap_budget, "bn available"), "is-green"),
      hv_kpi("Plan cost 2026\u20132031", fmt_bn(sum(g$cost)), "Capital plus running costs", "is-green"),
      hv_kpi("Net present value", fmt_bn(npv), "Gains minus all costs, discounted", if (npv >= 0) "is-blue" else "is-red"),
      hv_kpi("Farmers win back", fmt_bn(sum(g$gain_farmer)), "2026\u20132031", "is-blue"),
      hv_kpi("Government wins back", fmt_bn(sum(g$gain_gov)), "2026\u20132031", "is-blue")
    )
  })
  output$ej_ap_plot <- plotly::renderPlotly({
    g <- stats::aggregate(cbind(loss, gain) ~ year, ap_gain(), sum)
    ejo_gain_plot(g)
  })
  output$ej_ap_schedule <- renderUI({
    if (is.null(ejo_pm) || !nrow(ejo_pm)) return(hv_empty("The action-plan file has no activities."))
    tagList(lapply(names(ejo_plan_order), function(k) {
      pl <- ejo_pm[ejo_pm$cause == k, , drop = FALSE]
      if (!nrow(pl)) return(NULL)
      tags$div(
        class = "chart-card",
        tags$h3(pl$cause_label[1]),
        tags$p(class = "note", pl$where[1]),
        tags$table(
          class = "hv-table",
          tags$thead(tags$tr(
            tags$th("Work"), tags$th("Lead"), tags$th("When"), tags$th("Follows"), tags$th("Data already here"), tags$th("Report goes to"), tags$th("Cost")
          )),
          tags$tbody(lapply(seq_len(nrow(pl)), function(i) tags$tr(
            class = if (isTRUE(pl$critical[i])) "is-on" else NULL,
            tags$td(tags$b(pl$id[i]), " ", pl$activity[i]),
            tags$td(pl$lead[i]),
            tags$td(paste0(pl$start[i], " \u2013 ", pl$finish[i], " (", fmt_num(pl$likely[i], 0), " mo likely)")),
            tags$td(pl$after[i]),
            tags$td(pl$data_note[i]),
            tags$td(pl$report_to[i]),
            tags$td(fmt_bn(pl$cost[i]))
          )))
        )
      )
    }))
  })

  ejo_report_read <- function() {
    empty <- data.frame(ref = character(), filed = character(), option_id = character(), cause = character(),
                        filed_by = character(), addressed_to = character(), activity = character(),
                        where = character(), when = character(), note = character(), tonnes = character(), stringsAsFactors = FALSE)
    if (!file.exists(ejo_report_path)) return(empty)
    x <- tryCatch(utils::read.csv(ejo_report_path, stringsAsFactors = FALSE), error = function(e) empty)
    if (!nrow(x)) return(empty)
    for (col in names(empty)) if (!col %in% names(x)) x[[col]] <- ""
    x$tonnes[is.na(x$tonnes)] <- ""
    x$note[is.na(x$note)] <- ""
    x
  }
  ejo_report_log <- reactiveVal(ejo_report_read())
  ejo_user <- reactiveVal(NULL)
  ejo_db_tick <- reactiveVal(0L)

  observeEvent(input$gsas_login_go, {
    u <- tryCatch(
      ejo_db_auth(input$gsas_user, input$gsas_pass, NULL),
      error = function(e) NULL
    )
    if (is.null(u)) {
      output$gsas_login_msg <- renderUI(hv_empty("Login failed. Use the RISA account shown under the password, or an account you created."))
      return()
    }
    ejo_user(u)
    output$gsas_login_msg <- renderUI(NULL)
    view <- if (!is.null(input$ej_act) && isTruthy(input$ej_act$id)) "desk" else "home"
    session$sendCustomMessage("gsas_auth", list(
      ok = TRUE,
      username = u$username,
      institution = u$institution,
      role = u$role,
      display_name = u$display_name,
      tier = ejo_user_tier(u),
      view = view
    ))
  }, ignoreInit = TRUE)
  observeEvent(input$gsas_logout, {
    ejo_user(NULL)
    session$sendCustomMessage("gsas_auth", list(ok = FALSE))
  }, ignoreInit = TRUE)

  gsas_board <- reactive({
    ejo_db_tick()
    user <- ejo_user()
    req(user)
    tryCatch(
      ejo_activity_board(user$institution, ejo_sees_all(user)),
      error = function(e) data.frame()
    )
  })
  output$gsas_kpis <- renderUI({
    user <- ejo_user()
    req(user)
    if (!ejo_tier_sees_exec(ejo_user_tier(user))) return(NULL)
    board <- gsas_board()
    ejo_portal_kpi_row(user, nrow(board), sum(board$overdue, na.rm = TRUE))
  })
  output$gsas_next <- renderUI({
    user <- ejo_user()
    req(user)
    if (!ejo_tier_sees_exec(ejo_user_tier(user))) return(NULL)
    board <- gsas_board()
    focus <- board[board$cause %in% c("storage", "cold", "glut"), , drop = FALSE]
    tags$div(
      tags$h3(class = "gsas-block-title", "Drying, lack of cooling, too much supply"),
      ejo_portal_task_cards(if (nrow(focus)) focus else board, 6)
    )
  })
  output$gsas_home_list <- renderUI({
    user <- ejo_user()
    req(user)
    sols <- tryCatch(
      ejo_db_solutions_for_inst(user$institution, ejo_sees_all(user)),
      error = function(e) ejo_options[0, , drop = FALSE]
    )
    tags$div(class = "chart-card", tags$h3("Solutions on this institution"), ejo_portal_sol_table(sols, openable = TRUE))
  })
  output$gsas_inbox <- renderUI({
    board <- gsas_board()
    if (!nrow(board)) return(tags$p(class = "note", "No activities name your institution yet."))
    tags$div(class = "chart-card", ejo_portal_task_cards(board, nrow(board)))
  })
  output$gsas_pms <- renderUI({
    user <- ejo_user()
    req(user)
    board <- gsas_board()
    if (!nrow(board)) return(tags$p(class = "note", "No activities name your institution yet."))
    ch <- stats::setNames(board$key, paste0(board$cause_label, " · ", board$id, " · ", board$activity))
    cur <- isolate(input$gsas_act)
    sel <- if (!is.null(cur) && cur %in% board$key) cur else board$key[1]
    row <- board[board$key == sel, , drop = FALSE][1, ]
    tags$div(
      tags$div(
        class = "chart-card",
        tags$h3("Update one activity"),
        tags$p(class = "note", "Months are counted from January 2026. A critical-path delay delays that whole cause."),
        selectInput("gsas_act", "ACTIVITY", choices = ch, selected = sel, width = "100%"),
        selectInput("gsas_act_status", "STATUS", choices = stats::setNames(names(ejo_act_statuses), unname(ejo_act_statuses)), selected = row$status, width = "100%"),
        selectInput("gsas_act_step", "GOVERNMENT STEP", choices = stats::setNames(names(ejo_act_steps), unname(ejo_act_steps)), selected = row$framework_step, width = "100%"),
        textInput("gsas_act_who", "IF BLOCKED, WAITING ON", value = row$blocked_who, width = "100%"),
        textInput("gsas_act_what", "WAITING FOR WHAT", value = row$blocked_what, width = "100%"),
        textInput("gsas_act_owner", "NEXT OWNER", value = row$next_owner, width = "100%"),
        textInput("gsas_act_due", "NEXT DUE", value = row$next_due, width = "100%"),
        textAreaInput("gsas_act_note", "LAST REPORT", value = row$last_note, rows = 3, width = "100%"),
        actionButton("gsas_act_save", "Save activity", class = "btn-go"),
        uiOutput("gsas_act_saved")
      ),
      tags$div(class = "chart-card", tags$h3("My tasks"), ejo_portal_task_cards(board, nrow(board)))
    )
  })
  observeEvent(input$gsas_act, {
    board <- tryCatch(gsas_board(), error = function(e) NULL)
    if (is.null(board) || !nrow(board) || is.null(input$gsas_act)) return()
    row <- board[board$key == input$gsas_act, , drop = FALSE]
    if (!nrow(row)) return()
    row <- row[1, ]
    isolate({
      updateSelectInput(session, "gsas_act_status", selected = row$status)
      updateSelectInput(session, "gsas_act_step", selected = row$framework_step)
      updateTextInput(session, "gsas_act_who", value = row$blocked_who)
      updateTextInput(session, "gsas_act_what", value = row$blocked_what)
      updateTextInput(session, "gsas_act_owner", value = row$next_owner)
      updateTextInput(session, "gsas_act_due", value = row$next_due)
      updateTextAreaInput(session, "gsas_act_note", value = row$last_note)
    })
  }, ignoreInit = TRUE)
  observeEvent(input$gsas_act_save, {
    user <- ejo_user()
    if (is.null(user) || is.null(input$gsas_act) || !nzchar(input$gsas_act)) return()
    tryCatch(ejo_db_save_activity(
      input$gsas_act,
      if (is.null(input$gsas_act_status)) "not_started" else input$gsas_act_status,
      trimws(if (is.null(input$gsas_act_who)) "" else input$gsas_act_who),
      trimws(if (is.null(input$gsas_act_what)) "" else input$gsas_act_what),
      if (is.null(input$gsas_act_step)) "1" else input$gsas_act_step,
      trimws(if (is.null(input$gsas_act_owner)) "" else input$gsas_act_owner),
      trimws(if (is.null(input$gsas_act_due)) "" else input$gsas_act_due),
      trimws(if (is.null(input$gsas_act_note)) "" else input$gsas_act_note),
      user$username
    ), error = function(e) NULL)
    ejo_db_tick(isolate(ejo_db_tick()) + 1L)
    output$gsas_act_saved <- renderUI(tags$p(class = "note", "Activity saved."))
  }, ignoreInit = TRUE)
  output$gsas_alerts <- renderUI({
    board <- gsas_board()
    late <- if (nrow(board)) board[board$overdue, , drop = FALSE] else board
    quiet <- if (nrow(board)) board[!board$reported_month & board$status != "done", , drop = FALSE] else board
    tagList(
      if (!nrow(late)) tags$div(class = "gsas-alert is-ok", tags$strong("Schedule"), tags$p("No activity is past its likely finish."))
      else lapply(seq_len(nrow(late)), function(i) tags$div(
        class = "gsas-alert is-warn",
        tags$strong(paste(late$cause_label[i], late$id[i], "is overdue")),
        tags$p(paste0(late$activity[i], " · finish was ", late$finish[i], " · ", abs(late$months_left[i]), " months past the likely finish."))
      )),
      if (!nrow(quiet)) tags$div(class = "gsas-alert is-ok", tags$strong("Reports"), tags$p("Every open activity has a note this month."))
      else lapply(seq_len(min(nrow(quiet), 12)), function(i) tags$div(
        class = "gsas-alert",
        tags$strong(paste(quiet$cause_label[i], quiet$id[i], "has no report this month")),
        tags$p(quiet$activity[i])
      ))
    )
  })
  lapply(c("gsas_kpis", "gsas_next", "gsas_home_list", "gsas_inbox", "gsas_pms", "gsas_alerts"), function(id) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  })
  ejo_desk_server(input, output, session, ejo_user, ejo_db_tick)

  observeEvent(input$ej_opt_who, {
    a <- isolate(input$ej_act)
    if (is.null(a) || is.null(a$id)) return()
    if (!(identical(a$scope, "ap") || ejo_inst_is_option_id(a$scope, a$id))) return()
    op <- ejo_options[ejo_options$id == a$id, , drop = FALSE]
    if (!nrow(op)) return()
    parts <- ejo_lead_parts(op$lead[1])
    who_choices <- unique(c(parts[1], if (length(parts) > 1) parts[-1] else character()))
    if (!length(who_choices)) return()
    who <- input$ej_opt_who
    if (is.null(who) || !who %in% who_choices) who <- who_choices[1]
    acts <- ejo_option_work(op[1, ])
    mine <- if (nrow(acts)) acts[vapply(acts$lead, function(ld) tolower(who) %in% tolower(ejo_lead_parts(ld)), logical(1)), , drop = FALSE] else acts
    ch <- if (nrow(mine)) stats::setNames(paste(mine$id, mine$activity), paste(mine$id, mine$activity)) else stats::setNames(op$option[1], op$option[1])
    cur <- isolate(input$ej_opt_work)
    sel <- if (!is.null(cur) && cur %in% unname(ch)) cur else unname(ch)[1]
    isolate(updateSelectInput(session, "ej_opt_work", choices = ch, selected = sel))
  }, ignoreInit = TRUE)

  # Keep report framework-step selector in sync with institutional step radio.
  observeEvent(input$ej_inst_step, {
    sid <- as.character(input$ej_inst_step)
    cur <- isolate(input$ej_opt_step)
    if (is.null(cur) || identical(as.character(cur), sid)) return()
    isolate(updateSelectInput(session, "ej_opt_step", selected = sid))
  }, ignoreInit = TRUE)
  observeEvent(input$ej_opt_step, {
    sid <- as.character(input$ej_opt_step)
    cur <- isolate(input$ej_inst_step)
    if (is.null(cur) || identical(as.character(cur), sid)) return()
    isolate(updateRadioButtons(session, "ej_inst_step", selected = sid))
  }, ignoreInit = TRUE)

  observeEvent(input$ej_opt_send, {
    user <- ejo_user()
    if (is.null(user)) {
      output$ej_opt_done <- renderUI(hv_empty("Sign in before sending a report."))
      return()
    }
    a <- input$ej_act
    if (is.null(a) || is.null(a$id)) return()
    if (!(identical(a$scope, "ap") || ejo_inst_is_option_id(a$scope, a$id))) return()
    op <- ejo_options[ejo_options$id == a$id, , drop = FALSE]
    if (!nrow(op)) return()
    parts <- ejo_lead_parts(op$lead[1])
    lead <- parts[1]
    note <- trimws(if (is.null(input$ej_opt_note)) "" else input$ej_opt_note)
    if (!nzchar(note)) {
      output$ej_opt_done <- renderUI(hv_empty("Write what was done before sending the report."))
      return()
    }
    tonnes <- trimws(if (is.null(input$ej_opt_tonnes)) "" else input$ej_opt_tonnes)
    if (tonnes %in% c("NA", "\u2014")) tonnes <- ""
    if (nzchar(tonnes) && !is.finite(suppressWarnings(as.numeric(tonnes)))) {
      output$ej_opt_done <- renderUI(hv_empty("Measured tonnes must be a number, or left blank."))
      return()
    }
    sid <- if (!is.null(input$ej_opt_step)) as.character(input$ej_opt_step)
    else if (!is.null(input$ej_inst_step)) as.character(input$ej_inst_step) else "1"
    step_n <- max(1, min(6, suppressWarnings(as.integer(sid))))
    step <- ejo_inst_steps[[step_n]]
    fund <- if (is.null(input$ej_fund_source)) "Government" else input$ej_fund_source
    st_key <- if (is.null(input$ej_fund_status)) "process" else input$ej_fund_status
    pm_key <- if (is.null(input$ej_opt_pm)) "on_track" else input$ej_opt_pm
    pm_lab <- if (pm_key %in% names(ejo_pm_statuses)) ejo_pm_statuses[[pm_key]] else pm_key
    log <- ejo_report_read()
    ref <- paste0("EJ-", format(Sys.Date(), "%Y%m%d"), "-", sprintf("%03d", nrow(log) + 1))
    filed_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    out <- data.frame(
      ref = ref, filed = format(Sys.time(), "%Y-%m-%d %H:%M"), option_id = op$id[1],
      cause = unname(ejo_plan_order[[op$cause[1]]]), filed_by = input$ej_opt_who, addressed_to = lead,
      activity = if (is.null(input$ej_opt_work)) op$option[1] else input$ej_opt_work,
      where = input$ej_opt_where, when = input$ej_opt_when, note = note,
      tonnes = if (nzchar(tonnes)) tonnes else "",
      stringsAsFactors = FALSE
    )
    dir.create(dirname(ejo_report_path), showWarnings = FALSE)
    utils::write.table(out, ejo_report_path, sep = ",", row.names = FALSE, col.names = !file.exists(ejo_report_path), append = file.exists(ejo_report_path), qmethod = "double")
    ejo_report_log(ejo_report_read())
    tryCatch({
      ejo_db_add_report(list(
        ref = ref, filed_at = filed_at, option_id = op$id[1], cause = op$cause[1],
        filed_by = user$username, institution = if (!is.null(input$ej_opt_who)) input$ej_opt_who else user$institution,
        addressed_to = lead,
        activity = if (is.null(input$ej_opt_work)) op$option[1] else input$ej_opt_work,
        location = if (is.null(input$ej_opt_where)) "" else input$ej_opt_where,
        month_label = if (is.null(input$ej_opt_when)) "" else input$ej_opt_when,
        note = note, tonnes = if (nzchar(tonnes)) tonnes else "",
        framework_step = sid, framework_title = step$title,
        fund_source = fund, fund_status = st_key, pm_status = pm_lab,
        milestone = if (is.null(input$ej_opt_mile)) "" else trimws(input$ej_opt_mile),
        next_action = if (is.null(input$ej_opt_next)) "" else trimws(input$ej_opt_next),
        next_owner = if (is.null(input$ej_opt_next_owner)) "" else trimws(input$ej_opt_next_owner),
        next_when = if (is.null(input$ej_opt_next_when)) "" else trimws(input$ej_opt_next_when)
      ))
      ejo_db_save_state(op$id[1], op$cause[1], fund, st_key, sid, pm_key, user$username)
    }, error = function(e) NULL)
    ejo_db_tick(isolate(ejo_db_tick()) + 1L)
    updateTextAreaInput(session, "ej_opt_note", value = "")
    updateTextInput(session, "ej_opt_tonnes", value = "")
    updateTextInput(session, "ej_opt_mile", value = "")
    updateTextInput(session, "ej_opt_next", value = "")
    output$ej_opt_done <- renderUI(tags$p(
      class = "note",
      paste0("Report ", ref, " filed to ", lead, " under framework step ", sid, " (", step$title, ") and saved to the project database.")
    ))
  })

  output$ej_ap_table <- renderUI({
    a <- ap_all()
    ap <- a$ap[a$ap$id %in% a$ids, , drop = FALSE]
    ap <- ap[order(-ap$npv), , drop = FALSE]
    tags$div(
      tags$div(
        class = "ej-acts",
        lapply(seq_len(nrow(ap)), function(i) ej_act_pill(
          "ap", ap$id[i], paste0("#", i, " ", ap$id[i]), ap$option[i], ap$lead[i],
          c(ejo_causes[[ap$cause[i]]], paste0("gain ", fmt_bn(ap$gain[i])), paste0("NPV ", fmt_bn(ap$npv[i])),
            if (is.finite(ap$payback[i])) paste("pays back", ap$payback[i]) else "pays back after 2031"),
          paste0(fmt_num(ap$capex_bn[i], 1), " bn RWF"), i <= 3
        ))
      ),
      tags$p(class = "note", "0-1 knapsack (lpSolve): pick options to maximise the sum of net present values with total capital within the budget. Options interact in practice; the yearly gain above combines them as 1 \u2212 \u03a0(1 \u2212 effect \u00d7 roll-out).")
    )
  })

  # ---- institutional funding / 6-step / email (safe if no option open) ----
  ejo_inst_path <- file.path("reports", "ejo_inst_email.csv")
  output$ej_inst_step_panel <- renderUI({
    op <- ejo_inst_active_op(input$ej_act)
    if (is.null(op)) return(tags$p(class = "note", "Open a solution to see the framework step."))
    sid <- if (is.null(input$ej_inst_step)) "1" else as.character(input$ej_inst_step)
    step <- ejo_inst_steps[[max(1, min(6, suppressWarnings(as.integer(sid))))]]
    nst <- ejo_nst2_map[[op$cause]]
    if (is.null(nst)) nst <- list(nst2 = "", ministries = op$lead, instruments = ejo_or[[op$cause]])
    tags$div(
      tags$p(class = "kicker", paste("STEP", step$id, "OF 6")),
      tags$h3(step$title),
      tags$pre(class = "ej-inst-pre", step$body(op, nst)),
      tags$p(class = "note", "Partner reports filed while this step is selected are stored against it in the database.")
    )
  })
  output$ej_inst_milestones <- renderUI({
    op <- ejo_inst_active_op(input$ej_act)
    if (is.null(op) || is.null(ejo_user())) return(NULL)
    ejo_db_tick()
    sid <- if (is.null(input$ej_inst_step)) "1" else as.character(input$ej_inst_step)
    miles <- tryCatch(ejo_db_milestones(op$id, sid), error = function(e) data.frame())
    if (!nrow(miles)) return(tags$p(class = "note", "No milestones defined for this step."))
    selected <- miles$key[miles$done]
    checkboxGroupInput(
      "ej_mile_done",
      paste("Milestones for step", sid),
      choices = stats::setNames(miles$key, miles$label),
      selected = selected
    )
  })
  observeEvent(input$ej_mile_save, {
    user <- ejo_user()
    if (is.null(user)) {
      output$ej_mile_saved <- renderUI(hv_empty("Sign in to save milestones."))
      return()
    }
    op <- ejo_inst_active_op(input$ej_act)
    if (is.null(op)) return()
    sid <- if (is.null(input$ej_inst_step)) "1" else as.character(input$ej_inst_step)
    done <- if (is.null(input$ej_mile_done)) character() else input$ej_mile_done
    tryCatch(ejo_db_set_milestones(op$id, sid, done, user$username), error = function(e) NULL)
    ejo_db_tick(isolate(ejo_db_tick()) + 1L)
    output$ej_mile_saved <- renderUI(tags$p(class = "note", paste0("Milestones for step ", sid, " saved.")))
  }, ignoreInit = TRUE)
  observeEvent(input$ej_fund_save, {
    user <- ejo_user()
    if (is.null(user)) {
      output$ej_fund_saved <- renderUI(hv_empty("Sign in to save funding state."))
      return()
    }
    op <- ejo_inst_active_op(input$ej_act)
    if (is.null(op)) return()
    fund <- if (is.null(input$ej_fund_source)) "Government" else input$ej_fund_source
    st_key <- if (is.null(input$ej_fund_status)) "process" else input$ej_fund_status
    sid <- if (is.null(input$ej_inst_step)) "1" else as.character(input$ej_inst_step)
    pm_key <- if (is.null(input$ej_opt_pm)) "on_track" else input$ej_opt_pm
    tryCatch(ejo_db_save_state(op$id, op$cause, fund, st_key, sid, pm_key, user$username), error = function(e) NULL)
    ejo_db_tick(isolate(ejo_db_tick()) + 1L)
    output$ej_fund_saved <- renderUI(tags$p(class = "note", "Funding and framework state saved to the database."))
  }, ignoreInit = TRUE)
  output$ej_inst_eval <- renderUI({
    op <- ejo_inst_active_op(input$ej_act)
    if (is.null(op)) return(NULL)
    fund <- if (is.null(input$ej_fund_source)) "Government" else input$ej_fund_source
    st <- if (is.null(input$ej_fund_status)) "process" else input$ej_fund_status
    sid <- if (is.null(input$ej_inst_step)) "1" else as.character(input$ej_inst_step)
    ejo_inst_eval_ui(fund, st, sid)
  })
  # Do not depend on ej_act here — recreating the portal already seeds the textarea.
  observeEvent(
    list(input$ej_fund_source, input$ej_fund_status, input$ej_inst_step),
    {
      op <- ejo_inst_active_op(isolate(input$ej_act))
      if (is.null(op)) return()
      if (is.null(isolate(input$ej_email_preview))) return()
      fund <- if (is.null(input$ej_fund_source)) "Government" else input$ej_fund_source
      st_key <- if (is.null(input$ej_fund_status)) "process" else input$ej_fund_status
      st_lab <- if (st_key %in% names(ejo_fund_statuses)) ejo_fund_statuses[[st_key]] else st_key
      sid <- if (is.null(input$ej_inst_step)) "1" else as.character(input$ej_inst_step)
      isolate(updateTextAreaInput(session, "ej_email_preview", value = ejo_inst_email_body(op, fund, st_lab, sid)))
    },
    ignoreInit = TRUE
  )
  output$ej_fund_proposal <- downloadHandler(
    filename = function() {
      op <- ejo_inst_active_op(input$ej_act)
      id <- if (is.null(op)) "solution" else op$id
      paste0("Umuganura_Ejo_Funding_Proposal_", id, "_", format(Sys.Date(), "%Y%m%d"), ".txt")
    },
    content = function(file) {
      op <- ejo_inst_active_op(input$ej_act)
      fund <- if (is.null(input$ej_fund_source)) "Government" else input$ej_fund_source
      st_key <- if (is.null(input$ej_fund_status)) "process" else input$ej_fund_status
      st_lab <- if (st_key %in% names(ejo_fund_statuses)) ejo_fund_statuses[[st_key]] else st_key
      sid <- if (is.null(input$ej_inst_step)) "1" else as.character(input$ej_inst_step)
      writeLines(ejo_inst_proposal_text(op, fund, st_lab, sid), file, useBytes = TRUE)
    }
  )
  observeEvent(input$ej_email_send, {
    op <- ejo_inst_active_op(input$ej_act)
    if (is.null(op)) {
      output$ej_email_done <- renderUI(hv_empty("Open a solution before sending a report."))
      return()
    }
    to <- if (is.null(input$ej_email_to)) ejo_inst_email_for(ejo_lead_parts(op$lead)[1]) else input$ej_email_to
    body <- if (is.null(input$ej_email_preview) || !nzchar(trimws(input$ej_email_preview))) {
      fund <- if (is.null(input$ej_fund_source)) "Government" else input$ej_fund_source
      st_key <- if (is.null(input$ej_fund_status)) "process" else input$ej_fund_status
      st_lab <- if (st_key %in% names(ejo_fund_statuses)) ejo_fund_statuses[[st_key]] else st_key
      sid <- if (is.null(input$ej_inst_step)) "1" else as.character(input$ej_inst_step)
      ejo_inst_email_body(op, fund, st_lab, sid)
    } else input$ej_email_preview
    subj <- paste0("Umuganura Ejo Hazaza — ", op$id, " progress (", ejo_causes[[op$cause]], ")")
    mailto <- paste0(
      "mailto:", utils::URLencode(to, reserved = TRUE),
      "?subject=", utils::URLencode(subj, reserved = TRUE),
      "&body=", utils::URLencode(body, reserved = TRUE)
    )
    empty <- data.frame(
      filed = character(), option_id = character(), cause = character(), addressed_to = character(),
      funding = character(), status = character(), step = character(), subject = character(),
      stringsAsFactors = FALSE
    )
    log <- if (file.exists(ejo_inst_path)) {
      tryCatch(utils::read.csv(ejo_inst_path, stringsAsFactors = FALSE), error = function(e) empty)
    } else empty
    fund <- if (is.null(input$ej_fund_source)) "Government" else input$ej_fund_source
    st_key <- if (is.null(input$ej_fund_status)) "process" else input$ej_fund_status
    sid <- if (is.null(input$ej_inst_step)) "1" else as.character(input$ej_inst_step)
    out <- data.frame(
      filed = format(Sys.time(), "%Y-%m-%d %H:%M"), option_id = op$id, cause = op$cause,
      addressed_to = to, funding = fund, status = st_key, step = sid, subject = subj,
      stringsAsFactors = FALSE
    )
    dir.create(dirname(ejo_inst_path), showWarnings = FALSE)
    utils::write.table(out, ejo_inst_path, sep = ",", row.names = FALSE,
                       col.names = !file.exists(ejo_inst_path), append = file.exists(ejo_inst_path), qmethod = "double")
    session$sendCustomMessage("ejo_mailto", list(url = mailto))
    output$ej_email_done <- renderUI(tags$div(
      tags$p(class = "note", paste0("Report logged and opened for ", to, ". Evaluation below shows where the project sits.")),
      ejo_inst_eval_ui(fund, st_key, sid)
    ))
  }, ignoreInit = TRUE)

  # Full solution page (left Ejo overview when a solution pill is clicked).
  output$ej_sol_page <- renderUI({
    a <- input$ej_act
    req(a, isTruthy(a$id), a$id %in% ejo_options$id)
    ejo_db_tick()
    user <- ejo_user()
    state <- tryCatch(ejo_db_get_state(a$id), error = function(e) NULL)
    db_reps <- tryCatch(ejo_db_reports(a$id), error = function(e) NULL)
    ejo_option_portal(a$id, ejo_report_log(), user = user, state = state, db_reports = db_reps)
  })
  outputOptions(output, "ej_sol_page", suspendWhenHidden = FALSE)

  # Cause/action overlays: activities only (solutions use ej_sol_page).
  for (s in c(names(ejo_causes), "ap")) local({
    ss <- s
    oid <- paste0("ej_", ss, "_detail")
    output[[oid]] <- renderUI({
      a <- input$ej_act
      req(a, identical(a$scope, ss))
      if (a$id %in% ejo_options$id) {
        return(tags$p(class = "note", "This solution opens on its own page."))
      }
      ejo_activity_detail(ss, a$id, yr_f(ss), sel[[ss]]())
    })
    outputOptions(output, oid, suspendWhenHidden = FALSE)
  })
}
