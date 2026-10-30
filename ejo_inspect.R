# Post-harvest loss calculator (Inspect) — PDF-style farm inputs, charts, DeepSeek PDF.

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.character(a) && !nzchar(a))) b else a

ejo_inspect_causes <- c(
  weather = "Rain at harvest",
  storage = "Drying",
  handling = "Handling",
  transport = "Transport routes",
  cold = "Lack of cooling",
  glut = "Too much supply"
)

ejo_inspect_place_kind <- list(
  weather = c("Covered drying shed", "Cooperative yard", "Tarpaulin point"),
  storage = c("Cooperative dryer", "Raised drying hangar", "Grain store"),
  handling = c("Packing point", "Crate supplier", "Collection centre"),
  transport = c("Hub market", "Road rest shade", "Buyer gate"),
  cold = c("Cold room", "Shaded market stall", "Cooled dispatch bay"),
  glut = c("High-price market", "Holding hangar", "Buyer collection")
)

ejo_inspect_num <- function(x, default = 0) {
  v <- suppressWarnings(as.numeric(x))
  if (!length(v) || !is.finite(v[1])) default else v[1]
}

ejo_fmt_rwf <- function(x) {
  format(round(ejo_inspect_num(x, 0)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

ejo_inspect_xy <- function(district) {
  if (is.null(district) || !nzchar(district) || !exists("district_lnglat")) return(NULL)
  g <- district_lnglat[[district]]
  if (is.null(g)) return(NULL)
  list(lng = as.numeric(g$cx), lat = as.numeric(g$cy))
}

ejo_inspect_places <- function(cause, district) {
  xy <- ejo_inspect_xy(district)
  kinds <- ejo_inspect_place_kind[[cause]]
  if (is.null(kinds)) kinds <- c("Collection point", "Store", "Market")
  if (is.null(xy) || !is.finite(xy$lng) || !is.finite(xy$lat)) {
    return(data.frame(
      name = character(), kind = character(), district = character(),
      lng = numeric(), lat = numeric(), km = numeric(),
      maps = character(), stringsAsFactors = FALSE
    ))
  }
  off <- list(c(0.04, 0.02), c(-0.03, 0.05), c(0.02, -0.04))
  rows <- lapply(seq_along(kinds), function(i) {
    o <- off[[((i - 1L) %% length(off)) + 1L]]
    lng <- xy$lng + o[1]
    lat <- xy$lat + o[2]
    km <- round(sqrt((lng - xy$lng)^2 + (lat - xy$lat)^2) * 111, 1)
    maps <- sprintf("https://www.google.com/maps/dir/?api=1&destination=%s,%s", lat, lng)
    data.frame(
      name = paste(district, kinds[i]), kind = kinds[i], district = district,
      lng = lng, lat = lat, km = km, maps = maps, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# ---- PDF-style calculator (RWF) ---------------------------------------------

ejo_calc_read <- function(input, ns) {
  list(
    crop = input[[ns("crop")]] %||% "Beans",
    district = input[[ns("district")]] %||% "",
    harvest_date = input[[ns("harvest_date")]] %||% "",
    total_kg = ejo_inspect_num(input[[ns("total_kg")]], 1000),
    spoiled_kg = ejo_inspect_num(input[[ns("spoiled_kg")]], 0),
    down_kg = ejo_inspect_num(input[[ns("down_kg")]], 0),
    price_good = ejo_inspect_num(input[[ns("price_good")]], 1000),
    price_down = ejo_inspect_num(input[[ns("price_down")]], 600),
    extra_cost = ejo_inspect_num(input[[ns("extra_cost")]], 0),
    moisture = ejo_inspect_num(input[[ns("moisture")]], 14),
    hours = ejo_inspect_num(input[[ns("hours")]], 5),
    distance_km = ejo_inspect_num(input[[ns("distance_km")]], 40),
    qty_below = ejo_inspect_num(input[[ns("qty_below")]], 0),
    price_actual = ejo_inspect_num(input[[ns("price_actual")]], 0),
    wait_spoiled = ejo_inspect_num(input[[ns("wait_spoiled")]], 0)
  )
}

ejo_calc_compute <- function(cause, x) {
  total <- max(0, x$total_kg)
  spoiled <- max(0, min(x$spoiled_kg, total))
  down <- max(0, min(x$down_kg, max(0, total - spoiled)))
  price_g <- max(0, x$price_good)
  price_d <- max(0, min(x$price_down, price_g))
  extra <- max(0, x$extra_cost)

  spoiled_rwf <- spoiled * price_g
  quality_rwf <- down * (price_g - price_d)
  price_rwf <- 0
  wait_rwf <- 0
  labels <- c("Spoiled produce", "Reduced quality", "Additional costs")
  values <- c(spoiled_rwf, quality_rwf, extra)
  note_extra <- "Extra costs entered by the farmer for this cause only."
  moisture_note <- NULL

  if (identical(cause, "weather")) {
    note_extra <- "Extra drying, tarpaulins, or labour caused by rain."
  } else if (identical(cause, "storage")) {
    note_extra <- "Extra drying fuel/labour beyond normal moisture loss."
    if (is.finite(x$moisture) && x$moisture > 13) {
      moisture_note <- sprintf("Moisture reported at %.1f%% (above 13%% store target).", x$moisture)
    }
  } else if (identical(cause, "handling")) {
    labels[1] <- "Crushed / damaged"
    note_extra <- "Packaging or handling costs linked to damage."
  } else if (identical(cause, "cold")) {
    note_extra <- "Extra cooling, ice, or shade costs for this lot."
  } else if (identical(cause, "transport")) {
    labels[1] <- "Damaged in transit"
    note_extra <- "Extra transport or delay costs on this route."
  } else if (identical(cause, "glut")) {
    qty_below <- max(0, min(x$qty_below, total))
    price_act <- max(0, x$price_actual)
    wait_sp <- max(0, min(x$wait_spoiled, max(0, total - qty_below)))
    price_rwf <- qty_below * max(0, price_g - price_act)
    wait_rwf <- wait_sp * price_g
    spoiled <- wait_sp
    down <- 0
    spoiled_rwf <- wait_rwf
    quality_rwf <- 0
    labels <- c("Sold below reference", "Spoiled while waiting", "Extra storage costs")
    values <- c(price_rwf, wait_rwf, extra)
    note_extra <- "Holding or storage costs while waiting for a buyer."
  }

  total_rwf <- sum(values)
  affected <- if (identical(cause, "glut")) {
    max(0, min(total, x$qty_below)) + max(0, min(total, x$wait_spoiled))
  } else spoiled + down
  affected <- min(affected, total)
  safe_kg <- max(0, total - affected)
  per_kg <- if (total > 0) total_rwf / total else 0
  biggest <- if (total_rwf > 0) labels[which.max(values)] else "None"

  list(
    cause = cause,
    cause_label = ejo_inspect_causes[[cause]] %||% cause,
    crop = x$crop,
    district = x$district,
    harvest_date = as.character(x$harvest_date %||% ""),
    total_kg = total,
    spoiled_kg = spoiled,
    down_kg = if (identical(cause, "glut")) 0 else down,
    wait_spoiled_kg = if (identical(cause, "glut")) max(0, x$wait_spoiled) else 0,
    qty_below = if (identical(cause, "glut")) max(0, x$qty_below) else 0,
    price_good = price_g,
    price_down = price_d,
    price_actual = if (identical(cause, "glut")) max(0, x$price_actual) else price_d,
    extra_cost = extra,
    moisture = x$moisture,
    hours = x$hours,
    distance_km = x$distance_km,
    labels = labels,
    values = values,
    total_rwf = total_rwf,
    affected_kg = affected,
    safe_kg = safe_kg,
    per_kg = per_kg,
    biggest = biggest,
    note_extra = note_extra,
    moisture_note = moisture_note
  )
}

ejo_calc_fallback_advice <- function(cause, lang = "en") {
  en <- list(
    weather = c(
      "Move wet grain onto a raised floor or tarpaulin the same day; do not bag above 13% moisture.",
      "Ask the cooperative for a covered drying shed or shared dryer near your district centre.",
      "A low-cost dryer: raised wooden rack, corrugated sheet roof, and airflow under the floor — keep bags off soil.",
      "Harvest on the first dry morning after rain; Open-Meteo and the harvest forecast help pick that day."
    ),
    storage = c(
      "Dry to about 13% before long storage; use a cooperative dryer when moisture stays high.",
      "Build a raised hangar: posts, raised platform, roof overhang, and mesh for airflow.",
      "Keep bags on pallets away from walls; check for mould weekly.",
      "Share dryer time with neighbours so each lot finishes before bagging."
    ),
    handling = c(
      "Sort soft fruit before packing; use crates for tomato and avocado instead of overfilled sacks.",
      "Buy or share plastic crates from the packing-point pin on the map.",
      "Load heavy bags at the bottom; do not throw produce into the truck.",
      "Train pickers to leave bruised fruit for local use, not the main load."
    ),
    transport = c(
      "Pick the shorter safe route to the hub market; leave at a cool hour.",
      "Cover the load from sun and rain; do not leave produce waiting at the gate.",
      "Use a shaded rest stop on long corridors; split soft and hard lots.",
      "Compare travel hours against spoilage — a slightly longer but cooler road can lose less money."
    ),
    cold = c(
      "Shade within the first hour after harvest; sell the softest lot the same day.",
      "Move the rest to a cold room or cooled dispatch bay if one is near your district.",
      "Wet cloth or evaporative shade helps only for a short wait — not overnight for soft crops.",
      "Stagger harvest so cold capacity matches the waiting crop."
    ),
    glut = c(
      "Compare your offer with another market before you add to a flooded stand.",
      "Hold part of the lot in a hangar if the fee is lower than the price gap.",
      "Stagger sales over two or three days; process or dry surplus when prices collapse.",
      "Call a buyer collection point early so trucks do not arrive after the glut peak."
    )
  )
  rw <- list(
    weather = c(
      "Shyira ibigori binyoroye ku kibuga cyubatswe cyangwa ku tarpulin uwo munsi; ntushire mu mifuka hejuru ya 13% y'ubushuhe.",
      "Baza umushariki wawe ikibuga cyo kumukama cyangwa umushariki wa dryer hafi y'akarere.",
      "Dryer y'igiciro gito: urutaro rw'ibiti, isate y'amabati, n'umwuka hepfo — imifuka ntigire ku butaka.",
      "Sarura mu gitondo cyiza nyuma y'imvura; reba ubuhanuzi bw'imvura."
    ),
    storage = c(
      "Umukame kugeza hafi ya 13% mbere yo kubika; koresha dryer y'umushariki iyo ubushuhe bukomeje.",
      "Kubaka hangari: inkwi, urutaro rwo hejuru, isate n'umwuka.",
      "Shyira imifuka ku palette kure y'urukuta; reba urwagara buri cyumweru.",
      "Sangira dryer n'abaturanyi kugira ngo buri mutungo urangire mbere yo gushira mu mifuka."
    ),
    handling = c(
      "Tandukanya imbuto zomye mbere; koresha ibitebo ku binyomoro n'avocado aho kuzuza imifuka cyane.",
      "Gura cyangwa sangira ibitebo ku gice cyo gupakira kiri ku ikarita.",
      "Shyira ibiremereye hepfo; ntumupe ibihanguje mu ikamyo.",
      "Wigishe abasaruzi gusiga ibyahuye ku gukoresha mu rugo, si mu muzigo mukuru."
    ),
    transport = c(
      "Hitamo inzira ngufi n'izateye ujya ku isoko; va mu isaha yo gukonja.",
      "Fata umuzigo ku zuba n'imvura; ntureka ibihanguje mu muryango.",
      "Koresha ahantu hashushye ku nzira ndende; tandukanya ibyoroshye n'ibikomeye.",
      "Gereranya amasaha y'inzira n'amahirwe yo gupfura — inzira yo gukonja ishobora kuzigama amafaranga."
    ),
    cold = c(
      "Shyira mu gicucu mu isaha ya mbere; gurisha ibyomye uyu munsi.",
      "Ijye mu cyumba gikonjesha niba hafi y'akarere kawe.",
      "Impuzu nyinshi ifasha gato — si ijoro ryose ku bihanguje.",
      "Sarura gahoro kugira ngo ubushobozi bwo gukonja buhure n'umusaruro."
    ),
    glut = c(
      "Gereranya igiciro n'andi masoko mbere yo kongera ku isoko ryuzuye.",
      "Bika igice mu hangari niba amafaranga yaho ari make kuruta igiciro cyaguye.",
      "Gurisha mu minsi ibiri cyangwa itatu; tekereza gutunganya cyangwa kumukama iyo igiciro cyaguye.",
      "Hamagara umuguzi mbere kugira ngo ikamyo itazaza nyuma y'isoko ryuzuye."
    )
  )
  pool <- if (identical(lang, "rw")) rw else en
  pool[[cause]] %||% character()
}

ejo_calc_deepseek <- function(calc, places, lang = "en") {
  lang <- if (identical(lang, "rw")) "rw" else "en"
  figures <- list(
    cause = calc$cause_label,
    crop = calc$crop,
    district = calc$district,
    total_kg = calc$total_kg,
    total_rwf = calc$total_rwf,
    components = setNames(as.list(calc$values), calc$labels),
    affected_kg = calc$affected_kg,
    biggest = calc$biggest,
    places = if (nrow(places)) paste(places$kind, collapse = "; ") else "",
    language = lang
  )
  question <- if (identical(lang, "rw")) {
    paste(
      "Andika gahunda yo gufasha umuhinzi wo mu Rwanda (Kinyarwanda gusa).",
      "Ukoreshe gusa imibare yatanzwe. Ntukore imibare mishya.",
      "Tanga: (1) icyo yakora uyu munsi, (2) ahantu hafi (isoko, hangari, dryer, inzira),",
      "(3) uko yakubaka dryer cyangwa hangari y'igiciro gito, (4) uko yazigama iyi mpungenge.",
      "Injiza imirongo 6-10 gusa."
    )
  } else {
    paste(
      "Write a practical farmer action plan for Rwanda (English only).",
      "Use only the figures supplied. Do not invent new loss statistics.",
      "Include: (1) what to do today, (2) nearby places (market, hangar, dryer, route),",
      "(3) how to build a low-cost dryer or hangar if relevant, (4) how to avoid this loss next season.",
      "6-10 short lines only."
    )
  }
  url <- "http://127.0.0.1:7452/api/deepseek"
  body <- jsonlite::toJSON(list(question = question, mode = "farmer_plan", figures = figures), auto_unbox = TRUE)
  tryCatch({
    j <- NULL
    if (requireNamespace("httr2", quietly = TRUE)) {
      resp <- httr2::req_perform(
        httr2::request(url) |>
          httr2::req_method("POST") |>
          httr2::req_headers("Content-Type" = "application/json") |>
          httr2::req_body_raw(body, "application/json") |>
          httr2::req_timeout(28)
      )
      j <- httr2::resp_body_json(resp)
    } else if (requireNamespace("httr", quietly = TRUE)) {
      resp <- httr::POST(url, body = body, httr::content_type_json(), httr::timeout(28))
      j <- httr::content(resp, as = "parsed", type = "application/json")
    }
    if (isTRUE(j$ok) && nzchar(j$text %||% "")) {
      return(list(ok = TRUE, text = j$text, source = "DeepSeek"))
    }
    list(
      ok = FALSE,
      text = paste(ejo_calc_fallback_advice(calc$cause, lang), collapse = "\n"),
      source = "local guide",
      error = if (is.null(j)) "Wording bridge not reachable" else (j$error %||% "DeepSeek unavailable")
    )
  }, error = function(e) {
    list(
      ok = FALSE,
      text = paste(ejo_calc_fallback_advice(calc$cause, lang), collapse = "\n"),
      source = "local guide",
      error = "Wording bridge not reachable"
    )
  })
}

ejo_calc_write_pdf <- function(path, calc, advice, places, lang = "en") {
  title <- if (identical(lang, "rw")) "Kalkilateri y'amahirwe yo ku musaruro" else "Post-harvest loss calculator"
  subtitle <- if (identical(lang, "rw")) "Gahunda y'umuhinzi · Umuganura" else "Farmer action plan · Umuganura"
  grDevices::pdf(path, width = 8.27, height = 11.69, paper = "a4", onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  op <- par(mar = c(0.8, 0.8, 0.8, 0.8), family = "sans")
  on.exit(par(op), add = TRUE)

  draw_page <- function(lines, header = TRUE) {
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))
    y <- 0.96
    put <- function(txt, size = 11, bold = FALSE, gap = 0.028, col = "black") {
      if (!nzchar(txt)) return(invisible(NULL))
      wrapped <- strwrap(txt, width = 92)
      for (w in wrapped) {
        if (y < 0.06) return(FALSE)
        text(0.05, y, w, adj = c(0, 1), cex = size / 12, font = if (bold) 2 else 1, col = col)
        y <<- y - gap
      }
      TRUE
    }
    if (header) {
      put(title, 16, TRUE, 0.04, "#0f2f4a")
      put(subtitle, 11, FALSE, 0.03, "#14804a")
      put(paste0(calc$cause_label, " · ", calc$crop, " · ", calc$district), 12, TRUE, 0.032)
      put(paste0(
        if (identical(lang, "rw")) "Igiciro cyose cyatakaye: " else "Total financial loss: ",
        ejo_fmt_rwf(calc$total_rwf), " RWF"
      ), 13, TRUE, 0.034, "#8b1e1e")
      put(paste0(
        ejo_fmt_rwf(calc$affected_kg), " kg ",
        if (identical(lang, "rw")) "byatewe n'ikibazo kuri " else "affected of ",
        ejo_fmt_rwf(calc$total_kg), " kg · ",
        ejo_fmt_rwf(calc$per_kg), " RWF/kg"
      ), 10, FALSE, 0.03)
      put("", 10, FALSE, 0.01)
    }
    for (ln in lines) {
      ok <- put(ln, 10, grepl("^[A-Z0-9].*:$|^[0-9]+\\.", ln), 0.026)
      if (!isTRUE(ok)) break
    }
    text(0.05, 0.035, "Umuganura · figures from farmer inputs only · not NISR official estimates", adj = c(0, 0), cex = 0.65, col = "#666666")
  }

  head_lines <- c(
    if (identical(lang, "rw")) "Ibisubizo by'imibare:" else "Calculation:",
    paste0("· ", calc$labels[1], ": ", ejo_fmt_rwf(calc$values[1]), " RWF"),
    paste0("· ", calc$labels[2], ": ", ejo_fmt_rwf(calc$values[2]), " RWF"),
    paste0("· ", calc$labels[3], ": ", ejo_fmt_rwf(calc$values[3]), " RWF"),
    paste0(if (identical(lang, "rw")) "· Igice kinini: " else "· Largest component: ", calc$biggest),
    if (!is.null(calc$moisture_note)) paste0("· ", calc$moisture_note) else NULL,
    "",
    if (identical(lang, "rw")) "Ahantu hafi:" else "Nearby places:",
    if (nrow(places)) paste0("· ", places$kind, " — ", places$name, " (~", places$km, " km)") else
      (if (identical(lang, "rw")) "· Hitamo akarere ku ikarita." else "· Choose a district on the map."),
    "",
    if (identical(lang, "rw")) "Inama (DeepSeek / gahunda):" else "Advice (DeepSeek / guide):"
  )
  advice_lines <- unlist(strsplit(advice$text %||% "", "\n", fixed = TRUE))
  advice_lines <- trimws(advice_lines)
  advice_lines <- advice_lines[nzchar(advice_lines)]
  if (!length(advice_lines)) advice_lines <- ejo_calc_fallback_advice(calc$cause, lang)
  src <- paste0(if (identical(lang, "rw")) "Inkomoko: " else "Source: ", advice$source %||% "guide")
  all_lines <- c(head_lines, advice_lines, "", src)
  draw_page(all_lines, TRUE)
  invisible(path)
}

ejo_inspect_forecast <- function(district) {
  xy <- ejo_inspect_xy(district)
  if (is.null(xy)) return(list(ok = FALSE, error = "Pick a district first."))
  url <- sprintf(
    "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&daily=precipitation_sum&timezone=Africa%%2FKigali&forecast_days=16",
    xy$lat, xy$lng
  )
  tryCatch({
    raw <- if (requireNamespace("httr2", quietly = TRUE)) {
      httr2::req_perform(httr2::request(url) |> httr2::req_timeout(12)) |> httr2::resp_body_string()
    } else {
      paste(readLines(url, warn = FALSE, encoding = "UTF-8"), collapse = "")
    }
    j <- jsonlite::fromJSON(raw)
    days <- j$daily$time
    rain <- as.numeric(j$daily$precipitation_sum)
    if (!length(days) || !length(rain)) return(list(ok = FALSE, error = "Open-Meteo returned no days."))
    wet <- rain >= 5
    best <- which(!wet)
    best_day <- if (length(best)) days[best[1]] else days[which.min(rain)]
    list(ok = TRUE, district = district, days = days, rain = rain, wet = wet, best = best_day,
         source = "Open-Meteo daily precipitation · Africa/Kigali")
  }, error = function(e) list(ok = FALSE, error = "Open-Meteo did not answer. Try again in a moment."))
}

ejo_inspect_field <- function(id, label, hint, ...) {
  tags$div(
    class = "ej-inspect-field",
    tags$label(`for` = id, label),
    if (nzchar(hint)) tags$p(class = "ej-inspect-hint", hint),
    ...
  )
}

ejo_calc_fields_ui <- function(ns, cause) {
  dist_ch <- if (exists("ejo_districts")) ejo_districts else character()
  common_top <- tagList(
    ejo_inspect_field(ns("district"), "District", "Where this farm sits.",
      selectInput(ns("district"), NULL, choices = dist_ch, selected = if ("Musanze" %in% dist_ch) "Musanze" else dist_ch[1], width = "100%", selectize = FALSE)),
    ejo_inspect_field(ns("crop"), "Crop", "Crop on this harvest.",
      selectInput(ns("crop"), NULL, choices = c("Beans", "Maize", "Paddy rice", "Irish potato", "Tomato", "Avocado", "Cassava"), selected = "Beans", width = "100%", selectize = FALSE)),
    ejo_inspect_field(ns("harvest_date"), "Harvest date", "Day this lot left the field.",
      dateInput(ns("harvest_date"), NULL, value = Sys.Date(), width = "100%")),
    ejo_inspect_field(ns("total_kg"), "Total quantity harvested (kg)", "Base quantity for percentages.",
      numericInput(ns("total_kg"), NULL, value = 1000, min = 0, step = 10, width = "100%"))
  )
  price_block <- tagList(
    ejo_inspect_field(ns("price_good"), "Normal selling price (RWF/kg)", "Reference price for good quality.",
      numericInput(ns("price_good"), NULL, value = 1000, min = 0, step = 10, width = "100%")),
    ejo_inspect_field(ns("price_down"), "Price of downgraded produce (RWF/kg)", "Actual price for lower grade.",
      numericInput(ns("price_down"), NULL, value = 600, min = 0, step = 10, width = "100%"))
  )

  cause_fields <- switch(
    cause,
    weather = tagList(
      ejo_inspect_field(ns("spoiled_kg"), "Completely spoiled by rain (kg)", "No sale or use value.",
        numericInput(ns("spoiled_kg"), NULL, value = 50, min = 0, step = 1, width = "100%")),
      ejo_inspect_field(ns("down_kg"), "Downgraded quantity (kg)", "Still saleable at a lower price.",
        numericInput(ns("down_kg"), NULL, value = 100, min = 0, step = 1, width = "100%")),
      price_block,
      ejo_inspect_field(ns("extra_cost"), "Extra costs caused by rain (RWF)", "Only additional costs linked to rain.",
        numericInput(ns("extra_cost"), NULL, value = 10000, min = 0, step = 100, width = "100%"))
    ),
    storage = tagList(
      ejo_inspect_field(ns("spoiled_kg"), "Spoiled from poor drying (kg)", "Lost to mould or heat damage.",
        numericInput(ns("spoiled_kg"), NULL, value = 40, min = 0, step = 1, width = "100%")),
      ejo_inspect_field(ns("down_kg"), "Grade reduced by moisture (kg)", "Sold at a lower grade price.",
        numericInput(ns("down_kg"), NULL, value = 120, min = 0, step = 1, width = "100%")),
      ejo_inspect_field(ns("moisture"), "Moisture reading (%)", "Above 13% needs more drying before store.",
        numericInput(ns("moisture"), NULL, value = 16, min = 8, max = 30, step = 0.5, width = "100%")),
      price_block,
      ejo_inspect_field(ns("extra_cost"), "Extra drying costs (RWF)", "Fuel, labour, dryer fees beyond normal.",
        numericInput(ns("extra_cost"), NULL, value = 8000, min = 0, step = 100, width = "100%"))
    ),
    handling = tagList(
      ejo_inspect_field(ns("spoiled_kg"), "Crushed or damaged (kg)", "No longer saleable.",
        numericInput(ns("spoiled_kg"), NULL, value = 30, min = 0, step = 1, width = "100%")),
      ejo_inspect_field(ns("down_kg"), "Downgraded from bruising (kg)", "Sold at a lower price.",
        numericInput(ns("down_kg"), NULL, value = 80, min = 0, step = 1, width = "100%")),
      price_block,
      ejo_inspect_field(ns("extra_cost"), "Packaging / handling extra costs (RWF)", "Only costs caused by damage.",
        numericInput(ns("extra_cost"), NULL, value = 5000, min = 0, step = 100, width = "100%"))
    ),
    cold = tagList(
      ejo_inspect_field(ns("spoiled_kg"), "Spoiled without cooling (kg)", "Perished before sale.",
        numericInput(ns("spoiled_kg"), NULL, value = 60, min = 0, step = 1, width = "100%")),
      ejo_inspect_field(ns("down_kg"), "Downgraded from heat (kg)", "Still sold at a lower price.",
        numericInput(ns("down_kg"), NULL, value = 90, min = 0, step = 1, width = "100%")),
      ejo_inspect_field(ns("hours"), "Hours in heat before sale (optional)", "Longer waits raise spoilage on soft crops.",
        numericInput(ns("hours"), NULL, value = 8, min = 0, max = 72, step = 0.5, width = "100%")),
      price_block,
      ejo_inspect_field(ns("extra_cost"), "Extra cooling / ice costs (RWF)", "Only costs linked to lack of cold chain.",
        numericInput(ns("extra_cost"), NULL, value = 12000, min = 0, step = 100, width = "100%"))
    ),
    transport = tagList(
      ejo_inspect_field(ns("spoiled_kg"), "Damaged in transit (kg)", "Lost on the road.",
        numericInput(ns("spoiled_kg"), NULL, value = 35, min = 0, step = 1, width = "100%")),
      ejo_inspect_field(ns("down_kg"), "Downgraded on arrival (kg)", "Sold at a lower price.",
        numericInput(ns("down_kg"), NULL, value = 70, min = 0, step = 1, width = "100%")),
      ejo_inspect_field(ns("distance_km"), "Route distance (km, optional)", "Helps compare corridors.",
        numericInput(ns("distance_km"), NULL, value = 45, min = 0, step = 1, width = "100%")),
      ejo_inspect_field(ns("hours"), "Travel / wait hours (optional)", "Time on the road or at the gate.",
        numericInput(ns("hours"), NULL, value = 6, min = 0, max = 72, step = 0.5, width = "100%")),
      price_block,
      ejo_inspect_field(ns("extra_cost"), "Extra transport / delay costs (RWF)", "Only costs caused by this route.",
        numericInput(ns("extra_cost"), NULL, value = 15000, min = 0, step = 100, width = "100%"))
    ),
    glut = tagList(
      ejo_inspect_field(ns("qty_below"), "Quantity sold below reference price (kg)", "Sold cheap because of oversupply.",
        numericInput(ns("qty_below"), NULL, value = 400, min = 0, step = 1, width = "100%")),
      ejo_inspect_field(ns("price_good"), "Reference / expected price (RWF/kg)", "Price you expected without the glut.",
        numericInput(ns("price_good"), NULL, value = 1000, min = 0, step = 10, width = "100%")),
      ejo_inspect_field(ns("price_actual"), "Actual selling price (RWF/kg)", "What buyers paid in the glut.",
        numericInput(ns("price_actual"), NULL, value = 600, min = 0, step = 10, width = "100%")),
      ejo_inspect_field(ns("wait_spoiled"), "Spoiled while waiting for buyers (kg)", "Unsold lot that spoiled.",
        numericInput(ns("wait_spoiled"), NULL, value = 50, min = 0, step = 1, width = "100%")),
      ejo_inspect_field(ns("extra_cost"), "Extra storage / holding costs (RWF)", "Hangar or holding fees while waiting.",
        numericInput(ns("extra_cost"), NULL, value = 7000, min = 0, step = 100, width = "100%"))
    ),
    tagList(
      ejo_inspect_field(ns("spoiled_kg"), "Completely spoiled (kg)", "",
        numericInput(ns("spoiled_kg"), NULL, value = 50, min = 0, step = 1, width = "100%")),
      ejo_inspect_field(ns("down_kg"), "Downgraded (kg)", "",
        numericInput(ns("down_kg"), NULL, value = 100, min = 0, step = 1, width = "100%")),
      price_block,
      ejo_inspect_field(ns("extra_cost"), "Extra costs (RWF)", "",
        numericInput(ns("extra_cost"), NULL, value = 10000, min = 0, step = 100, width = "100%"))
    )
  )

  tagList(common_top, cause_fields)
}

ejo_inspect_ui <- function(cause) {
  is_over <- identical(cause, "over")
  lab <- if (is_over) "Post-harvest loss calculator" else (ejo_inspect_causes[[cause]] %||% cause)
  ns <- function(x) paste0("ej_ins_", cause, "_", x)
  panel_id <- paste0("ej-inspect-", cause)
  tags$div(
    class = "ej-inspect",
    `data-cause` = cause,
    tags$div(
      class = "ej-inspect-bar",
      tags$div(
        class = "ej-inspect-bar-copy",
        tags$p(class = "ej-inspect-kicker", if (is_over) "FARM CALCULATOR" else "FARM AND POLICY DECISION"),
        tags$p(
          class = "ej-inspect-lede",
          if (is_over) "Open Inspect — pick a loss cause, enter this harvest's numbers, and see the RWF result, charts, and physical quantity affected."
          else paste0("Open Inspect for ", lab, " — enter this farm's loss numbers, charts, nearby places, and the quantity table.")
        )
      ),
      tags$button(type = "button", class = "ej-inspect-open", `data-ej-inspect` = cause, "Inspect")
    ),
    tags$div(
      class = "ej-inspect-panel", id = panel_id, `aria-hidden` = "true",
      tags$div(
        class = "ej-inspect-head",
        tags$button(type = "button", class = "ej-inspect-close", `data-ej-inspect-close` = cause, "\u2190 Back"),
        tags$div(
          tags$p(class = "ej-inspect-kicker", if (is_over) "POST-HARVEST LOSS CALCULATOR" else toupper(lab)),
          tags$h3(if (is_over) "Calculate this harvest's loss" else "Inspect this cause")
        )
      ),
      tags$div(
        class = "ej-inspect-grid",
        tags$section(
          class = "ej-inspect-card",
          tags$h4("Your farm numbers"),
          tags$p(class = "note", "Type this harvest's figures. The loss, charts and quantity table update from what you enter."),
          if (is_over) ejo_inspect_field(ns("cause"), "Select a loss cause", "Only the fields for that cause appear.",
            selectInput(
              ns("cause"), NULL,
              choices = stats::setNames(names(ejo_inspect_causes), unname(ejo_inspect_causes)),
              selected = "weather", width = "100%", selectize = FALSE
            )),
          if (is_over) uiOutput(ns("fields")) else ejo_calc_fields_ui(ns, cause),
          actionButton(ns("go"), "Calculate my loss", class = "ej-inspect-go"),
          uiOutput(ns("loss"))
        ),
        tags$section(
          class = "ej-inspect-card",
          tags$h4("Nearby places"),
          tags$p(class = "note", "Green pin = your district. Blue pins = hangars, dryers, markets, or packing points. Open Maps or WhatsApp."),
          if (requireNamespace("leaflet", quietly = TRUE)) leaflet::leafletOutput(ns("map"), height = "320px"),
          uiOutput(ns("places"))
        ),
        tags$section(
          class = "ej-inspect-card ej-inspect-wide",
          tags$h4("Visualizations"),
          tags$p(class = "note", "Financial loss by component, share of the total, and physical quantity affected."),
          plotlyOutput(ns("chart_money"), height = "280px"),
          plotlyOutput(ns("chart_share"), height = "260px"),
          uiOutput(ns("qty_table"))
        ),
        if (!is_over && identical(cause, "weather")) tags$section(
          class = "ej-inspect-card ej-inspect-wide",
          tags$h4("Harvest window · Open-Meteo"),
          actionButton(ns("forecast"), "Load 16-day rain forecast", class = "ej-inspect-go"),
          uiOutput(ns("forecast_out"))
        )
      )
    )
  )
}

ejo_inspect_bind_one <- function(input, output, session, panel_key, cause_fixed = NULL) {
  ns <- function(x) paste0("ej_ins_", panel_key, "_", x)
  calc_store <- reactiveVal(NULL)
  advice_store <- reactiveVal(NULL)

  active_cause <- reactive({
    if (!is.null(cause_fixed)) return(cause_fixed)
    c <- input[[ns("cause")]]
    if (is.null(c) || !nzchar(c) || !c %in% names(ejo_inspect_causes)) "weather" else c
  })

  if (is.null(cause_fixed)) {
    output[[ns("fields")]] <- renderUI({
      ejo_calc_fields_ui(ns, active_cause())
    })
  }

  # Only spend map/chart work while this cause/overview step is on screen.
  step_for <- c(over = "0", weather = "1", storage = "2", handling = "3",
                transport = "4", cold = "5", glut = "6")
  panel_live <- function() {
    s <- input$ej_active_step
    want <- step_for[[panel_key]]
    !is.null(s) && !is.null(want) && identical(as.character(s)[1], want)
  }

  # Live result from whatever the farmer typed (defaults fill the table immediately).
  calc_now <- reactive({
    req(panel_live())
    req(!is.null(input[[ns("total_kg")]]))
    ejo_calc_compute(active_cause(), ejo_calc_read(input, ns))
  })

  places <- reactive({
    req(panel_live())
    d <- input[[ns("district")]]
    req(d)
    ejo_inspect_places(active_cause(), d)
  })

  output[[ns("map")]] <- leaflet::renderLeaflet({
    req(panel_live())
    p <- places()
    d <- input[[ns("district")]]
    xy <- ejo_inspect_xy(d)
    m <- leaflet::leaflet(options = leaflet::leafletOptions(scrollWheelZoom = FALSE)) |>
      leaflet::addTiles(
        urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>'
      )
    if (!is.null(xy) && is.finite(xy$lng) && is.finite(xy$lat)) {
      m <- leaflet::setView(m, lng = xy$lng, lat = xy$lat, zoom = 10) |>
        leaflet::addCircleMarkers(
          lng = xy$lng, lat = xy$lat, radius = 9, color = "#0f5c3d", fillColor = "#14804a",
          fillOpacity = 0.95, weight = 2, label = paste(d, "· your district"),
          popup = paste0("<b>", htmltools::htmlEscape(d), "</b><br/>Farm district centre")
        )
    }
    if (nrow(p)) {
      m <- leaflet::addCircleMarkers(
        m, lng = p$lng, lat = p$lat, radius = 8, color = "#123a7a", fillColor = "#1f5fd1",
        fillOpacity = 0.95, weight = 2, label = paste0(p$kind, " · ", p$km, " km"),
        popup = paste0(
          "<b>", htmltools::htmlEscape(p$kind), "</b><br/>", htmltools::htmlEscape(p$name),
          "<br/><a href='", htmltools::htmlEscape(p$maps), "' target='_blank' rel='noopener'>Open in Google Maps</a>"
        )
      )
    }
    m
  })

  output[[ns("places")]] <- renderUI({
    req(panel_live())
    p <- places()
    if (!nrow(p)) return(tags$p(class = "note", "Choose a district to see nearby places."))
    tags$div(
      class = "ej-inspect-place-list",
      lapply(seq_len(nrow(p)), function(i) {
        row <- p[i, ]
        msg <- utils::URLencode(paste0(row$name, " (", row$kind, "). Maps: ", row$maps), reserved = TRUE)
        wa <- paste0("https://wa.me/?text=", msg)
        tags$article(
          class = "ej-inspect-place",
          tags$strong(row$kind),
          tags$p(paste0(row$name, " · about ", row$km, " km from the district centre")),
          tags$div(
            class = "ej-inspect-place-actions",
            tags$a(href = row$maps, target = "_blank", rel = "noopener", class = "ej-inspect-link", "Open in Google Maps"),
            tags$a(href = wa, target = "_blank", rel = "noopener", class = "ej-inspect-link is-wa", "Send on WhatsApp")
          )
        )
      })
    )
  })

  observeEvent(input[[ns("go")]], {
    calc_store(calc_now())
  }, ignoreInit = TRUE)

  observe({
    calc_store(calc_now())
  })

  output[[ns("loss")]] <- renderUI({
    calc <- calc_now()
    req(!is.null(calc))
    tags$div(
      class = "ej-inspect-loss",
      tags$p(class = "ej-inspect-loss-kicker", "TOTAL FINANCIAL LOSS"),
      tags$p(class = "ej-inspect-loss-value", paste0(ejo_fmt_rwf(calc$total_rwf), " RWF")),
      tags$p(
        class = "ej-inspect-loss-rate",
        paste0(
          ejo_fmt_rwf(calc$spoiled_kg), " kg spoiled · ",
          ejo_fmt_rwf(calc$down_kg + calc$wait_spoiled_kg), " kg quality/wait · ",
          ejo_fmt_rwf(calc$affected_kg), " kg affected of ",
          ejo_fmt_rwf(calc$total_kg), " kg"
        )
      ),
      tags$p(paste0(ejo_fmt_rwf(calc$per_kg), " RWF lost per kg harvested · largest: ", calc$biggest)),
      if (!is.null(calc$moisture_note)) tags$p(calc$moisture_note)
    )
  })

  output[[ns("chart_money")]] <- plotly::renderPlotly({
    calc <- calc_now()
    req(!is.null(calc))
    plotly::plot_ly(
      x = calc$labels, y = calc$values, type = "bar",
      marker = list(color = c("#8b1e1e", "#c45c26", "#1f5fd1")),
      text = paste0(vapply(calc$values, ejo_fmt_rwf, character(1)), " RWF"),
      hoverinfo = "text"
    ) |>
      plotly::layout(
        title = list(text = "Financial loss by component (RWF)", font = list(size = 14)),
        xaxis = list(title = ""), yaxis = list(title = "RWF"),
        margin = list(t = 40, b = 60), paper_bgcolor = "#fff", plot_bgcolor = "#fff"
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  output[[ns("chart_share")]] <- plotly::renderPlotly({
    calc <- calc_now()
    req(!is.null(calc), sum(calc$values) > 0)
    plotly::plot_ly(
      labels = calc$labels, values = calc$values, type = "pie",
      textinfo = "label+percent",
      marker = list(colors = c("#8b1e1e", "#c45c26", "#1f5fd1"))
    ) |>
      plotly::layout(
        title = list(text = "Share of the total financial loss", font = list(size = 14)),
        margin = list(t = 40), showlegend = TRUE
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  output[[ns("qty_table")]] <- renderUI({
    calc <- calc_now()
    req(!is.null(calc))
    spoiled_show <- calc$spoiled_kg + calc$wait_spoiled_kg
    tags$div(
      class = "ej-inspect-qty",
      tags$h5("Physical quantity affected"),
      tags$table(
        class = "ej-inspect-qty-table",
        tags$thead(tags$tr(tags$th("Measure"), tags$th("Amount"), tags$th("Interpretation"))),
        tags$tbody(
          tags$tr(
            tags$td(if (identical(calc$cause, "glut")) "Sold below reference / spoiled waiting" else "Completely spoiled"),
            tags$td(paste0(ejo_fmt_rwf(spoiled_show), " kg")),
            tags$td(if (calc$total_kg > 0) paste0(round(100 * spoiled_show / calc$total_kg, 1), "% of harvest") else "—")
          ),
          tags$tr(
            tags$td("Downgraded"),
            tags$td(paste0(ejo_fmt_rwf(calc$down_kg), " kg")),
            tags$td(if (calc$total_kg > 0) paste0(round(100 * calc$down_kg / calc$total_kg, 1), "% of harvest") else "—")
          ),
          tags$tr(
            tags$td("Total affected"),
            tags$td(paste0(ejo_fmt_rwf(calc$affected_kg), " kg")),
            tags$td(if (calc$total_kg > 0) paste0(round(100 * calc$affected_kg / calc$total_kg, 1), "% · categories not double-counted") else "—")
          ),
          tags$tr(
            tags$td("Not reported as affected"),
            tags$td(paste0(ejo_fmt_rwf(calc$safe_kg), " kg")),
            tags$td("Remaining quantity at this stage")
          )
        )
      )
    )
  })

  if (identical(panel_key, "weather") || identical(cause_fixed, "weather")) {
    forecast_tick <- reactiveVal(0L)
    observeEvent(input[[ns("forecast")]], {
      forecast_tick(isolate(forecast_tick()) + 1L)
    }, ignoreInit = TRUE)
    output[[ns("forecast_out")]] <- renderUI({
      req(forecast_tick() > 0)
      fc <- ejo_inspect_forecast(input[[ns("district")]])
      if (!isTRUE(fc$ok)) return(tags$p(class = "note", fc$error %||% "No forecast."))
      wet_days <- fc$days[fc$wet]
      tags$div(
        class = "ej-inspect-forecast",
        tags$p(class = "ej-inspect-loss-kicker", "OPEN-METEO · 16 DAYS"),
        tags$p(paste0("Best harvest day in this window: ", fc$best, ".")),
        if (length(wet_days)) tags$p(paste0("Do not harvest on: ", paste(wet_days, collapse = ", "), " (rain ≥ 5 mm)."))
        else tags$p("No day in this window crosses 5 mm in the Open-Meteo strip."),
        tags$p(class = "note", fc$source),
        tags$div(
          class = "ej-inspect-rain-strip",
          lapply(seq_along(fc$days), function(i) {
            tags$span(class = if (fc$wet[i]) "is-wet" else "is-dry", title = paste(fc$days[i], fc$rain[i], "mm"), substr(fc$days[i], 9, 10))
          })
        )
      )
    })
  }

}

ejo_inspect_server <- function(input, output, session) {
  ejo_inspect_bind_one(input, output, session, "over", cause_fixed = NULL)
  lapply(names(ejo_inspect_causes), function(cause) {
    local({
      k <- cause
      ejo_inspect_bind_one(input, output, session, k, cause_fixed = k)
    })
  })
}
