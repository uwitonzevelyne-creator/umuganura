# Umuganura — Amakuru (Kaliza, the guide)

kaliza_audio_dir <- file.path(tempdir(), "kaliza-audio")
dir.create(kaliza_audio_dir, showWarnings = FALSE, recursive = TRUE)
shiny::addResourcePath("kaliza-audio", kaliza_audio_dir)

kaliza_env <- function(k) {
  v <- Sys.getenv(k)
  if (nzchar(v)) return(v)
  key_file <- "openai.key"
  if (k == "OPENAI_API_KEY" && file.exists(key_file)) {
    line <- trimws(readLines(key_file, n = 1, warn = FALSE))
    if (nzchar(line)) return(line)
  }
  NULL
}

kaliza_script <- "Hello! My name is Kaliza, I will guide you. Whatever you do, every profession helps our community grow. So tell me, what's your profession?"

kaliza_is_ag <- function(m) {
  grepl("farm|crop|maize|bean|rice|banana|potato|cassava|sorghum|wheat|coffee|tea|yield|harvest|irrig|soil|fertil|erosion|manure|market|price|stock|loss|rain|drought|export|gdp|agric|umuganura|district|season|store|cold|dry|food|livestock|milk|cattle|goat|reserve|silo|aflatoxin|problem|challenge|rwanda", m)
}

kaliza_district_chart <- function(chart_type = "bar") {
  prod <- tryCatch(home_district_production(), error = function(e) NULL)
  empty <- list(labels = character(), values = numeric(), title = "District harvest", unit = "MT", type = chart_type)
  if (is.null(prod) || !nrow(prod)) return(empty)
  if (identical(chart_type, "pie")) {
    top <- head(prod, 6)
    other <- sum(prod$figure) - sum(top$figure)
    if (is.finite(other) && other > 0) {
      top <- rbind(top, data.frame(district = "Other districts", figure = other, stringsAsFactors = FALSE))
    }
    prod <- top
  }
  list(
    labels = prod$district,
    values = round(prod$figure),
    title = "District harvest, 2019\u20132026",
    unit = "MT",
    type = chart_type
  )
}

kaliza_menu_farmer <- unname(amakuru_topics)

kaliza_tabs <- c(
  home = "home", about = "about", umusaruro = "umusaruro", harvest = "umusaruro",
  isoko = "factors", market = "factors", prices = "factors", factors = "factors",
  ikigega = "ikigega", stock = "ikigega", reserve = "ikigega", storage = "ikigega",
  amakuru = "amakuru", "ejo hazaza" = "ejo-hazaza", future = "ejo-hazaza"
)

kaliza_stats <- function(v, years) {
  ok <- is.finite(v)
  v <- v[ok]
  years <- years[ok]
  if (length(v) < 3) return(NULL)
  slope <- unname(stats::coef(stats::lm(v ~ years))[2])
  list(
    mean = mean(v), median = stats::median(v), sd = stats::sd(v), cv = 100 * stats::sd(v) / mean(v),
    min = min(v), min_year = years[which.min(v)], max = max(v), max_year = years[which.max(v)],
    slope = slope, latest = v[length(v)], latest_year = years[length(years)], n = length(v),
    from = min(years), to = max(years)
  )
}

kaliza_payload <- function(text, expr = "guiding", gesture = "neutral_gesturing", labels = character(), values = numeric(),
                           title = "", unit = "", menu = NULL, navigate = NULL, chart_type = "bar") {
  list(
    audio_stream_url = kaliza_tts(text),
    avatar_expression_trigger = expr,
    avatar_gesture_command = gesture,
    subtitle_text = text,
    chart_data = list(
      labels = as.list(unname(labels)),
      values = as.list(unname(round(as.numeric(values), 2))),
      title = title,
      unit = unit,
      type = chart_type
    ),
    menu = if (is.null(menu)) list() else as.list(menu),
    navigate = navigate
  )
}

# ---- voice: Azure (en-KE female) > ElevenLabs > browser ----
kaliza_tts <- function(text) {
  if (!requireNamespace("httr2", quietly = TRUE)) return("")
  key <- digest::digest(text, algo = "md5")
  out <- file.path(kaliza_audio_dir, paste0(key, ".mp3"))
  if (file.exists(out)) return(paste0("kaliza-audio/", key, ".mp3"))
  az_key <- kaliza_env("AZURE_SPEECH_KEY")
  az_reg <- kaliza_env("AZURE_SPEECH_REGION")
  el_key <- kaliza_env("ELEVENLABS_API_KEY")
  ok <- tryCatch({
    if (!is.null(az_key) && !is.null(az_reg)) {
      voice <- if (is.null(kaliza_env("AZURE_SPEECH_VOICE"))) "en-KE-AsiliaNeural" else kaliza_env("AZURE_SPEECH_VOICE")
      ssml <- sprintf("<speak version='1.0' xml:lang='en-KE'><voice name='%s'><prosody rate='-4%%'>%s</prosody></voice></speak>",
                      voice, htmltools::htmlEscape(text))
      resp <- httr2::request(sprintf("https://%s.tts.speech.microsoft.com/cognitiveservices/v1", az_reg)) |>
        httr2::req_headers(`Ocp-Apim-Subscription-Key` = az_key, `Content-Type` = "application/ssml+xml",
                           `X-Microsoft-OutputFormat` = "audio-24khz-48kbitrate-mono-mp3", `User-Agent` = "umuganura") |>
        httr2::req_body_raw(ssml) |> httr2::req_timeout(20) |> httr2::req_perform()
      writeBin(httr2::resp_body_raw(resp), out)
      TRUE
    } else if (!is.null(el_key)) {
      voice <- if (is.null(kaliza_env("ELEVENLABS_VOICE_ID"))) "EXAVITQu4vr4xnSDxMaL" else kaliza_env("ELEVENLABS_VOICE_ID")
      resp <- httr2::request(paste0("https://api.elevenlabs.io/v1/text-to-speech/", voice, "?output_format=mp3_44100_128")) |>
        httr2::req_headers(`xi-api-key` = el_key) |>
        httr2::req_body_json(list(text = text, model_id = "eleven_multilingual_v2", voice_settings = list(stability = 0.5, similarity_boost = 0.8, style = 0.3))) |>
        httr2::req_timeout(25) |> httr2::req_perform()
      writeBin(httr2::resp_body_raw(resp), out)
      TRUE
    } else FALSE
  }, error = function(e) FALSE)
  if (isTRUE(ok) && file.exists(out)) paste0("kaliza-audio/", key, ".mp3") else ""
}

# ---- optional LLM for free questions ----
kaliza_llm <- function(msg, context) {
  key <- kaliza_env("OPENAI_API_KEY")
  if (is.null(key) || !requireNamespace("httr2", quietly = TRUE)) return(NULL)
  url <- if (is.null(kaliza_env("KALIZA_LLM_URL"))) "https://api.openai.com/v1/chat/completions" else kaliza_env("KALIZA_LLM_URL")
  model <- if (is.null(kaliza_env("KALIZA_MODEL"))) "gpt-4o-mini" else kaliza_env("KALIZA_MODEL")
  sys <- paste(
    "You are Kaliza, the agricultural presenter inside Umuganura, a Rwanda harvest platform.",
    "Answer the way a strong general assistant would: clear, specific, and useful, in spoken English of about 80 to 140 words.",
    "Stay on agriculture in Rwanda: crops, livestock, districts, seasons, irrigation, soil, fertilizer, markets, stocks, exports and post-harvest loss.",
    "If the question is not about agriculture, say you only guide Rwandan agriculture and offer one farm problem with its answer.",
    "When the facts below contain a number, use that number. Do not invent a district total, a percentage, or a year.",
    "Name the stronger district and the weaker district when the facts include them, then say what a farmer or an officer can do.",
    "The screen already draws the district chart, so do not describe a chart you cannot see. Just explain the numbers.",
    "Facts:", context
  )
  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_auth_bearer_token(key) |>
      httr2::req_body_json(list(model = model, temperature = 0.4, max_tokens = 420,
                                messages = list(list(role = "system", content = sys), list(role = "user", content = msg)))) |>
      httr2::req_timeout(25) |> httr2::req_perform()
    out <- httr2::resp_body_json(resp)
    trimws(out$choices[[1]]$message$content)
  }, error = function(e) NULL)
}

# ---- data lookups ----
kaliza_find_crop <- function(msg) {
  syn <- c(potato = "irish_potato", potatoes = "irish_potato", ibirayi = "irish_potato", bean = "beans", banana = "cooking_banana",
           bananas = "cooking_banana", millet = "finger_millet", soya = "soya_bean", soybean = "soya_bean", groundnuts = "groundnut",
           peanut = "groundnut", sugar = "sugarcane", cattle = "beef", cow = "milk", goat = "goat_meat", chicken = "chicken_meat",
           pig = "pork", egg = "eggs", pepper = "sweet_pepper", tomatoes = "tomato", onions = "onion", carrots = "carrot")
  words <- strsplit(gsub("[^a-z' ]", " ", tolower(msg)), "\\s+")[[1]]
  for (i in seq_len(nrow(crop_catalog))) {
    keys <- tolower(c(crop_catalog$en[i], crop_catalog$rw[i]))
    keys <- keys[nzchar(keys)]
    if (any(vapply(keys, function(k) grepl(paste0("\\b", k, "\\b"), tolower(msg)), logical(1)))) return(crop_catalog$id[i])
  }
  hit <- intersect(words, names(syn))
  if (length(hit)) return(unname(syn[hit[1]]))
  NULL
}

kaliza_find_district <- function(msg) {
  m <- tolower(msg)
  d <- names(province_of)[vapply(tolower(names(province_of)), function(k) grepl(paste0("\\b", k, "\\b"), m), logical(1))]
  if (length(d)) d[1] else NULL
}

kaliza_topic_of <- function(msg) {
  m <- tolower(trimws(msg))
  if (grepl("^[1-5]$", m)) return(names(amakuru_topics)[as.integer(m)])
  rules <- c(
    irrigation = "irrigat|water|kuhira|drip|sprinkler|pump",
    soil = "soil|erosion|fertili|compost|manure|ubutaka|terrace|lime|agroforest",
    postharvest = "post-harvest|post harvest|postharvest|storage|store|drying|silo|warehouse|aflatoxin|loss",
    value = "productiv|process|value add|value-add|yield|agro-process|flour|milling",
    export = "export|abroad|foreign market|international"
  )
  for (k in names(rules)) if (grepl(rules[[k]], m)) return(k)
  NULL
}

kaliza_profession <- function(msg) {
  m <- tolower(msg)
  if (grepl("farm|umuhinzi|abahinzi|grower|cultivat|herder|livestock keeper|umworozi|aborozi|agronom|cooperative member", m)) return("farmer")
  if (grepl("government|ministry|minagri|policy|official|leta|mayor|district officer|civil servant|rab|naeb", m)) return("government")
  if (grepl("ngo|civil society|association|advocacy|cso", m)) return("civil")
  if (grepl("donor|development partner|partner|fao|wfp|world bank|usaid|ifad|investor|researcher|funder|bank", m)) return("partner")
  if (grepl("citizen|student|resident|umuturage|consumer|journalist|teacher|trader|business|other", m)) return("citizen")
  NULL
}

kaliza_topic <- function(k) {
  lp <- farm_practice
  pct <- function(df, col) nat_or_mean(df, col)
  if (k == "irrigation") {
    land <- lp$land
    irr <- pct(land, "irrig")
    d <- land[land$district != "Rwanda" & is.finite(land$irrig), , drop = FALSE]
    hi <- d[which.max(d$irrig), ]
    lo <- d[which.min(d$irrig), ]
    tech <- lp$irr_tech
    tv <- vapply(c("traditional", "surface", "flood", "drip", "sprinkler", "pivot"), function(c) pct(tech, c), numeric(1))
    txt <- paste0("In Season A 2026, ", fmt_num(irr, 0), "% of Rwandan farmers irrigated. ", hi$district, " leads with ", fmt_num(hi$irrig, 0),
                  "%, while ", lo$district, " is lowest at ", fmt_num(lo$irrig, 0), "%. Among irrigators, ", fmt_num(tv[["traditional"]], 0),
                  "% still use traditional methods; drip and sprinkler are rare. Small pumps and drip kits are the fastest gain.")
    return(kaliza_payload(txt, if (irr < 25) "concerned" else "excited", "point_right_hand",
                          c("Traditional", "Surface", "Flood", "Drip", "Sprinkler", "Pivot"), tv, "How irrigating farmers water their crops (%)", "%",
                          menu = c("Irrigation in Bugesera", "Soil conservation and fertility management", "Open Umusaruro")))
  }
  if (k == "soil") {
    v <- c(pct(lp$land, "erosion"), pct(lp$organic, "farmers"), pct(lp$inorganic, "farmers"), pct(lp$land, "agrofor"),
           nat_or_mean(transform(lp$erosion, bad = severe + moderate), "bad"))
    fu <- nutrient_use()
    txt <- paste0(fmt_num(v[1], 0), "% of farmers protect their land against erosion and ", fmt_num(v[2], 0), "% add manure, but only ",
                  fmt_num(v[3], 0), "% use inorganic fertilizer. Rwanda applies about ", fmt_num(fu$value, 0), " kg of fertilizer per hectare (",
                  fu$year, "), and ", fmt_num(v[5], 0), "% of plots still show severe or moderate erosion. Terraces, lime and balanced fertilizer protect the soil.")
    return(kaliza_payload(txt, if (v[3] < 50) "concerned" else "guiding", "point_right_hand",
                          c("Protect land from erosion", "Use manure", "Use inorganic fertilizer", "Practise agroforestry", "Plots eroding"), v,
                          "Soil care on Rwandan farms, Season A 2026 (%)", "%",
                          menu = c("Which districts erode most?", "Post-harvest collection and storage", "Open Umusaruro")))
  }
  if (k == "postharvest") {
    u <- sas_use[sas_use$year == max(sas_use$year) & sas_use$season == "A", , drop = FALSE]
    u <- u[is.finite(u$lost) & u$crop %in% c("Maize", "Sorghum", "Paddy rice", "Wheat", "Bush bean", "Climbing bean", "Soybean", "Groundnuts", "Irish potato", "Cassava"), , drop = FALSE]
    u <- u[order(-u$lost), , drop = FALSE]
    st <- stock_mass[stock_mass$crop %in% nsr_crops, , drop = FALSE]
    last <- stats::aggregate(stored_mt ~ year + season, st, sum)
    last <- last[order(last$year, last$season), , drop = FALSE]
    l <- last[nrow(last), ]
    txt <- paste0("After Season A ", max(sas_use$year), ", farmers lost the most ", tolower(u$crop[1]), " (", fmt_num(u$lost[1], 2),
                  "% of the harvest). Only about ", fmt_num(l$stored_mt, 0), " tonnes of grain and pulses went into store after Season ", l$season, " ", l$year,
                  ", far under the 46,000-tonne reserve floor. Drying on tarpaulins and hermetic bags cut losses.")
    return(kaliza_payload(txt, "concerned", "point_right_hand", u$crop, u$lost, paste0("Post-harvest loss, Season A ", max(sas_use$year), " (% of harvest)"), "%",
                          menu = c("Open Ikigega", "Agricultural productivity, processing and value addition", "Maize")))
  }
  if (k == "value") {
    ids <- c("maize", "rice", "beans", "cassava", "irish_potato", "cooking_banana")
    ch <- vapply(ids, function(id) {
      y <- crop_yield_nat(id)
      a <- y$value[y$year == 1995]
      b <- y$value[y$year == max(y$year)]
      if (length(a) && length(b) && a > 0) 100 * (b / a - 1) else NA_real_
    }, numeric(1))
    lab <- crop_catalog$en[match(ids, crop_catalog$id)]
    best <- lab[which.max(ch)]
    txt <- paste0("Over thirty years, yields moved a lot: ", tolower(best), " rose ", fmt_num(max(ch, na.rm = TRUE), 0), "% since 1995. ",
                  "Agriculture adds about ", home_stats$usd_now, " a year to the economy, but it is now ", home_stats$gdp_now,
                  " of GDP. Processing maize into flour, milk into yoghurt and cassava into flour keeps more money in the village.")
    return(kaliza_payload(txt, "excited", "point_right_hand", lab, ch, "Yield change since 1995 (%)", "%",
                          menu = c("Export market opportunities for Rwandan agricultural products", "Maize", "Open Isoko")))
  }
  if (k == "export") {
    ids <- c(naeb_coffee = "Coffee", naeb_tea = "Tea", naeb_hort = "Horticulture", naeb_all = "All agri exports")
    v <- vapply(names(ids), function(i) {
      d <- factor_data[factor_data$id == i & is.finite(factor_data$value), , drop = FALSE]
      if (nrow(d)) d$value[which.max(d$year)] / 1e6 else NA_real_
    }, numeric(1))
    yr <- max(factor_data$year[factor_data$id == "naeb_all"], na.rm = TRUE)
    txt <- paste0("In the year ending ", yr, ", Rwanda earned about $", fmt_num(v[["naeb_all"]], 0), " million from agricultural exports. Coffee brought $",
                  fmt_num(v[["naeb_coffee"]], 0), " million, tea $", fmt_num(v[["naeb_tea"]], 0), " million and horticulture $", fmt_num(v[["naeb_hort"]], 0),
                  " million. Avocado, chilli and passion fruit are the fast-growing openings, if the cold chain holds.")
    return(kaliza_payload(txt, "excited", "point_right_hand", unname(ids), v, "Agricultural export earnings (US$ million, NAEB)", "US$ m",
                          menu = c("Open Ikigega", "Small-scale irrigation", "Coffee")))
  }
  NULL
}

kaliza_crop <- function(id, district = NULL, nav = FALSE) {
  r <- crop_row(id)
  y <- crop_yield_nat(id)
  y <- y[y$year >= max(y$year) - 29, , drop = FALSE]
  unit <- crop_unit(id)
  s <- kaliza_stats(y$value, y$year)
  navigate <- if (nav) list(panel = "umusaruro", crop = id) else NULL
  if (is.null(s)) return(kaliza_payload(paste0("I do not have a national yield series for ", tolower(r$en), ". I can open its page in Umusaruro."), "concerned", "neutral_gesturing",
                                        navigate = list(panel = "umusaruro", crop = id)))
  if (!is.null(district) && !is.na(r$sas)) {
    d <- sas_yield[sas_yield$crop == r$sas & sas_yield$district == district, , drop = FALSE]
    n <- sas_yield[sas_yield$crop == r$sas & sas_yield$district == "Rwanda", , drop = FALSE]
    if (nrow(d) >= 3) {
      ds <- kaliza_stats(d$value, d$year + ifelse(d$season == "B", 0.5, 0))
      gap <- 100 * (ds$mean / mean(n$value) - 1)
      txt <- paste0("In ", district, ", ", tolower(r$en), " averaged ", fmt_num(ds$mean, 0), " kg per hectare across ", ds$n, " seasons since ", floor(ds$from),
                    ", ", fmt_num(abs(gap), 0), "% ", if (gap >= 0) "above" else "below", " the national average. It swung between ", fmt_num(ds$min, 0), " and ",
                    fmt_num(ds$max, 0), " kg, a standard deviation of ", fmt_num(ds$sd, 0), ".")
      return(kaliza_payload(txt, if (gap >= 0) "excited" else "concerned", "point_right_hand",
                            c("Mean", "Median", "Std. deviation", "Minimum", "Maximum", "National mean"),
                            c(ds$mean, ds$median, ds$sd, ds$min, ds$max, mean(n$value)),
                            paste0(r$en, " in ", district, " (kg/ha, NISR SAS)"), "kg/ha", menu = c(paste("Open", r$en), "Soil conservation and fertility management"), navigate = navigate))
    }
  }
  trend <- if (s$slope >= 0) paste0("rising about ", fmt_num(s$slope, 0), " ", unit, " a year") else paste0("falling about ", fmt_num(abs(s$slope), 0), " ", unit, " a year")
  txt <- paste0("Over the last ", s$n, " years (", s$from, " to ", s$to, "), ", tolower(r$en), " yield averaged ", fmt_num(s$mean, 0), " ", unit, ", ", trend,
                ". The best year was ", s$max_year, " at ", fmt_num(s$max, 0), " and the worst ", s$min_year, " at ", fmt_num(s$min, 0),
                ". Year-to-year spread is ", fmt_num(s$cv, 0), "% of the mean.")
  kaliza_payload(txt, if (s$slope >= 0) "excited" else "concerned", "point_right_hand",
                 c("Mean", "Median", "Std. deviation", "Minimum", "Maximum", "Latest"),
                 c(s$mean, s$median, s$sd, s$min, s$max, s$latest),
                 paste0(r$en, " yield, 30-year statistics (", unit, ", FAOSTAT)"), unit,
                 menu = c(paste("Open", r$en), paste(r$en, "in Musanze"), "Small-scale irrigation"), navigate = navigate)
}

kaliza_district <- function(d) {
  lp <- farm_practice
  g <- function(df, col) { if (is.null(df)) return(NA_real_); v <- df[df$district == d, col]; if (length(v)) v[1] else NA_real_ }
  v <- c(g(lp$land, "irrig"), g(lp$land, "erosion"), g(lp$inorganic, "farmers"), g(lp$organic, "farmers"),
         g(lp$erosion, "severe") + g(lp$erosion, "moderate"), mean(c(g(coop_reach, "seed"), g(coop_reach, "fert")), na.rm = TRUE))
  sh <- sas_prod[sas_prod$district == d & sas_prod$year == 2026 & sas_prod$season == "A" & sas_prod$crop %in% c("Maize", "Bean", "Irish potato", "Cassava", "Sweet potato", "Cooking banana", "Paddy rice", "Sorghum"), , drop = FALSE]
  top <- if (nrow(sh)) sh$crop[which.max(sh$value)] else NULL
  txt <- paste0(d, " in ", province_of[[d]], " Province: ", fmt_num(v[1], 0), "% of farmers irrigate, ", fmt_num(v[2], 0), "% protect land against erosion and ",
                fmt_num(v[3], 0), "% use inorganic fertilizer. ", fmt_num(v[5], 0), "% of plots show severe or moderate erosion.",
                if (!is.null(top)) paste0(" Its biggest Season A 2026 harvest by weight is ", tolower(top), ".") else "")
  kaliza_payload(txt, "guiding", "point_right_hand",
                 c("Irrigate", "Protect land", "Inorganic fertilizer", "Manure", "Plots eroding", "Cooperative reach"), v,
                 paste0(d, ": farm practice, Season A 2026 (%)"), "%", menu = c(paste("Maize in", d), "Small-scale irrigation", "Open Umusaruro"))
}

kaliza_context <- function() {
  lp <- farm_practice
  paste0(
    "Farmers irrigating: ", fmt_num(nat_or_mean(lp$land, "irrig"), 0), "%. Protecting land from erosion: ", fmt_num(nat_or_mean(lp$land, "erosion"), 0),
    "%. Using inorganic fertilizer: ", fmt_num(nat_or_mean(lp$inorganic, "farmers"), 0), "%. Fertilizer per hectare: ", fmt_num(nutrient_use()$value, 0),
    " kg. Agriculture share of GDP: ", home_stats$gdp_now, ". Jobs in agriculture: ", home_stats$jobs, ". Strategic reserve floor 46,000 tonnes."
  )
}

kaliza_district_facts <- function() {
  prod <- tryCatch(home_district_production(), error = function(e) NULL)
  if (is.null(prod) || !nrow(prod)) return("District production table is not loaded.")
  paste0(
    "District crop production, metric tonnes, 2019 to 2025 seasons A and B plus 2026 season A, strongest first: ",
    paste(paste0(prod$district, " ", round(prod$figure)), collapse = "; "),
    ". National sum ", round(sum(prod$figure)),
    ". A continuous 1996 to 2018 district total is not in these tables. The current 30 districts date from 2006."
  )
}

kaliza_chart_kind <- function(m) if (grepl("\\bpie\\b|share of|portion|slice", m)) "pie" else "bar"

kaliza_district_say <- function() {
  prod <- tryCatch(home_district_production(), error = function(e) NULL)
  if (is.null(prod) || nrow(prod) < 2) return("The district harvest table is not loaded.")
  hi <- prod[1, ]
  lo <- prod[nrow(prod), ]
  paste0(
    "Across the 30 districts, from 2019 to 2026, ", hi$district, " has the greater harvest at ", fmt_num(hi$figure, 0),
    " metric tonnes. ", lo$district, " has the weaker harvest at ", fmt_num(lo$figure, 0),
    " metric tonnes. The national sum is ", fmt_num(sum(prod$figure), 0),
    " metric tonnes. The chart on the right compares the districts. A continuous total from 1996 is not in these tables, because the current districts date from 2006."
  )
}

kaliza_now <- function(id) {
  if (!exists("factor_data")) return(NULL)
  d <- factor_data[factor_data$id == id & is.finite(factor_data$value), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d[which.max(d$year), ]
}

kaliza_isoko <- function(m, kind) {
  series <- NULL
  if (grepl("inflation|\\bcpi\\b|price rise", m)) series <- list(id = "inflation", unit = "%", title = "Inflation", say = "inflation")
  else if (grepl("interest|lending|borrow", m)) series <- list(id = "lending", unit = "%", title = "Lending rate", say = "the lending rate")
  else if (grepl("unemploy|jobless", m)) series <- list(id = "unemployment", unit = "%", title = "Unemployment, ILO model", say = "unemployment in the ILO model, not the NISR labour-force survey")
  else if (grepl("exchange|dollar|franc|\\brwf\\b|\\bfx\\b", m)) series <- list(id = "fx", unit = "RWF per US$", title = "Francs per US dollar", say = "the exchange rate")
  else if (grepl("\\bgdp\\b|economy share", m)) series <- list(id = "gdp_share", unit = "%", title = "Agriculture share of GDP", say = "agriculture's share of GDP")
  else if (grepl("export|\\bcoffee\\b|\\btea\\b|horticult", m)) return(kaliza_topic("export"))
  else if (grepl("price|\\bmarket\\b", m)) {
    crop <- kaliza_find_crop(m)
    idmap <- c(maize = "maize_price", sorghum = "sorghum_price", beans = "beans_price", cassava = "cassava_price", irish_potato = "potato_price", rice = "rice_price")
    pid <- if (!is.null(crop) && crop %in% names(idmap)) idmap[[crop]] else NULL
    if (is.null(pid)) {
      return(kaliza_payload("Isoko does not have a producer price for that product, so I will not guess it. I can show maize, sorghum, beans, cassava, potato or rice.", "concerned", "neutral_gesturing",
                            menu = c("Maize price", "Beans price", "Open Isoko")))
    }
    series <- list(id = pid, unit = "RWF/t", title = paste(crop_row(crop)$en, "producer price"), say = paste("the", tolower(crop_row(crop)$en), "producer price"))
  }
  if (is.null(series)) return(NULL)
  row <- kaliza_now(series$id)
  if (is.null(row)) {
    return(kaliza_payload(paste0("Isoko does not have ", series$say, " loaded, so I cannot give that number."), "concerned", "neutral_gesturing", menu = c("Open Isoko")))
  }
  d <- factor_data[factor_data$id == series$id & is.finite(factor_data$value), , drop = FALSE]
  d <- tail(d[order(d$year), , drop = FALSE], 8)
  extra <- if (series$id == "inflation") " When food prices rise, sell less at the peak and keep grain that Ikigega can store." else if (series$id == "fx") " A weaker franc raises export earnings in francs and the cost of imported fertilizer." else if (series$id == "unemployment") " This is the ILO modelled rate." else ""
  txt <- paste0("From Isoko, ", series$say, " was ", fmt_num(row$value, if (series$unit == "RWF per US$" || series$unit == "RWF/t") 0 else 1), " ", series$unit, " in ", row$year, ".", extra, " The chart is that series from this platform.")
  kaliza_payload(txt, "guiding", "point_right_hand", d$year, d$value, paste(series$title, "(", series$unit, ")"), series$unit,
                 menu = c("Open Isoko", "Open Ikigega"), chart_type = kind)
}

kaliza_ikigega <- function(m) {
  if (!exists("stock_mass") || is.null(stock_mass) || !nrow(stock_mass)) {
    return(kaliza_payload("Ikigega has no stock table loaded, so I cannot give that figure.", "concerned", "neutral_gesturing"))
  }
  st <- stock_mass[stock_mass$crop %in% nsr_crops & is.finite(stock_mass$stored_mt), , drop = FALSE]
  if (!nrow(st)) return(kaliza_payload("Ikigega does not have a stored-grain figure, so I will not guess one.", "concerned", "neutral_gesturing"))
  st <- st[order(st$year, st$season), , drop = FALSE]
  key <- paste(st$year, st$season)
  snap <- st[key == key[length(key)], , drop = FALSE]
  tot <- sum(snap$stored_mt)
  gap <- nsr_floor - tot
  txt <- paste0(
    "From Ikigega, after Season ", snap$season[1], " ", snap$year[1], ", grain and pulses in store were ", fmt_num(tot, 0),
    " tonnes. The reserve floor is ", fmt_num(nsr_floor, 0), " tonnes, so the stock is ",
    if (gap > 0) paste0(fmt_num(gap, 0), " tonnes under the floor. Dry the grain before the sack is closed, then fill the reserve before the next lean season.")
    else "above the floor. Keep drying and hermetic bags so it stays there."
  )
  if (grepl("cold|temperature|fridge", m)) {
    txt <- paste0(txt, " A cold-room temperature log is not published in Ikigega, so I cannot give a temperature.")
  }
  kaliza_payload(txt, if (gap > 0) "concerned" else "guiding", "point_right_hand",
                 snap$crop, snap$stored_mt, paste0("Stored grain, Season ", snap$season[1], " ", snap$year[1], " (tonnes)"), "t",
                 menu = c("Open Ikigega", "Open Ejo hazaza"))
}

kaliza_ejo <- function(m, kind) {
  if (!exists("ejo_cy") || is.null(ejo_cy) || !nrow(ejo_cy)) {
    return(kaliza_payload("Ejo Hazaza has no loss table loaded, so I cannot give that figure.", "concerned", "neutral_gesturing"))
  }
  keys <- c(transport = "transport|\\broad\\b|truck", storage = "dry|storage", weather = "rain|weather",
            cold = "cool|cold|fridge", glut = "glut|too much|surplus|oversupply", handling = "handling|packag")
  hit <- NULL
  for (k in names(keys)) if (grepl(keys[[k]], m)) hit <- k
  y <- 2026
  all <- ejo_cy[ejo_cy$year == y, , drop = FALSE]
  if (!nrow(all)) return(kaliza_payload("Ejo Hazaza does not have a 2026 loss total, so I will not guess it.", "concerned", "neutral_gesturing"))
  if (!is.null(hit)) {
    a <- all[all$cause == hit, , drop = FALSE]
    if (!nrow(a)) return(kaliza_payload("Ejo Hazaza does not have that cause, so I will not guess it.", "concerned", "neutral_gesturing"))
    pl <- if (exists("ejo_plans")) ejo_plans[ejo_plans$cause == hit, , drop = FALSE] else data.frame()
    act <- if (nrow(pl) && "activity" %in% names(pl)) pl$activity[1] else NULL
    txt <- paste0(
      "From Ejo Hazaza, ", ejo_causes[[hit]], " is ", fmt_bn(a$total[1]), " of the ", y, " loss, about ", fmt_num(a$tonnes[1], 0), " tonnes. ",
      "The solution on this platform is ", ejo_or[[hit]], ".",
      if (!is.null(act)) paste0(" The first activity in the plan is: ", act, ".") else ""
    )
    return(kaliza_payload(txt, "concerned", "point_right_hand", unname(ejo_causes[all$cause]), all$total / 1e9,
                          paste("Loss by cause,", y, "(billion RWF)"), "bn RWF",
                          menu = c("Open Ejo hazaza", "Open Ikigega"), chart_type = kind))
  }
  txt <- paste0(
    "From Ejo Hazaza, the ", y, " post-harvest loss is ", fmt_bn(sum(all$total)), ", about ", fmt_num(sum(all$tonnes), 0),
    " tonnes. The chart splits it across transport, drying and storage, rain, cooling, too much supply, and handling. Ask for one cause and I will give its solution from the plan."
  )
  kaliza_payload(txt, "concerned", "point_right_hand", unname(ejo_causes[all$cause]), all$total / 1e9,
                 paste("Loss by cause,", y, "(billion RWF)"), "bn RWF",
                 menu = c("Drying and storage", "Rain at harvest", "Open Ejo hazaza"), chart_type = kind)
}

kaliza_hort <- function(m, kind) {
  if (!exists("hort_district") || !nrow(hort_district)) {
    return(kaliza_payload("Ikigega does not have a district horticulture table, so I will not guess one.", "concerned", "neutral_gesturing"))
  }
  dist <- kaliza_find_district(m)
  y <- max(hort_district$year)
  sub <- hort_district[hort_district$year == y, , drop = FALSE]
  seasons <- paste(sort(unique(sub$season)), collapse = " and ")
  if (!is.null(dist)) {
    one <- sub[sub$district == dist, , drop = FALSE]
    if (!nrow(one)) {
      return(kaliza_payload(paste0(dist, " has no vegetables or fruits figure in the ", y, " SAS table, so I will not guess it."), "concerned", "neutral_gesturing"))
    }
    veg <- hort_sum(one$vegetables)
    fr <- hort_sum(one$fruits)
    txt <- paste0(
      "From Ikigega, NISR SAS puts ", dist, " vegetables at ", fmt_num(veg, 0), " tonnes and fruits at ", fmt_num(fr, 0),
      " tonnes in ", y, ", season ", seasons, ". That is the volume that has to be packed and moved. ",
      "NAEB does not publish horticulture exports by district, and a cold-room temperature is not published."
    )
    return(kaliza_payload(txt, "guiding", "point_right_hand", c("Vegetables", "Fruits"), c(veg, fr),
                          paste0(dist, ", ", y, " (tonnes)"), "t",
                          menu = c("Open Ikigega", "District harvest"), chart_type = kind))
  }
  both <- cbind(sub$vegetables, sub$fruits)
  sub$value <- ifelse(rowSums(is.finite(both)) == 0, NA_real_, rowSums(both, na.rm = TRUE))
  by <- stats::aggregate(value ~ district, sub, hort_sum)
  by <- by[is.finite(by$value), , drop = FALSE]
  by <- by[order(-by$value), , drop = FALSE]
  hi <- by[1, ]
  lo <- by[nrow(by), ]
  exp <- if (exists("hort_naeb") && nrow(hort_naeb)) hort_naeb[nrow(hort_naeb), ] else NULL
  wt <- if (exists("hort_naeb")) hort_naeb[is.finite(hort_naeb$export_t), , drop = FALSE] else hort_naeb
  naeb <- if (is.null(exp)) {
    "NAEB horticulture exports are not loaded."
  } else {
    paste0(
      "NAEB, national, reports $", fmt_num(exp$export_usd / 1e6, 1), " million of horticulture exports in ", exp$fiscal_year, ".",
      if (nrow(wt)) paste0(" The published export weight is ", fmt_num(wt$export_t[nrow(wt)], 0), " tonnes in ", wt$fiscal_year[nrow(wt)], ".") else "",
      " That weight is not split by district."
    )
  }
  txt <- paste0(
    "From Ikigega, in ", y, " season ", seasons, ", ", hi$district, " has the greater vegetables-and-fruits harvest at ",
    fmt_num(hi$value, 0), " tonnes. ", lo$district, " has the smaller one at ", fmt_num(lo$value, 0),
    " tonnes. The 30 districts together harvested ", fmt_num(sum(by$value), 0), " tonnes. ", naeb,
    " A cold-room temperature is not published, so I will not give one."
  )
  kaliza_payload(txt, "guiding", "point_right_hand", by$district, by$value,
                 paste0("Vegetables and fruits, ", y, " season ", seasons, " (tonnes)"), "t",
                 menu = c("Open Ikigega", "District harvest"), chart_type = kind)
}

kaliza_from_rooms <- function(m, kind) {
  if (grepl("ejo|action plan|money lost|bn rwf|transport route|rain at harvest|too much supply|handling and packaging|\\bsolution", m) ||
      (grepl("\\bloss\\b|\\blost\\b", m) && grepl("money|value|future|plan|solution|cause|forecast", m))) {
    return(kaliza_ejo(m, kind))
  }
  if (grepl("horticult|cold chain|cold-room|cold room|export storage|\\bcold\\b", m)) {
    return(kaliza_hort(m, kind))
  }
  if (grepl("ikigega|\\breserve\\b|strategic stock|\\bsilo\\b|46,?000", m) || (grepl("\\bstock\\b|stored", m) && !grepl("price", m))) {
    return(kaliza_ikigega(m))
  }
  if (grepl("isoko|inflation|interest|lending|unemploy|exchange|\\bdollar\\b|\\bfranc\\b|\\bgdp\\b|\\bexport\\b|producer price|market price|\\bprice\\b", m)) {
    return(kaliza_isoko(m, kind))
  }
  NULL
}

kaliza_reply <- function(msg, st) {
  m <- tolower(trimws(msg))
  nav_word <- grepl("\\b(open|go to|take me|show me the|navigate|visit|jya|fungura)\\b", m)

  if (nav_word) {
    hit <- names(kaliza_tabs)[vapply(names(kaliza_tabs), function(k) grepl(paste0("\\b", k, "\\b"), m), logical(1))]
    crop <- kaliza_find_crop(m)
    if (!is.null(crop)) {
      r <- crop_row(crop)
      return(list(st = st, p = kaliza_payload(paste0("Opening the ", tolower(r$en), " page in Umusaruro. Start with its profile, then step through the map, trends, risks and causes."),
                                              "guiding", "point_right_hand", navigate = list(panel = "umusaruro", crop = crop))))
    }
    if (length(hit)) {
      pn <- kaliza_tabs[[hit[1]]]
      say <- switch(pn,
        umusaruro = "Opening Umusaruro: every crop and animal product, with a page for each.",
        factors = "Opening Isoko, the market: prices and the macroeconomic forces behind them.",
        ikigega = "Opening Ikigega: the national reserve, cooperative stores and the cold chain.",
        home = "Taking you home.", about = "Opening About.", "ejo-hazaza" = "Opening Ejo hazaza.", amakuru = "We are already in Amakuru.")
      return(list(st = st, p = kaliza_payload(say, "guiding", "point_right_hand", navigate = list(panel = pn))))
    }
  }

  if (grepl("^(menu|help|options|what can you do|start over|restart)", m)) {
    dc <- kaliza_district_chart("bar")
    return(list(st = st, p = kaliza_payload("Here is what I can show you. Pick a topic, name a crop, or name a district. The chart is the district harvest.", "guiding", "neutral_gesturing",
                                            dc$labels, dc$values, dc$title, dc$unit, menu = kaliza_menu_farmer, chart_type = "bar")))
  }

  topic <- kaliza_topic_of(m)
  crop <- kaliza_find_crop(m)
  dist <- kaliza_find_district(m)
  kind <- kaliza_chart_kind(m)
  dc <- kaliza_district_chart(kind)
  prof_pick <- grepl("i am a farmer|i work in government|i am a citizen|civil society|development partner", m)

  if (!prof_pick) {
    from_room <- kaliza_from_rooms(m, kind)
    if (!is.null(from_room)) return(list(st = st, p = from_room))
  }

  if (is.null(st$profession) && is.null(topic) && is.null(crop) && is.null(dist) && !kaliza_is_ag(m)) {
    prof <- kaliza_profession(m)
    if (is.null(prof)) {
      return(list(st = st, p = kaliza_payload("Thank you. Tell me your profession so I can guide you: are you a farmer, in government, a citizen, in civil society, or a development partner?",
                                              "guiding", "neutral_gesturing", menu = c("I am a farmer", "I work in government", "I am a citizen", "Civil society", "Development partner"))))
    }
    st$profession <- prof
    lp <- farm_practice
    if (prof == "farmer") {
      mz <- sas_use[sas_use$crop == "Maize", , drop = FALSE]
      mz <- mz[order(mz$year, mz$season), , drop = FALSE][nrow(mz), ]
      v <- c(nat_or_mean(lp$land, "irrig"), nat_or_mean(lp$land, "erosion"), nat_or_mean(lp$inorganic, "farmers"), mz$stored, mz$sold)
      txt <- "Wonderful, farmers feed Rwanda and I am here to support you. Would you like to explore small-scale irrigation, soil conservation and fertility management, post-harvest collection and storage, agricultural productivity processing and value addition, or export market opportunities for Rwandan agricultural products?"
      return(list(st = st, p = kaliza_payload(txt, "excited", "point_right_hand",
                                              c("Farmers irrigating", "Protect land", "Use inorganic fertilizer", "Maize stored", "Maize sold"), v,
                                              "Where Rwandan farms stand today (%)", "%", menu = kaliza_menu_farmer)))
    }
    say <- switch(prof,
      government = "Welcome. For planning, I can show where yields are thin, which districts erode, and how close grain stocks sit to the 46,000-tonne floor. Pick a topic below.",
      civil = "Welcome. I can show which districts are left behind on irrigation, soil care and storage, so your programmes reach the right farmers. Pick a topic below.",
      partner = "Welcome. I can show thirty years of yields, the drivers that move them, and where investment returns most. Pick a topic below.",
      citizen = "Welcome. I can show what Rwanda grows, where, and what reaches your market and your plate. Pick a topic below.")
    return(list(st = st, p = kaliza_payload(say, "guiding", "point_right_hand",
                                            c("Agriculture in GDP", "Jobs in agriculture"), c(hv_num(home_stats$gdp_now), hv_num(home_stats$jobs)),
                                            "Agriculture in Rwanda's economy (%)", "%", menu = kaliza_menu_farmer)))
  }

  if (!is.null(topic) && is.null(crop) && !(topic == "value" && !is.null(dist))) {
    if (topic == "irrigation" && !is.null(dist)) return(list(st = st, p = kaliza_district(dist)))
    st$topic <- topic
    return(list(st = st, p = kaliza_topic(topic)))
  }
  if (!is.null(crop)) return(list(st = st, p = kaliza_crop(crop, dist, nav_word)))
  if (!is.null(dist)) return(list(st = st, p = kaliza_district(dist)))
  if (grepl("erode|erosion", m)) return(list(st = st, p = kaliza_topic("soil")))

  if (!kaliza_is_ag(m) && !grepl("hello|good morning|good afternoon|who are you|your name", m)) {
    return(list(st = st, p = kaliza_payload(
      "I stay with agriculture. I can still help: tell me if you want the irrigation gap, the soil that washes away, or the food lost after harvest, and I will give you the higher number, the lower number, and the answer.",
      "guiding", "neutral_gesturing",
      menu = c("Small-scale irrigation", "Soil conservation and fertility management", "Post-harvest collection and storage"))))
  }
  if (grepl("district|harvest|which district|greater|weaker", m) && is.null(crop) && is.null(topic)) {
    return(list(st = st, p = kaliza_payload(
      kaliza_district_say(),
      "guiding", "point_right_hand", dc$labels, dc$values, dc$title, dc$unit, menu = kaliza_menu_farmer, chart_type = kind)))
  }
  list(st = st, p = kaliza_payload(
    "That figure is not in Umusaruro, Isoko, Ikigega or Ejo Hazaza, so I will not guess it. Ask for a crop yield, a district harvest, a market price, the grain reserve, or a loss in the Ejo Hazaza plan.",
    "concerned", "neutral_gesturing",
    menu = c("District harvest", "Maize price", "Grain reserve", "Ejo hazaza loss")))
}

# ---- UI ----
amakuru_ui <- function() {
  tags$section(
    id = "amakuru", class = "panel",
    tags$div(
      class = "ak-stage",
      tags$div(
        class = "ak-grid",
        tags$div(
          class = "ak-left",
          tags$div(
            id = "kaliza-avatar", class = "ak-avatar", `data-expr` = "guiding",
            tags$img(class = "ak-face", src = "screenshoots/ejo%20hazaza/female-teacher-pointing-blackboard-holding-book-classroom-flat-vector-illustration-isolated-white-background-453465287.webp", alt = "Kaliza pointing at the numbers"),
            tags$button(id = "kaliza-mic", type = "button", class = "ak-mic", "aria-pressed" = "false", title = "Voice is off. Turn it on to hear Kaliza.",
                        tags$span(class = "ak-waves", tags$i(), tags$i(), tags$i()))
          ),
          tags$div(id = "kaliza-subs", class = "ak-subs", `aria-live` = "polite"),
          tags$div(id = "kaliza-menu", class = "ak-menu"),
          tags$form(
            id = "kaliza-form", class = "ak-chat", autocomplete = "off",
            tags$input(id = "kaliza-input", type = "text", placeholder = "Ask about Rwandan agriculture\u2026", `aria-label` = "Message Kaliza"),
            tags$button(id = "kaliza-send", type = "submit", class = "ak-send", "Send")
          )
        ),
        tags$div(
          class = "ak-right",
          tags$div(class = "ak-head", tags$p(class = "ak-kicker", "AMAKURU"),
                   tags$h2(id = "kaliza-chart-title", "District harvest")),
          tags$div(id = "kaliza-report", class = "ak-report"),
          tags$div(class = "ak-chart-switch",
                   tags$button(type = "button", class = "ak-chip", `data-chart` = "bar", "Bars"),
                   tags$button(type = "button", class = "ak-chip", `data-chart` = "pie", "Pie"),
                   tags$button(type = "button", class = "ak-chip", `data-chart` = "heat", "District heat")),
          tags$div(id = "kaliza-heat", class = "ak-heat"),
          tags$div(class = "ak-card ak-chart-card is-wait", tags$canvas(id = "kaliza-chart", height = "320")),
          tags$div(id = "kaliza-stats", class = "ak-stats"),
          tags$div(
            class = "ak-topics",
            lapply(seq_along(amakuru_topics), function(i) tags$button(type = "button", class = "ak-topic", `data-say` = amakuru_topics[[i]],
                                                                     tags$b(i), amakuru_topics[[i]]))
          ),
          tags$p(class = "ak-note", "Answers come from Umusaruro, Isoko, Ikigega and Ejo Hazaza. If a figure is not in those pages, Kaliza says so.")
        )
      )
    )
  )
}

amakuru_server <- function(input, output, session) {
  st <- reactiveValues(profession = NULL, topic = NULL, greeted = FALSE)
  send <- function(p) session$sendCustomMessage("kaliza", jsonlite::toJSON(p, auto_unbox = TRUE, null = "null", na = "null", digits = NA))

  observeEvent(input$kaliza_hello, {
    if (isTRUE(st$greeted)) return()
    st$greeted <- TRUE
    dc <- kaliza_district_chart("bar")
    send(kaliza_payload(kaliza_script, "guiding", "point_right_hand",
                        dc$labels, dc$values, dc$title, dc$unit, chart_type = "bar",
                        menu = c("I am a farmer", "I work in government", "I am a citizen", "Civil society", "Development partner")))
    hk <- kaliza_env("HEYGEN_API_KEY")
    if (!is.null(hk) && requireNamespace("httr2", quietly = TRUE)) {
      tok <- tryCatch({
        r <- httr2::request("https://api.heygen.com/v1/streaming.create_token") |> httr2::req_headers(`x-api-key` = hk) |>
          httr2::req_method("POST") |> httr2::req_timeout(15) |> httr2::req_perform()
        httr2::resp_body_json(r)$data$token
      }, error = function(e) NULL)
      if (!is.null(tok)) session$sendCustomMessage("kaliza_heygen", list(token = tok, avatar = Sys.getenv("HEYGEN_AVATAR_ID", "default")))
    }
  })

  observeEvent(input$kaliza_msg, {
    msg <- input$kaliza_msg$text
    if (is.null(msg) || !nzchar(trimws(msg))) return()
    state <- list(profession = st$profession, topic = st$topic)
    out <- tryCatch(kaliza_reply(msg, state), error = function(e) list(st = state, p = kaliza_payload("Something went wrong on my side. Try asking again in a moment.", "concerned")))
    st$profession <- out$st$profession
    st$topic <- out$st$topic
    send(out$p)
  })
}
