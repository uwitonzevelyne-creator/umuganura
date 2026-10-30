# Umuganura — Ikigega (Rwandan agricultural stock)

ik_crop_label <- c(
  "Maize" = "Maize / Ibigori", "Sorghum" = "Sorghum / Amasaka", "Paddy rice" = "Rice / Umuceri", "Wheat" = "Wheat / Ingano",
  "Bush bean" = "Bush bean / Ibishyimbo bigufi", "Climbing bean" = "Climbing bean / Ibishyimbo bishingirirwa",
  "Pea" = "Peas / Amashaza", "Soybean" = "Soya bean / Soya", "Groundnuts" = "Groundnut / Ubunyobwa",
  "Irish potato" = "Irish potato / Ibirayi", "Sweet potato" = "Sweet potato / Ibijumba", "Cassava" = "Cassava / Imyumbati",
  "Cooking banana" = "Cooking banana / Ibitoki", "Yams & Taro" = "Yams and taro / Ibikoro n'amateke",
  "Vegetables" = "Vegetables / Imboga", "Fruits" = "Fruits / Imbuto"
)
ik_choices <- function(crops) stats::setNames(crops, ifelse(crops %in% names(ik_crop_label), ik_crop_label[crops], crops))

ik_fbs <- c("Maize" = "Maize and products", "Paddy rice" = "Rice and products", "Sorghum" = "Sorghum and products",
            "Wheat" = "Wheat and products", "Bush bean" = "Beans", "Climbing bean" = "Beans", "Pea" = "Peas",
            "Soybean" = "Soyabeans", "Groundnuts" = "Groundnuts")

hort_ids <- c("avocado", "passion_fruit", "tomato", "onion", "carrot", "sweet_pepper", "plum")

# District vegetables and fruits come from the same NISR SAS production tables
# as the home map (maize in those tables matches the national production line).
# 2019–2025 are Seasons A and B; 2026 is Season A only. NAEB export weight and
# earnings are national. FAOSTAT crop tonnes are national. No cold-room
# temperature or district export-storage capacity is in these files.
hort_sum <- function(x) if (any(is.finite(x))) sum(x, na.rm = TRUE) else NA_real_
hort_files <- data.frame(
  year = as.character(home_prod_files[, 1]),
  file = as.character(home_prod_files[, 2]),
  season = c("A", "B", "A", "B", "A", "B", "A", "B", "A", "B", "A", "B", "A", "B", "A"),
  stringsAsFactors = FALSE
)
hort_one <- function(path, year, season) {
  x <- tryCatch(utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(x) || !ncol(x)) return(NULL)
  d <- hv_district(x[[1]])
  take <- function(label) {
    i <- which(tolower(trimws(names(x))) == tolower(label) & !grepl("_", names(x), fixed = TRUE))
    if (!length(i)) return(rep(NA_real_, nrow(x)))
    hv_num(x[[i[1]]])
  }
  ok <- which(!is.na(d) & d != "Rwanda" & !duplicated(d))
  if (!length(ok)) return(NULL)
  data.frame(
    year = as.integer(year), season = season, district = d[ok],
    vegetables = take("Vegetables")[ok], fruits = take("Fruits")[ok],
    stringsAsFactors = FALSE
  )
}
hort_root <- file.path("dataset hachthon", "CLEANED", "nisr_sas_tables")
hort_district <- do.call(rbind, lapply(seq_len(nrow(hort_files)), function(i) {
  hort_one(file.path(hort_root, hort_files$year[i], hort_files$file[i]), hort_files$year[i], hort_files$season[i])
}))
if (is.null(hort_district)) hort_district <- data.frame(year = integer(), season = character(), district = character(), vegetables = numeric(), fruits = numeric())
rownames(hort_district) <- NULL

hort_naeb <- local({
  empty <- data.frame(fiscal_year = character(), year = integer(), export_t = numeric(), export_usd = numeric(), stringsAsFactors = FALSE)
  if (!file.exists(naeb_export_path)) return(empty)
  x <- utils::read.csv(naeb_export_path, stringsAsFactors = FALSE)
  x <- x[x$commodity == "Horticulture", , drop = FALSE]
  if (!nrow(x)) return(empty)
  do.call(rbind, lapply(unique(x$fiscal_year), function(fy) {
    s <- x[x$fiscal_year == fy, , drop = FALSE]
    mt <- suppressWarnings(as.numeric(s$value[s$metric == "export_mt"]))
    kg <- suppressWarnings(as.numeric(s$value[s$metric == "export_kg"]))
    usd <- suppressWarnings(as.numeric(s$value[s$metric == "export_usd"]))
    tonnes <- if (length(mt) && any(is.finite(mt))) mt[is.finite(mt)][1] else if (length(kg) && any(is.finite(kg))) kg[is.finite(kg)][1] / 1000 else NA_real_
    data.frame(
      fiscal_year = fy,
      year = suppressWarnings(as.integer(sub(".*/", "", fy))),
      export_t = tonnes,
      export_usd = if (length(usd) && any(is.finite(usd))) usd[is.finite(usd)][1] else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
})
if (!is.null(hort_naeb) && nrow(hort_naeb)) hort_naeb <- hort_naeb[order(hort_naeb$year), , drop = FALSE]

hort_fao_choices <- c(
  "Avocados (FAO, national)" = "Avocados",
  "Tomatoes (FAO, national)" = "Tomatoes",
  "Cabbages (FAO, national)" = "Cabbages",
  "Pineapples (FAO, national)" = "Pineapples",
  "Onions and shallots (FAO, national)" = "Onions and shallots, dry (excluding dehydrated)",
  "Other tropical fruits (FAO, national)" = "Other tropical fruits, n.e.c.",
  "All vegetables (FAO, national)" = "Vegetables Primary",
  "All fruit (FAO, national)" = "Fruit Primary"
)
hort_fao <- if (exists("fao_prod") && nrow(fao_prod)) {
  fao_prod[fao_prod$item %in% unname(hort_fao_choices) & is.finite(fao_prod$value), c("item", "year", "value"), drop = FALSE]
} else {
  data.frame(item = character(), year = integer(), value = numeric())
}
hort_fao_choices <- hort_fao_choices[unname(hort_fao_choices) %in% unique(hort_fao$item)]
hort_year_bounds <- c(2019L, 2026L)
if (nrow(hort_district)) hort_year_bounds <- range(hort_district$year)
if (nrow(hort_fao)) hort_year_bounds[1] <- min(hort_year_bounds[1], min(hort_fao$year))

ik_steps <- c("1" = "National Strategic Reserve", "2" = "Cooperative warehousing", "3" = "Horticulture logistics")

ik_step_bar <- function(n) {
  tags$div(
    class = "hv-step-foot",
    if (n > 1) tags$button(type = "button", class = "btn-ghost", `data-step-go` = n - 1, paste("\u2190", ik_steps[[n - 1]])),
    tags$span(class = "hv-step-count", paste(n, "of", length(ik_steps))),
    if (n < length(ik_steps)) tags$button(type = "button", class = "btn-go", `data-step-go` = n + 1, paste(ik_steps[[n + 1]], "\u2192"))
  )
}

ikigega_ui <- function() {
  yrs <- c(1996, 2026)
  tags$section(
    id = "ikigega", class = "panel",
    hero_banner(
      "umusaruro/ikigega-hero.jpg", "Ikigega (Rwandan Agricultural Stock)",
      "After the harvest (umusaruro) and the market (isoko), what stays behind: grain held in reserve, produce in cooperative stores, and fresh crops in cold rooms.",
      "IKIGEGA"
    ),
    tags$div(
      id = "ik-land", class = "ik-land",
      tags$div(
        class = "ik-map",
        tags$svg(
          class = "ik-wires", viewBox = "0 0 1000 640", preserveAspectRatio = "none", `aria-hidden` = "true",
          tags$path(class = "ik-wire", d = "M390 250 H250 V110 H180"),
          tags$circle(class = "ik-dot", cx = "180", cy = "110", r = "7"),
          tags$path(class = "ik-wire", d = "M390 390 H250 V520 H180"),
          tags$circle(class = "ik-dot", cx = "180", cy = "520", r = "7"),
          tags$path(class = "ik-wire", d = "M610 250 H760 V110 H820"),
          tags$circle(class = "ik-dot", cx = "820", cy = "110", r = "7")
        ),
        tags$button(type = "button", class = "ik-node ik-coop", `data-ik-go` = "2", "Cooperative Warehouse"),
        tags$button(type = "button", class = "ik-node ik-nsr", `data-ik-go` = "1", "National Strategic Reserve"),
        tags$img(class = "ik-basket", src = "ikigega/basket.png?v=3", alt = "Woven Ikigega basket"),
        tags$button(type = "button", class = "ik-node ik-hort", `data-ik-go` = "3", "Horticulture Logistics")
      )
    ),
    tags$div(
      id = "ik-room", class = "hv-room step-scope ik-room is-hidden",
      tags$div(class = "ik-backbar", tags$button(type = "button", class = "btn-ghost", `data-ik-back` = "1", "\u2190 Ikigega")),
      tags$nav(
        class = "hv-steps",
        lapply(names(ik_steps), function(n) tags$button(type = "button", class = paste("hv-step-btn", if (n == "1") "is-on"), `data-step` = n, tags$b(n), ik_steps[[n]]))
      ),
      tags$div(
        class = "hv-step is-on", `data-step` = "1",
        tags$div(
          class = "hv-step-head",
          tags$p(class = "kicker", "1ST STOCK \u00b7 NATIONAL STRATEGIC RESERVE"),
          tags$h2("Bulk grain against the 46,000 MT floor"),
          tags$p("Depletion risk: the alarm turns red when grain held after harvest drops near or under the 46,000 MT baseline buffer.")
        ),
        tags$div(
          class = "price-filters hv-filters",
          selectizeInput("ik_nsr_products", "PRODUCTS", choices = ik_choices(nsr_crops), selected = nsr_crops, multiple = TRUE, width = "100%",
                         options = list(plugins = list("remove_button"))),
          sliderInput("ik_nsr_years", "30-YEAR RANGE", min = yrs[1], max = yrs[2], value = yrs, sep = "", width = "100%"),
          selectInput("ik_nsr_measure", "MEASURE", choices = c("Total mass volume (MT)" = "mass", "Baseline buffer thresholds (% of floor)" = "buffer"), selected = "mass", width = "100%"),
          selectInput("ik_nsr_visual", "VISUAL", choices = c("Solid gauge chart" = "gauge", "Horizontal progress bar" = "bar"), selected = "gauge", width = "100%")
        ),
        uiOutput("ik_nsr_alarm"),
        tags$div(
          class = "split",
          tags$div(
            class = "chart-card",
            tags$h3(class = "card-title", textOutput("ik_nsr_title", inline = TRUE)),
            conditionalPanel("input.ik_nsr_visual == 'gauge'", plotlyOutput("ik_nsr_gauge", height = "320px")),
            conditionalPanel("input.ik_nsr_visual == 'bar'", uiOutput("ik_nsr_bars"))
          ),
          tags$div(class = "chart-card", tags$h3(class = "card-title", "Products in Rwandan stock"), uiOutput("ik_nsr_in"))
        ),
        tags$div(class = "chart-card", tags$h3(class = "card-title", "Grain held after each harvest"), plotlyOutput("ik_nsr_trend", height = "420px")),
        tags$div(class = "chart-card", tags$h3(class = "card-title", "Net change in national stocks (FAO food balances)"), plotlyOutput("ik_nsr_fao", height = "320px")),
        uiOutput("ik_nsr_note"),
        ik_step_bar(1)
      ),
      tags$div(
        class = "hv-step", `data-step` = "2",
        tags$div(
          class = "hv-step-head",
          tags$p(class = "kicker", "2ND STOCK \u00b7 COOPERATIVE WAREHOUSING"),
          tags$h2("What goes into store, what goes to market, what rots"),
          tags$p("Aflatoxin and moisture contamination show up as post-harvest loss during drying and storage. Track how loss moves by product, season and year, and where cooperatives reach farmers.")
        ),
        tags$div(
          class = "price-filters hv-filters",
          selectInput("ik_coop_product", "PRODUCT", choices = ik_choices(intersect(names(ik_crop_label), unique(stock_mass$crop))), selected = "Maize", width = "100%"),
          sliderInput("ik_coop_years", "30-YEAR RANGE", min = yrs[1], max = yrs[2], value = yrs, sep = "", width = "100%"),
          selectInput("ik_coop_measure", "MEASURE", choices = c("Post-harvest loss rate (%)" = "loss", "Inflow / outflow volume (MT)" = "flow"), selected = "loss", width = "100%"),
          selectInput("ik_coop_district", "DISTRICT", choices = c("All districts", sort(names(province_of))), selected = "All districts", width = "100%")
        ),
        uiOutput("ik_coop_cards"),
        tags$div(class = "chart-card", plotlyOutput("ik_coop_chart", height = "420px")),
        tags$div(
          class = "hv-map-grid two",
          tags$div(
            class = "chart-card map-card",
            tags$h3(class = "card-title", "Where cooperatives reach farmers"),
            tags$p("Bigger store icons: more farmers buy seed and fertilizer through a cooperative. Zoom with + and \u2212."),
            if (has_leaflet) leaflet::leafletOutput("ik_coop_map", height = "480px")
          ),
          tags$aside(class = "hv-side", uiOutput("ik_coop_side"))
        ),
        uiOutput("ik_coop_note"),
        ik_step_bar(2)
      ),
      tags$div(
        class = "hv-step", `data-step` = "3",
        tags$div(
          class = "hv-step-head",
          tags$p(class = "kicker", "3RD STOCK \u00b7 HORTICULTURE LOGISTICS"),
          tags$h2("Export storage"),
          tags$p("Vegetables and fruits are the harvest that has to be packed and moved. District figures are NISR SAS production. What left the country is NAEB, and only as a national total. A cold-room temperature is not published, so it stays blank.")
        ),
        tags$div(
          class = "price-filters hv-filters",
          selectInput(
            "ik_cold_product", "PRODUCT",
            choices = c("Vegetables and fruits" = "both", "Vegetables" = "Vegetables", "Fruits" = "Fruits", hort_fao_choices),
            selected = "both", width = "100%"
          ),
          sliderInput("ik_cold_years", "YEARS", min = hort_year_bounds[1], max = hort_year_bounds[2], value = c(hort_year_bounds[2], hort_year_bounds[2]), sep = "", width = "100%"),
          selectInput("ik_cold_district", "DISTRICT", choices = c("All districts", sort(names(province_of))), selected = "All districts", width = "100%")
        ),
        uiOutput("ik_cold_cards"),
        tags$div(class = "chart-card", tags$h3(class = "card-title", textOutput("ik_cold_title", inline = TRUE)), plotlyOutput("ik_cold_plot", height = "560px")),
        uiOutput("ik_cold_table"),
        uiOutput("ik_cold_note"),
        tags$div(
          class = "ik-upload",
          fileInput("ik_cold_file", "A cold-room log is not published. A store can still upload its own (CSV).", accept = c(".csv", "text/csv"), width = "100%"),
          downloadButton("ik_cold_template", "Download the log template")
        ),
        uiOutput("ik_cold_upload"),
        ik_step_bar(3)
      )
    ),
    site_footer()
  )
}

ikigega_server <- function(input, output, session) {
  ik_range <- function(x) if (is.null(x)) c(1996, 2026) else x

  # ---- 1. National Strategic Reserve ----
  nsr_sel <- reactive({
    p <- input$ik_nsr_products
    if (is.null(p) || !length(p)) p <- nsr_crops
    yr <- ik_range(input$ik_nsr_years)
    d <- stock_mass[stock_mass$crop %in% p & stock_mass$year >= yr[1] & stock_mass$year <= yr[2] & is.finite(stock_mass$stored_mt), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    tot <- stats::aggregate(stored_mt ~ year + season, d, sum)
    tot <- tot[order(tot$year, tot$season), , drop = FALSE]
    last <- tot[nrow(tot), ]
    list(d = d, tot = tot, last = last, by = d[d$year == last$year & d$season == last$season, , drop = FALSE])
  })

  output$ik_nsr_title <- renderText({
    s <- nsr_sel()
    if (is.null(s)) return("No stock figure in this range")
    paste0("Grain held after Season ", s$last$season, " ", s$last$year)
  })

  output$ik_nsr_alarm <- renderUI({
    s <- nsr_sel()
    if (is.null(s)) return(hv_empty("No published stock figure falls in this year range. SAS storage shares start in 2019."))
    v <- s$last$stored_mt
    pct <- 100 * v / nsr_floor
    tone <- if (v < nsr_floor) "is-red" else if (v < 1.2 * nsr_floor) "is-amber" else "is-green"
    msg <- if (v < nsr_floor) paste0("Depletion alarm: ", fmt_num(v, 0), " MT is ", fmt_num(nsr_floor - v, 0), " MT under the 46,000 MT floor (", fmt_num(pct, 0), "% of the buffer).")
           else if (v < 1.2 * nsr_floor) paste0("Near the floor: ", fmt_num(v, 0), " MT, within 20% of the 46,000 MT buffer.")
           else paste0("Above the floor: ", fmt_num(v, 0), " MT, ", fmt_num(pct, 0), "% of the 46,000 MT buffer.")
    tags$div(class = paste("ik-alarm", tone), tags$b(if (v < nsr_floor) "ALARM" else if (v < 1.2 * nsr_floor) "WATCH" else "SAFE"), tags$span(msg))
  })

  output$ik_nsr_gauge <- renderPlotly({
    s <- nsr_sel()
    req(s)
    v <- s$last$stored_mt
    buffer <- identical(input$ik_nsr_measure, "buffer")
    val <- if (buffer) 100 * v / nsr_floor else v
    top <- if (buffer) max(200, val * 1.15) else max(2 * nsr_floor, v * 1.15)
    fl <- if (buffer) 100 else nsr_floor
    p <- plotly::plot_ly(
      type = "indicator", mode = "gauge+number", value = val,
      number = list(suffix = if (buffer) "%" else " MT", valueformat = ",.0f", font = list(size = 34, color = "#1b4332")),
      gauge = list(
        shape = "angular",
        axis = list(range = list(0, top), tickformat = ",.0f"),
        bar = list(color = if (v < nsr_floor) "#9b2226" else if (v < 1.2 * nsr_floor) "#e9a23b" else "#2d6a4f", thickness = 0.75),
        bgcolor = "#f4f6ef", borderwidth = 0,
        steps = list(list(range = c(0, fl), color = "#fde2e1"), list(range = c(fl, fl * 1.2), color = "#fff1cc"), list(range = c(fl * 1.2, top), color = "#e3f2e8")),
        threshold = list(line = list(color = "#9b2226", width = 4), thickness = 0.9, value = fl)
      )
    )
    plotly::layout(p, margin = list(l = 30, r = 30, t = 30, b = 10), paper_bgcolor = "rgba(0,0,0,0)", font = list(family = "Outfit, Segoe UI, sans-serif")) |>
      plotly::config(displayModeBar = FALSE)
  }) |> bindCache(input$ik_nsr_products, input$ik_nsr_years, input$ik_nsr_measure, cache = "app")

  output$ik_nsr_bars <- renderUI({
    s <- nsr_sel()
    req(s)
    buffer <- identical(input$ik_nsr_measure, "buffer")
    by <- s$by[order(-s$by$stored_mt), , drop = FALSE]
    v <- s$last$stored_mt
    bar <- function(label, x, max_x, tone = "", floor_at = NULL) {
      tags$div(
        class = "ik-bar-row",
        tags$span(label),
        tags$div(
          class = paste("ik-bar", tone),
          tags$i(style = sprintf("width:%.1f%%", min(100, 100 * x / max_x))),
          if (!is.null(floor_at)) tags$em(class = "ik-floor", style = sprintf("left:%.1f%%", 100 * floor_at / max_x), title = "46,000 MT floor")
        ),
        tags$b(if (buffer) paste0(fmt_num(100 * x / nsr_floor, 1), "%") else paste(fmt_num(x, 0), "MT"))
      )
    }
    top <- max(nsr_floor * 1.2, v)
    tags$div(
      class = "ik-bars",
      bar("All selected grain", v, top, if (v < nsr_floor) "is-red" else if (v < 1.2 * nsr_floor) "is-amber" else "is-green", nsr_floor),
      tags$p(class = "note", "The red tick on the first bar marks the 46,000 MT floor."),
      lapply(seq_len(nrow(by)), function(i) bar(ik_crop_label[[by$crop[i]]], by$stored_mt[i], max(by$stored_mt)))
    )
  })

  output$ik_nsr_in <- renderUI({
    s <- nsr_sel()
    if (is.null(s)) return(hv_empty("No product has a storage figure in this range."))
    by <- s$by[s$by$stored_mt > 0, , drop = FALSE]
    by <- by[order(-by$stored_mt), , drop = FALSE]
    tags$div(
      class = "ik-chips",
      lapply(seq_len(nrow(by)), function(i) tags$span(class = "ik-chip", tags$b(ik_crop_label[[by$crop[i]]]),
                                                      paste0(fmt_num(by$stored_mt[i], 0), " MT \u00b7 ", fmt_num(by$stored[i], 1), "% of harvest stored"))),
      tags$p(class = "note", paste0("Season ", s$last$season, " ", s$last$year, ". Stored = harvest \u00d7 share farmers report keeping in store (NISR SAS)."))
    )
  })

  output$ik_nsr_trend <- renderPlotly({
    s <- nsr_sel()
    validate(need(!is.null(s) && nrow(s$d) > 0, "No grain-held figure in this year range. SAS storage shares start in 2019."))
    d <- s$d
    d$lab <- paste(d$year, d$season)
    d$name <- unname(ik_crop_label[d$crop])
    lv <- unique(d$lab[order(d$year, d$season)])
    d$lab <- factor(d$lab, levels = lv)
    buffer <- identical(input$ik_nsr_measure, "buffer")
    if (buffer) d$stored_mt <- 100 * d$stored_mt / nsr_floor
    y_floor <- if (buffer) 100 else nsr_floor
    p <- plotly::plot_ly(
      d, x = ~lab, y = ~stored_mt, color = ~name, type = "bar",
      colors = c("#1b4332", "#2d6a4f", "#40916c", "#52b788", "#95d5b2", "#c6a336", "#6f4e37", "#9b2226", "#8d99ae"),
      hovertemplate = paste0("%{fullData.name}<br>%{x}: %{y:,.0f}", if (buffer) "% of floor" else " MT", "<extra></extra>")
    ) |>
      plotly::layout(
        barmode = "stack",
        shapes = list(list(
          type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = y_floor, y1 = y_floor,
          line = list(color = "#9b2226", dash = "dash", width = 2)
        )),
        annotations = list(list(
          xref = "paper", x = 1, y = y_floor, text = "46,000 MT floor",
          showarrow = FALSE, xanchor = "right", yanchor = "bottom",
          font = list(color = "#9b2226")
        ))
      )
    hv_plot_layout(p, if (buffer) "% of the 46,000 MT floor" else "Grain held (MT)")
  })

  output$ik_nsr_fao <- renderPlotly({
    p <- input$ik_nsr_products
    if (is.null(p) || !length(p)) p <- nsr_crops
    yr <- ik_range(input$ik_nsr_years)
    items <- unique(unname(ik_fbs[p[p %in% names(ik_fbs)]]))
    sv <- harvest_data$stockvar
    d <- sv[sv$item %in% items & sv$year >= yr[1] & sv$year <= yr[2], , drop = FALSE]
    validate(need(nrow(d) > 0, "FAO publishes no stock change for these products in this range."))
    p <- plotly::plot_ly(d, x = ~year, y = ~value, color = ~item, type = "bar",
                         colors = c("#1b4332", "#40916c", "#95d5b2", "#c6a336", "#6f4e37", "#9b2226", "#8d99ae"),
                         hovertemplate = "%{fullData.name}<br>%{x}: %{y:,.0f} MT<extra></extra>") |>
      plotly::layout(barmode = "relative")
    hv_plot_layout(p, "Added to (+) or drawn from (\u2212) stocks, MT")
  }) |> bindCache(input$ik_nsr_products, input$ik_nsr_years, cache = "app")

  output$ik_nsr_note <- renderUI(tags$p(
    class = "note",
    "The National Strategic Reserve warehouse level is not published as an open series. The gauge uses the closest published measure: grain that farmers keep in store after each harvest (NISR SAS production \u00d7 share stored, 2019\u20132026). The FAO chart shows the national stock balance from food balance sheets (2010\u20132023). Years outside these ranges stay empty."
  ))

  # ---- 2. Cooperative warehousing ----
  coop_sel <- reactive({
    cr <- input$ik_coop_product
    if (is.null(cr)) cr <- "Maize"
    yr <- ik_range(input$ik_coop_years)
    stock_mass[stock_mass$crop == cr & stock_mass$year >= yr[1] & stock_mass$year <= yr[2], , drop = FALSE]
  })

  output$ik_coop_cards <- renderUI({
    d <- coop_sel()
    if (!nrow(d)) return(hv_empty("No storage or loss figure for this product in this range."))
    d <- d[order(d$year, d$season), , drop = FALSE]
    last <- d[nrow(d), ]
    worst <- d[which.max(d$lost), ]
    dist <- input$ik_coop_district
    cr <- if (!is.null(dist) && dist != "All districts") coop_reach[coop_reach$district == dist, , drop = FALSE] else NULL
    tags$div(
      class = "kpi-row hv-kpis",
      hv_kpi("Loss rate, latest", paste0(fmt_num(last$lost, 2), "%"), paste0("Season ", last$season, " ", last$year)),
      hv_kpi("Worst season", paste0(fmt_num(worst$lost, 2), "%"), paste0("Season ", worst$season, " ", worst$year), "is-warn"),
      hv_kpi("Into store", paste(fmt_num(last$stored_mt, 0), "MT"), paste0(fmt_num(last$stored, 1), "% of the harvest")),
      hv_kpi("Out to market", paste(fmt_num(last$sold_mt, 0), "MT"), paste0(fmt_num(last$sold, 1), "% sold")),
      if (!is.null(cr) && nrow(cr)) hv_kpi(paste("Cooperatives in", dist), paste0(fmt_num(cr$fert, 1), "%"), paste0("of fertilizer buyers; ", fmt_num(cr$seed, 1), "% of seed buyers"))
    )
  })

  output$ik_coop_chart <- renderPlotly({
    d <- coop_sel()
    validate(need(nrow(d) > 0, "No storage or loss figure for this product in this range."))
    d <- d[order(d$year, d$season), , drop = FALSE]
    if (identical(input$ik_coop_measure, "flow")) {
      d$lab <- factor(paste(d$year, d$season), levels = unique(paste(d$year, d$season)))
      long <- rbind(
        data.frame(lab = d$lab, flow = "Inflow to store", mt = d$stored_mt),
        data.frame(lab = d$lab, flow = "Outflow to market", mt = d$sold_mt),
        data.frame(lab = d$lab, flow = "Lost after harvest", mt = d$lost_mt)
      )
      p <- plotly::plot_ly(long, x = ~lab, y = ~mt, color = ~flow, type = "bar",
                           colors = c("Inflow to store" = "#2d6a4f", "Outflow to market" = "#c6a336", "Lost after harvest" = "#9b2226"),
                           hovertemplate = "%{fullData.name}<br>%{x}: %{y:,.0f} MT<extra></extra>") |>
        plotly::layout(barmode = "group")
      return(hv_plot_layout(p, "Volume (MT)") |> plotly::layout(yaxis = list(type = "log", title = "Volume (MT, log scale)")))
    }
    d$Season <- paste("Season", d$season)
    p <- plotly::plot_ly(d, x = ~factor(year), y = ~lost, color = ~Season, type = "bar",
                         colors = c("Season A" = "#2d6a4f", "Season B" = "#c6a336", "Season C" = "#6f4e37"),
                         hovertemplate = "%{fullData.name} %{x}<br>Loss %{y:.2f}% of harvest<extra></extra>") |>
      plotly::layout(barmode = "group")
    hv_plot_layout(p, "Post-harvest loss (% of harvest)", "Year")
  }) |> bindCache(input$ik_coop_product, input$ik_coop_years, input$ik_coop_measure, cache = "app")

  output$ik_coop_map <- leaflet::renderLeaflet({
    cr <- coop_reach[coop_reach$district %in% names(district_lnglat), , drop = FALSE]
    cr$reach <- rowMeans(cr[, c("seed", "fert")], na.rm = TRUE)
    v <- stats::setNames(cr$reach, cr$district)
    pal <- leaflet::colorNumeric(c("#f4f6ef", "#b7e4c7", "#40916c", "#1b4332"), domain = range(v, na.rm = TRUE))
    m <- hv_fill(hv_base_map(), v, pal, function(d, x) {
      row <- cr[cr$district == d, , drop = FALSE]
      if (!nrow(row)) return(paste0("<b>", d, "</b>"))
      paste0("<b>", d, "</b><br>Fertilizer via cooperative: ", fmt_num(row$fert, 1), "%<br>Seed via cooperative: ", fmt_num(row$seed, 1), "%")
    })
    xy <- district_lnglat[cr$district]
    sz <- 18 + 30 * (cr$reach - min(cr$reach, na.rm = TRUE)) / max(1e-9, diff(range(cr$reach, na.rm = TRUE)))
    for (i in seq_len(nrow(cr))) {
      if (!is.finite(sz[i])) next
      m <- leaflet::addMarkers(m, lng = xy[[i]]$cx, lat = xy[[i]]$cy, layerId = paste0("coop-", cr$district[i]),
                               icon = leaflet::makeIcon("umusaruro/coop.svg", iconWidth = sz[i], iconHeight = sz[i]),
                               label = paste0(cr$district[i], ": ", fmt_num(cr$reach[i], 1), "% average cooperative reach"))
    }
    dist <- input$ik_coop_district
    if (!is.null(dist) && dist %in% names(district_lnglat)) {
      g <- district_lnglat[[dist]]
      m <- leaflet::addPolylines(m, lng = g$lng, lat = g$lat, color = "#3a86ff", weight = 4) |> leaflet::setView(g$cx, g$cy, 9)
    }
    leaflet::addLegend(m, "bottomright", pal = pal, values = v, title = "Cooperative<br>reach (%)", opacity = 0.9)
  })

  output$ik_coop_side <- renderUI({
    cr <- coop_reach
    cr$reach <- rowMeans(cr[, c("seed", "fert")], na.rm = TRUE)
    cr <- cr[cr$district %in% names(province_of), , drop = FALSE]
    cr <- cr[order(-cr$reach), , drop = FALSE]
    row <- function(i, tone = "") tags$div(class = "hv-bar-row", tags$span(cr$district[i]),
                                           tags$div(class = paste("hv-bar", tone), tags$i(style = sprintf("width:%.0f%%", max(2, 100 * cr$reach[i] / max(cr$reach))))),
                                           tags$b(paste0(fmt_num(cr$reach[i], 1), "%")))
    n <- nrow(cr)
    tags$div(
      tags$div(class = "hv-side-card", tags$h4(tags$img(src = "umusaruro/coop.svg", alt = ""), "Many cooperatives"), lapply(1:min(5, n), row)),
      tags$div(class = "hv-side-card is-low", tags$h4("Few cooperatives"), lapply(n:max(1, n - 4), row, tone = "is-low")),
      tags$p(class = "note", "Reach = average of the shares of farmers buying seed and fertilizer through a cooperative, NISR SAS 2026 Season A.")
    )
  })

  output$ik_coop_note <- renderUI(tags$p(
    class = "note",
    "Loss, storage and sales shares are national by product and season (NISR SAS 2019\u20132026); SAS does not split them by district, so the district picker moves the map and the cooperative card only. Laboratory aflatoxin and grain-moisture readings are not published; post-harvest loss is the measured outcome of that decay."
  ))

  # ---- 3. Horticulture logistics ----
  output$ik_cold_template <- downloadHandler(
    filename = function() "umuganura-cold-room-log-template.csv",
    content = function(file) utils::write.csv(cold_template, file, row.names = FALSE)
  )

  cold_log <- reactive({
    f <- input$ik_cold_file
    if (is.null(f)) return(NULL)
    x <- tryCatch(utils::read.csv(f$datapath, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    if (is.null(x)) return(list(error = "The file could not be read as CSV."))
    names(x) <- tolower(trimws(names(x)))
    need_cols <- c("lot_id", "product", "received_at", "temp_c")
    miss <- setdiff(need_cols, names(x))
    if (length(miss)) return(list(error = paste("Missing columns:", paste(miss, collapse = ", "))))
    if (!"dispatched_at" %in% names(x)) x$dispatched_at <- NA_character_
    parse_t <- function(v) as.POSIXct(v, tz = "Africa/Kigali", tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d"), optional = TRUE)
    x$received <- parse_t(x$received_at)
    x$dispatched <- parse_t(ifelse(nzchar(trimws(as.character(x$dispatched_at))), x$dispatched_at, NA))
    x$temp_c <- hv_num(x$temp_c)
    x <- x[!is.na(x$received), , drop = FALSE]
    list(d = x)
  })

  hort_pick <- reactive({
    pr <- input$ik_cold_product
    if (is.null(pr) || !nzchar(pr)) pr <- "both"
    yr <- ik_range(input$ik_cold_years)
    dist <- input$ik_cold_district
    if (is.null(dist) || !nzchar(dist)) dist <- "All districts"
    fao <- pr %in% hort_fao$item
    list(product = pr, yr = yr, district = dist, fao = fao)
  })

  hort_sas <- reactive({
    s <- hort_pick()
    if (s$fao || !nrow(hort_district)) return(NULL)
    d <- hort_district[hort_district$year >= s$yr[1] & hort_district$year <= s$yr[2], , drop = FALSE]
    if (s$district != "All districts") d <- d[d$district == s$district, , drop = FALSE]
    if (!nrow(d)) return(d)
    d$value <- if (s$product == "Vegetables") d$vegetables else if (s$product == "Fruits") d$fruits else {
      both <- cbind(d$vegetables, d$fruits)
      ifelse(rowSums(is.finite(both)) == 0, NA_real_, rowSums(both, na.rm = TRUE))
    }
    d
  })

  naeb_kpi <- function() {
    if (!nrow(hort_naeb)) return(NULL)
    last <- hort_naeb[nrow(hort_naeb), ]
    usd <- if (is.finite(last$export_usd)) paste0("$", fmt_num(last$export_usd / 1e6, 1), "m") else "—"
    wt <- hort_naeb[is.finite(hort_naeb$export_t), , drop = FALSE]
    weight_note <- if (nrow(wt)) paste0(fmt_num(wt$export_t[nrow(wt)], 0), " t exported in ", wt$fiscal_year[nrow(wt)], ". District export weight is not published.") else "Export weight is not published for this year."
    hv_kpi("Horticulture exports", usd, paste0("NAEB, national, ", last$fiscal_year, ". ", weight_note))
  }

  output$ik_cold_cards <- renderUI({
    s <- hort_pick()
    if (s$fao) {
      d <- hort_fao[hort_fao$item == s$product & hort_fao$year >= s$yr[1] & hort_fao$year <= s$yr[2], , drop = FALSE]
      d <- d[order(d$year), , drop = FALSE]
      if (!nrow(d)) return(hv_empty("FAOSTAT has no production figure for this crop in the selected years."))
      last <- d[nrow(d), ]
      first <- d[1, ]
      chg <- if (nrow(d) > 1 && is.finite(first$value) && first$value > 0) 100 * (last$value / first$value - 1) else NA_real_
      return(tags$div(class = "kpi-row hv-kpis",
        hv_kpi("National production", paste(fmt_num(last$value, 0), "t"), paste("FAOSTAT,", last$year)),
        hv_kpi("Change in this range", if (is.finite(chg)) fmt_pct(chg, 0) else "—", paste(first$year, "to", last$year)),
        hv_kpi("District split", "Not published", "FAOSTAT gives Rwanda as one total"),
        naeb_kpi()
      ))
    }
    d <- hort_sas()
    if (is.null(d) || !nrow(d) || !any(is.finite(d$value))) {
      return(tags$div(
        tags$div(class = "kpi-row hv-kpis", naeb_kpi()),
        hv_empty("NISR SAS does not publish district vegetables and fruits before 2019. The current 30 districts date from 2006, and these production tables start with 2019.")
      ))
    }
    if (s$district != "All districts") {
      by_year <- stats::aggregate(value ~ year, d, hort_sum)
      by_year <- by_year[order(by_year$year), , drop = FALSE]
      veg <- hort_sum(d$vegetables)
      fr <- hort_sum(d$fruits)
      return(tags$div(class = "kpi-row hv-kpis",
        hv_kpi(paste(s$district, "in view"), paste(fmt_num(hort_sum(d$value), 0), "t"), "NISR SAS production"),
        hv_kpi("Vegetables", paste(fmt_num(veg, 0), "t"), "Same seasons"),
        hv_kpi("Fruits", paste(fmt_num(fr, 0), "t"), "Same seasons"),
        naeb_kpi()
      ))
    }
    by_d <- stats::aggregate(value ~ district, d, hort_sum)
    by_d <- by_d[is.finite(by_d$value), , drop = FALSE]
    by_d <- by_d[order(-by_d$value), , drop = FALSE]
    hi <- by_d[1, ]
    lo <- by_d[nrow(by_d), ]
    span <- if (s$yr[1] == s$yr[2]) as.character(s$yr[1]) else paste(s$yr[1], "\u2013", s$yr[2])
    tags$div(class = "kpi-row hv-kpis",
      hv_kpi("Harvest in view", paste(fmt_num(sum(by_d$value), 0), "t"), paste("NISR SAS,", span)),
      hv_kpi("Leading district", hi$district, paste(fmt_num(hi$value, 0), "t")),
      hv_kpi("Least district", lo$district, paste(fmt_num(lo$value, 0), "t")),
      naeb_kpi()
    )
  })

  output$ik_cold_title <- renderText({
    s <- hort_pick()
    if (s$fao) return(paste(s$product, "— FAOSTAT, national tonnes"))
    if (s$district != "All districts") return(paste(s$district, "— vegetables and fruits by year"))
    "District vegetables and fruits, metric tonnes"
  })

  output$ik_cold_plot <- renderPlotly({
    s <- hort_pick()
    if (s$fao) {
      d <- hort_fao[hort_fao$item == s$product & hort_fao$year >= s$yr[1] & hort_fao$year <= s$yr[2], , drop = FALSE]
      d <- d[order(d$year), , drop = FALSE]
      validate(need(nrow(d) > 0, "No FAOSTAT figure in this range."))
      p <- plotly::plot_ly(d, x = ~year, y = ~value, type = "scatter", mode = "lines+markers",
                           line = list(color = "#14804a", width = 3), marker = list(color = "#14804a", size = 8),
                           hovertemplate = "%{x}: %{y:,.0f} t<extra></extra>")
      return(hv_plot_layout(p, "Tonnes", "Year"))
    }
    d <- hort_sas()
    validate(need(!is.null(d) && nrow(d) && any(is.finite(d$value)), "No district vegetables or fruits in this range."))
    if (s$district != "All districts") {
      by_year <- stats::aggregate(cbind(vegetables, fruits) ~ year, d, hort_sum)
      by_year <- by_year[order(by_year$year), , drop = FALSE]
      p <- plotly::plot_ly(by_year, x = ~year, y = ~vegetables, type = "bar", name = "Vegetables", marker = list(color = "#14804a"))
      p <- plotly::add_trace(p, y = ~fruits, name = "Fruits", marker = list(color = "#1f5fd1"))
      p <- plotly::layout(p, barmode = "stack")
      return(hv_plot_layout(p, "Metric tonnes", "Year"))
    }
    by_d <- stats::aggregate(value ~ district, d, hort_sum)
    by_d <- by_d[is.finite(by_d$value), , drop = FALSE]
    by_d <- by_d[order(by_d$value), , drop = FALSE]
    p <- plotly::plot_ly(by_d, x = ~value, y = ~district, type = "bar", orientation = "h",
                         marker = list(color = "#14804a"),
                         hovertemplate = "%{y}: %{x:,.0f} t<extra></extra>")
    p <- hv_plot_layout(p, NULL, "Metric tonnes")
    plotly::layout(p, margin = list(l = 120, r = 24, t = 16, b = 48), yaxis = list(categoryorder = "total ascending", title = ""))
  })

  output$ik_cold_table <- renderUI({
    s <- hort_pick()
    naeb_block <- if (nrow(hort_naeb)) tags$div(
      class = "chart-card",
      tags$h3("Horticulture exports, national"),
      tags$table(
        class = "hv-table",
        tags$thead(tags$tr(tags$th("Fiscal year"), tags$th("Export tonnes"), tags$th("Export US$"))),
        tags$tbody(lapply(seq_len(nrow(hort_naeb)), function(i) tags$tr(
          tags$td(hort_naeb$fiscal_year[i]),
          tags$td(fmt_num(hort_naeb$export_t[i], 0)),
          tags$td(if (is.finite(hort_naeb$export_usd[i])) paste0("$", fmt_num(hort_naeb$export_usd[i], 0)) else "—")
        )))
      )
    ) else NULL
    if (s$fao) {
      d <- hort_fao[hort_fao$item == s$product & hort_fao$year >= s$yr[1] & hort_fao$year <= s$yr[2], , drop = FALSE]
      d <- d[order(-d$year), , drop = FALSE]
      return(tagList(
        if (nrow(d)) tags$div(
          class = "chart-card",
          tags$h3(s$product),
          tags$table(
            class = "hv-table",
            tags$thead(tags$tr(tags$th("Year"), tags$th("Tonnes"))),
            tags$tbody(lapply(seq_len(nrow(d)), function(i) tags$tr(tags$td(d$year[i]), tags$td(fmt_num(d$value[i], 0)))))
          )
        ),
        naeb_block
      ))
    }
    d <- hort_sas()
    if (is.null(d) || !nrow(d)) return(naeb_block)
    if (s$district != "All districts") {
      rows <- d[order(-d$year, d$season), , drop = FALSE]
      return(tagList(
        tags$div(
          class = "chart-card",
          tags$h3(s$district),
          tags$table(
            class = "hv-table",
            tags$thead(tags$tr(tags$th("Year"), tags$th("Season"), tags$th("Vegetables (t)"), tags$th("Fruits (t)"))),
            tags$tbody(lapply(seq_len(nrow(rows)), function(i) tags$tr(
              tags$td(rows$year[i]), tags$td(rows$season[i]),
              tags$td(fmt_num(rows$vegetables[i], 0)), tags$td(fmt_num(rows$fruits[i], 0))
            )))
          )
        ),
        naeb_block
      ))
    }
    wide <- stats::aggregate(cbind(vegetables, fruits) ~ district, d, hort_sum)
    wide$total <- ifelse(is.finite(wide$vegetables) | is.finite(wide$fruits), rowSums(cbind(wide$vegetables, wide$fruits), na.rm = TRUE), NA_real_)
    wide <- wide[order(-wide$total), , drop = FALSE]
    tot <- hort_sum(wide$total)
    tagList(
      tags$div(
        class = "chart-card",
        tags$h3("District harvest in this range"),
        tags$table(
          class = "hv-table",
          tags$thead(tags$tr(tags$th("District"), tags$th("Vegetables (t)"), tags$th("Fruits (t)"), tags$th("Together (t)"), tags$th("Share"))),
          tags$tbody(lapply(seq_len(nrow(wide)), function(i) tags$tr(
            tags$td(wide$district[i]),
            tags$td(fmt_num(wide$vegetables[i], 0)),
            tags$td(fmt_num(wide$fruits[i], 0)),
            tags$td(fmt_num(wide$total[i], 0)),
            tags$td(if (is.finite(wide$total[i]) && is.finite(tot) && tot > 0) fmt_pct(100 * wide$total[i] / tot, 1) else "—")
          )))
        )
      ),
      naeb_block
    )
  })

  output$ik_cold_note <- renderUI(tags$p(
    class = "note",
    "District vegetables and fruits are NISR SAS production in metric tonnes: 2019\u20132025 Seasons A and B, and 2026 Season A, from the tables whose maize total matches the national production line. NAEB horticulture exports are a national total for 2018/19 and 2023/24 (weight and earnings). FAOSTAT tonnes are national and are not added to the SAS tonnes. A cold-room temperature and a district export-storage capacity are not in these files."
  ))

  output$ik_cold_upload <- renderUI({
    v <- cold_log()
    if (is.null(v)) return(NULL)
    if (!is.null(v$error)) return(hv_empty(v$error))
    x <- v$d
    if (!nrow(x)) return(hv_empty("No lot in that file has a readable received date."))
    now <- Sys.time()
    end <- ifelse(is.na(x$dispatched), as.numeric(now), as.numeric(x$dispatched))
    x$hours <- (end - as.numeric(x$received)) / 3600
    x$flag_age <- x$hours > 48
    x$flag_temp <- is.finite(x$temp_c) & x$temp_c > 10
    x <- x[x$flag_age | x$flag_temp, , drop = FALSE]
    if (!nrow(x)) return(tags$p(class = "note", "No uploaded lot breaks the 48-hour or 10 \u00b0C rule."))
    x <- x[order(-x$hours), , drop = FALSE]
    tags$div(
      class = "chart-card",
      tags$h3("Uploaded lots past the store rule"),
      tags$table(
        class = "hv-table",
        tags$thead(tags$tr(tags$th("Lot"), tags$th("Product"), tags$th("Hours in store"), tags$th("Temp (\u00b0C)"), tags$th("Why"))),
        tags$tbody(lapply(seq_len(min(50, nrow(x))), function(i) tags$tr(
          tags$td(x$lot_id[i]), tags$td(x$product[i]), tags$td(fmt_num(x$hours[i], 0)), tags$td(fmt_num(x$temp_c[i], 1)),
          tags$td(paste(c(if (isTRUE(x$flag_age[i])) "Over 48 h without dispatch", if (isTRUE(x$flag_temp[i])) "Above 10 \u00b0C"), collapse = "; "))
        )))
      )
    )
  })
}
