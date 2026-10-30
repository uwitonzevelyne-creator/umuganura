# Umuganura — Umusaruro (Rwanda harvest)

hv_steps <- c("1" = "Profile", "2" = "Where it grows", "3" = "Trend analysis", "4" = "Risk mitigation", "5" = "Causal analytics")

hv_step_bar <- function(n) {
  tags$div(
    class = "hv-step-foot",
    if (n > 1) tags$button(type = "button", class = "btn-ghost", `data-step-go` = n - 1, paste("\u2190", hv_steps[[n - 1]])),
    tags$span(class = "hv-step-count", paste(n, "of", length(hv_steps))),
    if (n < length(hv_steps)) tags$button(type = "button", class = "btn-go", `data-step-go` = n + 1, paste(hv_steps[[n + 1]], "\u2192"))
  )
}

hero_banner <- function(img, title, line, kicker = NULL) {
  tags$div(
    class = "factor-hero",
    tags$img(src = img, alt = ""),
    tags$div(class = "factor-scrim"),
    tags$div(
      class = "factor-hero-copy",
      if (!is.null(kicker)) tags$p(class = "hero-kicker", kicker),
      tags$h1(title),
      tags$span(class = "factor-rule"),
      tags$p(line)
    )
  )
}

umusaruro_ui <- function() {
  cats <- harvest_cats[harvest_cats %in% crop_catalog$cat]
  tags$section(
    id = "umusaruro", class = "panel",
    tags$div(
      id = "harvest-land",
      hero_banner(
        "umusaruro/hero.jpg", "Rwanda harvest",
        "Umusaruro w'ubuhinzi: every crop and animal product Rwanda harvests, where it grows best, where it is thin, and what it takes from the soil.",
        "UMUSARURO"
      ),
      tags$div(
        class = "hv-page",
        tags$div(
          class = "hv-head",
          tags$div(
            tags$h2("Umusaruro w'ubuhinzi"),
            tags$p(paste0("Crops + livestock \u00b7 ", nrow(crop_catalog), " products in ", length(cats), " categories. Point at a picture and open its page."))
          ),
          tags$input(type = "search", id = "hv-search", class = "hv-search", placeholder = "Search products\u2026", `aria-label` = "Search products")
        ),
        tags$div(
          class = "hv-chips",
          tags$button(type = "button", class = "hv-chip is-on", `data-cat` = "all", "All"),
          lapply(cats, function(k) tags$button(type = "button", class = "hv-chip", `data-cat` = k, k))
        ),
        lapply(cats, function(k) {
          rows <- crop_catalog[crop_catalog$cat == k, , drop = FALSE]
          tags$div(
            class = "hv-cat", `data-cat` = k,
            tags$h3(k, tags$span(paste(nrow(rows), if (nrow(rows) == 1) "product" else "products"))),
            tags$div(
              class = "hv-grid",
              lapply(seq_len(nrow(rows)), function(i) {
                r <- rows[i, ]
                tags$button(
                  type = "button", class = "hv-card",
                  `data-crop` = r$id, `data-label` = r$label,
                  `data-name` = tolower(paste(r$en, r$rw, r$cat)),
                  tags$span(
                    class = "hv-img",
                    tags$img(
                      src = r$thumb,
                      alt = r$en,
                      loading = "lazy",
                      onerror = paste0("this.onerror=null;this.src='", r$src, "';")
                    ),
                    tags$span(class = "hv-visit", "VISIT PAGE")
                  ),
                  tags$span(class = "hv-name", r$en),
                  if (nzchar(r$rw)) tags$span(class = "hv-rw", r$rw),
                  tags$span(class = "hv-kind", if (r$kind == "crop") "Crop" else "Livestock")
                )
              })
            )
          )
        })
      )
    ),
    tags$div(
      id = "harvest-room", class = "hv-room is-hidden",
      tags$div(
        class = "room-bar",
        tags$button(type = "button", class = "room-back", `data-hv-back` = "1", "\u2190 All products"),
        tags$span(id = "hv-pill", class = "room-pill", "Maize"),
        tags$span(class = "room-title", "Umuganura \u00b7 Umusaruro")
      ),
      tags$nav(
        class = "hv-steps",
        lapply(names(hv_steps), function(n) {
          tags$button(type = "button", class = paste("hv-step-btn", if (n == "1") "is-on"), `data-step` = n,
                      tags$b(n), hv_steps[[n]])
        })
      ),
      tags$div(
        class = "hv-step is-on", `data-step` = "1",
        uiOutput("hv_profile"),
        hv_step_bar(1)
      ),
      tags$div(
        class = "hv-step", `data-step` = "2",
        tags$div(
          class = "hv-step-head",
          tags$p(class = "kicker", "INTERACTIVE MAP INFOGRAPHIC"),
          tags$h2("Where it grows, and where it is thin"),
          tags$p("Darker districts carry more of the national harvest. Pictures mark the strongest and weakest districts with their share. Zoom with + and \u2212 or the mouse wheel.")
        ),
        tags$div(
          class = "hv-map-grid",
          tags$aside(class = "hv-side", uiOutput("hv_map_left")),
          tags$div(class = "chart-card map-card", if (has_leaflet) leaflet::leafletOutput("hv_map", height = "560px")),
          tags$aside(class = "hv-side", uiOutput("hv_map_right"))
        ),
        hv_step_bar(2)
      ),
      tags$div(
        class = "hv-step", `data-step` = "3",
        tags$div(
          class = "hv-step-head",
          tags$p(class = "kicker", "TREND ANALYSIS"),
          tags$h2("Yield intensity (kg/ha) by district and season"),
          uiOutput("hv_trend_lede")
        ),
        tags$div(
          class = "price-filters hv-filters",
          selectizeInput("hv_tr_district", "DISTRICTS", choices = c("National", names(province_of)), selected = "National",
                         multiple = TRUE, width = "100%", options = list(plugins = list("remove_button"), maxItems = 8)),
          checkboxGroupInput("hv_tr_season", "SEASON", choices = c("Season A" = "A", "Season B" = "B", "Season C" = "C"),
                             selected = c("A", "B"), inline = TRUE),
          sliderInput("hv_tr_year", "YEARS", min = 1961, max = 2026, value = c(1961, 2026), sep = "", width = "100%"),
          selectInput("hv_tr_chart", "CHART", choices = c("Multi-line chart" = "line", "Bar chart" = "bar"), selected = "line", width = "100%")
        ),
        tags$div(class = "chart-card", plotlyOutput("hv_trend", height = "460px")),
        uiOutput("hv_trend_note"),
        hv_step_bar(3)
      ),
      tags$div(
        class = "hv-step", `data-step` = "4",
        tags$div(
          class = "hv-step-head",
          tags$p(class = "kicker", "RISK MITIGATION"),
          tags$h2("Which districts swing most, and whose soil is slipping"),
          tags$p("High-risk districts surface first: the widest swings in yield from season to season, and soil that erodes faster than the rest.")
        ),
        tags$div(
          class = "price-filters hv-filters",
          selectInput("hv_risk_measure", "MEASURE", choices = c("Standard deviation (\u03c3)" = "sd", "Soil degradation outlier flags" = "soil"), selected = "sd", width = "100%"),
          selectInput("hv_risk_visual", "VISUAL", choices = c("Box and whisker plot" = "box", "Coloured geo-heatmap" = "map"), selected = "box", width = "100%"),
          selectInput("hv_risk_season", "SEASON", choices = c("Season A" = "A", "Season B" = "B"), selected = "A", width = "100%")
        ),
        uiOutput("hv_risk_cards"),
        conditionalPanel("input.hv_risk_visual == 'box'", tags$div(class = "chart-card", plotlyOutput("hv_risk_box", height = "480px"))),
        conditionalPanel("input.hv_risk_visual == 'map'", tags$div(class = "chart-card map-card", if (has_leaflet) leaflet::leafletOutput("hv_risk_map", height = "520px"))),
        uiOutput("hv_risk_note"),
        hv_step_bar(4)
      ),
      tags$div(
        class = "hv-step", `data-step` = "5",
        tags$div(
          class = "hv-step-head",
          tags$p(class = "kicker", "CAUSAL ANALYTICS"),
          tags$h2("What moves the yield, and what it costs the soil"),
          tags$p("A multiple linear regression on the national series: yield against fertilizer, rainfall and crop rotation, year by year.")
        ),
        tags$div(
          class = "price-filters hv-filters",
          selectInput("hv_cause_model", "MODEL", choices = c("Multiple linear regression" = "mlr"), selected = "mlr", width = "100%"),
          selectInput("hv_cause_driver", "YIELD AGAINST", choices = c("Fertilizer" = "fert", "Rainfall" = "rain", "Crop rotation" = "rot"), selected = "fert", width = "100%"),
          selectInput("hv_cause_visual", "VISUAL", choices = c("Dual-axis chart" = "dual", "Scatter plot matrix" = "splom"), selected = "dual", width = "100%")
        ),
        uiOutput("hv_cause_cards"),
        tags$div(class = "chart-card", plotlyOutput("hv_cause_plot", height = "480px")),
        uiOutput("hv_cause_table"),
        hv_step_bar(5)
      )
    ),
    site_footer()
  )
}

# ---- server helpers ----------------------------------------------------------

hv_base_map <- function() {
  leaflet::leaflet(options = leaflet::leafletOptions(scrollWheelZoom = TRUE, minZoom = 7, maxZoom = 13)) |>
    leaflet::addTiles(options = leaflet::tileOptions(opacity = 0.45)) |>
    leaflet::setView(29.9, -1.95, 8)
}

hv_fill <- function(map, values, pal, label) {
  for (d in names(district_lnglat)) {
    g <- district_lnglat[[d]]
    v <- unname(values[d])
    ok <- length(v) == 1 && is.finite(v)
    map <- leaflet::addPolygons(
      map, lng = g$lng, lat = g$lat, layerId = d,
      fillColor = if (ok) pal(v) else "#eef1ea", fillOpacity = 0.82,
      color = "#ffffff", weight = 1.2, label = htmltools::HTML(label(d, if (ok) v else NA)),
      highlightOptions = leaflet::highlightOptions(weight = 3, color = "#1b4332", bringToFront = TRUE)
    )
  }
  map
}

hv_pins <- function(map, districts, icon_url, tags_txt, cls = "crop-pin") {
  if (!length(districts)) return(map)
  xy <- district_lnglat[districts]
  lng <- vapply(xy, `[[`, numeric(1), "cx")
  lat <- vapply(xy, `[[`, numeric(1), "cy")
  map |>
    leaflet::addMarkers(lng = lng, lat = lat, icon = leaflet::makeIcon(iconUrl = icon_url, iconWidth = 34, iconHeight = 34, className = cls)) |>
    leaflet::addLabelOnlyMarkers(lng = lng, lat = lat, label = tags_txt,
                                 labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "bottom", offset = c(0, 14), className = paste("pin-tag", cls)))
}

hv_plot_layout <- function(p, ytitle = NULL, xtitle = NULL) {
  p |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Outfit, Segoe UI, sans-serif", size = 12, color = "#1a1a1a"),
      margin = list(l = 64, r = 24, t = 24, b = 56),
      xaxis = list(title = xtitle, gridcolor = "rgba(27,67,50,0.08)"),
      yaxis = list(title = ytitle, gridcolor = "rgba(27,67,50,0.08)"),
      legend = list(orientation = "h", y = -0.18)
    ) |>
    plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
}

hv_empty <- function(msg) tags$div(class = "empty-note", msg)

hv_kpi <- function(label, value, note = NULL, tone = NULL) {
  tags$article(class = paste("kpi", tone), tags$span(label), tags$strong(value), if (!is.null(note)) tags$p(note))
}

# ---- server ------------------------------------------------------------------

umusaruro_server <- function(input, output, session) {
  hv_id <- reactive({
    p <- input$hv_crop
    id <- if (is.list(p)) p$id else p
    if (is.null(id) || !id %in% crop_catalog$id) "maize" else id
  })
  hv_r <- reactive(crop_row(hv_id()))

  hv_share <- reactive({
    r <- hv_r()
    if (is.na(r$sas)) return(NULL)
    d <- sas_prod[sas_prod$crop == r$sas & sas_prod$district != "Rwanda" & is.finite(sas_prod$value), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    tot <- stats::aggregate(value ~ year + season, d, sum)
    tot <- tot[tot$value > 0, , drop = FALSE]
    if (!nrow(tot)) return(NULL)
    tot <- tot[order(-tot$year, tot$season), , drop = FALSE]
    y <- tot$year[1]
    s <- tot$season[1]
    d <- d[d$year == y & d$season == s, , drop = FALSE]
    d$share <- 100 * d$value / sum(d$value)
    list(d = d[order(-d$share), , drop = FALSE], year = y, season = s)
  })

  observeEvent(hv_id(), {
    r <- hv_r()
    ch <- c("National", names(province_of))
    sel <- "National"
    sh <- hv_share()
    if (!is.null(sh) && r$kind == "crop") sel <- c("National", utils::head(sh$d$district, 2))
    updateSelectizeInput(session, "hv_tr_district", choices = ch, selected = sel)
  })

  # ---- 1. profile ----
  output$hv_profile <- renderUI({
    r <- hv_r()
    y <- crop_yield_nat(r$id)
    y <- y[is.finite(y$value), , drop = FALSE]
    unit <- crop_unit(r$id)
    last <- if (nrow(y)) y[nrow(y), ] else NULL
    ten <- if (nrow(y) > 10) y[y$year == last$year - 10, , drop = FALSE] else NULL
    chg <- if (!is.null(ten) && nrow(ten) && ten$value > 0) 100 * (last$value / ten$value - 1) else NA
    sw <- soil_wear(r$id)
    sas_last <- if (!is.na(r$sas)) {
      s <- sas_yield[sas_yield$crop == r$sas & sas_yield$district == "Rwanda", , drop = FALSE]
      s <- s[order(s$year, s$season), , drop = FALSE]
      if (nrow(s)) s[nrow(s), ] else NULL
    } else NULL
    methods <- strsplit(r$methods, "|", fixed = TRUE)[[1]]
    tags$div(
      class = "hv-profile",
      tags$div(
        class = "hv-photo",
        tags$img(
          src = r$thumb,
          alt = r$en,
          onerror = paste0("this.onerror=null;this.src='", r$src, "';")
        )
      ),
      tags$div(
        class = "hv-facts",
        tags$p(class = "kicker", toupper(r$cat)),
        tags$h2(r$en, if (nzchar(r$rw)) tags$span(class = "hv-rw-big", paste0(" / ", r$rw))),
        tags$dl(
          tags$dt("Origin"), tags$dd(r$origin),
          tags$dt("Arrived in Rwanda"), tags$dd(r$arrived),
          tags$dt("Lifecycle"), tags$dd(r$cycle),
          tags$dt(if (r$kind == "crop") "Planting methods" else "Rearing methods"),
          tags$dd(tags$ul(lapply(methods, tags$li)))
        ),
        tags$div(
          class = "kpi-row hv-kpis",
          hv_kpi(if (r$kind == "crop") "National yield" else "Yield per animal",
                 if (!is.null(last)) paste(fmt_num(last$value, 0), unit) else "\u2014",
                 if (!is.null(last)) paste("FAOSTAT", last$year) else "No FAO series"),
          hv_kpi("Ten-year change", if (is.finite(chg)) paste0(if (chg >= 0) "+" else "", fmt_num(chg, 0), "%") else "\u2014",
                 if (is.finite(chg)) paste(last$year - 10, "to", last$year) else NULL,
                 if (is.finite(chg) && chg < 0) "is-warn" else NULL),
          if (!is.null(sas_last)) hv_kpi("Latest season", paste(fmt_num(sas_last$value, 0), "kg/ha"), paste0("NISR SAS ", sas_last$year, " Season ", sas_last$season))
        ),
        tags$h3(class = "hv-sub", "Soil wear indices"),
        if (is.null(sw)) {
          tags$p(class = "note", if (r$kind == "livestock") "Livestock does not draw nutrients from cropland directly. Manure from stalls returns nitrogen and phosphorus to the fields." else "No national yield series to estimate nutrient removal.")
        } else {
          tags$div(
            class = "kpi-row hv-kpis",
            hv_kpi("Nutrients removed", paste(fmt_num(sw$removed, 0), "kg/ha"), paste0("N + P\u2082O\u2085 + K\u2082O in the ", sw$year, " harvest")),
            hv_kpi("Fertilizer put back", paste(fmt_num(sw$fert, 0), "kg/ha"), paste("World Bank", sw$fert_year, ", all crops")),
            hv_kpi("Wear index", if (is.finite(sw$index)) paste0(fmt_num(sw$index, 1), "\u00d7") else "\u2014",
                   if (is.finite(sw$index)) (if (sw$index > 1) "Harvest takes more than fertilizer returns" else "Fertilizer covers the harvest") else NULL,
                   if (is.finite(sw$index) && sw$index > 1) "is-warn" else NULL),
            hv_kpi("Eroding plots", if (is.finite(sw$erosion)) paste0(fmt_num(sw$erosion, 1), "%") else "\u2014", "Severe or moderate erosion, SAS 2026 A")
          )
        },
        if (!is.null(sw) && sw$legume) tags$p(class = "note", "A legume: root nodules fix much of its own nitrogen, so the true drain on the soil is lower than the index shows."),
        tags$p(class = "note", "Nutrients removed = FAO yield \u00d7 typical nutrient content per tonne of harvest. An estimate to compare crops, not a soil test.")
      )
    )
  })

  # ---- 2. map ----
  output$hv_map <- leaflet::renderLeaflet({
    r <- hv_r()
    sh <- hv_share()
    m <- hv_base_map()
    if (is.null(sh)) {
      return(hv_fill(m, numeric(), function(v) "#eef1ea", function(d, v) paste0("<b>", d, "</b><br>No district figure")))
    }
    vals <- stats::setNames(sh$d$share, sh$d$district)
    pal <- leaflet::colorNumeric(c("#f1f6ea", "#b7e4c7", "#52b788", "#2d6a4f", "#1b4332"), domain = range(vals, na.rm = TRUE))
    m <- hv_fill(m, vals, pal, function(d, v) {
      if (!is.finite(v)) return(paste0("<b>", d, "</b><br>No harvest reported"))
      mt <- sh$d$value[sh$d$district == d][1]
      paste0("<b>", d, "</b><br>", fmt_num(mt, 0), " MT \u00b7 ", fmt_num(v, 1), "% of Rwanda")
    })
    pos <- sh$d[sh$d$value > 0, , drop = FALSE]
    top <- utils::head(pos, 3)
    low <- utils::tail(pos, 3)
    low <- low[!low$district %in% top$district, , drop = FALSE]
    pin <- sub("\\.jpg$", "-pin.jpg", r$thumb)
    m <- hv_pins(m, top$district, pin, paste0(top$district, " ", fmt_num(top$share, 1), "%"), "crop-pin")
    m <- hv_pins(m, low$district, pin, paste0(low$district, " ", fmt_num(low$share, 1), "%"), "crop-pin is-low")
    leaflet::addLegend(m, "bottomright", pal = pal, values = vals, title = "% of national<br>harvest", labFormat = leaflet::labelFormat(suffix = "%"), opacity = 0.9)
  })

  output$hv_map_left <- renderUI({
    r <- hv_r()
    sh <- hv_share()
    if (is.null(sh)) {
      return(tags$div(
        class = "hv-side-card",
        tags$h4("No district map"),
        tags$p(if (r$kind == "livestock") paste("NISR SAS covers crops only. District figures for", tolower(r$en), "are not in our tables.") else paste("NISR SAS does not publish", tolower(r$en), "by district.")),
        tags$p(class = "note", "The trend page shows the national FAO series instead.")
      ))
    }
    top <- utils::head(sh$d, 5)
    agg_note <- if (r$sas %in% c("Vegetables", "Fruits")) tags$p(class = "note", paste0("SAS reports all ", tolower(r$sas), " together. This map is the ", tolower(r$sas), " group, not ", tolower(r$en), " alone.")) else NULL
    tags$div(
      class = "hv-side-card",
      tags$h4(tags$img(src = sub("\\.jpg$", "-pin.jpg", r$thumb), alt = ""), "Concentrated here"),
      lapply(seq_len(nrow(top)), function(i) tags$div(
        class = "hv-bar-row",
        tags$span(top$district[i]),
        tags$div(class = "hv-bar", tags$i(style = sprintf("width:%.0f%%", 100 * top$share[i] / max(sh$d$share)))),
        tags$b(paste0(fmt_num(top$share[i], 1), "%"))
      )),
      tags$p(class = "note", paste0("Top five hold ", fmt_num(sum(top$share), 0), "% of the ", sh$year, " Season ", sh$season, " harvest (", fmt_num(sum(sh$d$value), 0), " MT).")),
      agg_note
    )
  })

  output$hv_map_right <- renderUI({
    r <- hv_r()
    sh <- hv_share()
    life <- tags$div(class = "hv-side-card", tags$h4("Lifecycle"), tags$p(r$cycle))
    if (is.null(sh)) return(life)
    pos <- sh$d[sh$d$value > 0, , drop = FALSE]
    low <- utils::tail(pos, 5)
    none <- setdiff(names(province_of), sh$d$district[sh$d$value > 0])
    tags$div(
      tags$div(
        class = "hv-side-card is-low",
        tags$h4("Thin here"),
        lapply(rev(seq_len(nrow(low))), function(i) tags$div(
          class = "hv-bar-row",
          tags$span(low$district[i]),
          tags$div(class = "hv-bar", tags$i(style = sprintf("width:%.0f%%", max(2, 100 * low$share[i] / max(sh$d$share))))),
          tags$b(paste0(fmt_num(low$share[i], 2), "%"))
        )),
        if (length(none)) tags$p(class = "note", paste0("No harvest reported in ", length(none), " district", if (length(none) > 1) "s", ": ", paste(utils::head(none, 6), collapse = ", "), if (length(none) > 6) "\u2026", "."))
      ),
      life
    )
  })

  # ---- 3. trend ----
  output$hv_trend_lede <- renderUI({
    r <- hv_r()
    tags$p(if (r$kind == "livestock") paste("Livestock yields are per animal and national only (FAOSTAT). Districts and seasons do not apply to", tolower(r$en), ".")
           else if (is.na(r$sas)) paste("SAS does not publish", tolower(r$en), "by district, so this is the national FAO series.")
           else "Pick districts and seasons. National is the FAO annual line plus the SAS national figure for each season.")
  })

  hv_trend_data <- reactive({
    r <- hv_r()
    yr <- input$hv_tr_year
    if (is.null(yr)) yr <- c(1961, 2026)
    seas <- input$hv_tr_season
    ds <- input$hv_tr_district
    if (is.null(ds) || !length(ds)) ds <- "National"
    out <- list()
    if ("National" %in% ds || is.na(r$sas)) {
      f <- crop_yield_nat(r$id)
      f <- f[is.finite(f$value) & f$year >= yr[1] & f$year <= yr[2], , drop = FALSE]
      if (nrow(f)) out$fao <- data.frame(series = "Rwanda \u00b7 FAO annual", year = f$year, x = f$year, value = f$value, lab = as.character(f$year), stringsAsFactors = FALSE)
    }
    if (!is.na(r$sas) && length(seas)) {
      pick <- c(setdiff(ds, "National"), if ("National" %in% ds) "Rwanda")
      dd <- sas_yield[sas_yield$crop == r$sas & sas_yield$season %in% seas & sas_yield$district %in% pick &
                        sas_yield$year >= yr[1] & sas_yield$year <= yr[2], , drop = FALSE]
      if (nrow(dd)) {
        off <- c(A = 0, B = 0.45, C = 0.75)
        out$sas <- data.frame(
          series = paste0(ifelse(dd$district == "Rwanda", "Rwanda (SAS)", dd$district), " \u00b7 Season ", dd$season),
          year = dd$year, x = dd$year + unname(off[dd$season]), value = dd$value,
          lab = paste(dd$year, dd$season), stringsAsFactors = FALSE
        )
      }
    }
    if (!length(out)) return(NULL)
    d <- do.call(rbind, out)
    d[order(d$series, d$x), , drop = FALSE]
  })

  output$hv_trend <- renderPlotly({
    d <- hv_trend_data()
    r <- hv_r()
    unit <- crop_unit(r$id)
    if (is.null(d)) return(hv_plot_layout(plotly::plot_ly(type = "scatter", mode = "markers") |> plotly::layout(annotations = list(list(text = "No figures for this choice", showarrow = FALSE, x = 0.5, y = 0.5, xref = "paper", yref = "paper")))))
    pal <- c("#1b4332", "#c6a336", "#40916c", "#9b2226", "#6f4e37", "#52b788", "#3a86ff", "#e76f51", "#8d99ae", "#2a9d8f")
    if (identical(input$hv_tr_chart, "bar")) {
      d$lab <- factor(d$lab, levels = unique(d$lab[order(d$x)]))
      p <- plotly::plot_ly(d, x = ~lab, y = ~value, color = ~series, colors = pal, type = "bar",
                           hovertemplate = paste0("%{fullData.name}<br>%{x}: %{y:,.0f} ", unit, "<extra></extra>")) |>
        plotly::layout(barmode = "group")
    } else {
      p <- plotly::plot_ly(d, x = ~x, y = ~value, color = ~series, colors = pal, type = "scatter", mode = "lines+markers",
                           text = ~lab, hovertemplate = paste0("%{fullData.name}<br>%{text}: %{y:,.0f} ", unit, "<extra></extra>"))
    }
    hv_plot_layout(p, paste0("Yield (", unit, ")"))
  })

  output$hv_trend_note <- renderUI({
    r <- hv_r()
    seas <- input$hv_tr_season
    notes <- list()
    if ("C" %in% seas) notes[[length(notes) + 1]] <- "Season C (the short marshland season) is not published by district, so it has no line."
    if (!is.na(r$sas)) notes[[length(notes) + 1]] <- "District seasons come from NISR SAS 2019\u20132026 (2021 Season A and 2026 Season B are not in our tables). FAO annual covers 1961\u20132024."
    if (!length(notes)) return(NULL)
    tags$p(class = "note", paste(unlist(notes), collapse = " "))
  })

  # ---- 4. risk ----
  hv_risk_tbl <- reactive({
    r <- hv_r()
    if (is.na(r$sas)) return(NULL)
    s <- input$hv_risk_season
    if (is.null(s)) s <- "A"
    d <- sas_yield[sas_yield$crop == r$sas & sas_yield$season == s & sas_yield$district != "Rwanda", , drop = FALSE]
    if (!nrow(d)) return(NULL)
    agg <- do.call(rbind, lapply(split(d, d$district), function(x) {
      sl <- if (nrow(x) >= 3) unname(stats::coef(stats::lm(value ~ year, x))[2]) else NA_real_
      data.frame(district = x$district[1], n = nrow(x), mean = mean(x$value),
                 sd = if (nrow(x) > 1) stats::sd(x$value) else NA_real_, slope = sl, stringsAsFactors = FALSE)
    }))
    agg$cv <- 100 * agg$sd / agg$mean
    er <- farm_practice$erosion
    ld <- farm_practice$land
    if (!is.null(er)) agg <- merge(agg, data.frame(district = er$district, erosion = er$severe + er$moderate), by = "district", all.x = TRUE) else agg$erosion <- NA_real_
    if (!is.null(ld)) agg <- merge(agg, data.frame(district = ld$district, protect = ld$erosion), by = "district", all.x = TRUE) else agg$protect <- NA_real_
    q <- stats::quantile(agg$erosion, c(0.25, 0.75), na.rm = TRUE)
    fence <- q[[2]] + 1.5 * (q[[2]] - q[[1]])
    z <- (agg$erosion - mean(agg$erosion, na.rm = TRUE)) / stats::sd(agg$erosion, na.rm = TRUE)
    agg$flag_er <- is.finite(agg$erosion) & (agg$erosion > fence | z > 1.5)
    agg$flag_decline <- is.finite(agg$slope) & agg$slope < 0 & is.finite(agg$erosion) & agg$erosion > stats::median(agg$erosion, na.rm = TRUE)
    agg$flag <- agg$flag_er | agg$flag_decline
    ok <- is.finite(agg$sd)
    cut <- if (sum(ok)) sort(agg$sd[ok], decreasing = TRUE)[min(5, sum(ok))] else Inf
    agg$sd_high <- ok & agg$sd >= cut
    list(agg = agg[order(-agg$sd), , drop = FALSE], d = d, season = s)
  })

  output$hv_risk_cards <- renderUI({
    r <- hv_r()
    t <- hv_risk_tbl()
    if (is.null(t)) return(hv_empty(if (r$kind == "livestock") "Risk by district needs district yields. SAS covers crops only." else paste("SAS does not publish", tolower(r$en), "by district, so district risk cannot be measured.")))
    a <- t$agg
    if (identical(input$hv_risk_measure, "soil")) {
      f <- a[a$flag, , drop = FALSE]
      top <- a[which.max(a$erosion), ]
      tags$div(
        class = "kpi-row hv-kpis",
        hv_kpi("Flagged districts", nrow(f), if (nrow(f)) paste(utils::head(f$district, 4), collapse = ", ") else "None stand out", if (nrow(f)) "is-warn" else NULL),
        hv_kpi("Most eroded", top$district, paste0(fmt_num(top$erosion, 1), "% of plots severe or moderate")),
        hv_kpi("Median erosion", paste0(fmt_num(stats::median(a$erosion, na.rm = TRUE), 1), "%"), "Across districts, SAS 2026 A"),
        hv_kpi("Yield falling", sum(is.finite(a$slope) & a$slope < 0), paste("districts with a downward Season", t$season, "trend"))
      )
    } else {
      hi <- a[1, ]
      lo <- a[which.min(a$sd), ]
      nat <- sas_yield[sas_yield$crop == r$sas & sas_yield$season == t$season & sas_yield$district == "Rwanda", "value"]
      tags$div(
        class = "kpi-row hv-kpis",
        hv_kpi("Most volatile", hi$district, paste0("\u03c3 = ", fmt_num(hi$sd, 0), " kg/ha (CV ", fmt_num(hi$cv, 0), "%)"), "is-warn"),
        hv_kpi("Most stable", lo$district, paste0("\u03c3 = ", fmt_num(lo$sd, 0), " kg/ha")),
        hv_kpi("National \u03c3", if (length(nat) > 1) paste(fmt_num(stats::sd(nat), 0), "kg/ha") else "\u2014", paste("Season", t$season, "across", length(nat), "years")),
        hv_kpi("High-risk districts", sum(a$sd_high), "The five widest swings")
      )
    }
  })

  output$hv_risk_box <- renderPlotly({
    t <- hv_risk_tbl()
    req(t)
    a <- t$agg
    if (identical(input$hv_risk_measure, "soil")) {
      a <- a[is.finite(a$erosion), , drop = FALSE]
      a$grp <- ifelse(a$flag, "Flagged", "Within range")
      p <- plotly::plot_ly() |>
        plotly::add_boxplot(y = a$erosion, name = "All districts", boxpoints = FALSE, marker = list(color = "#95d5b2"), line = list(color = "#2d6a4f"), x0 = "Severe + moderate erosion") |>
        plotly::add_markers(data = a, x = "Severe + moderate erosion", y = ~erosion, color = ~grp, colors = c("Flagged" = "#9b2226", "Within range" = "#40916c"),
                            text = ~paste0(district, "<br>Erosion ", round(erosion, 1), "% of plots<br>Protected land ", round(protect, 0), "% of farmers<br>Yield trend ", ifelse(is.finite(slope), paste0(round(slope), " kg/ha per year"), "\u2014")),
                            hoverinfo = "text", marker = list(size = 11, opacity = 0.85))
      f <- a[a$flag, , drop = FALSE]
      if (nrow(f)) p <- plotly::layout(p, annotations = lapply(seq_len(nrow(f)), function(i) list(x = "Severe + moderate erosion", y = f$erosion[i], text = f$district[i], xanchor = "left", xshift = 14, showarrow = FALSE, font = list(color = "#9b2226"))))
      return(hv_plot_layout(p, "% of plots with severe or moderate erosion"))
    }
    d <- t$d
    ord <- a$district
    d$district <- factor(d$district, levels = ord)
    hi <- a$district[a$sd_high]
    d$grp <- ifelse(d$district %in% hi, "High risk (top 5 \u03c3)", "Other districts")
    p <- plotly::plot_ly(d, x = ~district, y = ~value, color = ~grp, colors = c("High risk (top 5 \u03c3)" = "#9b2226", "Other districts" = "#40916c"),
                         type = "box", boxpoints = "all", jitter = 0.3, pointpos = 0,
                         text = ~paste(year, "Season", season), hovertemplate = "%{x}<br>%{text}: %{y:,.0f} kg/ha<extra></extra>") |>
      plotly::layout(xaxis = list(categoryorder = "array", categoryarray = ord, tickangle = -45))
    hv_plot_layout(p, paste("Season", t$season, "yield (kg/ha)"))
  })

  output$hv_risk_map <- leaflet::renderLeaflet({
    t <- hv_risk_tbl()
    req(t)
    a <- t$agg
    soil <- identical(input$hv_risk_measure, "soil")
    v <- if (soil) stats::setNames(a$erosion, a$district) else stats::setNames(a$sd, a$district)
    pal <- leaflet::colorNumeric(c("#fff3c4", "#f4a259", "#e76f51", "#9b2226"), domain = range(v, na.rm = TRUE))
    m <- hv_fill(hv_base_map(), v, pal, function(d, x) {
      row <- a[a$district == d, , drop = FALSE]
      if (!nrow(row)) return(paste0("<b>", d, "</b><br>No figure"))
      paste0("<b>", d, "</b><br>\u03c3 ", fmt_num(row$sd, 0), " kg/ha \u00b7 CV ", fmt_num(row$cv, 0), "%<br>Erosion ", fmt_num(row$erosion, 1), "% of plots",
             if (row$flag) "<br><b style='color:#9b2226'>Soil flag</b>" else "")
    })
    hot <- if (soil) a$district[a$flag] else a$district[a$sd_high]
    tag <- if (soil) paste0(hot, " ", fmt_num(a$erosion[match(hot, a$district)], 0), "%") else paste0(hot, " \u03c3", fmt_num(a$sd[match(hot, a$district)], 0))
    m <- hv_pins(m, hot, "umusaruro/warn.svg", tag, "warn-pin")
    leaflet::addLegend(m, "bottomright", pal = pal, values = v, title = if (soil) "Eroding plots (%)" else "\u03c3 yield (kg/ha)", opacity = 0.9)
  })

  output$hv_risk_note <- renderUI({
    if (is.null(hv_risk_tbl())) return(NULL)
    tags$p(class = "note", if (identical(input$hv_risk_measure, "soil"))
      "A soil flag means the share of severely or moderately eroding plots is a statistical outlier (above the upper Tukey fence or more than 1.5 standard deviations above the mean), or yield is trending down where erosion is above the median. Erosion is SAS 2026 Season A; yields are SAS 2019\u20132026."
    else "\u03c3 is the standard deviation of a district's yield across the published years for the chosen season. CV is \u03c3 as a share of the mean. Wider boxes mean a less dependable harvest.")
  })

  # ---- 5. causal ----
  hv_cause_df <- reactive({
    r <- hv_r()
    y <- crop_yield_nat(r$id)
    if (!nrow(y)) return(NULL)
    y <- data.frame(year = y$year, yield = y$value)
    f <- data.frame(year = fert$year, fert = fert$value)
    rn <- harvest_data$rain
    names(rn) <- c("year", "rain")
    rt <- harvest_data$diversity
    rt <- data.frame(year = rt$year, rot = 100 * rt$value)
    m <- Reduce(function(a, b) merge(a, b, by = "year"), list(y, f, rn, rt))
    m <- m[stats::complete.cases(m), , drop = FALSE]
    if (nrow(m) < 8) return(NULL)
    m
  })

  hv_fit <- reactive({
    m <- hv_cause_df()
    if (is.null(m)) return(NULL)
    fit <- stats::lm(yield ~ fert + rain + rot, data = m)
    s <- summary(fit)$coefficients
    k <- c("fert", "rain", "rot")
    co <- data.frame(term = k, est = s[k, 1], se = s[k, 2], p = s[k, 4], stringsAsFactors = FALSE)
    co$beta <- co$est * vapply(k, function(v) stats::sd(m[[v]]), numeric(1)) / stats::sd(m$yield)
    list(fit = fit, co = co, r2 = summary(fit)$r.squared, n = nrow(m), years = range(m$year), m = m)
  })

  drv_lab <- c(fert = "Fertilizer (kg/ha)", rain = "Rainfall (mm/year)", rot = "Crop rotation (diversity index)")

  output$hv_cause_cards <- renderUI({
    r <- hv_r()
    if (r$kind == "livestock") return(hv_empty("The causal model explains crop yield per hectare. It does not apply to livestock."))
    f <- hv_fit()
    if (is.null(f)) return(hv_empty(paste("Too few years where", tolower(r$en), "yield, fertilizer, rainfall and rotation all exist to fit a model.")))
    co <- f$co
    sig <- co[co$p < 0.1, , drop = FALSE]
    best <- if (nrow(sig)) sig[which.max(abs(sig$beta)), ] else NULL
    fe <- co[co$term == "fert", ]
    sw <- soil_wear(r$id)
    tags$div(
      class = "kpi-row hv-kpis",
      hv_kpi("Strongest driver", if (!is.null(best)) sub(" \\(.*", "", drv_lab[[best$term]]) else "None clear",
             if (!is.null(best)) paste0("Standardised effect ", fmt_num(best$beta, 2), ", p = ", formatC(best$p, format = "f", digits = 3)) else "No driver passes p < 0.10"),
      hv_kpi("Fit (R\u00b2)", fmt_num(f$r2, 2), paste0(f$n, " years, ", f$years[1], "\u2013", f$years[2])),
      hv_kpi("Return on fertilizer", paste(if (fe$est >= 0) "+" else "", fmt_num(fe$est, 1), "kg"), "Yield per extra kg/ha of fertilizer, other things equal",
             if (fe$est < 0) "is-warn" else NULL),
      hv_kpi("Soil balance", if (!is.null(sw)) paste0(if (sw$fert - sw$removed >= 0) "+" else "", fmt_num(sw$fert - sw$removed, 0), " kg/ha") else "\u2014",
             "Fertilizer applied minus nutrients removed", if (!is.null(sw) && sw$fert < sw$removed) "is-warn" else NULL)
    )
  })

  output$hv_cause_plot <- renderPlotly({
    r <- hv_r()
    req(r$kind == "crop")
    f <- hv_fit()
    req(f)
    m <- f$m
    if (identical(input$hv_cause_visual, "splom")) {
      p <- plotly::plot_ly(m, type = "splom",
                           dimensions = list(list(label = "Yield", values = ~yield), list(label = "Fertilizer", values = ~fert),
                                             list(label = "Rainfall", values = ~rain), list(label = "Rotation", values = ~rot)),
                           text = ~year, marker = list(color = ~year, colorscale = list(c(0, "#b7e4c7"), c(1, "#1b4332")), size = 8, line = list(color = "#fff", width = 0.5)),
                           diagonal = list(visible = FALSE))
      return(hv_plot_layout(p) |> plotly::layout(margin = list(l = 70, r = 20, t = 20, b = 60)))
    }
    drv <- input$hv_cause_driver
    if (is.null(drv)) drv <- "fert"
    p <- plotly::plot_ly(m) |>
      plotly::add_bars(x = ~year, y = ~yield, name = paste0("Yield (", crop_unit(r$id), ")"), marker = list(color = "rgba(64,145,108,0.75)"),
                       hovertemplate = "%{x}: %{y:,.0f} kg/ha<extra>Yield</extra>") |>
      plotly::add_lines(x = ~year, y = m[[drv]], name = drv_lab[[drv]], yaxis = "y2", line = list(color = "#c6a336", width = 3),
                        hovertemplate = paste0("%{x}: %{y:,.1f}<extra>", drv_lab[[drv]], "</extra>"))
    hv_plot_layout(p, "Yield (kg/ha)") |>
      plotly::layout(yaxis2 = list(title = drv_lab[[drv]], overlaying = "y", side = "right", showgrid = FALSE), margin = list(r = 70))
  })

  output$hv_cause_table <- renderUI({
    r <- hv_r()
    if (r$kind == "livestock") return(NULL)
    f <- hv_fit()
    if (is.null(f)) return(NULL)
    co <- f$co
    read <- function(p, est) if (p < 0.05) (if (est > 0) "Clear positive link" else "Clear negative link") else if (p < 0.1) "Weak link" else "No clear link"
    tags$div(
      class = "chart-card",
      tags$h3("Regression table"),
      tags$table(
        class = "hv-table",
        tags$thead(tags$tr(tags$th("Driver"), tags$th("Effect per unit"), tags$th("Std. error"), tags$th("Standardised"), tags$th("p-value"), tags$th("Reading"))),
        tags$tbody(lapply(seq_len(nrow(co)), function(i) tags$tr(
          tags$td(drv_lab[[co$term[i]]]), tags$td(paste(fmt_num(co$est[i], 2), "kg/ha")), tags$td(fmt_num(co$se[i], 2)),
          tags$td(fmt_num(co$beta[i], 2)), tags$td(formatC(co$p[i], format = "f", digits = 3)), tags$td(read(co$p[i], co$est[i]))
        )))
      ),
      tags$p(class = "note", paste0(
        "Yield = a + b\u2081\u00b7fertilizer + b\u2082\u00b7rainfall + b\u2083\u00b7rotation, fitted on ", f$n, " national years (", f$years[1], "\u2013", f$years[2], "). ",
        "Fertilizer is World Bank kg/ha of arable land; rainfall is the yearly NASA POWER mean of six Rwandan stations; rotation is a crop-diversity index (100 = many crops share the land) from FAO harvested areas. ",
        "A regression shows which inputs move with yield. It supports, but cannot by itself prove, cause."
      ))
    )
  })
}
