# Umuganura — analysis
# plotly is attached in global.R before UI modules (do not defer / re-gate here).

server <- function(input, output, session) {

  isoko_mounted <- reactiveVal(FALSE)
  isoko_inserted <- reactiveVal(character(0))

  isoko_report_pick <- function() {
    p <- input$factor_pick
    id <- if (is.null(p) || is.null(p$item) || !nzchar(as.character(p$item)[1])) {
      "inflation-overview"
    } else {
      as.character(p$item)[1]
    }
    if (id %in% isoko_report_ids) id else NULL
  }

  insert_isoko_report <- function(id) {
    id <- as.character(id)[1]
    if (!id %in% isoko_report_ids || id %in% isoko_inserted()) return(invisible(NULL))
    sel <- sprintf('#iso-report .iso-rep-pane[data-report="%s"]', id)
    insertUI(
      selector = sel,
      where = "beforeEnd",
      html = get_isoko_report_one(id),
      immediate = TRUE,
      session = session
    )
    isoko_inserted(unique(c(isolate(isoko_inserted()), id)))
    invisible(NULL)
  }

  observeEvent(input$factor_ready, {
    req(isTruthy(input$factor_ready))
    first <- !isoko_mounted()
    if (first) {
      isoko_mounted(TRUE)
      session$onFlushed(function() write_bridge_exports(), once = TRUE)
    }
    rid <- isoko_report_pick()
    if (!is.null(rid)) insert_isoko_report(rid)
  }, ignoreInit = TRUE)

  observeEvent(input$factor_pick, {
    req(isoko_mounted())
    rid <- isoko_report_pick()
    if (!is.null(rid)) insert_isoko_report(rid)
  }, ignoreInit = TRUE)

  # One store, two slices. Every room reads these instead of keeping its own copy.
  store <- reactiveValues(
    hub = list(crop = "Maize", metric = "figure", province = "All Rwanda", district = "All districts", page = 1L, cache = "miss"),
    catalogue = list(q = "", drawer = "All", grain = "All")
  )
  query_cache <- new.env(parent = emptyenv())
  window_size <- 12L

  observeEvent(input$hub_crop, {
    store$hub$crop <- input$hub_crop
    store$hub$page <- 1L
  }, ignoreInit = TRUE)
  observeEvent(input$hub_metric, {
    store$hub$metric <- input$hub_metric
    store$hub$page <- 1L
  }, ignoreInit = TRUE)
  observeEvent(input$hub_province, {
    store$hub$province <- input$hub_province
    store$hub$district <- "All districts"
    store$hub$page <- 1L
  }, ignoreInit = TRUE)
  observeEvent(input$hub_district, {
    store$hub$district <- input$hub_district
    store$hub$page <- 1L
  }, ignoreInit = TRUE)
  observeEvent(input$hub_prev, {
    store$hub$page <- max(1L, store$hub$page - 1L)
  })
  observeEvent(input$hub_next, {
    store$hub$page <- store$hub$page + 1L
  })

  green_layout <- function(p, title = NULL) {
    if (!has_plotly) return(p)
    p |>
      layout(
        title = list(text = title, font = list(size = 16, color = "#1b4332")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Outfit, Segoe UI, sans-serif", size = 13, color = "#1a1a1a"),
        margin = list(l = 56, r = 16, t = if (is.null(title)) 16 else 40, b = 48),
        xaxis = list(gridcolor = "rgba(27,67,50,0.08)", zeroline = FALSE),
        yaxis = list(gridcolor = "rgba(27,67,50,0.08)", zeroline = FALSE),
        hoverlabel = list(bgcolor = "#1b4332", font = list(color = "#ffffff", size = 13))
      )
  }

  observe({
    prov <- if (is.null(input$hub_province)) "All Rwanda" else input$hub_province
    d <- unique(district_crop$district)
    if (prov != "All Rwanda") {
      d <- unique(district_crop$district[district_crop$province == prov])
    }
    d <- sort(d[!is.na(d)])
    isolate(updateSelectInput(session, "hub_district", choices = c("All districts", d), selected = "All districts"))
  })

  hub <- reactive({
    crop <- store$hub$crop
    metric <- store$hub$metric
    province <- store$hub$province
    district <- store$hub$district
    key <- paste(crop, metric, province, district, sep = "|")
    if (exists(key, envir = query_cache, inherits = FALSE)) {
      return(get(key, envir = query_cache, inherits = FALSE))
    }
    d <- district_crop[district_crop$crop == crop, , drop = FALSE]
    if (!is.null(province) && province != "All Rwanda") {
      d <- d[d$province == province, , drop = FALSE]
    }
    if (!is.null(district) && district != "All districts") {
      d <- d[d$district == district, , drop = FALSE]
    }
    d$value <- d[[metric]]
    d <- d[is.finite(d$value), , drop = FALSE]
    d <- d[order(-d$value), , drop = FALSE]
    d$share <- if (nrow(d) && sum(d$value) > 0) d$value / sum(d$value) else NA_real_
    d$rank <- seq_len(nrow(d))
    assign(key, d, envir = query_cache)
    d
  })

  output$hub_glance <- renderUI({
    d <- hub()
    if (!nrow(d)) {
      return(tags$p(class = "note", "No published figure for this filter."))
    }
    x <- d$value
    cv <- if (mean(x) != 0) stats::sd(x) / mean(x) else NA_real_
    tags$div(
      class = "mega-row four glance",
      tags$article(class = "lux-card", tags$span("Districts"), tags$strong(nrow(d)), tags$p(input$hub_crop)),
      tags$article(class = "lux-card", tags$span("Leading"), tags$strong(d$district[1]), tags$p(paste(fmt_num(d$value[1], 0), "·", fmt_pct(100 * d$share[1], 1)))),
      tags$article(class = "lux-card", tags$span("Median"), tags$strong(fmt_num(stats::median(x), 0)), tags$p("Half sit below this figure")),
      tags$article(class = "lux-card", tags$span("Spread (CV)"), tags$strong(fmt_pct(100 * cv, 0)), tags$p("Coefficient of variation"))
    )
  })

  output$hub_stats <- renderUI({
    d <- hub()
    if (!nrow(d)) return(tags$p("No published figure for this choice."))
    x <- d$value
    cv <- if (mean(x) != 0) stats::sd(x) / mean(x) else NA_real_
    tags$ul(
      class = "stat-list",
      tags$li(tags$span("Sum"), tags$strong(fmt_num(sum(x), 0))),
      tags$li(tags$span("Mean"), tags$strong(fmt_num(mean(x), 1))),
      tags$li(tags$span("Top share"), tags$strong(fmt_pct(100 * d$share[1], 1)))
    )
  })

  output$hub_map <- renderPlotly({
    d <- hub()
    if (!has_plotly || is.null(rwanda_geo) || !nrow(d)) return(NULL)
    plot_ly(
      d,
      type = "choropleth",
      geojson = rwanda_geo,
      locations = ~district,
      z = ~value,
      text = ~paste0(district, "<br>", fmt_num(value, 1)),
      featureidkey = "properties.shapeName",
      colorscale = list(c(0, "#e8f0df"), c(0.45, "#52b788"), c(1, "#1b4332")),
      marker = list(line = list(width = 0.6, color = "#ffffff")),
      showscale = TRUE,
      colorbar = list(thickness = 12, len = 0.6, tickfont = list(size = 11))
    ) |>
      layout(
        geo = list(fitbounds = "locations", visible = FALSE),
        margin = list(l = 0, r = 0, t = 0, b = 0),
        paper_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Outfit, Segoe UI, sans-serif", size = 13)
      ) |>
      config(scrollZoom = TRUE, displayModeBar = TRUE, displaylogo = FALSE)
  })

  output$hub_bar <- renderPlotly({
    d <- head(hub(), 12)
    if (!has_plotly || !nrow(d)) return(NULL)
    green_layout(
      plot_ly(d, x = ~reorder(district, value), y = ~value, type = "bar",
              marker = list(color = "#2d6a4f"),
              hovertemplate = "%{x}: %{y:,.1f}<extra></extra>") |>
        layout(xaxis = list(title = "", categoryorder = "total ascending"), yaxis = list(title = ""))
    )
  })

  output$hub_hist <- renderPlotly({
    d <- hub()
    if (!has_plotly || !nrow(d)) return(NULL)
    green_layout(
      plot_ly(x = d$value, type = "histogram", marker = list(color = "#40916c"),
              nbinsx = 10, hovertemplate = "Districts: %{y}<extra></extra>") |>
        layout(xaxis = list(title = "Figure"), yaxis = list(title = "Districts"))
    )
  })

  output$hub_scatter <- renderPlotly({
    d <- hub()
    d <- d[is.finite(d$land) & d$land > 0 & is.finite(d$figure), , drop = FALSE]
    if (!has_plotly || nrow(d) < 3) return(NULL)
    fit <- stats::lm(figure ~ land, data = d)
    r <- stats::cor(d$land, d$figure)
    line_x <- seq(min(d$land), max(d$land), length.out = 40)
    line_y <- stats::predict(fit, newdata = data.frame(land = line_x))
    green_layout(
      plot_ly(d, x = ~land, y = ~figure, type = "scatter", mode = "markers",
              text = ~district, marker = list(color = "#1b4332", size = 9),
              hovertemplate = "%{text}<br>Land %{x:,.0f}<br>Figure %{y:,.0f}<extra></extra>") |>
        add_trace(x = line_x, y = line_y, mode = "lines", inherit = FALSE,
                  line = list(color = "#c6a336", width = 2),
                  name = paste0("Fit  r = ", format(round(r, 2), nsmall = 2)),
                  hoverinfo = "skip"),
      title = NULL
    )
  })

  output$hub_window <- renderText({
    d <- hub()
    pages <- max(1L, ceiling(nrow(d) / window_size))
    page <- min(isolate(store$hub$page), pages)
    if (page != isolate(store$hub$page)) isolate(store$hub$page <- page)
    start <- if (nrow(d)) (page - 1L) * window_size + 1L else 0L
    end <- min(nrow(d), page * window_size)
    paste0("Rows ", start, "–", end, " of ", nrow(d))
  })

  output$hub_table <- renderTable({
    d <- hub()
    if (!nrow(d)) return(NULL)
    pages <- max(1L, ceiling(nrow(d) / window_size))
    page <- min(store$hub$page, pages)
    start <- (page - 1L) * window_size + 1L
    end <- min(nrow(d), page * window_size)
    d <- d[seq.int(start, end), , drop = FALSE]
    data.frame(
      Rank = d$rank,
      Province = d$province,
      District = d$district,
      Figure = round(d$value, 1),
      Share = paste0(format(round(100 * d$share, 1), nsmall = 1), "%"),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "llrrr")

  output$hub_csv <- downloadHandler(
    filename = function() paste0("umuganura-", gsub(" ", "-", tolower(input$hub_crop)), ".csv"),
    content = function(file) {
      d <- hub()
      utils::write.csv(d[, c("rank", "province", "district", "value", "share")], file, row.names = FALSE)
    }
  )

  # Room opens via CSS (display:none → block). Gate on factor_ready so charts
  # only build after the room is visible, and ship as PNG uiOutputs so Shiny
  # never waits on plotOutput client width/height.
  factor_live <- reactive({
    req(input$factor_ready)
    TRUE
  })

  factor_pick <- reactive({
    factor_live()
    p <- input$factor_pick
    id <- if (is.null(p) || is.null(p$item) || !nzchar(as.character(p$item)[1]) || !as.character(p$item)[1] %in% factor_items$id) {
      "inflation-overview"
    } else {
      as.character(p$item)[1]
    }
    factor_items[factor_items$id == id, , drop = FALSE]
  })

  price_mode <- reactive({
    id <- as.character(factor_pick()$id)[1]
    if (id %in% c("inflation-overview", "interest-overview", "gdp-overview", "fx-overview", "jobs-overview")) return("report")
    if (identical(id, "inflation-input")) return("input")
    if (identical(id, "inflation-food")) return("food")
    if (identical(id, "inflation-livestock")) return("live")
    if (grepl("^(interest|jobs|gdp|fx)-", id)) return("force")
    "classic"
  })

  market_tab <- reactive({
    req(isTruthy(input$factor_ready))
    tab <- input$isoko_tabs
    mode <- price_mode()
    req(isTruthy(tab), isTruthy(mode))
    req(tab %in% c("food", "live"))
    req(identical(tab, mode))
    TRUE
  })

  observeEvent(input$factor_pick, {
    req(isTruthy(input$factor_ready))
    mode <- isolate(price_mode())
    req(isTruthy(mode))
    tab <- if (mode %in% c("overview", "input", "food", "live")) mode else "idle"
    cur <- isolate(input$isoko_tabs)
    if (!identical(cur, tab)) {
      isolate(updateTabsetPanel(session, "isoko_tabs", selected = tab))
    }
  }, ignoreInit = TRUE)

  factor_full <- reactive({
    sid <- as.character(factor_pick()$series)[1]
    if (!length(sid) || is.na(sid) || !nzchar(sid)) return(data.frame(year = integer(), value = numeric()))
    d <- factor_data[factor_data$id == sid, c("year", "value"), drop = FALSE]
    d <- d[is.finite(d$year) & is.finite(d$value), , drop = FALSE]
    d[order(d$year), , drop = FALSE]
  })

  observeEvent(list(input$factor_pick, input$factor_ready), {
    d <- factor_full()
    if (!nrow(d)) {
      updateSliderInput(session, "factor_year", min = 1960, max = 2025, value = c(1960, 2025))
      return()
    }
    lo <- as.integer(min(d$year))
    hi <- as.integer(max(d$year))
    updateSliderInput(session, "factor_year", min = lo, max = hi, value = c(lo, hi))
  }, ignoreInit = TRUE)

  factor_frame <- reactive({
    d <- factor_full()
    yr <- input$factor_year
    if (nrow(d) && !is.null(yr) && length(yr) == 2 && is.finite(yr[1]) && is.finite(yr[2])) {
      d <- d[d$year >= min(yr) & d$year <= max(yr), , drop = FALSE]
    }
    d
  })

  factor_ids <- function(row) {
    primary <- as.character(row$series)[1]
    extra <- as.character(row$peers)[1]
    bits <- c(primary, if (!is.na(extra) && nzchar(extra)) strsplit(extra, ",", fixed = TRUE)[[1]] else character())
    unique(bits[!is.na(bits) & nzchar(bits)])
  }

  factor_long <- reactive({
    ids <- factor_ids(factor_pick())
    empty <- data.frame(year = integer(), value = numeric(), series = character(), label = character(), stringsAsFactors = FALSE)
    if (!length(ids)) return(empty)
    parts <- lapply(ids, function(sid) {
      d <- factor_data[factor_data$id == sid, c("year", "value"), drop = FALSE]
      d <- d[is.finite(d$year) & is.finite(d$value), , drop = FALSE]
      if (!nrow(d)) return(NULL)
      lab <- unname(series_label[sid])
      d$series <- sid
      d$label <- if (length(lab) == 1 && !is.na(lab) && nzchar(lab)) lab else sid
      d
    })
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (!length(parts)) return(empty)
    out <- do.call(rbind, parts)
    yr <- input$factor_year
    if (!is.null(yr) && length(yr) == 2 && is.finite(yr[1]) && is.finite(yr[2])) {
      out <- out[out$year >= min(yr) & out$year <= max(yr), , drop = FALSE]
    }
    out[order(out$label, out$year), , drop = FALSE]
  })

  factor_cols <- c("#1b4332", "#40916c", "#c6a336", "#9b2226", "#1d3557", "#bc6c25", "#2a9d8f", "#6d597a")

  factor_theme <- function() {
    ggplot2::theme_minimal(base_family = "sans") +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#ffffff", colour = NA),
        panel.background = ggplot2::element_rect(fill = "#ffffff", colour = NA),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(colour = "#e8eee9"),
        axis.title = ggplot2::element_text(colour = "#1a1a1a", size = 11),
        axis.text = ggplot2::element_text(colour = "#1a1a1a", size = 10)
      )
  }

  factor_empty_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg, colour = "#5c6560", size = 4) +
      ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#ffffff", colour = NA))
  }

  factor_png <- function(plot_obj, width = 720, height = 280) {
    f <- tempfile(fileext = ".png")
    on.exit(unlink(f), add = TRUE)
    ggplot2::ggsave(
      filename = f, plot = plot_obj,
      width = width / 96, height = height / 96, dpi = 96, bg = "white",
      limitsize = FALSE
    )
    raw <- readBin(f, what = "raw", n = file.info(f)$size)
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      b64 <- jsonlite::base64_enc(raw)
    } else if (requireNamespace("base64enc", quietly = TRUE)) {
      b64 <- base64enc::base64encode(raw)
    } else {
      stop("Need jsonlite or base64enc to draw factor charts.")
    }
    src <- paste0("data:image/png;base64,", gsub("\n", "", b64))
    tags$img(src = src, class = "factor-chart", alt = "", width = width, height = height)
  }

  output$factor_head <- renderUI({
    req(identical(price_mode(), "classic"))
    row <- factor_pick()
    tags$div(
      tags$p(class = "kicker", row$force_label),
      tags$h2(row$label),
      tags$p(class = "lede", row$story)
    )
  })

  output$factor_glance <- renderUI({
    req(identical(price_mode(), "classic"))
    row <- factor_pick()
    d <- factor_frame()
    sid <- as.character(row$series)[1]
    primary_name <- if (nzchar(sid)) unname(series_label[sid]) else NA_character_
    latest_kicker <- if (length(primary_name) == 1 && !is.na(primary_name) && nzchar(as.character(row$peers)[1])) {
      paste("Latest ·", primary_name)
    } else {
      "Latest"
    }
    latest_txt <- "—"
    period_txt <- "—"
    change_txt <- "—"
    period_note <- "No published years"
    change_note <- "Needs two published years"
    if (nrow(d)) {
      latest_txt <- paste0(fmt_num(d$value[nrow(d)], row$digits), if (nzchar(row$unit)) paste0(" ", row$unit))
      period_txt <- paste(min(d$year), "–", max(d$year))
      period_note <- paste(nrow(d), "published years")
      if (nrow(d) >= 2) {
        delta <- d$value[nrow(d)] - d$value[nrow(d) - 1]
        change_txt <- paste0(if (delta > 0) "+" else "", fmt_num(delta, row$digits))
        change_note <- paste(d$year[nrow(d) - 1], "to", d$year[nrow(d)])
      }
    }
    tags$div(
      class = "mega-row glance",
      tags$article(class = "lux-card", tags$span(latest_kicker), tags$strong(latest_txt), tags$p(if (nrow(d)) paste("In", max(d$year)) else period_note)),
      tags$article(class = "lux-card", tags$span("Period"), tags$strong(period_txt), tags$p(period_note)),
      tags$article(class = "lux-card", tags$span("Change"), tags$strong(change_txt), tags$p(change_note))
    )
  })

  output$factor_chart <- renderUI({
    req(identical(price_mode(), "classic"))
    row <- factor_pick()
    mode <- input$factor_viz
    if (is.null(mode) || !nzchar(as.character(mode)[1])) mode <- "trend"
    mode <- as.character(mode)[1]
    d <- factor_frame()
    long <- factor_long()
    n_series <- length(unique(long$series))
    caption <- "Published years inside the range."
    p <- factor_empty_plot("No published series")

    if (mode == "trend") {
      caption <- if (n_series > 1) "Each published series over the selected years." else "Published years inside the range."
      if (n_series > 1) {
        p <- ggplot2::ggplot(long, ggplot2::aes(year, value, colour = label)) +
          ggplot2::geom_line(linewidth = 1) +
          ggplot2::geom_point(size = 1.6) +
          ggplot2::scale_colour_manual(values = factor_cols) +
          ggplot2::labs(x = NULL, y = row$unit, colour = NULL) +
          factor_theme() +
          ggplot2::theme(legend.position = "bottom")
      } else if (nrow(d)) {
        p <- ggplot2::ggplot(d, ggplot2::aes(year, value)) +
          ggplot2::geom_area(fill = "#40916c", alpha = 0.28) +
          ggplot2::geom_line(colour = "#1b4332", linewidth = 1) +
          ggplot2::geom_point(colour = "#1b4332", size = 1.8) +
          ggplot2::labs(x = NULL, y = row$unit) +
          factor_theme()
      }
    } else if (mode == "compare") {
      if (n_series > 1) {
        caption <- "Latest published year for each series."
        last <- do.call(rbind, lapply(split(long, long$label), function(z) z[which.max(z$year), , drop = FALSE]))
        last$label <- factor(last$label, levels = last$label[order(last$value)])
        p <- ggplot2::ggplot(last, ggplot2::aes(label, value, fill = label)) +
          ggplot2::geom_col(width = 0.72) +
          ggplot2::coord_flip() +
          ggplot2::scale_fill_manual(values = factor_cols) +
          ggplot2::labs(x = NULL, y = row$unit) +
          factor_theme() +
          ggplot2::theme(legend.position = "none")
      } else if (nrow(d) >= 2) {
        caption <- "Each published year, side by side."
        p <- ggplot2::ggplot(d, ggplot2::aes(year, value)) +
          ggplot2::geom_col(fill = "#2d6a4f", width = 0.8) +
          ggplot2::labs(x = NULL, y = row$unit) +
          factor_theme()
      } else if (!nrow(d)) {
        caption <- "No published series."
      } else {
        caption <- "Need two published years."
        p <- factor_empty_plot("Need two published years")
      }
    } else if (mode == "share") {
      caption <- "Share of the selected prices in each year. Not a household basket weight."
      if (n_series < 2) {
        caption <- "Part-to-whole needs two series. Basket weights were not published."
        p <- factor_empty_plot("Needs two published series")
      } else {
        tot <- aggregate(value ~ year, data = long, FUN = sum)
        names(tot)[2] <- "tot"
        sh <- merge(long, tot, by = "year")
        sh <- sh[is.finite(sh$tot) & sh$tot > 0, , drop = FALSE]
        sh$share <- sh$value / sh$tot
        p <- ggplot2::ggplot(sh, ggplot2::aes(year, share, fill = label)) +
          ggplot2::geom_area(position = "stack", colour = "#ffffff", linewidth = 0.2) +
          ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
          ggplot2::scale_fill_manual(values = factor_cols) +
          ggplot2::labs(x = NULL, y = "Share", fill = NULL) +
          factor_theme() +
          ggplot2::theme(legend.position = "bottom")
      }
    } else if (mode == "spread") {
      caption <- "Histogram and box plot of the published values."
      if (nrow(d) < 2) {
        caption <- "Need two published years."
        p <- factor_empty_plot("Need two published years")
      } else {
        hist_p <- ggplot2::ggplot(d, ggplot2::aes(value)) +
          ggplot2::geom_histogram(fill = "#40916c", bins = min(12, max(5, nrow(d))), colour = "#ffffff") +
          ggplot2::labs(x = row$unit, y = "Years", title = "Histogram") +
          factor_theme()
        box_p <- ggplot2::ggplot(d, ggplot2::aes(y = value)) +
          ggplot2::geom_boxplot(fill = "#1b4332", width = 0.35, colour = "#1b4332") +
          ggplot2::labs(x = NULL, y = row$unit, title = "Box plot") +
          factor_theme()
        ga <- ggplot2::ggplotGrob(hist_p)
        gb <- ggplot2::ggplotGrob(box_p)
        gt <- gtable::gtable(
          widths = grid::unit(c(1, 1), "null"),
          heights = grid::unit(1, "null")
        )
        p <- gtable::gtable_add_grob(gtable::gtable_add_grob(gt, ga, 1, 1), gb, 1, 2)
      }
    } else if (mode == "relate") {
      caption <- "Each published year, with a straight line when at least six years exist."
      if (!nrow(d)) {
        caption <- "No published series."
      } else {
        g <- ggplot2::ggplot(d, ggplot2::aes(year, value)) +
          ggplot2::geom_point(colour = "#1b4332", size = 2.2)
        f <- fit_line(d)
        if (!is.null(f)) {
          line_y <- as.numeric(stats::coef(f$fit)[1] + stats::coef(f$fit)[2] * d$year)
          g <- g + ggplot2::geom_line(ggplot2::aes(y = line_y), colour = "#c6a336", linewidth = 1)
        } else {
          caption <- "Points only. A line needs at least six published years."
        }
        p <- g + ggplot2::labs(x = NULL, y = row$unit) + factor_theme()
      }
    }

    tags$div(
      tags$p(caption),
      factor_png(p, width = 980, height = 420)
    )
  })

  output$factor_ahead <- renderUI({
    req(identical(price_mode(), "classic"))
    d <- factor_frame()
    row <- factor_pick()
    p <- if (!nrow(d)) {
      factor_empty_plot("No published series")
    } else {
      f <- fit_line(d)
      g <- ggplot2::ggplot(d, ggplot2::aes(year, value)) +
        ggplot2::geom_line(colour = "#1b4332", linewidth = 1)
      if (!is.null(f)) {
        g <- g + ggplot2::geom_point(
          data = data.frame(year = f$next_year, value = f$point),
          colour = "#c6a336", size = 3.5
        )
      }
      g + ggplot2::labs(x = NULL, y = row$unit) + factor_theme()
    }
    factor_png(p)
  })

  output$factor_model <- renderUI({
    req(identical(price_mode(), "classic"))
    d <- factor_frame()
    row <- factor_pick()
    var_txt <- "—"
    cv_txt <- "—"
    slope_txt <- "—"
    r2_txt <- "—"
    if (nrow(d) >= 2) {
      sdv <- stats::sd(d$value)
      var_txt <- fmt_num(stats::var(d$value), row$digits)
      cv_txt <- if (is.finite(mean(d$value)) && mean(d$value) != 0) fmt_pct(100 * sdv / mean(d$value), 0) else "—"
    }
    f <- fit_line(d)
    if (!is.null(f)) {
      slope_txt <- fmt_num(f$slope, row$digits)
      r2_txt <- format(round(f$r2, 2), nsmall = 2)
    }
    tags$div(
      class = "mega-row four glance",
      tags$article(class = "lux-card", tags$span("Variance"), tags$strong(var_txt), tags$p("Sample variance of the published years")),
      tags$article(class = "lux-card", tags$span("Coefficient of variation"), tags$strong(cv_txt), tags$p("Standard deviation divided by the mean")),
      tags$article(class = "lux-card", tags$span("Slope per year"), tags$strong(slope_txt), tags$p("Ordinary least squares, value = a + b × year")),
      tags$article(class = "lux-card", tags$span("R²"), tags$strong(r2_txt), tags$p(if (is.null(f)) "Fewer than 6 years, so no line is fitted" else paste(f$n, "years in the fit")))
    )
  })

  output$factor_forecast <- renderUI({
    req(identical(price_mode(), "classic"))
    d <- factor_frame()
    row <- factor_pick()
    f <- fit_line(d)
    point <- "—"
    band <- "No next-year figure. A line needs at least 6 published years."
    if (!is.null(f)) {
      point <- paste0(fmt_num(f$point, row$digits), if (nzchar(row$unit)) paste0(" ", row$unit))
      band <- paste0(
        "95% interval ", fmt_num(f$lo, row$digits), " to ", fmt_num(f$hi, row$digits),
        ". Straight-line baseline for ", f$next_year, ", not a seasonal forecast."
      )
    }
    tags$div(
      class = "mega-row four glance",
      tags$article(class = "lux-card", tags$span("Next year"), tags$strong(point), tags$p(band))
    )
  })

  output$factor_note <- renderUI({
    req(identical(price_mode(), "classic"))
    row <- factor_pick()
    tags$div(class = "method", paste(row$caveat, "Source:", if (nzchar(row$source)) row$source else "Not in the folder.", "Empty years were not filled."))
  })

  # Do not force classic factor outputs while hidden — that overloaded Isoko/inflation tabs.

  output$gdp_plot <- renderPlotly({
    if (!has_plotly || !nrow(gdp_share)) return(NULL)
    green_layout(
      plot_ly(gdp_share, x = ~year, y = ~value, type = "scatter", mode = "lines",
              line = list(color = "#1b4332", width = 2.4),
              fill = "tozeroy", fillcolor = "rgba(45,106,79,0.15)",
              hovertemplate = "%{x}: %{y:.1f}% of GDP<extra></extra>") |>
        layout(yaxis = list(title = "% of GDP", ticksuffix = "%"))
    )
  }) |> bindCache("gdp-share", cache = "app")

  output$jobs_plot <- renderPlotly({
    d <- empl[empl$year >= 1991 & is.finite(empl$value), , drop = FALSE]
    if (!has_plotly || !nrow(d)) return(NULL)
    green_layout(
      plot_ly(d, x = ~year, y = ~value, type = "scatter", mode = "lines",
              line = list(color = "#2d6a4f", width = 2.4),
              hovertemplate = "%{x}: %{y:.1f}% of jobs<extra></extra>") |>
        layout(yaxis = list(title = "% of employment", ticksuffix = "%"))
    )
  }) |> bindCache("jobs-share", cache = "app")

  output$gap_map <- renderUI({
    d <- district_crop[district_crop$crop == "Maize" & is.finite(district_crop$ratio), , drop = FALSE]
    if (is.null(rwanda_geo) || !nrow(d)) return(NULL)
    HTML(build_svg_map(rwanda_geo, stats::setNames(d$ratio, d$district)))
  })

  output$gap_bar <- renderPlotly({
    d <- district_crop[district_crop$crop == "Maize" & is.finite(district_crop$ratio), , drop = FALSE]
    if (!has_plotly || !nrow(d)) return(NULL)
    cut <- stats::quantile(d$ratio, 0.25, na.rm = TRUE)
    thin <- d[d$ratio <= cut, , drop = FALSE]
    thin <- thin[order(thin$ratio), , drop = FALSE]
    green_layout(
      plot_ly(thin, x = ~reorder(district, ratio), y = ~ratio, type = "bar",
              marker = list(color = "#40916c"),
              hovertemplate = "%{x}: %{y:.2f} per hectare<extra></extra>") |>
        layout(xaxis = list(title = ""), yaxis = list(title = "Figure per hectare"))
    )
  }) |> bindCache("maize-gap", cache = "app")

  output$gap_read <- renderUI({
    d <- district_crop[district_crop$crop == "Maize" & is.finite(district_crop$ratio), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    cv <- stats::sd(d$ratio) / mean(d$ratio)
    tags$div(
      class = "method",
      paste0(
        "Maize figure-per-hectare across ", nrow(d),
        " districts has a coefficient of variation of ", fmt_pct(100 * cv, 0),
        ". The chart keeps the bottom quarter. Irrigation at national scale is ",
        home_stats$irrig, " of agricultural land (World Bank ", home_stats$irrig_year,
        "), so most of this spread is rainfed land, not a full irrigation map."
      )
    )
  })

  forecast <- reactive({
    item <- input$forecast_item
    if (is.null(item) || !nrow(fao_prod)) return(NULL)
    d <- fao_prod[fao_prod$item == item, c("year", "value")]
    fit_line(d)
  })

  output$forecast_plot <- renderPlotly({
    f <- forecast()
    if (!has_plotly || is.null(f)) return(NULL)
    years <- c(f$df$year, f$next_year)
    fitted <- stats::predict(f$fit, newdata = data.frame(year = years))
    green_layout(
      plot_ly() |>
        add_trace(data = f$df, x = ~year, y = ~value, type = "scatter", mode = "markers",
                  name = "FAO production", marker = list(color = "#1b4332", size = 8),
                  hovertemplate = "%{x}: %{y:,.0f} t<extra></extra>") |>
        add_trace(x = years, y = fitted, type = "scatter", mode = "lines",
                  name = "Straight line", line = list(color = "#2d6a4f", width = 2)) |>
        add_trace(x = f$next_year, y = f$point, type = "scatter", mode = "markers",
                  name = paste("Baseline", f$next_year),
                  marker = list(color = "#c6a336", size = 14, symbol = "diamond"),
                  error_y = list(type = "data", array = f$hi - f$point, arrayminus = f$point - f$lo,
                                 color = "#c6a336")) |>
        layout(xaxis = list(title = ""), yaxis = list(title = "Tonnes"),
               legend = list(orientation = "h", y = -0.2))
    )
  }) |> bindCache(input$forecast_item, cache = "app")

  output$forecast_read <- renderUI({
    f <- forecast()
    if (is.null(f)) {
      return(tags$div(class = "method", "This series is too short for a line."))
    }
    tags$div(
      class = "method",
      paste0(
        "Method: ordinary least squares, production = a + b × year, on ", f$n,
        " FAO years. Slope ", fmt_num(f$slope, 1), " tonnes per year. R² ",
        format(round(f$r2, 2), nsmall = 2),
        ". Residual standard error ", fmt_num(f$sigma, 0), " tonnes. Baseline for ",
        f$next_year, " is ", fmt_num(f$point, 0),
        " tonnes, with a 95% interval for the mean line from ",
        fmt_num(f$lo, 0), " to ", fmt_num(f$hi, 0),
        ". This does not use district weather. It is the national trend only."
      )
    )
  })

  store$prices <- list(item = if ("Maize (corn)" %in% price_crops) "Maize (corn)" else price_crops[1])
  observeEvent(input$price_item, {
    store$prices$item <- input$price_item
  }, ignoreInit = TRUE)

  price_frame <- reactive({
    item <- if (is.null(store$prices$item)) price_crops[1] else store$prices$item
    d <- crop_prices[crop_prices$item == item, c("year", "value"), drop = FALSE]
    d <- d[is.finite(d$year) & is.finite(d$value), , drop = FALSE]
    d[order(d$year), , drop = FALSE]
  })

  output$price_cards <- renderUI({
    item <- if (is.null(store$prices$item)) price_crops[1] else store$prices$item
    v <- crop_value[crop_value$item == item & is.finite(crop_value$share), , drop = FALSE]
    v <- v[order(v$year), , drop = FALSE]
    share_txt <- "—"
    share_note <- "No FAO gross production value for this crop."
    if (nrow(v)) {
      last <- v[nrow(v), ]
      share_txt <- fmt_pct(100 * last$share, 1)
      share_note <- paste("FAO constant 2014–2016 I$,", last$year, "· not agricultural GDP")
    }
    p <- fao_prod[fao_prod$item == item & is.finite(fao_prod$value), , drop = FALSE]
    p <- p[order(p$year), , drop = FALSE]
    prod_txt <- "—"
    prod_note <- "No FAO production tonnes for this crop. Not a household survey."
    if (nrow(p)) {
      lastp <- p[nrow(p), ]
      tot <- sum(fao_prod$value[fao_prod$year == lastp$year & is.finite(fao_prod$value)])
      if (is.finite(tot) && tot > 0) {
        prod_txt <- fmt_pct(100 * lastp$value / tot, 1)
        prod_note <- paste("Share of FAO production tonnes,", lastp$year, "· welfare proxy, not EICV")
      }
    }
    d <- price_frame()
    yoy_txt <- "—"
    cagr_txt <- "—"
    if (nrow(d) >= 2 && d$value[nrow(d) - 1] > 0) {
      yoy_txt <- fmt_pct(100 * (d$value[nrow(d)] / d$value[nrow(d) - 1] - 1), 1)
    }
    span <- if (nrow(d) >= 2) d$year[nrow(d)] - d$year[1] else 0
    if (nrow(d) >= 2 && span > 0 && d$value[1] > 0) {
      cagr_txt <- fmt_pct(100 * ((d$value[nrow(d)] / d$value[1])^(1 / span) - 1), 1)
    }
    tags$div(
      class = "mega-row four glance",
      tags$article(class = "lux-card", tags$span("Share of production value"), tags$strong(share_txt), tags$p(share_note)),
      tags$article(class = "lux-card", tags$span("Share of tonnes"), tags$strong(prod_txt), tags$p(prod_note)),
      tags$article(class = "lux-card", tags$span("Latest year-on-year"), tags$strong(yoy_txt), tags$p("Nominal LCU per tonne")),
      tags$article(class = "lux-card", tags$span("CAGR"), tags$strong(cagr_txt), tags$p("First published year to last"))
    )
  })

  output$price_plot <- renderPlotly({
    d <- price_frame()
    if (!has_plotly || !nrow(d)) return(NULL)
    green_layout(
      plot_ly(d, x = ~year, y = ~value, type = "scatter", mode = "lines+markers",
              name = "Nominal LCU/tonne",
              line = list(color = "#1b4332", width = 2.4),
              marker = list(color = "#1b4332", size = 7),
              hovertemplate = "%{x}: %{y:,.0f} LCU/t<extra></extra>") |>
        layout(yaxis = list(title = "LCU per tonne"),
               legend = list(orientation = "h", y = -0.2))
    )
  })

  output$price_read <- renderUI({
    d <- price_frame()
    item <- if (is.null(store$prices$item)) price_crops[1] else store$prices$item
    if (!nrow(d)) {
      return(tags$div(class = "method", paste(item, "has no published producer price in LCU per tonne. The cell stays empty.")))
    }
    cv <- if (mean(d$value) != 0) stats::sd(d$value) / mean(d$value) else NA_real_
    f <- fit_line(d)
    trend <- if (is.null(f)) {
      "Fewer than 6 years, so no trend line is fitted."
    } else {
      paste0(
        "Ordinary least squares, price = a + b × year, on ", f$n,
        " years. Slope ", fmt_num(f$slope, 1), " LCU per tonne per year. R² ",
        format(round(f$r2, 2), nsmall = 2), "."
      )
    }
    tags$div(
      class = "method",
      paste0(
        "Source: FAOSTAT producer prices, element Producer Price (LCU/tonne). Each point is the mean of the months FAO published for that year. Missing months are not filled. ",
        "Coefficient of variation ", fmt_pct(100 * cv, 0), ". ", trend,
        " Contribution uses FAO Gross Production Value (constant 2014–2016 thousand international dollars) divided by the sum of that element in the same year. That is a share of recorded production value, not of agricultural GDP. ",
        "The tonne share is a welfare proxy from FAO production, not NISR EICV household income. ",
        "No real-price line: add World Bank FP.CPI.TOTL or NY.GDP.DEFL.ZS before an inflation-adjusted trace can be drawn. Crop: ", item, "."
      )
    )
  })


  price_keep <- function(id, choices, fallback) {
    cur <- isolate(input[[id]])
    sel <- if (!is.null(cur) && cur %in% choices) cur else fallback
    sig <- paste(c(sel, choices), collapse = "|")
    list(sel = sel, sig = sig)
  }

  prov_sig <- reactiveVal("")
  dist_sig <- reactiveVal("")
  mkt_sig <- reactiveVal("")
  cat_sig <- reactiveVal("")
  com_sig <- reactiveVal("")
  scope_sig <- reactiveVal("")
  in_item_sig <- reactiveVal("")
  in_meas_sig <- reactiveVal("")

  province_order <- c("Kigali", "Southern", "Western", "Northern", "Eastern")

  tab_prices <- reactive({
    req(isTRUE(market_tab()))
    mode <- price_mode()
    req(mode %in% c("food", "live"))
    d <- naeb_prices[naeb_prices$tab == mode, , drop = FALSE]
    req(nrow(d) > 0)
    d
  })

  place_filter <- function(d, year = TRUE, market = TRUE) {
    if (year) {
      yr <- input$price_year
      if (isTruthy(yr) && !identical(yr, "all")) {
        yi <- suppressWarnings(as.integer(yr))
        if (is.finite(yi)) d <- d[d$year == yi, , drop = FALSE]
      }
    }
    prov <- input$price_province
    if (isTruthy(prov) && prov != "All provinces") d <- d[d$province == prov, , drop = FALSE]
    dist <- input$price_district
    if (isTruthy(dist) && dist != "All districts") d <- d[d$district == dist, , drop = FALSE]
    if (market) {
      mkt <- input$price_market
      if (isTruthy(mkt) && mkt != "All markets") d <- d[d$market == mkt, , drop = FALSE]
    }
    d
  }

  observe({
    req(isTRUE(market_tab()))
    d <- tab_prices()
    ch <- c("All provinces", intersect(province_order, unique(d$province)))
    kept <- price_keep("price_province", ch, "All provinces")
    if (identical(isolate(prov_sig()), kept$sig)) return()
    prov_sig(kept$sig)
    isolate(updateSelectInput(session, "price_province", choices = ch, selected = kept$sel))
  })

  observe({
    req(isTRUE(market_tab()))
    d <- tab_prices()
    prov <- input$price_province
    if (isTruthy(prov) && prov != "All provinces") d <- d[d$province == prov, , drop = FALSE]
    ch <- c("All districts", sort(unique(d$district)))
    kept <- price_keep("price_district", ch, "All districts")
    if (identical(isolate(dist_sig()), kept$sig)) return()
    dist_sig(kept$sig)
    isolate(updateSelectInput(session, "price_district", choices = ch, selected = kept$sel))
  })

  observe({
    req(isTRUE(market_tab()))
    d <- place_filter(tab_prices(), year = FALSE, market = FALSE)
    mk <- unique(d[, c("district", "market"), drop = FALSE])
    mk <- mk[order(mk$district, mk$market), , drop = FALSE]
    flat <- c("All markets", unique(mk$market))
    kept <- price_keep("price_market", flat, "All markets")
    if (identical(isolate(mkt_sig()), kept$sig)) return()
    mkt_sig(kept$sig)
    grouped <- c(list(`All markets` = "All markets"), lapply(split(mk$market, mk$district), function(v) as.list(unique(v))))
    isolate(updateSelectInput(session, "price_market", choices = grouped, selected = kept$sel))
  })

  observe({
    mode <- price_mode()
    req(mode %in% c("food", "live"))
    req(isTRUE(market_tab()))
    cats <- if (identical(mode, "food")) food_price_categories else live_price_categories
    cats <- cats[cats %in% unique(naeb_prices$category[naeb_prices$tab == mode])]
    ch <- c("All categories", cats)
    kept <- price_keep("price_category", ch, "All categories")
    if (identical(isolate(cat_sig()), kept$sig)) return()
    cat_sig(kept$sig)
    isolate(updateSelectInput(session, "price_category", choices = ch, selected = kept$sel))
  })

  observe({
    req(isTRUE(market_tab()))
    d <- place_filter(tab_prices())
    cat <- input$price_category
    by_cat <- function(x) if (isTruthy(cat) && cat != "All categories") x[x$category == cat, , drop = FALSE] else x
    d <- by_cat(d)
    if (!nrow(d)) d <- by_cat(tab_prices())
    pc <- unique(d[, c("category", "product"), drop = FALSE])
    pc <- pc[order(pc$category, pc$product), , drop = FALSE]
    ch <- unique(pc$product)
    if (!length(ch)) ch <- "—"
    pref <- if (identical(price_mode(), "food")) c("Maize", "Beans/Mutiki", "Irish-potato") else c("Milk-fresh", "Beef/ imvange", "Egg- Pondese")
    fallback <- c(pref[pref %in% ch], ch)[1]
    kept <- price_keep("price_commodity", ch, fallback)
    if (identical(isolate(com_sig()), kept$sig)) return()
    com_sig(kept$sig)
    grouped <- if (nrow(pc)) {
      lapply(split(pc$product, pc$category), function(v) {
        u <- unique(v)
        stats::setNames(as.list(u), prod_label(u))
      })
    } else {
      list("—")
    }
    isolate(updateSelectInput(session, "price_commodity", choices = grouped, selected = kept$sel))
  })

  scope_choices <- function(mode) {
    if (identical(mode, "input")) {
      return(c(
        "All farm inputs" = "all", "Fertilizers" = "cat:fertilizer", "Pesticides" = "cat:pesticide", "Seeds" = "seed",
        "DAP" = "prod:DAP", "NPK" = "prod:NPK", "Urea" = "prod:Urea", "Dithane" = "prod:Dithane"
      ))
    }
    cats <- if (identical(mode, "food")) food_price_categories else live_price_categories
    cats <- cats[cats %in% unique(naeb_prices$category[naeb_prices$tab == mode])]
    c(
      setNames("all", if (identical(mode, "food")) "All food" else "All animal products"),
      setNames(paste0("cat:", cats), cap_first(cats)),
      "Selected product" = "selected"
    )
  }

  observe({
    mode <- price_mode()
    req(mode %in% c("input", "food", "live"))
    ch <- scope_choices(mode)
    kept <- price_keep("infl_scope", unname(ch), "all")
    if (identical(isolate(scope_sig()), kept$sig)) return()
    scope_sig(kept$sig)
    isolate(updateSelectInput(session, "infl_scope", choices = ch, selected = kept$sel))
  })

  observeEvent(input$input_group, {
    req(identical(price_mode(), "input"))
    g <- input$input_group
    req(isTruthy(g))
    to <- c(Fertilizer = "cat:fertilizer", Pesticide = "cat:pesticide", Seed = "seed")[[g]]
    req(isTruthy(to))
    if (!identical(isolate(input$infl_scope), to)) {
      isolate(updateSelectInput(session, "infl_scope", selected = to))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$stat_group, {
    req(isTruthy(input$stat_group))
    ch <- stat_measures[[input$stat_group]]
    req(length(ch) > 0)
    cur <- isolate(input$stat_measure)
    sel <- if (!is.null(cur) && cur %in% ch) cur else ch[1]
    isolate(updateSelectInput(session, "stat_measure", choices = ch, selected = sel))
  }, ignoreInit = TRUE)

  input_parent <- c(Fertilizer = "Inorganic fertilizer", Seed = "Improved seeds", Pesticide = "Pesticides (any)")

  observe({
    req(identical(price_mode(), "input"))
    g <- input$input_group
    lv <- input$input_level
    req(isTruthy(g), isTruthy(lv))
    d <- input_series[input_series$group == g & input_series$level == lv, , drop = FALSE]
    ch <- unique(d$item[!is.na(d$item) & nzchar(d$item)])
    if (!length(ch)) ch <- "—"
    prefer <- if (identical(lv, "District")) {
      unname(input_parent[g])
    } else if (identical(g, "Fertilizer")) {
      "Nutrient nitrogen N (total)"
    } else if (identical(g, "Pesticide")) {
      "Pesticides (total)"
    } else {
      "Maize and products"
    }
    fallback <- if (length(prefer) == 1 && prefer %in% ch) prefer else ch[1]
    kept <- price_keep("input_item", ch, fallback)
    if (identical(isolate(in_item_sig()), kept$sig)) return()
    in_item_sig(kept$sig)
    isolate(updateSelectInput(session, "input_item", choices = ch, selected = kept$sel))
  })

  observe({
    req(identical(price_mode(), "input"))
    g <- input$input_group
    lv <- input$input_level
    it <- input$input_item
    req(isTruthy(g), isTruthy(lv), isTruthy(it), !identical(it, "—"))
    d <- input_series[input_series$group == g & input_series$level == lv & input_series$item == it, , drop = FALSE]
    ch <- unique(d$element[!is.na(d$element) & nzchar(d$element)])
    if (!length(ch)) ch <- "—"
    prefer <- if (identical(lv, "District")) {
      "Farmers who used it (%)"
    } else if (identical(g, "Fertilizer")) {
      "Use per area of cropland"
    } else if (identical(g, "Pesticide")) {
      "Agricultural Use"
    } else {
      "Seed"
    }
    fallback <- if (prefer %in% ch) prefer else ch[1]
    kept <- price_keep("input_measure", ch, fallback)
    if (identical(isolate(in_meas_sig()), kept$sig)) return()
    in_meas_sig(kept$sig)
    isolate(updateSelectInput(session, "input_measure", choices = ch, selected = kept$sel))
  })


  price_place <- reactive({
    req(isTRUE(market_tab()))
    place_filter(tab_prices())
  })

  price_scoped <- reactive({
    d <- price_place()
    cat <- input$price_category
    if (isTruthy(cat) && cat != "All categories") d <- d[d$category == cat, , drop = FALSE]
    d
  })

  price_focus <- reactive({
    d <- price_scoped()
    com <- as.character(input$price_commodity)[1]
    if (!isTruthy(com) || identical(com, "—") || is.na(com)) return(d[0, , drop = FALSE])
    d[d$product == com, , drop = FALSE]
  })

  price_rows <- reactive({
    d <- price_scoped()
    q <- input$price_search
    if (is.null(q)) q <- ""
    q <- tolower(trimws(q))
    if (nzchar(q)) {
      hit <- grepl(q, tolower(d$product), fixed = TRUE) | grepl(q, tolower(d$market), fixed = TRUE)
      d <- d[hit, , drop = FALSE]
    }
    if (nrow(d)) d <- d[order(d$date, decreasing = TRUE), , drop = FALSE]
    d
  })

  place_highlight <- reactive({
    d <- tab_prices()
    mkt <- input$price_market
    dist <- input$price_district
    prov <- input$price_province
    if (!is.null(mkt) && mkt != "All markets") return(unique(d$district[d$market == mkt]))
    if (!is.null(dist) && dist != "All districts") return(dist)
    if (!is.null(prov) && prov != "All provinces") return(unique(d$district[d$province == prov]))
    character()
  })

  output$price_head <- renderUI({
    req(identical(input$isoko_tabs, price_mode()))
    req(price_mode() %in% c("food", "live", "input"))
    row <- factor_pick()
    tags$div(
      tags$p(class = "kicker", row$force_label),
      tags$h2(row$label),
      tags$p(class = "lede", row$story)
    )
  })

  pl_cfg <- function(p, zoom = FALSE) {
    plotly::config(p, displaylogo = FALSE, scrollZoom = zoom, modeBarButtonsToRemove = c("lasso2d", "select2d"))
  }
  rise_col <- "#b5502d"
  fall_col <- "#2d6a4f"
  signed_cols <- function(v) ifelse(is.finite(v) & v < 0, fall_col, rise_col)
  div_scale <- list(c(0, "#1b7837"), c(0.5, "#f4f1e6"), c(1, "#b2182b"))
  seq_scale <- list(c(0, "#e8f0df"), c(0.45, "#52b788"), c(1, "#1b4332"))
  pal <- function(n) if (n <= length(factor_cols)) factor_cols[seq_len(n)] else grDevices::colorRampPalette(factor_cols)(n)
  sgn <- function(x, d = 2, suf = "") {
    ifelse(is.finite(x), paste0(ifelse(x > 0, "+", ""), formatC(x, format = "f", digits = d, big.mark = ","), suf), "—")
  }
  arr <- function(x) if (length(x) == 1) I(x) else x
  vline <- function(x, color = "#c6a336", dash = "dash") list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1, yref = "paper", line = list(color = color, dash = dash, width = 2))
  hline <- function(y, color = "#c6a336", dash = "dash") list(type = "line", y0 = y, y1 = y, x0 = 0, x1 = 1, xref = "paper", line = list(color = color, dash = dash, width = 2))
  vband <- function(a, b) list(type = "rect", x0 = a, x1 = b, y0 = 0, y1 = 1, yref = "paper", fillcolor = "rgba(198,163,54,0.16)", line = list(width = 0))
  hband <- function(a, b) list(type = "rect", y0 = a, y1 = b, x0 = 0, x1 = 1, xref = "paper", fillcolor = "rgba(198,163,54,0.16)", line = list(width = 0))
  empty_box <- function(msg) tags$div(class = "empty-note", tags$p(msg))

  geo_df <- get_geo_df()
  geo_mid <- if (nrow(geo_df)) stats::aggregate(cbind(lon, lat) ~ district, data = geo_df, FUN = mean) else data.frame(district = character(), lon = numeric(), lat = numeric())
  all_districts <- if (!is.null(rwanda_geo)) vapply(rwanda_geo$features, function(f) f$properties$shapeName, character(1)) else names(province_of)

  choro <- function(vals, title, scale = seq_scale, zmid = NULL, highlight = character(), source = NULL, height = 560, empty_note = "No published figure") {
    if (is.null(rwanda_geo)) return(empty_box("The district outline file is missing from www/geo."))
    v <- vals[is.finite(vals$value) & vals$district %in% all_districts, , drop = FALSE]
    rest <- setdiff(all_districts, v$district)
    p <- plot_ly(height = height, source = source)
    if (length(rest)) {
      p <- add_trace(
        p, type = "choropleth", geojson = rwanda_geo, featureidkey = "properties.shapeName",
        locations = arr(rest), z = arr(rep(0, length(rest))),
        colorscale = list(c(0, "#eceee8"), c(1, "#eceee8")), showscale = FALSE,
        hovertext = arr(paste0("<b>", rest, "</b><br>", empty_note)), hoverinfo = "text",
        customdata = arr(paste0(rest, ": ", empty_note)),
        marker = list(line = list(width = 0.7, color = "#ffffff"))
      )
    }
    if (nrow(v)) {
      hl <- v$district %in% highlight
      tr <- list(
        p, type = "choropleth", geojson = rwanda_geo, featureidkey = "properties.shapeName",
        locations = arr(v$district), z = arr(v$value), colorscale = scale,
        hovertext = arr(v$hover), hoverinfo = "text", customdata = arr(v$pin),
        marker = list(line = list(width = arr(ifelse(hl, 3, 0.8)), color = arr(ifelse(hl, "#c6a336", "#ffffff")))),
        colorbar = list(title = list(text = title), thickness = 12, len = 0.7)
      )
      if (!is.null(zmid)) tr$zmid <- zmid
      p <- do.call(add_trace, tr)
    }
    if (nrow(geo_mid)) {
      p <- add_trace(
        p, type = "scattergeo", mode = "text", lon = geo_mid$lon, lat = geo_mid$lat, text = geo_mid$district,
        textfont = list(size = 9, color = "#1a1a1a"), hoverinfo = "skip", showlegend = FALSE
      )
    }
    p <- layout(
      p,
      geo = list(fitbounds = "locations", visible = FALSE, projection = list(type = "mercator")),
      margin = list(l = 0, r = 0, t = 0, b = 0),
      paper_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Outfit, Segoe UI, sans-serif", size = 12),
      hoverlabel = list(bgcolor = "#1b4332", font = list(color = "#ffffff", size = 13)),
      dragmode = "pan"
    )
    p <- pl_cfg(p, zoom = TRUE)
    if (!is.null(source)) p <- event_register(p, "plotly_click")
    p
  }

  output$price_total <- renderUI({
    req(isTRUE(market_tab()))
    qty <- suppressWarnings(as.numeric(input$price_qty))
    if (!length(qty) || !is.finite(qty) || qty <= 0) qty <- 1
    com <- as.character(input$price_commodity)[1]
    d <- price_focus()
    if (!nrow(d)) return(tags$p(paste("No published price for", prod_label(com), "in this selection.")))
    ym <- format(d$date, "%Y-%m")
    last <- max(ym)
    snap <- d[ym == last, , drop = FALSE]
    med <- stats::median(snap$price)
    unit <- snap$unit[1]
    when <- as.Date(paste0(last, "-01"))
    prev_ym <- paste0(as.integer(substr(last, 1, 4)) - 1L, substr(last, 5, 7))
    all <- place_filter(tab_prices(), year = FALSE)
    all <- all[all$product == com, , drop = FALSE]
    prev <- all[format(all$date, "%Y-%m") == prev_ym, , drop = FALSE]
    yoy_txt <- if (nrow(prev)) {
      pm <- stats::median(prev$price)
      paste0("Same month a year earlier: ", fmt_num(pm, 0), " RWF. Change: ", sgn(100 * (med / pm - 1), 1, "%"), ".")
    } else {
      "No reading in the same month a year earlier, so no year-on-year change here."
    }
    mkt <- input$price_market
    dist <- input$price_district
    prov <- input$price_province
    catg <- input$price_category
    one_mkt <- !is.null(mkt) && mkt != "All markets"
    xy <- if (one_mkt && nrow(market_xy)) market_xy[market_xy$market == mkt, , drop = FALSE] else market_xy[0, ]
    if (one_mkt && !nrow(xy) && nrow(market_xy)) xy <- market_xy[market_xy$market == mkt & market_xy$district == snap$district[1], , drop = FALSE]
    coord <- if (nrow(xy)) paste0(formatC(xy$lat[1], format = "f", digits = 4), ", ", formatC(xy$lon[1], format = "f", digits = 4)) else "—"
    tile <- function(cls, k, v, n) tags$article(class = paste("calc-tile", cls), tags$span(k), tags$strong(v), tags$p(n))
    bit <- function(k, v) tags$article(class = "detail-tile", tags$span(k), tags$strong(v))
    tagList(
      tags$div(
        class = "calc-tiles",
        tile("tile-unit", "UNIT PRICE", paste(fmt_num(med, 0), "RWF"), paste("per", unit)),
        tile("tile-qty", "QUANTITY", format(qty), unit),
        tile("tile-cost", "TOTAL COST", paste(fmt_num(med * qty, 0), "RWF"), paste(prod_label(com)))
      ),
      tags$div(
        class = "detail-tiles",
        bit("MARKET NAME", if (one_mkt) mkt else paste(length(unique(snap$market)), "markets")),
        bit("PROVINCE", if (!is.null(prov) && prov != "All provinces") prov else paste(length(unique(snap$province)), "provinces")),
        bit("DISTRICT", if (!is.null(dist) && dist != "All districts") dist else paste(length(unique(snap$district)), "districts")),
        bit("CATEGORY", if (!is.null(catg) && catg != "All categories") cap_first(catg) else cap_first(snap$category[1]))
      ),
      tags$div(
        class = "detail-tiles two",
        bit("LAST UPDATED", format(when, "%d/%m/%Y")),
        bit("COORDINATES", coord)
      ),
      tags$p(class = "prod-name", paste0(
        fmt_num(med, 0), " RWF per ", unit, " × ", format(qty), " ", unit, " of ", prod_label(com),
        ". Median of ", nrow(snap), " readings in ", format(when, "%B %Y"), ". ", yoy_txt
      ))
    )
  })

  output$price_pin <- renderUI({
    req(isTRUE(market_tab()))
    com <- as.character(input$price_commodity)[1]
    d <- tab_prices()
    yr <- input$price_year
    if (!is.null(yr) && !identical(yr, "all")) d <- d[d$year == suppressWarnings(as.integer(yr)), , drop = FALSE]
    d <- d[d$product == com, , drop = FALSE]
    if (!nrow(d)) return(tags$p(paste("No market published", com, "for this year.")))
    a <- stats::aggregate(price ~ district + province, data = d, FUN = mean)
    mk <- tapply(d$market, d$district, function(v) paste(sort(unique(v)), collapse = ", "))
    nr <- tapply(d$price, d$district, length)
    unit <- d$unit[1]
    a$value <- a$price
    a$hover <- paste0(
      "<b>", a$district, "</b> · ", a$province, "<br>Markets: ", mk[a$district],
      "<br>Average: ", fmt_num(a$price, 0), " RWF per ", unit, "<br>Readings: ", nr[a$district]
    )
    a$pin <- paste0(a$district, ": ", fmt_num(a$price, 0), " RWF per ", unit, " at ", mk[a$district])
    tagList(
      choro(a, paste("RWF per", unit), seq_scale, highlight = place_highlight(), height = 420, empty_note = paste("No market sells", prod_label(com), "here in this file")),
      tags$p(paste0(nrow(a), " districts and ", length(unique(d$market)), " markets published ", prod_label(com), ". Scroll to zoom, drag to move."))
    )
  })

  market_points <- reactive({
    req(isTRUE(market_tab()))
    com <- as.character(input$price_commodity)[1]
    d <- tab_prices()
    yr <- input$price_year
    if (!is.null(yr) && !identical(yr, "all")) d <- d[d$year == suppressWarnings(as.integer(yr)), , drop = FALSE]
    d <- d[d$product == com, , drop = FALSE]
    if (!nrow(d) || !nrow(market_xy)) return(d[0, ])
    a <- stats::aggregate(price ~ market + district + province, data = d, FUN = mean)
    a$n <- stats::aggregate(price ~ market + district + province, data = d, FUN = length)$price
    a$last <- as.character(tapply(as.character(d$date), paste(d$market, d$district), function(v) max(v)))
    a <- merge(a, market_xy, by = c("market", "district"), all.x = TRUE)
    a$unit <- d$unit[1]
    a$product <- com
    a
  })

  if (has_leaflet) {
    output$price_map <- leaflet::renderLeaflet({
      leaflet::leaflet(options = leaflet::leafletOptions(scrollWheelZoom = TRUE, minZoom = 7, maxZoom = 16)) |>
        leaflet::addTiles() |>
        leaflet::setView(lng = 29.87, lat = -1.94, zoom = 8)
    })
    observe({
      req(isTRUE(market_tab()))
      a <- market_points()
      proxy <- leaflet::leafletProxy("price_map") |> leaflet::clearMarkers() |> leaflet::clearControls()
      if (!nrow(a) || !any(is.finite(a$lat) & is.finite(a$lon))) return()
      ok <- a[is.finite(a$lat) & is.finite(a$lon), , drop = FALSE]
      pal <- leaflet::colorNumeric(c("#95d5b2", "#2d6a4f", "#1b4332"), domain = ok$price)
      pick <- input$price_market
      picked <- !is.null(pick) && pick != "All markets" & ok$market == pick
      lab <- paste0(
        "<b>", ok$market, "</b><br>", ok$district, " · ", ok$province,
        "<br>", prod_label(ok$product[1]), ": ", fmt_num(ok$price, 0), " RWF per ", ok$unit,
        "<br>", ok$n, " readings"
      )
      proxy <- leaflet::addCircleMarkers(
        proxy, lng = ok$lon, lat = ok$lat, radius = pmax(7, 8 + 10 * (ok$price - min(ok$price)) / max(1, diff(range(ok$price)))),
        color = ifelse(picked, "#1d4ed8", "#ffffff"), weight = ifelse(picked, 3, 1),
        fillColor = pal(ok$price), fillOpacity = 0.9,
        popup = lab, label = ok$market
      )
      if (any(picked)) {
        proxy <- leaflet::addMarkers(proxy, lng = ok$lon[picked][1], lat = ok$lat[picked][1], popup = lab[picked][1])
      }
      leaflet::addLegend(proxy, position = "bottomright", pal = pal, values = ok$price, title = paste("RWF /", ok$unit[1]), opacity = 0.9)
    })
  }

  output$price_map_note <- renderUI({
    req(isTRUE(market_tab()))
    a <- market_points()
    n <- if (nrow(a)) sum(is.finite(a$lat)) else 0L
    miss <- if (nrow(a)) sum(!is.finite(a$lat)) else 0L
    tags$p(paste0(
      n, " market locations for ", prod_label(as.character(input$price_commodity)[1]),
      ". Blue pin: the market you picked. Darker, larger dots are dearer. Use + and − or scroll to zoom, drag to move.",
      if (miss) paste0(" ", miss, " market(s) have no published coordinate.") else "",
      " Coordinates are OpenStreetMap lookups of the NAEB / Esoko market names; a few sit at the district centre when the market was not found."
    ))
  })

  output$price_table <- renderUI({
    req(isTRUE(market_tab()))
    d <- price_rows()
    if (!nrow(d)) return(tags$p("No rows in this selection."))
    show <- utils::head(d, 80)
    tagList(
      tags$div(
        class = "price-wrap",
        tags$table(
          class = "price-table",
          tags$thead(tags$tr(lapply(c("Date", "District", "Market", "Product", "Unit", "RWF"), tags$th))),
          tags$tbody(lapply(seq_len(nrow(show)), function(i) {
            tags$tr(
              tags$td(format(show$date[i])), tags$td(show$district[i]), tags$td(show$market[i]),
              tags$td(class = "prod-name", prod_label(show$product[i])),
              tags$td(show$unit[i]), tags$td(fmt_num(show$price[i], 0))
            )
          }))
        )
      ),
      tags$p(paste0("Showing ", nrow(show), " of ", fmt_num(nrow(d), 0), " readings. The CSV has them all. ", naeb_screen_note))
    )
  })

  output$price_csv <- downloadHandler(
    filename = function() paste0("umuganura-naeb-prices-", Sys.Date(), ".csv"),
    content = function(file) {
      d <- price_rows()
      utils::write.csv(d[, c("date", "province", "district", "market", "category", "product", "unit", "price"), drop = FALSE], file, row.names = FALSE)
    }
  )

  viz_note <- function(plot_obj, title, note, width = 1100, height = 460) {
    tagList(tags$h3(title), factor_png(plot_obj, width, height), tags$p(note))
  }

  viz_cols <- function(n) {
    if (n <= length(factor_cols)) factor_cols[seq_len(n)] else grDevices::colorRampPalette(factor_cols)(n)
  }

  white_void <- function() {
    ggplot2::theme_void() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#ffffff", colour = NA),
        legend.text = ggplot2::element_text(size = 10)
      )
  }

  donut_plot <- function(df) {
    df <- df[is.finite(df$value) & df$value > 0, , drop = FALSE]
    df <- df[order(-df$value), , drop = FALSE]
    df$share <- df$value / sum(df$value)
    df$label <- factor(df$label, levels = df$label)
    df$text <- ifelse(df$share >= 0.04, paste0(round(100 * df$share), "%"), "")
    ggplot2::ggplot(df, ggplot2::aes(x = 2, y = share, fill = label)) +
      ggplot2::geom_col(width = 1, colour = "white") +
      ggplot2::geom_text(ggplot2::aes(label = text), position = ggplot2::position_stack(vjust = 0.5), colour = "white", size = 3.8) +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::xlim(0.5, 2.5) +
      ggplot2::scale_fill_manual(values = viz_cols(nrow(df)), name = NULL) +
      white_void()
  }

  district_map_plot <- function(vals, unit) {
    geo_df <- get_geo_df()
    if (!nrow(geo_df)) return(factor_empty_plot("No district outline in www/geo"))
    m <- geo_df
    m$value <- vals$value[match(m$district, vals$district)]
    mid <- stats::aggregate(cbind(lon, lat) ~ district, data = geo_df, FUN = mean)
    mid <- mid[mid$district %in% vals$district[is.finite(vals$value)], , drop = FALSE]
    ggplot2::ggplot(m, ggplot2::aes(lon, lat, group = piece, fill = value)) +
      ggplot2::geom_polygon(colour = "white", linewidth = 0.4) +
      ggplot2::geom_text(data = mid, ggplot2::aes(lon, lat, label = district), inherit.aes = FALSE, size = 2.6, colour = "#1a1a1a") +
      ggplot2::scale_fill_gradient(low = "#d8f3dc", high = "#1b4332", na.value = "#eef1ea", name = unit) +
      ggplot2::coord_equal() +
      white_void()
  }

  input_frame <- reactive({
    req(identical(price_mode(), "input"))
    d <- input_series[
      input_series$group == input$input_group &
        input_series$level == input$input_level &
        input_series$item == input$input_item &
        input_series$element == input$input_measure, , drop = FALSE]
    if (identical(input$input_level, "District")) {
      pv <- input$input_province
      if (!is.null(pv) && pv != "All provinces") d <- d[d$province == pv, , drop = FALSE]
      d <- d[order(-d$value), , drop = FALSE]
    } else {
      lo <- suppressWarnings(as.integer(input$input_from))
      hi <- suppressWarnings(as.integer(input$input_to))
      if (length(lo) == 1 && length(hi) == 1 && is.finite(lo) && is.finite(hi)) {
        d <- d[d$year >= min(lo, hi) & d$year <= max(lo, hi), , drop = FALSE]
      }
      d <- d[order(d$year), , drop = FALSE]
    }
    d
  })

  input_pie_items <- function(g, items) {
    drop <- c("Pesticides (total)", "Cereals - Excluding Beer", "Starchy Roots", "Pulses", "Oilcrops")
    keep <- setdiff(items, drop)
    if (identical(g, "Pesticide")) keep <- intersect(keep, c("Insecticides", "Herbicides", "Fungicides and Bactericides", "Plant Growth Regulators", "Rodenticides"))
    keep
  }

  input_visual <- function(type) {
    d <- input_frame()
    it <- input$input_item
    me <- input$input_measure
    unit <- if (nrow(d)) d$unit[1] else ""
    title <- paste(it, "·", me)
    if (!nrow(d)) return(viz_note(factor_empty_plot("No published figure"), title, "This selection has no published value. The cell stays empty."))
    district <- identical(input$input_level, "District")

    if (district) {
      if (type == "bar") {
        p <- ggplot2::ggplot(d, ggplot2::aes(stats::reorder(district, value), value, fill = province)) +
          ggplot2::geom_col(width = 0.72) +
          ggplot2::coord_flip() +
          ggplot2::scale_fill_manual(values = viz_cols(length(unique(d$province))), name = "Province") +
          ggplot2::labs(x = NULL, y = unit) +
          factor_theme()
        return(viz_note(p, title, "One bar per district. NISR SAS 2026 Season A.", 1100, max(360, 22 * nrow(d) + 60)))
      }
      if (type == "line") {
        return(viz_note(factor_empty_plot("A line needs years"), title, "District figures are one season, 2026 A. Set DATA to National by year to draw a line."))
      }
      if (type == "pie") {
        if (grepl("^Share of", me)) {
          all <- input_series[input_series$group == input$input_group & input_series$level == "District" & input_series$element == me, , drop = FALSE]
          pv <- input$input_province
          if (!is.null(pv) && pv != "All provinces") all <- all[all$province == pv, , drop = FALSE]
          ag <- stats::aggregate(value ~ item, data = all, FUN = mean)
          names(ag)[1] <- "label"
          return(viz_note(donut_plot(ag), me, "Average share across the districts in this selection. Parts add up to the whole."))
        }
        used <- mean(d$value, na.rm = TRUE)
        df <- data.frame(label = c("Used", "Did not use"), value = c(used, max(0, 100 - used)), stringsAsFactors = FALSE)
        return(viz_note(donut_plot(df), title, paste0("Average of ", nrow(d), " districts: ", fmt_num(used, 1), "% used it.")))
      }
      if (type == "scatter") {
        base <- input_series[input_series$group == input$input_group & input_series$level == "District", , drop = FALSE]
        if (grepl("^Share of", me)) {
          par <- unname(input_parent[input$input_group])
          y <- base[base$item == par & base$element == "Farmers who used it (%)", c("district", "value")]
          ylab <- paste(par, "· farmers who used it (%)")
        } else {
          y <- base[base$item == it & base$element == "Land where it was used (%)", c("district", "value")]
          ylab <- paste(it, "· land where it was used (%)")
        }
        names(y)[2] <- "y"
        both <- merge(d[, c("district", "province", "value")], y, by = "district")
        if (nrow(both) < 3) return(viz_note(factor_empty_plot("Need three districts"), title, "A scatter needs at least three districts with both figures."))
        p <- ggplot2::ggplot(both, ggplot2::aes(value, y)) +
          ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "#c6a336", linewidth = 0.9) +
          ggplot2::geom_point(ggplot2::aes(colour = province), size = 3) +
          ggplot2::geom_text(ggplot2::aes(label = district), size = 2.7, vjust = -0.9, colour = "#1a1a1a") +
          ggplot2::scale_colour_manual(values = viz_cols(length(unique(both$province))), name = "Province") +
          ggplot2::labs(x = paste(it, "·", me), y = ylab) +
          factor_theme()
        r <- suppressWarnings(stats::cor(both$value, both$y))
        return(viz_note(p, title, paste0("Each dot is a district. Correlation r = ", format(round(r, 2), nsmall = 2), ". The gold line is a straight-line fit.")))
      }
      p <- district_map_plot(d[, c("district", "value")], unit)
      return(viz_note(p, title, "Darker green is a higher figure. Pale districts are outside the selection or not published.", 1000, 640))
    }

    if (type == "bar") {
      p <- ggplot2::ggplot(d, ggplot2::aes(year, value)) +
        ggplot2::geom_col(fill = "#1b4332", width = 0.75) +
        ggplot2::labs(x = NULL, y = unit) +
        factor_theme()
      return(viz_note(p, title, paste("FAOSTAT, Rwanda national.", min(d$year), "to", max(d$year), ".")))
    }
    if (type == "line") {
      p <- ggplot2::ggplot(d, ggplot2::aes(year, value)) +
        ggplot2::geom_line(colour = "#1b4332", linewidth = 1.1) +
        ggplot2::geom_point(colour = "#1b4332", size = 2) +
        ggplot2::labs(x = NULL, y = unit) +
        factor_theme()
      return(viz_note(p, title, "Published years only. Missing years are not filled."))
    }
    if (type == "pie") {
      yr <- max(d$year)
      all <- input_series[input_series$group == input$input_group & input_series$level == "National" & input_series$element == me & input_series$year == yr, , drop = FALSE]
      all <- all[all$item %in% input_pie_items(input$input_group, unique(all$item)), , drop = FALSE]
      if (nrow(all) < 2) return(viz_note(factor_empty_plot("Needs two parts"), title, "This measure has fewer than two parts in that year."))
      ag <- stats::aggregate(value ~ item, data = all, FUN = sum)
      names(ag)[1] <- "label"
      return(viz_note(donut_plot(ag), paste(me, "·", yr), "Parts of the same measure in the last year of the range. Totals are left out so nothing is counted twice."))
    }
    if (type == "scatter") {
      p <- ggplot2::ggplot(d, ggplot2::aes(year, value)) +
        ggplot2::geom_point(colour = "#1b4332", size = 2.6)
      if (nrow(d) >= 3) p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "#c6a336", linewidth = 0.9)
      p <- p + ggplot2::labs(x = "Year", y = unit) + factor_theme()
      return(viz_note(p, title, "Each dot is a published year. The gold line is a straight-line fit."))
    }
    viz_note(factor_empty_plot("National figure, no district split"), title, "FAOSTAT publishes Rwanda as one figure. Set DATA to By district to draw the map.")
  }


  market_visual <- function(type) {
    com <- as.character(input$price_commodity)[1]
    topn <- suppressWarnings(as.integer(input$price_topn))
    if (!length(topn) || !is.finite(topn)) topn <- 10
    if (type == "pie") {
      s <- price_scoped()
      if (!nrow(s)) return(empty_box("No readings in this selection."))
      all_cats <- is.null(input$price_category) || identical(input$price_category, "All categories")
      key <- if (all_cats) s$category else s$product
      tb <- sort(table(key), decreasing = TRUE)
      if (length(tb) > 9) tb <- c(tb[1:8], Other = sum(tb[-(1:8)]))
      lab <- cap_first(names(tb))
      share <- 100 * as.numeric(tb) / sum(tb)
      p <- plot_ly(
        labels = lab, values = as.numeric(tb), type = "pie", hole = 0.55, sort = FALSE,
        marker = list(colors = pal(length(tb)), line = list(color = "#ffffff", width = 1)),
        textinfo = "label+percent", textposition = "inside",
        hovertext = paste0("<b>", lab, "</b><br>", fmt_num(as.numeric(tb), 0), " readings<br>", formatC(share, format = "f", digits = 2), "% of this selection"),
        hoverinfo = "text", height = 500
      )
      p <- pl_cfg(green_layout(p) |> layout(showlegend = TRUE, legend = list(orientation = "v")))
      return(tagList(
        tags$h3(paste("Pie / donut · price readings by", if (all_cats) "category" else "product")),
        p,
        tags$p(paste0("Share of the ", fmt_num(nrow(s), 0), " published readings in this selection. Hover a slice for the exact count and percentage."))
      ))
    }
    d <- price_focus()
    if (!nrow(d)) return(empty_box(paste("No published price for", com, "in this selection.")))
    unit <- d$unit[1]
    if (type == "bar") {
      one_market <- !is.null(input$price_market) && input$price_market != "All markets"
      if (one_market) {
        s <- price_scoped()
        a <- stats::aggregate(price ~ product + unit, data = s, FUN = mean)
        a$n <- stats::aggregate(price ~ product + unit, data = s, FUN = length)$price
        a$label <- paste0(a$product, " (", a$unit, ")")
        title <- paste("Bar chart · average prices at", input$price_market)
        note <- paste("Highest", topn, "average prices at this market. Units differ by product.")
      } else {
        a <- stats::aggregate(price ~ market + district, data = d, FUN = mean)
        a$n <- stats::aggregate(price ~ market + district, data = d, FUN = length)$price
        a$label <- paste0(a$market, " · ", a$district)
        a$unit <- unit
        title <- paste("Bar chart ·", com, "by market location")
        note <- paste0("Average ", com, " price per ", unit, " at each market. Highest ", topn, " shown.")
      }
      a <- utils::head(a[order(-a$price), , drop = FALSE], topn)
      a <- a[order(a$price), , drop = FALSE]
      p <- plot_ly(
        a, x = ~price, y = ~label, type = "bar", orientation = "h", height = max(360, 30 * nrow(a) + 90),
        marker = list(color = "#2d6a4f"), text = fmt_num(a$price, 0), textposition = "outside", cliponaxis = FALSE,
        hovertext = paste0("<b>", a$label, "</b><br>", fmt_num(a$price, 0), " RWF per ", a$unit, "<br>", a$n, " readings"),
        hoverinfo = "text"
      )
      p <- pl_cfg(green_layout(p) |> layout(
        yaxis = list(title = "", categoryorder = "array", categoryarray = a$label),
        xaxis = list(title = "Average price (RWF)"), margin = list(l = 200)
      ))
      return(tagList(tags$h3(title), p, tags$p(note)))
    }
    if (type == "line") {
      mo <- stats::aggregate(price ~ year + month, data = d, FUN = stats::median)
      mo$date <- as.Date(sprintf("%d-%02d-01", mo$year, mo$month))
      mo <- mo[order(mo$date), , drop = FALSE]
      nat <- tab_prices()
      nat <- nat[nat$product == com, , drop = FALSE]
      yr <- input$price_year
      if (!is.null(yr) && !identical(yr, "all")) nat <- nat[nat$year == suppressWarnings(as.integer(yr)), , drop = FALSE]
      nm <- stats::aggregate(price ~ year + month, data = nat, FUN = stats::median)
      nm$date <- as.Date(sprintf("%d-%02d-01", nm$year, nm$month))
      nm <- nm[order(nm$date), , drop = FALSE]
      p <- plot_ly(height = 460) |>
        add_trace(
          x = mo$date, y = mo$price, type = "scatter", mode = "lines+markers", name = "This selection",
          line = list(color = "#1b4332", width = 3), marker = list(size = 7),
          hovertext = paste0(format(mo$date, "%b %Y"), "<br>This selection: ", fmt_num(mo$price, 0), " RWF per ", unit), hoverinfo = "text"
        )
      if (nrow(nm) && length(place_highlight())) {
        p <- add_trace(
          p, x = nm$date, y = nm$price, type = "scatter", mode = "lines", inherit = FALSE, name = "Rwanda, all markets",
          line = list(color = "#c6a336", width = 2, dash = "dash"),
          hovertext = paste0(format(nm$date, "%b %Y"), "<br>Rwanda: ", fmt_num(nm$price, 0), " RWF per ", unit), hoverinfo = "text"
        )
      }
      p <- pl_cfg(green_layout(p) |> layout(xaxis = list(title = ""), yaxis = list(title = paste("Median RWF per", unit)), legend = list(orientation = "h", y = -0.15)))
      return(tagList(tags$h3(paste("Line ·", com, "month by month")), p, tags$p("Median of the readings in each month. Months with no reading are left out, not filled.")))
    }
    if (type == "scatter") {
      top <- names(sort(table(d$district), decreasing = TRUE))[seq_len(min(8, length(unique(d$district))))]
      d$grp <- ifelse(d$district %in% top, d$district, "Other districts")
      grps <- unique(d$grp)
      cols <- pal(length(grps))
      p <- plot_ly(height = 480)
      for (i in seq_along(grps)) {
        s <- d[d$grp == grps[i], , drop = FALSE]
        p <- add_trace(
          p, x = s$date, y = s$price, type = "scatter", mode = "markers", name = grps[i],
          marker = list(color = cols[i], size = 7, opacity = 0.75),
          hovertext = paste0("<b>", s$market, "</b> · ", s$district, "<br>", format(s$date), "<br>", fmt_num(s$price, 0), " RWF per ", unit),
          hoverinfo = "text"
        )
      }
      p <- pl_cfg(green_layout(p) |> layout(xaxis = list(title = ""), yaxis = list(title = paste("RWF per", unit)), legend = list(orientation = "h", y = -0.15)))
      return(tagList(tags$h3(paste("Scatter ·", com, "readings")), p, tags$p(paste0("Each dot is one published reading. ", fmt_num(nrow(d), 0), " readings, coloured by district."))))
    }
    m <- tab_prices()
    m <- m[m$product == com, , drop = FALSE]
    yr <- input$price_year
    if (!is.null(yr) && !identical(yr, "all")) m <- m[m$year == suppressWarnings(as.integer(yr)), , drop = FALSE]
    a <- stats::aggregate(price ~ district + province, data = m, FUN = mean)
    nmk <- tapply(m$market, m$district, function(v) length(unique(v)))
    a$value <- a$price
    a$hover <- paste0("<b>", a$district, "</b> · ", a$province, "<br>Average ", com, ": ", fmt_num(a$price, 0), " RWF per ", unit, "<br>", nmk[a$district], " market(s)")
    a$pin <- paste0(a$district, ": ", fmt_num(a$price, 0), " RWF per ", unit)
    tagList(
      tags$h3(paste("Map ·", com, "average price by district")),
      choro(a, paste("RWF per", unit), seq_scale, highlight = place_highlight(), height = 600, empty_note = "No published price"),
      tags$p("Darker green is a higher average price. Grey districts have no reading. Scroll to zoom in and out, drag to move.")
    )
  }

  output$price_visual <- renderUI({
    req(identical(input$isoko_tabs, price_mode()))
    req(price_mode() %in% c("input", "food", "live"))
    type <- as.character(input$price_viz)[1]
    if (is.na(type) || !nzchar(type)) type <- "bar"
    if (identical(price_mode(), "input")) input_visual(type) else market_visual(type)
  })


  period_months <- reactive({
    p <- input$infl_period
    if (is.null(p) || identical(p, "ytd")) return(naeb_months)
    m <- suppressWarnings(as.integer(p))
    if (length(m) == 1 && is.finite(m)) m else naeb_months
  })


  infl_scope_val <- reactive({
    s <- input$infl_scope
    if (is.null(s) || !nzchar(s)) "all" else s
  })

  infl_rows <- reactive({
    mode <- price_mode()
    req(identical(input$isoko_tabs, mode))
    req(mode %in% c("input", "food", "live"))
    sc <- infl_scope_val()
    m <- naeb_month[naeb_month$tab == mode, , drop = FALSE]
    if (startsWith(sc, "cat:")) {
      m <- m[m$category == substring(sc, 5), , drop = FALSE]
    } else if (startsWith(sc, "prod:")) {
      m <- m[m$product == substring(sc, 6), , drop = FALSE]
    } else if (identical(sc, "selected")) {
      m <- m[m$product == as.character(input$price_commodity)[1], , drop = FALSE]
    } else if (identical(sc, "seed")) {
      m <- m[0, , drop = FALSE]
    }
    pv <- if (identical(mode, "input")) input$input_province else input$price_province
    if (!is.null(pv) && pv != "All provinces") m <- m[m$province == pv, , drop = FALSE]
    m
  })

  infl_key <- reactive(list(
    price_mode(), infl_scope_val(),
    if (identical(infl_scope_val(), "selected")) as.character(input$price_commodity)[1] else "",
    if (identical(price_mode(), "input")) input$input_province else input$price_province
  ))

  infl_calc <- reactive(infl_core(infl_rows(), price_mode(), period_months())) |>
    bindCache(infl_key(), period_months())

  infl_monthly <- reactive({
    type <- as.character(input$infl_chart)[1]
    req(identical(type, "line"))
    m <- infl_rows()
    mode <- price_mode()
    out <- lapply(naeb_months, function(mm) {
      r <- infl_core(m, mode, mm)
      if (is.null(r)) NULL else list(month = mm, r = r)
    })
    Filter(Negate(is.null), out)
  }) |>
    bindCache(infl_key(), input$infl_chart)

  scope_label <- reactive({
    sc <- infl_scope_val()
    if (identical(sc, "selected")) return(as.character(input$price_commodity)[1])
    ch <- scope_choices(price_mode())
    nm <- names(ch)[ch == sc]
    if (length(nm)) nm[1] else "Selection"
  })

  place_label <- reactive({
    pv <- if (identical(price_mode(), "input")) input$input_province else input$price_province
    if (is.null(pv) || pv == "All provinces") "Rwanda" else if (pv == "Kigali") "Kigali City" else paste(pv, "Province")
  })

  infl_highlight <- reactive({
    if (identical(price_mode(), "input")) character() else place_highlight()
  })

  infl_empty <- function() {
    if (identical(infl_scope_val(), "seed")) {
      return("No seed price is published in the NAEB / Esoko market file, so seed inflation and its contribution stay empty. Seed use by district (SAS 2026 A) is in the chart above.")
    }
    "No product in this basket has prices in both years for these months and places. The cell stays empty."
  }

  part_of <- function(r) {
    pd <- r$pd
    if (length(unique(pd$category)) > 1) return(list(col = "category", lab = cap_first(pd$category), word = "category"))
    lab <- pd$product
    top <- names(sort(tapply(abs(pd$to_nat), lab, sum), decreasing = TRUE))
    top <- top[seq_len(min(8, length(top)))]
    lab[!lab %in% top] <- "Other products"
    list(col = "product", lab = lab, word = "product")
  }

  output$infl_head <- renderUI({
    mode <- price_mode()
    req(identical(input$isoko_tabs, mode))
    req(mode %in% c("input", "food", "live"))
    weights <- if (identical(mode, "input")) {
      "Inside each district, DAP, NPK, and Urea are weighted by the share of farmers who used each fertilizer, and Dithane by its share of pesticide use (NISR SAS 2026 A)."
    } else {
      "Every product in the basket has the same weight inside a district."
    }
    tags$div(
      tags$h3(paste0("How ", tolower(scope_label()), " prices moved, and which districts drove it")),
      tags$p(paste(
        "NAEB / Esoko market prices,", paste0(naeb_span, "."),
        "Year-on-year inflation compares the average price over the chosen months with the same months a year earlier.",
        weights,
        "National inflation is the average of district inflation. A district's contribution is its inflation divided by the number of districts with prices, so contributions add up to the national figure.",
        naeb_screen_note
      ))
    )
  })

  output$infl_cards <- renderUI({
    req(identical(input$isoko_tabs, price_mode()))
    req(price_mode() %in% c("input", "food", "live"))
    r <- infl_calc()
    card <- function(k, v, n) tags$article(class = "lux-card", tags$span(k), tags$strong(v), tags$p(n))
    wb <- factor_data[factor_data$id == "inflation" & is.finite(factor_data$value), , drop = FALSE]
    wb_card <- if (nrow(wb)) {
      l <- wb[which.max(wb$year), ]
      card("Headline CPI", fmt_pct(l$value, 1), paste("Rwanda, all items, World Bank ·", l$year))
    } else {
      card("Headline CPI", "—", "Not in the World Bank extract")
    }
    if (is.null(r)) {
      return(tags$div(class = "mega-row five glance", card(scope_label(), "—", infl_empty()), card("Highest district", "—", ""), card("Lowest district", "—", ""), card("Biggest driver", "—", ""), wb_card))
    }
    hi <- r$dist[1, ]
    lo <- r$dist[nrow(r$dist), ]
    top <- r$prod[which.max(abs(r$prod$to_nat)), ]
    tags$div(
      class = "mega-row five glance",
      card(paste(scope_label(), "inflation"), sgn(r$national, 1, "%"), paste0(place_label(), " · ", r$D, " districts · ", period_text(r$months))),
      card("Highest district", sgn(hi$yoy, 1, "%"), paste0(hi$district, " · rank 1 of ", r$D)),
      card("Lowest district", sgn(lo$yoy, 1, "%"), paste0(lo$district, " · rank ", lo$rank, " of ", r$D)),
      card("Biggest driver", sgn(top$to_nat, 2, " pts"), paste0(top$product, " · ", sgn(top$yoy, 1, "%"), " year on year")),
      wb_card
    )
  })

  yoy_plot <- function(r, type) {
    per <- period_text(r$months)
    hl <- infl_highlight()
    d <- r$dist
    hov <- function(x) paste0("<b>", x$district, "</b> · ", x$province, "<br>Inflation: ", sgn(x$yoy, 2, "%"), "<br>Rank ", x$rank, " of ", r$D, "<br>", x$products, " products · ", fmt_num(x$n, 0), " readings")
    pin <- function(x) paste0(x$district, " (", x$province, "): ", sgn(x$yoy, 2, "%"), " year on year, rank ", x$rank, " of ", r$D, ", ", x$products, " products, ", fmt_num(x$n, 0), " readings")
    if (type == "bar") {
      d <- d[order(d$yoy), , drop = FALSE]
      p <- plot_ly(
        d, x = ~yoy, y = ~district, type = "bar", orientation = "h", source = "infl", height = max(380, 26 * nrow(d) + 100),
        marker = list(color = signed_cols(d$yoy), line = list(color = ifelse(d$district %in% hl, "#c6a336", "rgba(0,0,0,0)"), width = 3)),
        text = sgn(d$yoy, 1, "%"), textposition = "outside", cliponaxis = FALSE,
        hovertext = hov(d), hoverinfo = "text", customdata = pin(d)
      )
      p <- green_layout(p) |> layout(
        yaxis = list(title = "", categoryorder = "array", categoryarray = d$district),
        xaxis = list(title = "Year-on-year change (%)", ticksuffix = "%"),
        shapes = list(vline(r$national)),
        annotations = list(list(x = r$national, y = 1.03, yref = "paper", text = paste("National", sgn(r$national, 1, "%")), showarrow = FALSE, font = list(color = "#8a6d1d", size = 12)))
      )
      return(tagList(
        tags$h3(paste("Inflation by district, ranked ·", per)),
        event_register(pl_cfg(p), "plotly_click"),
        tags$p("Highest at the top. Red is a price rise, green a fall. The gold line is national inflation. Hover or click a bar for the exact figure.")
      ))
    }
    if (type == "line") {
      mo <- infl_monthly()
      if (!length(mo)) return(empty_box("No month has prices in both years."))
      lab_m <- function(m) paste(month.abb[m], naeb_cur_year)
      xs <- lab_m(naeb_months)
      nat <- data.frame(month = vapply(mo, function(z) z$month, numeric(1)), value = vapply(mo, function(z) z$r$national, numeric(1)))
      dl <- do.call(rbind, lapply(mo, function(z) data.frame(month = z$month, district = z$r$dist$district, value = z$r$dist$yoy, n = z$r$dist$n, stringsAsFactors = FALSE)))
      top <- names(sort(tapply(dl$n, dl$district, sum), decreasing = TRUE))
      top <- unique(c(intersect(hl, top), top))[seq_len(min(6, length(top)))]
      cols <- pal(length(top))
      p <- plot_ly(source = "infl", height = 480)
      for (i in seq_along(top)) {
        s <- dl[dl$district == top[i], , drop = FALSE]
        p <- add_trace(
          p, x = lab_m(s$month), y = s$value, type = "scatter", mode = "lines+markers", name = top[i],
          line = list(color = cols[i], width = 1.6), marker = list(size = 6),
          hovertext = paste0("<b>", top[i], "</b><br>", lab_m(s$month), ": ", sgn(s$value, 2, "%")), hoverinfo = "text",
          customdata = paste0(top[i], ", ", lab_m(s$month), ": ", sgn(s$value, 2, "%"), " year on year")
        )
      }
      p <- add_trace(
        p, x = lab_m(nat$month), y = nat$value, type = "scatter", mode = "lines+markers", name = "National (average of districts)",
        line = list(color = "#1b4332", width = 4), marker = list(size = 9),
        hovertext = paste0("<b>National</b><br>", lab_m(nat$month), ": ", sgn(nat$value, 2, "%")), hoverinfo = "text",
        customdata = paste0("National, ", lab_m(nat$month), ": ", sgn(nat$value, 2, "%"), " year on year")
      )
      p <- green_layout(p) |> layout(
        xaxis = list(title = "", categoryorder = "array", categoryarray = xs),
        yaxis = list(title = "Year-on-year change (%)", ticksuffix = "%"),
        shapes = list(hline(0, "#999999", "dot")),
        legend = list(orientation = "h", y = -0.18)
      )
      return(tagList(
        tags$h3(paste("Inflation month by month,", naeb_cur_year, "vs", naeb_base_year)),
        event_register(pl_cfg(p), "plotly_click"),
        tags$p("Each month is compared with the same month a year earlier. The thick line is national; thin lines are the six districts with the most readings.")
      ))
    }
    if (type == "pie") {
      many <- nrow(r$prod) >= 3
      x <- if (many) r$prod else r$dist
      nm <- if (many) x$product else x$district
      cls <- ifelse(x$yoy > 1, "Rose more than 1%", ifelse(x$yoy < -1, "Fell more than 1%", "Held within ±1%"))
      lv <- c("Rose more than 1%", "Held within ±1%", "Fell more than 1%")
      cnt <- table(factor(cls, levels = lv))
      keep <- cnt > 0
      lst <- vapply(lv, function(k) {
        s <- which(cls == k)
        s <- s[order(-abs(x$yoy[s]))]
        txt <- paste0(nm[s], " ", sgn(x$yoy[s], 1, "%"))
        paste(c(utils::head(txt, 12), if (length(txt) > 12) paste("and", length(txt) - 12, "more")), collapse = "<br>")
      }, character(1))
      word <- if (many) "products" else "districts"
      share <- 100 * as.numeric(cnt) / sum(cnt)
      p <- plot_ly(
        labels = lv[keep], values = as.numeric(cnt)[keep], type = "pie", hole = 0.55, sort = FALSE, source = "infl", height = 500,
        marker = list(colors = c(rise_col, "#c6a336", fall_col)[keep], line = list(color = "#ffffff", width = 1)),
        textinfo = "label+percent",
        hovertext = paste0("<b>", lv[keep], "</b><br>", as.numeric(cnt)[keep], " ", word, " (", formatC(share[keep], format = "f", digits = 1), "%)<br>", lst[keep]),
        hoverinfo = "text",
        customdata = paste0(lv[keep], ": ", as.numeric(cnt)[keep], " ", word, " (", formatC(share[keep], format = "f", digits = 1), "%)")
      )
      return(tagList(
        tags$h3(paste("How many", word, "got dearer ·", per)),
        event_register(pl_cfg(green_layout(p)), "plotly_click"),
        tags$p(paste0("Each slice counts ", word, " by their year-on-year change. Hover a slice to see which ones and by how much."))
      ))
    }
    if (type == "scatter") {
      many <- nrow(r$prod) >= 3
      s <- if (many) r$prod else r$pd
      lab <- if (many) s$product else s$district
      rng <- range(c(s$price_0, s$price_1), na.rm = TRUE)
      p <- plot_ly(source = "infl", height = 540) |>
        add_trace(
          x = s$price_0, y = s$price_1, type = "scatter", mode = "markers", name = if (many) "Products" else "Districts",
          marker = list(size = 11, color = signed_cols(s$yoy), line = list(color = "#ffffff", width = 1)),
          hovertext = paste0("<b>", lab, "</b><br>", naeb_base_year, ": ", fmt_num(s$price_0, 0), " RWF per ", s$unit, "<br>", naeb_cur_year, ": ", fmt_num(s$price_1, 0), "<br>Change: ", sgn(s$yoy, 2, "%")),
          hoverinfo = "text",
          customdata = paste0(lab, ": ", fmt_num(s$price_0, 0), " to ", fmt_num(s$price_1, 0), " RWF per ", s$unit, " (", sgn(s$yoy, 2, "%"), ")")
        ) |>
        add_trace(x = rng, y = rng, type = "scatter", mode = "lines", inherit = FALSE, name = "No change", line = list(color = "#c6a336", dash = "dash"), hoverinfo = "skip")
      p <- green_layout(p) |> layout(
        xaxis = list(type = "log", title = paste("Average price,", naeb_base_year, "(RWF)")),
        yaxis = list(type = "log", title = paste("Average price,", naeb_cur_year, "(RWF)")),
        showlegend = FALSE
      )
      return(tagList(
        tags$h3(paste("Price then and now ·", per)),
        event_register(pl_cfg(p), "plotly_click"),
        tags$p(paste0("Each dot is a ", if (many) "product, averaged across districts" else "district", ". Above the gold line the price rose, below it fell. Log scale, so cheap and dear items fit on one chart."))
      ))
    }
    v <- data.frame(district = d$district, value = d$yoy, hover = hov(d), pin = pin(d), stringsAsFactors = FALSE)
    tagList(
      tags$h3(paste("Map of district inflation ·", per)),
      choro(v, "YoY %", div_scale, zmid = 0, highlight = hl, source = "infl", height = 620, empty_note = "No price in both years"),
      tags$p("Red districts saw prices rise the most, green ones saw them fall. Grey means no price in both years. Scroll to zoom in and out, drag to move, click a district to pin its figure.")
    )
  }

  contrib_plot <- function(r, type) {
    per <- period_text(r$months)
    hl <- infl_highlight()
    d <- r$dist
    share_txt <- function(x) ifelse(is.finite(x), paste0(formatC(x, format = "f", digits = 1), "% of national"), "")
    hov <- function(x) paste0("<b>", x$district, "</b> · ", x$province, "<br>Contribution: ", sgn(x$to_nat, 3, " pts"), "<br>", share_txt(x$share), "<br>District inflation: ", sgn(x$yoy, 2, "%"), " · rank ", x$rank, " of ", r$D)
    pin <- function(x) paste0(x$district, ": ", sgn(x$to_nat, 3, " points"), " of national inflation (", share_txt(x$share), "); district inflation ", sgn(x$yoy, 2, "%"))
    if (type == "bar") {
      pt <- part_of(r)
      pd <- r$pd
      pd$part <- pt$lab
      agg <- stats::aggregate(to_nat ~ district + part, data = pd, FUN = sum)
      ord <- d$district[order(d$to_nat)]
      parts <- unique(agg$part[order(-abs(agg$to_nat))])
      cols <- pal(length(parts))
      p <- plot_ly(source = "infl", height = max(400, 26 * length(ord) + 130))
      for (i in seq_along(parts)) {
        a <- agg[agg$part == parts[i], , drop = FALSE]
        tot <- d$to_nat[match(a$district, d$district)]
        p <- add_trace(
          p, x = a$to_nat, y = a$district, type = "bar", orientation = "h", name = parts[i],
          marker = list(color = cols[i]),
          hovertext = paste0("<b>", a$district, "</b><br>", parts[i], ": ", sgn(a$to_nat, 3, " pts"), "<br>District total: ", sgn(tot, 3, " pts"), "<br>National: ", sgn(r$national, 2, "%")),
          hoverinfo = "text",
          customdata = paste0(a$district, ", ", parts[i], ": ", sgn(a$to_nat, 3, " points"), " of national inflation")
        )
      }
      p <- add_trace(
        p, x = d$to_nat, y = d$district, type = "scatter", mode = "text", text = sgn(d$to_nat, 2, ""),
        textposition = ifelse(d$to_nat < 0, "middle left", "middle right"), showlegend = FALSE, hoverinfo = "skip",
        textfont = list(color = "#1a1a1a", size = 11)
      )
      p <- green_layout(p) |> layout(
        barmode = "relative",
        yaxis = list(title = "", categoryorder = "array", categoryarray = ord),
        xaxis = list(title = "Contribution to national inflation (percentage points)"),
        legend = list(orientation = "h", y = -0.12)
      )
      return(tagList(
        tags$h3(paste("Each district's part of national inflation ·", per)),
        event_register(pl_cfg(p), "plotly_click"),
        tags$p(paste0(
          "National inflation is ", sgn(r$national, 2, "%"), ", the sum of every bar. A bar is a district's inflation divided by ", r$D,
          " districts, split by ", pt$word, ". Ranked from the biggest push at the top. Hover or click for the exact points."
        ))
      ))
    }
    if (type == "line") {
      mo <- infl_monthly()
      if (!length(mo)) return(empty_box("No month has prices in both years."))
      lab_m <- function(m) paste(month.abb[m], naeb_cur_year)
      pt <- part_of(r)
      keep <- unique(pt$lab)
      cl <- do.call(rbind, lapply(mo, function(z) {
        pd <- z$r$pd
        part <- if (pt$col == "category") cap_first(pd$category) else ifelse(pd$product %in% keep, pd$product, "Other products")
        a <- stats::aggregate(pd$to_nat, by = list(part = part), FUN = sum)
        names(a)[2] <- "to_nat"
        a$month <- z$month
        a
      }))
      nat <- data.frame(month = vapply(mo, function(z) z$month, numeric(1)), value = vapply(mo, function(z) z$r$national, numeric(1)))
      parts <- unique(cl$part)
      cols <- pal(length(parts))
      p <- plot_ly(source = "infl", height = 480)
      for (i in seq_along(parts)) {
        s <- cl[cl$part == parts[i], , drop = FALSE]
        s <- s[order(s$month), , drop = FALSE]
        p <- add_trace(
          p, x = lab_m(s$month), y = s$to_nat, type = "scatter", mode = "lines+markers", name = parts[i],
          line = list(color = cols[i], width = 2), marker = list(size = 6),
          hovertext = paste0("<b>", parts[i], "</b><br>", lab_m(s$month), ": ", sgn(s$to_nat, 3, " pts")), hoverinfo = "text",
          customdata = paste0(parts[i], ", ", lab_m(s$month), ": ", sgn(s$to_nat, 3, " points"), " of national inflation")
        )
      }
      p <- add_trace(
        p, x = lab_m(nat$month), y = nat$value, type = "scatter", mode = "lines+markers", name = "National inflation",
        line = list(color = "#1b4332", width = 4), marker = list(size = 9),
        hovertext = paste0("<b>National</b><br>", lab_m(nat$month), ": ", sgn(nat$value, 2, "%")), hoverinfo = "text",
        customdata = paste0("National, ", lab_m(nat$month), ": ", sgn(nat$value, 2, "%"))
      )
      p <- green_layout(p) |> layout(
        xaxis = list(title = "", categoryorder = "array", categoryarray = lab_m(naeb_months)),
        yaxis = list(title = "Percentage points"),
        shapes = list(hline(0, "#999999", "dot")),
        legend = list(orientation = "h", y = -0.18)
      )
      return(tagList(
        tags$h3(paste("Who drove national inflation, month by month")),
        event_register(pl_cfg(p), "plotly_click"),
        tags$p(paste0("Each thin line is one ", pt$word, "'s contribution in points. In every month they add up to the thick national line."))
      ))
    }
    if (type == "pie") {
      x <- d[order(-abs(d$to_nat)), , drop = FALSE]
      pos <- x$to_nat >= 0
      cols <- character(nrow(x))
      if (any(pos)) cols[pos] <- grDevices::colorRampPalette(c("#7a2e17", "#e7b08f"))(sum(pos))
      if (any(!pos)) cols[!pos] <- grDevices::colorRampPalette(c("#1b4332", "#a7d7b8"))(sum(!pos))
      p <- plot_ly(
        labels = x$district, values = abs(x$to_nat), type = "pie", hole = 0.5, sort = FALSE, source = "infl", height = 560,
        marker = list(colors = cols, line = list(color = "#ffffff", width = 1)),
        textinfo = "label+percent", textposition = "inside", insidetextorientation = "radial",
        hovertext = hov(x), hoverinfo = "text", customdata = pin(x)
      )
      return(tagList(
        tags$h3(paste("Share of national inflation by district ·", per)),
        event_register(pl_cfg(green_layout(p) |> layout(showlegend = FALSE)), "plotly_click"),
        tags$p("Slice size is how hard each district pushed national inflation. Red-brown slices pushed prices up, green slices pulled them down. Hover or click a slice for the signed points and the share.")
      ))
    }
    if (type == "scatter") {
      s <- r$prod
      p <- plot_ly(source = "infl", height = 520) |>
        add_trace(
          x = s$yoy, y = s$to_nat, type = "scatter", mode = "markers",
          marker = list(size = 8 + 2 * sqrt(s$districts), color = signed_cols(s$to_nat), opacity = 0.85, line = list(color = "#ffffff", width = 1)),
          hovertext = paste0("<b>", s$product, "</b> · ", s$category, "<br>Year on year: ", sgn(s$yoy, 2, "%"), "<br>Contribution: ", sgn(s$to_nat, 3, " pts"), "<br>Priced in ", s$districts, " districts"),
          hoverinfo = "text",
          customdata = paste0(s$product, ": ", sgn(s$yoy, 2, "%"), " year on year, ", sgn(s$to_nat, 3, " points"), " of national inflation, ", s$districts, " districts")
        )
      p <- green_layout(p) |> layout(
        xaxis = list(title = "Year-on-year change (%)", ticksuffix = "%", zeroline = TRUE),
        yaxis = list(title = "Contribution to national inflation (points)", zeroline = TRUE),
        showlegend = FALSE
      )
      return(tagList(
        tags$h3(paste("Products: change against contribution ·", per)),
        event_register(pl_cfg(p), "plotly_click"),
        tags$p("Each dot is a product. Further right means a bigger price change; higher means a bigger push on national inflation. Bigger dots are priced in more districts.")
      ))
    }
    v <- data.frame(district = d$district, value = d$to_nat, hover = hov(d), pin = pin(d), stringsAsFactors = FALSE)
    tagList(
      tags$h3(paste("Map of each district's contribution ·", per)),
      choro(v, "Points", div_scale, zmid = 0, highlight = hl, source = "infl", height = 620, empty_note = "No price in both years"),
      tags$p("Deep red districts contribute the most to national inflation, green ones pull it down. Grey means no price in both years. Scroll to zoom in and out, drag to move, click a district to pin its figure.")
    )
  }

  output$infl_visual <- renderUI({
    req(identical(input$isoko_tabs, price_mode()))
    req(price_mode() %in% c("input", "food", "live"))
    r <- infl_calc()
    if (is.null(r)) return(empty_box(infl_empty()))
    type <- as.character(input$infl_chart)[1]
    if (is.na(type) || !nzchar(type)) type <- "bar"
    if (identical(input$infl_view, "contrib")) contrib_plot(r, type) else yoy_plot(r, type)
  })

  infl_pin <- reactiveVal(NULL)
  observeEvent(if (identical(input$isoko_tabs, price_mode()) && price_mode() %in% c("input", "food", "live")) suppressWarnings(event_data("plotly_click", source = "infl")), {
    ev <- suppressWarnings(event_data("plotly_click", source = "infl"))
    infl_pin(if (!is.null(ev$customdata)) as.character(ev$customdata)[1] else NULL)
  })
  observeEvent(list(price_mode(), input$infl_view, input$infl_scope, input$infl_period, input$infl_chart, input$price_province, input$price_district), infl_pin(NULL), ignoreInit = TRUE)

  output$infl_click <- renderUI({
    req(identical(input$isoko_tabs, price_mode()))
    req(price_mode() %in% c("input", "food", "live"))
    pin <- infl_pin()
    if (is.null(pin)) return(tags$p(class = "click-pin muted", "Click any bar, point, slice, or district to pin its exact figure here."))
    tags$p(class = "click-pin", tags$strong("Pinned: "), pin)
  })

  output$infl_rank <- renderUI({
    req(identical(input$isoko_tabs, price_mode()))
    req(price_mode() %in% c("input", "food", "live"))
    r <- infl_calc()
    if (is.null(r)) return(tags$p(infl_empty()))
    d <- r$dist
    tags$div(
      style = "overflow:auto; max-height:460px;",
      tags$table(
        class = "price-table",
        tags$thead(tags$tr(lapply(c("Rank", "District", "Province", "Inflation (%)", "Contribution (points)", "Share of national (%)", "Products", "Readings"), tags$th))),
        tags$tbody(lapply(seq_len(nrow(d)), function(i) {
          tags$tr(
            tags$td(d$rank[i]), tags$td(d$district[i]), tags$td(d$province[i]),
            tags$td(sgn(d$yoy[i], 2)), tags$td(sgn(d$to_nat[i], 3)),
            tags$td(if (is.finite(d$share[i])) formatC(d$share[i], format = "f", digits = 1) else "—"),
            tags$td(d$products[i]), tags$td(fmt_num(d$n[i], 0))
          )
        }))
      )
    )
  })

  output$input_glance <- renderUI({
    req(identical(input$isoko_tabs, "input"))
    req(identical(price_mode(), "input"))
    d <- input_frame()
    unit <- if (nrow(d)) d$unit[1] else ""
    card <- function(k, v, n) tags$article(class = "lux-card", tags$span(k), tags$strong(v), tags$p(n))
    if (!nrow(d)) {
      return(tags$div(class = "mega-row glance", card("Selected", "—", "No published value"), card("Coverage", "—", ""), card("Spread", "—", "")))
    }
    if (identical(input$input_level, "District")) {
      hi <- d[which.max(d$value), ]
      lo <- d[which.min(d$value), ]
      return(tags$div(
        class = "mega-row glance",
        card("Highest", paste(fmt_num(hi$value, 1), unit), hi$district),
        card("Lowest", paste(fmt_num(lo$value, 1), unit), lo$district),
        card("Districts", as.character(nrow(d)), "NISR SAS 2026 Season A")
      ))
    }
    last <- d[nrow(d), ]
    first <- d[1, ]
    delta <- last$value - first$value
    tags$div(
      class = "mega-row glance",
      card("Latest", paste(fmt_num(last$value, 2), unit), paste("In", last$year)),
      card("Period", paste(first$year, "–", last$year), paste(nrow(d), "published years")),
      card("Change", paste0(if (delta > 0) "+" else "", fmt_num(delta, 2)), paste(first$year, "to", last$year))
    )
  })

  output$input_table <- renderUI({
    req(identical(input$isoko_tabs, "input"))
    req(identical(price_mode(), "input"))
    d <- input_frame()
    if (!nrow(d)) return(tags$p("No rows in this selection."))
    tags$div(
      style = "overflow:auto; max-height:440px;",
      tags$table(
        class = "price-table",
        tags$thead(tags$tr(lapply(c("Year", "Province", "District", "Input", "Item", "Measure", "Value", "Unit"), tags$th))),
        tags$tbody(lapply(seq_len(nrow(d)), function(i) {
          tags$tr(
            tags$td(d$year[i]), tags$td(d$province[i]), tags$td(d$district[i]),
            tags$td(d$group[i]), tags$td(d$item[i]), tags$td(d$element[i]),
            tags$td(fmt_num(d$value[i], 2)), tags$td(d$unit[i])
          )
        }))
      )
    )
  })

  output$input_csv <- downloadHandler(
    filename = function() paste0("umuganura-inputs-", Sys.Date(), ".csv"),
    content = function(file) {
      d <- input_frame()
      utils::write.csv(d[, c("year", "level", "province", "district", "group", "item", "element", "value", "unit"), drop = FALSE], file, row.names = FALSE)
    }
  )


  stat_source <- reactive({
    mode <- price_mode()
    req(identical(input$isoko_tabs, mode))
    req(mode %in% c("input", "food", "live"))
    empty <- data.frame(label = character(), value = numeric(), district = character(), t = numeric(), n = numeric(), stringsAsFactors = FALSE)
    if (identical(input$stat_data, "level")) {
      if (identical(mode, "input")) {
        d <- input_frame()
        district <- identical(input$input_level, "District")
        df <- if (nrow(d)) {
          data.frame(
            label = if (district) d$district else as.character(d$year), value = d$value,
            district = if (district) d$district else NA_character_, t = if (district) NA_real_ else d$year,
            n = NA_real_, stringsAsFactors = FALSE
          )
        } else empty
        return(list(
          df = df, unit = if (nrow(d)) d$unit[1] else "", what = paste(input$input_item, "·", input$input_measure),
          each = if (district) "districts" else "years", kind = if (district) "district" else "year"
        ))
      }
      d <- price_focus()
      com <- as.character(input$price_commodity)[1]
      if (!nrow(d)) return(list(df = empty, unit = "", what = paste(com, "prices"), each = "markets", kind = "market"))
      a <- stats::aggregate(price ~ market + district, data = d, FUN = mean)
      a$n <- stats::aggregate(price ~ market + district, data = d, FUN = length)$price
      mo <- stats::aggregate(price ~ year + month, data = d, FUN = stats::median)
      mo <- mo[order(mo$year, mo$month), , drop = FALSE]
      ts <- data.frame(label = paste(month.abb[mo$month], mo$year), value = mo$price, district = NA_character_, t = seq_len(nrow(mo)), n = NA_real_, stringsAsFactors = FALSE)
      return(list(
        df = data.frame(label = paste0(a$market, " · ", a$district), value = a$price, district = a$district, t = NA_real_, n = a$n, stringsAsFactors = FALSE),
        unit = paste("RWF per", d$unit[1]), what = paste(com, "average price by market"), each = "markets", kind = "market", ts = ts
      ))
    }
    r <- infl_calc()
    contrib <- identical(input$infl_view, "contrib")
    what <- paste(scope_label(), if (contrib) "contribution to national inflation, by district" else "year-on-year inflation, by district")
    if (is.null(r)) return(list(df = empty, unit = if (contrib) "pts" else "%", what = what, each = "districts", kind = "district"))
    d <- r$dist
    list(
      df = data.frame(label = d$district, value = if (contrib) d$to_nat else d$yoy, district = d$district, t = NA_real_, n = d$n, stringsAsFactors = FALSE),
      unit = if (contrib) "pts" else "%", what = what, each = "districts", kind = "district"
    )
  })

  mode_info <- function(v) {
    mag <- stats::median(abs(v))
    dg <- if (mag >= 10) 0 else if (mag >= 1) 1 else 2
    tb <- table(round(v, dg))
    if (max(tb) >= 2) {
      val <- as.numeric(names(tb)[tb == max(tb)])
      return(list(value = val[1], note = paste0("The most frequent value after rounding to ", dg, " decimal place(s). It appears ", max(tb), " times.")))
    }
    h <- graphics::hist(v, plot = FALSE, breaks = "Sturges")
    k <- which.max(h$counts)
    list(value = h$mids[k], note = paste0(
      "No value repeats, so this is the modal class: the busiest interval, ",
      fmt_num(h$breaks[k], 2), " to ", fmt_num(h$breaks[k + 1], 2), ", holding ", h$counts[k], " values."
    ))
  }

  stat_pack <- function(v, k) {
    n <- length(v)
    mu <- mean(v)
    sdv <- if (n > 1) stats::sd(v) else NA_real_
    q <- stats::quantile(v, c(0.25, 0.5, 0.75, k / 100), names = FALSE, type = 7)
    list(
      n = n, mean = mu, median = q[2], mode = mode_info(v), min = min(v), max = max(v), range = max(v) - min(v),
      var = if (n > 1) stats::var(v) else NA_real_, sd = sdv, q1 = q[1], q3 = q[3], iqr = q[3] - q[1], pk = q[4], k = k,
      z = if (is.finite(sdv) && sdv > 0) (v - mu) / sdv else rep(NA_real_, n)
    )
  }

  vs_values <- function(df, vs) {
    if (identical(vs, "readings")) return(df$n)
    if (identical(vs, "time")) return(df$t)
    item <- c(fert = "Inorganic fertilizer", seed = "Improved seeds", pest = "Pesticides (any)")[vs]
    sas <- input_sas[input_sas$item == item & input_sas$element == "Farmers who used it (%)", , drop = FALSE]
    sas$value[match(df$district, sas$district)]
  }

  stat_flags <- function(meas, v, st) {
    if (meas == "Z-score") {
      a <- abs(st$z)
      return(ifelse(a > 2, "Beyond 2 SD (outlier)", ifelse(a > 1, "1 to 2 SD from the mean", "Within 1 SD of the mean")))
    }
    switch(meas,
      "Mean" = ifelse(v > st$mean, "Above the mean", "At or below the mean"),
      "Median" = ifelse(v > st$median, "Above the median", "At or below the median"),
      "Mode" = ifelse(v > st$mode$value, "Above the mode", "At or below the mode"),
      "Range" = ifelse(v == st$max, "Highest value", ifelse(v == st$min, "Lowest value", "In between")),
      "Variance" = ifelse(abs(v - st$mean) <= st$sd, "Within 1 SD of the mean", "Further than 1 SD"),
      "Standard deviation" = ifelse(abs(v - st$mean) <= st$sd, "Within 1 SD of the mean", "Further than 1 SD"),
      "Interquartile range (IQR)" = ifelse(v < st$q1, "Below Q1", ifelse(v > st$q3, "Above Q3", "Middle 50%")),
      "Percentile" = ifelse(v > st$pk, paste0("Above P", st$k), paste0("At or below P", st$k)),
      rep("Value", length(v))
    )
  }

  flag_cols <- c(
    "Above the mean" = "#1b4332", "At or below the mean" = "#95d5b2",
    "Above the median" = "#1b4332", "At or below the median" = "#95d5b2",
    "Above the mode" = "#1b4332", "At or below the mode" = "#95d5b2",
    "Highest value" = "#b5502d", "Lowest value" = "#2d6a4f", "In between" = "#b7c4bb",
    "Within 1 SD of the mean" = "#52b788", "Further than 1 SD" = "#b5502d",
    "Below Q1" = "#2d6a4f", "Middle 50%" = "#c6a336", "Above Q3" = "#b5502d",
    "Beyond 2 SD (outlier)" = "#b2182b", "1 to 2 SD from the mean" = "#e7a86b",
    "Concordant (pushes r up)" = "#1b4332", "Discordant (pulls r down)" = "#b5502d"
  )
  flag_col <- function(f) {
    out <- unname(flag_cols[f])
    out[is.na(out)] <- ifelse(grepl("^Above P", f[is.na(out)]), "#1b4332", "#95d5b2")
    out
  }

  stat_refs <- function(meas, st) {
    switch(meas,
      "Mean" = list(lines = st$mean),
      "Median" = list(lines = st$median),
      "Mode" = list(lines = st$mode$value),
      "Range" = list(band = c(st$min, st$max)),
      "Variance" = list(band = c(st$mean - st$sd, st$mean + st$sd), lines = st$mean),
      "Standard deviation" = list(band = c(st$mean - st$sd, st$mean + st$sd), lines = st$mean),
      "Interquartile range (IQR)" = list(band = c(st$q1, st$q3), lines = st$median),
      "Percentile" = list(lines = st$pk),
      "Z-score" = list(lines = c(-2, -1, 0, 1, 2)),
      list()
    )
  }

  stat_shapes <- function(ref, axis = "x") {
    out <- list()
    if (length(ref$band) == 2 && all(is.finite(ref$band))) out <- c(out, list(if (axis == "x") vband(ref$band[1], ref$band[2]) else hband(ref$band[1], ref$band[2])))
    for (x in ref$lines[is.finite(ref$lines)]) out <- c(out, list(if (axis == "x") vline(x) else hline(x)))
    out
  }

  stat_visual <- function(s, df, st, meas, chart) {
    zmode <- meas == "Z-score"
    val <- if (zmode) st$z else df$value
    unit <- if (zmode) "SD" else s$unit
    flag <- stat_flags(meas, df$value, st)
    ref <- stat_refs(meas, st)
    hover <- paste0("<b>", df$label, "</b><br>Value: ", fmt_num(df$value, 2), " ", s$unit, if (!all(is.na(st$z))) paste0("<br>z-score: ", sgn(st$z, 2)) else "", "<br>", flag)
    pin <- paste0(df$label, ": ", fmt_num(df$value, 2), " ", s$unit, if (!all(is.na(st$z))) paste0(", z = ", sgn(st$z, 2)) else "", " (", flag, ")")
    ord <- order(val)
    if (chart == "bar") {
      o <- ord
      p <- plot_ly(
        x = val[o], y = df$label[o], type = "bar", orientation = "h", height = max(380, 24 * length(o) + 90), source = "stat",
        marker = list(color = flag_col(flag[o])), text = fmt_num(val[o], 2), textposition = "outside", cliponaxis = FALSE,
        hovertext = hover[o], hoverinfo = "text", customdata = pin[o]
      )
      p <- green_layout(p) |> layout(
        yaxis = list(title = "", categoryorder = "array", categoryarray = df$label[o]),
        xaxis = list(title = unit), shapes = stat_shapes(ref, "x"), margin = list(l = 190)
      )
      return(event_register(pl_cfg(p), "plotly_click"))
    }
    if (chart == "line") {
      o <- ord
      p <- plot_ly(
        x = seq_along(o), y = val[o], type = "scatter", mode = "lines+markers", height = 440, source = "stat",
        line = list(color = "#1b4332", width = 2), marker = list(size = 9, color = flag_col(flag[o])),
        hovertext = paste0("Position ", seq_along(o), " of ", length(o), "<br>", hover[o]), hoverinfo = "text", customdata = pin[o]
      )
      p <- green_layout(p) |> layout(
        xaxis = list(title = paste("Position, lowest to highest (", s$each, ")")),
        yaxis = list(title = unit), shapes = stat_shapes(ref, "y")
      )
      return(event_register(pl_cfg(p), "plotly_click"))
    }
    if (chart == "pie") {
      lv <- unique(flag[order(match(flag, names(flag_cols)))])
      cnt <- vapply(lv, function(k) sum(flag == k), numeric(1))
      lst <- vapply(lv, function(k) paste(utils::head(df$label[flag == k], 12), collapse = "<br>"), character(1))
      share <- 100 * cnt / sum(cnt)
      p <- plot_ly(
        labels = lv, values = cnt, type = "pie", hole = 0.55, sort = FALSE, height = 480, source = "stat",
        marker = list(colors = flag_col(lv), line = list(color = "#ffffff", width = 1)), textinfo = "label+percent",
        hovertext = paste0("<b>", lv, "</b><br>", cnt, " ", s$each, " (", formatC(share, format = "f", digits = 1), "%)<br>", lst),
        hoverinfo = "text", customdata = paste0(lv, ": ", cnt, " ", s$each, " (", formatC(share, format = "f", digits = 1), "%)")
      )
      return(event_register(pl_cfg(green_layout(p)), "plotly_click"))
    }
    if (chart == "scatter") {
      o <- ord
      p <- plot_ly(
        x = val[o], y = df$label[o], type = "scatter", mode = "markers", height = max(380, 22 * length(o) + 90), source = "stat",
        marker = list(size = 12, color = flag_col(flag[o]), line = list(color = "#ffffff", width = 1)),
        hovertext = hover[o], hoverinfo = "text", customdata = pin[o]
      )
      p <- green_layout(p) |> layout(
        yaxis = list(title = "", categoryorder = "array", categoryarray = df$label[o]),
        xaxis = list(title = unit), shapes = stat_shapes(ref, "x"), margin = list(l = 190)
      )
      return(event_register(pl_cfg(p), "plotly_click"))
    }
    if (identical(s$kind, "year")) return(empty_box("These values are national years, not districts, so there is nothing to map. Pick another chart, or set DATA to By district."))
    dv <- tapply(df$value, df$district, mean)
    dd <- data.frame(district = names(dv), raw = as.numeric(dv), stringsAsFactors = FALSE)
    sdv <- if (is.finite(st$sd) && st$sd > 0) st$sd else NA_real_
    metric <- switch(meas,
      "Mean" = dd$raw - st$mean, "Median" = dd$raw - st$median, "Mode" = dd$raw - st$mode$value,
      "Range" = dd$raw, "Interquartile range (IQR)" = dd$raw - st$median,
      "Percentile" = 100 * vapply(dd$raw, function(x) mean(df$value <= x), numeric(1)),
      (dd$raw - st$mean) / sdv
    )
    mlab <- switch(meas,
      "Mean" = "Gap to mean", "Median" = "Gap to median", "Mode" = "Gap to mode", "Range" = s$unit,
      "Interquartile range (IQR)" = "Gap to median", "Percentile" = "Percentile rank", "z-score"
    )
    dd$value <- metric
    dd$hover <- paste0("<b>", dd$district, "</b><br>Value: ", fmt_num(dd$raw, 2), " ", s$unit, "<br>", mlab, ": ", fmt_num(metric, 2))
    dd$pin <- paste0(dd$district, ": ", fmt_num(dd$raw, 2), " ", s$unit, ", ", tolower(mlab), " ", fmt_num(metric, 2))
    diverging <- !(meas %in% c("Range", "Percentile"))
    choro(dd, mlab, if (diverging) div_scale else seq_scale, zmid = if (diverging) 0 else NULL, source = "stat", height = 600, empty_note = "No value in this selection")
  }

  corr_visual <- function(s, df, vs, chart) {
    use_ts <- identical(vs, "time") && !is.null(s$ts)
    if (use_ts) df <- s$ts
    x <- vs_values(df, vs)
    y <- df$value
    ok <- is.finite(x) & is.finite(y)
    vs_lab <- names(stat_vs_choices)[stat_vs_choices == vs]
    if (sum(ok) < 3) {
      why <- if (identical(vs, "time")) "Time only applies to a series of months or years." else if (vs %in% c("fert", "seed", "pest")) "SAS figures are published by district, so they pair only with district values." else "This figure is not available for these items."
      return(list(head = NULL, ui = empty_box(paste("A correlation needs at least three pairs.", why))))
    }
    df <- df[ok, , drop = FALSE]
    x <- x[ok]
    y <- y[ok]
    n <- length(x)
    r <- stats::cor(x, y)
    pv <- if (n >= 4) tryCatch(stats::cor.test(x, y)$p.value, error = function(e) NA_real_) else NA_real_
    zx <- (x - mean(x)) / stats::sd(x)
    zy <- (y - mean(y)) / stats::sd(y)
    pz <- zx * zy
    flag <- ifelse(pz >= 0, "Concordant (pushes r up)", "Discordant (pulls r down)")
    strength <- if (!is.finite(r)) "undefined" else if (abs(r) < 0.1) "no" else if (abs(r) < 0.3) "a weak" else if (abs(r) < 0.5) "a moderate" else "a strong"
    dir <- if (is.finite(r) && r < 0) "negative" else "positive"
    head <- list(
      big = if (is.finite(r)) formatC(r, format = "f", digits = 3) else "—",
      explain = paste0(
        "r = ", formatC(r, format = "f", digits = 3), " across ", n, " pairs: ", strength, if (strength != "no") paste0(" ", dir) else "", " linear relationship between ",
        low_first(s$what), " and ", low_first(vs_lab), ".", if (is.finite(pv)) paste0(" p-value ", formatC(pv, format = "f", digits = 3), if (pv < 0.05) ", unlikely to be chance." else ", could be chance.") else ""
      )
    )
    hover <- paste0("<b>", df$label, "</b><br>", vs_lab, ": ", fmt_num(x, 2), "<br>Value: ", fmt_num(y, 2), " ", s$unit, "<br>Share of r: ", sgn(pz / (n - 1), 3))
    pin <- paste0(df$label, ": ", fmt_num(x, 2), " against ", fmt_num(y, 2), " ", s$unit, " (share of r ", sgn(pz / (n - 1), 3), ")")
    fit <- stats::lm(y ~ x)
    xs <- seq(min(x), max(x), length.out = 40)
    ys <- stats::predict(fit, newdata = data.frame(x = xs))
    ui <- if (chart == "bar") {
      o <- order(pz)
      p <- plot_ly(
        x = pz[o] / (n - 1), y = df$label[o], type = "bar", orientation = "h", height = max(380, 24 * n + 90), source = "stat",
        marker = list(color = flag_col(flag[o])), text = sgn(pz[o] / (n - 1), 3), textposition = "outside", cliponaxis = FALSE,
        hovertext = hover[o], hoverinfo = "text", customdata = pin[o]
      )
      event_register(pl_cfg(green_layout(p) |> layout(yaxis = list(title = "", categoryorder = "array", categoryarray = df$label[o]), xaxis = list(title = "Share of r"), margin = list(l = 190))), "plotly_click")
    } else if (chart == "pie") {
      lv <- unique(flag)
      cnt <- vapply(lv, function(k) sum(flag == k), numeric(1))
      p <- plot_ly(
        labels = lv, values = cnt, type = "pie", hole = 0.55, sort = FALSE, height = 460, source = "stat",
        marker = list(colors = flag_col(lv)), textinfo = "label+percent",
        hovertext = paste0("<b>", lv, "</b><br>", cnt, " of ", n, " pairs"), hoverinfo = "text",
        customdata = paste0(lv, ": ", cnt, " of ", n, " pairs")
      )
      event_register(pl_cfg(green_layout(p)), "plotly_click")
    } else if (chart == "map") {
      if (all(is.na(df$district))) {
        empty_box("These pairs are months or years, not districts, so there is nothing to map.")
      } else {
        dd <- data.frame(district = df$district, value = pz / (n - 1), hover = hover, pin = pin, stringsAsFactors = FALSE)
        dd <- dd[!duplicated(dd$district), , drop = FALSE]
        choro(dd, "Share of r", div_scale, zmid = 0, source = "stat", height = 600, empty_note = "Not in this pairing")
      }
    } else {
      o <- order(x)
      pts <- list(
        x = x[o], y = y[o], type = "scatter", mode = if (chart == "line") "lines+markers" else "markers", name = "Pairs",
        marker = list(size = 11, color = flag_col(flag[o]), line = list(color = "#ffffff", width = 1)),
        hovertext = hover[o], hoverinfo = "text", customdata = pin[o]
      )
      if (chart == "line") pts$line <- list(color = "#95d5b2")
      p <- do.call(add_trace, c(list(plot_ly(source = "stat", height = 500)), pts)) |>
        add_trace(x = xs, y = ys, type = "scatter", mode = "lines", inherit = FALSE, name = "Straight-line fit", line = list(color = "#c6a336", width = 2.5), hoverinfo = "skip")
      event_register(pl_cfg(green_layout(p) |> layout(xaxis = list(title = vs_lab), yaxis = list(title = s$unit), showlegend = FALSE)), "plotly_click")
    }
    list(head = head, ui = ui)
  }

  stat_explain <- c(
    "Mean" = "The mathematical average: add all values and divide by how many there are.",
    "Median" = "The exact middle value once sorted. Half the values sit below it and half above.",
    "Range" = "The highest value minus the lowest, the total span of the data.",
    "Variance" = "The average of the squared differences from the mean (sample formula). Its unit is squared.",
    "Standard deviation" = "The square root of the variance: the typical distance of a value from the mean.",
    "Interquartile range (IQR)" = "Q3 minus Q1, the spread of the middle 50% of the values.",
    "Percentile" = "The value below which the chosen share of the data falls.",
    "Z-score" = "How many standard deviations each value sits from the mean. Beyond ±2 is treated as an outlier.",
    "Correlation coefficient" = "A number from −1 to +1 for the strength and direction of a straight-line link between two variables."
  )

  output$stat_panel <- renderUI({
    s <- stat_source()
    df <- s$df[is.finite(s$df$value), , drop = FALSE]
    meas <- as.character(input$stat_measure)[1]
    grp <- as.character(input$stat_group)[1]
    chart <- as.character(input$stat_chart)[1]
    if (is.na(meas) || !meas %in% unlist(stat_measures)) meas <- "Mean"
    if (is.na(grp) || !grp %in% names(stat_measures)) grp <- "center"
    if (!meas %in% stat_measures[[grp]]) meas <- stat_measures[[grp]][1]
    if (is.na(chart) || !nzchar(chart)) chart <- "bar"
    if (nrow(df) < 2) return(empty_box("Fewer than two values in this selection, so no statistic can be measured. Change the basket, period, or DATA."))
    k <- suppressWarnings(as.numeric(input$stat_pct))
    if (!length(k) || !is.finite(k)) k <- 90
    st <- stat_pack(df$value, k)
    u <- s$unit
    chip <- function(lbl, v) tags$span(class = "stat-chip", tags$em(lbl), tags$b(v))
    num <- function(x, unit = u) if (is.finite(x)) paste(fmt_num(x, 2), unit) else "—"
    chips <- switch(grp,
      center = list(chip("Mean", num(st$mean)), chip("Median", num(st$median)), chip("Mode", num(st$mode$value))),
      spread = list(chip("Range", num(st$range)), chip("Variance", num(st$var, paste0("(", u, ")²"))), chip("Standard deviation", num(st$sd)), chip("IQR", num(st$iqr))),
      position = list(chip(paste0("P", k), num(st$pk)), chip("Largest |z|", if (all(is.na(st$z))) "—" else paste0(df$label[which.max(abs(st$z))], " · ", sgn(st$z[which.max(abs(st$z))], 2))), chip("Outliers (|z| > 2)", as.character(sum(abs(st$z) > 2, na.rm = TRUE)))),
      relation = list(chip("Pairs needed", "3 or more"), chip("Scale", "−1 to +1"))
    )
    if (meas == "Correlation coefficient") {
      vs <- as.character(input$stat_vs)[1]
      if (is.na(vs) || !vs %in% stat_vs_choices) vs <- "fert"
      cv <- corr_visual(s, df, vs, chart)
      big <- if (is.null(cv$head)) "—" else cv$head$big
      explain <- if (is.null(cv$head)) stat_explain[[meas]] else cv$head$explain
      return(tagList(
        tags$h3(paste(meas, "·", s$what)),
        tags$p(class = "price-total", big),
        tags$p(stat_explain[[meas]]),
        tags$p(explain),
        tags$div(class = "stat-chips", chips),
        cv$ui,
        tags$p("Gold line: straight-line fit. Dark dots move with the trend, terracotta dots against it. Hover or click for exact figures.")
      ))
    }
    big <- switch(meas,
      "Mean" = num(st$mean), "Median" = num(st$median), "Mode" = num(st$mode$value),
      "Range" = num(st$range), "Variance" = num(st$var, paste0("(", u, ")²")), "Standard deviation" = num(st$sd),
      "Interquartile range (IQR)" = num(st$iqr), "Percentile" = paste0("P", k, " = ", num(st$pk)),
      "Z-score" = if (all(is.na(st$z))) "—" else paste0(df$label[which.max(abs(st$z))], ": z = ", sgn(st$z[which.max(abs(st$z))], 2))
    )
    detail <- switch(meas,
      "Mean" = paste0("Average of ", st$n, " ", s$each, "."),
      "Median" = paste0("Middle of ", st$n, " ", s$each, " sorted from lowest to highest."),
      "Mode" = st$mode$note,
      "Range" = paste0("Highest ", fmt_num(st$max, 2), " minus lowest ", fmt_num(st$min, 2), ". Gold band: the full span."),
      "Variance" = paste0("Mean ", fmt_num(st$mean, 2), ". Gold band: mean ± one standard deviation."),
      "Standard deviation" = paste0("Mean ", fmt_num(st$mean, 2), ", so most values sit between ", fmt_num(st$mean - st$sd, 2), " and ", fmt_num(st$mean + st$sd, 2), ". Gold band: mean ± one SD."),
      "Interquartile range (IQR)" = paste0("Q1 ", fmt_num(st$q1, 2), ", Q3 ", fmt_num(st$q3, 2), ". Gold band: the middle 50%; gold line: the median."),
      "Percentile" = paste0(k, "% of the ", s$each, " sit at or below ", fmt_num(st$pk, 2), " ", u, ". Gold line: P", k, "."),
      "Z-score" = paste0("The value furthest from the mean. Bars show each z-score; gold lines mark −2, −1, 0, +1, and +2. ", sum(abs(st$z) > 2, na.rm = TRUE), " outlier(s) beyond ±2.")
    )
    tagList(
      tags$h3(paste(meas, "·", s$what)),
      tags$p(class = "price-total", big),
      tags$p(if (meas == "Mode") "The value or values that appear most often in the data." else stat_explain[[meas]]),
      tags$p(detail),
      tags$div(class = "stat-chips", chips),
      stat_visual(s, df, st, meas, chart),
      tags$p("Colours group the values against the statistic. Hover or click any bar, point, slice, or district for the exact figure.")
    )
  })

  output$price_overview <- renderUI({
    req(identical(price_mode(), "overview"))
    row <- factor_items[factor_items$id == "inflation-overview", , drop = FALSE]
    card <- function(k, v, n) tags$article(class = "lux-card", tags$span(k), tags$strong(v), tags$p(n))
    tabs <- c(input = "Farm inputs", food = "Food prices", live = "Livestock (Animal Products)")
    per <- period_text(naeb_months)
    tab_card <- function(t) {
      r <- get_infl_overview_tab(t)
      if (is.null(r)) return(card(tabs[[t]], "—", "No price in both years"))
      top <- r$prod[which.max(abs(r$prod$to_nat)), ]
      card(tabs[[t]], sgn(r$national, 1, "%"), paste0("Year on year, ", r$D, " districts. Biggest driver: ", top$product, " (", sgn(top$to_nat, 2, " pts"), ")."))
    }
    wb <- factor_data[factor_data$id == "inflation" & is.finite(factor_data$value), , drop = FALSE]
    wb_card <- if (nrow(wb)) {
      l <- wb[which.max(wb$year), ]
      card("Headline CPI", fmt_pct(l$value, 1), paste("Rwanda, all items, World Bank ·", l$year))
    } else {
      card("Headline CPI", "—", "Not in the World Bank extract")
    }
    rows <- get_infl_overview_cats()
    if (!is.null(rows)) {
      rows$category <- cap_first(rows$category)
      rows$tab <- unname(tabs[rows$tab])
    }
    chart <- if (is.null(rows) || !nrow(rows)) {
      empty_box("No category has prices in both years.")
    } else {
      rows <- rows[order(rows$yoy), , drop = FALSE]
      tcol <- c("Farm inputs" = "#c6a336", "Food prices" = "#40916c", "Livestock (Animal Products)" = "#1b4332")
      p <- plot_ly(height = max(380, 32 * nrow(rows) + 100))
      for (tb in names(tcol)) {
        s <- rows[rows$tab == tb, , drop = FALSE]
        if (!nrow(s)) next
        p <- add_trace(
          p, x = s$yoy, y = s$category, type = "bar", orientation = "h", name = tb, marker = list(color = tcol[[tb]]),
          text = sgn(s$yoy, 1, "%"), textposition = "outside", cliponaxis = FALSE,
          hovertext = paste0("<b>", s$category, "</b> · ", tb, "<br>Year on year: ", sgn(s$yoy, 2, "%"), "<br>", s$products, " products in ", s$D, " districts"),
          hoverinfo = "text"
        )
      }
      p <- pl_cfg(green_layout(p) |> layout(
        yaxis = list(title = "", categoryorder = "array", categoryarray = rows$category),
        xaxis = list(title = "Year-on-year change (%)", ticksuffix = "%"),
        legend = list(orientation = "h", y = -0.12), margin = list(l = 170)
      ))
      tagList(p, tags$p(paste0("NAEB / Esoko market prices, ", per, ". Each category is the average of its districts, with products weighted equally (inputs weighted by SAS use). Hover a bar for the exact figure.")))
    }
    food <- get_infl_overview_tab("food")
    map <- if (is.null(food)) {
      empty_box("No food price in both years.")
    } else {
      d <- food$dist
      v <- data.frame(
        district = d$district, value = d$yoy,
        hover = paste0("<b>", d$district, "</b> · ", d$province, "<br>Food inflation: ", sgn(d$yoy, 2, "%"), "<br>Contribution to national: ", sgn(d$to_nat, 3, " pts"), "<br>Rank ", d$rank, " of ", food$D),
        pin = paste0(d$district, ": ", sgn(d$yoy, 2, "%")), stringsAsFactors = FALSE
      )
      tagList(choro(v, "YoY %", div_scale, zmid = 0, height = 560, empty_note = "No food price in both years"), tags$p("Red districts saw food prices rise the most. Scroll to zoom, drag to move."))
    }
    tags$div(
      tags$p(class = "kicker", row$force_label),
      tags$h2(row$label),
      tags$p(class = "lede", row$story),
      tags$div(class = "mega-row four glance", tab_card("input"), tab_card("food"), tab_card("live"), wb_card),
      tags$div(class = "chart-card solo", tags$h3(paste("Inflation by category ·", per)), chart),
      tags$div(class = "chart-card solo", tags$h3("Food inflation by district"), map)
    )
  })

  force_on <- reactive(identical(price_mode(), "force"))

  force_ids <- reactive({
    row <- factor_pick()
    factor_ids(row)
  })

  observeEvent(list(input$factor_pick, input$factor_ready), {
    req(force_on())
    ids <- force_ids()
    labs <- unname(series_label[ids])
    labs[is.na(labs) | !nzchar(labs)] <- ids
    ch <- if (identical(as.character(factor_pick()$id)[1], "gdp-district")) {
      c("Crop figure by district" = "district")
    } else {
      c("All published series" = "all", stats::setNames(ids, labs))
    }
    cur <- isolate(input$force_scope)
    updateSelectInput(session, "force_scope", choices = ch, selected = if (!is.null(cur) && cur %in% ch) cur else ch[1])
    long <- factor_long()
    if (nrow(long)) {
      updateSliderInput(session, "force_year", min = min(long$year), max = max(long$year), value = c(min(long$year), max(long$year)))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$force_stat_group, {
    ch <- stat_measures[[input$force_stat_group]]
    if (is.null(ch)) return()
    cur <- isolate(input$force_stat_measure)
    updateSelectInput(session, "force_stat_measure", choices = ch, selected = if (!is.null(cur) && cur %in% ch) cur else ch[1])
  }, ignoreInit = TRUE)

  force_long_yr <- reactive({
    d <- factor_long()
    yr <- input$force_year
    if (nrow(d) && !is.null(yr) && length(yr) == 2) d <- d[d$year >= min(yr) & d$year <= max(yr), , drop = FALSE]
    sc <- input$force_scope
    if (!is.null(sc) && !sc %in% c("all", "district", "") && sc %in% d$series) d <- d[d$series == sc, , drop = FALSE]
    d
  })

  force_district <- reactive({
    crop <- as.character(input$force_crop)[1]
    if (is.na(crop) || !nzchar(crop)) crop <- "Maize"
    d <- district_crop[district_crop$crop == crop & is.finite(district_crop$figure), , drop = FALSE]
    if (!nrow(d)) return(d)
    d$value <- d$figure
    d$share <- 100 * d$figure / sum(d$figure)
    d[order(-d$figure), , drop = FALSE]
  })

  force_yoy <- function(d) {
    if (!nrow(d)) return(d[0, ])
    parts <- lapply(split(d, d$series), function(z) {
      z <- z[order(z$year), , drop = FALSE]
      if (nrow(z) < 2) return(NULL)
      last <- z[nrow(z), , drop = FALSE]
      prev <- z[nrow(z) - 1, , drop = FALSE]
      last$prev <- prev$value
      last$prev_year <- prev$year
      last$yoy <- 100 * (last$value / prev$value - 1)
      last
    })
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (!length(parts)) return(d[0, ])
    out <- do.call(rbind, parts)
    if (nrow(out) && any(is.finite(out$yoy))) out$to_nat <- out$yoy / sum(abs(out$yoy), na.rm = TRUE) * sum(out$yoy, na.rm = TRUE)
    else out$to_nat <- NA_real_
    out
  }

  output$force_head <- renderUI({
    req(force_on())
    row <- factor_pick()
    tags$div(
      tags$p(class = "kicker", row$force_label),
      tags$h2(row$label),
      tags$p(class = "lede", row$story)
    )
  })

  output$force_cards <- renderUI({
    req(force_on())
    row <- factor_pick()
    card <- function(k, v, n) tags$article(class = "lux-card", tags$span(k), tags$strong(v), tags$p(n))
    if (identical(as.character(row$id)[1], "gdp-district")) {
      d <- force_district()
      if (!nrow(d)) return(card("District harvest", "—", "No published figure for this crop."))
      return(tags$div(class = "mega-row four glance",
        card("NATIONAL SUM", fmt_num(sum(d$figure), 0), paste(nrow(d), "districts · SAS 2026 A")),
        card("HIGHEST DISTRICT", d$district[1], paste0(fmt_num(d$figure[1], 0), " · ", fmt_pct(d$share[1], 1))),
        card("LOWEST DISTRICT", d$district[nrow(d)], paste0(fmt_num(d$figure[nrow(d)], 0), " · rank ", nrow(d))),
        card("MEDIAN", fmt_num(stats::median(d$figure), 0), as.character(input$force_crop))
      ))
    }
    d <- force_long_yr()
    y <- force_yoy(d)
    latest <- if (nrow(d)) {
      one <- d[d$series == as.character(row$series)[1], , drop = FALSE]
      if (!nrow(one)) one <- d
      one <- one[order(one$year), ]
      one[nrow(one), ]
    } else NULL
    y1 <- if (nrow(y)) y[match(as.character(row$series)[1], y$series), ] else NULL
    if (!is.null(y1) && nrow(y1) && is.na(y1$series[1])) y1 <- y[1, ]
    tags$div(
      class = "mega-row five glance",
      card("LATEST", if (!is.null(latest)) paste(fmt_num(latest$value, row$digits), row$unit) else "—", if (!is.null(latest)) paste(latest$label, "·", latest$year) else "No published year"),
      card("YEAR ON YEAR", if (!is.null(y1) && nrow(y1) && is.finite(y1$yoy)) sgn(y1$yoy, 1, "%") else "—", if (!is.null(y1) && nrow(y1)) paste(y1$prev_year, "to", y1$year) else "Needs two published years"),
      card("HIGHEST YEAR", if (nrow(d)) paste(fmt_num(d$value[which.max(d$value)], row$digits), d$year[which.max(d$value)]) else "—", if (nrow(d)) d$label[which.max(d$value)] else ""),
      card("LOWEST YEAR", if (nrow(d)) paste(fmt_num(d$value[which.min(d$value)], row$digits), d$year[which.min(d$value)]) else "—", if (nrow(d)) d$label[which.min(d$value)] else ""),
      card("SERIES IN VIEW", as.character(length(unique(d$series))), row$caveat)
    )
  })

  force_pin <- reactiveVal(NULL)
  observeEvent(if (force_on()) suppressWarnings(event_data("plotly_click", source = "force")), {
    ev <- suppressWarnings(event_data("plotly_click", source = "force"))
    force_pin(if (!is.null(ev$customdata)) as.character(ev$customdata)[1] else NULL)
  })
  observeEvent(list(price_mode(), input$force_scope, input$force_chart, input$force_year, input$force_crop), force_pin(NULL), ignoreInit = TRUE)

  output$force_visual <- renderUI({
    req(force_on())
    row <- factor_pick()
    type <- as.character(input$force_chart)[1]
    if (is.na(type) || !nzchar(type)) type <- "bar"
    district <- identical(as.character(row$id)[1], "gdp-district")
    if (district) {
      d <- force_district()
      if (!nrow(d)) return(empty_box("No published crop figure for this choice."))
      crop <- d$crop[1]
      hover <- paste0("<b>", d$district, "</b> · ", d$province, "<br>", crop, ": ", fmt_num(d$figure, 0), "<br>", fmt_pct(d$share, 2), " of the national figure")
      pin <- paste0(d$district, ": ", fmt_num(d$figure, 0), " (", fmt_pct(d$share, 2), ")")
      if (type == "map") {
        v <- data.frame(district = d$district, value = d$figure, hover = hover, pin = pin, stringsAsFactors = FALSE)
        return(tagList(tags$h3(paste(crop, "crop figure by district · SAS 2026 A")), choro(v, "Figure", seq_scale, source = "force", height = 560, empty_note = "No figure"), tags$p("Darker green is a larger figure. Scroll to zoom, drag to move, click a district to pin it.")))
      }
      o <- order(d$figure)
      if (type == "pie") {
        top <- utils::head(d, 8)
        other <- if (nrow(d) > 8) sum(d$figure[-(1:8)]) else 0
        lab <- c(top$district, if (other > 0) "Other districts")
        val <- c(top$figure, if (other > 0) other)
        p <- plot_ly(labels = lab, values = val, type = "pie", hole = 0.55, source = "force", height = 480,
          marker = list(colors = pal(length(lab))), textinfo = "label+percent",
          hovertext = paste0("<b>", lab, "</b><br>", fmt_num(val, 0)), hoverinfo = "text",
          customdata = paste0(lab, ": ", fmt_num(val, 0)))
        return(tagList(tags$h3(paste(crop, "share of the national figure")), event_register(pl_cfg(green_layout(p)), "plotly_click")))
      }
      p <- plot_ly(
        x = if (type == "bar") d$figure[o] else seq_along(o),
        y = if (type == "bar") d$district[o] else d$figure[o],
        type = if (type == "bar") "bar" else "scatter",
        mode = if (type == "bar") NULL else if (type == "line") "lines+markers" else "markers",
        orientation = if (type == "bar") "h" else NULL,
        source = "force", height = max(380, 22 * nrow(d) + 80),
        marker = list(color = "#2d6a4f"),
        hovertext = hover[o], hoverinfo = "text", customdata = pin[o],
        text = if (type == "bar") fmt_pct(d$share[o], 1) else NULL, textposition = if (type == "bar") "outside" else NULL, cliponaxis = FALSE
      )
      p <- green_layout(p) |> layout(
        yaxis = list(title = if (type == "bar") "" else "Crop figure", categoryorder = if (type == "bar") "array" else NULL, categoryarray = if (type == "bar") d$district[o] else NULL),
        xaxis = list(title = if (type == "bar") "Crop figure" else "Rank, lowest to highest"),
        margin = list(l = 160)
      )
      return(tagList(tags$h3(paste(crop, "by district · SAS 2026 A")), event_register(pl_cfg(p), "plotly_click"), tags$p("Hover or click a bar for the exact figure and its share of the national total.")))
    }
    d <- force_long_yr()
    if (!nrow(d)) return(empty_box("No published year in this range."))
    y <- force_yoy(d)
    if (type == "map") return(empty_box("This series is national. District figures were not published, so the map stays empty. Open District harvest under GDP for the SAS district map."))
    if (type == "line" || type == "scatter") {
      p <- plot_ly(source = "force", height = 460)
      for (s in unique(d$series)) {
        z <- d[d$series == s, ]
        z <- z[order(z$year), ]
        p <- add_trace(
          p, x = z$year, y = z$value, type = "scatter", mode = if (type == "line") "lines+markers" else "markers",
          name = z$label[1], marker = list(size = 8),
          hovertext = paste0("<b>", z$label, "</b><br>", z$year, ": ", fmt_num(z$value, 2), " ", row$unit),
          hoverinfo = "text", customdata = paste0(z$label, " · ", z$year, ": ", fmt_num(z$value, 2), " ", row$unit)
        )
      }
      return(tagList(tags$h3(paste(row$label, "over published years")), event_register(pl_cfg(green_layout(p) |> layout(xaxis = list(title = ""), yaxis = list(title = row$unit), legend = list(orientation = "h", y = -0.15))), "plotly_click"), tags$p("Years with no published figure stay off the line. Hover or click for the exact number.")))
    }
    if (type == "pie") {
      last <- do.call(rbind, lapply(split(d, d$label), function(z) z[which.max(z$year), ]))
      last <- last[is.finite(last$value) & last$value > 0, ]
      if (!nrow(last)) return(empty_box("Part-to-whole needs positive values in the latest year."))
      p <- plot_ly(labels = last$label, values = last$value, type = "pie", hole = 0.55, source = "force", height = 480,
        marker = list(colors = pal(nrow(last))), textinfo = "label+percent",
        hovertext = paste0("<b>", last$label, "</b><br>", last$year, ": ", fmt_num(last$value, 2)), hoverinfo = "text",
        customdata = paste0(last$label, ": ", fmt_num(last$value, 2)))
      return(tagList(tags$h3("Latest published year, part to whole"), event_register(pl_cfg(green_layout(p)), "plotly_click"), tags$p("Share of the selected series in their latest year. Not a household or credit weight unless the unit is already a share.")))
    }
    use <- if (nrow(y)) y else {
      last <- do.call(rbind, lapply(split(d, d$label), function(z) z[which.max(z$year), ]))
      last$yoy <- last$value
      last
    }
    use <- use[order(use$yoy), ]
    p <- plot_ly(
      x = use$yoy, y = use$label, type = "bar", orientation = "h", source = "force", height = max(360, 48 * nrow(use) + 80),
      marker = list(color = signed_cols(use$yoy)), text = if (nrow(y)) sgn(use$yoy, 1, "%") else fmt_num(use$yoy, row$digits),
      textposition = "outside", cliponaxis = FALSE,
      hovertext = if (nrow(y)) paste0("<b>", use$label, "</b><br>", use$prev_year, " to ", use$year, ": ", sgn(use$yoy, 2, "%"), "<br>", fmt_num(use$prev, 2), " → ", fmt_num(use$value, 2)) else paste0("<b>", use$label, "</b><br>", fmt_num(use$value, 2)),
      hoverinfo = "text",
      customdata = if (nrow(y)) paste0(use$label, ": ", sgn(use$yoy, 2, "%"), " from ", use$prev_year, " to ", use$year) else paste0(use$label, ": ", fmt_num(use$value, 2))
    )
    tagList(
      tags$h3(if (nrow(y)) paste("Year-on-year change ·", row$label) else paste("Latest published figure ·", row$label)),
      event_register(pl_cfg(green_layout(p) |> layout(yaxis = list(title = ""), xaxis = list(title = if (nrow(y)) "Year-on-year change (%)" else row$unit), margin = list(l = 200))), "plotly_click"),
      tags$p(if (nrow(y)) "Each bar is the latest published year against the year before it. Hover or click for the exact percentage." else "Only one published year, so there is no year-on-year change.")
    )
  })

  output$force_click <- renderUI({
    req(force_on())
    pin <- force_pin()
    if (is.null(pin)) return(tags$p(class = "click-pin muted", "Click any bar, point, slice, or district to pin its exact figure here."))
    tags$p(class = "click-pin", tags$strong("Pinned: "), pin)
  })

  output$force_stat <- renderUI({
    req(force_on())
    row <- factor_pick()
    district <- identical(as.character(row$id)[1], "gdp-district")
    if (district) {
      d <- force_district()
      df <- data.frame(label = d$district, value = d$figure, district = d$district, t = NA_real_, n = NA_real_, stringsAsFactors = FALSE)
      s <- list(df = df, unit = "crop figure", what = paste(input$force_crop, "crop figure by district"), each = "districts", kind = "district")
    } else {
      d <- force_long_yr()
      if (length(unique(d$series)) > 1 && !is.null(input$force_scope) && identical(input$force_scope, "all")) {
        d <- d[d$series == as.character(row$series)[1], , drop = FALSE]
      }
      df <- data.frame(label = as.character(d$year), value = d$value, district = NA_character_, t = d$year, n = NA_real_, stringsAsFactors = FALSE)
      s <- list(df = df, unit = row$unit, what = paste(row$label, "over published years"), each = "years", kind = "year")
    }
    df <- s$df[is.finite(s$df$value), , drop = FALSE]
    meas <- as.character(input$force_stat_measure)[1]
    grp <- as.character(input$force_stat_group)[1]
    chart <- as.character(input$force_stat_chart)[1]
    if (is.na(meas) || !meas %in% unlist(stat_measures)) meas <- "Mean"
    if (is.na(grp) || !grp %in% names(stat_measures)) grp <- "center"
    if (!meas %in% stat_measures[[grp]]) meas <- stat_measures[[grp]][1]
    if (is.na(chart) || !nzchar(chart)) chart <- "bar"
    if (nrow(df) < 2) return(empty_box("Fewer than two published values, so no statistic can be measured."))
    if (meas == "Correlation coefficient") return(empty_box("Correlation in this room needs a pairing series. Open Inflation for the full correlation panel, or pick another measure."))
    k <- suppressWarnings(as.numeric(input$force_stat_pct))
    if (!length(k) || !is.finite(k)) k <- 90
    st <- stat_pack(df$value, k)
    tagList(
      tags$h3(paste(meas, "·", s$what)),
      tags$p(class = "price-total", switch(meas,
        "Mean" = paste(fmt_num(st$mean, 2), s$unit), "Median" = paste(fmt_num(st$median, 2), s$unit),
        "Mode" = paste(fmt_num(st$mode$value, 2), s$unit), "Range" = paste(fmt_num(st$range, 2), s$unit),
        "Variance" = paste(fmt_num(st$var, 2)), "Standard deviation" = paste(fmt_num(st$sd, 2), s$unit),
        "Interquartile range (IQR)" = paste(fmt_num(st$iqr, 2), s$unit),
        "Percentile" = paste0("P", k, " = ", fmt_num(st$pk, 2), " ", s$unit),
        "Z-score" = paste0(df$label[which.max(abs(st$z))], ": z = ", sgn(st$z[which.max(abs(st$z))], 2))
      )),
      tags$p(if (meas == "Mode") st$mode$note else stat_explain[[meas]]),
      stat_visual(s, df, st, meas, chart)
    )
  })

  output$force_table <- renderUI({
    req(force_on())
    row <- factor_pick()
    if (identical(as.character(row$id)[1], "gdp-district")) {
      d <- force_district()
      if (!nrow(d)) return(tags$p("No rows."))
      show <- d[, c("district", "province", "figure", "land", "ratio", "share"), drop = FALSE]
      names(show) <- c("District", "Province", "Figure", "Land", "Figure per ha", "Share %")
      show$Figure <- fmt_num(d$figure, 0)
      show$Land <- fmt_num(d$land, 0)
      show$`Figure per ha` <- fmt_num(d$ratio, 2)
      show$`Share %` <- fmt_num(d$share, 2)
      return(tags$div(class = "price-wrap", tags$table(class = "price-table",
        tags$thead(tags$tr(lapply(names(show), tags$th))),
        tags$tbody(lapply(seq_len(nrow(show)), function(i) tags$tr(lapply(as.character(show[i, ]), tags$td))))
      )))
    }
    d <- force_long_yr()
    if (!nrow(d)) return(tags$p("No published years."))
    tags$div(class = "price-wrap", tags$table(class = "price-table",
      tags$thead(tags$tr(lapply(c("Year", "Series", "Value"), tags$th))),
      tags$tbody(lapply(seq_len(nrow(d)), function(i) tags$tr(
        tags$td(d$year[i]), tags$td(d$label[i]), tags$td(fmt_num(d$value[i], row$digits))
      )))
    ))
  })

  umusaruro_server(input, output, session)
  ikigega_server(input, output, session)
  ejo_server(input, output, session)
}
