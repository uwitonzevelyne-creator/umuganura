# Umuganura — interface

ui <- tagList(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("Umuganura — The first fruits walk the country"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Cormorant+Garamond:wght@600;700&family=Outfit:wght@400;500;600&display=swap"
    ),
    tags$link(rel = "stylesheet", href = "css/umuganura.css?v=55"),
    tags$link(rel = "stylesheet", href = "css/rooms.css?v=41"),
    tags$link(rel = "stylesheet", href = "css/about.css?v=5", media = "print", onload = "this.media='all'"),
    tags$link(rel = "stylesheet", href = "css/ejo-portal.css?v=7", media = "print", onload = "this.media='all'"),
    tags$link(rel = "stylesheet", href = "css/ejo-action.css?v=6", media = "print", onload = "this.media='all'"),
    tags$link(rel = "stylesheet", href = "css/loading.css?v=3"),
    tags$style(HTML("
      #factor-room .shiny-plot-output,
      #factor-room .js-plotly-plot,
      #factor-room .html-widget,
      #ik-room .shiny-plot-output,
      #ik-room .js-plotly-plot,
      #ik-room .html-widget,
      #ejo-hazaza .shiny-plot-output,
      #ejo-hazaza .js-plotly-plot,
      #ejo-hazaza .html-widget,
      #factor-room .chart-card .tab-content,
      #ik-room .tab-content,
      #ejo-hazaza .tab-content {
        min-height: 450px !important;
      }
      #factor-room .tabbable > .nav { display: none !important; }
      #factor-room .tabbable > .tab-content {
        min-height: 0 !important;
        padding: 0;
        border: 0;
        background: transparent;
      }
      #factor-room .recalculating,
      #ik-room .recalculating,
      #ejo-hazaza .recalculating {
        min-height: 450px !important;
      }
      body { scroll-behavior: auto !important; }
    "))
  ),

  tags$nav(
    class = "nav",
    tags$a(href = "#home", class = "brand", `data-panel` = "home",
           tags$img(src = "hero/logo.png?v=2", alt = "Umuganura")),
    tags$ul(
      class = "nav-links",
      tags$li(tags$a("HOME", href = "#home", `data-panel` = "home", class = "is-active")),
      tags$li(tags$a("ABOUT", href = "#about", `data-panel` = "about")),
      tags$li(tags$a("UMUSARURO", href = "#umusaruro", `data-panel` = "umusaruro")),
      tags$li(tags$a("ISOKO", href = "#factors", `data-panel` = "factors")),
      tags$li(tags$a("IKIGEGA", href = "#ikigega", `data-panel` = "ikigega")),
      tags$li(tags$a("EJO HAZAZA", href = "#ejo-hazaza", `data-panel` = "ejo-hazaza"))
    )
  ),

  tags$section(
    id = "home", class = "panel is-active",
    tags$div(
      class = "stage",
      tags$div(
        class = "stage-copy",
        tags$p(class = "kicker", "WELCOME TO UMUGANURA"),
        tags$h1(class = "home-title", tags$span(class = "home-cap", "P"), "ost harvest loss tracking platform + solutions"),
        tags$p(
          class = "lede home-lines",
          tags$span("Umuganura is Rwanda's ceremonial harvest day, and also the moment the year is counted: what the season gained, and what it lost after the crop left the field.")
        ),
        tags$div(
          class = "hero-actions",
          tags$button(class = "btn-go", type = "button", `data-panel` = "umusaruro", "OPEN THE HARVEST"),
          tags$button(class = "btn-ghost", type = "button", `data-panel` = "factors", "OPEN THE MARKET")
        )
      ),
      tags$div(
        class = "stage-woman",
        tags$img(class = "woman", src = "hero/cut/woman-front.png", alt = "Woman in umushanana, hoe on her shoulder, basket in her hand", loading = "lazy", decoding = "async")
      ),
      tags$div(
        class = "stage-harvest",
        tags$img(id = "stage-crop", src = "hero/cut/plant-avocado.png", alt = "Avocado")
      )
    ),
    tags$div(
      class = "home-follow",
      tags$div(
        class = "hx",
        tags$section(
          class = "hx-loss",
          tags$div(
            class = "hx-loss-copy",
            tags$h2(id = "hx-type", "What We Lose in Agriculture EVERY YEAR"),
            tags$p("Umuganura serves to visualize agricultural losses by comprehensively analyzing four key pillars: first, our total production and harvest yield; second, the active market dynamics and price drivers; third, the inventory levels remaining in our reserve stocks; and finally, the market intelligence and external news shocks that influence trade. By integrating these four components, the platform delivers a clearer operational understanding, an optimized dashboard design, and an accurate system to track historical post-harvest loss trajectories over recent years.")
          ),
          tags$div(
            class = "hx-art",
            tags$img(class = "hx-ill hx-ill-clear", src = "home/scene-person.png?v=10", alt = ""),
            tags$div(
              class = "hx-pm-papers",
              `aria-hidden` = "true",
              tags$aside(
                class = "hx-pm-sheet hx-pm-sheet-steps",
                tags$span(class = "hx-pm-bar"),
                tags$ul(
                  tags$li(tags$i(class = "hx-pm-ok"), "Yield"),
                  tags$li(tags$i(class = "hx-pm-ok"), "Market prices"),
                  tags$li(tags$i(class = "hx-pm-ok"), "Reserve stocks"),
                  tags$li(tags$i(class = "hx-pm-todo"), "Farm news"),
                  tags$li(tags$i(class = "hx-pm-todo"), "Six causes"),
                  tags$li(tags$i(class = "hx-pm-todo"), "Action desk")
                )
              ),
              tags$aside(
                class = "hx-pm-sheet hx-pm-sheet-report",
                tags$span(class = "hx-pm-bar"),
                tags$p(class = "hx-pm-kicker", "Navigate"),
                tags$p(class = "hx-pm-line", "Name the cause"),
                tags$p(class = "hx-pm-line", "Open the solution"),
                tags$p(class = "hx-pm-line", "Hand it to the desk")
              )
            )
          )
        ),
        tags$section(
          class = "hx-stats",
          tags$article(tags$strong(paste0(".", home_cover$districts)), tags$span(class = "hx-ring"), tags$p("Districts we cover. The current districts on the harvest map. They date from 2006.")),
          tags$article(tags$strong(paste0(".", home_cover$datasets)), tags$span(class = "hx-ring"), tags$p("Datasets. National series from the World Bank, FAO and NAEB, plus the NISR district tables, NASA POWER rain, and the market prices.")),
          tags$article(tags$strong(class = "hx-years", home_cover$years), tags$span(class = "hx-ring"), tags$p("Years. National series run across this span. District production in these tables is 2019\u20132026."))
        ),
        tags$section(
          class = "hx-know",
          tags$h2("What to know first"),
          tags$div(
            class = "hx-know-row",
            tags$div(class = "hx-know-art", tags$img(class = "hx-ill hx-ill-cream", src = "home/download%20(6).jpg?v=1", alt = "")),
            tags$div(
              class = "hx-know-list",
              tags$ol(
                class = "hx-steps",
                tags$li(tags$i("01"), tags$div(tags$strong("Umusaruro"), tags$p("First, the yield: how much each district grows this season."))),
                tags$li(tags$i("02"), tags$div(tags$strong("Isoko"), tags$p("Then the market: what is for sale, and the price that meets it."))),
                tags$li(tags$i("03"), tags$div(tags$strong("Ikigega"), tags$p("Then the reserve: what is still in the stores and cold rooms."))),
                tags$li(tags$i("04"), tags$div(tags$strong("Ejo Hazaza"), tags$p("Last, it tracks the post-harvest loss using operations research.")))
              )
            )
          )
        ),
        tags$section(
          class = "hx-ask",
          tags$div(
            tags$h2("Still have questions?"),
            tags$p("Dive into platform and see how operations research and project management intervenes to present and solve the agriculture loss"),
            tags$div(
              class = "hx-pm-icons",
              tags$span(
                class = "hx-pm-icon",
                title = "Institutions & partners",
                HTML('<svg viewBox="0 0 24 24" width="22" height="22" aria-hidden="true"><path fill="currentColor" d="M12 12a3.5 3.5 0 1 0-3.5-3.5A3.5 3.5 0 0 0 12 12Zm-7.5 8a4.6 4.6 0 0 1 4.6-4.6h5.8A4.6 4.6 0 0 1 19.5 20v1H4.5Zm13.2-9.5a2.7 2.7 0 1 0-2.7-2.7 2.7 2.7 0 0 0 2.7 2.7Zm-11.4 0a2.7 2.7 0 1 0-2.7-2.7 2.7 2.7 0 0 0 2.7 2.7Z"/></svg>')
              ),
              tags$span(
                class = "hx-pm-icon",
                title = "Reports & proposals",
                HTML('<svg viewBox="0 0 24 24" width="22" height="22" aria-hidden="true"><path fill="currentColor" d="M7 2h8l4 4v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2Zm7 1.5V7h3.5L14 3.5ZM8 11h8v1.5H8V11Zm0 3.5h8V16H8v-1.5Zm0 3.5h5V19.5H8V18Z"/></svg>')
              )
            ),
            tags$button(class = "hx-nav hx-ejo", type = "button", `data-panel` = "ejo-hazaza", "OPEN EJO HAZAZA")
          ),
          tags$div(
            class = "hx-phone",
            tags$span(class = "hx-blob hx-blob-phone"),
            tags$div(
              class = "hx-phone-screen",
              tags$img(src = "home/scene-person.png?v=10", alt = ""),
              tags$strong("Ejo Hazaza"),
              tags$p("Six-step framework, funding clearance, and logged partner reports.")
            )
          )
        ),
        tags$section(
          id = "hx-bike",
          class = "hx-bike",
          tags$h2(id = "hx-float", "Know the harvest first. Then stop losing it."),
          tags$article(
            id = "hx-detail", class = "hx-detail", hidden = NA,
            tags$img(src = "cards/umusaruro.jpg", alt = ""),
            tags$div(
              tags$p(class = "hx-detail-kicker"),
              tags$h3(""),
              tags$p(class = "hx-detail-copy"),
              tags$button(class = "hx-open", type = "button", `data-panel` = "umusaruro", "Open")
            )
          ),
          tags$div(
            class = "hx-window",
            tags$div(
              id = "hx-rail",
              class = "hx-rail",
              tags$button(type = "button", class = "hx-card", `data-room` = "umusaruro", `data-kicker` = "UMUSARURO", `data-title` = "Umusaruro", `data-img` = "cards/umusaruro.jpg", `data-open` = "Open Umusaruro", `data-copy` = "What we harvest: every crop and animal product, how much each district grows and how it changed by season.", tags$img(src = "cards/umusaruro.jpg", alt = ""), tags$span("UMUSARURO")),
              tags$button(type = "button", class = "hx-card", `data-room` = "factors", `data-kicker` = "ISOKO", `data-title` = "Isoko", `data-img` = "cards/isoko.jpg", `data-open` = "Open Isoko", `data-copy` = "Where it sells and for how much: prices in 57 markets, year to year, and the inflation, credit, jobs and franc that move them.", tags$img(src = "cards/isoko.jpg", alt = ""), tags$span("ISOKO")),
              tags$button(type = "button", class = "hx-card", `data-room` = "ikigega", `data-kicker` = "IKIGEGA", `data-title` = "Ikigega", `data-img` = "cards/ikigega.jpg", `data-open` = "Open Ikigega", `data-copy` = "What is kept and by whom: the national reserve, cooperative stores and cold rooms, and where stock runs low.", tags$img(src = "cards/ikigega.jpg", alt = ""), tags$span("IKIGEGA")),
              tags$button(type = "button", class = "hx-card", `data-room` = "ejo-hazaza", `data-kicker` = "EJO HAZAZA", `data-title` = "Ejo Hazaza", `data-img` = "cards/ejo.jpg", `data-open` = "Open Ejo Hazaza", `data-copy` = "The money we lose after harvest to six causes, the loss forecast to 2031, and the operations-research plans that win it back.", tags$img(src = "cards/ejo.jpg", alt = ""), tags$span("EJO HAZAZA"))
            )
          )
        )
      ),
      site_footer()
    )
  ),

  tags$section(
    id = "factors", class = "panel",
    tags$div(
      id = "factor-land", class = "iso-land",
      tags$div(
        class = "iso-block",
        tags$div(
          class = "iso-banner",
          tags$p(class = "iso-banner-title", "ISOKO = MARKET"),
          tags$p(class = "iso-banner-copy", "The market acts as the central bridge connecting agriculture to the broader macroeconomic factors of a country. Agriculture produces the goods, macroeconomic factors set the economic environment (like INFLATION, INTEREST RATES, UNEMPLOYMENT, GDP and EXCHANGE RATES), and the market is where these forces collide to determine prices, supply, and economic rates.")
        ),
        tags$h1(
          class = "iso-float",
          "Market is driven by macro economic factors",
          tags$span(class = "iso-turn", tags$span(class = "iso-hash", "#"), tags$span(id = "iso-word", class = "iso-word", "inflation"))
        ),
        tags$div(
        class = "iso-stage",
        tags$div(
          class = "iso-col",
          tags$button(type = "button", class = "iso-card", `data-open` = "inflation", `data-item` = "inflation-overview", `data-label` = "Inflation",
            tags$img(src = "isoko/poster-inflation.jpg", alt = ""),
            tags$h3("Inflation"),
            tags$p("Higher national inflation drives up the market price for farming inputs like fertilizer, fuel, and seeds.")),
          tags$button(type = "button", class = "iso-card", `data-open` = "interest", `data-item` = "interest-overview", `data-label` = "Interest",
            tags$img(src = "isoko/poster-interest.jpg", alt = ""),
            tags$h3("Interest rate"),
            tags$p("High interest rates make bank loans expensive, restricting farmers from buying equipment or expanding storage via the financial market.")),
          tags$button(type = "button", class = "iso-card", `data-open` = "fx", `data-item` = "fx-overview", `data-label` = "Exchange",
            tags$img(src = "isoko/poster-exchange.jpg", alt = ""),
            tags$h3("Exchange rate"),
            tags$p("A weak local currency makes a country's agricultural exports cheaper and more attractive on the global market, boosting local farm incomes."))
        ),
        tags$div(
          class = "iso-scene",
          tags$img(src = "isoko/kaliza-sky.png?v=2", alt = "Kaliza in farm boots, arms bent, pointing left to inflation and right to GDP")
        ),
        tags$div(
          class = "iso-col iso-right",
          tags$button(type = "button", class = "iso-card", `data-open` = "gdp", `data-item` = "gdp-overview", `data-label` = "GDP",
            tags$img(src = "isoko/poster-gdp.jpg", alt = ""),
            tags$h3("GDP"),
            tags$p("As a nation's GDP grows, consumer income rises, shifting market demand from basic staple grains toward higher-value products like meat, dairy, and organic produce.")),
          tags$button(type = "button", class = "iso-card", `data-open` = "jobs", `data-item` = "jobs-overview", `data-label` = "Unemployment",
            tags$img(src = "isoko/poster-jobs.jpg", alt = ""),
            tags$h3("Unemployment"),
            tags$p("A lack of jobs that cuts consumer spending and alters the farm labor supply."))
        )
        )
      )
    ),
    tags$div(
      id = "factor-room", class = "factor-room is-hidden mode-classic",
      tags$div(
        class = "room-bar",
        tags$button(type = "button", class = "room-back", `data-back` = "factors", "\u2190 Back"),
        tags$span(id = "room-pill", class = "room-pill", "Inflation"),
        tags$span(class = "room-title", "Umuganura \u00b7 Isoko")
      ),
      tags$div(
        class = "factor-filters",
        tags$div(
          class = "year-box",
          tags$span(class = "filter-kicker", "YEAR RANGE"),
          sliderInput("factor_year", NULL, min = 1960, max = 2025, value = c(1960, 2025), sep = "", width = "100%")
        ),
        selectInput(
          "factor_viz", "VISUALIZATION",
          choices = c(
            "Trends over time" = "trend",
            "Comparisons" = "compare",
            "Part to whole" = "share",
            "Distributions" = "spread",
            "Correlation" = "relate"
          ),
          selected = "trend",
          width = "100%"
        ),
        tags$div(
          class = "filter-field",
          tags$span(class = "filter-kicker", "AREA"),
          tags$select(
            class = "factor-area",
            disabled = "disabled",
            tags$option("National")
          ),
          tags$p("Province and district are not published for these series.")
        )
      ),
      tags$div(
        class = "factor-desk",
        tags$aside(
          class = "factor-side",
          lapply(unique(factor_items$force), function(f) {
            rows <- factor_items[factor_items$force == f, , drop = FALSE]
            tags$div(
              class = paste("factor-group", if (f != "inflation") "is-hidden"),
              `data-group` = f,
              lapply(seq_len(nrow(rows)), function(i) {
                tags$button(
                  type = "button",
                  class = paste("factor-link", if (rows$id[i] == "inflation-overview") "is-on"),
                  `data-force` = f,
                  `data-item` = rows$id[i],
                  rows$label[i]
                )
              })
            )
          })
        ),
        tags$div(
          class = "factor-main",
          tags$div(
            id = "iso-report",
            lapply(isoko_report_ids, function(id) {
              tags$div(class = "iso-rep-pane", `data-report` = id)
            })
          ),
          tags$div(
            id = "factor-classic",
            uiOutput("factor_head"),
            uiOutput("factor_glance"),
            tags$div(class = "chart-card solo", uiOutput("factor_chart")),
            tags$details(
              class = "stats-drawer",
              tags$summary("Advanced statistics"),
              uiOutput("factor_model"),
              tags$div(
                class = "chart-card",
                tags$h3("Next year"),
                tags$p("A straight-line baseline. Not a seasonal forecast."),
                uiOutput("factor_forecast"),
                uiOutput("factor_ahead")
              )
            ),
            uiOutput("factor_note")
          ),
          tags$div(
            id = "force-deck",
            uiOutput("force_head"),
            tags$div(
              class = "price-filters",
              sliderInput("force_year", "YEAR RANGE", min = 1960, max = 2025, value = c(1960, 2025), sep = "", width = "100%"),
              selectInput("force_scope", "SERIES", choices = c("All published series" = "all"), selected = "all", width = "100%"),
              tags$div(
                class = "force-crop",
                selectInput("force_crop", "CROP ON THE MAP", choices = hub_crops, selected = "Maize", width = "100%")
              ),
              selectInput("force_chart", "DATA VISUALIZATION", choices = viz_type_choices, selected = "bar", width = "100%")
            ),
            uiOutput("force_cards"),
            uiOutput("force_visual"),
            uiOutput("force_click"),
            tags$div(
              id = "force-stat",
              class = "chart-card",
              tags$p(class = "kicker", "STATISTICAL MEASURES"),
              tags$div(
                class = "price-filters",
                selectInput("force_stat_group", "GROUP", choices = stat_group_choices, selected = "center", width = "100%"),
                selectInput("force_stat_measure", "MEASURE", choices = stat_measures$center, selected = "Mean", width = "100%"),
                selectInput("force_stat_chart", "DATA VISUALIZATION", choices = viz_type_choices, selected = "bar", width = "100%"),
                conditionalPanel(
                  "input.force_stat_measure == 'Percentile'",
                  selectInput("force_stat_pct", "PERCENTILE", choices = c(10, 25, 50, 75, 90, 95), selected = 90, width = "100%")
                )
              ),
              uiOutput("force_stat")
            ),
            tags$details(
              class = "stats-drawer table-last",
              tags$summary("Published figures"),
              uiOutput("force_table")
            )
          ),
          tabsetPanel(
            id = "isoko_tabs",
            selected = "idle",
            tabPanel(title = "Idle", value = "idle"),
            tabPanel(
              title = "Overview",
              value = "overview",
              tags$div(id = "factor-overview")
            ),
            tabPanel(
              title = "Input Prices",
              value = "input",
              tags$div(id = "lazy-input")
            ),
            tabPanel(
              title = "Food Prices",
              value = "food",
              tags$div(id = "lazy-food")
            ),
            tabPanel(
              title = "Livestock Prices",
              value = "live",
              tags$div(id = "lazy-live")
            )
          ),
          conditionalPanel(
            condition = "input.isoko_tabs == 'input' || input.isoko_tabs == 'food' || input.isoko_tabs == 'live'",
            tags$div(
              id = "price-deck",
            uiOutput("price_head"),
            tags$div(
              id = "filters-input",
              class = "price-filters",
              selectInput("input_group", "INPUT", choices = c("Fertilizers" = "Fertilizer", "Seeds" = "Seed", "Pesticides" = "Pesticide"), selected = "Fertilizer", width = "100%"),
              selectInput("input_level", "DATA", choices = input_level_choices, selected = "District", width = "100%"),
              selectInput("input_item", "ITEM", choices = "Inorganic fertilizer", selected = "Inorganic fertilizer", width = "100%"),
              selectInput("input_measure", "MEASURE", choices = "Farmers who used it (%)", selected = "Farmers who used it (%)", width = "100%"),
              conditionalPanel(
                "input.input_level == 'District'",
                selectInput("input_province", "PROVINCE", choices = c("All provinces", "Kigali", "Southern", "Western", "Northern", "Eastern"), selected = "All provinces", width = "100%")
              ),
              conditionalPanel(
                "input.input_level == 'National'",
                selectInput("input_from", "FROM YEAR", choices = sort(unique(input_fao$year)), selected = min(input_fao$year), width = "100%")
              ),
              conditionalPanel(
                "input.input_level == 'National'",
                selectInput("input_to", "TO YEAR", choices = sort(unique(input_fao$year)), selected = max(input_fao$year), width = "100%")
              )
            ),
            tags$div(class = "input-only", uiOutput("input_glance")),
            tags$div(
              id = "filters-market",
              class = "price-filters",
              selectInput("price_year", "YEAR", choices = price_year_choices, selected = "all", width = "100%"),
              selectInput("price_province", "PROVINCE", choices = c("All provinces", naeb_provinces), selected = "All provinces", width = "100%"),
              selectInput("price_district", "DISTRICT", choices = "All districts", selected = "All districts", width = "100%"),
              selectInput("price_market", "MARKET LOCATION", choices = "All markets", selected = "All markets", width = "100%"),
              selectInput("price_category", "CATEGORY", choices = c("All categories", food_price_categories), selected = "All categories", width = "100%"),
              selectInput("price_commodity", "PRODUCT", choices = "Maize", selected = "Maize", width = "100%"),
              numericInput("price_qty", "QUANTITY", value = 1, min = 0.1, step = 1, width = "100%"),
              selectInput("price_topn", "SHOW TOP", choices = c(5, 10, 20), selected = 10, width = "100%")
            ),
            tags$div(
              id = "market-blocks",
              tags$div(
                class = "chart-card calc-card",
                tags$h3(class = "card-title", "Price calculation"),
                uiOutput("price_total")
              ),
              tags$div(
                class = "chart-card map-card",
                tags$h3(class = "card-title", "Market location"),
                tags$p("Every market that sells this product. Bigger, darker dots are dearer. The blue pin is the market you picked. Use + and − or scroll to zoom, drag to move, click a dot for its prices."),
                if (has_leaflet) leaflet::leafletOutput("price_map", height = "540px") else uiOutput("price_pin"),
                uiOutput("price_map_note")
              )
            ),
            tags$div(
              class = "chart-card viz-page",
              tags$div(
                class = "viz-one",
                selectInput("price_viz", "DATA VISUALIZATION", choices = viz_type_choices, selected = "bar", width = "100%")
              ),
              uiOutput("price_visual")
            ),
            tags$div(
              id = "infl-card",
              class = "chart-card viz-page",
              tags$p(class = "kicker", "INFLATION"),
              uiOutput("infl_head"),
              tags$div(
                class = "price-filters",
                selectInput("infl_view", "SHOW", choices = c("Year-on-year inflation (%)" = "yoy", "Contribution to national inflation (points)" = "contrib"), selected = "yoy", width = "100%"),
                selectInput("infl_scope", "BASKET", choices = c("All" = "all"), selected = "all", width = "100%"),
                selectInput("infl_period", "PERIOD", choices = naeb_period_choices, selected = "ytd", width = "100%"),
                selectInput("infl_chart", "DATA VISUALIZATION", choices = viz_type_choices, selected = "bar", width = "100%")
              ),
              uiOutput("infl_cards"),
              uiOutput("infl_visual"),
              uiOutput("infl_click"),
              tags$details(
                class = "stats-drawer",
                tags$summary("District ranking table"),
                uiOutput("infl_rank")
              )
            ),
            tags$div(
              id = "stat-card",
              class = "chart-card",
              tags$p(class = "kicker", "STATISTICAL MEASURES"),
              tags$div(
                class = "price-filters",
                selectInput("stat_data", "DATA", choices = c("District inflation" = "infl", "Prices or use figures" = "level"), selected = "infl", width = "100%"),
                selectInput("stat_group", "GROUP", choices = stat_group_choices, selected = "center", width = "100%"),
                selectInput("stat_measure", "MEASURE", choices = stat_measures$center, selected = "Mean", width = "100%"),
                selectInput("stat_chart", "DATA VISUALIZATION", choices = viz_type_choices, selected = "bar", width = "100%"),
                conditionalPanel(
                  "input.stat_measure == 'Percentile'",
                  selectInput("stat_pct", "PERCENTILE", choices = c(10, 25, 50, 75, 90, 95), selected = 90, width = "100%")
                ),
                conditionalPanel(
                  "input.stat_measure == 'Correlation coefficient'",
                  selectInput("stat_vs", "AGAINST", choices = stat_vs_choices, selected = "fert", width = "100%")
                )
              ),
              uiOutput("stat_panel")
            ),
            tags$div(
              class = "chart-card input-only",
              tags$h3("Input table"),
              uiOutput("input_table"),
              downloadButton("input_csv", "Download CSV")
            ),
            tags$details(
              id = "price-table-card",
              class = "stats-drawer table-last market-only",
              tags$summary("Price table"),
              textInput("price_search", "SEARCH", placeholder = "Product or market", width = "240px"),
              uiOutput("price_table"),
              downloadButton("price_csv", "Download CSV")
            )
          )
          )
        )
      )
    ),
    site_footer()
  ),

  umusaruro_ui(),

  ikigega_ui(),

  about_ui(),

  ejo_ui(),

    tags$script(src = "js/nav.js?v=30"),
    tags$script(src = "js/select-lock.js?v=4"),
  tags$script(src = "js/maps.js?v=1"),
  tags$script(src = "js/rooms.js?v=8"),
    tags$script(src = "js/stage.js?v=3"),
  tags$script(src = "js/home.js?v=2"),
  tags$script(src = "js/about.js?v=1", defer = TRUE),
  tags$script(src = "js/ejo.js?v=25", defer = TRUE),
  tags$script(src = "js/ejo-inspect.js?v=7", defer = TRUE),
  tags$script(src = "js/ejo-portal.js?v=5", defer = TRUE),
  tags$script(src = "js/ejo-action.js?v=9", defer = TRUE),
  tags$script(src = "js/keepalive.js?v=2"),
  tags$script(src = "js/loading.js?v=4")
)
