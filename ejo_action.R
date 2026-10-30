# Ejo Hazaza — Action plan (One Acre–style shell, Umuganura brand).
# Landing + six tabs; Dashboard drills cause → solution cards. No GSAS login.

ejo_action_causes <- c(
  weather = "Rain at harvest",
  storage = "Drying and storage",
  handling = "Handling and packaging",
  transport = "Transport routes",
  cold = "Lack of cooling",
  glut = "Too much supply"
)

ejo_action_cause_img <- c(
  weather = "ejo/rain%20at%20harvest/1061301468447690579.jpg",
  storage = "ejo/drying/341499584238192256.jpg",
  handling = "ejo/handling/1140747780602942335.jpg",
  transport = "ejo/transport%20routes/1087197166270512469.jpg",
  cold = "ejo/lack%20of%20cooling/1115837245195757523.jpg",
  glut = "ejo/too%20much%20supply/540432024028847740.jpg"
)
ejo_action_cause_icon <- c(
  weather = "ejo/action/sol/weather-icon.png",
  storage = "ejo/action/sol/storage-icon.png",
  handling = "ejo/action/sol/handling-icon.png",
  transport = "ejo/action/sol/transport-icon.png",
  cold = "ejo/action/sol/cold-icon.png",
  glut = "ejo/action/sol/glut-icon.png"
)
ejo_action_tab_intro <- function(kicker, title, body, img) {
  tags$article(
    class = "ap-tab-intro",
    tags$div(
      class = "ap-tab-intro-copy",
      tags$p(class = "ap-tab-intro-kicker", kicker),
      tags$h2(title),
      tags$p(body)
    ),
    tags$div(
      class = "ap-tab-intro-media",
      style = paste0("background-image:url('", img, "');")
    )
  )
}

ejo_action_cause_blurb <- c(
  weather = "Wet harvest weeks leave grain mouldy when drying capacity is short.",
  storage = "Moisture and pests take value after the crop is bagged.",
  handling = "Rough packing and loading bruise and spill saleable produce.",
  transport = "Long detours and weak corridors raise time, cost and spoilage.",
  cold = "Heat after harvest ruins milk, vegetables and export lots.",
  glut = "Everyone harvests together — prices fall and unsold stock rots."
)

# Solution library: evidence-based options per cause (feeds Overview + Dashboard).
ejo_action_library <- list(
  weather = list(
    list(
      id = "W1", title = "Covered drying facilities",
      primary = TRUE,
      problem = "Rain during harvest and drying delays moisture removal and raises mould risk.",
      intervention = "Improve access to covered drying sheds and size capacity to harvest volumes and weather.",
      why = "Shelter cuts direct rain exposure; shared capacity handles peak arrivals that yards cannot.",
      success = "Facility sized, accessible, affordable, and operated with clear moisture checks.",
      outcome = "Less rain exposure, more reliable drying, lower physical loss.",
      complexity = "Medium", cost = "Medium–High",
      evidence = "Planning assumption linked to harvest-rain risk and cooperative dryer packages (W1/W5)."
    ),
    list(
      id = "W2", title = "Weather-based harvest & drying advisories",
      primary = FALSE,
      problem = "Farmers harvest into wet windows without a clear drying plan.",
      intervention = "Local forecasts + harvest readiness + drying windows, issued by SMS or coop boards.",
      why = "Timing reduces the share of crop that meets rain on the yard.",
      success = "Forecasts trusted; maturity, labour and field risk weighed — not rain alone.",
      outcome = "Better harvest/drying windows; fewer wet-bag events.",
      complexity = "Low", cost = "Low",
      evidence = "Meteo / NASA POWER harvest-month rain; advisory channels (W3/W4)."
    ),
    list(
      id = "W3", title = "Tarpaulins and raised drying surfaces",
      primary = FALSE,
      problem = "Crop dries on soil and stays open to sudden showers.",
      intervention = "Clean raised surfaces, suitable tarpaulins, turn and cover routines, moisture test before store.",
      why = "Low-cost protection for households when shared dryers are far or full.",
      success = "Materials kept clean; rain cover ready; moisture verified before bagging.",
      outcome = "Less soil contamination and rain splash; faster safe bagging.",
      complexity = "Low", cost = "Low",
      evidence = "Household drying package (W2)."
    ),
    list(
      id = "W4", title = "Shared solar or mechanical dryers",
      primary = FALSE,
      problem = "Peak volumes and wet spells exceed open-air drying.",
      intervention = "Evaluate capacity, energy, cost/kg, crop suitability, maintenance and coop management before install.",
      why = "Raises throughput when weather or volume makes open drying unreliable.",
      success = "Utilization and spare parts planned; operators trained; fees sustainable.",
      outcome = "Higher drying capacity in wet seasons where justified.",
      complexity = "High", cost = "High",
      evidence = "Not automatic everywhere — only where volume, weather and cost justify (W1)."
    ),
    list(
      id = "W5", title = "Harvest and drying coordination",
      primary = FALSE,
      problem = "Queues at dryers leave lots exposed while waiting.",
      intervention = "Schedule dryer slots by arrival, cycle time, crop compatibility and capacity.",
      why = "OR-style scheduling cuts wait and rain exposure at the facility gate.",
      success = "Farmers book slots; peak days staffed; incompatible lots not mixed.",
      outcome = "Shorter queues; less crop waiting in the open.",
      complexity = "Medium", cost = "Low–Medium",
      evidence = "Scheduling using capacity and cycle duration (planning model)."
    )
  ),
  storage = list(
    list(id = "S1", title = "Moisture testing & controlled drying", primary = TRUE,
         problem = "Grain enters store above safe moisture.",
         intervention = "Crop-specific meters, sampling, verify moisture before seal or warehouse intake.",
         why = "Hermetic bags and warehouses fail if the crop is still wet.",
         success = "Meters calibrated; thresholds match crop and storage length.",
         outcome = "Fewer mould and aflatoxin events in store.",
         complexity = "Low", cost = "Low", evidence = "RSB/RAB moisture practice; not one threshold for all crops."),
    list(id = "S2", title = "Hermetic (PICS) storage", primary = FALSE,
         problem = "Insects and air exchange spoil household stocks.",
         intervention = "Dry first, then seal suitable bags; protect from puncture and damp.",
         why = "Cuts insect loss when drying is already adequate.",
         success = "Never seal wet grain; inspect seals; compare cost to open bags.",
         outcome = "Lower insect damage in household stores.",
         complexity = "Low", cost = "Low–Medium", evidence = "Hermetic trial assumptions (S1)."),
    list(id = "S3", title = "Cooperative warehouses", primary = FALSE,
         problem = "No safe bulk store near production.",
         intervention = "Demand, site, ventilation, pests, rotation, records, security, fees.",
         why = "Shared store serves many farms over short roads (p-median logic).",
         success = "Sites verified on land and access; moisture and pest SOPs live.",
         outcome = "Lower storage loss at coop scale.",
         complexity = "High", cost = "High", evidence = "Warehouse package (S2); site model on Drying step."),
    list(id = "S4", title = "Warehouse receipt system", primary = FALSE,
         problem = "Farmers must sell at once because they cannot store and borrow.",
         intervention = "Grade, weigh, quality control, receipts, and bank link — separate from the building.",
         why = "Finance unlocks ‘store and sell later’.",
         success = "Eligible commodities and partners clear; stock records audit-ready.",
         outcome = "More deferred sales without distress prices.",
         complexity = "High", cost = "Medium", evidence = "WRS option (S3)."),
    list(id = "S5", title = "Metal silos & improved structures", primary = FALSE,
         problem = "Large coops need durable bulk capacity.",
         intervention = "Size, cost, maintenance and fill/empty procedures for silos or upgraded stores.",
         why = "Durable volume where bags are not enough.",
         success = "Throughput matches harvest; maintenance funded.",
         outcome = "Stable bulk storage for large lots.",
         complexity = "High", cost = "High", evidence = "Metal silo package (S5).")
  ),
  handling = list(
    list(id = "H1", title = "Reusable plastic crates", primary = TRUE,
         problem = "Sacks crush soft produce on the way to market.",
         intervention = "Crop-fit crates, load limits, clean/return logistics, cost vs sacks.",
         why = "Cuts bruising where produce is crush-sensitive.",
         success = "Return loop works; fees cover replacement.",
         outcome = "Higher saleable share after handling.",
         complexity = "Medium", cost = "Medium", evidence = "Crate break-even (H1)."),
    list(id = "H2", title = "Post-harvest handling procedures", primary = FALSE,
         problem = "Rough harvest-to-dispatch steps damage crop.",
         intervention = "Crop-specific harvest → sort → load → move → grade → pack → dispatch.",
         why = "Behaviour and process cut loss before packaging spend.",
         success = "Training reaches pickers and loaders; standards posted at collection.",
         outcome = "Less bruise and spill on the path to the buyer.",
         complexity = "Low", cost = "Low", evidence = "Extension handling package (H2)."),
    list(id = "H3", title = "Sorting and grading stations", primary = FALSE,
         problem = "Mixed quality lowers price and raises rejects.",
         intervention = "Capacity, space, standards, labour, reject/recovery, market specs.",
         why = "Grading protects the premium lot and finds uses for the rest.",
         success = "Standards match buyers; labour rostered at peak.",
         outcome = "Clearer grades; fewer market rejects.",
         complexity = "Medium", cost = "Medium", evidence = "Collection grading (H3)."),
    list(id = "H4", title = "Mechanical threshers & shellers", primary = FALSE,
         problem = "Manual threshing is slow and can damage grain.",
         intervention = "Compare throughput, damage, labour, energy, utilization, cost/kg.",
         why = "Mechanisation helps where volumes and labour justify it.",
         success = "Seasonal use high enough to pay; spare parts available.",
         outcome = "Faster processing with controlled damage.",
         complexity = "Medium", cost = "Medium–High", evidence = "Thresher package (H4)."),
    list(id = "H5", title = "Standard market packaging", primary = FALSE,
         problem = "Non-standard packs fail market or export specs.",
         intervention = "Cartons/nets to RSB/market rules; supplier and cost plan.",
         why = "Compliance keeps lots saleable.",
         success = "Packs available at collection; cost in the price.",
         outcome = "Fewer packing-related rejects.",
         complexity = "Low", cost = "Low–Medium", evidence = "Standard packaging (H5).")
  ),
  transport = list(
    list(id = "T1", title = "Optimized collection routes", primary = TRUE,
         problem = "Detours and weak routing raise time, cost and spoilage.",
         intervention = "Points, volumes, vehicles, road times, windows, perishability → route plan.",
         why = "Transportation LP / VRP cuts francs and hours on the road.",
         success = "Distances and capacities current; buyers accept windows.",
         outcome = "Lower transport cost/kg and damage in transit.",
         complexity = "Medium", cost = "Low–Medium", evidence = "Route model on Transport step (T1)."),
    list(id = "T2", title = "Sector collection centres", primary = FALSE,
         problem = "Trucks collect scattered small loads inefficiently.",
         intervention = "Candidate sites, throughput, access, load capacity, opex.",
         why = "One load point shortens farm-to-truck time.",
         success = "Land and access verified beyond the model site list.",
         outcome = "Fewer partial loads; faster dispatch.",
         complexity = "High", cost = "High", evidence = "Collection centres (T2)."),
    list(id = "T3", title = "Shared / ventilated transport", primary = FALSE,
         problem = "Single farmers cannot fill or protect a truck.",
         intervention = "Consolidate loads, schedule, cost-share, shade/vent or cold as needed.",
         why = "Pooling raises utilization and protection.",
         success = "Clear damage liability and fee rules.",
         outcome = "Better vehicle fill; less sun/rain damage.",
         complexity = "Medium", cost = "Medium", evidence = "Shared trucks (T4)."),
    list(id = "T4", title = "Feeder-road prioritization", primary = FALSE,
         problem = "Bad feeders block markets in peak season.",
         intervention = "Rank corridors by volume, access, cost, seasonality, benefits vs works cost.",
         why = "Prioritisation steers scarce road budgets.",
         success = "Engineering assessment still required after ranking.",
         outcome = "Highest-value corridors improved first.",
         complexity = "High", cost = "High", evidence = "Corridor rehab (T3)."),
    list(id = "T5", title = "Direct buyer & market coordination", primary = FALSE,
         problem = "Produce waits while buyers and trucks are unaligned.",
         intervention = "Match volumes, quality, delivery slots and transport.",
         why = "Coordination cuts gate wait and distress sales.",
         success = "Contracts or standing orders exist; quality rules clear.",
         outcome = "More on-time deliveries to paying buyers.",
         complexity = "Low–Medium", cost = "Low", evidence = "Night loading / buyer contracts (T5).")
  ),
  cold = list(
    list(id = "C1", title = "Solar cold rooms", primary = TRUE,
         problem = "Perishables wait in heat without sized cooling.",
         intervention = "Volumes, crop temps, arrivals, duration, power, cost, utilization, business model.",
         why = "Queue model sizes rooms so wait — and spoil — fall where justified.",
         success = "Throughput and fees cover opex; not built where demand is thin.",
         outcome = "Shorter harvest-to-cool time; lower spoil.",
         complexity = "High", cost = "High", evidence = "M/M/c sizing on Cooling step (C1)."),
    list(id = "C2", title = "Milk collection chillers", primary = FALSE,
         problem = "Milk spoils without rapid chill at collection.",
         intervention = "Capacity, power, hygiene, tests, schedules, maintenance, processor link.",
         why = "Dairy needs hours-not-days to safe temperature.",
         success = "Hygiene SOP live; downtime spare capacity.",
         outcome = "Fewer rejected milk lots.",
         complexity = "High", cost = "High", evidence = "Milk chiller package (C2)."),
    list(id = "C3", title = "Refrigerated transport", primary = FALSE,
         problem = "Cool rooms help little if the road is hot.",
         intervention = "When reefers beat alternatives; capacity and cost compare.",
         why = "Protects export and long hauls.",
         success = "Load factor and contracts sustain the truck.",
         outcome = "Lower in-transit heat loss on priority corridors.",
         complexity = "High", cost = "High", evidence = "Reefer trucks (C3)."),
    list(id = "C4", title = "Evaporative cooling", primary = FALSE,
         problem = "Markets need short-hold cool without full cold rooms.",
         intervention = "Use where humidity allows; water, maintenance, suitable crops.",
         why = "Low-energy buffer for short waits.",
         success = "Not used as overnight cold chain in humid peaks.",
         outcome = "Cheaper short holds where climate fits.",
         complexity = "Low", cost = "Low", evidence = "Zero-energy coolers (C4)."),
    list(id = "C5", title = "Packhouse & cold dispatch", primary = FALSE,
         problem = "Export lots miss cooled pack and 48-hour rules.",
         intervention = "Sort, pre-cool, store, dispatch with temperature logs.",
         why = "Keeps export value on the chain.",
         success = "Buyers’ time/temp rules met and logged.",
         outcome = "Fewer export rejects.",
         complexity = "High", cost = "High", evidence = "Pack-house / airport cold (C5).")
  ),
  glut = list(
    list(id = "G1", title = "Staggered planting calendars", primary = TRUE,
         problem = "Simultaneous harvest floods one market.",
         intervention = "Crop- and place-specific calendars using agronomy and rain — not price alone.",
         why = "Spreads arrivals before the crash.",
         success = "District extension owns the calendar; farmers can follow it.",
         outcome = "Less peak glut; stabler prices.",
         complexity = "Medium", cost = "Low", evidence = "Staggered planting (G1)."),
    list(id = "G2", title = "Processing surplus", primary = FALSE,
         problem = "Fresh market cannot absorb the peak.",
         intervention = "Compare paste, flour, juice, dried fruit on capacity, cost, demand, safety.",
         why = "Moves volume out of the fresh glut.",
         success = "Raw material and market both real; costs covered.",
         outcome = "Less unsold fresh waste; more product forms.",
         complexity = "High", cost = "High", evidence = "Processing units (G2)."),
    list(id = "G3", title = "Store and sell later", primary = FALSE,
         problem = "Distress sales at harvest trough prices.",
         intervention = "Compare future price vs store cost, shrink, finance risk (inventory LP logic).",
         why = "Holding pays only when the maths clears.",
         success = "Store fees < expected price lift; quality holds.",
         outcome = "Higher net when delay is justified.",
         complexity = "Medium", cost = "Medium", evidence = "Multi-period plan on Glut step (G3)."),
    list(id = "G4", title = "Institutional buyers", primary = FALSE,
         problem = "No reliable bulk outlet in the peak.",
         intervention = "Aggregate for schools/eligible buyers under real procurement rules.",
         why = "Steady offtake absorbs part of the glut.",
         success = "Contracts and quality specs exist — not assumed.",
         outcome = "More volume under known demand.",
         complexity = "Medium", cost = "Low", evidence = "Institutional buyers (G4)."),
    list(id = "G5", title = "Market information & other buyers", primary = FALSE,
         problem = "Farmers sell into the flooded stand by habit.",
         intervention = "Prices, quality, regional/export options — verify demand before diverting.",
         why = "Information cuts blind dumping on one market.",
         success = "Alternative buyers confirmed; logistics feasible.",
         outcome = "Better matching; less unsold peak stock.",
         complexity = "Low", cost = "Low", evidence = "Price / export links (G5).")
  )
)

ejo_action_playbook_bits <- function(sol, cause_lab) {
  list(
    mechanism = sol$problem,
    design = sol$intervention,
    steps = c(
      "Confirm the local problem with harvest volumes and loss evidence.",
      "Select this intervention only if crop, volume, weather, cost and access fit.",
      "Assign a lead and write operating procedures.",
      "Pilot, measure, then scale or stop."
    ),
    resources = "See cost category and evidence notes on the solution card; detailed BOQs follow feasibility.",
    stakeholders = "Lead institution from the national options list; cooperatives, districts and private partners as required.",
    conditions = paste("Appropriate for", cause_lab, "when conditions for success are met:", sol$success),
    indicators = c("Physical loss rate", "Cost per kg handled", "Utilization or coverage", "Quality / reject rate"),
    decisions = c(
      "If utilization is high and losses fall → test whether more capacity is justified.",
      "If underused → check access, fees, hours, crop fit and awareness.",
      "If losses stay high → revisit moisture, handling, pests or routing assumptions."
    ),
    risks = paste(sol$evidence, "Common failure: building first without demand, fees or operators.")
  )
}

ejo_action_card_sol <- function(cause, sol) {
  tags$button(
    type = "button",
    class = paste("ap-sol-card", if (isTRUE(sol$primary)) "is-primary" else NULL),
    `data-ap-sol` = sol$id,
    `data-ap-cause` = cause,
    tags$div(
      class = "ap-sol-card-top",
      tags$span(class = "ap-sol-kicker", if (isTRUE(sol$primary)) "Primary intervention" else "Supporting intervention"),
      tags$span(class = "ap-sol-id", sol$id)
    ),
    tags$h3(sol$title),
    tags$p(class = "ap-sol-line", tags$strong("Problem: "), sol$problem),
    tags$p(class = "ap-sol-line", tags$strong("Intervention: "), sol$intervention),
    tags$div(
      class = "ap-sol-meta",
      tags$span(paste("Complexity ·", sol$complexity)),
      tags$span(paste("Cost ·", sol$cost))
    ),
    tags$span(class = "ap-sol-cta", "Open playbook →")
  )
}

ejo_action_ui <- function() {
  hero_slides <- c(
    "ejo/land/overview.png",
    "ejo/land/rain.png",
    "ejo/land/drying.png",
    "ejo/land/handling.png",
    "ejo/land/truck.png",
    "ejo/land/cooling.png",
    "ejo/land/supply.png",
    "ejo/land/solutions.png"
  )
  tags$section(
    id = "ejo-solution",
    class = "panel ap-panel",
    tags$div(
      class = "ap-shell",
      # Brand header (Umuganura green — not One Acre yellow)
      tags$header(
        class = "ap-brand",
        tags$div(
          class = "ap-brand-inner",
          tags$a(
            href = "#", class = "ap-brand-logo", `data-ap-home` = "1",
            tags$img(src = "hero/logo.png", alt = "Umuganura"),
            tags$span("UMUGANURA")
          ),
          tags$p(class = "ap-brand-mid", "Ejo Hazaza · Action plan"),
          tags$p(class = "ap-brand-right", "Solutions that succeed · Farmers first")
        ),
        tags$nav(
          class = "ap-tabs", role = "tablist",
          tags$button(type = "button", class = "ap-tab is-on", `data-ap-tab` = "overview", "Solution Overview"),
          tags$button(type = "button", class = "ap-tab", `data-ap-tab` = "dashboard", "Dashboard"),
          tags$button(type = "button", class = "ap-tab", `data-ap-tab` = "roadmap", "Roadmap"),
          tags$button(type = "button", class = "ap-tab", `data-ap-tab` = "performance", "Performance Framework"),
          tags$button(type = "button", class = "ap-tab", `data-ap-tab` = "decision", "Decision Intelligence")
        )
      ),

      # Landing (One Acre–style hero with moving slides)
      tags$div(
        class = "ap-landing", id = "ap-landing",
        tags$div(
          class = "ap-hero",
          tags$div(
            class = "ap-hero-slides",
            lapply(seq_along(hero_slides), function(i) {
              tags$div(
                class = paste("ap-hero-slide", if (i == 1) "is-on" else NULL),
                style = paste0("background-image:url('", hero_slides[[i]], "?v=1');")
              )
            })
          ),
          tags$div(
            class = "ap-hero-copy",
            tags$p(class = "ap-hero-eyebrow", "ACTION PLAN — POST-HARVEST SOLUTIONS"),
            tags$h1("From six loss causes to interventions that succeed"),
            tags$p("Umuganura turns Rwanda’s post-harvest evidence into solution analytics, deep implementation roadmaps, budgets and leads — our data, our brand, our OR."),
            tags$div(
              class = "ap-hero-actions",
              tags$button(type = "button", class = "ap-btn ap-btn-primary", `data-ap-tab` = "dashboard", "Open causes dashboard"),
              tags$button(type = "button", class = "ap-btn ap-btn-ghost", `data-ap-tab` = "overview", "Explore analytics")
            )
          )
        ),
        ejo_action_tab_intro(
          "THE CHALLENGE",
          "Harvest value is still lost after the field",
          "Rain, weak drying, rough handling, long routes, missing cold chain and market gluts take francs from farmers and the public purse. This Action plan names what to do first — and how to succeed — without assuming one tool fits every district.",
          "ejo/land/overview.png"
        ),
        tags$div(
          class = "ap-landing-gallery",
          lapply(
            c(
              "ejo/land/rain.png", "ejo/land/drying.png", "ejo/land/handling.png", "ejo/land/truck.png",
              "ejo/land/cooling.png", "ejo/land/supply.png", "ejo/land/solutions.png", "ejo/land/overview.png"
            ),
            function(src) {
              tags$div(class = "ap-landing-shot", style = paste0("background-image:url('", src, "?v=6');"))
            }
          )
        ),
        tags$div(
          class = "ap-landing-strip",
          tags$article(tags$h3("Six causes"), tags$p("Rain, drying, handling, transport, cooling, glut — each with costed options.")),
          tags$article(tags$h3("Deep roadmaps"), tags$p("Step budgets, leads, forgotten checks, and the deployment map together.")),
          tags$article(tags$h3("Data visuals"), tags$p("Loss series, effect, capex and NPV from ejo_options and ejo_action_plans."))
        )
      ),

      tags$div(
        class = "ap-body",
        # Overview — long scroll after a solution is chosen
        tags$section(
          class = "ap-view is-on", `data-ap-view` = "overview",
          tags$div(id = "ap-hdr-overview", class = "ap-tab-intro ap-hdr-card"),
          tags$div(id = "ap-scroll-overview", class = "ap-long-scroll")
        ),

        # Dashboard — header card + cause → solution cards
        tags$section(
          class = "ap-view", `data-ap-view` = "dashboard",
          tags$div(id = "ap-hdr-dashboard", class = "ap-tab-intro ap-hdr-card"),
          tags$div(
            id = "ap-dash-causes",
            class = "ap-dash-layer is-on",
            tags$div(
              class = "ap-cause-grid",
              lapply(names(ejo_action_causes), function(cause) {
                tags$button(
                  type = "button",
                  class = "ap-cause-card",
                  `data-ap-open-cause` = cause,
                  tags$div(
                    class = "ap-cause-card-box",
                    tags$div(
                      class = "ap-cause-card-media",
                      style = paste0("background-image:url('", ejo_action_cause_img[[cause]], "?v=3');")
                    ),
                    tags$div(
                      class = "ap-cause-card-body",
                      tags$p(class = "ap-dash-badge", "Cause dashboard"),
                      tags$h3(ejo_action_causes[[cause]]),
                      tags$p(ejo_action_cause_blurb[[cause]]),
                      tags$span(
                        class = "ap-cause-count",
                        paste(length(ejo_action_library[[cause]]), "solutions")
                      )
                    )
                  )
                )
              })
            )
          ),
          tags$div(
            id = "ap-dash-sols",
            class = "ap-dash-layer",
            tags$div(
              class = "ap-toolbar",
              tags$button(type = "button", class = "ap-back", `data-ap-back` = "causes", "← Back to causes"),
              tags$div(
                tags$p(class = "ap-kicker", "SOLUTIONS"),
                tags$h2(id = "ap-sol-cause-title", "Solutions")
              )
            ),
            tags$div(id = "ap-sol-grid", class = "ap-sol-grid")
          )
        ),

        tags$section(
          class = "ap-view", `data-ap-view` = "roadmap",
          tags$div(id = "ap-hdr-roadmap", class = "ap-tab-intro ap-hdr-card"),
          tags$div(id = "ap-scroll-roadmap", class = "ap-long-scroll"),
          tags$div(
            class = "ap-map-frame ap-map-frame-tall",
            if (requireNamespace("leaflet", quietly = TRUE)) leaflet::leafletOutput("ap_deploy_map", height = "520px")
            else tags$p(class = "note", "Install leaflet to show the deployment map.")
          ),
          tags$div(id = "ap-scroll-roadmap-tail", class = "ap-long-scroll")
        ),
        tags$section(
          class = "ap-view", `data-ap-view` = "performance",
          tags$div(id = "ap-hdr-performance", class = "ap-tab-intro ap-hdr-card"),
          tags$div(id = "ap-scroll-performance", class = "ap-long-scroll")
        ),
        tags$section(
          class = "ap-view", `data-ap-view` = "decision",
          tags$div(id = "ap-hdr-decision", class = "ap-tab-intro ap-hdr-card"),
          tags$div(id = "ap-scroll-decision", class = "ap-long-scroll")
        )
      ),

      tags$button(type = "button", class = "ap-exit", id = "ap-exit", "← Back to Ejo Hazaza")
    ),
    # Library JSON for client drill-down (media_index loaded by ejo-action.js)
    tags$script(
      HTML(paste0(
        "window.AP_LIB = ",
        jsonlite::toJSON(
          lapply(names(ejo_action_library), function(cause) {
            list(
              cause = cause,
              label = unname(ejo_action_causes[[cause]]),
              img = unname(ejo_action_cause_img[[cause]]),
              icon = unname(ejo_action_cause_icon[[cause]]),
              or = unname(ejo_action_or_line[[cause]]),
              blurb = unname(ejo_action_cause_blurb[[cause]]),
              solutions = ejo_action_library[[cause]]
            )
          }),
          auto_unbox = TRUE, force = TRUE
        ),
        ";window.AP_MEDIA=window.AP_MEDIA||{};"
      ))
    )
  )
}

ejo_action_server <- function(input, output, session) {
  if (requireNamespace("leaflet", quietly = TRUE) && exists("district_lnglat")) {
    output$ap_deploy_map <- leaflet::renderLeaflet({
      m <- if (exists("hv_base_map", mode = "function")) hv_base_map() else {
        leaflet::leaflet() |>
          leaflet::addProviderTiles("CartoDB.Positron") |>
          leaflet::setView(lng = 29.9, lat = -1.95, zoom = 8)
      }
      ds <- if (exists("ejo_districts")) ejo_districts else names(district_lnglat)
      lng <- vapply(ds, function(d) {
        g <- district_lnglat[[d]]
        if (is.null(g)) return(NA_real_)
        if (!is.null(g$cx)) g$cx else if (!is.null(g$lng)) g$lng else NA_real_
      }, numeric(1))
      lat <- vapply(ds, function(d) {
        g <- district_lnglat[[d]]
        if (is.null(g)) return(NA_real_)
        if (!is.null(g$cy)) g$cy else if (!is.null(g$lat)) g$lat else NA_real_
      }, numeric(1))
      ok <- is.finite(lng) & is.finite(lat)
      if (any(ok)) {
        m <- leaflet::addCircleMarkers(
          m, lng = lng[ok], lat = lat[ok], radius = 5,
          color = "#14804a", fillColor = "#14804a", fillOpacity = 0.75, weight = 1,
          label = ds[ok],
          popup = paste0("<b>", ds[ok], "</b><br/>Action plan deployment district")
        )
      }
      m
    })
  }
  invisible({})
}
