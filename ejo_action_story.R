# Long-scroll story blocks for Action plan Overview (One Acre–style depth, Umuganura content).

ejo_action_or_line <- c(
  weather = "Monte Carlo harvest-rain seasons + expected monetary value for defence options",
  storage = "p-median facility location — warehouses and dryers on short roads to harvest",
  handling = "Pareto loss by product + crate break-even",
  transport = "Transportation LP on real district–market roads",
  cold = "M/M/c queue (Erlang C) — cold rooms sized to waiting horticulture",
  glut = "Multi-period inventory LP — sell now vs store and sell later"
)

ejo_action_band <- function(kicker, title, body, media = NULL, reverse = FALSE) {
  tags$article(
    class = paste("ap-band", if (isTRUE(reverse)) "is-reverse" else NULL),
    if (!is.null(media)) tags$div(class = "ap-band-media", style = paste0("background-image:url('", media, "');")),
    tags$div(
      class = "ap-band-copy",
      tags$p(class = "ap-kicker", kicker),
      tags$h2(title),
      if (is.character(body)) lapply(body, function(p) tags$p(p)) else body
    )
  )
}

ejo_action_overview_story <- function() {
  tagList(
    ejo_action_band(
      "THE CHALLENGE",
      "Rwanda’s harvest still loses value after it leaves the field",
      c(
        "Umusaruro counts what grows. Isoko prices it. Ikigega shows what is stored. Ejo Hazaza multiplies tonnes lost × price into francs lost — by cause — then carries an operations-research plan into this Action plan.",
        "Smallholders and cooperatives face rain at harvest, weak drying, rough handling, long routes, missing cold chain, and market gluts. Blanket tools fail: a mechanical dryer is not always first, and a cold room is wasteful where volumes cannot pay.",
        "This Overview answers one question for each cause: what should be considered first, why that mix, and under which conditions it succeeds — from an evidence-based solution library, not a single gadget."
      ),
      media = "ejo/land/overview.png?v=5"
    ),
    ejo_action_band(
      "HOW WE KNOW",
      "Six causes, six OR engines, one delivery desk",
      tagList(
        tags$p("Cause pages in Ejo Hazaza hold the deep models. Action plan holds the solutions and how to perform them."),
        tags$ul(class = "ap-or-list", lapply(names(ejo_action_or_line), function(k) {
          tags$li(tags$strong(ejo_action_causes[[k]]), " — ", ejo_action_or_line[[k]])
        })),
        tags$p("Options and effects live in ejo_options.csv. Implementation activities and PERT durations live in ejo_action_plans.csv. A 0–1 knapsack under each cause budget proposes a default package; you can still open every supporting intervention.")
      ),
      media = "ejo/land/solutions.png?v=5",
      reverse = TRUE
    ),
    ejo_action_band(
      "PIPELINE",
      "From loss figures to interventions that succeed",
      c(
        "1 · Measure — NISR SAS, NAEB prices, APHLIS stage shares, NASA POWER rain and heat where the cause needs them.",
        "2 · Diagnose — open the matching Ejo cause step for maps, simulations and charts.",
        "3 · Recommend — Solution Overview cards below name the primary intervention and supporting set.",
        "4 · Playbook — Dashboard opens the nine-block intervention guide.",
        "5 · Roadmap · Performance · Decisions · Map — follow the selected solution until evaluation says scale, fix or stop."
      ),
      media = "ejo/land/drying.png?v=5"
    ),
    tags$div(
      class = "ap-story-block",
      tags$p(class = "ap-kicker", "NATIONAL LOSS STORY"),
      tags$h2("Money left on the table — then money a plan can keep"),
      tags$p("On the Ejo land carousel and overview desk you already see farmer and government loss windows and the forward gain of the default package (2026–2030). Action plan does not redraw those charts; it turns them into delivery choices."),
      tags$div(
        class = "ap-stat-row",
        tags$article(tags$span("Causes tracked"), tags$strong("6"), tags$p("Rain · drying · handling · transport · cooling · glut")),
        tags$article(tags$span("Solution playbooks"), tags$strong(as.character(sum(vapply(ejo_action_library, length, 1L)))), tags$p("Same structure so teams can compare")),
        tags$article(tags$span("OR methods"), tags$strong("6"), tags$p("Simulation · location · Pareto · LP · queues · inventory")),
        tags$article(tags$span("Delivery tabs"), tags$strong("6"), tags$p("Overview through deployment map"))
      )
    ),
    tags$div(
      class = "ap-story-block",
      tags$p(class = "ap-kicker", "DESIGN RULE"),
      tags$h2("Never assume one tool fits every district"),
      tags$p("Primary interventions below are starting points. Crop type, harvest volume, weather, infrastructure, cost and access must still clear the conditions for success on each card. Supporting interventions (advisories, tarpaulins, scheduling, market information) often unlock the primary tool."),
      tags$p("If a facility would sit idle, Decision Intelligence says investigate access and fees before building another. If utilization is high and losses fall, the same tab asks whether more capacity is justified — with the OR model re-run on the cause step when constraints change.")
    ),
    # Deep per-cause narrative before the recommendation cards
    lapply(names(ejo_action_library), function(cause) {
      sols <- ejo_action_library[[cause]]
      primary <- Filter(function(s) isTRUE(s$primary), sols)[[1]]
      support <- Filter(function(s) !isTRUE(s$primary), sols)
      tags$div(
        class = "ap-cause-story",
        tags$div(
          class = "ap-cause-story-hero",
          style = paste0("background-image:url('", ejo_action_cause_img[[cause]], "?v=5');")
        ),
        tags$div(
          class = "ap-cause-story-body",
          tags$p(class = "ap-kicker", paste("CAUSE ·", toupper(ejo_action_causes[[cause]]))),
          tags$h2(ejo_action_causes[[cause]]),
          tags$p(class = "ap-lead", ejo_action_cause_blurb[[cause]]),
          tags$p(tags$strong("Operations research: "), ejo_action_or_line[[cause]]),
          tags$h3("Priority problem"),
          tags$p(primary$problem),
          tags$h3("Primary intervention"),
          tags$p(tags$strong(primary$title), " — ", primary$intervention),
          tags$h3("Supporting interventions"),
          tags$ul(lapply(support, function(s) tags$li(tags$strong(s$title), " — ", s$intervention))),
          tags$h3("Why this combination"),
          tags$p(primary$why),
          tags$p("Together they address planning, physical protection, capacity and coordination — not only hardware."),
          tags$h3("Expected outcome"),
          tags$p(primary$outcome),
          tags$h3("Conditions for success"),
          tags$p(primary$success),
          tags$div(
            class = "ap-overview-meta",
            tags$span(paste("Complexity ·", primary$complexity)),
            tags$span(paste("Cost ·", primary$cost)),
            tags$span(paste("Lead card ·", primary$id)),
            tags$span(paste(length(sols), "playbooks"))
          ),
          tags$p(class = "ap-evidence", tags$strong("Evidence & assumptions: "), primary$evidence),
          tags$button(
            type = "button", class = "ap-btn ap-btn-primary",
            `data-ap-open-cause` = cause, "Open this cause in Dashboard →"
          )
        )
      )
    }),
    tags$div(
      class = "ap-story-block",
      tags$p(class = "ap-kicker", "HOW TO USE THE TABS"),
      tags$h2("A stable path from story to map"),
      tags$ol(class = "ap-steps-ol",
        tags$li(tags$strong("Solution Overview"), " — you are here: scroll the challenge, pipeline and every cause recommendation."),
        tags$li(tags$strong("Dashboard"), " — cause cards → solution cards → nine-block playbook."),
        tags$li(tags$strong("Roadmap"), " — stages, outputs and who moves next for the selected solution."),
        tags$li(tags$strong("Performance Framework"), " — indicators, units and how to set targets without inventing numbers."),
        tags$li(tags$strong("Decision Intelligence"), " — if results are good or poor, what to do next."),
        tags$li(tags$strong("Deployment Map"), " — always-on Rwanda map plus fit rules for the selected intervention.")
      ),
      tags$p("The green header and tabs stay fixed while you scroll this Overview — same stability pattern as a long research landing page.")
    ),
    tags$div(
      class = "ap-story-block ap-story-end",
      tags$p(class = "ap-kicker", "CARD LIBRARY"),
      tags$h2("Compact recommendation cards"),
      tags$p("Each card below repeats the primary recommendation in a comparable format — problem, intervention, why, outcome, success conditions, complexity, cost and evidence — then jumps into Dashboard.")
    )
  )
}

ejo_action_dash_intro <- function() {
  tagList(
    tags$div(
      class = "ap-story-block",
      tags$p(class = "ap-kicker", "HOW THIS DASHBOARD WORKS"),
      tags$h2("Causes first, then solutions, then the playbook"),
      tags$p("Each professional box is a cause dashboard. Open it to see every intervention for that cause as solution cards. Open a solution to load the nine-block playbook and to feed Roadmap, Performance, Decision Intelligence and the Deployment Map."),
      tags$p("Deep OR charts stay on Ejo cause steps. Here you compare and implement.")
    ),
    tags$div(
      class = "ap-or-strip",
      lapply(names(ejo_action_or_line), function(k) {
        tags$article(
          tags$p(class = "ap-dash-badge", "OR"),
          tags$h3(ejo_action_causes[[k]]),
          tags$p(ejo_action_or_line[[k]])
        )
      })
    ),
    tags$div(
      class = "ap-story-block",
      tags$p(class = "ap-kicker", "BEFORE YOU CLICK"),
      tags$h2("What good looks like inside a cause"),
      tags$ul(
        tags$li("Primary card marked — start there unless local conditions fail."),
        tags$li("Supporting cards — advisories, low-cost protection, scheduling, information."),
        tags$li("Playbook — mechanism, design, steps, resources, stakeholders, conditions, indicators, decisions, risks."),
        tags$li("Back anytime — causes ← solutions ← playbook.")
      )
    )
  )
}

ejo_action_tab_scaffold <- function(kind) {
  # Rich default content when no solution selected yet
  if (identical(kind, "roadmap")) {
    return(tagList(
      tags$div(class = "ap-story-block",
        tags$h3("Standard implementation spine"),
        tags$p("Every playbook maps onto the same spine. Selecting a solution fills the stage cards with that intervention’s wording."),
        tags$ol(
          tags$li("Diagnosis — volumes, loss evidence, access gaps."),
          tags$li("Feasibility — crop, weather, infrastructure, cost, operators."),
          tags$li("Design — the intervention itself."),
          tags$li("Financing — capital and operating costs in the card’s cost band."),
          tags$li("Procurement / setup — equipment or procedures and SOPs."),
          tags$li("Training — leads, coop staff, users."),
          tags$li("Monitoring — Performance Framework indicators."),
          tags$li("Evaluation — Decision Intelligence; scale, fix or stop.")
        )
      ),
      tags$div(id = "ap-roadmap", class = "ap-roadmap")
    ))
  }
  if (identical(kind, "performance")) {
    return(tagList(
      tags$div(class = "ap-story-block",
        tags$h3("Indicator library principles"),
        tags$p("No universal KPI list. Transport and cold storage need different measures. Each indicator should name definition, unit, calculation, baseline, target method, data source, frequency, role and limits — and never invent a target without evidence or a labelled planning assumption.")
      ),
      tags$div(id = "ap-performance", class = "ap-perf")
    ))
  }
  if (identical(kind, "decision")) {
    return(tagList(
      tags$div(class = "ap-story-block",
        tags$h3("From measurement to action"),
        tags$p("Decision rules are stored with the solution library. Advanced cases re-open the cause OR model with updated constraints (capacity, fees, service area, routes).")
      ),
      tags$div(id = "ap-decision", class = "ap-decision")
    ))
  }
  NULL
}
