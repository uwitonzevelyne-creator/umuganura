# About Umuganura — portfolio-style story, mission, values, and team.

about_ui <- function() {
  tags$section(
    id = "about",
    class = "panel about-panel",
    tags$div(
      class = "about-shell",

      # ---- Hero (image-1 composition) ----
      tags$section(
        class = "about-hero",
        tags$div(
          class = "about-hero-copy",
          tags$span(class = "about-pill", "Post-harvest loss platform · Rwanda"),
          tags$h1(
            class = "about-hero-title",
            "We count what the harvest ",
            tags$span(class = "about-accent", "left behind")
          ),
          tags$p(
            class = "about-hero-lede",
            "Umuganura turns Umusaruro, Isoko and Ikigega into one clear story: tonnes lost after the field, who pays, and what a farmer or ministry can do next."
          ),
          tags$div(
            class = "about-hero-actions",
            tags$button(type = "button", class = "about-btn about-btn-fill", `data-panel` = "ejo-hazaza", "Open Ejo Hazaza"),
            tags$button(type = "button", class = "about-btn about-btn-ghost", `data-panel` = "umusaruro", "Open Umusaruro")
          )
        ),
        tags$div(
          class = "about-hero-visual",
          tags$div(
            class = "about-hero-duo",
            tags$img(
              src = "about/Confident%20Black%20Women%20in%20Business%20Attire.png?v=1",
              alt = "Uwitonze Evelyne and Ishimwe Gloria",
              class = "about-hero-duo-photo"
            )
          ),
          tags$div(
            class = "about-proof",
            tags$strong("2 builders"),
            tags$span("Data analyst · Data engineer"),
            tags$p("Figures stay tied to loaded harvest, price and store tables — never invented.")
          )
        )
      ),

      # ---- How the platform works (accordion like services) ----
      tags$section(
        class = "about-block",
        tags$div(
          class = "about-block-head",
          tags$h2("How we bring ", tags$span(class = "about-accent", "harvest truth"), " to life"),
          tags$button(type = "button", class = "about-link-btn", `data-panel` = "umusaruro", "Open Umusaruro")
        ),
        tags$div(
          class = "about-accordion",
          about_acc("01", "Umusaruro", "What Rwanda harvests", TRUE,
            c("Season production", "District and crop cuts", "Yield against history"),
            "The harvest room holds what left the field. Every later loss figure starts here — not from a guessed national percentage."
          ),
          about_acc("02", "Isoko", "What the market paid", FALSE,
            c("Farm-gate and market prices", "Macro bridges", "Crop value signals"),
            "Isoko prices the tonne. Loss in francs only appears when a harvested kilogram meets a real price path."
          ),
          about_acc("03", "Ikigega", "What was stored and lost", FALSE,
            c("National stocks", "Food balance residual", "Grain after the season"),
            "Ikigega shows what entered store and what the balances imply was lost between harvest and plate."
          ),
          about_acc("04", "Ejo Hazaza", "Who pays and what to do", FALSE,
            c("Six loss causes", "Money lost and gained", "Action plan desk"),
            "Ejo Hazaza multiplies tonnes × price across causes, then carries an operations-research plan and institutional desk."
          ),
          about_acc("05", "Farm calculator", "A farmer's own harvest numbers", FALSE,
            c("Cause-specific inputs", "RWF loss charts", "English / Kinyarwanda PDF"),
            "The farm calculator lets a farmer type this season's spoiled and downgraded kilograms, see the franc loss, and download a tailored action PDF."
          )
        )
      ),

      # ---- Mission & Vision ----
      tags$section(
        class = "about-block about-mv",
        tags$h2("Mission & Vision"),
        tags$div(
          class = "about-mv-row",
          tags$div(class = "about-mv-icon", HTML("&#9678;")),
          tags$div(
            tags$h3("Our Mission"),
            tags$p("Make Rwanda's post-harvest loss visible, measurable and actionable — from national FAO balances down to a farmer's typed harvest — so less food and fewer francs leave the chain unused.")
          )
        ),
        tags$div(
          class = "about-mv-row",
          tags$div(class = "about-mv-icon", HTML("&#9673;")),
          tags$div(
            tags$h3("Our Vision"),
            tags$p("A Rwanda where every season's loss has a named cause, a price, a nearby place to act, and a clear plan for farmers, cooperatives and public institutions.")
          )
        )
      ),

      # ---- Core values ----
      tags$section(
        class = "about-block",
        tags$h2("Core Values"),
        tags$div(
          class = "about-values",
          about_value("&#9878;", "Data integrity", "Harvest, price and store figures stay tied to loaded sources. We do not invent a national loss number to fill a chart."),
          about_value("&#10227;", "Farm-to-policy bridge", "The same platform serves a farmer calculating one rain-damaged lot and a ministry reading the national cause mix."),
          about_value("&#9881;", "Practical innovation", "OR models and farmer PDFs shorten the path from a loss figure to a dryer, hangar, route or clearance.")
        )
      ),

      # ---- Who benefits ----
      tags$section(
        class = "about-block",
        tags$h2(class = "about-center", "Who benefits from Umuganura?"),
        tags$div(
          class = "about-benefit-grid",
          about_benefit("&#127806;", "Farmers & cooperatives", "Measure this harvest's loss in RWF, see nearby dryers and markets, and leave with a printable action plan."),
          about_benefit("&#128202;", "Researchers & analysts", "Trace tonnes and francs across causes, crops and years without leaving the FAO and price evidence behind."),
          about_benefit("&#127963;", "Ministries & partners", "See who pays, which cause dominates, and how the action-plan desk tracks delivery across institutions.")
        )
      ),

      # ---- Team ----
      tags$section(
        class = "about-block about-team-block",
        tags$div(
          class = "about-block-head",
          tags$div(
            tags$p(class = "about-kicker", "About the builders"),
            tags$h2("Who is ", tags$span(class = "about-accent", "behind Umuganura"), "?")
          )
        ),
        tags$div(
          class = "about-team-grid",
          about_person(
            photo = "about/Confident%20Corporate%20Portrait%20in%20Glasses.png?v=1",
            name = "Uwitonze Evelyne",
            role = "Data Analyst",
            bio = "Data analyst focused on turning Rwanda's harvest, market and store tables into clear loss stories. She shapes the indicators, cause breakdowns and farmer-facing calculator so every franc lost can be explained from a typed input or a loaded balance.",
            education = "Applied statistics: Actuarial Science",
            school = "University of Rwanda",
            skills = c("Loss accounting", "Dashboard analytics", "Indicators", "Cause breakdowns")
          ),
          about_person(
            photo = "about/Confident%20Corporate%20Portrait%20with%20Braids.png?v=1",
            name = "Ishimwe Gloria",
            role = "Data Engineer",
            bio = "Data engineer who keeps Umuganura's pipelines reliable — cleaning production and price feeds, wiring the bridge services, and making sure the farm calculator and Ejo Hazaza read the same trusted figures every session.",
            education = "Applied statistics: Economic Statistics",
            school = "University of Rwanda",
            skills = c("Data engineering", "ETL pipelines", "API bridge", "Data quality")
          )
        )
      )
    ),
    site_footer()
  )
}

about_acc <- function(num, title, subtitle, open = FALSE, chips = character(), body = "") {
  body_ui <- tags$div(
    class = "about-acc-body",
    if (length(chips)) tags$div(class = "about-chips", lapply(chips, function(c) tags$span(c))),
    tags$p(body)
  )
  summary_ui <- tags$summary(
    tags$span(class = "about-acc-num", paste0(num, ".")),
    tags$span(class = "about-acc-title", title),
    tags$span(class = "about-acc-sub", subtitle),
    tags$span(class = "about-acc-arrow", HTML("&rarr;"))
  )
  if (isTRUE(open)) {
    tags$details(class = "about-acc is-open", open = NA, summary_ui, body_ui)
  } else {
    tags$details(class = "about-acc", summary_ui, body_ui)
  }
}

about_value <- function(icon, title, body) {
  tags$article(
    class = "about-value",
    tags$div(class = "about-value-icon", HTML(icon)),
    tags$div(
      tags$h3(title),
      tags$p(body)
    )
  )
}

about_benefit <- function(icon, title, body) {
  tags$article(
    class = "about-benefit",
    tags$div(class = "about-benefit-icon", HTML(icon)),
    tags$h3(title),
    tags$p(body)
  )
}

about_person <- function(photo, name, role, bio, education, school, skills = character()) {
  tags$article(
    class = "about-person",
    tags$div(
      class = "about-person-media",
      tags$img(src = photo, alt = name)
    ),
    tags$div(
      class = "about-person-body",
      tags$h3(name),
      tags$p(class = "about-person-role", role),
      tags$p(class = "about-person-bio", bio),
      tags$div(
        class = "about-person-edu",
        tags$span(class = "about-person-edu-icon", HTML("&#127891;")),
        tags$div(
          tags$strong(education),
          tags$span(school)
        )
      ),
      if (length(skills)) tags$div(class = "about-person-skills", lapply(skills, function(s) tags$span(s)))
    )
  )
}
