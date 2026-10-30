# GSAS-style institutional portal for Ejo Hazaza solutions (login + desk).

ejo_portal_creds_ui <- function() {
  u <- ejo_portal_demo_users()
  tags$div(
    class = "gsas-creds",
    tags$p(class = "gsas-creds-title", `data-i18n` = "creds_title", "Demo credentials — copy into the boxes"),
        tags$p(class = "gsas-creds-pass", tags$span(`data-i18n` = "creds_pass", "Password for every account:"), " ", tags$code("123456")),
        tags$p(class = "gsas-creds-pass", "RISA governance admin: ", tags$code("risa"), " · same demo password. Sign-in is still required."),
    tags$ul(lapply(seq_len(nrow(u)), function(i) {
      tags$li(
        tags$code(u$username[i]),
        tags$span(paste0(" · ", u$institution[i])),
        tags$button(
          type = "button", class = "gsas-fill",
          `data-user` = u$username[i], `data-inst` = u$institution[i], `data-pass` = "123456",
          `data-i18n` = "fill", "Fill"
        )
      )
    }))
  )
}

ejo_portal_login_ui <- function() {
  tags$div(
    id = "gsas-login",
    class = "gsas-login",
    tags$div(
      class = "gsas-brand",
      tags$div(
        class = "gsas-brand-inner",
          tags$img(src = "ejo/coat-rwanda-color.png?v=1", alt = "Republic of Rwanda", class = "gsas-arms"),
        tags$h1(class = "gsas-acronym", "UMUGANURA"),
        tags$p(class = "gsas-fullname", `data-i18n` = "brand_sub", "Ejo Hazaza · Government delivery desk")
      )
    ),
    tags$div(
      class = "gsas-auth",
      tags$div(
        class = "gsas-auth-inner",
        tags$h2(`data-i18n` = "login", "Login"),
        tags$p(class = "gsas-hint", `data-i18n` = "login_hint", "Please enter your credentials!"),
        tags$label(class = "gsas-label", `for` = "gsas_user", `data-i18n` = "username", "Username"),
        textInput("gsas_user", NULL, value = "", width = "100%", placeholder = "admin.RISA@gmail.com"),
        tags$label(class = "gsas-label", `for` = "gsas_pass", `data-i18n` = "password", "Password"),
        tags$div(
          class = "gsas-pass-wrap",
          passwordInput("gsas_pass", NULL, value = "", width = "100%"),
          tags$button(type = "button", class = "gsas-eye", id = "gsas-eye", "Show", `aria-label` = "Toggle password")
        ),
        tags$p(class = "gsas-login-sample", "username = admin.RISA@gmail.com"),
        tags$p(class = "gsas-login-sample", "password = 12345"),
        actionButton("gsas_login_go", "Login", class = "gsas-login-btn"),
        checkboxInput("gsas_create_open", "Create account", FALSE),
        conditionalPanel(
          condition = "input.gsas_create_open == true",
          tags$div(
            class = "gsas-create",
            tags$p(class = "note", "RISA stores these details. Only the RISA directory can open them. The duty ticks set your level."),
            textInput("gsas_new_name", "FULL NAME", placeholder = "Your name", width = "100%"),
            textInput("gsas_new_user", "USERNAME", placeholder = "agr_huye", width = "100%"),
            selectInput("gsas_new_age", "AGE RANGE", choices = c("18–24", "25–34", "35–44", "45–54", "55+"), width = "100%"),
            textInput("gsas_new_id", "IDENTITY OR PASSPORT NUMBER", width = "100%"),
            selectInput("gsas_new_inst", "INSTITUTION", choices = setdiff(ejo_portal_institutions, "RISA"), width = "100%"),
            checkboxGroupInput(
              "gsas_duty", "WHERE YOU SIT",
              choiceNames = c(
                "I record harvest conditions in the field",
                "I audit technical figures before they go upward",
                "I authorise the plan and report to the ministry"
              ),
              choiceValues = c("field", "audit", "lead")
            ),
            passwordInput("gsas_new_pass", "PASSWORD", width = "100%"),
            passwordInput("gsas_new_pass2", "CONFIRM PASSWORD", width = "100%"),
            actionButton("gsas_create", "Classify and create", class = "btn-go")
          )
        ),
        uiOutput("gsas_login_msg"),
        tags$p(class = "gsas-foot", `data-i18n` = "powered", "Powered by Umuganura · Republic of Rwanda")
      )
    )
  )
}

ejo_portal_app_ui <- function() {
  tags$div(
    id = "gsas-app",
    class = "gsas-app is-hidden",
    tags$header(
      class = "gsas-top",
        tags$div(
          class = "gsas-ident",
          tags$img(src = "ejo/coat-rwanda-color.png?v=1", alt = "", class = "gsas-ident-arms"),
        tags$div(
          tags$strong(id = "gsas-agency", "MINAGRI"),
          tags$span(id = "gsas-role-tag", class = "gsas-rank", "officer")
        )
      ),
      tags$div(
        class = "gsas-top-actions",
        tags$div(
          class = "gsas-profile",
          tags$span(class = "gsas-avatar", id = "gsas-avatar", "U"),
          tags$span(id = "gsas-display", "User"),
          tags$button(type = "button", class = "gsas-bell", id = "gsas-bell", "Alerts", tags$span(id = "gsas-bell-n", class = "gsas-badge")),
          actionButton("gsas_logout", "Logout", class = "gsas-logout")
        )
      )
    ),
    tags$div(
      class = "gsas-body",
      tags$nav(
        class = "gsas-side",
        tags$button(type = "button", class = "gsas-nav is-on", `data-gsas-view` = "home", `data-i18n` = "nav_home", "Dashboard"),
        tags$button(type = "button", class = "gsas-nav", `data-gsas-view` = "inbox", `data-i18n` = "nav_inbox", "Solution inbox"),
        tags$button(type = "button", class = "gsas-nav", `data-gsas-view` = "pms", `data-i18n` = "nav_pms", "Tasks & schedule"),
        tags$button(type = "button", class = "gsas-nav", `data-gsas-view` = "desk", `data-i18n` = "nav_desk", "Open solution desk"),
        tags$button(type = "button", class = "gsas-nav", `data-gsas-view` = "alerts", `data-i18n` = "nav_alerts", "Alerts"),
        tags$button(type = "button", class = "gsas-nav", `data-gsas-view` = "survey", "Field survey"),
        tags$button(type = "button", class = "gsas-nav", `data-gsas-view` = "messages", "Messages"),
        tags$button(type = "button", class = "gsas-nav gsas-nav-risa", `data-gsas-view` = "risa", "RISA directory"),
        tags$button(type = "button", class = "gsas-back", `data-ej-sol-back` = "1", `data-i18n` = "nav_back", "← Back to Ejo Hazaza")
      ),
      tags$main(
        class = "gsas-main",
        tags$section(
          id = "gsas-view-home", class = "gsas-view is-on",
          tags$h2(`data-i18n` = "dash_title", "Delivery cockpit"),
          tags$p(id = "gsas-tier-hint", class = "note", `data-i18n` = "dash_sub", "Solutions led by your institution, schedule position, and clearance status."),
          tags$div(
            class = "gsas-exec",
            uiOutput("gsas_kpis"),
            uiOutput("gsas_next"),
            uiOutput("gsas_mail"),
            uiOutput("gsas_approve"),
            tags$button(type = "button", class = "btn-go gsas-brief", "Executive briefing"),
            tags$div(id = "gsas-brief-out", class = "note gsas-brief-slot"),
            uiOutput("gsas_home_list")
          ),
          tags$div(
            class = "gsas-tech",
            tags$h3("Engineering console"),
            tags$p(class = "note", "These two inputs are the operational parameters. The window below shows how the wait changes."),
            sliderInput("gsas_moisture", "STORAGE MOISTURE (%)", min = 8, max = 18, value = 14, step = 0.5, width = "100%"),
            sliderInput("gsas_queue", "COLD-ROOM SERVERS", min = 1, max = 8, value = 2, step = 1, width = "100%"),
            verbatimTextOutput("gsas_console"),
            actionButton("gsas_dispatch", "Generate & dispatch report", class = "btn-go")
          ),
          tags$div(
            class = "gsas-field",
            tags$h3("Field alerts"),
            uiOutput("gsas_field"),
            tags$h3("Field diagnostic"),
            textAreaInput("gsas_f_crop", "CROP STATUS", rows = 2, width = "100%"),
            textAreaInput("gsas_f_rot", "ROT OR BRUISING", rows = 2, width = "100%"),
            textAreaInput("gsas_f_transport", "TRANSPORT DELAYS", rows = 2, width = "100%"),
            textAreaInput("gsas_f_weather", "WEATHER ON THE FIELD", rows = 2, width = "100%"),
            tags$button(type = "button", class = "btn-go", id = "gsas-field-go", "Write the locked report"),
            tags$pre(id = "gsas-field-out", class = "gsas-lock"),
            tags$button(type = "button", class = "btn-go", id = "gsas-field-copy", "Copy Report Text"),
            tags$p(id = "gsas-field-note", class = "note"),
            tags$a(id = "gsas-field-mail", class = "btn-go", target = "_blank", href = "https://mail.google.com/", "Open Gmail")
          )
        ),
        tags$section(
          id = "gsas-view-inbox", class = "gsas-view",
          tags$h2(`data-i18n` = "inbox_title", "Solution inbox"),
          uiOutput("gsas_inbox")
        ),
        tags$section(
          id = "gsas-view-pms", class = "gsas-view",
          tags$h2(`data-i18n` = "pms_title", "Tasks & schedule"),
          uiOutput("gsas_pms")
        ),
        tags$section(
          id = "gsas-view-desk", class = "gsas-view",
          tags$h2(`data-i18n` = "desk_title", "Solution desk"),
          tags$p(class = "note", `data-i18n` = "desk_sub", "Funding, six-step framework, milestones, and partner reports for the opened solution."),
          uiOutput("ej_sol_page")
        ),
        tags$section(
          id = "gsas-view-alerts", class = "gsas-view",
          tags$h2(`data-i18n` = "alerts_title", "Alerts"),
          uiOutput("gsas_alerts")
        ),
        tags$section(
          id = "gsas-view-survey", class = "gsas-view",
          tags$h2("Field survey"),
          tags$p(class = "note", "One question at a time. App alerts land in this desk. SMS and WhatsApp stay in the local gateway queue until a provider is connected, and the text can be copied into mail."),
          uiOutput("gsas_survey")
        ),
        tags$section(
          id = "gsas-view-messages", class = "gsas-view",
          tags$h2("Messages"),
          tags$p(class = "note", "Each message is tagged with the recipient's username and opens only in that person's queue."),
          uiOutput("gsas_messages")
        ),
        tags$section(
          id = "gsas-view-risa", class = "gsas-view",
          tags$h2("RISA directory"),
          tags$p(class = "note", "Accounts, tiers, surveys, and message queues on this desk."),
          uiOutput("gsas_risa")
        )
      )
    )
  )
}

ejo_portal_ui <- function() {
  tags$section(
    id = "ejo-solution", class = "panel",
    ejo_portal_login_ui(),
    ejo_portal_app_ui()
  )
}

ejo_portal_kpi_row <- function(user, n_tasks, n_overdue) {
  tags$div(
    class = "gsas-kpis",
    tags$article(
      tags$span("My tasks"),
      tags$strong(as.character(n_tasks)),
      tags$p(user$institution)
    ),
    tags$article(
      tags$span("Overdue"),
      tags$strong(as.character(n_overdue)),
      tags$p(if (n_overdue > 0) "Likely finish month has passed" else "None past the likely finish")
    ),
    tags$article(
      tags$span("Role"),
      tags$strong(user$role),
      tags$p(user$display_name)
    )
  )
}

ejo_portal_step_bar <- function(step) {
  n <- suppressWarnings(as.integer(step))
  if (!length(n) || !is.finite(n)) n <- 1L
  tags$span(
    class = "gsas-steps",
    title = ejo_act_steps[[as.character(n)]],
    lapply(1:6, function(i) tags$i(class = if (i < n) "is-past" else if (i == n) "is-on" else NULL))
  )
}

ejo_portal_task_cards <- function(board, limit = 8) {
  if (!nrow(board)) return(tags$p(class = "note", "No activities name your institution yet."))
  open <- board[board$status != "done", , drop = FALSE]
  if (!nrow(open)) open <- board
  open <- open[order(-as.integer(open$overdue), open$months_left), , drop = FALSE]
  open <- head(open, limit)
  tags$div(
    class = "gsas-task-grid",
    lapply(seq_len(nrow(open)), function(i) {
      r <- open[i, ]
      st <- if (r$status %in% names(ejo_act_statuses)) ejo_act_statuses[[r$status]] else r$status
      tags$article(
        class = paste("gsas-task", if (isTRUE(r$overdue)) "is-late" else if (isTRUE(r$critical)) "is-crit"),
        tags$p(class = "gsas-task-kicker", paste(r$cause_label, "·", r$id, if (isTRUE(r$critical)) "· critical path" else "")),
        tags$h3(r$activity),
        tags$p(paste0(
          r$lead, " · ", r$start, " – ", r$finish, " · ",
          if (isTRUE(r$overdue)) paste0(abs(r$months_left), " mo overdue") else paste0(r$months_left, " mo left")
        )),
        ejo_portal_step_bar(r$framework_step),
        tags$p(class = "gsas-task-next", paste0(
          st, " · next: ", if (nzchar(r$next_owner)) r$next_owner else "—",
          if (nzchar(r$next_due)) paste0(" by ", r$next_due) else ""
        )),
        if (nzchar(r$last_note)) tags$p(class = "note", r$last_note) else tags$p(class = "note", "No report filed on this activity yet."),
        if (identical(r$status, "blocked")) tags$p(class = "gsas-task-block", paste0("Waiting on ", r$blocked_who, ": ", r$blocked_what))
      )
    })
  )
}

ejo_portal_sol_table <- function(sols, openable = TRUE) {
  if (!nrow(sols)) {
    return(tags$p(class = "note", `data-i18n` = "no_sols", "No solutions currently name your institution as lead or partner."))
  }
  tags$table(
    class = "hv-table gsas-table",
    tags$thead(tags$tr(
      tags$th("ID"), tags$th(`data-i18n` = "col_sol", "Solution"),
      tags$th(`data-i18n` = "col_cause", "Cause"), tags$th(`data-i18n` = "col_lead", "Lead"),
      tags$th(`data-i18n` = "col_capex", "Capital"), if (openable) tags$th("")
    )),
    tags$tbody(lapply(seq_len(nrow(sols)), function(i) {
      tags$tr(
        tags$td(tags$b(sols$id[i])),
        tags$td(sols$option[i]),
        tags$td(ejo_causes[[sols$cause[i]]]),
        tags$td(sols$lead[i]),
        tags$td(paste0(fmt_num(sols$capex_bn[i], 1), " bn")),
        if (openable) tags$td(
          tags$button(
            type = "button", class = "gsas-open-sol btn-go",
            `data-scope` = sols$cause[i], `data-act` = sols$id[i], `data-kind` = "solution",
            `data-i18n` = "open", "Open desk"
          )
        )
      )
    }))
  )
}
