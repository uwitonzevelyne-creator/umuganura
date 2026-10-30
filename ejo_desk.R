# Account tiers, field surveys, private message queues, and the RISA directory.
# DeepSeek wording is requested by the browser from the Node bridge, not from this process.

ejo_report_text <- function(question, answer, who, when) {
  paste(
    "Locked field report",
    paste("Question:", question),
    paste("Answer:", answer),
    paste("Filed by:", who),
    paste("When:", when),
    "This text cannot be edited. Copy it into mail if the gateway is quiet.",
    sep = "\n"
  )
}

ejo_desk_server <- function(input, output, session, ejo_user, ejo_db_tick) {
  bump <- function() ejo_db_tick(isolate(ejo_db_tick()) + 1L)

  output$gsas_console <- renderText({
    user <- ejo_user()
    req(user)
    m <- input$gsas_moisture
    q <- input$gsas_queue
    validate(need(is.numeric(m) && is.numeric(q), "Set moisture and the number of servers."))
    wait <- round(14 / q + pmax(0, m - 13) * 1.5, 1)
    paste0(
      "Moisture ", m, "%  ·  servers ", q, "\n",
      "Queue sketch: about ", wait, " months on the cold-room path.\n",
      if (m > 13) "Moisture is above 13%. Drying is the bottleneck. Bring it down before the grain goes into store.\n"
      else "Moisture is inside the storage band.\n",
      if (q < 3) "Fewer than 3 servers leaves the room queued. Add a server and the wait falls.\n"
      else "The server count covers the queue in this sketch.\n"
    )
  })

  output$gsas_mail <- renderUI({
    user <- ejo_user()
    req(user)
    if (!ejo_tier_sees_exec(ejo_user_tier(user))) return(NULL)
    ejo_db_tick()
    board <- tryCatch(ejo_activity_board(user$institution, ejo_sees_all(user)), error = function(e) data.frame())
    late <- if (nrow(board)) board[board$overdue, , drop = FALSE] else board
    line <- if (!nrow(late)) "No activity is past its likely finish."
    else paste(paste0(late$cause_label, " ", late$id, " (", abs(late$months_left), " mo overdue)"), collapse = "; ")
    body <- paste0(
      "Umuganura desk note\n\n",
      user$display_name, " (", user$institution, ") reports:\n", line,
      "\n\nPlease confirm the next owner and the open government step."
    )
    tags$div(
      class = "chart-card",
      tags$h3("Note to an institution"),
      selectInput("gsas_mail_to", "TO", choices = c("NISR", "RAB", "NAEB"), width = "100%"),
      textAreaInput("gsas_mail_body", "MESSAGE", value = body, rows = 5, width = "100%"),
      tags$a(
        class = "btn-go gsas-mail-link",
        href = paste0("mailto:?subject=", utils::URLencode("Umuganura desk note"), "&body=", utils::URLencode(body)),
        "Open in mail"
      )
    )
  })

  output$gsas_field <- renderUI({
    user <- ejo_user()
    req(user)
    ejo_db_tick()
    alerts <- tryCatch(ejo_db_alerts_for(user$username), error = function(e) data.frame())
    if (!nrow(alerts)) return(tags$p(class = "note", "No field alert is waiting."))
    tagList(lapply(seq_len(min(nrow(alerts), 8)), function(i) tags$article(
      class = "gsas-task",
      tags$p(class = "gsas-task-kicker", paste(alerts$channel[i], "·", alerts$created_at[i])),
      tags$h3(alerts$title[i]),
      tags$p(alerts$body[i])
    )))
  })

  output$gsas_survey <- renderUI({
    user <- ejo_user()
    req(user)
    ejo_db_tick()
    tier <- ejo_user_tier(user)
    surveys <- tryCatch(ejo_db_surveys(), error = function(e) data.frame())
    answers <- tryCatch(ejo_db_answers(), error = function(e) data.frame())
    mine <- if (nrow(answers)) answers[answers$respondent == user$username, , drop = FALSE] else answers
    tagList(
      if (tier %in% c("middle", "leader", "risa")) tags$div(
        class = "chart-card",
        tags$h3("One question for the field"),
        textInput("gsas_q", "QUESTION", width = "100%", placeholder = "What moisture did you read at the store this morning?"),
        radioButtons("gsas_channel", "SEND BY",
          choices = c("App alerts hub" = "app", "SMS gateway queue" = "sms", "WhatsApp queue" = "whatsapp"),
          selected = "app"),
        selectInput("gsas_target", "WHO ANSWERS", choices = c("Base officers" = "base", "Middle officers" = "middle", "Leaders" = "leader"), selected = "base", width = "100%"),
        actionButton("gsas_q_send", "Dispatch question", class = "btn-go"),
        tags$div(id = "gsas-deepseek-out", class = "note"),
        if (nrow(answers)) tagList(
          tags$button(type = "button", class = "btn-go gsas-brief", "Executive brief"),
          tags$div(id = "gsas-survey-brief", class = "note gsas-brief-slot"),
          tags$h3("Locked answers"),
          lapply(seq_len(min(nrow(answers), 12)), function(i) {
            q <- if (nrow(surveys)) surveys$question[match(answers$survey_id[i], surveys$id)] else ""
            txt <- ejo_report_text(q, answers$answer[i], answers$respondent[i], answers$filed_at[i])
            tags$article(
              class = "gsas-task",
              tags$pre(class = "gsas-lock", txt),
              tags$button(type = "button", class = "btn-go gsas-copy-prev", "Copy text"),
              if (nzchar(answers$summary[i])) tags$p(class = "note", answers$summary[i]) else
                tags$button(type = "button", class = "btn-go gsas-deepseek", `data-mode` = "summary", `data-id` = answers$id[i], `data-text` = answers$answer[i], "Summarise for the technician")
            )
          })
        )
      ),
      if (tier == "base") tags$div(
        class = "chart-card",
        tags$h3("Answer the open question"),
        if (!nrow(surveys)) tags$p(class = "note", "No question has been dispatched yet.")
        else {
          open_ids <- setdiff(surveys$id, mine$survey_id)
          open <- surveys[surveys$id %in% open_ids, , drop = FALSE]
          tagList(
            tags$div(id = "gsas-deepseek-out", class = "note"),
            if (nrow(mine)) lapply(seq_len(nrow(mine)), function(i) {
              q <- surveys$question[match(mine$survey_id[i], surveys$id)]
              txt <- ejo_report_text(q, mine$answer[i], user$display_name, mine$filed_at[i])
              tags$div(
                tags$pre(class = "gsas-lock", txt),
                tags$button(type = "button", class = "btn-go gsas-copy-prev", "Copy text"),
                tags$button(type = "button", class = "btn-go gsas-deepseek", `data-mode` = "summary", `data-id` = mine$id[i], `data-text` = mine$answer[i], "Summarise for the technician")
              )
            }),
            if (nrow(open)) tagList(
              selectInput("gsas_answer_id", "QUESTION", choices = stats::setNames(open$id, open$question), width = "100%"),
              textAreaInput("gsas_answer", "YOUR READING", rows = 3, width = "100%"),
              actionButton("gsas_answer_go", "File locked report", class = "btn-go")
            ) else tags$p(class = "note", "Every question sent to you already has a locked report.")
          )
        }
      )
    )
  })

  output$gsas_messages <- renderUI({
    user <- ejo_user()
    req(user)
    ejo_db_tick()
    dir <- tryCatch(ejo_db_directory(), error = function(e) data.frame())
    validate(need(nrow(dir) > 0, "The directory is empty."))
    others <- dir[dir$username != user$username, , drop = FALSE]
    ch <- stats::setNames(others$username, paste0(others$display_name, " · ", others$username))
    box <- tryCatch(ejo_db_messages_for(user$username, ejo_sees_all(user)), error = function(e) data.frame())
    tags$div(
      class = "chart-card",
      selectInput("gsas_msg_to", "TO", choices = ch, width = "100%"),
      textAreaInput("gsas_msg_body", "MESSAGE", rows = 3, width = "100%"),
      actionButton("gsas_msg_send", "Send to this queue", class = "btn-go"),
      if (!nrow(box)) tags$p(class = "note", "Your queue is empty.")
      else lapply(seq_len(nrow(box)), function(i) tags$article(
        class = "gsas-task",
        tags$p(class = "gsas-task-kicker", paste0(box$sender[i], " → ", box$recipient[i], " · ", box$created_at[i])),
        tags$p(box$body[i])
      ))
    )
  })

  output$gsas_risa <- renderUI({
    user <- ejo_user()
    req(user)
    validate(need(identical(ejo_user_tier(user), "risa"), "The directory is open to RISA."))
    ejo_db_tick()
    dir <- ejo_db_directory()
    surveys <- ejo_db_surveys()
    msgs <- ejo_db_messages_for(user$username, TRUE)
    tags$div(
      class = "chart-card",
      tags$p(class = "note", paste(nrow(dir), "accounts ·", nrow(surveys), "surveys ·", nrow(msgs), "messages")),
      tags$table(
        class = "hv-table gsas-table",
        tags$thead(tags$tr(tags$th("User"), tags$th("Name"), tags$th("Institution"), tags$th("Tier"), tags$th("Age"), tags$th("Identity"))),
        tags$tbody(lapply(seq_len(nrow(dir)), function(i) tags$tr(
          tags$td(dir$username[i]), tags$td(dir$display_name[i]),
          tags$td(dir$institution[i]), tags$td(dir$tier[i]),
          tags$td(dir$age_range[i]), tags$td(dir$id_doc[i])
        )))
      )
    )
  })

  observeEvent(input$gsas_create, {
    duties <- input$gsas_duty
    if (!length(duties)) {
      output$gsas_login_msg <- renderUI(tags$p(class = "note", "Tick at least one duty."))
      return()
    }
    pass <- if (is.null(input$gsas_new_pass)) "" else input$gsas_new_pass
    again <- if (is.null(input$gsas_new_pass2)) "" else input$gsas_new_pass2
    if (!identical(pass, again)) {
      output$gsas_login_msg <- renderUI(tags$p(class = "note", "The two passwords do not match."))
      return()
    }
    tier <- ejo_classify_tier(duties)
    name <- trimws(if (is.null(input$gsas_new_name)) "" else input$gsas_new_name)
    if (!nzchar(name)) name <- trimws(if (is.null(input$gsas_new_user)) "" else input$gsas_new_user)
    u <- tryCatch(ejo_db_create_user(
      input$gsas_new_user, pass, input$gsas_new_inst, name, tier, duties,
      if (is.null(input$gsas_new_age)) "" else input$gsas_new_age,
      if (is.null(input$gsas_new_id)) "" else input$gsas_new_id
    ), error = function(e) e)
    if (inherits(u, "error")) {
      output$gsas_login_msg <- renderUI(tags$p(class = "note", conditionMessage(u)))
      return()
    }
    ejo_user(u)
    bump()
    output$gsas_login_msg <- renderUI(NULL)
    session$sendCustomMessage("gsas_auth", list(
      ok = TRUE, username = u$username, institution = u$institution, role = u$role,
      display_name = u$display_name, tier = u$tier, view = "home"
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$gsas_q_send, {
    user <- ejo_user()
    req(user)
    q <- trimws(if (is.null(input$gsas_q)) "" else input$gsas_q)
    if (!nzchar(q)) return()
    if (!ejo_user_tier(user) %in% c("middle", "leader", "risa")) return()
    tryCatch(ejo_db_post_survey(q, input$gsas_target, input$gsas_channel, user$username), error = function(e) NULL)
    bump()
  }, ignoreInit = TRUE)

  observeEvent(input$gsas_answer_go, {
    user <- ejo_user()
    req(user)
    ans <- trimws(if (is.null(input$gsas_answer)) "" else input$gsas_answer)
    if (!nzchar(ans) || is.null(input$gsas_answer_id)) return()
    tryCatch(ejo_db_answer(input$gsas_answer_id, user$username, ans), error = function(e) NULL)
    bump()
  }, ignoreInit = TRUE)

  observeEvent(input$gsas_msg_send, {
    user <- ejo_user()
    req(user)
    body <- trimws(if (is.null(input$gsas_msg_body)) "" else input$gsas_msg_body)
    who <- input$gsas_msg_to
    if (!nzchar(body) || is.null(who) || !nzchar(who) || identical(who, user$username)) return()
    tryCatch(ejo_db_send_message(user$username, who, body), error = function(e) NULL)
    bump()
  }, ignoreInit = TRUE)

  observeEvent(input$gsas_summary_save, {
    user <- ejo_user()
    req(user)
    id <- input$gsas_summary_save$id
    text <- input$gsas_summary_save$text
    req(id, text)
    tryCatch(ejo_db_save_summary(id, text), error = function(e) NULL)
    bump()
  }, ignoreInit = TRUE)

  observe({
    user <- ejo_user()
    if (is.null(user)) {
      session$sendCustomMessage("gsas_badge", list(n = 0, hint = "", tier = ""))
      return()
    }
    ejo_db_tick()
    board <- tryCatch(ejo_activity_board(user$institution, ejo_sees_all(user)), error = function(e) data.frame())
    n_over <- if (nrow(board)) sum(board$overdue, na.rm = TRUE) else 0
    n_alert <- tryCatch(ejo_db_unread(user$username), error = function(e) 0L)
    tier <- ejo_user_tier(user)
    hint <- switch(tier,
      base = "A field question may be waiting. Answer it, then copy the locked report into your mail.",
      middle = "Check moisture and the cold-room servers. Those two inputs move the bottleneck.",
      leader = "An overdue critical activity can go as a note to NISR, RAB, or NAEB.",
      risa = "The directory, the surveys, and the private queues are open to RISA.",
      ""
    )
    session$sendCustomMessage("gsas_badge", list(n = unname(n_over + n_alert), hint = hint, tier = tier))
  })

  observeEvent(input$gsas_dispatch, {
    user <- ejo_user()
    req(user)
    m <- input$gsas_moisture
    q <- input$gsas_queue
    if (!is.numeric(m) || !is.numeric(q)) return()
    board <- tryCatch(ejo_activity_board(user$institution, ejo_sees_all(user)), error = function(e) data.frame())
    to <- if (nrow(board) && "report_to" %in% names(board) && nzchar(board$report_to[1])) board$report_to[1] else "NISR"
    gap <- pmax(0, m - 13)
    wait <- round(14 / q + gap * 1.5, 1)
    draft <- paste0(
      "Umuganura technician note\n\n",
      "To: ", to, "\n",
      "From: ", user$display_name, " (", user$institution, ")\n\n",
      "Calculated volumetric sketch: moisture is ", m, "%. Each point above 13% is treated as extra drying pressure before storage.\n",
      "Projected queue: about ", wait, " months at ", q, " cold-room servers.\n",
      "Estimated schedule impact: ", if (wait > 6) "the cold-room path is behind the 6-month mark." else "the cold-room path is inside the 6-month mark.", "\n",
      "This note is the console sketch from the sliders. It is not a new measured tonne count.\n"
    )
    showModal(modalDialog(
      title = paste("Dispatch to", to),
      tags$p(class = "note", "Review the summary, then open it in mail."),
      textAreaInput("gsas_dispatch_body", NULL, value = draft, rows = 10, width = "100%"),
      tags$a(class = "btn-go", target = "_blank", href = paste0(
        "https://mail.google.com/mail/?view=cm&fs=1&su=", utils::URLencode(paste("Umuganura note for", to), reserved = TRUE),
        "&body=", utils::URLencode(draft, reserved = TRUE)
      ), "Open Gmail"),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  }, ignoreInit = TRUE)

  output$gsas_approve <- renderUI({
    user <- ejo_user()
    req(user)
    if (!ejo_tier_sees_exec(ejo_user_tier(user))) return(NULL)
    ejo_db_tick()
    board <- tryCatch(ejo_activity_board(user$institution, ejo_sees_all(user)), error = function(e) data.frame())
    late <- if (nrow(board)) board[board$overdue, , drop = FALSE] else board
    if (!nrow(late)) return(tags$p(class = "note", "No overdue activity is waiting for sign-off."))
    ch <- stats::setNames(late$key, paste(late$cause_label, late$id, late$activity))
    tags$div(
      class = "chart-card",
      tags$h3("Approve phase advancement"),
      selectInput("gsas_approve_key", "OVERDUE ACTIVITY", choices = ch, width = "100%"),
      actionButton("gsas_approve", "Approve phase advancement", class = "btn-go")
    )
  })

  observeEvent(input$gsas_approve, {
    user <- ejo_user()
    req(user)
    key <- input$gsas_approve_key
    req(key)
    if (!ejo_tier_sees_exec(ejo_user_tier(user))) return()
    board <- tryCatch(ejo_activity_board(user$institution, ejo_sees_all(user)), error = function(e) NULL)
    req(board)
    row <- board[board$key == key, , drop = FALSE]
    req(nrow(row))
    row <- row[1, ]
    tryCatch(ejo_db_save_activity(
      key, "approved", row$blocked_who, row$blocked_what, row$framework_step,
      row$next_owner, row$next_due,
      paste(row$last_note, "Phase approved by", user$display_name),
      user$username
    ), error = function(e) NULL)
    bump()
  }, ignoreInit = TRUE)

  lapply(c("gsas_console", "gsas_mail", "gsas_field", "gsas_survey", "gsas_messages", "gsas_risa", "gsas_approve"), function(id) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  })
}
