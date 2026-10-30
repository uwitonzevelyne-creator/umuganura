# SQLite project-management store for Ejo Hazaza solutions.
# Links partner reports ↔ six-step framework ↔ funding clearance, with login.

ejo_db_ok <- requireNamespace("digest", quietly = TRUE) &&
  requireNamespace("DBI", quietly = TRUE) &&
  requireNamespace("RSQLite", quietly = TRUE)
if (!ejo_db_ok) {
  warning("Ejo PM database needs packages digest, DBI, and RSQLite. Install with renv::install('RSQLite').")
}

ejo_db_path <- file.path("reports", "ejo_pm.sqlite")

ejo_db_hash <- function(user, pass) {
  digest::digest(paste0(user, "::", pass, "::umuganura-ejo"), algo = "sha256")
}

ejo_portal_institutions <- c(
  "MINAGRI", "RAB", "NISR", "MINECOFIN", "NAEB", "RSB",
  "MININFRA", "PRIVATE SECTOR", "RISA", "OTHER"
)

ejo_portal_demo_users <- function() {
  data.frame(
    username = c(
      "admin", "minagri", "rab", "nisr", "minecofin", "naeb", "rsb",
      "mininfra", "private", "other", "admin.risa@gmail.com"
    ),
    password = c(rep("123456", 10), "12345"),
    institution = c(
      "MINAGRI", "MINAGRI", "RAB", "NISR", "MINECOFIN", "NAEB", "RSB",
      "MININFRA", "PRIVATE SECTOR", "OTHER", "RISA"
    ),
    role = c(
      "admin", "lead", "partner", "oversight", "oversight", "partner", "partner",
      "partner", "partner", "partner", "risa"
    ),
    display_name = c(
      "Platform admin", "MINAGRI officer", "RAB officer", "NISR officer",
      "MINECOFIN officer", "NAEB officer", "RSB officer", "MININFRA officer",
      "Private sector partner", "Other institution", "RISA governance admin"
    ),
    tier = c(
      "leader", "leader", "middle", "middle", "leader", "leader", "middle",
      "middle", "base", "base", "risa"
    ),
    stringsAsFactors = FALSE
  )
}

ejo_db_connect <- function() {
  if (!isTRUE(ejo_db_ok)) stop("Ejo database packages are not available.")
  dir.create(dirname(ejo_db_path), showWarnings = FALSE, recursive = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ejo_db_path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  con
}

ejo_db_seed_users <- function(con) {
  seed <- ejo_portal_demo_users()
  seed$password_hash <- mapply(ejo_db_hash, seed$username, seed$password, USE.NAMES = FALSE)
  for (i in seq_len(nrow(seed))) {
    DBI::dbExecute(con, "
      INSERT INTO users (username, password_hash, institution, role, display_name, tier)
      VALUES (?, ?, ?, ?, ?, ?)
      ON CONFLICT(username) DO UPDATE SET
        password_hash = excluded.password_hash,
        institution = excluded.institution,
        role = excluded.role,
        display_name = excluded.display_name,
        tier = excluded.tier
    ", params = list(
      seed$username[i], seed$password_hash[i], seed$institution[i],
      seed$role[i], seed$display_name[i], seed$tier[i]
    ))
  }
  invisible(TRUE)
}

ejo_db_init <- function() {
  if (!isTRUE(ejo_db_ok)) return(invisible(FALSE))
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      username TEXT PRIMARY KEY,
      password_hash TEXT NOT NULL,
      institution TEXT NOT NULL,
      role TEXT NOT NULL,
      display_name TEXT NOT NULL
    )")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS solution_state (
      option_id TEXT PRIMARY KEY,
      cause TEXT,
      fund_source TEXT,
      fund_status TEXT,
      framework_step TEXT,
      pm_status TEXT,
      updated_by TEXT,
      updated_at TEXT
    )")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS progress_reports (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      ref TEXT UNIQUE,
      filed_at TEXT,
      option_id TEXT,
      cause TEXT,
      filed_by TEXT,
      institution TEXT,
      addressed_to TEXT,
      activity TEXT,
      location TEXT,
      month_label TEXT,
      note TEXT,
      tonnes TEXT,
      framework_step TEXT,
      framework_title TEXT,
      fund_source TEXT,
      fund_status TEXT,
      pm_status TEXT,
      milestone TEXT,
      next_action TEXT,
      next_owner TEXT,
      next_when TEXT
    )")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS step_milestones (
      option_id TEXT,
      step_id TEXT,
      milestone_key TEXT,
      done INTEGER DEFAULT 0,
      done_by TEXT,
      done_at TEXT,
      PRIMARY KEY (option_id, step_id, milestone_key)
    )")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS activity_state (
      act_key TEXT PRIMARY KEY,
      cause TEXT,
      act_id TEXT,
      status TEXT DEFAULT 'not_started',
      blocked_who TEXT,
      blocked_what TEXT,
      framework_step TEXT DEFAULT '1',
      next_owner TEXT,
      next_due TEXT,
      last_note TEXT,
      updated_by TEXT,
      updated_at TEXT
    )")
  ejo_db_add_col(con, "users", "tier", "TEXT")
  ejo_db_add_col(con, "users", "duties", "TEXT")
  ejo_db_add_col(con, "users", "age_range", "TEXT")
  ejo_db_add_col(con, "users", "id_doc", "TEXT")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS desk_surveys (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      question TEXT NOT NULL,
      target_tier TEXT NOT NULL,
      channel TEXT NOT NULL,
      created_by TEXT,
      created_at TEXT
    )")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS desk_answers (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      survey_id INTEGER NOT NULL,
      respondent TEXT NOT NULL,
      answer TEXT NOT NULL,
      filed_at TEXT,
      locked INTEGER DEFAULT 1,
      summary TEXT,
      UNIQUE (survey_id, respondent)
    )")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS desk_alerts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT NOT NULL,
      title TEXT,
      body TEXT,
      channel TEXT,
      created_at TEXT,
      seen INTEGER DEFAULT 0
    )")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS desk_messages (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      sender TEXT NOT NULL,
      recipient TEXT NOT NULL,
      body TEXT NOT NULL,
      created_at TEXT
    )")
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  ejo_db_seed_users(con)
  ejo_db_seed_activities(con)
  invisible(TRUE)
}

ejo_db_add_col <- function(con, table, col, def) {
  info <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", table))
  if (!col %in% info$name) DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s %s", table, col, def))
  invisible(TRUE)
}

ejo_db_auth <- function(user, pass, institution = NULL) {
  user <- tolower(trimws(as.character(user)[1]))
  pass <- as.character(pass)[1]
  if (!nzchar(user) || !nzchar(pass)) return(NULL)
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  row <- DBI::dbGetQuery(
    con,
    "SELECT username, institution, role, display_name, tier FROM users WHERE username = ? AND password_hash = ?",
    params = list(user, ejo_db_hash(user, pass))
  )
  if (!nrow(row)) return(NULL)
  u <- as.list(row[1, ])
  if (!is.null(institution) && nzchar(trimws(institution))) {
    inst <- trimws(as.character(institution)[1])
    if (!identical(u$role, "admin") && !identical(u$role, "risa") && !identical(toupper(u$institution), toupper(inst))) {
      return(NULL)
    }
    if (identical(u$role, "admin")) u$institution <- inst
  }
  u$tier <- ejo_user_tier(u)
  u
}

ejo_user_tier <- function(user) {
  if (is.null(user)) return("base")
  t <- tolower(as.character(user$tier)[1])
  if (t %in% c("base", "middle", "leader", "risa")) return(t)
  role <- tolower(as.character(user$role)[1])
  if (role == "risa") return("risa")
  if (role %in% c("admin", "lead")) return("leader")
  if (role == "oversight") return("middle")
  "base"
}

ejo_tier_sees_exec <- function(tier) tier %in% c("leader", "risa")

ejo_sees_all <- function(user) {
  is.list(user) && (identical(user$role, "admin") || identical(user$role, "risa") || identical(ejo_user_tier(user), "risa"))
}

ejo_classify_tier <- function(duties) {
  d <- tolower(paste(as.character(duties), collapse = " "))
  if (grepl("lead", d)) return("leader")
  if (grepl("audit", d)) return("middle")
  "base"
}

ejo_db_get_state <- function(option_id) {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  row <- DBI::dbGetQuery(con, "SELECT * FROM solution_state WHERE option_id = ?", params = list(option_id))
  if (!nrow(row)) return(NULL)
  as.list(row[1, ])
}

ejo_db_save_state <- function(option_id, cause, fund_source, fund_status, framework_step, pm_status, user) {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(con, "
    INSERT INTO solution_state (option_id, cause, fund_source, fund_status, framework_step, pm_status, updated_by, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(option_id) DO UPDATE SET
      cause = excluded.cause,
      fund_source = excluded.fund_source,
      fund_status = excluded.fund_status,
      framework_step = excluded.framework_step,
      pm_status = excluded.pm_status,
      updated_by = excluded.updated_by,
      updated_at = excluded.updated_at
  ", params = list(option_id, cause, fund_source, fund_status, framework_step, pm_status, user, now))
}

ejo_step_milestones <- list(
  "1" = c(align_nst2 = "NST2 / ministry alignment note filed", align_budget = "Budget line or envelope identified"),
  "2" = c(track_chosen = "Legal track chosen (PPP / Umucyo / NCST)", track_memo = "Arrangement memo cleared by lead"),
  "3" = c(vfm = "VFM / fiscal-risk note for MINECOFIN/RDB", tech = "Technical feasibility signed off"),
  "4" = c(rfq = "RFQ / RFP issued", spv = "SPV or supplier shortlist recorded"),
  "5" = c(sla = "SLA / RWF contract terms agreed", signed = "Contract signed by contracting authority"),
  "6" = c(steer = "Steering Committee met", handover = "Handover / monitoring line active")
)

ejo_db_milestones <- function(option_id, step_id) {
  keys <- ejo_step_milestones[[as.character(step_id)]]
  if (is.null(keys)) return(data.frame())
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  have <- DBI::dbGetQuery(
    con,
    "SELECT milestone_key, done, done_by, done_at FROM step_milestones WHERE option_id = ? AND step_id = ?",
    params = list(option_id, as.character(step_id))
  )
  data.frame(
    key = names(keys),
    label = unname(keys),
    done = names(keys) %in% have$milestone_key[have$done == 1],
    stringsAsFactors = FALSE
  )
}

ejo_db_set_milestones <- function(option_id, step_id, done_keys, user) {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  step_id <- as.character(step_id)
  all_keys <- names(ejo_step_milestones[[step_id]])
  if (is.null(all_keys)) return(invisible(FALSE))
  for (k in all_keys) {
    done <- as.integer(k %in% done_keys)
    DBI::dbExecute(con, "
      INSERT INTO step_milestones (option_id, step_id, milestone_key, done, done_by, done_at)
      VALUES (?, ?, ?, ?, ?, ?)
      ON CONFLICT(option_id, step_id, milestone_key) DO UPDATE SET
        done = excluded.done,
        done_by = excluded.done_by,
        done_at = excluded.done_at
    ", params = list(option_id, step_id, k, done, if (done) user else "", if (done) now else ""))
  }
  invisible(TRUE)
}

ejo_db_add_report <- function(rec) {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cols <- c(
    "ref", "filed_at", "option_id", "cause", "filed_by", "institution", "addressed_to",
    "activity", "location", "month_label", "note", "tonnes", "framework_step", "framework_title",
    "fund_source", "fund_status", "pm_status", "milestone", "next_action", "next_owner", "next_when"
  )
  for (c in cols) if (is.null(rec[[c]])) rec[[c]] <- ""
  DBI::dbExecute(con, paste0(
    "INSERT INTO progress_reports (", paste(cols, collapse = ", "), ") VALUES (",
    paste(rep("?", length(cols)), collapse = ", "), ")"
  ), params = unname(as.list(rec[cols])))
  invisible(rec$ref)
}

ejo_db_reports <- function(option_id) {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(
    con,
    "SELECT * FROM progress_reports WHERE option_id = ? ORDER BY id DESC",
    params = list(option_id)
  )
}

ejo_db_timeline <- function(option_id) {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(
    con,
    "SELECT filed_at, framework_step, framework_title, pm_status, filed_by, institution, note, next_action
     FROM progress_reports WHERE option_id = ? ORDER BY id ASC",
    params = list(option_id)
  )
}

ejo_act_statuses <- c(
  not_started = "Not started",
  in_progress = "In progress",
  blocked = "Blocked",
  approved = "Approved",
  done = "Done"
)
ejo_act_steps <- c(
  "1" = "Alignment",
  "2" = "Arrangement",
  "3" = "Feasibility",
  "4" = "Procurement",
  "5" = "Contract",
  "6" = "Co-implementation"
)

ejo_act_key <- function(cause, id) paste(cause, id, sep = "|")

ejo_db_seed_activities <- function(con = NULL) {
  if (!exists("ejo_pm") || !nrow(ejo_pm)) return(invisible(FALSE))
  own <- is.null(con)
  if (own) {
    if (!isTRUE(ejo_db_ok)) return(invisible(FALSE))
    con <- ejo_db_connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  have <- DBI::dbGetQuery(con, "SELECT act_key FROM activity_state")$act_key
  for (i in seq_len(nrow(ejo_pm))) {
    key <- ejo_act_key(ejo_pm$cause[i], ejo_pm$id[i])
    if (key %in% have) next
    owner <- ejo_lead_parts(ejo_pm$lead[i])[1]
    if (!length(owner) || is.na(owner)) owner <- ""
    DBI::dbExecute(con, "
      INSERT INTO activity_state
        (act_key, cause, act_id, status, blocked_who, blocked_what, framework_step, next_owner, next_due, last_note, updated_by, updated_at)
      VALUES (?, ?, ?, 'not_started', '', '', '1', ?, ?, '', '', '')
    ", params = list(key, ejo_pm$cause[i], ejo_pm$id[i], owner, ejo_pm$finish[i]))
  }
  invisible(TRUE)
}

ejo_activity_board <- function(institution = "", admin = FALSE) {
  if (!exists("ejo_pm") || !nrow(ejo_pm)) return(data.frame())
  ejo_db_seed_activities()
  sched <- do.call(rbind, lapply(unique(ejo_pm$cause), function(k) {
    cp <- ejo_cpm(k)
    if (is.null(cp)) return(NULL)
    data.frame(cause = k, id = cp$plan$id, es = cp$plan$es, ef = cp$plan$ef, critical = cp$plan$critical, stringsAsFactors = FALSE)
  }))
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  st <- DBI::dbGetQuery(con, "SELECT * FROM activity_state")
  now_m <- as.numeric(Sys.Date() - as.Date("2026-01-01")) / 30.4375
  board <- ejo_pm
  board$key <- ejo_act_key(board$cause, board$id)
  board$es <- sched$es[match(paste(board$cause, board$id), paste(sched$cause, sched$id))]
  board$ef <- sched$ef[match(paste(board$cause, board$id), paste(sched$cause, sched$id))]
  board$months_left <- round(board$ef - now_m, 1)
  hit <- match(board$key, st$act_key)
  board$status <- ifelse(is.na(hit), "not_started", st$status[hit])
  board$blocked_who <- ifelse(is.na(hit), "", st$blocked_who[hit])
  board$blocked_what <- ifelse(is.na(hit), "", st$blocked_what[hit])
  board$framework_step <- ifelse(is.na(hit), "1", st$framework_step[hit])
  board$next_owner <- ifelse(is.na(hit), "", st$next_owner[hit])
  board$next_due <- ifelse(is.na(hit), "", st$next_due[hit])
  board$last_note <- ifelse(is.na(hit), "", st$last_note[hit])
  board$updated_at <- ifelse(is.na(hit), "", st$updated_at[hit])
  board$status[is.na(board$status) | !nzchar(board$status)] <- "not_started"
  board$framework_step[is.na(board$framework_step) | !nzchar(board$framework_step)] <- "1"
  for (col in c("blocked_who", "blocked_what", "next_owner", "next_due", "last_note", "updated_at")) {
    board[[col]][is.na(board[[col]])] <- ""
  }
  board$overdue <- is.finite(board$months_left) & board$months_left < 0 & !board$status %in% c("done", "approved")
  this_month <- format(Sys.Date(), "%Y-%m")
  board$reported_month <- startsWith(board$updated_at, this_month) & nzchar(board$last_note)
  if (!isTRUE(admin) && nzchar(institution)) {
    keep <- vapply(board$lead, function(ld) tolower(institution) %in% tolower(ejo_lead_parts(ld)), logical(1))
    board <- board[keep, , drop = FALSE]
  }
  board
}

ejo_db_save_activity <- function(key, status, blocked_who, blocked_what, step, next_owner, next_due, last_note, user) {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  parts <- strsplit(key, "|", fixed = TRUE)[[1]]
  cause <- parts[1]
  act_id <- paste(parts[-1], collapse = "|")
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(con, "
    INSERT INTO activity_state
      (act_key, cause, act_id, status, blocked_who, blocked_what, framework_step, next_owner, next_due, last_note, updated_by, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(act_key) DO UPDATE SET
      status = excluded.status,
      blocked_who = excluded.blocked_who,
      blocked_what = excluded.blocked_what,
      framework_step = excluded.framework_step,
      next_owner = excluded.next_owner,
      next_due = excluded.next_due,
      last_note = excluded.last_note,
      updated_by = excluded.updated_by,
      updated_at = excluded.updated_at
  ", params = list(key, cause, act_id, status, blocked_who, blocked_what, step, next_owner, next_due, last_note, user, now))
  invisible(TRUE)
}

ejo_db_solutions_for_inst <- function(institution, admin = FALSE) {
  op <- ejo_options
  if (!isTRUE(admin) && nzchar(institution)) {
    hit <- vapply(op$lead, function(ld) {
      parts <- tolower(ejo_lead_parts(ld))
      tolower(institution) %in% parts || any(grepl(tolower(institution), parts, fixed = TRUE))
    }, logical(1))
    op <- op[hit, , drop = FALSE]
  }
  op
}

ejo_db_directory <- function() {
  if (!isTRUE(ejo_db_ok)) return(data.frame())
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, "SELECT username, institution, role, display_name, tier, COALESCE(age_range, '') AS age_range, COALESCE(id_doc, '') AS id_doc FROM users ORDER BY institution, username")
}

ejo_db_create_user <- function(username, pass, institution, display_name, tier, duties, age_range, id_doc) {
  username <- tolower(trimws(username))
  if (!grepl("^[a-z][a-z0-9_]{2,20}$", username)) stop("Username must be 3–21 letters, starting with a letter.")
  if (grepl("risa", username)) stop("That account name is reserved for RISA.")
  if (!institution %in% setdiff(ejo_portal_institutions, "RISA")) stop("Choose an institution other than RISA.")
  if (nchar(pass) < 6) stop("Use at least 6 characters for the password.")
  if (!nzchar(trimws(id_doc))) stop("Add an identity or passport number.")
  if (!tier %in% c("base", "middle", "leader")) tier <- "base"
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  hit <- DBI::dbGetQuery(con, "SELECT username FROM users WHERE username = ?", params = list(username))
  if (nrow(hit)) stop("That username is already in the directory.")
  DBI::dbExecute(con, "
    INSERT INTO users (username, password_hash, institution, role, display_name, tier, duties, age_range, id_doc)
    VALUES (?, ?, ?, 'partner', ?, ?, ?, ?, ?)
  ", params = list(username, ejo_db_hash(username, pass), institution, display_name, tier, paste(duties, collapse = ","), age_range, trimws(id_doc)))
  list(username = username, institution = institution, role = "partner", display_name = display_name, tier = tier)
}

ejo_db_unread <- function(username) {
  if (!isTRUE(ejo_db_ok)) return(0L)
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM desk_alerts WHERE username = ? AND seen = 0", params = list(username))
  as.integer(n$n[1])
}

ejo_db_alerts_for <- function(username) {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, "SELECT * FROM desk_alerts WHERE username = ? ORDER BY id DESC", params = list(username))
}

ejo_db_mark_alerts <- function(username) {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE desk_alerts SET seen = 1 WHERE username = ?", params = list(username))
}

ejo_db_post_survey <- function(question, target_tier, channel, created_by) {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(con, "
    INSERT INTO desk_surveys (question, target_tier, channel, created_by, created_at)
    VALUES (?, ?, ?, ?, ?)
  ", params = list(question, target_tier, channel, created_by, now))
  id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
  who <- DBI::dbGetQuery(con, "SELECT username FROM users WHERE tier = ?", params = list(target_tier))
  title <- paste("Field question via", channel)
  body <- question
  if (nrow(who)) for (i in seq_len(nrow(who))) {
    DBI::dbExecute(con, "
      INSERT INTO desk_alerts (username, title, body, channel, created_at, seen)
      VALUES (?, ?, ?, ?, ?, 0)
    ", params = list(who$username[i], title, body, channel, now))
  }
  id
}

ejo_db_surveys <- function() {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, "SELECT * FROM desk_surveys ORDER BY id DESC")
}

ejo_db_answers <- function(survey_id = NULL) {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (is.null(survey_id)) DBI::dbGetQuery(con, "SELECT * FROM desk_answers ORDER BY id DESC")
  else DBI::dbGetQuery(con, "SELECT * FROM desk_answers WHERE survey_id = ? ORDER BY id", params = list(as.integer(survey_id)))
}

ejo_db_answer <- function(survey_id, respondent, answer) {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  survey_id <- as.integer(survey_id)
  hit <- DBI::dbGetQuery(con, "SELECT * FROM desk_answers WHERE survey_id = ? AND respondent = ?", params = list(survey_id, respondent))
  if (nrow(hit)) return(as.list(hit[1, ]))
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(con, "
    INSERT INTO desk_answers (survey_id, respondent, answer, filed_at, locked, summary)
    VALUES (?, ?, ?, ?, 1, '')
  ", params = list(survey_id, respondent, answer, now))
  as.list(DBI::dbGetQuery(con, "SELECT * FROM desk_answers WHERE survey_id = ? AND respondent = ?", params = list(survey_id, respondent))[1, ])
}

ejo_db_save_summary <- function(answer_id, summary) {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE desk_answers SET summary = ? WHERE id = ?", params = list(summary, as.integer(answer_id)))
}

ejo_db_messages_for <- function(username, see_all = FALSE) {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (isTRUE(see_all)) DBI::dbGetQuery(con, "SELECT * FROM desk_messages ORDER BY id DESC LIMIT 80")
  else DBI::dbGetQuery(
    con,
    "SELECT * FROM desk_messages WHERE sender = ? OR recipient = ? ORDER BY id DESC LIMIT 80",
    params = list(username, username)
  )
}

ejo_db_send_message <- function(sender, recipient, body) {
  con <- ejo_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(con, "
    INSERT INTO desk_messages (sender, recipient, body, created_at) VALUES (?, ?, ?, ?)
  ", params = list(sender, recipient, body, now))
  DBI::dbExecute(con, "
    INSERT INTO desk_alerts (username, title, body, channel, created_at, seen)
    VALUES (?, 'Message', ?, 'app', ?, 0)
  ", params = list(recipient, paste0(sender, ": ", body), now))
  invisible(TRUE)
}

tryCatch(ejo_db_init(), error = function(e) {
  warning("Ejo PM database could not initialise: ", conditionMessage(e))
  FALSE
})
