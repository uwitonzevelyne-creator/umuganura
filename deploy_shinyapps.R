# Deploy Umuganura to shinyapps.io (account must already be linked via rsconnect::setAccountInfo).
# Runs a boot smoke check first so UI crashes (e.g. missing plotly) never get published.
# Usage from app.R folder:
#   Rscript deploy_shinyapps.R
# Or from repo root against the slim staging folder:
#   Rscript deploy_shinyapps.R path/to/_deploy_umuganura

args <- commandArgs(trailingOnly = TRUE)
app_dir <- if (length(args)) normalizePath(args[[1]], winslash = "/", mustWork = TRUE) else getwd()
setwd(app_dir)

stopifnot(requireNamespace("rsconnect", quietly = TRUE))

message("Smoke check in ", app_dir, " ...")
smoke <- system2(
  file.path(R.home("bin"), "Rscript"),
  args = shQuote(file.path(app_dir, "tools_smoke_boot.R")),
  stdout = TRUE,
  stderr = TRUE
)
cat(paste(smoke, collapse = "\n"), "\n")
if (!any(grepl("^SMOKE_OK$", smoke))) {
  stop("Boot smoke failed — aborting deploy. Fix startup, then retry.")
}

acc <- tryCatch(rsconnect::accounts(), error = function(e) NULL)
if (is.null(acc) || !nrow(acc)) {
  stop(
    "No shinyapps account linked yet.\n",
    "On https://www.shinyapps.io/admin/#/tokens click Show → Show secret → Copy,\n",
    "paste that one line into R, then re-run this script."
  )
}

# Prefer the live Umuganura account; never silently pick another linked login.
want <- "uwitonzeevelyne"
pick <- if (want %in% acc$name) which(acc$name == want)[1] else 1L
acct <- acc$name[pick]
serv <- acc$server[pick]
if (!identical(acct, want)) {
  warning("Account '", want, "' not found; using '", acct, "'. Linked: ", paste(acc$name, collapse = ", "))
}
message("Account: ", acct, " @ ", serv)
message("Deploying appName=umuganura from ", app_dir)
options(rsconnect.http.timeout = 1200, timeout = 1200)

rsconnect::deployApp(
  appDir = app_dir,
  appName = "umuganura",
  appTitle = "Umuganura",
  account = acct,
  server = serv,
  launch.browser = FALSE,
  forceUpdate = TRUE,
  lint = FALSE
)

message("Done. Open: https://", acct, ".shinyapps.io/umuganura/")
