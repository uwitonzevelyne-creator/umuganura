# Umuganura
# Click Run App. This file is the app. Do not use a bare runApp()
# on the folder — the folder is named app.R.

# Long interactive sessions (scroll / idle up to ~30 minutes).
options(
  shiny.maxRequestSize = 64 * 1024^2,
  shiny.sanitize.errors = TRUE
)

source("global.R", local = FALSE)
source("interface.R", local = FALSE)
source("analysis.R", local = FALSE)

server_wrapped <- function(input, output, session) {
  try(session$allowReconnect(TRUE), silent = TRUE)
  observeEvent(input$umuganura_keepalive, {
    # Heartbeat from www/js/keepalive.js — keeps websocket warm.
    invisible(TRUE)
  }, ignoreNULL = TRUE)
  server(input, output, session)
}

shinyApp(ui = ui, server = server_wrapped)
