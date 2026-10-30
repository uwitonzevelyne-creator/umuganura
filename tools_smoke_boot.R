# Pre-deploy / local smoke: UI must build without crashing (plotly attached, etc.).
# Usage: Rscript tools_smoke_boot.R

options(warn = 1)
fail <- function(...) {
  cat("SMOKE_FAIL:", paste0(...), "\n", file = stderr())
  quit(status = 1)
}

needed <- c("shiny", "ggplot2", "plotly", "jsonlite")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) fail("missing packages: ", paste(missing, collapse = ", "))

tryCatch(
  {
    source("global.R", local = FALSE)
    source("interface.R", local = FALSE)
  },
  error = function(e) fail(conditionMessage(e))
)

if (!exists("plotlyOutput", mode = "function")) fail("plotlyOutput not available after boot")
if (!exists("ui")) fail("ui object missing after interface.R")
if (!isTRUE(has_plotly)) fail("has_plotly is not TRUE")

cat("SMOKE_OK\n")
