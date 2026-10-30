# Copy warmed cache/*.rds into data/prebuilt/ for shinyapps deploy (cache/ is .rscignore'd).
stopifnot(dir.exists("cache"))
dir.create("data/prebuilt", recursive = TRUE, showWarnings = FALSE)
for (f in list.files("cache", pattern = "\\.rds$", full.names = TRUE)) {
  file.copy(f, file.path("data/prebuilt", basename(f)), overwrite = TRUE)
}
message("Updated data/prebuilt from cache/ (", length(list.files("data/prebuilt", pattern = "\\.rds$")), " files).")
