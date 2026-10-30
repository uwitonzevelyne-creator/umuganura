# Deploy-friendly RDS cache: data/prebuilt/ ships with the app; cache/ is local dev only.

prebuilt_dir <- "data/prebuilt"

.rds_cache_paths <- function(name) {
  c(file.path(prebuilt_dir, name), file.path(cache_dir, name))
}

read_rds_cache <- function(name, sig = NULL) {
  for (path in .rds_cache_paths(name)) {
    if (!file.exists(path)) next
    old <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.null(old)) next
    if (is.null(sig) || identical(old$sig, sig)) return(old)
  }
  NULL
}

write_rds_cache <- function(name, payload) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  try(saveRDS(payload, file.path(cache_dir, name), compress = FALSE), silent = TRUE)
}
