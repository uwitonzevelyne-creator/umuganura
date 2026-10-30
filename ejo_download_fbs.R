# FAOSTAT historic food balance sheets (1961-2013): Rwanda production and losses, long format.
out_dir <- file.path("dataset hachthon", "EXTERNAL", "ejo_hazaza")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out <- file.path(out_dir, "fao_fbs_historic_rwanda.csv")
zip <- file.path(tempdir(), "fbsh.zip")
options(timeout = 900)
utils::download.file("https://bulks-faostat.fao.org/production/FoodBalanceSheetsHistoric_E_Africa.zip", zip, mode = "wb", quiet = TRUE)
files <- utils::unzip(zip, list = TRUE)$Name
main <- files[grepl("FoodBalanceSheetsHistoric_E_Africa(_NOFLAG)?\\.csv$", files)][1]
utils::unzip(zip, files = main, exdir = tempdir())
d <- utils::read.csv(file.path(tempdir(), main), stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "latin1")
d <- d[d$Area == "Rwanda" & d$Element %in% c("Production", "Losses"), , drop = FALSE]
yc <- grep("^Y[0-9]{4}$", names(d), value = TRUE)
long <- do.call(rbind, lapply(yc, function(y) data.frame(Item = d$Item, Element = d$Element, Year = as.integer(sub("Y", "", y)), Unit = d$Unit, Value = d[[y]], stringsAsFactors = FALSE)))
long <- long[is.finite(long$Value), , drop = FALSE]
long$source <- "FAOSTAT Food Balances (old methodology, -2013)"
utils::write.csv(long, out, row.names = FALSE)
cat("rows", nrow(long), "items", length(unique(long$Item)), "years", range(long$Year), "\n")
