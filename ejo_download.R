# Ejo hazaza: fetch the external datasets the loss models use.
# Run from the app.R folder:  Rscript ejo_download.R
# Writes to dataset hachthon/EXTERNAL/ejo_hazaza. Safe to re-run: finished files are skipped.

suppressMessages(source("global.R", local = FALSE))
out_dir <- file.path("dataset hachthon", "EXTERNAL", "ejo_hazaza")
dir.create(file.path(out_dir, "aphlis"), recursive = TRUE, showWarnings = FALSE)
stamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
say <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")

get_json <- function(u, tries = 4) {
  for (i in seq_len(tries)) {
    r <- tryCatch(jsonlite::fromJSON(u, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(r)) return(r)
    Sys.sleep(2 * i)
  }
  NULL
}
get_text <- function(u, tries = 3) {
  for (i in seq_len(tries)) {
    r <- tryCatch(paste(readLines(url(u, encoding = "UTF-8"), warn = FALSE, encoding = "UTF-8"), collapse = "\n"), error = function(e) NULL)
    if (!is.null(r)) return(r)
    Sys.sleep(2 * i)
  }
  NULL
}

# ---- nodes: 30 district centres + 57 NAEB / Esoko markets ----
cen <- do.call(rbind, lapply(names(district_lnglat), function(d) {
  data.frame(node = d, type = "district", district = d, lon = district_lnglat[[d]]$cx, lat = district_lnglat[[d]]$cy)
}))
mk <- utils::read.csv(file.path(naeb_clean_dir, "naeb_market_locations.csv"), stringsAsFactors = FALSE)
mk <- data.frame(node = mk$market, type = "market", district = mk$district, lon = mk$lon, lat = mk$lat)
nodes <- rbind(cen, mk)
nodes$source <- ifelse(nodes$type == "district", "Centroid of the NISR district polygon", "OpenStreetMap Nominatim geocode of the NAEB / Esoko market")
utils::write.csv(nodes, file.path(out_dir, "ejo_nodes.csv"), row.names = FALSE)
say("nodes", nrow(nodes))

# ---- OSRM road distance and drive-time matrix ----
mx_path <- file.path(out_dir, "osrm_road_matrix.csv")
if (!file.exists(mx_path)) {
  coords <- paste(sprintf("%.6f,%.6f", nodes$lon, nodes$lat), collapse = ";")
  r <- get_json(paste0("https://router.project-osrm.org/table/v1/driving/", coords, "?annotations=distance,duration"))
  if (!is.null(r) && identical(r$code, "Ok")) {
    n <- nrow(nodes)
    num <- function(v) if (is.null(v)) NA_real_ else as.numeric(v)
    rows <- lapply(seq_len(n), function(i) data.frame(
      from = nodes$node[i], to = nodes$node,
      km = vapply(r$distances[[i]], num, numeric(1)) / 1000,
      hours = vapply(r$durations[[i]], num, numeric(1)) / 3600
    ))
    m <- do.call(rbind, rows)
    m$source <- "OSRM routing on OpenStreetMap roads (router.project-osrm.org)"
    m$retrieved <- stamp
    utils::write.csv(m, mx_path, row.names = FALSE)
    say("matrix", nrow(m))
  } else say("matrix FAILED")
}

# ---- NASA POWER monthly weather per district ----
pw_path <- file.path(out_dir, "nasa_power_district_monthly.csv")
if (!file.exists(pw_path)) {
  dim_days <- function(y, m) as.integer(format(as.Date(sprintf("%d-%02d-01", y + (m == 12), m %% 12 + 1)) - 1, "%d"))
  pw <- list()
  for (i in seq_len(nrow(cen))) {
    u <- sprintf("https://power.larc.nasa.gov/api/temporal/monthly/point?parameters=PRECTOTCORR,T2M_MAX,RH2M&community=AG&longitude=%.4f&latitude=%.4f&start=2001&end=2024&format=JSON",
                 cen$lon[i], cen$lat[i])
    r <- get_json(u)
    if (is.null(r)) { say("power failed", cen$node[i]); next }
    p <- r$properties$parameter
    k <- names(p$PRECTOTCORR)
    k <- k[substr(k, 5, 6) != "13"]
    y <- as.integer(substr(k, 1, 4)); m <- as.integer(substr(k, 5, 6))
    v <- function(x) { z <- as.numeric(unlist(p[[x]][k])); z[z < -900] <- NA; z }
    pw[[i]] <- data.frame(district = cen$node[i], year = y, month = m,
                          rain_mm = round(v("PRECTOTCORR") * mapply(dim_days, y, m), 1),
                          tmax_c = v("T2M_MAX"), rh_pct = v("RH2M"))
    Sys.sleep(0.4)
  }
  pw <- do.call(rbind, pw)
  pw$source <- "NASA POWER monthly point API (AG community), district centroid"
  pw$retrieved <- stamp
  utils::write.csv(pw, pw_path, row.names = FALSE)
  say("power rows", nrow(pw))
}

# ---- APHLIS post-harvest loss estimates (cereals) ----
ap_path <- file.path(out_dir, "aphlis_rwanda_losses.csv")
if (!file.exists(ap_path)) {
  crops <- c("maize", "rice", "sorghum", "wheat", "millets")
  rows <- list()
  for (cr in crops) for (yr in 2012:2023) {
    for (tb in c("value-chain", "dry-weight-losses")) {
      f <- file.path(out_dir, "aphlis", sprintf("%s_%s_%d.csv", tb, cr, yr))
      if (!file.exists(f)) {
        tx <- get_text(sprintf("https://www.aphlis.net/en/data/tables/%s/RW/%s/%d?metric=prc&download=csv", tb, cr, yr))
        if (is.null(tx) || !nzchar(tx) || grepl("<html", tx, ignore.case = TRUE)) next
        writeLines(tx, f, useBytes = TRUE)
        Sys.sleep(0.4)
      }
      z <- tryCatch(utils::read.csv(f, sep = ";", check.names = FALSE, stringsAsFactors = FALSE, header = TRUE), error = function(e) NULL)
      if (is.null(z) || !nrow(z)) next
      names(z)[1] <- "row"
      long <- do.call(rbind, lapply(names(z)[-1], function(cn) data.frame(table = tb, crop = cr, year = yr, row = z$row, column = cn,
                                                                           value = suppressWarnings(as.numeric(z[[cn]])))))
      rows[[length(rows) + 1]] <- long[is.finite(long$value), , drop = FALSE]
    }
    say("aphlis", cr)
  }
  ap <- do.call(rbind, rows)
  if (!is.null(ap) && nrow(ap)) {
    ap$source <- "APHLIS (African Postharvest Losses Information System), aphlis.net, % dry-weight loss"
    ap$retrieved <- stamp
    utils::write.csv(ap, ap_path, row.names = FALSE)
    say("aphlis rows", nrow(ap))
  }
}

# ---- World Bank diesel pump price ----
wb_path <- file.path(out_dir, "wb_rwanda_diesel_price.csv")
if (!file.exists(wb_path)) {
  r <- get_json("https://api.worldbank.org/v2/country/RWA/indicator/EP.PMP.DESL.CD?format=json&per_page=100")
  if (!is.null(r) && length(r) > 1) {
    d <- do.call(rbind, lapply(r[[2]], function(x) data.frame(year = as.integer(x$date), usd_per_litre = if (is.null(x$value)) NA_real_ else as.numeric(x$value))))
    d <- d[is.finite(d$usd_per_litre), ]
    d$source <- "World Bank WDI EP.PMP.DESL.CD"
    d$retrieved <- stamp
    utils::write.csv(d[order(d$year), ], wb_path, row.names = FALSE)
    say("diesel", nrow(d))
  }
}

# ---- OSRM road geometry: every district to the main markets, and Nyabugogo to every market ----
rt_path <- file.path(out_dir, "osrm_routes.csv")
hubs <- c("Nyabugogo", "Kimironko", "Kicukiro-Center", "Nyamirambo", "Butare", "Muhanga", "Nyanza", "Ruhango",
          "Musanze", "Gisenyi", "Kamembe", "Kibuye", "Rwamagana", "Kibungo", "Nyagatare", "Byumba")
pairs <- rbind(
  expand.grid(from = cen$node, to = hubs, stringsAsFactors = FALSE),
  data.frame(from = "Nyabugogo", to = setdiff(mk$node, "Nyabugogo"), stringsAsFactors = FALSE)
)
done <- if (file.exists(rt_path)) utils::read.csv(rt_path, stringsAsFactors = FALSE) else NULL
key_done <- if (is.null(done)) character() else paste(done$from, done$to)
pairs <- pairs[!paste(pairs$from, pairs$to) %in% key_done, , drop = FALSE]
xy <- function(n) nodes[match(n, nodes$node), c("lon", "lat")]
buf <- list()
flush <- function() {
  if (!length(buf)) return(invisible())
  b <- do.call(rbind, buf)
  utils::write.table(b, rt_path, sep = ",", row.names = FALSE, col.names = !file.exists(rt_path), append = file.exists(rt_path), qmethod = "double")
  buf <<- list()
}
for (i in seq_len(nrow(pairs))) {
  a <- xy(pairs$from[i]); b <- xy(pairs$to[i])
  u <- sprintf("https://router.project-osrm.org/route/v1/driving/%.6f,%.6f;%.6f,%.6f?overview=simplified&geometries=geojson",
               a$lon, a$lat, b$lon, b$lat)
  r <- get_json(u)
  if (!is.null(r) && identical(r$code, "Ok") && length(r$routes)) {
    g <- r$routes[[1]]
    co <- vapply(g$geometry$coordinates, function(p) sprintf("%.5f %.5f", p[[1]], p[[2]]), character(1))
    buf[[length(buf) + 1]] <- data.frame(from = pairs$from[i], to = pairs$to[i], km = g$distance / 1000, hours = g$duration / 3600,
                                         path = paste(co, collapse = "|"), source = "OSRM route on OpenStreetMap roads", retrieved = stamp)
  }
  if (length(buf) >= 25) { flush(); say("routes", i, "/", nrow(pairs)) }
  Sys.sleep(0.25)
}
flush()
say("done")
