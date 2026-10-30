# Umuganura — data store
# Four drawers: basket (district SAS), money (World Bank / FAO / NAEB),
# gaps (the thin places), weather is held for a later season model.

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(ggplot2))

# plotlyOutput() is used bare in UI modules at source time — never defer this attach.
if (!requireNamespace("plotly", quietly = TRUE)) {
  stop("Package 'plotly' is required to start Umuganura. Install it, then retry.", call. = FALSE)
}
suppressPackageStartupMessages(library(plotly))
has_plotly <- TRUE
has_json <- requireNamespace("jsonlite", quietly = TRUE)

cache_dir <- "cache"
source("startup_cache.R", local = FALSE)

data_dir <- file.path("dataset hachthon", "CLEANED", "master_rwanda_ag")
sas_2026 <- file.path("dataset hachthon", "CLEANED", "nisr_sas_tables", "2026")

read_wb <- function(path) {
  if (!file.exists(path)) {
    return(data.frame(year = integer(), value = numeric(), indicator_name = character()))
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}

pick_series <- function(df, name) {
  if (!nrow(df) || !("indicator_name" %in% names(df))) {
    return(data.frame(year = integer(), value = numeric()))
  }
  out <- df[df$indicator_name == name & !is.na(df$value), c("year", "value")]
  out[order(out$year), , drop = FALSE]
}

wb <- read_wb(file.path(data_dir, "rwanda_world_bank_ag_indicators_1961_2026.csv"))
wb_extra <- read_wb(file.path(data_dir, "rwanda_world_bank_ag_indicators_extra.csv"))

gdp_share <- pick_series(wb, "agriculture_value_added_pct_gdp")
gdp_share <- gdp_share[gdp_share$year >= 1965, , drop = FALSE]
ag_usd <- pick_series(wb_extra, "ag_value_added_const_usd")
empl <- pick_series(wb, "employment_in_agriculture_pct")
fert <- pick_series(wb, "fertilizer_consumption_kg_per_ha")
irrig <- pick_series(wb, "agricultural_irrigated_land_pct")
cereal <- pick_series(wb_extra, "cereal_yield_kg_per_ha")

latest <- function(df) if (nrow(df)) df$value[nrow(df)] else NA_real_
latest_year <- function(df) if (nrow(df)) df$year[nrow(df)] else NA_integer_

fmt_num <- function(x, digits = 0) {
  out <- rep("—", length(x))
  ok <- is.finite(x)
  out[ok] <- format(round(x[ok], digits), big.mark = ",", nsmall = digits, scientific = FALSE, trim = TRUE)
  out
}
fmt_pct <- function(x, digits = 1) {
  out <- rep("—", length(x))
  ok <- is.finite(x)
  out[ok] <- paste0(format(round(x[ok], digits), nsmall = digits), "%")
  out
}

home_stats <- list(
  gdp_now = fmt_pct(latest(gdp_share)),
  gdp_year = latest_year(gdp_share),
  gdp_1965 = fmt_pct(gdp_share$value[gdp_share$year == 1965][1]),
  usd_now = paste0("$", format(round(latest(ag_usd) / 1e9, 1), nsmall = 1), " bn"),
  usd_year = latest_year(ag_usd),
  jobs = fmt_pct(latest(empl), 0),
  jobs_year = latest_year(empl),
  fert = paste0(round(latest(fert)), " kg/ha"),
  fert_year = latest_year(fert),
  irrig = fmt_pct(latest(irrig), 1),
  irrig_year = latest_year(irrig),
  cereal = paste0(format(round(latest(cereal) / 1000, 1), nsmall = 1), " t/ha"),
  cereal_year = latest_year(cereal)
)

read_district_table <- function(path) {
  df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  names(df)[1] <- "district"
  df <- df[!grepl("^Source", df$district) & nzchar(df$district), , drop = FALSE]
  df
}

# Table 11 carries Total developed land, so it is the land table.
# Table 14 is the matching crop table for the same season.
# Ratio = Table 14 / Table 11. Blank where land is 0.
area_alias <- c(
  "Maize" = "Maize",
  "Sorghum" = "Sorghum",
  "Paddy rice" = "Paddy rice",
  "Wheat" = "Wheat",
  "Cassava" = "Cassava",
  "Sweet potato" = "Sweet potato",
  "Irish potato" = "Irish potato",
  "Yams & Taro" = "Yams & Taro",
  "Bananas" = "Banana",
  "Bean" = "Bean",
  "Pea" = "Pea",
  "Ground nuts" = "Groundnuts",
  "Soybean" = "Soybean",
  "vegetables" = "vegetables",
  "Fruits" = "Fruits"
)

area_tbl <- read_district_table(file.path(sas_2026, "Tables_2026 Season A__Table_11.csv"))
prod_tbl <- read_district_table(file.path(sas_2026, "Tables_2026 Season A__Table_14.csv"))

as_num <- function(x) suppressWarnings(as.numeric(x))

district_crop <- do.call(rbind, lapply(names(area_alias), function(crop) {
  land_col <- unname(area_alias[[crop]])
  land <- if (land_col %in% names(area_tbl)) as_num(area_tbl[[land_col]]) else rep(NA_real_, nrow(area_tbl))
  land_df <- data.frame(district = area_tbl$district, land = land, stringsAsFactors = FALSE)
  figure <- if (crop %in% names(prod_tbl)) as_num(prod_tbl[[crop]]) else rep(NA_real_, nrow(prod_tbl))
  fig_df <- data.frame(district = prod_tbl$district, crop = crop, figure = figure, stringsAsFactors = FALSE)
  m <- merge(fig_df, land_df, by = "district", all = TRUE)
  m$crop <- crop
  m$ratio <- ifelse(is.finite(m$land) & m$land > 0 & is.finite(m$figure), m$figure / m$land, NA_real_)
  m
}))
rownames(district_crop) <- NULL

hub_crops <- names(area_alias)
metric_choices <- c(
  "Crop figure (Table 14)" = "figure",
  "Land (Table 11)" = "land",
  "Figure per hectare" = "ratio"
)

rwanda_geo <- NULL
geo_path <- "www/geo/rwanda_districts_lite.geojson"
if (!file.exists(geo_path)) geo_path <- "www/geo/rwanda_districts.geojson"
if (has_json && file.exists(geo_path)) {
  rwanda_geo <- jsonlite::fromJSON(geo_path, simplifyVector = FALSE)
  # plotly draws outer rings as clockwise, the opposite of RFC 7946
  ring_area <- function(ring) {
    x <- vapply(ring, function(q) as.numeric(q[[1]]), numeric(1))
    y <- vapply(ring, function(q) as.numeric(q[[2]]), numeric(1))
    sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y) / 2
  }
  clockwise <- function(poly) {
    lapply(seq_along(poly), function(i) {
      a <- ring_area(poly[[i]])
      if ((i == 1 && a > 0) || (i > 1 && a < 0)) rev(poly[[i]]) else poly[[i]]
    })
  }
  rwanda_geo$features <- lapply(rwanda_geo$features, function(f) {
    g <- f$geometry
    if (identical(g$type, "Polygon")) {
      g$coordinates <- clockwise(g$coordinates)
    } else if (identical(g$type, "MultiPolygon")) {
      keep <- Filter(function(poly) abs(ring_area(poly[[1]])) > 0, g$coordinates)
      g$coordinates <- lapply(keep, clockwise)
    }
    f$geometry <- g
    f
  })
  shape_names <- vapply(rwanda_geo$features, function(f) f$properties$shapeName, character(1))
  district_crop <- district_crop[district_crop$district %in% shape_names, , drop = FALSE]
  rownames(district_crop) <- NULL
}

province_of <- c(
  Nyarugenge = "Kigali", Gasabo = "Kigali", Kicukiro = "Kigali",
  Nyanza = "Southern", Gisagara = "Southern", Nyaruguru = "Southern",
  Huye = "Southern", Nyamagabe = "Southern", Ruhango = "Southern",
  Muhanga = "Southern", Kamonyi = "Southern",
  Karongi = "Western", Rutsiro = "Western", Rubavu = "Western",
  Nyabihu = "Western", Ngororero = "Western", Rusizi = "Western",
  Nyamasheke = "Western",
  Rulindo = "Northern", Gakenke = "Northern", Musanze = "Northern",
  Burera = "Northern", Gicumbi = "Northern",
  Rwamagana = "Eastern", Nyagatare = "Eastern", Gatsibo = "Eastern",
  Kayonza = "Eastern", Kirehe = "Eastern", Ngoma = "Eastern",
  Bugesera = "Eastern"
)
if (nrow(district_crop)) {
  district_crop$province <- unname(province_of[district_crop$district])
}
provinces <- c("All Rwanda", "Kigali", "Southern", "Western", "Northern", "Eastern")

build_svg_map <- function(geo, values) {
  rings_of <- function(geom) {
    coords <- geom$coordinates
    if (is.null(coords) || !length(coords)) return(list())
    if (identical(geom$type, "Polygon")) return(list(coords[[1]]))
    lapply(coords, function(poly) poly[[1]])
  }
  xy_of <- function(ring) {
    cbind(
      vapply(ring, function(p) as.numeric(p[[1]]), numeric(1)),
      vapply(ring, function(p) as.numeric(p[[2]]), numeric(1))
    )
  }
  pieces <- lapply(geo$features, function(f) {
    list(name = f$properties$shapeName, rings = lapply(rings_of(f$geometry), xy_of))
  })
  all_xy <- do.call(rbind, unlist(lapply(pieces, function(p) p$rings), recursive = FALSE))
  minlon <- min(all_xy[, 1]); maxlon <- max(all_xy[, 1])
  minlat <- min(all_xy[, 2]); maxlat <- max(all_xy[, 2])
  minlon <- minlon - (maxlon - minlon) * 0.04
  maxlon <- maxlon + (maxlon - minlon) * 0.04
  minlat <- minlat - (maxlat - minlat) * 0.04
  maxlat <- maxlat + (maxlat - minlat) * 0.04
  width <- 640
  height <- 560
  project <- function(xy) {
    cbind(
      (xy[, 1] - minlon) / (maxlon - minlon) * width,
      (1 - (xy[, 2] - minlat) / (maxlat - minlat)) * height
    )
  }
  vals <- values[is.finite(values)]
  vmin <- if (length(vals)) min(vals) else 0
  vmax <- if (length(vals)) max(vals) else 1
  if (vmax == vmin) vmax <- vmin + 1
  val <- values
  mix <- function(t) {
    t <- pmin(1, pmax(0, t))
    sprintf(
      "#%02X%02X%02X",
      round(232 + (27 - 232) * t),
      round(240 + (67 - 240) * t),
      round(223 + (50 - 223) * t)
    )
  }
  paths <- vapply(pieces, function(p) {
    d <- paste(vapply(p$rings, function(ring) {
      if (!length(ring) || !nrow(ring)) return("")
      xy <- project(ring)
      paste0("M", paste(sprintf("%.1f %.1f", xy[, 1], xy[, 2]), collapse = "L"), "Z")
    }, character(1)), collapse = "")
    v <- unname(val[p$name])
    fill <- if (length(v) == 1 && is.finite(v)) mix((v - vmin) / (vmax - vmin)) else "#f4f6ef"
    label <- if (length(v) == 1 && is.finite(v)) paste0(p$name, ": ", fmt_num(v, 0)) else p$name
    sprintf(
      '<path d="%s" fill="%s" stroke="#ffffff" stroke-width="1.4"><title>%s</title></path>',
      d, fill, label
    )
  }, character(1))
  paste0(
    '<svg class="land-svg" viewBox="0 0 ', width, " ", height,
    '" role="img" aria-label="Rwanda districts">',
    paste(paths, collapse = ""),
    "</svg>"
  )
}

build_home_svg <- function(geo, maize) {
  build_svg_map(geo, stats::setNames(maize$figure, maize$district))
}

# District production tables, chosen because their maize total matches the
# national production line in the same SAS workbook (not the area or yield table).
# 1996–2018 district production is not in this folder. The current 30 districts
# were created in 2006, so a continuous 1996–2026 district total is not published.
home_prod_files <- rbind(
  c("2019", "SAS 2019 _Annual report _Tables__Table_18.csv"),
  c("2019", "SAS 2019 _Annual report _Tables__Table_19.csv"),
  c("2020", "USAS 2020 _Final_Tables__Table_20.csv"),
  c("2020", "USAS 2020 _Final_Tables__Table_21.csv"),
  c("2021", "SAS 2021_Tables__Table_24.csv"),
  c("2021", "SAS 2021_Tables__Table_25.csv"),
  c("2022", "SAS 2022_Tables_0__Table_23.csv"),
  c("2022", "SAS 2022_Tables_0__Table_24.csv"),
  c("2023", "SAS 2023_Annual Tables (3)__Table_22.csv"),
  c("2023", "SAS 2023_Annual Tables (3)__Table_23.csv"),
  c("2024", "SAS 2024_Annual Tables__Table_21.csv"),
  c("2024", "SAS 2024_Annual Tables__Table_22.csv"),
  c("2025", "SAS 2025_Annual Tables (1)__Table_24.csv"),
  c("2025", "SAS 2025_Annual Tables (1)__Table_25.csv"),
  c("2026", "Tables_2026 Season A__Table_15.csv")
)

sum_prod_table <- function(path) {
  if (!file.exists(path)) return(NULL)
  df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!ncol(df)) return(NULL)
  names(df)[1] <- "district"
  key <- tolower(trimws(names(df)))
  parent <- c(
    "maize", "sorghum", "paddy rice", "wheat", "cassava",
    "sweet potato", "sweet potatoes", "irish potato", "irish potatoes",
    "yams & taro", "yams and taro", "yarms & taro",
    "banana", "bananas", "bean", "beans", "pea", "peas",
    "ground nuts", "groundnuts", "groundnut", "soybean", "soya beans",
    "vegetables", "fruits"
  )
  banana_child <- c("cooking banana", "dessert banana", "banana for beer")
  bean_child <- c("bush bean", "small red bean", "climbing bean")
  keep <- function(names_ok) which(key %in% names_ok & !grepl("_", key, fixed = TRUE))
  use <- keep(parent)
  if (!any(key %in% c("banana", "bananas"))) use <- c(use, keep(banana_child))
  if (!any(key %in% c("bean", "beans"))) use <- c(use, keep(bean_child))
  use <- unique(use)
  if (!length(use)) return(NULL)
  nums <- lapply(df[use], function(col) {
    x <- gsub(",", "", as.character(col), fixed = TRUE)
    x[x %in% c("", "-", "NA")] <- NA
    suppressWarnings(as.numeric(x))
  })
  fig <- rowSums(as.data.frame(nums, stringsAsFactors = FALSE), na.rm = TRUE)
  out <- data.frame(district = df$district, figure = fig, stringsAsFactors = FALSE)
  out[out$district %in% names(province_of) & is.finite(out$figure), , drop = FALSE]
}

home_district_production <- function() {
  root <- file.path("dataset hachthon", "CLEANED", "nisr_sas_tables")
  parts <- lapply(seq_len(nrow(home_prod_files)), function(i) {
    sum_prod_table(file.path(root, home_prod_files[i, 1], home_prod_files[i, 2]))
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) return(NULL)
  all <- do.call(rbind, parts)
  agg <- aggregate(figure ~ district, all, sum)
  agg[order(-agg$figure), , drop = FALSE]
}

home_maize <- list(
  top = "—", top_fig = "—", top_share = "—",
  low = "—", low_fig = "—", low_share = "—",
  median = "—", cv = "—", n = "—", sum = "—",
  span = "2019–2026"
)
home_map_svg <- ""
home_district_agg <- tryCatch(home_district_production(), error = function(e) NULL)
if (!is.null(rwanda_geo)) {
  produced <- home_district_agg
  if (!is.null(produced) && nrow(produced)) {
    total <- sum(produced$figure)
    last <- produced[nrow(produced), ]
    home_maize <- list(
      top = produced$district[1],
      top_fig = fmt_num(produced$figure[1], 0),
      top_share = fmt_pct(100 * produced$figure[1] / total, 1),
      low = last$district,
      low_fig = fmt_num(last$figure, 0),
      low_share = fmt_pct(100 * last$figure / total, 1),
      median = fmt_num(stats::median(produced$figure), 0),
      cv = fmt_pct(100 * stats::sd(produced$figure) / mean(produced$figure), 0),
      n = as.character(nrow(produced)),
      sum = fmt_num(total, 0),
      span = "2019–2026"
    )
    home_map_svg <- build_svg_map(rwanda_geo, stats::setNames(produced$figure, produced$district))
  }
}

.geo_df_store <- new.env(parent = emptyenv())
get_geo_df <- function() {
  if (exists("df", .geo_df_store, inherits = FALSE)) {
    return(get("df", .geo_df_store, inherits = FALSE))
  }
  geo_df <- data.frame(lon = numeric(), lat = numeric(), district = character(), piece = character(), stringsAsFactors = FALSE)
  if (!is.null(rwanda_geo)) {
    geo_parts <- list()
    for (f in rwanda_geo$features) {
      g <- f$geometry
      rings <- if (identical(g$type, "Polygon")) list(g$coordinates[[1]]) else lapply(g$coordinates, function(p) p[[1]])
      nm <- f$properties$shapeName
      for (r in seq_along(rings)) {
        ring <- rings[[r]]
        if (!length(ring)) next
        geo_parts[[length(geo_parts) + 1]] <- data.frame(
          lon = vapply(ring, function(p) as.numeric(p[[1]]), numeric(1)),
          lat = vapply(ring, function(p) as.numeric(p[[2]]), numeric(1)),
          district = nm, piece = paste(nm, r),
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(geo_parts)) geo_df <- do.call(rbind, geo_parts)
  }
  assign("df", geo_df, .geo_df_store)
  geo_df
}

fao_prod <- data.frame(item = character(), year = integer(), value = numeric())
fao_path <- file.path(data_dir, "fao_rwanda_crops_livestock_production.csv")
if (file.exists(fao_path)) {
  fao <- utils::read.csv(fao_path, stringsAsFactors = FALSE)
  keep <- fao$Element == "Production" & !is.na(fao$Value)
  fao_prod <- data.frame(
    item = fao$Item[keep],
    year = fao$Year[keep],
    value = fao$Value[keep],
    stringsAsFactors = FALSE
  )
}
fao_items <- sort(intersect(unique(fao_prod$item), c(
  "Maize (corn)", "Sorghum", "Rice", "Bananas", "Avocados", "Tomatoes",
  "Pineapples", "Millet", "Sugar cane", "Cassava, fresh", "Potatoes",
  "Sweet potatoes", "Beans, dry", "Cabbages"
)))

# FAO producer prices are monthly. The annual point is the mean of the
# published months in that year. A year with no month stays absent.
# No CPI or GDP deflator is in the World Bank extract, so no real-price line.
crop_prices <- data.frame(item = character(), year = integer(), value = numeric())
price_path <- file.path(data_dir, "fao_rwanda_producer_prices.csv")
if (file.exists(price_path)) {
  pp <- utils::read.csv(price_path, stringsAsFactors = FALSE, check.names = FALSE)
  pp <- pp[pp$Element == "Producer Price (LCU/tonne)" & !is.na(pp$Value), c("Item", "Year", "Value")]
  if (nrow(pp)) {
    key <- paste(pp$Item, pp$Year, sep = "\r")
    crop_prices <- aggregate(Value ~ Item + Year, data = pp, FUN = mean)
    names(crop_prices) <- c("item", "year", "value")
    crop_prices <- crop_prices[order(crop_prices$item, crop_prices$year), , drop = FALSE]
  }
}

# Share of FAO gross production value (constant 2014-2016 thousand I$).
# This is not a share of agricultural GDP.
crop_value <- data.frame(item = character(), year = integer(), value = numeric(), share = numeric())
value_path <- file.path(data_dir, "fao_rwanda_value_of_production.csv")
if (file.exists(value_path)) {
  qv <- utils::read.csv(value_path, stringsAsFactors = FALSE, check.names = FALSE)
  qv <- qv[qv$Element == "Gross Production Value (constant 2014-2016 thousand I$)" & !is.na(qv$Value),
           c("Item", "Year", "Value")]
  if (nrow(qv)) {
    totals <- aggregate(Value ~ Year, data = qv, FUN = sum)
    names(totals)[2] <- "total"
    qv <- merge(qv, totals, by = "Year")
    crop_value <- data.frame(
      item = qv$Item,
      year = qv$Year,
      value = qv$Value,
      share = ifelse(qv$total > 0, qv$Value / qv$total, NA_real_),
      stringsAsFactors = FALSE
    )
  }
}

plant_item <- function(x) {
  !grepl("Meat of|Raw milk|Hen eggs|honey|Game meat|Beeswax", x)
}
price_crops <- sort(unique(c(
  crop_prices$item[plant_item(crop_prices$item)],
  crop_value$item[plant_item(crop_value$item)]
)))
if (!length(price_crops)) price_crops <- "Maize (corn)"

fit_line <- function(df) {
  df <- df[is.finite(df$year) & is.finite(df$value), , drop = FALSE]
  if (nrow(df) < 6) return(NULL)
  fit <- stats::lm(value ~ year, data = df)
  nxt <- max(df$year) + 1L
  pred <- stats::predict(fit, newdata = data.frame(year = nxt), interval = "confidence", level = 0.95)
  sm <- summary(fit)
  list(
    df = df,
    fit = fit,
    next_year = nxt,
    point = as.numeric(pred[1, "fit"]),
    lo = as.numeric(pred[1, "lwr"]),
    hi = as.numeric(pred[1, "upr"]),
    r2 = unname(sm$r.squared),
    n = nrow(df),
    sigma = sm$sigma,
    slope = unname(stats::coef(fit)[["year"]])
  )
}

catalogue <- data.frame(
  drawer = c("Basket", "Basket", "Money", "Money", "Money", "Money", "Gaps", "Gaps", "Next season", "Prices", "Prices"),
  title = c(
    "SAS 2026 Season A land",
    "SAS 2026 Season A crop figure",
    "Agriculture share of GDP",
    "Agriculture value added",
    "Employment in agriculture",
    "NAEB export cash",
    "Irrigated land",
    "Fertilizer on cropland",
    "FAO crop production",
    "FAO producer price (LCU/tonne)",
    "FAO gross production value"
  ),
  source = c(
    "NISR SAS Table 11",
    "NISR SAS Table 14",
    "World Bank",
    "World Bank",
    "World Bank",
    "NAEB 2023/24",
    "World Bank",
    "World Bank",
    "FAOSTAT",
    "FAOSTAT producer prices",
    "FAOSTAT value of production"
  ),
  years = c("2026 A", "2026 A", "1965–2025", "latest series", "1991–2025", "2023/24",
            home_stats$irrig_year, home_stats$fert_year, "national series",
            "annual mean of published months", "constant 2014–2016 I$"),
  grain = c("District", "District", "National", "National", "National", "National",
            "National", "National", "National", "National", "National"),
  quality = c("Published", "Published", "Empty 1961–64", "Published", "Published",
              "National only", "Last year 2005", "Published", "No coffee/tea rows",
              "No CPI in the World Bank extract", "Not a GDP share"),
  room = c("Harvest Hub", "Harvest Hub", "Value", "Value", "Value", "Value",
           "Gaps", "Gaps", "Next season", "Prices", "Prices"),
  stringsAsFactors = FALSE
)

stack_series <- function(id, df) {
  if (is.null(df) || !nrow(df)) {
    return(data.frame(id = character(), year = integer(), value = numeric()))
  }
  out <- data.frame(id = id, year = as.integer(df$year), value = as.numeric(df$value), stringsAsFactors = FALSE)
  out[is.finite(out$year) & is.finite(out$value), , drop = FALSE]
}

macro_factors <- data.frame(code = character(), year = integer(), value = numeric())
macro_path <- file.path(data_dir, "factors", "rwanda_macro_factors_national.csv")
if (file.exists(macro_path)) {
  mf <- utils::read.csv(macro_path, stringsAsFactors = FALSE, check.names = FALSE)
  mf$value <- suppressWarnings(as.numeric(mf$value))
  macro_factors <- data.frame(code = mf$indicator_code, year = as.integer(mf$year), value = mf$value, stringsAsFactors = FALSE)
  macro_factors <- macro_factors[is.finite(macro_factors$year) & is.finite(macro_factors$value), , drop = FALSE]
}

pull_code <- function(code) {
  d <- macro_factors[macro_factors$code == code, c("year", "value"), drop = FALSE]
  d[order(d$year), , drop = FALSE]
}
item_price <- function(item) {
  d <- crop_prices[crop_prices$item == item, c("year", "value"), drop = FALSE]
  d[order(d$year), , drop = FALSE]
}
item_prod <- function(item) {
  d <- fao_prod[fao_prod$item == item, c("year", "value"), drop = FALSE]
  d[order(d$year), , drop = FALSE]
}

maize_lcu <- item_price("Maize (corn)")
fx_rwf <- pull_code("PA.NUS.FCRF")
maize_usd <- merge(maize_lcu, fx_rwf, by = "year", suffixes = c("_lcu", "_fx"))
maize_usd <- maize_usd[is.finite(maize_usd$value_lcu) & is.finite(maize_usd$value_fx) & maize_usd$value_fx > 0, , drop = FALSE]
maize_usd <- data.frame(year = maize_usd$year, value = maize_usd$value_lcu / maize_usd$value_fx, stringsAsFactors = FALSE)

read_fao_stat <- function(path, element, item, unit = NULL) {
  empty <- data.frame(year = integer(), value = numeric())
  if (!file.exists(path)) return(empty)
  x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c("Element", "Item", "Year", "Value") %in% names(x))) return(empty)
  x$Value <- suppressWarnings(as.numeric(x$Value))
  keep <- x$Element == element & x$Item == item
  if (!is.null(unit) && "Unit" %in% names(x)) keep <- keep & x$Unit == unit
  d <- data.frame(year = as.integer(x$Year[keep]), value = x$Value[keep], stringsAsFactors = FALSE)
  d[is.finite(d$year) & is.finite(d$value), , drop = FALSE]
}

fao_credit_path <- file.path("dataset hachthon", "FAO", "INVESTMENT", "Credit to Agriculture", "FAOSTAT_data_en_9-18-2026.csv")
fao_spend_path <- file.path("dataset hachthon", "FAO", "INVESTMENT", "GOVERNMENT EXPENDITURE", "FAOSTAT_data_en_9-18-2026.csv")
ag_credit <- read_fao_stat(fao_credit_path, "Value US$, 2015 prices", "Credit to Agriculture, Forestry and Fishing")
ag_credit_share <- read_fao_stat(fao_credit_path, "Share of Total Credit US$, 2015 prices", "Credit to Agriculture, Forestry and Fishing")
ag_spend <- read_fao_stat(fao_spend_path, "Value US$, 2015 prices", "Agriculture, forestry, fishing (general government expenditure)")
ag_spend_share <- read_fao_stat(fao_spend_path, "Share of Total Expenditure", "Agriculture, forestry, fishing (general government expenditure)")

naeb_export_path <- file.path("dataset hachthon", "RWANDA_INSTITUTIONS", "NAEB", "cleaned", "naeb_export_performance_panel.csv")
if (!file.exists(naeb_export_path)) naeb_export_path <- file.path("dataset hachthon", "CLEANED", "rwanda_institutions", "naeb_export_performance_panel.csv")
naeb_export_year <- function(commodity) {
  empty <- data.frame(year = integer(), value = numeric())
  if (!file.exists(naeb_export_path)) return(empty)
  x <- utils::read.csv(naeb_export_path, stringsAsFactors = FALSE)
  x <- x[x$commodity == commodity & x$metric == "export_usd", , drop = FALSE]
  if (!nrow(x)) return(empty)
  yr <- suppressWarnings(as.integer(sub(".*/", "", x$fiscal_year)))
  data.frame(year = yr, value = as.numeric(x$value), stringsAsFactors = FALSE)
}

factor_data <- rbind(
  stack_series("inflation", pull_code("FP.CPI.TOTL.ZG")),
  stack_series("cpi", pull_code("FP.CPI.TOTL")),
  stack_series("lending", pull_code("FR.INR.LEND")),
  stack_series("real_rate", pull_code("FR.INR.RINR")),
  stack_series("unemployment", pull_code("SL.UEM.TOTL.ZS")),
  stack_series("youth", pull_code("SL.UEM.1524.ZS")),
  stack_series("remittances", pull_code("BX.TRF.PWKR.CD.DT")),
  stack_series("debt", pull_code("DT.DOD.DECT.CD")),
  stack_series("gdp_growth", pull_code("NY.GDP.MKTP.KD.ZG")),
  stack_series("fx", fx_rwf),
  stack_series("gdp_share", gdp_share),
  stack_series("ag_usd", ag_usd),
  stack_series("farm_jobs", empl),
  stack_series("fert_use", fert),
  stack_series("maize_price", maize_lcu),
  stack_series("sorghum_price", item_price("Sorghum")),
  stack_series("beans_price", item_price("Beans, dry")),
  stack_series("cassava_price", item_price("Cassava, fresh")),
  stack_series("potato_price", item_price("Potatoes")),
  stack_series("rice_price", item_price("Rice")),
  stack_series("milk_price", item_price("Raw milk of cattle")),
  stack_series("meat_price", item_price("Meat of cattle with the bone, fresh or chilled")),
  stack_series("maize_prod", item_prod("Maize (corn)")),
  stack_series("sorghum_prod", item_prod("Sorghum")),
  stack_series("milk_prod", item_prod("Raw milk of cattle")),
  stack_series("maize_usd", maize_usd),
  stack_series("ag_credit", ag_credit),
  stack_series("ag_credit_share", ag_credit_share),
  stack_series("ag_spend", ag_spend),
  stack_series("ag_spend_share", ag_spend_share),
  stack_series("naeb_all", naeb_export_year("All_agri_exports")),
  stack_series("naeb_coffee", naeb_export_year("Coffee")),
  stack_series("naeb_tea", naeb_export_year("Tea")),
  stack_series("naeb_hort", naeb_export_year("Horticulture"))
)

series_label <- c(
  inflation = "Inflation",
  cpi = "Consumer price index",
  lending = "Lending rate",
  real_rate = "Real interest rate",
  unemployment = "Unemployment",
  youth = "Youth unemployment",
  remittances = "Remittances",
  debt = "External debt",
  gdp_growth = "GDP growth",
  fx = "Exchange rate",
  gdp_share = "Agriculture share of GDP",
  ag_usd = "Agriculture value added",
  farm_jobs = "Jobs in agriculture",
  fert_use = "Fertilizer use",
  maize_price = "Maize",
  sorghum_price = "Sorghum",
  beans_price = "Beans",
  cassava_price = "Cassava",
  potato_price = "Potatoes",
  rice_price = "Rice",
  milk_price = "Raw milk",
  meat_price = "Cattle meat",
  maize_prod = "Maize production",
  sorghum_prod = "Sorghum production",
  milk_prod = "Milk production",
  maize_usd = "Maize in USD",
  ag_credit = "Credit to agriculture",
  ag_credit_share = "Agriculture share of credit",
  ag_spend = "Government spend on agriculture",
  ag_spend_share = "Agriculture share of spending",
  naeb_all = "All agri exports",
  naeb_coffee = "Coffee exports",
  naeb_tea = "Tea exports",
  naeb_hort = "Horticulture exports"
)

factor_items <- data.frame(
  id = c(
    "inflation-overview", "inflation-materials", "inflation-labor", "inflation-energy", "inflation-capital", "inflation-food", "inflation-farm", "inflation-basket",
    "interest-overview", "interest-season", "interest-machines", "interest-coop", "interest-land", "interest-mfi",
    "jobs-overview", "jobs-labour", "jobs-remit", "jobs-youth", "jobs-offfarm", "jobs-subsistence",
    "gdp-overview", "gdp-diet", "gdp-feed", "gdp-roads", "gdp-mills", "gdp-cold",
    "fx-overview", "fx-seed", "fx-export", "fx-import", "fx-pack", "fx-debt"
  ),
  force = c(rep("inflation", 8), rep(c("interest", "jobs", "gdp", "fx"), each = 6)),
  force_label = c(rep("Inflation", 8), rep(c("Interest", "Unemployment", "GDP", "Exchange"), each = 6)),
  label = c(
    "Overview", "Raw materials", "Labor costs", "Energy and utilities", "Capital and infrastructure", "Food prices", "Farm prices", "The household basket",
    "Overview", "Seasonal credit", "Machinery loans", "Cooperative debt", "Land as collateral", "Microfinance",
    "Overview", "Farm labour", "Remittances", "Youth", "Off-farm income", "Subsistence",
    "Overview", "Diets", "Animal feed", "Rural works", "Processing", "Cold chain",
    "Overview", "Imported inputs", "Export price", "Cheap imports", "Packaging", "Dollar debt"
  ),
  story = c(
    "How fast consumer prices rise, and what that does to a farmer's costs.",
    "Seeds, fertilizer, and pesticides. No import-price file was published, so this chart is fertilizer use per hectare.",
    "Wages and benefits for farm work. No wage-rate series was published.",
    "Electricity and diesel for tillage, milling, and chilling. No tariff or pump-price series was published.",
    "Rent, machinery, and the cost of borrowed money. The chart is the bank lending rate, not an equipment quote.",
    "Staple producer prices, annual mean of the published months. Not a food CPI.",
    "Livestock product prices: raw milk and cattle meat, francs per tonne.",
    "Food, housing, transport, health, apparel, and the rest of what a household buys. Only headline CPI was published.",
    "The price of borrowed money. This is the bank lending rate, not a farm-loan quote.",
    "Season A seed and fertilizer are bought months before the sale. No farm-credit rate was published.",
    "Long loans for mills and churns. The chart is the national real interest rate, not an equipment quote.",
    "Cooperatives borrow for stores and chilling tanks. No cooperative-debt series was published.",
    "Land is the collateral. No rural land-price series was published.",
    "Small sorghum loans often come from a microfinance institution. No MFI rate was published.",
    "The share of the labour force without work. This is the ILO model, not the NISR labour-force table.",
    "A slack rural labour market changes the daily wage. No district wage series was published.",
    "Money sent home buys seed. This is national remittances in dollars, not a district transfer.",
    "Youth without work. ILO ages 15–24, not the NISR youth underemployment rate.",
    "A second job cushions a failed season. The chart is the share of jobs in agriculture, not off-farm pay.",
    "Joblessness pushes people back onto small plots. No subsistence-area series was published.",
    "How fast the whole economy grows. Agriculture's own share sits on a later card.",
    "Richer households buy more milk. The chart is raw-milk tonnes, not butter sales.",
    "More meat means more feed grain. The chart is maize tonnes, not feed-mill offtake.",
    "Growth funds roads and power. The chart is agriculture value added, not a road inventory.",
    "Growth draws mills and dairies. The chart is sorghum tonnes, not a factory census.",
    "Supermarket cold rooms. No chilling-centre series was published.",
    "Rwandan francs per US dollar, period average. One national rate.",
    "A weaker franc raises the franc cost of imported seed and fertilizer. The chart is fertilizer use, not a price.",
    "Maize producer price divided by the official rate. Dollars per tonne, not an export contract.",
    "A stronger franc makes foreign grain cheaper. The chart is the sorghum producer price in francs.",
    "Foil and plastic for packed butter. No packaging-price series was published.",
    "Country external debt in dollars. Not a factory loan."
  ),
  series = c(
    "inflation", "fert_use", "", "", "lending", "maize_price", "milk_price", "cpi",
    "lending", "", "real_rate", "", "", "",
    "unemployment", "", "remittances", "youth", "farm_jobs", "",
    "gdp_growth", "milk_prod", "maize_prod", "ag_usd", "sorghum_prod", "",
    "fx", "fert_use", "maize_usd", "sorghum_price", "", "debt"
  ),
  peers = c(
    "", "", "", "", "",
    "maize_price,sorghum_price,beans_price,cassava_price,potato_price,rice_price",
    "milk_price,meat_price",
    "",
    "", "", "", "", "", "",
    "", "", "", "", "", "",
    "", "", "", "", "", "",
    "", "", "", "", "", ""
  ),
  unit = c(
    "% per year", "kg/ha", "", "", "%", "RWF/tonne", "RWF/tonne", "index, 2010 = 100",
    "%", "", "%", "", "", "",
    "% of labour force", "", "current US$", "% of labour force 15–24", "% of employment", "",
    "% per year", "tonnes", "tonnes", "constant US$", "tonnes", "",
    "RWF per USD", "kg/ha", "USD/tonne", "RWF/tonne", "", "current US$"
  ),
  digits = c(
    1, 1, 0, 0, 1, 0, 0, 1,
    1, 0, 1, 0, 0, 0,
    1, 0, 0, 1, 1, 0,
    1, 0, 0, 0, 0, 0,
    1, 1, 1, 0, 0, 0
  ),
  source = c(
    "World Bank FP.CPI.TOTL.ZG", "World Bank fertilizer kg/ha", "", "", "World Bank FR.INR.LEND",
    "FAOSTAT producer prices: maize, sorghum, beans, cassava, potatoes, rice",
    "FAOSTAT producer prices: raw milk of cattle, cattle meat",
    "World Bank FP.CPI.TOTL",
    "World Bank FR.INR.LEND", "", "World Bank FR.INR.RINR", "", "", "",
    "World Bank SL.UEM.TOTL.ZS, ILO model", "", "World Bank BX.TRF.PWKR.CD.DT", "World Bank SL.UEM.1524.ZS, ILO model", "World Bank employment in agriculture", "",
    "World Bank NY.GDP.MKTP.KD.ZG", "FAOSTAT raw milk production", "FAOSTAT maize production", "World Bank agriculture value added", "FAOSTAT sorghum production", "",
    "World Bank PA.NUS.FCRF", "World Bank fertilizer kg/ha", "FAO maize price ÷ official exchange rate", "FAOSTAT producer price, sorghum", "", "World Bank DT.DOD.DECT.CD"
  ),
  caveat = c(
    "National headline inflation. Not a district CPI and not the food basket.",
    "Nutrient use per hectare. Not a seed, fertilizer, or pesticide price.",
    "No daily wage or benefits series. SAS wage_ratio is a harvest share, not a wage.",
    "Diesel and electricity prices were not in the World Bank API.",
    "Bank lending rate. Not rent, not a machinery quote, and not a farm-loan rate.",
    "Annual mean of published months, about 1991 onward. Not a food CPI, and not a 1960 start.",
    "Producer prices, not farm wages and not a supermarket shelf price.",
    "Headline CPI. Food, housing, transport, health, and apparel weights were not in the folder.",
    "Bank lending rate. Not a seasonal farm loan and not an MFI rate.",
    "No farm-credit rate from the National Bank of Rwanda.",
    "National real interest rate. Not a machinery-loan quote.",
    "No cooperative debt series.",
    "No rural land-price series.",
    "No microfinance rate.",
    "ILO modelled unemployment. Not the NISR Labour Force Survey district table.",
    "No district farm-wage series.",
    "National remittances. Not a transfer into Nyagatare or any other district.",
    "ILO youth unemployment, ages 15–24. Not NISR underemployment.",
    "Share of jobs in agriculture. Not off-farm income.",
    "No series for a return to subsistence plots.",
    "National GDP growth. Not agricultural GDP and not a district product.",
    "Raw-milk tonnes. Not butter demand and not a household diet survey.",
    "Maize tonnes. Not tonnes entering feed mills.",
    "Agriculture value added, constant US$. Not kilometres of feeder road.",
    "Sorghum tonnes. Not a count of mills or dairies.",
    "No chilling-centre or supermarket cold-chain series.",
    "One official rate for the country. Districts do not have their own rate.",
    "Fertilizer use in kg/ha. Not the import price of seed or fertilizer.",
    "Published maize price converted at the official rate. Not a border contract.",
    "Sorghum producer price in francs. Not a count of imported tonnes.",
    "No packaging-price series.",
    "Country external debt stock. Not dollar debt of a dairy or a mill."
  ),
  stringsAsFactors = FALSE
)

factor_items <- factor_items[!factor_items$id %in% c("inflation-labor", "inflation-energy", "inflation-capital", "inflation-basket"), , drop = FALSE]
rownames(factor_items) <- NULL
factor_items$id[factor_items$id == "inflation-materials"] <- "inflation-input"
factor_items$id[factor_items$id == "inflation-farm"] <- "inflation-livestock"
factor_items$label[factor_items$id == "inflation-input"] <- "Inputs"
factor_items$label[factor_items$id == "inflation-food"] <- "Food prices"
factor_items$label[factor_items$id == "inflation-livestock"] <- "Livestock (Animal Products)"
factor_items$story[factor_items$id == "inflation-overview"] <- "A plain reading of farm inputs, food prices, and livestock products."
factor_items$story[factor_items$id == "inflation-input"] <- "Fertilizers, seeds, and pesticides. District use from NISR SAS 2026 Season A and national use from FAOSTAT, then DAP, NPK, Urea, and Dithane shop prices from NAEB / Esoko markets for inflation."
factor_items$story[factor_items$id == "inflation-food"] <- "Cereals and tubers, oil and fats, pulses and nuts, vegetables and fruits, and miscellaneous food. NAEB / Esoko market prices by market location and district."
factor_items$story[factor_items$id == "inflation-livestock"] <- "Milk and dairy, meat, fish and eggs, hides and skins, live animals, honey, and animal feed. NAEB / Esoko market prices by market location and district."

factor_items <- factor_items[factor_items$force == "inflation", , drop = FALSE]
force_add <- data.frame(
  id = c(
    "interest-overview", "interest-credit", "interest-rates", "interest-spend",
    "jobs-overview", "jobs-farm", "jobs-youth", "jobs-remit",
    "gdp-overview", "gdp-growth", "gdp-district", "gdp-food",
    "fx-overview", "fx-rate", "fx-export", "fx-debt"
  ),
  force = c(rep("interest", 4), rep("jobs", 4), rep("gdp", 4), rep("fx", 4)),
  force_label = c(rep("Interest", 4), rep("Unemployment", 4), rep("GDP", 4), rep("Exchange", 4)),
  label = c(
    "Overview", "Credit to farms", "Bank rates", "Public spend",
    "Overview", "Farm jobs", "Youth and joblessness", "Remittances",
    "Overview", "Growth and value", "District harvest", "Food production",
    "Overview", "The franc", "Export cash", "Dollar debt"
  ),
  story = c(
    "What borrowed money costs a farm, how much of all credit goes to agriculture, and what the government spends on it.",
    "Credit to agriculture, forestry and fishing, and its share of all credit. FAOSTAT, constant 2015 US$. Not a seasonal farm-loan rate.",
    "The bank lending rate and the real interest rate. World Bank. Not a cooperative or microfinance quote.",
    "General government spending on agriculture, forestry and fishing, and its share of all spending. FAOSTAT, constant 2015 US$.",
    "Who is without work, who still works on the land, and the money sent home that can buy seed.",
    "Share of jobs in agriculture. World Bank. Not a daily wage and not a district labour-force table.",
    "ILO modelled unemployment for all ages and for youth 15–24. Not the NISR Labour Force Survey.",
    "Personal remittances received, current US$. National, not a transfer into any one district.",
    "How fast the economy grows, what agriculture is worth, and where this season's harvest sits.",
    "National GDP growth, agriculture's share of GDP, and agriculture value added. World Bank.",
    "NISR SAS 2026 Season A crop figure by district. This is the only district map in this room. Not a district GDP.",
    "FAOSTAT tonnes of maize, sorghum, and raw milk. Not feed-mill offtake and not a diet survey.",
    "The franc against the dollar, what export crops fetched, and the country's dollar debt.",
    "Official exchange rate, Rwandan francs per US dollar, period average. One national rate.",
    "NAEB export cash for coffee, tea, horticulture, and all agri exports, plus the maize producer price in dollars.",
    "Country external debt stock, current US$. Not the dollar loan of a dairy or a mill."
  ),
  series = c(
    "lending", "ag_credit", "lending", "ag_spend",
    "unemployment", "farm_jobs", "unemployment", "remittances",
    "gdp_growth", "gdp_growth", "", "maize_prod",
    "fx", "fx", "naeb_all", "debt"
  ),
  peers = c(
    "ag_credit_share,ag_spend_share,real_rate",
    "ag_credit_share",
    "real_rate",
    "ag_spend_share",
    "youth,farm_jobs",
    "",
    "youth",
    "",
    "gdp_share,ag_usd",
    "gdp_share,ag_usd",
    "",
    "sorghum_prod,milk_prod",
    "maize_usd,debt",
    "",
    "naeb_coffee,naeb_tea,naeb_hort,maize_usd",
    ""
  ),
  unit = c(
    "%", "million 2015 US$", "%", "million 2015 US$",
    "% of labour force", "% of employment", "% of labour force", "current US$",
    "% per year", "% per year", "crop figure", "tonnes",
    "RWF per USD", "RWF per USD", "current US$", "current US$"
  ),
  digits = c(
    1, 1, 1, 1,
    1, 1, 1, 0,
    1, 1, 0, 0,
    1, 1, 0, 0
  ),
  source = c(
    "World Bank lending rate; FAOSTAT credit and spending shares",
    "FAOSTAT Credit to Agriculture, constant 2015 US$",
    "World Bank FR.INR.LEND and FR.INR.RINR",
    "FAOSTAT Government Expenditure, agriculture forestry fishing",
    "World Bank ILO unemployment, youth, and employment in agriculture",
    "World Bank employment in agriculture",
    "World Bank SL.UEM.TOTL.ZS and SL.UEM.1524.ZS, ILO model",
    "World Bank BX.TRF.PWKR.CD.DT",
    "World Bank GDP growth, agriculture share, and agriculture value added",
    "World Bank NY.GDP.MKTP.KD.ZG, agriculture share of GDP, agriculture value added",
    "NISR SAS 2026 Season A, Table 14 crop figure",
    "FAOSTAT production: maize, sorghum, raw milk of cattle",
    "World Bank PA.NUS.FCRF, FAO maize in USD, World Bank external debt",
    "World Bank PA.NUS.FCRF",
    "NAEB export performance panel; FAO maize price ÷ official rate",
    "World Bank DT.DOD.DECT.CD"
  ),
  caveat = c(
    "National series. No district interest rate was published.",
    "Not a seasonal farm-loan rate from the National Bank of Rwanda.",
    "Bank rates. Not a cooperative, land, or microfinance rate.",
    "General government. Not a district budget line.",
    "ILO modelled rates. Not the NISR district labour-force table.",
    "Share of jobs, not pay and not a count of hired days.",
    "ILO ages 15–24 for youth. Not NISR underemployment.",
    "National remittances. Not a transfer into Nyagatare or any other district.",
    "National money. The district map is the SAS crop figure, not district GDP.",
    "Not agricultural GDP growth as a separate published series.",
    "One season, one year. Not a time series and not a GDP.",
    "National tonnes. Empty years were not filled.",
    "One official rate. Districts do not have their own rate.",
    "Period average. Not a bureau de change quote.",
    "NAEB years are fiscal years. Only the published years are shown.",
    "Country debt stock. Not a factory loan."
  ),
  stringsAsFactors = FALSE
)
factor_items <- rbind(factor_items, force_add)
rownames(factor_items) <- NULL

iso_label <- function(code) {
  known <- c(
    RWA = "Rwanda", BDI = "Burundi", UGA = "Uganda", TZA = "Tanzania", COD = "DR Congo",
    KEN = "Kenya", ETH = "Ethiopia", SSD = "South Sudan", SOM = "Somalia", SDN = "Sudan",
    CAF = "Central African Republic", NER = "Niger", NGA = "Nigeria", MLI = "Mali",
    BFA = "Burkina Faso", TCD = "Chad", CMR = "Cameroon", MOZ = "Mozambique", MWI = "Malawi",
    ZMB = "Zambia", ZWE = "Zimbabwe", AGO = "Angola", BEN = "Benin", GHA = "Ghana",
    SEN = "Senegal", SLE = "Sierra Leone", LBR = "Liberia", GIN = "Guinea", GMB = "Gambia",
    MRT = "Mauritania", AFG = "Afghanistan", YEM = "Yemen", SYR = "Syria", IRQ = "Iraq",
    LBN = "Lebanon", PSE = "Palestine", JOR = "Jordan", UKR = "Ukraine", HTI = "Haiti",
    COL = "Colombia", VEN = "Venezuela", GTM = "Guatemala", HND = "Honduras",
    SLV = "El Salvador", NIC = "Nicaragua", MMR = "Myanmar", BGD = "Bangladesh",
    NPL = "Nepal", LKA = "Sri Lanka", PAK = "Pakistan", KGZ = "Kyrgyzstan",
    TJK = "Tajikistan", LAO = "Laos", KHM = "Cambodia", PHL = "Philippines",
    IDN = "Indonesia", ARM = "Armenia", GEO = "Georgia", MDA = "Moldova"
  )
  out <- unname(known[as.character(code)])
  ifelse(is.na(out) | !nzchar(out), as.character(code), out)
}

food_price_categories <- c(
  "cereals and tubers",
  "oil and fats",
  "pulses and nuts",
  "vegetables and fruits",
  "miscellaneous food"
)
live_price_categories <- c(
  "milk and dairy",
  "meat, fish and eggs",
  "hides and skins",
  "live animals",
  "honey",
  "animal feed"
)
input_price_categories <- c("fertilizer", "pesticide")

naeb_category <- function(product) {
  p <- tolower(trimws(product))
  hit <- function(rx) grepl(rx, p)
  out <- rep("vegetables and fruits", length(p))
  out[hit("maize|minimex|rice|sorghum|wheat|^cassava$|cassava-flour|potato|^yams$|^taro$|^banana$")] <- "cereals and tubers"
  out[hit("^bean|beans|^peas$|groundnut|soya|^fresh bean|^fresh peas")] <- "pulses and nuts"
  out[hit("oil")] <- "oil and fats"
  out[hit("^sugar$")] <- "miscellaneous food"
  out[hit("milk")] <- "milk and dairy"
  out[hit("beef|meat|meet|fish|^rabbit$") | (hit("^egg") & !hit("eggplant"))] <- "meat, fish and eggs"
  out[hit("skin")] <- "hides and skins"
  out[hit("^goat$|^gaot|^sheep$|^pig$|^hen$|chicken")] <- "live animals"
  out[hit("honey")] <- "honey"
  out[hit("mash|feed|bran")] <- "animal feed"
  out[hit("^dap$|^npk$|^uree$|^urea$")] <- "fertilizer"
  out[hit("ditane|dithane")] <- "pesticide"
  out[hit("charcoal|^gaz$|coke")] <- "fuel"
  out
}

naeb_dir <- file.path("dataset hachthon", "RWANDA_INSTITUTIONS", "NAEB", "market prices")
naeb_prices <- data.frame(
  date = as.Date(character()), year = integer(), month = integer(),
  product = character(), unit = character(), market = character(),
  district = character(), province = character(), category = character(),
  tab = character(), price = numeric(),
  stringsAsFactors = FALSE
)
naeb_files <- if (dir.exists(naeb_dir)) list.files(naeb_dir, pattern = "\\.csv$", full.names = TRUE) else character()
naeb_keys <- c("tab", "category", "product", "unit", "district", "province", "year", "month")
naeb_sig <- local({
  fi <- file.info(naeb_files)
  paste(c("naeb-v2", basename(naeb_files), fi$size, as.numeric(fi$mtime), deparse(naeb_category)), collapse = "|")
})
naeb_cache <- read_rds_cache("naeb_prices.rds", naeb_sig)
naeb_cached <- !is.null(naeb_cache)
if (naeb_cached) {
  naeb_prices <- naeb_cache$prices
  naeb_screened <- naeb_cache$screened
  naeb_month <- naeb_cache$month
}
rm(naeb_cache)
if (!naeb_cached && length(naeb_files)) {
  naeb_need <- c("observed_date", "product", "unit", "location", "region", "price")
  naeb_read <- function(f) {
    z <- tryCatch(
      if (requireNamespace("data.table", quietly = TRUE)) {
        data.table::fread(f, data.table = FALSE, showProgress = FALSE, encoding = "UTF-8")
      } else {
        utils::read.csv(f, stringsAsFactors = FALSE)
      },
      error = function(e) NULL
    )
    if (is.null(z) || !all(naeb_need %in% names(z))) return(NULL)
    z <- z[, naeb_need]
    z[] <- lapply(z, as.character)
    z
  }
  raw <- do.call(rbind, lapply(naeb_files, naeb_read))
  if (!is.null(raw) && nrow(raw)) {
    raw <- raw[!duplicated(raw), , drop = FALSE]
    dt <- suppressWarnings(as.Date(substr(raw$observed_date, 1, 10)))
    reg <- trimws(raw$region)
    reg_u <- unique(reg)
    dist <- unname(setNames(tools::toTitleCase(tolower(reg_u)), reg_u)[reg])
    prod <- trimws(raw$product)
    prod[prod == "Uree"] <- "Urea"
    prod[prod == "Ditane"] <- "Dithane"
    catg <- naeb_category(prod)
    tab <- ifelse(catg %in% food_price_categories, "food",
      ifelse(catg %in% live_price_categories, "live",
        ifelse(catg %in% input_price_categories, "input", "other")))
    naeb_prices <- data.frame(
      date = dt,
      year = as.integer(format(dt, "%Y")),
      month = as.integer(format(dt, "%m")),
      product = prod,
      unit = trimws(raw$unit),
      market = trimws(raw$location),
      district = dist,
      province = unname(province_of[dist]),
      category = catg,
      tab = tab,
      price = suppressWarnings(as.numeric(raw$price)),
      stringsAsFactors = FALSE
    )
    naeb_prices <- naeb_prices[
      is.finite(naeb_prices$price) & naeb_prices$price > 0 & !is.na(naeb_prices$date) &
        naeb_prices$tab != "other" & !is.na(naeb_prices$province), , drop = FALSE]
  }
  rm(raw)
}
if (!naeb_cached) {
  naeb_screened <- 0L
  if (nrow(naeb_prices)) {
    ratio <- naeb_prices$price / stats::ave(naeb_prices$price, naeb_prices$product, FUN = stats::median)
    keep <- ratio >= 0.2 & ratio <= 5
    naeb_screened <- sum(!keep)
    naeb_prices <- naeb_prices[keep, , drop = FALSE]
  }
  naeb_month <- if (nrow(naeb_prices)) {
    key <- do.call(paste, c(naeb_prices[naeb_keys], sep = "\r"))
    first <- !duplicated(key)
    m <- naeb_prices[first, naeb_keys]
    m$price <- as.numeric(tapply(naeb_prices$price, key, stats::median)[key[first]])
    m$n <- as.integer(tapply(naeb_prices$price, key, length)[key[first]])
    rownames(m) <- NULL
    m
  } else {
    cbind(naeb_prices[0, naeb_keys], price = numeric(), n = integer())
  }
  if (length(naeb_files)) {
    write_rds_cache("naeb_prices.rds", list(sig = naeb_sig, prices = naeb_prices, screened = naeb_screened, month = naeb_month))
  }
}
naeb_screen_note <- paste0(
  format(naeb_screened, big.mark = ","), " readings more than five times above or below their product's median were treated as entry errors and left out."
)

naeb_clean_dir <- file.path("dataset hachthon", "RWANDA_INSTITUTIONS", "NAEB", "cleaned")
naeb_names <- local({
  p <- file.path(naeb_clean_dir, "naeb_product_names.csv")
  if (file.exists(p)) utils::read.csv(p, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = character()) else data.frame(product = character(), en = character(), rw = character())
})
prod_en <- function(x) {
  v <- naeb_names$en[match(x, naeb_names$product)]
  ifelse(is.na(v) | !nzchar(v), x, v)
}
prod_rw <- function(x) {
  v <- naeb_names$rw[match(x, naeb_names$product)]
  ifelse(is.na(v), "", v)
}
prod_label <- function(x) {
  rw <- prod_rw(x)
  ifelse(nzchar(rw), paste(prod_en(x), "/", rw), prod_en(x))
}

market_xy <- local({
  p <- file.path(naeb_clean_dir, "naeb_market_locations.csv")
  if (!file.exists(p)) return(data.frame(market = character(), district = character(), lat = numeric(), lon = numeric(), method = character(), exact = logical()))
  x <- utils::read.csv(p, stringsAsFactors = FALSE)
  key <- paste(round(x$lat, 4), round(x$lon, 4))
  k <- stats::ave(seq_along(key), key, FUN = seq_along) - 1
  ang <- 2 * pi * k / 6
  x$lat <- x$lat + ifelse(k > 0, 0.012 * sin(ang), 0)
  x$lon <- x$lon + ifelse(k > 0, 0.012 * cos(ang), 0)
  x[, c("market", "district", "lat", "lon", "method", "exact")]
})
has_leaflet <- requireNamespace("leaflet", quietly = TRUE)

naeb_cur_year <- if (nrow(naeb_prices)) max(naeb_prices$year) else NA_integer_
naeb_base_year <- naeb_cur_year - 1L
naeb_months <- sort(unique(naeb_prices$month[naeb_prices$year %in% naeb_cur_year]))
naeb_period_choices <- if (length(naeb_months)) {
  c(
    setNames("ytd", paste0(month.abb[min(naeb_months)], " – ", month.abb[max(naeb_months)], " ", naeb_cur_year, " vs same months ", naeb_base_year)),
    setNames(as.character(rev(naeb_months)), paste(month.abb[rev(naeb_months)], naeb_cur_year, "vs", naeb_base_year))
  )
} else {
  c("Year to date" = "ytd")
}
naeb_span <- if (nrow(naeb_prices)) paste(format(min(naeb_prices$date), "%b %Y"), "to", format(max(naeb_prices$date), "%b %Y")) else "—"
price_year_choices <- c("All years" = "all", setNames(as.character(sort(unique(naeb_prices$year))), sort(unique(naeb_prices$year))))
cap_first <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))
low_first <- function(x) paste0(tolower(substring(x, 1, 1)), substring(x, 2))
viz_type_choices <- c("Bar chart" = "bar", "Line" = "line", "Pie / donut" = "pie", "Scatter" = "scatter", "Map" = "map")
stat_group_choices <- c(
  "Central tendency (the center)" = "center",
  "Dispersion (the spread)" = "spread",
  "Position (the rank)" = "position",
  "Relationship (the connection)" = "relation"
)
stat_measures <- list(
  center = c("Mean", "Median", "Mode"),
  spread = c("Range", "Variance", "Standard deviation", "Interquartile range (IQR)"),
  position = c("Percentile", "Z-score"),
  relation = c("Correlation coefficient")
)
stat_vs_choices <- c(
  "Farmers using inorganic fertilizer (%) · SAS 2026 A" = "fert",
  "Farmers using improved seeds (%) · SAS 2026 A" = "seed",
  "Farmers using pesticides (%) · SAS 2026 A" = "pest",
  "Number of price readings" = "readings",
  "Time (month or year)" = "time"
)
naeb_provinces <- intersect(c("Kigali", "Southern", "Western", "Northern", "Eastern"), unique(naeb_prices$province))

read_fao_use <- function(path, group, element_keep = NULL) {
  empty <- data.frame(
    group = character(), item = character(), element = character(),
    year = integer(), unit = character(), value = numeric(),
    stringsAsFactors = FALSE
  )
  if (!file.exists(path)) return(empty)
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  need <- c("Item", "Element", "Year", "Unit", "Value")
  if (!all(need %in% names(raw))) return(empty)
  raw$Value <- suppressWarnings(as.numeric(raw$Value))
  raw$Year <- suppressWarnings(as.integer(raw$Year))
  raw <- raw[is.finite(raw$Value) & is.finite(raw$Year), , drop = FALSE]
  if (!is.null(element_keep)) raw <- raw[raw$Element %in% element_keep, , drop = FALSE]
  if (!nrow(raw)) return(empty)
  data.frame(
    group = group, item = raw$Item, element = raw$Element,
    year = raw$Year, unit = raw$Unit, value = raw$Value,
    stringsAsFactors = FALSE
  )
}

input_fao <- rbind(
  read_fao_use(file.path(data_dir, "fao_rwanda_fertilizers.csv"), "Fertilizer"),
  read_fao_use(file.path(data_dir, "fao_rwanda_pesticides_use.csv"), "Pesticide"),
  read_fao_use(file.path(data_dir, "fao_rwanda_food_balances.csv"), "Seed", "Seed")
)
input_fao$level <- rep("National", nrow(input_fao))
input_fao$district <- rep("Rwanda", nrow(input_fao))
input_fao$province <- rep("National", nrow(input_fao))

read_sas_input <- function(n, group, kind, item = NULL, measure = NULL) {
  empty <- input_fao[0, , drop = FALSE]
  path <- file.path(sas_2026, paste0("Tables_2026 Season A__Table_", n, ".csv"))
  if (!file.exists(path)) return(empty)
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  raw <- raw[as.character(raw[[1]]) %in% names(province_of), , drop = FALSE]
  if (!nrow(raw)) return(empty)
  meta <- c("source", "dataset", "year", "season", "downloaded_at", "Total")
  build <- function(it, me, vals) {
    data.frame(
      group = group, item = it, element = me, year = 2026L, unit = "%",
      value = suppressWarnings(as.numeric(vals)),
      level = "District", district = raw[[1]],
      province = unname(province_of[raw[[1]]]),
      stringsAsFactors = FALSE
    )
  }
  if (identical(kind, "pct")) {
    labels <- c("Farmers who used it (%)", "Plots where it was used (%)", "Land where it was used (%)")
    out <- do.call(rbind, lapply(1:3, function(k) build(item, labels[k], raw[[3 * k - 1]])))
  } else {
    cols <- setdiff(names(raw)[-1], meta)
    out <- do.call(rbind, lapply(cols, function(cl) build(paste0(item, cl), measure, raw[[cl]])))
  }
  out[is.finite(out$value), , drop = FALSE]
}

input_sas <- rbind(
  read_sas_input(20, "Seed", "pct", "Improved seeds"),
  read_sas_input(22, "Seed", "share", "Seed source: ", "Share of improved seed by source (%)"),
  read_sas_input(24, "Fertilizer", "pct", "Organic fertilizer"),
  read_sas_input(25, "Fertilizer", "pct", "Inorganic fertilizer"),
  read_sas_input(28, "Fertilizer", "share", "", "Share of inorganic fertilizer by type (%)"),
  read_sas_input(26, "Fertilizer", "share", "Fertilizer source: ", "Share of inorganic fertilizer by source (%)"),
  read_sas_input(29, "Pesticide", "pct", "Pesticides (any)"),
  read_sas_input(30, "Pesticide", "share", "", "Share of pesticide use by product (%)")
)
input_series <- rbind(input_fao, input_sas)

# Weight of each priced input inside a district basket: share of farmers using the
# input group (SAS Tables 25 and 29) times the product's share of that group (Tables 28 and 30).
input_price_weights <- local({
  pick <- function(item, element) input_sas[input_sas$item == item & input_sas$element == element, c("district", "value"), drop = FALSE]
  fert <- pick("Inorganic fertilizer", "Farmers who used it (%)")
  pest <- pick("Pesticides (any)", "Farmers who used it (%)")
  part <- function(product, element, use) {
    j <- merge(pick(product, element), use, by = "district", suffixes = c("_share", "_use"))
    if (!nrow(j)) return(NULL)
    data.frame(product = product, district = j$district, weight = j$value_use * j$value_share / 100, stringsAsFactors = FALSE)
  }
  ft <- "Share of inorganic fertilizer by type (%)"
  out <- rbind(
    part("NPK", ft, fert), part("Urea", ft, fert), part("DAP", ft, fert),
    part("Dithane", "Share of pesticide use by product (%)", pest)
  )
  if (is.null(out)) data.frame(product = character(), district = character(), weight = numeric(), stringsAsFactors = FALSE) else out[is.finite(out$weight), , drop = FALSE]
})
input_level_choices <- c(
  "By district · NISR SAS 2026 Season A" = "District",
  "National by year · FAOSTAT" = "National"
)
livestock_milk <- factor_data[factor_data$id == "milk_price", c("year", "value"), drop = FALSE]

infl_core <- function(m, mode, months) {
  if (is.null(m) || !nrow(m) || !length(months) || !is.finite(naeb_cur_year)) return(NULL)
  keys <- c("category", "product", "unit", "district", "province")
  m <- m[m$month %in% months & m$year %in% c(naeb_base_year, naeb_cur_year), , drop = FALSE]
  if (!nrow(m)) return(NULL)
  gk <- do.call(paste, c(m[keys], sep = "\r"))
  g <- paste(gk, m$year, sep = "\t")
  first <- !duplicated(g)
  sp <- rowsum(m$price, g)
  cn <- rowsum(rep(1, nrow(m)), g)
  sn <- rowsum(m$n, g)
  pm <- m[first, c(keys, "year"), drop = FALSE]
  pm$price <- (sp[, 1] / cn[, 1])[g[first]]
  pm$n <- sn[g[first], 1]
  pk <- gk[first]
  is0 <- pm$year == naeb_base_year
  is1 <- pm$year == naeb_cur_year
  j <- match(pk[is0], pk[is1])
  ok <- !is.na(j)
  if (!any(ok)) return(NULL)
  a <- pm[is0, , drop = FALSE][ok, , drop = FALSE]
  b <- pm[is1, , drop = FALSE][j[ok], , drop = FALSE]
  pd <- a[, keys, drop = FALSE]
  pd$price_0 <- a$price
  pd$price_1 <- b$price
  pd$n_0 <- a$n
  pd$n_1 <- b$n
  rownames(pd) <- NULL
  pd$yoy <- 100 * (pd$price_1 / pd$price_0 - 1)
  pd$n <- pd$n_0 + pd$n_1
  raw_w <- rep(1, nrow(pd))
  if (identical(mode, "input")) {
    w <- input_price_weights$weight[match(paste(pd$product, pd$district), paste(input_price_weights$product, input_price_weights$district))]
    raw_w <- ifelse(is.finite(w) & w > 0, w, 0)
  }
  tot <- stats::ave(raw_w, pd$district, FUN = sum)
  cnt <- stats::ave(raw_w, pd$district, FUN = length)
  pd$weight <- ifelse(tot > 0, raw_w / tot, 1 / cnt)
  pd$contrib <- pd$weight * pd$yoy
  yd <- rowsum(pd$contrib, pd$district)
  dist <- data.frame(
    district = rownames(yd), yoy = yd[, 1],
    n = rowsum(pd$n, pd$district)[rownames(yd), 1],
    products = as.integer(rowsum(rep(1, nrow(pd)), pd$district)[rownames(yd), 1]),
    stringsAsFactors = FALSE
  )
  dist$province <- unname(province_of[dist$district])
  rownames(dist) <- NULL
  D <- nrow(dist)
  national <- mean(dist$yoy)
  dist$to_nat <- dist$yoy / D
  dist$share <- if (abs(national) > 1e-9) 100 * dist$to_nat / national else rep(NA_real_, D)
  dist$rank <- rank(-dist$yoy, ties.method = "min")
  dist <- dist[order(-dist$yoy), , drop = FALSE]
  pd$to_nat <- pd$contrib / D
  pf <- !duplicated(pd$product)
  prod <- pd[pf, c("category", "product", "unit"), drop = FALSE]
  cnt_p <- rowsum(rep(1, nrow(pd)), pd$product)[prod$product, 1]
  prod$to_nat <- rowsum(pd$to_nat, pd$product)[prod$product, 1]
  prod$price_0 <- rowsum(pd$price_0, pd$product)[prod$product, 1] / cnt_p
  prod$price_1 <- rowsum(pd$price_1, pd$product)[prod$product, 1] / cnt_p
  prod$yoy <- rowsum(pd$yoy, pd$product)[prod$product, 1] / cnt_p
  prod$districts <- as.integer(cnt_p)
  rownames(prod) <- NULL
  cs <- rowsum(pd$to_nat, pd$category)
  cat_sum <- data.frame(category = rownames(cs), to_nat = cs[, 1], stringsAsFactors = FALSE)
  list(pd = pd, dist = dist, prod = prod, cat = cat_sum, national = national, D = D, months = months)
}

period_text <- function(months) {
  if (!length(months)) return("")
  if (length(months) == 1) return(paste(month.abb[months], naeb_cur_year, "vs", month.abb[months], naeb_base_year))
  paste0(month.abb[min(months)], "–", month.abb[max(months)], " ", naeb_cur_year, " vs the same months of ", naeb_base_year)
}

.infl_overview_store <- new.env(parent = emptyenv())
.infl_overview_sig <- function() {
  paste(
    naeb_sig, paste(deparse(infl_core), collapse = ""),
    paste(input_price_weights$product, input_price_weights$district, input_price_weights$weight, collapse = ";")
  )
}

get_infl_overview_tab <- function(tab) {
  tab <- as.character(tab)[1]
  key <- paste0("tab:", tab)
  if (exists(key, .infl_overview_store, inherits = FALSE)) {
    return(get(key, .infl_overview_store, inherits = FALSE))
  }
  sig <- .infl_overview_sig()
  old <- read_rds_cache("infl_overview.rds", sig)
  if (!is.null(old) && !is.null(old$value$tabs[[tab]])) {
    assign(key, old$value$tabs[[tab]], .infl_overview_store)
    return(old$value$tabs[[tab]])
  }
  res <- infl_core(naeb_month[naeb_month$tab == tab, , drop = FALSE], tab, naeb_months)
  assign(key, res, .infl_overview_store)
  res
}

get_infl_overview_cats <- function() {
  if (exists("cats", .infl_overview_store, inherits = FALSE)) {
    return(get("cats", .infl_overview_store, inherits = FALSE))
  }
  sig <- .infl_overview_sig()
  old <- read_rds_cache("infl_overview.rds", sig)
  if (!is.null(old) && !is.null(old$value$cats)) {
    assign("cats", old$value$cats, .infl_overview_store)
    return(old$value$cats)
  }
  cats <- c(input_price_categories, food_price_categories, live_price_categories)
  rows <- do.call(rbind, lapply(cats, function(cg) {
    m <- naeb_month[naeb_month$category == cg, , drop = FALSE]
    if (!nrow(m)) return(NULL)
    r <- infl_core(m, m$tab[1], naeb_months)
    if (is.null(r)) return(NULL)
    data.frame(category = cg, tab = m$tab[1], yoy = r$national, D = r$D, products = nrow(r$prod), stringsAsFactors = FALSE)
  }))
  assign("cats", rows, .infl_overview_store)
  rows
}

get_infl_overview <- function() {
  if (exists("value", .infl_overview_store, inherits = FALSE)) {
    return(get("value", .infl_overview_store, inherits = FALSE))
  }
  sig <- .infl_overview_sig()
  old <- read_rds_cache("infl_overview.rds", sig)
  if (!is.null(old)) {
    assign("value", old$value, .infl_overview_store)
    for (t in names(old$value$tabs)) assign(paste0("tab:", t), old$value$tabs[[t]], .infl_overview_store)
    if (!is.null(old$value$cats)) assign("cats", old$value$cats, .infl_overview_store)
    return(old$value)
  }
  tabs <- c("input", "food", "live")
  res <- stats::setNames(lapply(tabs, get_infl_overview_tab), tabs)
  rows <- get_infl_overview_cats()
  value <- list(tabs = res, cats = rows)
  write_rds_cache("infl_overview.rds", list(sig = sig, value = value))
  assign("value", value, .infl_overview_store)
  value
}

factor_span <- function(series_id) {
  d <- factor_data[factor_data$id == series_id & is.finite(factor_data$value), , drop = FALSE]
  if (!nrow(d)) return("—")
  paste0(fmt_num(d$value[which.max(d$year)], 1), " · ", max(d$year))
}

site_footer <- function() {
  tags$footer(
    class = "site-foot",
    tags$div(
      class = "foot-grid",
      tags$div(
        tags$h4("Umuganura"),
        tags$p("The first fruits walk the country. A harvest cockpit for the people who decide.")
      ),
      tags$div(
        tags$h4("Open a room"),
        tags$ul(
          tags$li(tags$a("Home", href = "#home", `data-panel` = "home")),
          tags$li(tags$a("About", href = "#about", `data-panel` = "about")),
          tags$li(tags$a("Umusaruro", href = "#umusaruro", `data-panel` = "umusaruro")),
          tags$li(tags$a("Isoko", href = "#factors", `data-panel` = "factors")),
          tags$li(tags$a("Ikigega", href = "#ikigega", `data-panel` = "ikigega")),
          tags$li(tags$a("Ejo hazaza", href = "#ejo-hazaza", `data-panel` = "ejo-hazaza"))
        )
      ),
      tags$div(
        tags$h4("Drawers"),
        tags$ul(
          tags$li("Umusaruro — 33 products, district yields 2019–2026"),
          tags$li("Isoko — inflation, interest, jobs, growth, the franc"),
          tags$li("Ikigega — reserve, cooperative stores, cold chain"),
          tags$li("Ejo Hazaza — loss causes, forecast, action plan")
        )
      ),
      tags$div(
        tags$h4("Sources"),
        tags$ul(
          tags$li("NISR Seasonal Agricultural Survey"),
          tags$li("World Bank agriculture series"),
          tags$li("FAOSTAT production"),
          tags$li("NAEB export performance")
        )
      )
    ),
    tags$p(class = "foot-legal", "Umuganura · NISR 2026 Big Data Hackathon · Track 1 Agricultural Productivity. Figures are published values. Empty cells stay empty.")
  )
}

source("harvest.R", local = FALSE, encoding = "UTF-8")
source("umusaruro.R", local = FALSE, encoding = "UTF-8")
source("ikigega.R", local = FALSE, encoding = "UTF-8")
source("about.R", local = FALSE, encoding = "UTF-8")
source("ejo.R", local = FALSE, encoding = "UTF-8")
source("ejo_inspect.R", local = FALSE, encoding = "UTF-8")
source("ejo_detail.R", local = FALSE, encoding = "UTF-8")
source("ejo_institution.R", local = FALSE, encoding = "UTF-8")
source("ejo_db.R", local = FALSE, encoding = "UTF-8")
source("ejo_portal.R", local = FALSE, encoding = "UTF-8")
source("ejo_action.R", local = FALSE, encoding = "UTF-8")
source("ejo_action_story.R", local = FALSE, encoding = "UTF-8")
source("ejo_desk.R", local = FALSE, encoding = "UTF-8")

home_factor_html <- ""
home_stock_html <- ""
home_factor_html <- tryCatch(local({
  specs <- list(
    list(id = "inflation", lab = "Inflation", suf = "%"),
    list(id = "lending", lab = "Interest rate", suf = "%"),
    list(id = "gdp_share", lab = "GDP share", suf = "%"),
    list(id = "unemployment", lab = "Unemployment", suf = "%"),
    list(id = "fx", lab = "Exchange rate", suf = "")
  )
  rows <- lapply(specs, function(s) {
    d <- factor_data[factor_data$id == s$id & is.finite(factor_data$value), , drop = FALSE]
    if (!nrow(d)) return("")
    last <- d[which.max(d$year), ]
    peak <- max(abs(d$value))
    w <- if (is.finite(peak) && peak > 0) round(100 * abs(last$value) / peak) else 0
    sprintf(
      '<div class="home-bar"><span>%s</span><i><b style="width:%d%%"></b></i><strong>%s%s · %s</strong></div>',
      s$lab, w, formatC(last$value, format = "f", digits = 1, big.mark = ","), s$suf, last$year
    )
  })
  paste0('<div class="home-bars">', paste(rows, collapse = ""), "</div>")
}), error = function(e) "")

home_stock_html <- tryCatch(local({
  if (!exists("stock_mass") || !nrow(stock_mass)) return("")
  y <- max(stock_mass$year)
  d <- stock_mass[stock_mass$year == y, , drop = FALSE]
  vals <- c(
    Grain = sum(d$stored_mt[d$crop %in% nsr_crops], na.rm = TRUE),
    Roots = sum(d$stored_mt[d$crop %in% c("Irish potato", "Sweet potato", "Cassava", "Yams & Taro")], na.rm = TRUE),
    Fresh = sum(d$stored_mt[d$crop %in% c("Vegetables", "Fruits", "Cooking banana")], na.rm = TRUE)
  )
  if (!any(vals > 0)) return("")
  cols <- c("#1b4332", "#c6a336", "#1f5fd1")
  pct <- round(100 * vals / sum(vals), 1)
  a <- 0
  parts <- character()
  for (i in seq_along(vals)) {
    b <- a + pct[i]
    parts <- c(parts, paste0(cols[i], " ", a, "% ", b, "%"))
    a <- b
  }
  pie <- paste0('<div class="home-pie" style="background:conic-gradient(', paste(parts, collapse = ","), ')"></div>')
  legend <- paste0(
    '<div class="home-legend">',
    paste(sprintf('<span><i style="background:%s"></i>%s · %s%% · %s MT</span>', cols, names(vals), pct, formatC(vals, format = "f", digits = 0, big.mark = ",")), collapse = ""),
    "</div>"
  )
  paste0(pie, legend, '<p class="note">SAS ', y, ".</p>")
}), error = function(e) "")

home_brief <- list(
  price = "\u2014", price_year = "",
  kept = "\u2014", kept_note = "No stored-grain figure is loaded",
  fresh = "\u2014", fresh_note = "",
  lost = "\u2014", lost_note = "No 2026 loss total is loaded"
)
tryCatch({
  d <- factor_data[factor_data$id == "inflation" & is.finite(factor_data$value), , drop = FALSE]
  if (nrow(d)) {
    last <- d[which.max(d$year), ]
    home_brief$price <- fmt_pct(last$value, 1)
    home_brief$price_year <- as.character(last$year)
  }
}, error = function(e) NULL)
tryCatch({
  y <- max(stock_mass$year)
  d <- stock_mass[stock_mass$year == y, , drop = FALSE]
  grain <- sum(d$stored_mt[d$crop %in% nsr_crops], na.rm = TRUE)
  fresh <- sum(d$stored_mt[d$crop %in% c("Vegetables", "Fruits", "Cooking banana")], na.rm = TRUE)
  if (is.finite(grain) && grain > 0) {
    home_brief$kept <- paste(fmt_num(grain, 0), "t")
    gap <- nsr_floor - grain
    home_brief$kept_note <- paste0(
      "SAS ", y, " stored share of grain. ",
      if (gap > 0) paste(fmt_num(gap, 0), "t under the 46,000 t reserve floor") else "Above the 46,000 t reserve floor"
    )
  }
  if (is.finite(fresh) && fresh > 0) {
    home_brief$fresh <- paste(fmt_num(fresh, 0), "t")
    home_brief$fresh_note <- paste("Vegetables, fruits and cooking banana stored, SAS", y)
  }
}, error = function(e) NULL)
tryCatch({
  cy <- ejo_cy[ejo_cy$year == 2026, , drop = FALSE]
  if (nrow(cy) && any(is.finite(cy$total))) {
    home_brief$lost <- fmt_bn(sum(cy$total, na.rm = TRUE))
    home_brief$lost_note <- paste(fmt_num(sum(cy$tonnes, na.rm = TRUE), 0), "t across six causes in 2026")
  }
}, error = function(e) NULL)
home_cover <- list(
  districts = home_maize$n,
  datasets = "\u2014",
  years = "\u2014"
)
tryCatch({
  ok <- factor_data[is.finite(factor_data$year) & is.finite(factor_data$value), c("id", "year"), drop = FALSE]
  if (!nrow(ok)) stop("no series")
  n <- length(unique(ok$id))
  if (exists("home_prod_files") && nrow(home_prod_files)) n <- n + 1L
  if (exists("ejo_power") && is.data.frame(ejo_power) && nrow(ejo_power)) n <- n + 1L
  if (exists("naeb_prices") && is.data.frame(naeb_prices) && nrow(naeb_prices)) n <- n + 1L
  home_cover$datasets <- as.character(n)
  y0 <- min(ok$year)
  y1 <- max(ok$year)
  sas <- suppressWarnings(as.integer(home_prod_files[, 1]))
  sas <- sas[is.finite(sas)]
  if (length(sas)) {
    y0 <- min(y0, min(sas))
    y1 <- max(y1, max(sas))
  }
  home_cover$years <- paste0(y0, "\u2013", y1)
}, error = function(e) NULL)
if (exists("hort_naeb") && nrow(hort_naeb) && is.finite(hort_naeb$export_usd[nrow(hort_naeb)])) {
  home_brief$export <- paste0("$", fmt_num(hort_naeb$export_usd[nrow(hort_naeb)] / 1e6, 1), "m")
  home_brief$export_note <- paste("NAEB horticulture, national,", hort_naeb$fiscal_year[nrow(hort_naeb)])
} else {
  home_brief$export <- "\u2014"
  home_brief$export_note <- "No NAEB horticulture export figure is loaded"
}

iso_series <- function(id) {
  d <- factor_data[factor_data$id == id & is.finite(factor_data$year) & is.finite(factor_data$value), c("year", "value"), drop = FALSE]
  if (!nrow(d)) return(d)
  d[order(d$year), , drop = FALSE]
}

iso_line_svg <- function(years, values, stroke) {
  ok <- is.finite(years) & is.finite(values)
  years <- years[ok]
  values <- values[ok]
  if (length(values) < 2) return("")
  n <- length(values)
  x0 <- 54
  x1 <- 620
  y0 <- 18
  y1 <- 228
  rng <- range(values)
  if (diff(rng) == 0) rng <- rng + c(-1, 1)
  pad <- diff(rng) * 0.08
  rng <- rng + c(-pad, pad)
  xs <- x0 + (seq_len(n) - 1) / (n - 1) * (x1 - x0)
  ys <- y1 - (values - rng[1]) / diff(rng) * (y1 - y0)
  grid <- vapply(seq(0, 4), function(i) {
    yy <- y0 + i / 4 * (y1 - y0)
    val <- rng[2] - i / 4 * diff(rng)
    paste0('<line x1="', x0, '" y1="', round(yy, 1), '" x2="', x1, '" y2="', round(yy, 1), '" stroke="#e6edf5" stroke-width="1"/>',
           '<text x="', x0 - 8, '" y="', round(yy + 4, 1), '" text-anchor="end" fill="#7b8da3" font-size="11" font-family="Outfit,Segoe UI,sans-serif">',
           htmltools::htmlEscape(fmt_num(val, 1)), '</text>')
  }, character(1))
  area <- paste(c(paste(round(xs, 1), round(ys, 1), sep = ","), paste(x1, y1, sep = ","), paste(x0, y1, sep = ",")), collapse = " ")
  path <- paste(round(xs, 1), round(ys, 1), sep = ",", collapse = " ")
  labs <- unique(c(1, ceiling(n / 2), n))
  xlab <- paste(vapply(labs, function(i) {
    paste0('<text x="', round(xs[i], 1), '" y="252" text-anchor="middle" fill="#5c6b80" font-size="12" font-family="Outfit,Segoe UI,sans-serif">', years[i], '</text>')
  }, character(1)), collapse = "")
  paste0(
    '<svg class="iso-line" viewBox="0 0 640 270" role="img">',
    paste(grid, collapse = ""),
    '<polygon points="', area, '" fill="', stroke, '" fill-opacity="0.08"/>',
    '<polyline points="', path, '" fill="none" stroke="', stroke, '" stroke-width="2.5" stroke-linejoin="round" stroke-linecap="round"/>',
    '<circle cx="', round(xs[n], 1), '" cy="', round(ys[n], 1), '" r="5" fill="', stroke, '"/>',
    xlab,
    '</svg>'
  )
}

iso_card <- function(label, value, note, tone) {
  paste0(
    '<article class="iso-kpi is-', tone, '"><span>', htmltools::htmlEscape(label), '</span><strong>',
    htmltools::htmlEscape(value), '</strong><small>', htmltools::htmlEscape(note), '</small></article>'
  )
}

iso_chg_cell <- function(v, digits, bad_up) {
  if (!is.finite(v)) return('<td class="num">—</td>')
  cls <- if (abs(v) < 1e-9) "flat" else if (xor(v > 0, bad_up)) "good" else "bad"
  mark <- if (v > 0) "\u25b2 " else if (v < 0) "\u25bc " else ""
  paste0('<td class="num is-', cls, '">', mark, htmltools::htmlEscape(sgn_plain(v, digits)), '</td>')
}

sgn_plain <- function(v, digits) {
  paste0(if (v > 0) "+" else "", fmt_num(v, digits))
}

iso_report_build <- function(spec, district) {
  primary <- iso_series(spec$series)
  if (!nrow(primary)) {
    return(paste0(
      '<section class="iso-rep"><header class="iso-rep-banner"><p class="iso-rep-brand">UMUGANURA · ISOKO</p><h2>',
      htmltools::htmlEscape(spec$title), '</h2><p>', htmltools::htmlEscape(spec$empty), '</p></header></section>'
    ))
  }
  n <- nrow(primary)
  last <- primary[n, ]
  prev <- if (n >= 2) primary[n - 1, ] else NULL
  delta <- if (!is.null(prev) && prev$value != 0 && spec$kind == "pct") 100 * (last$value / prev$value - 1) else if (!is.null(prev)) last$value - prev$value else NA_real_
  tone <- if (!is.finite(delta) || abs(delta) < 1e-9) "navy" else if (xor(delta > 0, spec$bad_up)) "green" else "red"
  recent <- tail(primary, 12)
  rows <- tail(primary, 8)
  chg <- c(NA_real_, diff(rows$value))
  table_rows <- paste(vapply(seq_len(nrow(rows)), function(i) {
    paste0(
      '<tr class="', if (i %% 2 == 0) "alt" else "", '"><td>', rows$year[i], '</td><td class="num">',
      htmltools::htmlEscape(paste(fmt_num(rows$value[i], spec$digits), spec$unit)), '</td>',
      iso_chg_cell(chg[i], spec$digits, spec$bad_up), '</tr>'
    )
  }, character(1)), collapse = "")
  cards <- paste(vapply(spec$cards, function(card) {
    d <- iso_series(card$id)
    if (!nrow(d)) return(iso_card(card$label, "—", "Not in the published files", "navy"))
    z <- d[nrow(d), ]
    note <- paste(z$year)
    tone_c <- "navy"
    if (nrow(d) >= 2) {
      step <- if (identical(card$kind, "pct") && d$value[nrow(d) - 1] != 0) 100 * (z$value / d$value[nrow(d) - 1] - 1) else z$value - d$value[nrow(d) - 1]
      tone_c <- if (!is.finite(step) || abs(step) < 1e-9) "navy" else if (xor(step > 0, card$bad_up)) "green" else "red"
      note <- paste0(z$year, " · ", sgn_plain(step, card$digits), if (identical(card$kind, "pct")) "%" else paste0(" ", card$unit), " vs ", d$year[nrow(d) - 1])
    }
    iso_card(card$label, paste(fmt_num(z$value, card$digits), card$unit), note, tone_c)
  }, character(1)), collapse = "")
  dist_html <- if (is.null(district) || !nrow(district)) {
    '<p class="iso-miss">A district breakdown of this national series was not published.</p>'
  } else {
    show <- rbind(head(district, 5), tail(district, 3))
    show <- show[!duplicated(show$district), , drop = FALSE]
    total <- sum(district$figure)
    paste0(
      '<table class="iso-table"><thead><tr><th>District</th><th>Province</th><th class="num">Harvest 2019–2026</th><th class="num">Share</th></tr></thead><tbody>',
      paste(vapply(seq_len(nrow(show)), function(i) {
        paste0('<tr class="', if (i %% 2 == 0) "alt" else "", '"><td>', htmltools::htmlEscape(show$district[i]),
               '</td><td>', htmltools::htmlEscape(unname(province_of[show$district[i]])),
               '</td><td class="num">', htmltools::htmlEscape(paste(fmt_num(show$figure[i], 0), "MT")),
               '</td><td class="num">', htmltools::htmlEscape(fmt_pct(100 * show$figure[i] / total, 1)), '</td></tr>')
      }, character(1)), collapse = ""),
      '</tbody></table><p class="iso-src">NISR SAS production, 2019–2025 seasons A and B plus 2026 season A. This is the district harvest the national rate meets. A district CPI, lending rate, exchange rate, or unemployment rate is not in these files.</p>'
    )
  }
  note <- htmltools::htmlEscape(spec$note)
  paste0(
    '<section class="iso-rep">',
    '<header class="iso-rep-banner"><p class="iso-rep-brand">UMUGANURA · ISOKO · RESEARCH</p>',
    '<h2>', htmltools::htmlEscape(spec$title), '</h2><span class="iso-rep-rule"></span>',
    '<p>', htmltools::htmlEscape(spec$lede), '</p>',
    '<div class="iso-rep-pills"><span>AS OF ', last$year, '</span><span>', htmltools::htmlEscape(spec$pill), ' ',
    htmltools::htmlEscape(paste(fmt_num(last$value, spec$digits), spec$unit)), '</span></div></header>',
    '<h3 class="iso-rep-h">At a glance</h3><div class="iso-rep-grid">', cards, '</div>',
    '<h3 class="iso-rep-h">', htmltools::htmlEscape(spec$trend), '</h3>',
    '<div class="iso-rep-split"><div class="iso-rep-chart">', iso_line_svg(recent$year, recent$value, if (tone == "red") "#c1121f" else if (tone == "green") "#0f7b4c" else "#0A2540"),
    '<p class="iso-src">', htmltools::htmlEscape(spec$source), '</p></div>',
    '<div class="iso-rep-side"><table class="iso-table"><thead><tr><th>Year</th><th class="num">', htmltools::htmlEscape(spec$col), '</th><th class="num">Change</th></tr></thead><tbody>',
    table_rows, '</tbody></table><p class="iso-src">Change is against the previous published year. Missing years stay off the table.</p></div></div>',
    '<h3 class="iso-rep-h">District markets this national figure meets</h3>', dist_html,
    '<h3 class="iso-rep-h">Research note</h3><aside class="iso-rep-note"><p>', note, '</p></aside>',
    '<footer class="iso-rep-foot"><span>Umuganura · Isoko</span><span>', htmltools::htmlEscape(spec$source), '</span></footer>',
    '</section>'
  )
}

iso_district_harvest <- home_district_agg

isoko_report_ids <- c(
  "inflation-overview", "interest-overview", "gdp-overview", "fx-overview", "jobs-overview"
)

.isoko_report_store <- new.env(parent = emptyenv())

isoko_report_specs <- function() {
  list(
  "inflation-overview" = list(
    title = "Inflation — latest published snapshot",
    lede = "Headline consumer-price inflation from the World Bank, with the farm prices that meet it. This is not a monthly NISR CPI.",
    pill = "CPI", series = "inflation", unit = "%", digits = 1, kind = "level", bad_up = TRUE,
    trend = "Headline inflation", col = "Inflation",
    source = "World Bank FP.CPI.TOTL.ZG. Producer prices are FAOSTAT, annual mean of published months.",
    empty = "The inflation series is not loaded, so this page stays empty.",
    note = "National headline inflation is not a district CPI and not the food basket. When the rate rises, fertilizer, fuel, and seed cost more in the market. The district table is the published harvest, not a local inflation rate.",
    cards = list(
      list(id = "inflation", label = "Headline inflation", unit = "%", digits = 1, kind = "level", bad_up = TRUE),
      list(id = "cpi", label = "CPI index", unit = "", digits = 1, kind = "level", bad_up = TRUE),
      list(id = "maize_price", label = "Maize producer price", unit = "RWF/t", digits = 0, kind = "level", bad_up = TRUE),
      list(id = "fert_use", label = "Fertilizer on cropland", unit = "kg/ha", digits = 1, kind = "level", bad_up = FALSE)
    )
  ),
  "gdp-overview" = list(
    title = "GDP — latest published snapshot",
    lede = "National real GDP growth, agriculture's share, and the district harvest that growth meets. Not a quarterly NISR GDP release.",
    pill = "GDP growth", series = "gdp_growth", unit = "%", digits = 1, kind = "level", bad_up = FALSE,
    trend = "Real GDP growth", col = "Growth",
    source = "World Bank NY.GDP.MKTP.KD.ZG, agriculture share NY from the agriculture-indicators file, value added in constant US$.",
    empty = "The GDP series is not loaded, so this page stays empty.",
    note = "National GDP growth is not agricultural GDP and not a district product. A rising economy lifts demand for milk, meat, and processed food. The district table shows where the harvest that meets that demand is grown.",
    cards = list(
      list(id = "gdp_growth", label = "Real GDP growth", unit = "%", digits = 1, kind = "level", bad_up = FALSE),
      list(id = "gdp_share", label = "Agriculture share of GDP", unit = "%", digits = 1, kind = "level", bad_up = FALSE),
      list(id = "ag_usd", label = "Agriculture value added", unit = "US$", digits = 0, kind = "level", bad_up = FALSE),
      list(id = "maize_prod", label = "Maize production", unit = "t", digits = 0, kind = "level", bad_up = FALSE)
    )
  ),
  "fx-overview" = list(
    title = "Exchange rate — latest published snapshot",
    lede = "Rwandan francs per US dollar, period average. One national rate. A rise means the franc buys fewer dollars.",
    pill = "RWF per USD", series = "fx", unit = "RWF", digits = 1, kind = "level", bad_up = TRUE,
    trend = "Francs per US dollar", col = "RWF per USD",
    source = "World Bank PA.NUS.FCRF. Maize in dollars is the FAO producer price divided by this rate.",
    empty = "The exchange-rate series is not loaded, so this page stays empty.",
    note = "There is one official rate, not a district rate. A weaker franc raises the franc cost of imported seed and fertilizer and the franc value of exports. The district table is the harvest those traded prices meet.",
    cards = list(
      list(id = "fx", label = "Francs per US dollar", unit = "RWF", digits = 1, kind = "level", bad_up = TRUE),
      list(id = "maize_usd", label = "Maize producer price", unit = "US$/t", digits = 1, kind = "level", bad_up = TRUE),
      list(id = "naeb_coffee", label = "Coffee exports", unit = "US$", digits = 0, kind = "level", bad_up = FALSE),
      list(id = "naeb_tea", label = "Tea exports", unit = "US$", digits = 0, kind = "level", bad_up = FALSE)
    )
  ),
  "interest-overview" = list(
    title = "Interest rates — latest published snapshot",
    lede = "The bank lending rate and the real rate. These are not a farm-loan quote and not a microfinance rate.",
    pill = "Lending rate", series = "lending", unit = "%", digits = 1, kind = "level", bad_up = TRUE,
    trend = "Bank lending rate", col = "Lending rate",
    source = "World Bank FR.INR.LEND and FR.INR.RINR. Credit to agriculture is FAOSTAT.",
    empty = "The lending-rate series is not loaded, so this page stays empty.",
    note = "A higher lending rate makes store credit and equipment loans dearer. No district lending rate was published. The district table is the harvest farmers would borrow against.",
    cards = list(
      list(id = "lending", label = "Bank lending rate", unit = "%", digits = 1, kind = "level", bad_up = TRUE),
      list(id = "real_rate", label = "Real interest rate", unit = "%", digits = 1, kind = "level", bad_up = TRUE),
      list(id = "ag_credit", label = "Credit to agriculture", unit = "US$", digits = 0, kind = "level", bad_up = FALSE),
      list(id = "ag_credit_share", label = "Agriculture share of credit", unit = "%", digits = 1, kind = "level", bad_up = FALSE)
    )
  ),
  "jobs-overview" = list(
    title = "Unemployment — latest published snapshot",
    lede = "ILO modelled unemployment for Rwanda. This is not the NISR labour-force survey and not a district rate.",
    pill = "ILO unemployment", series = "unemployment", unit = "%", digits = 1, kind = "level", bad_up = TRUE,
    trend = "ILO unemployment rate", col = "Unemployment",
    source = "World Bank SL.UEM.TOTL.ZS and SL.UEM.1524.ZS, ILO model. Jobs in agriculture are the World Bank employment share.",
    empty = "The unemployment series is not loaded, so this page stays empty.",
    note = "The ILO model is not the NISR Labour Force Survey. No district unemployment table is in these files. A higher rate weakens what households can pay in the market. The district table is the harvest that labour brings in.",
    cards = list(
      list(id = "unemployment", label = "Unemployment, ILO model", unit = "%", digits = 1, kind = "level", bad_up = TRUE),
      list(id = "youth", label = "Youth unemployment, ILO 15–24", unit = "%", digits = 1, kind = "level", bad_up = TRUE),
      list(id = "farm_jobs", label = "Jobs in agriculture", unit = "%", digits = 1, kind = "level", bad_up = FALSE),
      list(id = "remittances", label = "Remittances", unit = "US$", digits = 0, kind = "level", bad_up = FALSE)
    )
  )
  )
}

get_isoko_report_one <- function(id) {
  id <- as.character(id)[1]
  if (exists(id, .isoko_report_store, inherits = FALSE)) {
    return(get(id, .isoko_report_store, inherits = FALSE))
  }
  spec <- isoko_report_specs()[[id]]
  if (is.null(spec)) return("")
  html <- iso_report_build(spec, iso_district_harvest)
  assign(id, html, .isoko_report_store)
  html
}

get_isoko_report_html <- function() {
  stats::setNames(lapply(isoko_report_ids, get_isoko_report_one), isoko_report_ids)
}

write_bridge_exports <- function() {
  if (!has_json) return(invisible(NULL))
  bridge_dir <- file.path("www", "bridge")
  dir.create(bridge_dir, recursive = TRUE, showWarnings = FALSE)
  tryCatch({
  iso_ids <- c("inflation", "lending", "unemployment", "fx", "gdp_share")
  iso_lab <- c("Inflation", "Lending rate", "Unemployment", "Franc per dollar", "Agriculture share of GDP")
  iso_suf <- c("%", "%", "%", " RWF", "%")
  series <- lapply(seq_along(iso_ids), function(i) {
    d <- factor_data[factor_data$id == iso_ids[i] & is.finite(factor_data$value), c("year", "value"), drop = FALSE]
    d <- d[order(d$year), , drop = FALSE]
    list(id = iso_ids[i], label = iso_lab[i], suffix = iso_suf[i], points = lapply(seq_len(nrow(d)), function(r) {
      list(year = d$year[r], value = round(d$value[r], 4))
    }))
  })
  jsonlite::write_json(
    list(room = "isoko", source = "World Bank macro factors and agriculture share, prepared in R", series = series),
    file.path(bridge_dir, "isoko.json"), auto_unbox = TRUE, pretty = FALSE
  )
  if (exists("stock_mass") && nrow(stock_mass)) {
    utils::write.csv(stock_mass[, c("year", "season", "crop", "stored_mt")], file.path(bridge_dir, "stock_snapshot.csv"), row.names = FALSE)
    grain <- stock_mass[stock_mass$crop %in% nsr_crops & is.finite(stock_mass$stored_mt), , drop = FALSE]
    by_year <- aggregate(stored_mt ~ year, grain, sum)
    by_year <- by_year[order(by_year$year), , drop = FALSE]
    latest <- max(grain$year)
    by_crop <- aggregate(stored_mt ~ crop, grain[grain$year == latest, , drop = FALSE], sum)
    by_crop <- by_crop[order(-by_crop$stored_mt), , drop = FALSE]
    jsonlite::write_json(
      list(
        room = "ikigega",
        source = "SAS production times the share held after harvest, prepared in R",
        latest = latest,
        byYear = lapply(seq_len(nrow(by_year)), function(r) list(year = by_year$year[r], stored_mt = round(by_year$stored_mt[r], 1))),
        byCrop = lapply(seq_len(nrow(by_crop)), function(r) list(crop = by_crop$crop[r], stored_mt = round(by_crop$stored_mt[r], 1)))
      ),
      file.path(bridge_dir, "ikigega.json"), auto_unbox = TRUE, pretty = FALSE
    )
  }
  }, error = function(e) message("bridge json: ", conditionMessage(e)))
  invisible(NULL)
}
