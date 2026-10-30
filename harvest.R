# Umuganura — harvest (Umusaruro), stock (Ikigega) and Amakuru data

hv_num <- function(x) suppressWarnings(as.numeric(gsub("[ ,%]", "", as.character(x))))

hv_district <- function(x) {
  ref <- names(province_of)
  k <- gsub("[^a-z]", "", tolower(x))
  out <- ref[match(k, gsub("[^a-z]", "", tolower(ref)))]
  out[is.na(out) & grepl("^(rwanda|total|national|overall)", k)] <- "Rwanda"
  out
}

sas_canon <- function(x) {
  k <- gsub("[^a-z]", "", tolower(x))
  out <- rep(NA_character_, length(k))
  rules <- c(
    "Maize" = "^maize$", "Sorghum" = "^sorghum$", "Paddy rice" = "^(paddy)?rice$", "Wheat" = "^wheat$",
    "Cassava" = "^cassava$", "Sweet potato" = "^sweetpotato(es)?$", "Irish potato" = "^irishpotato(es)?$",
    "Yams & Taro" = "^(yams?taro|taroyams?)$", "Cooking banana" = "^cookingbanana$",
    "Bean" = "^beans?$", "Bush bean" = "^bushbeans?$", "Climbing bean" = "^climbingbeans?$",
    "Pea" = "^peas?$", "Groundnuts" = "^groundnuts?$", "Soybean" = "^soybeans?$",
    "Vegetables" = "^vegetables$", "Fruits" = "^fruits$"
  )
  for (nm in names(rules)) out[is.na(out) & grepl(rules[[nm]], k)] <- nm
  out
}

sas_root <- file.path("dataset hachthon", "CLEANED", "nisr_sas_tables")

# District × crop tables carry no titles; each was matched by its national maize figure.
sas_reg <- utils::read.csv(text = "year,season,kind,file
2019,A,yield,SAS 2019 _Annual report _Tables__Table_13.csv
2019,B,yield,SAS 2019 _Annual report _Tables__Table_14.csv
2019,A,prod,SAS 2019 _Annual report _Tables__Table_18.csv
2019,B,prod,SAS 2019 _Annual report _Tables__Table_19.csv
2020,A,yield,USAS 2020 _Final_Tables__Table_14.csv
2020,B,yield,USAS 2020 _Final_Tables__Table_15_.csv
2020,A,prod,USAS 2020 _Final_Tables__Table_20.csv
2020,B,prod,USAS 2020 _Final_Tables__Table_21.csv
2021,B,yield,SAS 2021_Tables__Table_18_.csv
2021,B,prod,SAS 2021_Tables__Table_25.csv
2022,A,yield,SAS 2022_Tables_0__Table_17.csv
2022,B,yield,SAS 2022_Tables_0__Table_18_.csv
2022,A,prod,SAS 2022_Tables_0__Table_23.csv
2022,B,prod,SAS 2022_Tables_0__Table_24.csv
2023,A,yield,SAS 2023_Annual Tables (3)__Table_16.csv
2023,B,yield,SAS 2023_Annual Tables (3)__Table_17.csv
2024,A,yield,SAS 2024_Annual Tables__Table_16.csv
2024,B,yield,SAS 2024_Annual Tables__Table_17.csv
2024,A,prod,SAS 2024_Annual Tables__Table_21.csv
2024,B,prod,SAS 2024_Annual Tables__Table_22.csv
2025,A,yield,SAS 2025_Annual Tables (1)__Table_19.csv
2025,B,yield,SAS 2025_Annual Tables (1)__Table_20.csv
2025,A,prod,SAS 2025_Annual Tables (1)__Table_24.csv
2025,B,prod,SAS 2025_Annual Tables (1)__Table_25.csv
2026,A,yield,Tables_2026 Season A__Table_13.csv
2026,A,prod,Tables_2026 Season A__Table_15.csv
2019,A,use,SAS 2019 _Annual report _Tables__Table_21.csv
2019,B,use,SAS 2019 _Annual report _Tables__Table_22.csv
2019,C,use,SAS 2019 _Annual report _Tables__Table_23.csv
2020,A,use,USAS 2020 _Final_Tables___Table_23.csv
2020,B,use,USAS 2020 _Final_Tables__Table_24.csv
2020,C,use,USAS 2020 _Final_Tables__Table_25.csv
2021,B,use,SAS 2021_Tables__Table_28.csv
2021,C,use,SAS 2021_Tables__Table_29.csv
2022,A,use,SAS 2022_Tables_0___Table_26.csv
2022,B,use,SAS 2022_Tables_0__Table_27.csv
2022,C,use,SAS 2022_Tables_0__Table_28.csv
2023,A,use,SAS 2023_Annual Tables (3)__Table_25.csv
2023,B,use,SAS 2023_Annual Tables (3)__Table_26.csv
2023,C,use,SAS 2023_Annual Tables (3)__Table_27.csv
2024,A,use,SAS 2024_Annual Tables__Table_24.csv
2024,B,use,SAS 2024_Annual Tables__Table_25.csv
2024,C,use,SAS 2024_Annual Tables__Table_26.csv
2025,A,use,SAS 2025_Annual Tables (1)__Table_27.csv
2025,B,use,SAS 2025_Annual Tables (1)__Table_28.csv
2025,C,use,SAS 2025_Annual Tables (1)__Table_29.csv
2026,A,use,Tables_2026 Season A__Table_16.csv
", stringsAsFactors = FALSE)
sas_reg$path <- file.path(sas_root, sas_reg$year, sas_reg$file)
sas_reg <- sas_reg[file.exists(sas_reg$path), , drop = FALSE]

read_sas_dc <- function(path) {
  x <- tryCatch(utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(x) || ncol(x) < 3) return(NULL)
  d <- hv_district(x[[1]])
  canon <- sas_canon(names(x))
  cols <- which(!is.na(canon) & !duplicated(canon))
  rows <- which(!is.na(d) & !duplicated(d))
  if (!length(cols) || !length(rows)) return(NULL)
  do.call(rbind, lapply(cols, function(j) data.frame(district = d[rows], crop = canon[j], value = hv_num(x[rows, j]), stringsAsFactors = FALSE)))
}

read_sas_use <- function(path) {
  x <- tryCatch(utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(x) || ncol(x) < 5) return(NULL)
  nm <- tolower(names(x))
  col <- function(p) { j <- grep(p, nm)[1]; if (is.na(j)) rep(NA_real_, nrow(x)) else hv_num(x[[j]]) }
  crop <- sas_canon(x[[1]])
  out <- data.frame(
    crop = crop, sold = col("^sold"), consumed = col("consumption"), seeds = col("^seed"),
    stored = col("^stored"), lost = col("loss"), stringsAsFactors = FALSE
  )
  out[!is.na(out$crop) & !duplicated(out$crop), , drop = FALSE]
}

read_sas_wide <- function(file, cols, skip_sub = TRUE) {
  path <- file.path(sas_root, "2026", file)
  if (!file.exists(path)) return(NULL)
  x <- tryCatch(utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(x)) return(NULL)
  d <- hv_district(x[[1]])
  keep <- which(!is.na(d) & !duplicated(d))
  out <- data.frame(district = d[keep], stringsAsFactors = FALSE)
  for (nm in names(cols)) out[[nm]] <- hv_num(x[keep, cols[[nm]]])
  out
}

harvest_sig <- local({
  extra <- c(
    file.path(data_dir, "fao_rwanda_crops_livestock_production.csv"),
    file.path(data_dir, "rwanda_monthly_climate_1961_2026.csv"),
    file.path(data_dir, "fao_rwanda_food_balances.csv"),
    file.path(sas_root, "2026", c("Tables_2026 Season A__Table_17.csv", "Tables_2026 Season A__Table_22.csv",
                                  "Tables_2026 Season A__Table_24.csv", "Tables_2026 Season A__Table_25.csv",
                                  "Tables_2026 Season A__Table_26.csv", "Tables_2026 Season A__Table_31.csv",
                                  "Tables_2026 Season A__Table_32.csv", "Tables_2026 Season A__Table_35.csv"))
  )
  f <- c(sas_reg$path, extra[file.exists(extra)])
  i <- file.info(f)
  paste(c("harvest-v3", basename(f), i$size, as.numeric(i$mtime), deparse(sas_canon), deparse(read_sas_dc), deparse(read_sas_use)), collapse = "|")
})

harvest_data <- local({
  old <- read_rds_cache("harvest.rds", harvest_sig)
  if (!is.null(old)) return(old$value)

  pull <- function(kind) {
    r <- sas_reg[sas_reg$kind == kind, , drop = FALSE]
    out <- lapply(seq_len(nrow(r)), function(i) {
      t <- if (kind == "use") read_sas_use(r$path[i]) else read_sas_dc(r$path[i])
      if (is.null(t) || !nrow(t)) return(NULL)
      cbind(year = r$year[i], season = r$season[i], t, stringsAsFactors = FALSE)
    })
    do.call(rbind, out)
  }
  yld <- pull("yield")
  yld <- yld[is.finite(yld$value) & yld$value > 0, , drop = FALSE]
  prd <- pull("prod")
  prd <- prd[is.finite(prd$value) & prd$value >= 0, , drop = FALSE]
  use <- pull("use")

  # National seasonal yield: the published Rwanda row, else production over implied area.
  nat <- yld[yld$district == "Rwanda", , drop = FALSE]
  dis <- yld[yld$district != "Rwanda", , drop = FALSE]
  key <- unique(dis[, c("year", "season", "crop")])
  have <- paste(nat$year, nat$season, nat$crop)
  add <- do.call(rbind, lapply(seq_len(nrow(key)), function(i) {
    k <- key[i, ]
    if (paste(k$year, k$season, k$crop) %in% have) return(NULL)
    y <- dis[dis$year == k$year & dis$season == k$season & dis$crop == k$crop, , drop = FALSE]
    p <- prd[prd$year == k$year & prd$season == k$season & prd$crop == k$crop & prd$district != "Rwanda", , drop = FALSE]
    m <- merge(y, p, by = "district", suffixes = c("_y", "_p"))
    m <- m[m$value_y > 0 & m$value_p > 0, , drop = FALSE]
    v <- if (nrow(m) >= 5) sum(m$value_p) * 1000 / sum(m$value_p * 1000 / m$value_y) else stats::median(y$value)
    data.frame(year = k$year, season = k$season, district = "Rwanda", crop = k$crop, value = v, stringsAsFactors = FALSE)
  }))
  yld <- rbind(dis, nat, add)

  prd_nat <- stats::aggregate(value ~ year + season + crop, data = prd[prd$district != "Rwanda", , drop = FALSE], FUN = sum)

  fao_raw <- utils::read.csv(file.path(data_dir, "fao_rwanda_crops_livestock_production.csv"), stringsAsFactors = FALSE, check.names = FALSE)
  fao_raw <- fao_raw[is.finite(fao_raw$Value), c("Item Code (CPC)", "Item", "Element", "Unit", "Year", "Value")]
  names(fao_raw) <- c("cpc", "item", "element", "unit", "year", "value")
  ufac <- c("hg/ha" = 0.1, "100 g/ha" = 0.1, "kg/ha" = 1, "t/ha" = 1000, "hg/An" = 0.1, "100 g/An" = 0.1,
            "kg/An" = 1, "g/An" = 0.001, "0.1 g/An" = 1e-4, "100 mg/An" = 1e-4)
  fy <- fao_raw$element %in% c("Yield", "Yield/Carcass Weight")
  f <- ifelse(fao_raw$unit %in% names(ufac), ufac[fao_raw$unit], 1)
  fao_raw$value[fy] <- fao_raw$value[fy] * f[fy]
  fao_raw$unit[fy] <- ifelse(grepl("/An", fao_raw$unit[fy]), "kg per animal", ifelse(fao_raw$unit[fy] %in% names(ufac), "kg/ha", fao_raw$unit[fy]))
  fao_keep <- fao_raw[fao_raw$element %in% c("Production", "Area harvested", "Yield", "Yield/Carcass Weight", "Producing Animals/Slaughtered", "Milk Animals", "Laying", "Stocks"), , drop = FALSE]

  ah <- fao_raw[fao_raw$element == "Area harvested" & !grepl("^F", fao_raw$cpc), , drop = FALSE]
  diversity <- do.call(rbind, lapply(split(ah, ah$year), function(d) {
    p <- d$value / sum(d$value)
    data.frame(year = d$year[1], value = 1 - sum(p^2))
  }))

  cl <- utils::read.csv(file.path(data_dir, "rwanda_monthly_climate_1961_2026.csv"), stringsAsFactors = FALSE)
  cl <- cl[is.finite(cl$precip_mm), , drop = FALSE]
  full <- stats::aggregate(month ~ location + year, cl, length)
  full <- full[full$month == 12, c("location", "year")]
  cl <- merge(cl, full, by = c("location", "year"))
  rain_loc <- stats::aggregate(precip_mm ~ location + year, cl, sum)
  rain <- stats::aggregate(precip_mm ~ year, rain_loc, mean)
  names(rain) <- c("year", "value")
  temp <- stats::aggregate(temp_c_mean ~ year, cl, mean)
  names(temp) <- c("year", "value")

  fb <- utils::read.csv(file.path(data_dir, "fao_rwanda_food_balances.csv"), stringsAsFactors = FALSE, check.names = FALSE)
  stockvar <- fb[fb$Element == "Stock Variation" & is.finite(fb$Value), c("Item", "Year", "Value")]
  names(stockvar) <- c("item", "year", "value")
  stockvar$value <- stockvar$value * 1000

  practice <- list(
    land = read_sas_wide("Tables_2026 Season A__Table_31.csv", list(erosion = 2, mech = 5, irrig = 8, agrofor = 11)),
    erosion = read_sas_wide("Tables_2026 Season A__Table_35.csv", list(severe = 2, moderate = 3, low = 4, vlow = 5)),
    inorganic = read_sas_wide("Tables_2026 Season A__Table_25.csv", list(farmers = 2, plots = 5, land = 8)),
    organic = read_sas_wide("Tables_2026 Season A__Table_24.csv", list(farmers = 2, plots = 5, land = 8)),
    seed_src = read_sas_wide("Tables_2026 Season A__Table_22.csv", list(gov = 2, multipliers = 3, dealers = 4, ngo = 5, market = 6, coop = 7)),
    fert_src = read_sas_wide("Tables_2026 Season A__Table_26.csv", list(gov = 2, dealers = 3, ngo = 4, market = 5, coop = 6)),
    system = read_sas_wide("Tables_2026 Season A__Table_17.csv", list(pure = 2, mixed = 3)),
    irr_tech = read_sas_wide("Tables_2026 Season A__Table_32.csv", list(surface = 2, flood = 3, drip = 4, sprinkler = 5, pivot = 6, traditional = 7))
  )

  value <- list(yield = yld, prod = prd, prod_nat = prd_nat, use = use, fao = fao_keep,
                diversity = diversity, rain = rain, temp = temp, stockvar = stockvar, practice = practice)
  write_rds_cache("harvest.rds", list(sig = harvest_sig, value = value))
  value
})

sas_yield <- harvest_data$yield
sas_prod <- harvest_data$prod
sas_prod_nat <- harvest_data$prod_nat
sas_use <- harvest_data$use
fao_series <- harvest_data$fao
farm_practice <- harvest_data$practice

district_lnglat <- local({
  geo_df <- get_geo_df()
  if (!nrow(geo_df)) return(list())
  out <- lapply(split(geo_df, geo_df$district), function(g) {
    parts <- split(g, g$piece)
    big <- parts[[which.max(vapply(parts, nrow, integer(1)))]]
    list(
      lng = unlist(lapply(parts, function(p) c(p$lon, NA)), use.names = FALSE),
      lat = unlist(lapply(parts, function(p) c(p$lat, NA)), use.names = FALSE),
      cx = mean(range(big$lon)), cy = mean(range(big$lat))
    )
  })
  out
})

# ---- Product catalogue -------------------------------------------------------

harvest_cats <- c(
  "cereals and tubers", "pulses and nuts", "vegetables and fruits",
  "miscellaneous food", "non-food", "meat, fish and eggs", "milk and dairy"
)

cp <- function(id, en, rw, cat, img, fao, sas = NA, use = NA, removal = NA, kind = "crop",
               cycle = "", origin = "", arrived = "", methods = "") {
  data.frame(id = id, en = en, rw = rw, cat = cat, img = img, fao = fao, sas = sas, use = use,
             removal = removal, kind = kind, cycle = cycle, origin = origin, arrived = arrived,
             methods = methods, stringsAsFactors = FALSE)
}

crop_catalog <- rbind(
  cp("maize", "Maize", "Ibigori", "cereals and tubers", "maize.png", "Maize (corn)", "Maize", "Maize", 25,
     cycle = "Season A: sow Sep–Oct, harvest Jan–Feb. Season B: sow Feb–Mar, harvest Jun–Jul. 4–5 months.",
     origin = "Domesticated from wild teosinte in southern Mexico about 9,000 years ago.",
     arrived = "Reached East Africa on Indian Ocean and Portuguese trade routes around the 1600s. A priority crop of the Crop Intensification Program since 2007.",
     methods = "Row planting at 75 × 25 cm|Hybrid or improved seed from agro-dealers|DAP or NPK at planting, urea top-dress at knee height|Land consolidation: neighbours plant the same crop"),
  cp("rice", "Rice", "Umuceri", "cereals and tubers", "rice.png", "Rice", "Paddy rice", "Paddy rice", 22,
     cycle = "Marshland crop in Seasons A and B. 4–5 months from transplanting.",
     origin = "Asian rice was domesticated in the Yangtze valley of China about 9,000 years ago.",
     arrived = "Brought into Rwanda's marshlands in the 1960s with Chinese and Taiwanese technical missions.",
     methods = "Seedlings raised in nurseries, transplanted into flooded plots|Canals control water in developed marshlands|Urea and NPK in split doses|Cooperatives run the water and the drying floors"),
  cp("sorghum", "Sorghum", "Amasaka", "cereals and tubers", "sorghum.png", "Sorghum", "Sorghum", "Sorghum", 25,
     cycle = "Mostly Season A. 5–6 months.",
     origin = "Domesticated in north-east Africa (Sudan and Ethiopia) about 5,000 years ago.",
     arrived = "One of Rwanda's oldest crops. Its first grains are shared at the Umuganura first-fruits festival.",
     methods = "Broadcast or row sowing on hillsides|Tolerates the drier eastern soils|Often intercropped with beans|Bird scaring before harvest"),
  cp("wheat", "Wheat", "Ingano", "cereals and tubers", "wheat.png", "Wheat", "Wheat", "Wheat", 34,
     cycle = "Mostly Season B in the cool north. About 4 months.",
     origin = "Domesticated in the Fertile Crescent about 10,000 years ago.",
     arrived = "Brought to the northern highlands in the colonial era. Grown in Musanze, Burera, Nyabihu and Gicumbi.",
     methods = "Row drilling on land above 1,800 m|Rust-resistant improved varieties|NPK at sowing, urea top-dress|Threshing and drying on tarpaulins"),
  cp("finger_millet", "Finger millet", "Uburo", "cereals and tubers", "finger millet.png", "Millet", NA, NA, 30,
     cycle = "Season A. 4–5 months.",
     origin = "Domesticated in the East African highlands (Uganda and Ethiopia) about 5,000 years ago.",
     arrived = "An ancient Rwandan grain. Millet bread is part of the Umuganura harvest feast.",
     methods = "Broadcast sowing on fine seedbeds|Hand weeding and thinning|Heads cut one by one at harvest|Stores for years with little loss"),
  cp("cassava", "Cassava", "Imyumbati", "cereals and tubers", "cassava.png", "Cassava, fresh", "Cassava", "Cassava", 8,
     cycle = "Planted in Season A or B. Roots harvested 9–18 months later.",
     origin = "Domesticated in south-western Amazonia (Brazil) about 8,000 years ago.",
     arrived = "Portuguese traders brought it to Central Africa in the 1500s. It reached Rwanda by the 1800s.",
     methods = "Stem cuttings of 20–25 cm on ridges or mounds|Clean cuttings against mosaic and brown streak|Harvest 9–18 months after planting|Peel, soak and dry for flour"),
  cp("sweet_potato", "Sweet potato", "Ibijumba", "cereals and tubers", "sweet potatoes.png", "Sweet potatoes", "Sweet potato", "Sweet potato", 11,
     cycle = "All three seasons, including marshlands in Season C. 4–5 months.",
     origin = "Domesticated in Central or South America at least 5,000 years ago.",
     arrived = "Reached East Africa through Portuguese trade in the 1500s–1600s. A hunger-gap food in Rwanda.",
     methods = "Vine cuttings planted on mounds or ridges|Orange-fleshed varieties for vitamin A|Piecemeal harvest as roots are needed|Marshland planting in Season C"),
  cp("irish_potato", "Irish potato", "Ibirayi", "cereals and tubers", "irish potatoes.png", "Potatoes", "Irish potato", "Irish potato", 11,
     cycle = "Seasons A and B on the volcanic north. 3–4 months.",
     origin = "Domesticated in the Andes of Peru and Bolivia about 8,000 years ago.",
     arrived = "Introduced by German and Belgian missionaries in the early 1900s. Musanze, Nyabihu, Rubavu and Burera are now the potato belt.",
     methods = "Certified seed tubers on ridges|Fungicide sprays against late blight|Earthing up to cover tubers|Dark, aired stores after harvest"),
  cp("taro", "Taro", "Amateke", "cereals and tubers", "taro.jpg", "Taro", "Yams & Taro", "Yams & Taro", 10,
     cycle = "Wet valley soils. Corms ready after 6–12 months.",
     origin = "Domesticated in South-East Asia and New Guinea at least 7,000 years ago.",
     arrived = "Came to East Africa over a thousand years ago on Indian Ocean trade. Grown in moist valley bottoms.",
     methods = "Corms or suckers planted in wet soil|Mulch to hold moisture|Harvest after 6–12 months"),
  cp("yam", "Yam", "Ibikoro", "cereals and tubers", "yams.jpg", "Yams", "Yams & Taro", "Yams & Taro", 10,
     cycle = "Tubers ready after 8–10 months.",
     origin = "Domesticated separately in West Africa and South-East Asia several thousand years ago.",
     arrived = "An old Rwandan garden crop, grown on stakes around the homestead.",
     methods = "Plant seed yams or tuber pieces|Stake the vines|Harvest after 8–10 months"),
  cp("cooking_banana", "Cooking banana", "Ibitoki", "cereals and tubers", "cooking banana.jpg", "Bananas", "Cooking banana", "Cooking banana", 8,
     cycle = "Perennial. First bunch 12–15 months after planting, then year round.",
     origin = "Bananas were domesticated in New Guinea about 7,000 years ago.",
     arrived = "East African highland bananas have grown around the Great Lakes for more than 1,000 years.",
     methods = "Suckers planted in pits filled with manure|Mulch, de-leaf, keep three stems per mat|Cut the male bud against Xanthomonas wilt|Harvest bunches year round"),
  cp("beans", "Beans", "Ibishyimbo", "pulses and nuts", "beans.png", "Beans, dry", "Bean", "Bush bean", 59,
     cycle = "Seasons A and B. About 3 months.",
     origin = "Common beans were domesticated in Mesoamerica and the Andes about 7,000 years ago.",
     arrived = "Reached Africa in the 1500s. Rwanda now eats more beans per person than almost any country and leads in climbing beans.",
     methods = "Climbing beans on stakes on small hillside plots|Bush beans intercropped with maize or banana|Iron-rich biofortified varieties|Root nodules fix nitrogen from the air"),
  cp("peas", "Peas", "Amashaza", "pulses and nuts", "peas.png", "Peas, dry", "Pea", "Pea", 57,
     cycle = "Cool highlands. About 3 months.",
     origin = "Domesticated in the Near East about 10,000 years ago.",
     arrived = "Grown in Rwanda's cool highlands since the colonial era.",
     methods = "Row sowing on high-altitude plots|Staking for climbing types|Root nodules fix nitrogen"),
  cp("soya_bean", "Soya bean", "Soya", "pulses and nuts", "soya bean.jpg", "Soya beans", "Soybean", "Soybean", 94,
     cycle = "Seasons A and B. 3–4 months.",
     origin = "Domesticated in north-east China 3,000–5,000 years ago.",
     arrived = "Promoted in Rwanda from the 1980s for protein and oil. Now a key animal-feed crop.",
     methods = "Row planting with Rhizobium inoculant|Phosphorus at planting|Rotated after maize"),
  cp("groundnut", "Groundnut", "Ubunyobwa", "pulses and nuts", "groundnut.jpg", "Groundnuts, excluding shelled", "Groundnuts", "Groundnuts", 66,
     cycle = "Warm eastern and south-western lowlands. 3–4 months.",
     origin = "Domesticated in South America (Bolivia and Argentina) about 7,500 years ago.",
     arrived = "Brought to Africa by Portuguese traders in the 1500s.",
     methods = "Shelled seed planted in rows on sandy loam|Lime or gypsum for pod filling|Dry pods fast against aflatoxin"),
  cp("avocado", "Avocado", "Avoka", "vegetables and fruits", "avocado.jpg", "Avocados", "Fruits", "Fruits", 10,
     cycle = "Perennial tree. Fruits from year 3–4.",
     origin = "Domesticated in south-central Mexico about 5,000 years ago.",
     arrived = "Planted around Rwandan homesteads through the 1900s. Hass exports grew after 2015.",
     methods = "Grafted seedlings in large pits with manure|Trees 7–8 m apart|Pick mature fruit and ripen it off the tree"),
  cp("passion_fruit", "Passion fruit", "Marakuja", "vegetables and fruits", "passion fruit.jpg", "Other tropical fruits, n.e.c.", "Fruits", "Fruits", 10,
     cycle = "Vine. Fruits 6–9 months after planting.",
     origin = "Native to southern Brazil, Paraguay and northern Argentina.",
     arrived = "Spread across East Africa in the 1900s. Now a Rwandan horticulture export.",
     methods = "Vines trained on wire trellis|Pruning and hand pollination|Collect ripe fallen fruit"),
  cp("plum", "Plum", "", "vegetables and fruits", "plum.jpg", "Plums and sloes", "Fruits", "Fruits", 5,
     cycle = "Perennial tree in cool highlands.",
     origin = "Domesticated in western Asia and China several thousand years ago.",
     arrived = "Grown in small numbers in Rwanda's cool highlands.",
     methods = "Grafted trees|Varieties that need little winter chill"),
  cp("tomato", "Tomato", "Inyanya", "vegetables and fruits", "tomato.jpg", "Tomatoes", "Vegetables", "Vegetables", 7,
     cycle = "Any season with water. 3–4 months.",
     origin = "Domesticated in western South America and Mexico about 2,500 years ago.",
     arrived = "Came with colonial-era market gardens. Now grown near Kigali, in the east, and in greenhouses.",
     methods = "Nursery seedlings transplanted|Staking and pruning|Sprays against blight and Tuta absoluta|Drip irrigation or greenhouse"),
  cp("onion", "Onion", "Igitunguru", "vegetables and fruits", "onion.jpg", "Onions and shallots, dry (excluding dehydrated)", "Vegetables", "Vegetables", 6,
     cycle = "4–5 months from transplanting.",
     origin = "Domesticated in Central Asia about 5,000 years ago.",
     arrived = "Grown in Rwandan market gardens since the colonial era. Red onion leads.",
     methods = "Nursery seedlings transplanted|Raised beds|Cure bulbs in shade before storage"),
  cp("carrot", "Carrot", "Karoti", "vegetables and fruits", "carrot.jpg", "Carrots and turnips", "Vegetables", "Vegetables", 6,
     cycle = "Cool highlands. About 3 months.",
     origin = "Domesticated in Persia (Iran and Afghanistan) more than 1,000 years ago.",
     arrived = "Grown in Rwanda's cool highlands for Kigali markets.",
     methods = "Direct sowing in deep, loose beds|Thinning|Even watering"),
  cp("sweet_pepper", "Sweet pepper", "Puwavuro", "vegetables and fruits", "sweet pepper.jpg", "Chillies and peppers, green (Capsicum spp. and Pimenta spp.)", "Vegetables", "Vegetables", 9,
     cycle = "3–4 months from transplanting.",
     origin = "Capsicum peppers were domesticated in Mexico about 6,000 years ago.",
     arrived = "A newer Rwandan horticulture crop, often grown in greenhouses for hotels and export.",
     methods = "Nursery seedlings|Greenhouse or open field|Staking and drip irrigation"),
  cp("sugarcane", "Sugarcane", "Ibisheke", "miscellaneous food", "sugarcane.jpg", "Sugar cane", NA, NA, 4,
     cycle = "12–18 months to the first cut, then ratoon crops.",
     origin = "Domesticated in New Guinea about 8,000 years ago.",
     arrived = "Grown in Rwanda's marshlands. The Kabuye estate supplies Rwandan sugar.",
     methods = "Stem setts planted in furrows|Ratoon crops regrow after each cut|Mill within a day of cutting"),
  cp("coffee", "Coffee", "Ikawa", "miscellaneous food", "coffee.png", "Coffee, green", NA, NA, 86,
     cycle = "Perennial shrub. First crop in year 3. Harvest March–July.",
     origin = "Arabica coffee comes from the forests of south-western Ethiopia.",
     arrived = "German missionaries planted it at Mibirizi in 1904. Specialty washed coffee since the 2000s.",
     methods = "Shade-grown arabica on hillsides|Mulching and pruning|Pick only ripe red cherries|Washing stations ferment and dry the beans"),
  cp("tea", "Tea", "Icyayi", "miscellaneous food", "tea.png", "Tea leaves", NA, NA, 78,
     cycle = "Perennial bush. Plucked every 7–14 days all year.",
     origin = "Native to south-western China and north-eastern India.",
     arrived = "First planted in Rwanda in 1952 at Mulindi. Now a leading export crop.",
     methods = "Clonal cuttings on high, acid soils|Pluck two leaves and a bud|Factories wither, roll, ferment and dry"),
  cp("pyrethrum", "Pyrethrum", "Ibireti", "non-food", "pyrethrum.png", "Pyrethrum, dried flowers", NA, NA, 40,
     cycle = "Perennial. Flowers picked every 2–3 weeks.",
     origin = "Native to the Dalmatian coast of the Balkans.",
     arrived = "Introduced on the Virunga volcano slopes in the 1930s. Its dried flowers make a natural insecticide.",
     methods = "Split plants on volcanic soils above 2,000 m|Pick open flowers every 2–3 weeks|Dry flowers quickly"),
  cp("beef", "Beef", "Inyama z'inka", "meat, fish and eggs", "beef.jpg", "Meat of cattle with the bone, fresh or chilled", kind = "livestock",
     cycle = "Cattle reach slaughter weight in 2–4 years.",
     origin = "Cattle were domesticated in the Near East about 10,500 years ago.",
     arrived = "Long-horned Ankole cattle have been part of Rwandan life for centuries. Girinka (one cow per poor family) since 2006.",
     methods = "Zero-grazing in stalls|Napier and Brachiaria fodder|Slaughter at licensed abattoirs"),
  cp("goat_meat", "Goat meat", "Inyama z'ihene", "meat, fish and eggs", "goat meat.jpg", "Meat of goat, fresh or chilled", kind = "livestock",
     cycle = "Ready for market at about 1 year.",
     origin = "Goats were domesticated in the Zagros Mountains of Iran about 10,000 years ago.",
     arrived = "Kept by most rural households as living savings.",
     methods = "Tethering or zero-grazing|Fodder trees and crop residues|Deworming and vaccination"),
  cp("sheep_meat", "Mutton", "Inyama z'intama", "meat, fish and eggs", "sheep meat.jpg", "Meat of sheep, fresh or chilled", kind = "livestock",
     cycle = "Ready for market at about 1 year.",
     origin = "Sheep were domesticated in Mesopotamia about 10,000 years ago.",
     arrived = "Kept mainly in the cool north and west highlands.",
     methods = "Grazing on hillside pasture|Night shelters|Deworming"),
  cp("pork", "Pork", "Inyama z'ingurube", "meat, fish and eggs", "pork.jpg", "Meat of pig with the bone, fresh or chilled", kind = "livestock",
     cycle = "Pigs reach market weight in 6–8 months.",
     origin = "Pigs were domesticated in Anatolia and China about 9,000 years ago.",
     arrived = "Pig farming grew fast in Rwanda after 2010 with the rise of pork restaurants.",
     methods = "Pens with concrete floors|Feed from maize bran and kitchen waste|Biosecurity against African swine fever"),
  cp("chicken_meat", "Chicken meat", "Inyama z'inkoko", "meat, fish and eggs", "chicken meat.jpg", "Meat of chickens, fresh or chilled", kind = "livestock",
     cycle = "Broilers ready in 6–8 weeks.",
     origin = "Chickens were domesticated from red junglefowl in South-East Asia about 3,500 years ago.",
     arrived = "Village chickens are old. Commercial broiler farms grew after 2010.",
     methods = "Deep-litter houses|Starter, grower and finisher feeds|Newcastle disease vaccination"),
  cp("eggs", "Eggs", "Amagi", "meat, fish and eggs", "eggs.jpg", "Hen eggs in shell, fresh", kind = "livestock",
     cycle = "Layers start at about 5 months and lay for a year or more.",
     origin = "Chickens were domesticated in South-East Asia about 3,500 years ago.",
     arrived = "Layer farms around Kigali and in the east supply most market eggs.",
     methods = "Cage or deep-litter layer houses|Layer mash with calcium|Light and water every day"),
  cp("milk", "Milk", "Amata", "milk and dairy", "milk.jpg", "Raw milk of cattle", kind = "livestock",
     cycle = "Daily. A lactation lasts about 300 days.",
     origin = "Cattle were domesticated in the Near East about 10,500 years ago.",
     arrived = "Milk has deep cultural roots in Rwanda. Girinka and milk collection centres widened supply after 2006.",
     methods = "Zero-grazing with planted fodder|Crossbred Friesian and Jersey cows|Milk collection centres keep milk cold")
)
crop_catalog$label <- ifelse(nzchar(crop_catalog$rw), paste0(crop_catalog$en, " / ", crop_catalog$rw), crop_catalog$en)
# Always forward-slash URLs (file.path on Windows breaks <img src>).
crop_catalog$src <- paste0("screenshoots/umusaruro/", gsub(" ", "%20", crop_catalog$img, fixed = TRUE))
crop_catalog$thumb <- paste0("umusaruro/thumb/", crop_catalog$id, ".jpg")
crop_catalog$cat_n <- match(crop_catalog$cat, harvest_cats)

crop_row <- function(id) {
  r <- crop_catalog[crop_catalog$id == id, , drop = FALSE]
  if (!nrow(r)) crop_catalog[1, , drop = FALSE] else r[1, , drop = FALSE]
}

fao_get <- function(item, element) {
  if (is.na(item)) return(data.frame(year = integer(), value = numeric()))
  d <- fao_series[fao_series$item == item & fao_series$element %in% element, c("year", "value", "unit"), drop = FALSE]
  d[order(d$year), , drop = FALSE]
}

crop_yield_nat <- function(id) {
  r <- crop_row(id)
  el <- if (r$kind == "livestock") c("Yield/Carcass Weight", "Yield", "Production") else "Yield"
  for (e in el) {
    d <- fao_get(r$fao, e)
    if (nrow(d)) break
  }
  d[!duplicated(d$year), , drop = FALSE]
}

crop_unit <- function(id) {
  r <- crop_row(id)
  d <- crop_yield_nat(id)
  if (r$kind == "crop") "kg/ha" else if (nrow(d)) d$unit[1] else "t"
}

nutrient_use <- function() {
  f <- fert[is.finite(fert$value), , drop = FALSE]
  if (!nrow(f)) return(list(value = NA_real_, year = NA))
  list(value = f$value[nrow(f)], year = f$year[nrow(f)])
}

soil_wear <- function(id) {
  r <- crop_row(id)
  y <- crop_yield_nat(id)
  y <- y[is.finite(y$value), , drop = FALSE]
  if (r$kind != "crop" || !nrow(y) || !is.finite(r$removal)) return(NULL)
  last <- y[nrow(y), ]
  removed <- last$value / 1000 * r$removal
  fu <- nutrient_use()
  er <- farm_practice$erosion
  er_nat <- if (!is.null(er) && nrow(er)) {
    rw <- er[er$district == "Rwanda", , drop = FALSE]
    if (nrow(rw)) rw$severe + rw$moderate else mean(er$severe + er$moderate, na.rm = TRUE)
  } else NA_real_
  list(
    year = last$year, yield = last$value, removed = removed,
    fert = fu$value, fert_year = fu$year,
    index = if (is.finite(fu$value) && fu$value > 0) removed / fu$value else NA_real_,
    erosion = er_nat,
    legume = r$cat == "pulses and nuts"
  )
}

# ---- Stock (Ikigega) ---------------------------------------------------------

nsr_floor <- 46000
nsr_crops <- c("Maize", "Sorghum", "Paddy rice", "Wheat", "Bush bean", "Climbing bean", "Pea", "Soybean", "Groundnuts")

stock_mass <- local({
  u <- sas_use[sas_use$crop %in% c(nsr_crops, "Irish potato", "Sweet potato", "Cassava", "Cooking banana", "Vegetables", "Fruits", "Yams & Taro"), , drop = FALSE]
  m <- merge(u, sas_prod_nat, by = c("year", "season", "crop"))
  names(m)[names(m) == "value"] <- "prod"
  m$stored_mt <- m$prod * m$stored / 100
  m$sold_mt <- m$prod * m$sold / 100
  m$lost_mt <- m$prod * m$lost / 100
  m[order(m$year, m$season, m$crop), , drop = FALSE]
})

stock_years <- sort(unique(stock_mass$year))

coop_reach <- local({
  s <- farm_practice$seed_src
  f <- farm_practice$fert_src
  if (is.null(s) || is.null(f)) return(data.frame(district = character(), seed = numeric(), fert = numeric()))
  m <- merge(s[, c("district", "coop")], f[, c("district", "coop")], by = "district", all = TRUE, suffixes = c("_seed", "_fert"))
  names(m) <- c("district", "seed", "fert")
  m
})

cold_template <- data.frame(
  lot_id = character(), product = character(), received_at = character(),
  dispatched_at = character(), temp_c = numeric(), stringsAsFactors = FALSE
)

# ---- Amakuru topics ----------------------------------------------------------

amakuru_topics <- c(
  irrigation = "Small-scale irrigation",
  soil = "Soil conservation and fertility management",
  postharvest = "Post-harvest collection and storage",
  value = "Agricultural productivity, processing and value addition",
  export = "Export market opportunities for Rwandan agricultural products"
)

nat_or_mean <- function(df, col) {
  if (is.null(df) || !nrow(df)) return(NA_real_)
  r <- df[df$district == "Rwanda", col]
  if (length(r) && is.finite(r[1])) r[1] else mean(df[df$district != "Rwanda", col], na.rm = TRUE)
}
