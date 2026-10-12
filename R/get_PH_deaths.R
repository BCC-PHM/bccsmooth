#' Fetch Birmingham deaths from the PH_Deaths warehouse
#'
#' Rows are selected on the warehouse's own `FinYear` field, which is derived
#' from the date of death, so a period covers deaths that occurred between
#' 1 April and the following 31 March. Deaths are coded to 2011 LSOAs and are
#' reallocated onto 2021 LSOAs using area-intersection weights, which makes the
#' death counts fractional.
#'
#' @param financial_year Character vector of financial years in the warehouse's
#'   own format, e.g. "2025/2026". Multiple years may be supplied.
#' @param diagnostic_code Character vector of ICD-10 patterns for the underlying
#'   cause, e.g. c("I2%", "I6%"). `NULL` returns all causes.
#' @param age One comparison such as "<75", or two bounds such as c("15", "64").
#'   `NULL` returns all ages.
#' @param geo_level Geography to model at: "lsoa21" or "ward".
#' @param server,database,driver Connection details.
#' @returns A list with `death_data` (one row per death), `inla_dat` (one row per
#'   area and age band), `g` (the INLA adjacency graph), `map` (the boundaries)
#'   and `area_col` (the name of the area code column in `inla_dat`).
#' @importFrom dplyr tbl select filter mutate between case_when group_by summarise rename left_join distinct pull n
#' @importFrom tibble tibble
#' @importFrom sf st_drop_geometry
#' @importFrom spdep poly2nb nb2mat
#' @importFrom magrittr %>%
#' @export
get_ph_deaths = function(
    financial_year  = "2025/2026",
    diagnostic_code = NULL,
    age             = NULL,
    geo_level       = "lsoa21",
    server          = "SVWVSQ016\\SVWVSQ016",
    database        = "PH_Deaths",
    driver          = "ODBC Driver 17 for SQL Server"
) {
  #---------------------------------------------------------
  #INLA is suggested, not imported - fail early with help
  #---------------------------------------------------------
  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("Package 'INLA' is required but not installed.\n",
         "Install it with:\n",
         '  install.packages(\n',
         '    "INLA",\n',
         '    repos = c(getOption("repos"),\n',
         '              INLA = "https://inla.r-inla-download.org/R/stable"),\n',
         '    dep  = TRUE,\n',
         '    type = "binary"\n',
         '  )',
         call. = FALSE)
  }

  #---------------------------------------------------------
  #financial year labels must match the FinYear column
  #---------------------------------------------------------
  if (!length(financial_year) || anyNA(financial_year) ||
      any(!grepl("^[0-9]{4}/[0-9]{4}$", financial_year))) {
    stop('`financial_year` must match the FinYear column, e.g. "2025/2026".')
  }

  #---------------------------------------------------------
  #geography must be known before we hit the database
  #---------------------------------------------------------
  if (!tolower(geo_level) %in% c("lsoa21", "ward")) {
    stop(
      paste0(
        "Unexpected geo_level. Given '",
        geo_level,
        "'. Available geo_level values are lsoa21, ward."
      ))
  }

  #===========================================================
  #ordered age groups, shared throughout
  #===========================================================
  age_levels = c(
    "UNDER 1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
    "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
    "70-74","75-79","80-84","85-89","90+"
  )

  #---------------------------------------------------------
  #connect, and guarantee disconnect
  #---------------------------------------------------------
  con = DBI::dbConnect(odbc::odbc(), Driver = driver, Server = server,
                       Database = database, Trusted_Connection = "Yes")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  #===========================================================
  #age band lookup and boundary validation
  #===========================================================
  #-----------------------------------------------------
  #map every single year of age to its 5-year band
  #-----------------------------------------------------
  age_crossing = tibble(
    age_levels = c(
      rep("UNDER 1", 1),
      rep("1-4", 4),
      rep("5-9", 5),
      rep("10-14", 5),
      rep("15-19", 5),
      rep("20-24", 5),
      rep("25-29", 5),
      rep("30-34", 5),
      rep("35-39", 5),
      rep("40-44", 5),
      rep("45-49", 5),
      rep("50-54", 5),
      rep("55-59", 5),
      rep("60-64", 5),
      rep("65-69", 5),
      rep("70-74", 5),
      rep("75-79", 5),
      rep("80-84", 5),
      rep("85-89", 5),
      rep("90+", 31)
    ),
    single_age = 0:120
  )

  #-----------------------------------------------------
  #ages a band starts at - any other cut splits a band
  #-----------------------------------------------------
  valid_upper = c(1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90)

  age_script = NULL
  age_filter = age_levels

  if (length(age) && !identical(age, "")) {
    #-----------------------------------------------------
    #basic shape of the age argument
    #-----------------------------------------------------
    if (!is.character(age) || !length(age) %in% 1:2 || anyNA(age)) {
      stop('`age` must be one condition (e.g. "<75") or two bounds (e.g. c("15", "64")), or NULL.')
    }

    if (length(age) == 1) {
      #-----------------------------------------------------
      #one condition: split into operator and number
      #-----------------------------------------------------
      op = stringr::str_extract(age, "^[<>=]+")
      n  = as.numeric(stringr::str_extract(age, "\\d+"))
      if (is.na(op) || !op %in% c("<", ">", ">=", "<=") || is.na(n)) {
        stop('Could not parse `age`: "', age, '". Expected something like "<75".')
      }

      #-----------------------------------------------------
      #the cut must land on a band edge; collect the bands
      #-----------------------------------------------------
      if (op == "<" && n %in% valid_upper) {
        age_filter = age_crossing %>% filter(single_age < n) %>% distinct(age_levels) %>% pull(age_levels)
      } else if (op == "<=" && n + 1 %in% valid_upper) {
        age_filter = age_crossing %>% filter(single_age <= n) %>% distinct(age_levels) %>% pull(age_levels)
      } else if (op == ">=" && n %in% valid_upper) {
        age_filter = age_crossing %>% filter(single_age >= n) %>% distinct(age_levels) %>% pull(age_levels)
      } else if (op == ">" && n + 1 %in% valid_upper) {
        age_filter = age_crossing %>% filter(single_age > n) %>% distinct(age_levels) %>% pull(age_levels)
      } else {
        stop(paste0(
          "Age filter '", toString(age),
          "' would split an age band. Valid examples include ",
          "'<75', '<=74', '>=75', '>74', 'c(\"15\",\"64\")'."
        ))
      }
      age_script = paste0("AgeInYears ", op, " ", n)

    } else {
      #-----------------------------------------------------
      #two bounds: both ends must land on a band edge
      #-----------------------------------------------------
      n1 = as.numeric(stringr::str_extract(age[[1]], "\\d+"))
      n2 = as.numeric(stringr::str_extract(age[[2]], "\\d+"))
      if (is.na(n1) || is.na(n2)) {
        stop('Could not parse `age`: ', toString(age), '. Expected two numbers, e.g. c("15", "64").')
      }

      if (n2 > n1 && n1 %in% valid_upper && n2 + 1 %in% valid_upper) {
        age_filter = age_crossing %>% filter(single_age >= n1, single_age <= n2) %>%
          distinct(age_levels) %>% pull(age_levels)
      } else {
        stop(paste0(
          "Age filter '", toString(age),
          "' would split an age band. Valid examples include ",
          "'<75', '<=74', '>=75', '>74', 'c(\"15\",\"64\")'."
        ))
      }
      age_script = paste0("between(AgeInYears, ", n1, ", ", n2, ")")
    }

    #-----------------------------------------------------
    #one band gives the crude rate, not a standardised one
    #-----------------------------------------------------
    if (length(age_filter) < 2) {
      stop("Age filter '", toString(age), "' leaves ", length(age_filter),
           " age band(s). Age standardisation needs at least two.")
    }
  }

  #---------------------------------------------------------
  #query
  #---------------------------------------------------------
  out = dplyr::tbl(con, "tblMonthlyDeathsData_BCC") %>%
    dplyr::select(
      FinYear, Year, Month, MonthText,
      `ANONYMISED ID`, AGE, AgeInYears, SEX,
      `DATE OF DEATH OF DECEASED`,
      `DATE OF REGISTRATION`,
      `POSTCODE OF USUAL RESIDENCE OF DECEASED`,
      `LOWER SOA CODE OF USUAL RESIDENCE OF DECEASED`,
      `LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED`,
      `ELECTORAL WARD CODE OF USUAL RESIDENCE OF DECEASED`,
      `UNDERLYING CAUSE OF DEATH CODE`,
      `PLACE OF DEATH`
    ) %>%
    dplyr::filter(FinYear %in% financial_year) %>%
    dplyr::filter(`LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED` == "E08000025")

  if (!is.null(diagnostic_code)) {
    dx = paste0("`UNDERLYING CAUSE OF DEATH CODE` %LIKE% ",
                encodeString(diagnostic_code, quote = '"'), collapse = " | ")
    out = dplyr::filter(out, !!rlang::parse_expr(dx))
  }
  if (!is.null(age_script)) {
    out = dplyr::filter(out, !!rlang::parse_expr(age_script))
  }

  out = out %>%
    dplyr::collect() %>%
    dplyr::mutate(
      death_date = as.Date(sprintf("%08d", as.integer(`DATE OF DEATH OF DECEASED`)), "%d%m%Y")
    )

  #==============================================================
  #process the data up to inla_dat standard
  #==============================================================

  if (tolower(geo_level) == "lsoa21") {

    area_col    = "LSOA21CD"
    popestimate = lsoa21pop
    map         = lsoa21_boundaries

    #===============================================================
    #prcoess mortality data
    #===============================================================

    out_filtered = out %>%
      mutate(AgeGroup = case_when(
        AGE <=0               ~ "UNDER 1",
        AGE >= 1  & AGE <= 4  ~ "1-4",
        AGE >= 5  & AGE <= 9  ~ "5-9",
        AGE >= 10 & AGE <= 14 ~ "10-14",
        AGE >= 15 & AGE <= 19 ~ "15-19",
        AGE >= 20 & AGE <= 24 ~ "20-24",
        AGE >= 25 & AGE <= 29 ~ "25-29",
        AGE >= 30 & AGE <= 34 ~ "30-34",
        AGE >= 35 & AGE <= 39 ~ "35-39",
        AGE >= 40 & AGE <= 44 ~ "40-44",
        AGE >= 45 & AGE <= 49 ~ "45-49",
        AGE >= 50 & AGE <= 54 ~ "50-54",
        AGE >= 55 & AGE <= 59 ~ "55-59",
        AGE >= 60 & AGE <= 64 ~ "60-64",
        AGE >= 65 & AGE <= 69 ~ "65-69",
        AGE >= 70 & AGE <= 74 ~ "70-74",
        AGE >= 75 & AGE <= 79 ~ "75-79",
        AGE >= 80 & AGE <= 84 ~ "80-84",
        AGE >= 85 & AGE <= 89 ~ "85-89",
        AGE >= 90            ~ "90+",
        TRUE                 ~ NA_character_
      )) %>%
      rename(postcode = `POSTCODE OF USUAL RESIDENCE OF DECEASED`) %>%
      group_by(`LOWER SOA CODE OF USUAL RESIDENCE OF DECEASED`, AgeGroup) %>%
      summarise(death = n(),
                .groups = "drop") %>%
      rename(LSOA11CD = `LOWER SOA CODE OF USUAL RESIDENCE OF DECEASED`)

    #-----------------------------------------------------
    #deaths are coded to 2011 LSOAs - reallocate to 2021
    #-----------------------------------------------------
    out_filtered = out_filtered %>%
      left_join(lsoa11_to_lsoa21, by = c("LSOA11CD" = "lsoa11")) %>%
      group_by(LSOA21CD = lsoa21, AgeGroup) %>%
      summarise(death = sum(death * weight), .groups = "drop")

    #make sure data above the max pop estmate year use max year pop from pop data
    if (min(out$Year) < min(popestimate$year)){
      stop("Population estimates only go back to ", min(popestimate$year), ".")
    } else if (min(out$Year) <= max(popestimate$year)){
      use_year = min(out$Year)
    } else {
      use_year = max(popestimate$year)
    }

    out_proceesed = popestimate %>%
      filter(year == use_year) %>%
      left_join(out_filtered, by = c("LSOA21CD","age_band" = "AgeGroup")) %>%
      mutate(death = ifelse(is.na(death), 0, death))

    #-----------------------------------------------------
    #every death must survive the reallocation and joins
    #-----------------------------------------------------
    if (abs(sum(out_proceesed$death) - nrow(out)) > 0.5) {
      warning("Deaths went in as ", nrow(out), " and came out as ",
              round(sum(out_proceesed$death), 2), " after reallocation.")
    }

    # Create a stable numeric ID for each LSOA (in current data order)
    map$new_id = 1:nrow(map)

    # Create adjacency list
    mcnty_nb = poly2nb(map, row.names = map$new_id, queen = TRUE)

    # Build the INLA graph directly from the adjacency matrix
    g = INLA::inla.read.graph(nb2mat(mcnty_nb, style = "B", zero.policy = TRUE))

    # ------------------------------------------------------------
    # Construct INLA indexing variables
    # ------------------------------------------------------------
    inla_dat = out_proceesed %>%
      rename(localpop = count, AgeGroup = age_band) %>%
      left_join(map %>% st_drop_geometry() %>% select(LSOA21CD, new_id), by = "LSOA21CD") %>%
      mutate(
        # Align area indices with adjacency graph (1...N)
        new_id = as.integer(new_id),
        # Age group index (ordered)
        Age_index = as.integer(factor(AgeGroup, levels = age_levels)))

    #-----------------------------------------------------
    #every area must land on the graph
    #-----------------------------------------------------
    if (anyNA(inla_dat$new_id)) {
      stop(sum(is.na(inla_dat$new_id)), " rows have no matching area in the boundary data.")
    }

    # ------------------------------------------------------------
    # Restrict to the requested age bands
    # ------------------------------------------------------------
    inla_dat = inla_dat %>%
      filter(AgeGroup %in% age_filter)

  } else if (tolower(geo_level) == "ward") {

    area_col    = "WD21CD"
    popestimate = ward21pop
    map         = ward21_boundaries

    #===============================================================
    #prcoess mortality data
    #===============================================================

    out_filtered = out %>%
      mutate(AgeGroup = case_when(
        AGE <=0               ~ "UNDER 1",
        AGE >= 1  & AGE <= 4  ~ "1-4",
        AGE >= 5  & AGE <= 9  ~ "5-9",
        AGE >= 10 & AGE <= 14 ~ "10-14",
        AGE >= 15 & AGE <= 19 ~ "15-19",
        AGE >= 20 & AGE <= 24 ~ "20-24",
        AGE >= 25 & AGE <= 29 ~ "25-29",
        AGE >= 30 & AGE <= 34 ~ "30-34",
        AGE >= 35 & AGE <= 39 ~ "35-39",
        AGE >= 40 & AGE <= 44 ~ "40-44",
        AGE >= 45 & AGE <= 49 ~ "45-49",
        AGE >= 50 & AGE <= 54 ~ "50-54",
        AGE >= 55 & AGE <= 59 ~ "55-59",
        AGE >= 60 & AGE <= 64 ~ "60-64",
        AGE >= 65 & AGE <= 69 ~ "65-69",
        AGE >= 70 & AGE <= 74 ~ "70-74",
        AGE >= 75 & AGE <= 79 ~ "75-79",
        AGE >= 80 & AGE <= 84 ~ "80-84",
        AGE >= 85 & AGE <= 89 ~ "85-89",
        AGE >= 90            ~ "90+",
        TRUE                 ~ NA_character_
      )) %>%
      rename(postcode = `POSTCODE OF USUAL RESIDENCE OF DECEASED`) %>%
      group_by(`ELECTORAL WARD CODE OF USUAL RESIDENCE OF DECEASED`, AgeGroup) %>%
      summarise(death = n(),
                .groups = "drop") %>%
      rename(WD21CD = `ELECTORAL WARD CODE OF USUAL RESIDENCE OF DECEASED`)

    #make sure data above the max pop estmate year use max year pop from pop data
    if (min(out$Year) < min(popestimate$year)){
      stop("Population estimates only go back to ", min(popestimate$year), ".")
    } else if (min(out$Year) <= max(popestimate$year)){
      use_year = min(out$Year)
    } else {
      use_year = max(popestimate$year)
    }

    out_proceesed = popestimate %>%
      filter(year == use_year) %>%
      left_join(out_filtered, by = c("WD21CD","age_band" = "AgeGroup")) %>%
      mutate(death = ifelse(is.na(death), 0, death))

    # Create a stable numeric ID for each ward (in current data order)
    map$new_id = 1:nrow(map)

    # Create adjacency list
    mcnty_nb = poly2nb(map, row.names = map$new_id, queen = TRUE)

    # Build the INLA graph directly from the adjacency matrix
    g = INLA::inla.read.graph(nb2mat(mcnty_nb, style = "B", zero.policy = TRUE))

    # ------------------------------------------------------------
    # Construct INLA indexing variables
    # ------------------------------------------------------------
    inla_dat = out_proceesed %>%
      rename(localpop = count, AgeGroup = age_band) %>%
      left_join(map %>% st_drop_geometry() %>% select(WD21CD, new_id), by = "WD21CD") %>%
      mutate(
        # Align area indices with adjacency graph (1...N)
        new_id = as.integer(new_id),
        # Age group index (ordered)
        Age_index = as.integer(factor(AgeGroup, levels = age_levels)))

    #-----------------------------------------------------
    #every area must land on the graph
    #-----------------------------------------------------
    if (anyNA(inla_dat$new_id)) {
      stop(sum(is.na(inla_dat$new_id)), " rows have no matching area in the boundary data.")
    }

    # ------------------------------------------------------------
    # Restrict to the requested age bands
    # ------------------------------------------------------------
    inla_dat = inla_dat %>%
      filter(AgeGroup %in% age_filter)
  }

  return(list(death_data = out, inla_dat = inla_dat, g = g, map = map,
              area_col = area_col))
}
