#' Fetch Birmingham inpatient admissions from the PH_HES warehouse
#'
#' HES inpatient data sit in one table per financial year
#' (`tblHES_IP_et25_26gen` for 2025/26). The same query is run on each
#' requested year's table and the results are stacked. Admissions are coded
#' to 2011 LSOAs and are reallocated onto 2021 LSOAs using area-intersection
#' weights, which makes the admission counts fractional.
#'
#' Babies under 1 are recorded in `ADMIAGE` as codes 7001-7007; these are
#' converted to age 0 in a new column, `age_yrs`, which is used for the age
#' filter and the age bands.
#'
#' @param financial_year Character vector of financial years in the format
#'   "2025/2026". Multiple years may be supplied. Each must have a
#'   `tblHES_IP_etYY_YYgen` table in the database.
#' @param primary_diagnostic_code Character vector of ICD-10 patterns for SQL
#'   `LIKE`, e.g. `c("K7[0-7]%", "C22%")`. `NULL` returns all causes.
#' @param secondary_diagnostic_code `FALSE` matches the patterns in `DIAG_01`
#'   only. `TRUE` matches them in any of `DIAG_01` to `DIAG_20`.
#' @param epistat,epiorder,sex,classpat Values kept for each HES field.
#' @param resladst_ons Local authority of residence code.
#' @param age One comparison such as "<75", or two bounds such as c("15", "64").
#'   Uses age at admission. `NULL` returns all ages.
#' @param geo_level Geography to model at: "lsoa21" or "ward".
#' @param server,database,driver Connection details.
#' @returns A list with `admission_data` (one row per admission), `inla_dat`
#'   (one row per area and age band, with the count in `admission`), `g` (the
#'   INLA adjacency graph), `map` (the boundaries), `area_col` (the name of the
#'   area code column in `inla_dat`), `period` and `range`.
#' @importFrom dplyr tbl select filter mutate case_when group_by summarise rename left_join distinct pull n collect bind_rows
#' @importFrom tibble tibble
#' @importFrom sf st_drop_geometry
#' @importFrom spdep poly2nb nb2mat
#' @importFrom magrittr %>%
#' @export
get_ph_hes = function(
    financial_year  = "2025/2026",
    primary_diagnostic_code = NULL,
    secondary_diagnostic_code = FALSE,
    epistat         = "3",
    epiorder        = "1",
    sex             = c("1", "2"),
    classpat        = c("1", "2"),
    resladst_ons    = "E08000025",
    age             = NULL,
    geo_level       = "lsoa21",
    server          = "SVWVSQ016\\SVWVSQ016",
    database        = "PH_HES",
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
  #"2022/2023" (warehouse) -> "2223" (population file)
  #---------------------------------------------------------
  to_pop_finyear = function(fy) paste0(substr(fy, 3, 4), substr(fy, 8, 9))

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
      age_script = paste0("age_yrs ", op, " ", n)

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
      age_script = paste0("between(age_yrs, ", n1, ", ", n2, ")")
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
  #table name, and guard if no table found
  #---------------------------------------------------------
  table_name =paste0("tblHES_IP_et", substr(financial_year, 3, 4), "_", substr(financial_year, 8, 9), "gen")

  #hes inpatient tables currently in the database
  avail_tbl = grep("^tblHES_IP_et\\d{2}_\\d{2}gen$", DBI::dbListTables(con), value = TRUE)

  #stop if any requested year has no table
  if (!all(table_name %in% avail_tbl)) {
    avail_fy = sort(paste0("20", substr(avail_tbl, 13, 14), "/20", substr(avail_tbl, 16, 17)))
    stop("No HES table for ", toString(financial_year[!table_name %in% avail_tbl]),
         ". Available years: ", toString(avail_fy), ".")
  }

  #---------------------------------------------------------
  #diagnosis: DIAG_01 only, or any of the 20 positions
  #---------------------------------------------------------
  dx_script = NULL

  if (!is.null(primary_diagnostic_code)) {

    dx_cols = if (isTRUE(secondary_diagnostic_code)) {
      sprintf("DIAG_%02d", 1:20)
    } else {
      "DIAG_01"
    }

    dx_script = paste0(
      rep(dx_cols, each = length(primary_diagnostic_code)),
      " %LIKE% ",
      rep(
        encodeString(primary_diagnostic_code, quote = '"'),
        times = length(dx_cols)
      ),
      collapse = " | "
    )
  }


  #---------------------------------------------------------
  #query each year's table, then stack
  #---------------------------------------------------------
  out = lapply(seq_along(table_name), function(i) {
    q = dplyr::tbl(con, table_name[i]) %>%
      dplyr::select("ACTIVAGE", "ADMIAGE", "ADMIDATE", "ADMIMETH", "BEDYEAR", "CLASSPAT", "CSNUM",
                    "DIAG_01", "DIAG_02", "DIAG_03", "DIAG_04", "DIAG_05", "DIAG_06", "DIAG_07",
                    "DIAG_08", "DIAG_09", "DIAG_10", "DIAG_11", "DIAG_12", "DIAG_13", "DIAG_14",
                    "DIAG_15", "DIAG_16", "DIAG_17", "DIAG_18", "DIAG_19", "DIAG_20",
                    "DISDATE", "DISREADYDATE", "DISDEST", "DISMETH", "ELECDATE", "ELECDUR",
                    "ENDAGE", "EPIDUR", "EPIEND", "EPIKEY", "EPIORDER", "EPISTART", "EPISTAT",
                    "EPITYPE", "SEX", "LSOA01", "MSOA01", "FYEAR", "PARTYEAR", "LSOA11", "MSOA11",
                    "CURRWARD_ONS", "PCON_ONS", "RESCTY_ONS", "RESGOR_ONS", "RESLADST_ONS") %>%
      dplyr::filter(RESLADST_ONS == !!resladst_ons,
                    EPISTAT      %in% !!epistat,
                    EPIORDER     %in% !!epiorder,
                    SEX          %in% !!sex,
                    CLASSPAT     %in% !!classpat) %>%
      dplyr::mutate(age_yrs = ifelse(ADMIAGE >= 7000, 0L, ADMIAGE))

    if (!is.null(dx_script))  q = dplyr::filter(q, !!rlang::parse_expr(dx_script))
    if (!is.null(age_script)) q = dplyr::filter(q, !!rlang::parse_expr(age_script))

    q %>%
      dplyr::collect() %>%
      dplyr::mutate(FinYear = financial_year[i])
  }) %>%
    dplyr::bind_rows()


  #==============================================================
  #process the data up to inla_dat standard
  #==============================================================

  if (tolower(geo_level) == "lsoa21") {

    area_col    = "LSOA21CD"
    popestimate = lsoa21pop
    map         = lsoa21_boundaries

    #===============================================================
    #process admission data
    #===============================================================
    out_filtered = out %>%
      mutate(AgeGroup = case_when(
        age_yrs <= 0                  ~ "UNDER 1",
        age_yrs >= 1  & age_yrs <= 4  ~ "1-4",
        age_yrs >= 5  & age_yrs <= 9  ~ "5-9",
        age_yrs >= 10 & age_yrs <= 14 ~ "10-14",
        age_yrs >= 15 & age_yrs <= 19 ~ "15-19",
        age_yrs >= 20 & age_yrs <= 24 ~ "20-24",
        age_yrs >= 25 & age_yrs <= 29 ~ "25-29",
        age_yrs >= 30 & age_yrs <= 34 ~ "30-34",
        age_yrs >= 35 & age_yrs <= 39 ~ "35-39",
        age_yrs >= 40 & age_yrs <= 44 ~ "40-44",
        age_yrs >= 45 & age_yrs <= 49 ~ "45-49",
        age_yrs >= 50 & age_yrs <= 54 ~ "50-54",
        age_yrs >= 55 & age_yrs <= 59 ~ "55-59",
        age_yrs >= 60 & age_yrs <= 64 ~ "60-64",
        age_yrs >= 65 & age_yrs <= 69 ~ "65-69",
        age_yrs >= 70 & age_yrs <= 74 ~ "70-74",
        age_yrs >= 75 & age_yrs <= 79 ~ "75-79",
        age_yrs >= 80 & age_yrs <= 84 ~ "80-84",
        age_yrs >= 85 & age_yrs <= 89 ~ "85-89",
        age_yrs >= 90                 ~ "90+",
        TRUE                          ~ NA_character_
      )) %>%
      group_by(LSOA11, AgeGroup, FinYear) %>%
      summarise(admission = n(), .groups = "drop") %>%
      rename(LSOA11CD = LSOA11)


    #-----------------------------------------------------
    #admissions are coded to 2011 LSOAs - reallocate to 2021
    #-----------------------------------------------------
    out_filtered = out_filtered %>%
      left_join(lsoa11_to_lsoa21, by = c("LSOA11CD" = "lsoa11")) %>%
      group_by(LSOA21CD = lsoa21, AgeGroup, fin_year = to_pop_finyear(FinYear)) %>%
      summarise(admission = sum(admission * weight), .groups = "drop")

    #-----------------------------------------------------
    #map each requested year to an available population year
    #-----------------------------------------------------
    req_fin_year   = to_pop_finyear(financial_year)
    avail_fin_year = sort(unique(popestimate$fin_year))

    if (any(req_fin_year < min(avail_fin_year))) {
      stop("Population estimates only go back to ", min(avail_fin_year), ".")
    }

    use_fin_year = ifelse(req_fin_year %in% avail_fin_year,
                          req_fin_year, max(avail_fin_year))

    if (any(use_fin_year != req_fin_year)) {
      latest = paste0("20", substr(max(avail_fin_year), 1, 2), "/",
                      substr(max(avail_fin_year), 3, 4))
      message("No official population estimates for ",
              toString(financial_year[use_fin_year != req_fin_year]),
              " - ", latest, " population used instead.")
    }

    year_map = tibble(fin_year = req_fin_year, pop_fin_year = use_fin_year)

    #-----------------------------------------------------
    #join year by year, then pool to person-years
    #-----------------------------------------------------
    out_proceesed = year_map %>%
      left_join(popestimate, by = c("pop_fin_year" = "fin_year"),
                relationship = "many-to-many") %>%
      left_join(out_filtered, by = c("LSOA21CD", "age_band" = "AgeGroup", "fin_year")) %>%
      mutate(admission = ifelse(is.na(admission), 0, admission)) %>%
      group_by(LSOA21CD, age_band) %>%
      summarise(localpop     = sum(count),
                admission    = sum(admission),
                standard_pop = dplyr::first(standard_pop), .groups = "drop")


    #-----------------------------------------------------
    #every admission must survive the reallocation and joins
    #-----------------------------------------------------
    if (abs(sum(out_proceesed$admission) - nrow(out)) > 0.5) {
      warning("Admissions went in as ", nrow(out), " and came out as ",
              round(sum(out_proceesed$admission), 2), " after reallocation.")
    }

    #-----------------------------------------------------
    #stable numeric id, adjacency, inla graph
    #-----------------------------------------------------
    map$new_id = 1:nrow(map)
    mcnty_nb = poly2nb(map, row.names = map$new_id, queen = TRUE)
    g = INLA::inla.read.graph(nb2mat(mcnty_nb, style = "B", zero.policy = TRUE))

    #-----------------------------------------------------
    #construct inla indexing variables
    #-----------------------------------------------------
    inla_dat = out_proceesed %>%
      rename(AgeGroup = age_band) %>%
      left_join(map %>% st_drop_geometry() %>% select(LSOA21CD, new_id), by = "LSOA21CD") %>%
      mutate(new_id    = as.integer(new_id),
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
    #process admission data
    #===============================================================
    out_filtered = out %>%
      mutate(AgeGroup = case_when(
        age_yrs <= 0                  ~ "UNDER 1",
        age_yrs >= 1  & age_yrs <= 4  ~ "1-4",
        age_yrs >= 5  & age_yrs <= 9  ~ "5-9",
        age_yrs >= 10 & age_yrs <= 14 ~ "10-14",
        age_yrs >= 15 & age_yrs <= 19 ~ "15-19",
        age_yrs >= 20 & age_yrs <= 24 ~ "20-24",
        age_yrs >= 25 & age_yrs <= 29 ~ "25-29",
        age_yrs >= 30 & age_yrs <= 34 ~ "30-34",
        age_yrs >= 35 & age_yrs <= 39 ~ "35-39",
        age_yrs >= 40 & age_yrs <= 44 ~ "40-44",
        age_yrs >= 45 & age_yrs <= 49 ~ "45-49",
        age_yrs >= 50 & age_yrs <= 54 ~ "50-54",
        age_yrs >= 55 & age_yrs <= 59 ~ "55-59",
        age_yrs >= 60 & age_yrs <= 64 ~ "60-64",
        age_yrs >= 65 & age_yrs <= 69 ~ "65-69",
        age_yrs >= 70 & age_yrs <= 74 ~ "70-74",
        age_yrs >= 75 & age_yrs <= 79 ~ "75-79",
        age_yrs >= 80 & age_yrs <= 84 ~ "80-84",
        age_yrs >= 85 & age_yrs <= 89 ~ "85-89",
        age_yrs >= 90                 ~ "90+",
        TRUE                          ~ NA_character_
      )) %>%
      group_by(CURRWARD_ONS, AgeGroup, fin_year = to_pop_finyear(FinYear)) %>%
      summarise(admission = n(), .groups = "drop") %>%
      rename(WD21CD = CURRWARD_ONS)


    #-----------------------------------------------------
    #map each requested year to an available population year
    #-----------------------------------------------------
    req_fin_year   = to_pop_finyear(financial_year)
    avail_fin_year = sort(unique(popestimate$fin_year))

    if (any(req_fin_year < min(avail_fin_year))) {
      stop("Population estimates only go back to ", min(avail_fin_year), ".")
    }

    use_fin_year = ifelse(req_fin_year %in% avail_fin_year,
                          req_fin_year, max(avail_fin_year))

    if (any(use_fin_year != req_fin_year)) {
      latest = paste0("20", substr(max(avail_fin_year), 1, 2), "/",
                      substr(max(avail_fin_year), 3, 4))
      message("No official population estimates for ",
              toString(financial_year[use_fin_year != req_fin_year]),
              " - ", latest, " population used instead.")
    }

    year_map = tibble(fin_year = req_fin_year, pop_fin_year = use_fin_year)

    #-----------------------------------------------------
    #join year by year, then pool to person-years
    #-----------------------------------------------------
    out_proceesed = year_map %>%
      left_join(popestimate, by = c("pop_fin_year" = "fin_year"),
                relationship = "many-to-many") %>%
      left_join(out_filtered, by = c("WD21CD", "age_band" = "AgeGroup", "fin_year")) %>%
      mutate(admission = ifelse(is.na(admission), 0, admission)) %>%
      group_by(WD21CD, age_band) %>%
      summarise(localpop     = sum(count),
                admission    = sum(admission),
                standard_pop = dplyr::first(standard_pop), .groups = "drop")

    #-----------------------------------------------------
    #every admission must survive the joins
    #-----------------------------------------------------
    if (abs(sum(out_proceesed$admission) - nrow(out)) > 0.5) {
      warning("Admissions went in as ", nrow(out), " and came out as ",
              round(sum(out_proceesed$admission), 2), " after the ward join.")
    }

    #-----------------------------------------------------
    #stable numeric id, adjacency, inla graph
    #-----------------------------------------------------
    map$new_id = 1:nrow(map)
    mcnty_nb = poly2nb(map, row.names = map$new_id, queen = TRUE)
    g = INLA::inla.read.graph(nb2mat(mcnty_nb, style = "B", zero.policy = TRUE))

    #-----------------------------------------------------
    #construct inla indexing variables
    #-----------------------------------------------------
    inla_dat = out_proceesed %>%
      rename(AgeGroup = age_band) %>%
      left_join(map %>% st_drop_geometry() %>% select(WD21CD, new_id), by = "WD21CD") %>%
      mutate(new_id    = as.integer(new_id),
             Age_index = as.integer(factor(AgeGroup, levels = age_levels)))

    #-----------------------------------------------------
    #every area must land on the graph
    #-----------------------------------------------------
    if (anyNA(inla_dat$new_id)) {
      stop(sum(is.na(inla_dat$new_id)), " rows have no matching area in the boundary data.")
    }

    inla_dat = inla_dat %>%
      filter(AgeGroup %in% age_filter)
  }


  #---------------------------------------------------------
  #period label, e.g. "2023/24-2025/26"
  #---------------------------------------------------------
  fy_sorted = sort(financial_year)
  fy_short  = sub("/\\d{2}(\\d{2})$", "/\\1", fy_sorted)

  period = if (length(fy_short) == 1) {
    fy_short
  } else {
    paste0(fy_short[1], "-", fy_short[length(fy_short)])
  }

  range = paste0(length(financial_year), " year range")


  return(list(admission_data = out,
              inla_dat = inla_dat,
              g = g,
              map = map,
              area_col = area_col,
              period = period,
              range = range ))


}


