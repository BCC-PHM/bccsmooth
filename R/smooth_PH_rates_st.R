#' Smooth area x age x year rates with an age-space-time model
#'
#' Fits the age-space-time model of Goicoa et al. (2016, Model 8) in INLA to
#' the `inla_dat_st` table from `get_ph_deaths()` or `get_ph_hes()`, then
#' builds age-standardised rates per area and year from posterior draws.
#'
#' @param material_list Output of `get_ph_deaths()` or `get_ph_hes()`.
#' @param threads Number of threads for INLA.
#' @param post_sample Number of posterior draws.
#' @param verbose Print progress messages.
#' @returns A list with `fit` (the INLA model); `final_result` (an sf with one
#'   row per area and year: ASR with 80/90/95% intervals, local population and
#'   counts, relative precision, reliability and exceedance against that year's
#'   city ASR); `city_year` (city ASR per year); `age_eff`, `space_eff`,
#'   `time_eff`, `space_age_eff`, `space_time_eff` and `age_time_eff` (posterior
#'   summaries of each model term on the rate-ratio scale); `map`, `area_col`,
#'   `outcome`, `age_bands`, `period`, `range` and `ex_cols`.
#' @importFrom dplyr left_join group_by summarise mutate case_when
#' @importFrom rlang .data :=
#' @importFrom magrittr %>%
#' @export
smooth_ph_rates_st = function(material_list = NULL,
                              threads       = 6,
                              post_sample   = 1000,
                              verbose       = TRUE){

  if (is.null(material_list)) {
    stop("Please supply the material list.")
  } else if (!all(c("inla_dat_st", "g", "map", "period", "range") %in% names(material_list))) {
    stop("Wrong material list supplied - use get_ph_deaths() or get_ph_hes() to obtain it.")
  }

  #---------------------------------------------------
  #which count column to model: admissions or deaths
  #---------------------------------------------------
  if ("admission_data" %in% names(material_list)) {
    outcome = "admission"
  } else if ("death_data" %in% names(material_list)) {
    outcome = "death"
  } else {
    stop("Wrong material list supplied - use get_ph_deaths() or get_ph_hes() to obtain it.")
  }

  if (!outcome %in% names(material_list$inla_dat_st)) {
    stop("Column `", outcome, "` not found in inla_dat_st.")
  }

  #---------------------------------------------------
  #which column holds the area code
  #---------------------------------------------------
  area_col = intersect(c("LSOA21CD", "WD21CD"), names(material_list$inla_dat_st))[1]
  if (is.na(area_col)) {
    stop("No area code column found in inla_dat_st (expected LSOA21CD or WD21CD).")
  }

  #===========================================================
  #dimensions and interaction ids
  #===========================================================
  inla_dat_st = material_list$inla_dat_st
  g           = material_list$g

  n_space = g$n
  n_year  = dplyr::n_distinct(inla_dat_st$time_id)
  n_age   = dplyr::n_distinct(inla_dat_st$Age_index)

  #-----------------------------------------------------
  #rep() ids need every area x age x year row present
  #-----------------------------------------------------
  if (nrow(inla_dat_st) != n_space * n_age * n_year) {
    stop("inla_dat_st has ", nrow(inla_dat_st), " rows; expected ", n_space * n_age * n_year,
         " (", n_space, " areas x ", n_age, " age bands x ", n_year, " years).")
  }

  #===========================================================
  #zero-population cells
  #===========================================================
  if (anyNA(inla_dat_st$localpop)) stop("Missing population in inla_dat_st. Check the population file.")
  inla_dat_st = inla_dat_st %>% dplyr::mutate(pop_offset = ifelse(localpop == 0, 1, localpop))

  #-----------------------------------------------------
  #goicoa's rep() ids assume year > area > age order
  #-----------------------------------------------------
  inla_dat_st = inla_dat_st %>%
    dplyr::arrange(time_id, new_id, Age_index) %>%
    dplyr::mutate(id_space_age  = rep(seq(1, n_space * n_age), n_year),
                  id_year_space = rep(seq(1, n_space * n_year), each = n_age),
                  id_year_age   = Age_index + n_age * (time_id - 1))

  #===========================================================
  #main effect structure matrices
  #===========================================================
  #-----------------------------------------------------
  #icar across areas, from the inla graph
  #-----------------------------------------------------
  Q_space = matrix(0, g$n, g$n)
  for (i in 1:g$n){
    Q_space[i, i]          = g$nnbs[[i]]
    Q_space[i, g$nbs[[i]]] = -1
  }

  #-----------------------------------------------------
  #leroux C matrix for generic1 (equation 3)
  #-----------------------------------------------------
  C_leroux = diag(nrow(Q_space)) - Q_space

  #-----------------------------------------------------
  #rw1 across age bands
  #-----------------------------------------------------
  D_age = diff(diag(n_age), differences = 1)
  Q_age = t(D_age) %*% D_age

  #-----------------------------------------------------
  #rw1 across years
  #-----------------------------------------------------
  D_year = diff(diag(n_year), differences = 1)
  Q_year = t(D_year) %*% D_year

  #===========================================================
  #interactions: first word = outer index
  #===========================================================
  #-----------------------------------------------------
  #space x time: year outer, space inner
  #-----------------------------------------------------
  ns_year_space = kronecker_null_space(Q_year, Q_space)
  Q_year_space  = ns_year_space[[1]]
  A_year_space  = as.matrix(ns_year_space[[2]])

  #-----------------------------------------------------
  #space x age: space outer, age inner
  #-----------------------------------------------------
  ns_space_age = kronecker_null_space(Q_space, Q_age)
  Q_space_age  = ns_space_age[[1]]
  A_space_age  = as.matrix(ns_space_age[[2]])

  #-----------------------------------------------------
  #age x time: year outer, age inner
  #-----------------------------------------------------
  ns_year_age = kronecker_null_space(Q_year, Q_age)
  Q_year_age  = ns_year_age[[1]]
  A_year_age  = as.matrix(ns_year_age[[2]])

  # ------------------------------------------------------------
  # Specify the INLA age-space-time model
  # (after Goicoa et al., 2016, Model 8)
  # Outcome: event counts by area x age band x year
  # Likelihood: Poisson with log(population) offset (models an underlying rate)
  # Main effects:
  # (1) Space: Leroux CAR across areas (generic1), sum-to-zero
  # (2) Time: RW(1) across years (trend common to all areas and ages)
  # (3) Age: RW(1) across ordered age bands (age pattern common to all areas and years)
  # Interactions (generic0, Kronecker product of structure matrices, Type IV):
  # (4) Space x age: ICAR x RW(1) - area-specific age patterns
  # (5) Space x time: RW(1) x ICAR - area-specific time trends
  # (6) Age x time: RW(1) x RW(1) - age-specific time trends
  # Each interaction is constrained to sum to zero over each of its indices
  # (null-space constraints from kronecker_null_space()), so it captures only
  # departures from the main effects and cannot absorb them
  # Priors:
  # Leroux: loggamma(1, 0.01) on the precision, logitbeta(1, 1) on the mixing parameter
  # RW(1) and interaction precisions: INLA defaults (to be replaced with PC priors)
  # ------------------------------------------------------------
  formula = death ~ 1 +
    #space: leroux car
    f(new_id,
      model   = "generic1",
      Cmatrix = C_leroux,
      constr  = TRUE,
      hyper   = list(prec = list(prior = "loggamma", param = c(1, 0.01)),
                     beta = list(prior = "logitbeta", param = c(1, 1)))
    ) +
    #time and age main effects
    f(time_id, model = "rw1") +
    f(Age_index, model = "rw1") +
    #space x age
    f(id_space_age,
      model       = "generic0",
      Cmatrix     = Q_space_age,
      rankdef     = nrow(A_space_age),
      constr      = FALSE,
      extraconstr = list(A = A_space_age, e = rep(0, nrow(A_space_age)))
    ) +
    #space x time
    f(id_year_space,
      model       = "generic0",
      Cmatrix     = Q_year_space,
      rankdef     = nrow(A_year_space),
      constr      = FALSE,
      extraconstr = list(A = A_year_space, e = rep(0, nrow(A_year_space)))
    ) +
    #age x time
    f(id_year_age,
      model       = "generic0",
      Cmatrix     = Q_year_age,
      rankdef     = nrow(A_year_age),
      constr      = FALSE,
      extraconstr = list(A = A_year_age, e = rep(0, nrow(A_year_age)))
    )

  #---------------------------------------------------
  #put the right count column on the left of the formula
  #---------------------------------------------------
  formula[[2]] = as.name(outcome)

  # ------------------------------------------------------------
  # Fit the model
  # simplified laplace to reduce computation time given the dimensions
  # offset = log(localpop) makes the model estimate rates per person,
  # while the outcome remains as counts
  # ------------------------------------------------------------
  if (any(inla_dat_st[[outcome]] != round(inla_dat_st[[outcome]]))) {
    fam = "xpoisson"
  } else {
    fam = "poisson"
  }

  if (verbose) message("Smoothing the rate ... (", nrow(inla_dat_st), " area-age-year cells)")
  t0 = Sys.time()

  fit = INLA::inla(formula,
                   family            = fam,
                   offset            = log(pop_offset),
                   data              = inla_dat_st,
                   num.threads       = threads,
                   control.predictor = list(compute = TRUE),
                   control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
                   control.inla      = list(strategy = "simplified.laplace"),
                   verbose           = FALSE)

  if (verbose) message("  done in ", round(difftime(Sys.time(), t0, units = "secs")), "s")

  #===========================================================
  #posterior draws
  #===========================================================
  if (verbose) message("Posterior sampling ... (", post_sample, " draws)")
  t0 = Sys.time()

  draws = INLA::inla.posterior.sample(n = post_sample, result = fit)

  #-----------------------------------------------------
  #predictor carries the offset: these are expected counts
  #-----------------------------------------------------
  idx      = grep("^Predictor", rownames(draws[[1]]$latent))
  rate_mat = sapply(draws, function(d) exp(d$latent[idx]))
  std_mat  = rate_mat / inla_dat_st$pop_offset * inla_dat_st$standard_pop

  if (verbose) message("  done in ", round(difftime(Sys.time(), t0, units = "secs")), "s")

  #===========================================================
  #area x year asr per draw
  #===========================================================
  ay_f  = factor(inla_dat_st$id_year_space, levels = seq_len(n_space * n_year))
  A     = Matrix::sparse.model.matrix(~ 0 + ay_f)
  num   = as.matrix(Matrix::crossprod(A, std_mat))
  denom = as.numeric(Matrix::crossprod(A, inla_dat_st$standard_pop))
  asr   = num / denom * 100000

  #-----------------------------------------------------
  #summarise across draws
  #-----------------------------------------------------
  qs = t(apply(asr, 1, quantile, probs = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975)))

  #-----------------------------------------------------
  #labels for each area x year row
  #-----------------------------------------------------
  keys = inla_dat_st %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c("id_year_space", "time_id", "fin_year", "new_id", area_col))))

  asr_year = tibble::tibble(id_year_space = seq_len(n_space * n_year),
                            asr_mean      = rowMeans(asr),
                            asr_median    = qs[, 4],
                            asr_80LL      = qs[, 3],
                            asr_80UL      = qs[, 5],
                            asr_90LL      = qs[, 2],
                            asr_90UL      = qs[, 6],
                            asr_95LL      = qs[, 1],
                            asr_95UL      = qs[, 7]) %>%
    dplyr::left_join(keys, by = "id_year_space")


  #===========================================================
  #city asr per year and draw: pool areas, then standardise
  #===========================================================
  ya_f = factor(inla_dat_st$id_year_age, levels = seq_len(n_year * n_age))
  YA   = Matrix::sparse.model.matrix(~ 0 + ya_f)
  cnt  = as.matrix(Matrix::crossprod(YA, rate_mat / inla_dat_st$pop_offset * inla_dat_st$localpop))
  pop  = as.numeric(Matrix::crossprod(YA, inla_dat_st$localpop))

  #-----------------------------------------------------
  #esp weight per age band, repeated for every year
  #-----------------------------------------------------
  esp = inla_dat_st %>%
    dplyr::distinct(Age_index, standard_pop) %>%
    dplyr::arrange(Age_index) %>%
    dplyr::pull(standard_pop)

  if (length(esp) != n_age) stop("standard_pop is not unique within each age band.")

  w       = rep(esp, n_year)
  year_of = rep(seq_len(n_year), each = n_age)

  city_asr = rowsum(cnt / pop * w, year_of) / sum(esp) * 100000

  #-----------------------------------------------------
  #summarise across draws
  #-----------------------------------------------------
  qs_city = t(apply(city_asr, 1, quantile, probs = c(0.025, 0.5, 0.975)))

  city_year = tibble::tibble(time_id    = seq_len(n_year),
                             asr_mean   = rowMeans(city_asr),
                             asr_median = qs_city[, 2],
                             asr_95LL   = qs_city[, 1],
                             asr_95UL   = qs_city[, 3]) %>%
    dplyr::left_join(dplyr::distinct(inla_dat_st, time_id, fin_year), by = "time_id")


  #===========================================================
  #reliability: relative precision plus population floor
  #===========================================================
  final_result = asr_year %>%
    left_join(inla_dat_st %>%
                group_by(.data[[area_col]], new_id, fin_year) %>%
                summarise(local_pop = sum(localpop),
                          !!paste0("local_", outcome) := sum(.data[[outcome]]), .groups = "drop"),
              by = c(area_col, "new_id", "fin_year")) %>%
    mutate(rp_95 = asr_median / (asr_95UL - asr_95LL),
           rp_90 = asr_median / (asr_90UL - asr_90LL),
           rp_80 = asr_median / (asr_80UL - asr_80LL),
           reliability_80 = case_when(rp_80 > 1 ~ "reliable at 80% CrI",
                                      TRUE      ~ "Unreliable"),
           reliability_90 = case_when(rp_90 > 1 ~ "reliable at 90% CrI",
                                      TRUE      ~ "Unreliable"),
           reliability_95 = case_when(rp_95 > 1 ~ "reliable at 95% CrI",
                                      TRUE      ~ "Unreliable"),
           city_mean       = rowMeans(city_asr)[time_id],
           exceedance_prob = rowMeans(asr > rowMeans(city_asr)[rep(seq_len(n_year), each = n_space)]),
           period          = material_list$period,
           range           = material_list$range)


  ex_levels = c(">95%", "90-95%", "80-90%", "20-80%",
                "5-20%", "1-5%", "<1%")

  ex_cols = c(">95%"   = "#B2182B",
              "90-95%" = "#D6604D",
              "80-90%" = "#F4A582",
              "20-80%" = "#F7F7F7",
              "5-20%"  = "#92C5DE",
              "1-5%"   = "#4393C3",
              "<1%"    = "#2166AC")

  final_result = final_result %>%
    mutate(ex_prob_label = case_when(exceedance_prob >  0.95 ~ ">95%",
                                     exceedance_prob >  0.90 ~ "90-95%",
                                     exceedance_prob >  0.80 ~ "80-90%",
                                     exceedance_prob >= 0.20 ~ "20-80%",
                                     exceedance_prob >= 0.05 ~ "5-20%",
                                     exceedance_prob >= 0.01 ~ "1-5%",
                                     TRUE                    ~ "<1%"),
           ex_prob_label = factor(ex_prob_label, levels = ex_levels))

  final_result = material_list$map %>%
    left_join(final_result, by = c("new_id", area_col))

  #===========================================================
  # create different data tables for plotting
  # model effects on the rate-ratio scale
  # age effect
  # space effect
  # temporal effect
  # age-space
  # age-time
  # space-time
  #===========================================================
  #===========================================================
  #model effects on the rate-ratio scale (helper: tidy_posterior)
  #===========================================================
  age_eff        = tidy_posterior(fit, inla_dat_st, "Age_index", "AgeGroup")
  space_eff      = tidy_posterior(fit, inla_dat_st, "new_id", area_col)
  time_eff       = tidy_posterior(fit, inla_dat_st, "time_id", "fin_year")
  space_age_eff  = tidy_posterior(fit, inla_dat_st, "id_space_age", c("new_id", area_col, "Age_index", "AgeGroup"))
  space_time_eff = tidy_posterior(fit, inla_dat_st, "id_year_space", c("new_id", area_col, "time_id", "fin_year"))
  age_time_eff   = tidy_posterior(fit, inla_dat_st, "id_year_age", c("Age_index", "AgeGroup", "fin_year"))


  return(list(fit            = fit,
              final_result   = final_result,
              city_year      = city_year,
              age_eff        = age_eff,
              space_eff      = space_eff,
              time_eff       = time_eff,
              space_age_eff  = space_age_eff,
              space_time_eff = space_time_eff,
              age_time_eff   = age_time_eff,
              map            = material_list$map,
              area_col       = area_col,
              outcome        = outcome,
              age_bands      = material_list$age_bands,
              period         = material_list$period,
              range          = material_list$range,
              ex_cols        = ex_cols))
}











