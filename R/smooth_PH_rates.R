#' Fit a BYM2 small-area model and summarise age-standardised rates
#'
#' @param material_list Output of [get_ph_deaths()] or [get_ph_hes()] - a list
#'   with `inla_dat`, `g`, `map` and either `death_data` or `admission_data`.
#'   The count column modelled is `death` or `admission` accordingly.
#' @param threads Number of threads passed to INLA.
#' @param post_sample Number of posterior draws.
#' @param verbose Print progress messages to the console.
#' @returns A list with `fit` (the INLA model), `final_result` (one row per
#'   area, with the age-standardised rate, credible intervals, relative
#'   precision, reliability labels, exceedance probability and the period
#'   the estimate covers), `map`, `area_col` and `ex_cols` (the colour
#'   scale used by [plot_exceedance_prob()]).
#' @importFrom dplyr distinct arrange pull group_by summarise left_join mutate case_when
#' @importFrom tibble tibble
#' @importFrom Matrix sparse.model.matrix crossprod
#' @importFrom stats quantile
#' @importFrom rlang := .data
#' @importFrom magrittr %>%
#' @export
smooth_ph_rates = function(material_list = NULL,
                           threads = 6,
                           post_sample = 1000,
                           verbose = TRUE){

  if (is.null(material_list)) {
    stop("Please supply the material list.")
  } else if (!all(c("inla_dat", "g", "map", "period", "range") %in% names(material_list))) {
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

  if (!outcome %in% names(material_list$inla_dat)) {
    stop("Column `", outcome, "` not found in inla_dat.")
  }

  #---------------------------------------------------
  #which column holds the area code
  #---------------------------------------------------
  area_col = intersect(c("LSOA21CD", "WD21CD"), names(material_list$inla_dat))[1]
  if (is.na(area_col)) {
    stop("No area code column found in inla_dat (expected LSOA21CD or WD21CD).")
  }

  # ------------------------------------------------------------
  # Specify the INLA model
  # Outcome: ward-age-specific under-75 ALD death counts
  # Likelihood: Poisson with log(population) offset (models an underlying rate)
  # Random effects:
  # (1) Age effect: AR(1) structure across ordered age groups (smooth age pattern)
  # (2) Spatial effect: BYM2 across wards (structured + unstructured components)
  # (3) Space-age interaction: ward BYM2 effect allowed to vary by age, with an AR(1)
  # correlation across age groups (via group = Age_index)
  # Priors:
  # Penalised complexity (PC) priors for precisions and correlations
  # ------------------------------------------------------------

  formula = death ~ 1+
    f(Age_index,
      model = "ar1",
      constr = TRUE,
      hyper = list(
        theta1 = list(prior = "pc.prec", param = c(1, 0.01)), # PC prior on age-effect precision (encourages smoothness)
        theta2 = list(prior = "pc.cor1", param = c(0, 0.9)) # PC prior on AR(1) correlation across adjacent age groups
      ))+
    f(new_id,
      model = "bym2",
      graph = material_list$g,
      scale.model = TRUE, # scale BYM2 components for consistent interpretation across graphs
      hyper = list(
        prec = list(prior = "pc.prec", param = c(1, 0.01)), # PC prior for overall spatial precision
        phi = list(prior = "pc", param = c(0.5, 0.5)) # PC prior for mixing (structured vs unstructured)
      ),
      group = Age_index, # allow spatial effect to vary by age group
      control.group = list(model = "ar1") # AR(1) dependence of spatial effects across age groups
    )

  #---------------------------------------------------
  #put the right count column on the left of the formula
  #---------------------------------------------------
  formula[[2]] = as.name(outcome)

  # ------------------------------------------------------------
  # Fit the model
  # offset = log(local_pop) makes the model estimate mortality rates per person-year (or per person),
  # while the outcome remains as counts.
  # ------------------------------------------------------------
  if (any(material_list$inla_dat[[outcome]] != round(material_list$inla_dat[[outcome]]))) {
    fam = "xpoisson"
  } else {
    fam = "poisson"
  }

  if (verbose) message("Smoothing the rate ... (", nrow(material_list$inla_dat),
                       " area-age cells)")
  t0 = Sys.time()

  fit = INLA::inla(
    formula,
    family = fam,
    offset = log(localpop),
    data = material_list$inla_dat,
    num.threads = threads,
    control.predictor = list(compute = TRUE),
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
    verbose = FALSE
  )

  if (verbose) message("  done in ", round(difftime(Sys.time(), t0, units = "secs")), "s")

  #-----------------------------------------------------------
  #posterior draw
  if (verbose) message("Posterior sampling ... (", post_sample, " draws)")
  t0 = Sys.time()

  draws = INLA::inla.posterior.sample(n = post_sample, result = fit)

  if (verbose) message("  done in ", round(difftime(Sys.time(), t0, units = "secs")), "s")

  idx      = grep("^Predictor", rownames(draws[[1]]$latent))
  rate_mat = sapply(draws, function(d) exp(d$latent[idx]))

  std_mat = rate_mat / material_list$inla_dat$localpop * material_list$inla_dat$standard_pop

  #---------------------------------------------------------
  #sum age bands within each draw, by area
  #---------------------------------------------------------
  area_f = factor(material_list$inla_dat[[area_col]])
  A      = sparse.model.matrix(~ 0 + area_f)

  num   = as.matrix(crossprod(A, std_mat))
  denom = as.numeric(crossprod(A, material_list$inla_dat$standard_pop))
  asr   = num / denom * 100000

  #---------------------------------------------------------
  #95% CrI across draws
  #---------------------------------------------------------
  qs = t(apply(asr, 1, quantile, probs = c(0.025,  0.05,0.1, 0.5, 0.9, 0.95,0.975)))
  qs_mean = rowMeans(asr)

  asr_result = tibble(area_code    = levels(area_f),
                      asr_mean     = qs_mean,
                      asr_median   = qs[, 4],
                      asr_80LL     = qs[, 3],
                      asr_80UL     = qs[, 5],
                      asr_90LL     = qs[, 2],
                      asr_90UL     = qs[, 6],
                      asr_95LL     = qs[, 1],
                      asr_95UL     = qs[, 7])
  names(asr_result)[1] = area_col

  #---------------------------------------------------------
  #city asr per draw: pool by age band, then standardise
  #---------------------------------------------------------
  age_lv = material_list$inla_dat %>%
    distinct(AgeGroup, Age_index) %>%
    arrange(Age_index) %>%
    pull(AgeGroup)

  age_f = factor(material_list$inla_dat$AgeGroup, levels = age_lv)
  B     = sparse.model.matrix(~ 0 + age_f)

  exp_deaths_a = as.matrix(crossprod(B, rate_mat))            # rate_mat is already counts
  pop_a        = as.numeric(crossprod(B, material_list$inla_dat$localpop))
  city_rate_a  = exp_deaths_a / pop_a

  esp = tapply(material_list$inla_dat$standard_pop, age_f, function(x) x[1])
  city_asr = colSums(city_rate_a * as.numeric(esp)) / sum(esp) * 1e5

  city_asr_mean = mean(city_asr)

  #===========================================================
  #reliability: relative precision plus population floor
  #===========================================================
  final_result = asr_result %>%
    left_join(material_list$inla_dat %>%
                group_by(.data[[area_col]], new_id) %>%
                summarise(local_pop = sum(localpop),
                          !!paste0("local_", outcome) := sum(.data[[outcome]]), .groups = "drop"),
              by = area_col) %>%
    mutate(rp_95 = asr_median / (asr_95UL - asr_95LL),
           rp_90 = asr_median / (asr_90UL - asr_90LL),
           rp_80 = asr_median / (asr_80UL - asr_80LL),
           reliability_80 = case_when(rp_80 > 1 ~ "reliable at 80% CrI",
                                      TRUE      ~ "Unreliable"),
           reliability_90 = case_when(rp_90 > 1 ~ "reliable at 90% CrI",
                                      TRUE      ~ "Unreliable"),
           reliability_95 = case_when(rp_95 > 1 ~ "reliable at 95% CrI",
                                      TRUE      ~ "Unreliable"),
           city_mean       = mean(city_asr),
           exceedance_prob = rowMeans(asr > city_asr_mean),
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

  return(list(fit          = fit,
              final_result = final_result,
              map          = material_list$map,
              area_col     = area_col,
              ex_cols      = ex_cols
  ))

}

