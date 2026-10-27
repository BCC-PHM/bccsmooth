#' Posterior summary of exp(effect) for one INLA random term
#'
#' @param fit Fitted INLA model.
#' @param dat Data the model was fitted on, holding the id and label columns.
#' @param id Name of the random term's id column, e.g. "Age_index".
#' @param label_cols Columns to attach as labels, e.g. "AgeGroup".
#' @returns A tibble with one row per level: id, rr_mean, rr_median,
#'   rr_lwr, rr_upr and the label columns.
#' @importFrom dplyr transmute distinct left_join across all_of
#' @importFrom rlang :=
#' @importFrom magrittr %>%
#' @keywords internal
tidy_posterior = function(fit, dat, id, label_cols) {
  rr = sapply(fit$marginals.random[[id]], function(m) INLA::inla.emarginal(exp, m))

  fit$summary.random[[id]] %>%
    transmute(!!id      := ID,
              rr_mean   = rr,
              rr_median = exp(`0.5quant`),
              rr_lwr    = exp(`0.025quant`),
              rr_upr    = exp(`0.975quant`)) %>%
    left_join(distinct(dat, across(all_of(c(id, label_cols)))), by = id)
}
