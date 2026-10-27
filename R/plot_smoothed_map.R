#' Map smoothed small-area rates with reliability suppression
#'
#' Draws a choropleth of the smoothed age-standardised rate and blanks areas
#' flagged as unreliable on the chosen credible interval. Accepts output from
#' either [smooth_ph_rates()] or [smooth_ph_rates_st()]. For age-space-time
#' output (a `fin_year` column is present), the map is faceted by financial
#' year. Optionally returns the central estimate alongside its lower and
#' upper bounds as a three-map list.
#'
#' Unreliable areas are those whose `reliability_<ci>` value is `NA` or
#' `"Unreliable"`. Every `asr_` column is set to `NA` for them, and they
#' appear in the legend as "Unreliable".
#'
#' @param final_map_data The full list returned by [smooth_ph_rates()] or
#'   [smooth_ph_rates_st()] (must contain `fit`, `final_result`, `map`,
#'   `area_col` and `ex_cols`). Its `final_result` sf object is mapped.
#' @param credible_interval One of `"80"`, `"90"` or `"95"` (the `%` is
#'   optional). `NULL` uses 95%.
#' @param credible_interval_map `TRUE` returns a list of three maps: the
#'   central estimate and both bounds.
#' @param fill_value Which rate to map: `"median"` (default) or `"mean"`.
#'   `"asr_median"` and `"asr_mean"` are also accepted; case is ignored.
#' @param fill_title Map title. `NULL` builds one from the data's `period`.
#' @param title_in Note drawn inside the map frame. Left empty, a sentence
#'   describing the reliability threshold is used.
#' @param title_in_position,legend_position tmap position objects.
#' @param palette A cols4all palette name, e.g. `"brewer.blues"`.
#' @param legend_text_size,legend_title_size Relative text sizes.
#' @param boundary_colour Colour of the polygon outlines.
#' @param fn_col,fn_row Number of facet columns and rows when mapping
#'   age-space-time output by financial year. Ignored otherwise.
#' @returns A `tmap` object (faceted by `fin_year` for age-space-time
#'   output), or, when `credible_interval_map` is `TRUE`, a named list of
#'   three: `asr_map`, `asr_map_LL` and `asr_map_UL`.
#' @importFrom tmap tm_pos_in tm_facets
#' @importFrom dplyr mutate if_else
#' @export
plot_smoothed_map = function(final_map_data = NULL,
                             credible_interval = NULL,
                             credible_interval_map = FALSE,
                             fill_value = "median",
                             fill_title = NULL,
                             title_in        = "",
                             title_in_position = tm_pos_in(pos.h = 0.025, pos.v = 1.035),
                             palette = "brewer.blues",
                             legend_text_size = 0.6,
                             legend_title_size = 0.7,
                             legend_position = tm_pos_in(pos.h = 0.02, pos.v = 1.01),
                             boundary_colour = "grey70",
                             fn_col = 7,
                             fn_row = 4){



  if (is.null(final_map_data)) {
    stop("Please supply the smoothing output.")
  } else if (!all(c("fit", "final_result", "map", "area_col", "ex_cols") %in% names(final_map_data))) {
    stop("Wrong list supplied - use smooth_ph_rates() or smooth_ph_rates_st() to obtain it.")
  }

  final_map_data = final_map_data$final_result

  if(!tolower(fill_value)  %in% c("asr_median", "median","asr_mean", "mean" )){
    stop('Invalid fill_value supplied, please use one of the following: "median", "mean"')
  }
  else if(tolower(fill_value) %in% c("asr_median", "median")){
    fill_value = "asr_median"
    message(paste0('The default smoothed map uses median, if you would like mean instead, specify: fill_value = "mean"'))
  }
  else if (tolower(fill_value) %in% c("asr_mean", "mean")){
    fill_value = "asr_mean"
  }



  if(is.null(credible_interval)){
    credible_interval = "95%"
    message(paste0('The default reliability uses 95% CrI when no credible_interval is specified, other intervals are 90% and 80%'))
  }
  else if(!is.null(credible_interval) && !credible_interval %in% c("80", "80%", "90", "90%", "95%", "95")){

    stop('Invalid credible_interval supplied, please use one of the following: "80%", "90%", "95%"')

  }
  else if(credible_interval %in% c("95%", "95")){
    credible_interval = "95%"
    message('Reliability uses relative precision on the 95% credible interval. ',
            'For a less strict threshold, try 90% or 80%.')
  }
  else if(credible_interval %in% c("90%", "90")){
    credible_interval = "90%"
    message('Reliability uses relative precision on the 90% credible interval. ',
            'For a less strict threshold, try 80%.')
  }
  else if(credible_interval %in% c("80%", "80")){
    credible_interval = "80%"
    message('Reliability uses relative precision on the 80% credible interval. ',
            'This is the least strict threshold available, and is accepted in the ',
            'small-area literature, but state the threshold wherever the map is published.')
  }


  if(!all(c("fin_year") %in% names(final_map_data))){

    #---------------------------------------------------
    #build the maps
    #---------------------------------------------------
    if(is.null(fill_title)){
      fill_title = paste0("Smoothed disease-related rate, Birmingham", "\nin ", final_map_data$period)
    } else {
      fill_title
    }


    #---------------------------------------------------
    #build the maps
    #---------------------------------------------------
    ci = sub("%$", "", credible_interval)

    #---------------------------------------------------
    #suppress every rate column for unreliable areas
    #---------------------------------------------------
    rel_col = paste0("reliability_", ci)
    if (!rel_col %in% names(final_map_data)) {
      stop("Column `", rel_col, "` not found. Was this produced by smooth_ph_rates()?")
    }

    bad = which(is.na(final_map_data[[rel_col]]) |
                  final_map_data[[rel_col]] == "Unreliable")
    for (col in grep("^asr_", names(final_map_data), value = TRUE)) {
      final_map_data[[col]][bad] = NA
    }

    #---------------------------------------------------
    #keep the user's title; build one only if absent
    #---------------------------------------------------
    if (!is.character(title_in) || length(title_in) != 1 || is.na(title_in)) {
      title_in = ""
    }
    if (!nzchar(title_in)) {
      title_in = paste0("Reliability uses relative precision on the ", ci, "% CrI")
    }

    one_map = function(col, lab) {
      smoothed_tmap_base(final_map_data,
                         fill_value = col,
                         fill_title = lab,
                         title_in   = title_in,
                         title_in_position = title_in_position,
                         palette    = palette,
                         legend_text_size  = legend_text_size,
                         legend_title_size = legend_title_size,
                         legend_position = legend_position,
                         boundary_colour = boundary_colour)
    }

    if (!credible_interval_map) {
      return(one_map(fill_value, fill_title))
    }

    list(asr_map    = one_map(fill_value, fill_title),
         asr_map_LL = one_map(paste0("asr_", ci, "LL"),
                              paste0(fill_title, " (LL ", ci, "%)")),
         asr_map_UL = one_map(paste0("asr_", ci, "UL"),
                              paste0(fill_title, " (UL ", ci, "%)")))

  } else {

    final_map_data = final_map_data %>%
      mutate( fin_year = paste0(if_else(as.numeric(substr(fin_year, 1, 2)) >= 90, "19", "20"),
                                substr(fin_year, 1, 2),
                                "/",
                                substr(fin_year, 3, 4)))

    #---------------------------------------------------
    #build the maps
    #---------------------------------------------------
    if(is.null(fill_title)){
      fill_title = paste0("Smoothed disease-related rate, Birmingham", "\nin ", final_map_data$period)
    } else {
      fill_title
    }


    #---------------------------------------------------
    #build the maps
    #---------------------------------------------------
    ci = sub("%$", "", credible_interval)

    #---------------------------------------------------
    #suppress every rate column for unreliable areas
    #---------------------------------------------------
    rel_col = paste0("reliability_", ci)
    if (!rel_col %in% names(final_map_data)) {
      stop("Column `", rel_col, "` not found. Was this produced by smooth_ph_rates()?")
    }

    bad = which(is.na(final_map_data[[rel_col]]) |
                  final_map_data[[rel_col]] == "Unreliable")
    for (col in grep("^asr_", names(final_map_data), value = TRUE)) {
      final_map_data[[col]][bad] = NA
    }

    #---------------------------------------------------
    #keep the user's title; build one only if absent
    #---------------------------------------------------
    if (!is.character(title_in) || length(title_in) != 1 || is.na(title_in)) {
      title_in = ""
    }
    if (!nzchar(title_in)) {
      title_in = paste0("Reliability uses relative precision on the ", ci, "% CrI")
    }

    title_in_position = tm_pos_out(cell.h = "center", cell.v = "top",
                                   pos.h  = 0.01, pos.v = 1, just.h = "left", just.v = "top")

    one_map = function(col, lab) {
      smoothed_tmap_base_st(final_map_data,
                            fill_value = col,
                            fill_title = lab,
                            title_in   = title_in,
                            title_in_position = title_in_position,
                            palette    = palette,
                            legend_text_size  = legend_text_size,
                            legend_title_size = legend_title_size,
                            boundary_colour = boundary_colour)+
        tm_facets("fin_year", ncol = fn_col, nrow = fn_row)+
        tm_credits(
          text     = paste0("Contains OS data © Crown copyright and database right ", format(Sys.Date(), "%Y"), ". Source: Office for National Statistics licensed under the Open Government Licence v.3.0."),
          size     = 1,
          color    = "grey30",
          position = tm_pos_out(cell.h = "center", cell.v = "bottom", pos.h = "left", pos.v = "top", just.h = "left")
        )
    }


    if (!credible_interval_map) {
      return(one_map(fill_value, fill_title))
    }

    list(asr_map    = one_map(fill_value, fill_title),
         asr_map_LL = one_map(paste0("asr_", ci, "LL"),
                              paste0(fill_title, " (LL ", ci, "%)")),
         asr_map_UL = one_map(paste0("asr_", ci, "UL"),
                              paste0(fill_title, " (UL ", ci, "%)")))
  }
}
