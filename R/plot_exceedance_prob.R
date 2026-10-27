#' Map the exceedance probability against the Birmingham average
#'
#' Draws a categorical choropleth of `ex_prob_label`, the banded probability
#' that an area's smoothed rate exceeds the city mean. Accepts output from
#' either [smooth_ph_rates()] or [smooth_ph_rates_st()]. For age-space-time
#' output (a `fin_year` column is present), the map is faceted by financial
#' year, with the subtitle and legend drawn in each panel.
#'
#' @param final_map_data The full list returned by [smooth_ph_rates()] or
#'   [smooth_ph_rates_st()] (must contain `fit`, `final_result`, `map`,
#'   `area_col` and `ex_cols`).
#' @param fill_value Column holding the banded exceedance labels.
#' @param fill_title Map title.
#' @param title_in Subtitle drawn in grey in each panel of the age-space-time
#'   map. Defaults to `""` (none). Ignored for single maps.
#' @param title_in_position,legend_position tmap position objects.
#'   `title_in_position` is replaced by an outside-top position for
#'   age-space-time maps.
#' @param legend_text_size,legend_title_size Relative text sizes.
#' @param boundary_colour Colour of the polygon outlines.
#' @param fn_col,fn_row Number of facet columns and rows when mapping
#'   age-space-time output by financial year. Ignored otherwise.
#' @returns A `tmap` object, faceted by `fin_year` for age-space-time output.
#' @importFrom tmap tm_shape tm_polygons tm_scale_categorical tm_legend tm_layout
#' @importFrom tmap tm_title tm_compass tm_credits tm_pos_in tm_pos_out tm_facets
#' @importFrom dplyr mutate if_else
#' @export
plot_exceedance_prob = function(final_map_data = NULL,
                                fill_value = "ex_prob_label",
                                fill_title = "Exceedance probability that the rate is greater than \nBirmingham average",
                                title_in = "",
                                title_in_position = tm_pos_in(pos.h = 0.025, pos.v = 1.035),
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

  ex_cols        = final_map_data$ex_cols
  final_map_data = final_map_data$final_result

  if (!"fin_year" %in% names(final_map_data)) {

    #---------------------------------------------------
    #single map
    #---------------------------------------------------
    ex_map = tm_shape(final_map_data) +
      tm_polygons(
        fill        = fill_value,
        fill.scale  = tm_scale_categorical(values = ex_cols),
        fill.legend = tm_legend(title = "Pr>bham mean", frame = FALSE, group_id = "legend"),
        col         = boundary_colour
      ) +
      tm_layout(
        frame             = FALSE,
        inner.margins     = c(0.07, 0, 0.01, 0),
        legend.position   = legend_position,
        legend.text.size  = legend_text_size,
        legend.title.size = legend_title_size,
        legend.bg         = FALSE,
        legend.bg.alpha   = 0
      ) +
      tm_title(fill_title) +
      tm_compass(type = "8star", size = 4, color.light = "white", position = c("RIGHT", "bottom")) +
      tm_credits(
        text     = paste0("Contains OS data © Crown copyright and database right ", format(Sys.Date(), "%Y"), ". Source:\nOffice for National Statistics licensed under the Open Government Licence v.3.0."),
        position = c("LEFT", "BOTTOM")
      )

    return(ex_map)

  } else {

    #---------------------------------------------------
    #relabel financial year for facets, e.g. 2324 -> 2023/24
    #---------------------------------------------------
    final_map_data = final_map_data %>%
      mutate(fin_year = paste0(if_else(as.numeric(substr(fin_year, 1, 2)) >= 90, "19", "20"), substr(fin_year, 1, 2), "/", substr(fin_year, 3, 4)))

    #---------------------------------------------------
    #title and legend positions for faceted panels
    #---------------------------------------------------
    title_in_position = tm_pos_out(cell.h = "center", cell.v = "top", pos.h = 0.01, pos.v = 1, just.h = "left", just.v = "top")

    #---------------------------------------------------
    #st map: one framed panel per financial year
    #---------------------------------------------------
    ex_map = tm_shape(final_map_data) +
      tm_polygons(
        fill        = fill_value,
        fill.scale  = tm_scale_categorical(values = ex_cols),
        fill.legend = tm_legend(title = "Pr>bham mean", frame = TRUE, group_id = "legend"),
        col         = boundary_colour
      ) +
      tm_layout(
        frame             = TRUE,
        inner.margins     = c(0.02, 0.2, 0.02, 0.02),
        meta.margins       = c(0.05, 0, 0.08, 0.1),
        legend.text.size  = legend_text_size,
        legend.title.size = legend_title_size,
        legend.bg         = FALSE,
        legend.bg.alpha   = 0
      ) +
      tm_title(fill_title, size = 2, position = tm_pos_out(cell.h = "center", cell.v = "top", pos.h = 0.01, pos.v = 1, just.h = "left", just.v = "top")) +
      tm_title(title_in, color = "grey30", size = 1, position = title_in_position) +
      tm_compass(type = "8star", size = 4, color.light = "white", position = tm_pos_out(pos.h = "right", pos.v = "bottom")) +
      tm_facets("fin_year", ncol = fn_col, nrow = fn_row)+
      tm_credits(
        text     = paste0("Contains OS data © Crown copyright and database right ", format(Sys.Date(), "%Y"), ". Source: Office for National Statistics licensed under the Open Government Licence v.3.0."),
        size     = 1,
        color    = "grey30",
        position = tm_pos_out(cell.h = "center", cell.v = "bottom", pos.h = "left", pos.v = "top", just.h = "left")
      )

    return(ex_map)
  }
}
