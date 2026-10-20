#' Map the exceedance probability against the Birmingham average
#'
#' Draws a categorical choropleth of `ex_prob_label`, the banded probability
#' that an area's smoothed rate exceeds the city mean.
#'
#' @param final_map_data Output of [smooth_ph_rates()] - a list with `fit`,
#'   `final_result`, `map`, `area_col` and `ex_cols`.
#' @param fill_value Column holding the banded exceedance labels.
#' @param fill_title Map title.
#' @param legend_text_size,legend_title_size Relative text sizes.
#' @param legend_position A tmap position object.
#' @param boundary_colour Colour of the polygon outlines.
#' @returns A `tmap` object.
#' @importFrom tmap tm_shape tm_polygons tm_scale_categorical tm_legend tm_layout
#' @importFrom tmap tm_title tm_compass tm_credits tm_pos_in
#' @export
plot_exceedance_prob = function(final_map_data = NULL,
                                fill_value = "ex_prob_label",
                                fill_title = "Exceedance probability that the rate is greater than \nBirmingham average",
                                legend_text_size = 0.6,
                                legend_title_size = 0.7,
                                legend_position = tm_pos_in(pos.h = 0.02, pos.v = 1.01),
                                boundary_colour = "grey70"){

  if (is.null(final_map_data)) {
    stop("Please supply the smoothing output.")
  } else if (!all(c("fit", "final_result", "map", "area_col", "ex_cols") %in% names(final_map_data))) {
    stop("Wrong list supplied - use smooth_ph_rates() to obtain it.")
  }

  ex_cols        = final_map_data$ex_cols
  final_map_data = final_map_data$final_result


  ex_map = tm_shape(final_map_data) +
    tm_polygons(
      fill        = fill_value,
      fill.scale = tm_scale_categorical(values = ex_cols),
      fill.legend = tm_legend(title = "Pr>bham mean", frame = FALSE, group_id = "legend"),
      col         = boundary_colour
    ) +
    #------------------------------------------------------
  #legend positions
  #------------------------------------------------------
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
    tm_compass(type = "8star", size = 4, color.light = "white",
               position = c("RIGHT", "bottom")) +
    tm_credits(
      text = paste0(
        "Contains OS data © Crown copyright and database right ",
        format(Sys.Date(), "%Y"),
        ". Source:\nOffice for National Statistics licensed under the Open Government Licence v.3.0."
      ),
      position = c("LEFT", "BOTTOM")
    )

  return(ex_map)
}
