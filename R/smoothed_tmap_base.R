#' Build the base tmap for a smoothed small-area map
#'
#' Internal helper used by [plot_smoothed_map()]. Draws one choropleth with
#' the standard BCC furniture: legend, title, compass and OS/ONS credits.
#'
#' @param final_map_data An sf object with the column named in `fill_value`.
#' @param fill_value Name of the column to colour by.
#' @param fill_title Map title.
#' @param title_in Small note drawn inside the map frame.
#' @param title_in_position,legend_position tmap position objects.
#' @param palette A cols4all palette name, e.g. "brewer.blues".
#' @param legend_text_size,legend_title_size Relative text sizes.
#' @param boundary_colour Colour of the polygon outlines.
#' @returns A `tmap` object.
#' @importFrom tmap tm_shape tm_polygons tm_scale_intervals tm_legend tm_layout
#' @importFrom tmap tm_title tm_title_in tm_compass tm_credits tm_pos_in
#' @keywords internal
smoothed_tmap_base = function(final_map_data,
                              fill_value = "asr_median",
                              fill_title = "Smoothed rate",
                              title_in        = "",
                              title_in_position = tm_pos_in(pos.h = 0.025, pos.v = 1.035),
                              palette = "brewer.blues",
                              legend_text_size = 0.6,
                              legend_title_size = 0.7,
                              legend_position = tm_pos_in(pos.h = 0.02, pos.v = 1.01),
                              boundary_colour = "grey70"){

  base_map = tm_shape(final_map_data) +
    tm_polygons(
      fill        = fill_value,
      fill.scale  = tm_scale_intervals(n = 6, style = "quantile", values = palette, label.na = "Unreliable"),
      fill.legend = tm_legend(title = "per 100,000", frame = FALSE, group_id = "legend"),
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
    tm_title_in(title_in, color = "grey30", size = 0.6, position = title_in_position) +
    tm_compass(type = "8star", size = 4, color.light = "white",
               position = c("RIGHT", "bottom")) +
    tm_credits(
      text = paste0(
        "Contains OS data \u00A9 Crown copyright and database right ",
        format(Sys.Date(), "%Y"),
        ". Source:\nOffice for National Statistics licensed under the Open Government Licence v.3.0."
      ),
      position = c("LEFT", "BOTTOM")
    )

  return(base_map)
}









