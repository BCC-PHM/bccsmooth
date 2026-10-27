#' Build the base tmap for a smoothed age-space-time map
#'
#' Internal helper used by [plot_smoothed_map()]. Draws one choropleth with
#' the standard BCC furniture: quantile legend (6 classes, "per 100,000"),
#' main title, optional subtitle and an 8-point compass.
#'
#' @param final_map_data An sf object with the column named in `fill_value`.
#' @param fill_value Name of the column to colour by. Defaults to
#'   `"asr_median"`. `NA` values are shown in the legend as "Unreliable".
#' @param fill_title Main map title, drawn above the map frame.
#' @param title_in Subtitle text drawn in grey, e.g. the year or age band.
#'   Defaults to `""` (none).
#' @param title_in_position tmap position for `title_in`. Defaults to outside
#'   the frame, top-left, the same position as `fill_title`.
#' @param palette A cols4all palette name, e.g. "brewer.blues".
#' @param legend_text_size,legend_title_size Relative text sizes.
#' @param boundary_colour Colour of the polygon outlines.
#' @returns A `tmap` object.
#' @importFrom tmap tm_shape tm_polygons tm_scale_intervals tm_legend tm_layout
#' @importFrom tmap tm_title tm_compass tm_pos_in tm_pos_out
#' @keywords internal
smoothed_tmap_base_st = function(final_map_data,
                              fill_value = "asr_median",
                              fill_title = "Smoothed rate",
                              title_in        = "",
                              title_in_position = tm_pos_out(cell.h = "center", cell.v = "top",
                                                             pos.h  = 0.01, pos.v = 1, just.h = "left", just.v = "top"),
                              palette = "brewer.blues",
                              legend_text_size = 0.6,
                              legend_title_size = 0.7,
                              boundary_colour = "grey70"){

  base_map = tm_shape(final_map_data) +
    tm_polygons(
      fill        = fill_value,
      fill.scale  = tm_scale_intervals(n = 6, style = "quantile", values = palette, label.na = "Unreliable"),
      fill.legend = tm_legend(title = "per 100,000", frame = TRUE, group_id = "legend"),
      col         = boundary_colour
    ) +
    #------------------------------------------------------
  #legend positions
  #------------------------------------------------------
  tm_layout(
    frame             = TRUE,
    inner.margins      = c(0.02, 0.2, 0.02, 0.02),
    meta.margins       = c(0.05, 0, 0.08, 0.1),
    legend.text.size  = legend_text_size,
    legend.title.size = legend_title_size,
    legend.bg         = FALSE,
    legend.bg.alpha   = 0
  ) +
    tm_title(fill_title, position = tm_pos_out(cell.h = "center", cell.v = "top",
                                    pos.h  = 0.01, pos.v = 1, just.h = "left", just.v = "top"),
              size=2) +
    tm_title(title_in, color = "grey30",  size= 1,
             position = title_in_position ) +
    tm_compass(type = "8star", size = 4, color.light = "white", position = tm_pos_out(pos.h = "right",
                                                                                      pos.v = "bottom"))

  return(base_map)

}
