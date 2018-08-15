#' Geom timeline
#'
#' @details This function creates timeline chart.
#'
#' @import ggplot2
#'
#' @examples
#' raw_data <- eq_read_data()
#' clean_data <- eq_clean_data(raw_data)
#' clean_data %>%
#' filter(COUNTRY %in% c("HUNGARY", "SLOVAKIA", "CZECH REPUBLIC", "CROATIA", "POLAND"), YEAR >= 1500) %>%
#'   ggplot(aes(x = DATE, y = COUNTRY)) +
#'   geom_timeline()
#'
#' @param mapping Mapping
#' @param data Data
#' @param stat Stat
#' @param position Position
#' @param na.rm NA remove
#' @param show.legend Show legend
#' @param inherit.aes Inherit aes
#' @param ... Other parameters
#'
#' @export

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping, data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...)
  )
}

#' Geom timeline ggproto
#'
#' @details The ggproto function for custom timeline chart.
#'
#' @import ggplot2
#' @importFrom grid pointsGrob gpar unit polylineGrob gList
#'
#' @export

GeomTimeline <- ggplot2::ggproto('GeomTimeline', ggplot2::Geom,
                                 required_aes = c('x'),
                                 default_aes = ggplot2::aes(y = NULL, colour = 'red', shape = 19,
                                                            fill = 'red', size = 5, alpha = 0.3),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_scales, coord) {

                                    if (!("y" %in% colnames(data))) {
                                      data$y <- 0.15
                                    }

                                    coords <- coord$transform(data, panel_scales)

                                    points <- grid::pointsGrob(
                                      coords$x, coords$y,
                                      pch = coords$shape, size = unit(coords$size / 4, "char"),
                                      gp = grid::gpar(
                                        col = scales::alpha(coords$colour, coords$alpha),
                                        fill = scales::alpha(coords$colour, coords$alpha)
                                      )
                                    )
                                    y_lines <- unique(coords$y)

                                    lines <- grid::polylineGrob(
                                      x = unit(rep(c(0, 1), each = length(y_lines)), "npc"),
                                      y = unit(c(y_lines, y_lines), "npc"),
                                      id = rep(seq_along(y_lines), 2),
                                      gp = grid::gpar(col = "grey",
                                                      lwd = .pt)
                                    )

                                    grid::gList(points, lines)
                                 }
)



#' Geom timeline label
#'
#' @details This function add label to the created custom timeline chart.
#'
#' @import ggplot2
#'
#' @examples
#' raw_data <- eq_read_data()
#' clean_data <- eq_clean_data(raw_data)
#' clean_data %>%
#' filter(COUNTRY %in% c("JAPAN"), YEAR >= 2000) %>%
#' ggplot(aes(x = DATE)) +
#' geom_timeline()+
#' geom_timeline_label(aes(label = LOCATION_NAME, size = EQ_PRIMARY, nmax = 2, y = 0.15))
#'
#' @param mapping Mapping
#' @param data Data
#' @param stat Stat
#' @param position Position
#' @param na.rm NA remove
#' @param show.legend Show legend
#' @param inherit.aes Inherit aes
#' @param ... Other parameters
#'
#' @export

geom_timeline_label = function(mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", na.rm = FALSE,
                               show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



#' Geom timeline label ggproto
#'
#' @details The ggproto function for custom timeline label.
#'
#' @import ggplot2
#' @importFrom grid pointsGrob gpar unit polylineGrob gList textGrob
#' @import dplyr
#'
#' @export

GeomTimelineLabel = ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                     required_aes = c("x", "label", "size"),
                                     default_aes = ggplot2::aes(y = 0.15, nmax = 5, stroke = 0, colour = "black", alpha = 0.5),
                                     draw_key = ggplot2::draw_key_abline,
                                     setup_data = function(data, params){
                                       line_length = length(unique(data$y))
                                       if (line_length < 1){line_length = 1;}
                                       data$line_length = line_length
                                       data
                                     },

                                     draw_group = function(data, panel_scales, coord){

                                       coords <- coord$transform(data, panel_scales)

                                       main_list = grid::gList()

                                       data_point = data.frame(x = coords$x, y = coords$y,
                                                               size = coords$size, label = coords$label)

                                       data_point = data_point %>%
                                         dplyr::arrange(desc(size)) %>%
                                         utils::head(coords$nmax[1])

                                       for (i in 1:nrow(data_point)){

                                         lG = grid::linesGrob(
                                           rep(data_point[i, "x"], 2),
                                           c(data_point[i, "y"],
                                             data_point[i, "y"] + 0.15/coords$line_length),
                                           gp = grid::gpar(col = scales::alpha(coords$colour, coords$alpha/2))
                                         )

                                         tG = grid::textGrob(
                                           data_point[i, "label"],
                                           data_point[i, "x"],
                                           data_point[i, "y"] + 0.18/coords$line_length,
                                           rot = 30,
                                           just = c("left", "bottom"),
                                           gp = grid::gpar(col="grey20", fontsize=12, fontface = "plain")
                                         )

                                         main_list = grid::gList(main_list, lG, tG)
                                       }

                                       main_list
                                     }
)
