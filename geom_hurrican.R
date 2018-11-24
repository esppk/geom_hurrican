GeomHurricane <- ggproto("GeomHurricane", GeomPolygon,
                   required_aes =c("x", "y", "ne", "se", "nw", "sw", "colour"),
                   default_aes = aes(fill = NA, size = 0.5, linetype = 1,
                                     alpha = NA)
)

geom_hurricane <- function(mapping = NULL, data = NULL,
                                        position = "identity", na.rm = FALSE, show.legend = NA,
                                        inherit.aes = TRUE, ...) {
    ggplot2::layer(
        stat = StatHurricane, geom = GeomHurricane, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}



