GeomHurricane <- ggproto("GeomHurricane", GeomPolygon,
                   required_aes =c("x", "y", "r_ne", "r_se", "r_nw", "r_sw",
                                   "fill", "color"),
                   default_aes = aes(scale_radii = 1, alpha = 0.8)
)

geom_hurricane <- function(mapping = NULL, data = NULL, 
                                        position = "identity", na.rm = FALSE, show.legend = NA, 
                                        inherit.aes = TRUE, ...) {
    layer(
        stat = StatHurricane, geom = GeomHurricane, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}