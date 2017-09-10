StatHurrican <- ggproto("StatHurrican", Stat,
                        required_aes = c("lat","lon","r_ne","r_se","r_sw","r_nw", "fill", "color","scale_radii"),
                        compute_group = function(data, scales){
                            len <- nrow(data)
                            bearing <- data.frame(dir_ = rep(c("r_ne","r_se","r_sw","r_nw"),each = len), 
                                bearing_ = c(seq(0,90,length.out = len), seq(90,180,length.out = len),
                                        seq(180,270,length.out = len), seq(270,360,length.out = len)))
                            
                            data <- data %>% 
                                tidyr::gather(dir_, dist_, -lat,-lon,-fill) %>% 
                                dplyr::mutate(dist_ = dist_*1852*scale_radii) %>% 
                               
                                dplyr::left_join(bearing, by = dir_) 
                            
                            coord = data %>% select(lat, lon) %>% as.matrix()
                            bearing <- data %>% select(bearing_) %>% as.vector()
                            dist_ <- data %>% select(dist_) %>% as.vector()
                            geosphere::destPoint(coord, bearing, dist_)

                        }
)

stat_hurrican <- stat_confint <- function(mapping = NULL, data = NULL, geom = "polygon",
                                          position = "identity", na.rm = FALSE, 
                                          show.legend = NA, inherit.aes = TRUE, scale_radii = 1,...) {
    ggplot2::layer(
        stat = StatHurrican, 
        data = data, 
        mapping = mapping, 
        geom = geom, 
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

