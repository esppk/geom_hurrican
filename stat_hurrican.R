StatHurricane <- ggproto("StatHurricane", Stat,
                        required_aes = c("x","y","r_ne","r_se","r_sw","r_nw","fill","color"),
                        default_aes = aes(scale_radii = 1),
                        compute_group = function(data, scales){
        
                            bearing <- data.frame(dir = rep(c("r_ne","r_se","r_sw","r_nw"),each = 90), 
                                bearing_ = c(seq(0,89), seq(90,179),
                                        seq(180,269), seq(270,359)))
                            
                            dat_ <- data %>% 
                                dplyr::select(fill,r_ne,r_se,r_sw,r_nw) %>% 
                                tidyr::gather(dir_, dist_, -1) %>% 
                                dplyr::mutate(dist_ = dist_*1852*scale_radii) %>% 
                                tidyr::spread(fill, dist_) %>% 
                                purrr::map_df(~rep(.x, each  = 90)) %>% 
                                dplyr::bind_cols(bearing) %>% 
                                dplyr::select(-5) %>% 
                                tidyr::gather(fill, dist_, -dir_,-bearing_) %>% 
                                dplyr::mutate(color = fill)
                            
                            points <- geosphere::destPoint(data[1,c("x","y")], dat_$bearing_, 
                                                dat_$dist_)  %>% as.data.frame() %>% 
                                bind_cols(dat_ %>% select(fill, color))  
                            points

                        }
)

stat_hurricane <- function(mapping = NULL, data = NULL, geom = "polygon",
                                          position = "identity", na.rm = FALSE, 
                                          show.legend = NA, inherit.aes = TRUE,...) {
    ggplot2::layer(
        stat = StatHurricane, 
        data = data, 
        mapping = mapping, 
        geom = geom, 
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

