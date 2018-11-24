StatHurricane <- ggproto("StatHurricane", Stat,

                        #default_aes = aes(scale_radii = 1),
                        compute_group = function(data, scales){

                            bearing <- data.frame(dir = rep(c("r_ne","r_se","r_sw","r_nw"), each = 90),
                                                  bearing_ = c(seq(0,89), seq(90,179),
                                                               seq(180,269), seq(270,359)))

                            dat_ <- data %>%
                                dplyr::rename(wind_speed = fill) %>%
                                dplyr::select(wind_speed, ne,se,sw,nw) %>%
                                tidyr::gather(dir_, dist_, -wind_speed) %>%
                                dplyr::mutate(dist_ = dist_*1852*1) %>%
                                tidyr::spread(wind_speed, dist_) %>%
                                purrr::map_df(~rep(.x, each  = 90)) %>%
                                dplyr::bind_cols(bearing) %>%
                                dplyr::select(-dir) %>%
                                tidyr::gather(wind_speed, dist_, -dir_,-bearing_)

                            points <- geosphere::destPoint(c(23.1, -75.1), dat_$bearing_,
                                                           dat_$dist_)  %>% as.data.frame() %>%
                                bind_cols(dat_ %>% select(wind_speed))

                            #data.frame(x = points$lon, y = points$lat, fill = points$wind_speed)
                            #data.frame(x = dist, y = 8, colour = data$speed)
                            names(points) <- c("x", "y", "fill")
                            points

                        },
                        required_aes = c("ne", "nw", "se", "sw")
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

