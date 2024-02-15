
### points sampling
set.seed(8584)
pointWeightSampling <- function(dt, n) {
  dt2 <- dt[sample.int(nrow(dt),
                       n,
                       replace = FALSE, prob = dt$population), ]
  return(dt2)
}
## beijing
# bj_sample_points <- st_sample(bj_circle,
#                               size = 1000000,
#                               type = 'random',
#                               exact = TRUE) %>%
#   st_sf('SID' = seq(length(.)), 'geometry' = .) %>%
#   st_intersection(., bj_circle) %>%
#   st_intersection(bj_pop)

bj_sample_points <-
  st_read(
    '/Users/sousekilyu/Documents/Meta Data/OSM/China-poi/hotosm_chn_east_points_of_interest_points_shp/hotosm_chn_east_points_of_interest_points.shp'
  )
bj_sample_points <- st_transform(bj_sample_points, crs = st_crs(bj_pop))
bj_sample_points %<>%
  st_intersection(bj_circle) %>%
  st_intersection(bj_pop)

N = 10000
list_name_id <- sample(N:(N * 9), 2 * N, replace = FALSE)

bj_sample_list <-
  replicate(N, pointWeightSampling(bj_sample_points, 2), simplify = FALSE)
names(bj_sample_list) <- list_name_id[1:(length(list_name_id) / 2)]
bj_sample_df <-
  do.call(rbind, Map(cbind, group = names(bj_sample_list), bj_sample_list))
## shanghai
sh_sample_points <-
  st_read(
    '/Users/sousekilyu/Documents/Meta Data/OSM/China-poi/hotosm_chn_south2_points_of_interest_points_shp/hotosm_chn_south2_points_of_interest_points.shp'
  )
sh_sample_points <- st_transform(sh_sample_points, crs = st_crs(sh_pop))
sh_sample_points %<>%
  st_intersection(sh_circle) %>%
  st_intersection(sh_pop)
sh_sample_list <-
  replicate(N, pointWeightSampling(sh_sample_points, 2), simplify = FALSE)
names(sh_sample_list) <-
  list_name_id[(length(list_name_id) / 2 + 1):length(list_name_id)]
sh_sample_df <-
  do.call(rbind, Map(cbind, group = names(sh_sample_list), sh_sample_list))

saveRDS(bj_sample_points, 'data/bj_sample_points.rds')
saveRDS(bj_sample_list, 'data/bj_sample_list.rds')
saveRDS(bj_sample_df, 'data/bj_sample_df.rds')
saveRDS(sh_sample_points, 'data/sh_sample_points.rds')
saveRDS(sh_sample_list, 'data/sh_sample_list.rds')
saveRDS(sh_sample_df, 'data/sh_sample_df.rds')

#
# # #点抽样是否符合预期
# ggplot() +
#   geom_spatraster_rgb(data = bj_basemap) +
#   geom_sf(
#     data = bj_sample_df,
#     color = "#F16E67",
#     alpha = .3,
#     size = 1
#   ) +
#   stat_density_2d(
#     data = bj_sample_df,
#     mapping = ggplot2::aes(
#       x = purrr::map_dbl(geometry, ~ .[1]),
#       y = purrr::map_dbl(geometry, ~
#                            .[2]),
#       fill = stat(density)
#     ),
#     geom = 'tile',
#     contour = FALSE,
#     alpha = 0.5
#   ) +
#   scale_fill_viridis_c()
