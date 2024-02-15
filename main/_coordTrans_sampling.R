library(data.table)
library(risingCoord)
a = 6378245.0
f = 1 / 298.3
b = a * (1 - f)
ee = 1 - (b * b) / (a * a)

source('~/Documents/R/BeijingShanghai/main/data_clean_map.R')
##
amap_bj_poi <-
  fread(
    '/Users/sousekilyu/Documents/Meta Data/【冉冉的素材小铺】-2022高德poi/2022高德poi/新建文件夹/北京POI数据/北京市POI数据.csv'
  ) %>%
  rename('lng' = '经度', 'lat' = '纬度') %>%
  mutate(wgs_lng = gcj2wgs(lng, lat)$trans_lng,
         wgs_lat = gcj2wgs(lng, lat)$trans_lat,) %>%
  sf::st_as_sf(coords = c("wgs_lng", "wgs_lat"), crs = 4326) %>%
  st_transform(crs = st_crs(bj_circle)) %>%
  st_intersection(bj_circle) %>%
  st_intersection(bj_pop)

amap_sh_poi <-
  fread(
    '/Users/sousekilyu/Documents/Meta Data/高德poi/2022高德poi/上海POI数据/上海市POI数据.csv'
  ) %>%
  rename('lng' = '经度', 'lat' = '纬度') %>%
  mutate(wgs_lng = gcj2wgs(lng, lat)$trans_lng,
         wgs_lat = gcj2wgs(lng, lat)$trans_lat,) %>%
  sf::st_as_sf(coords = c("wgs_lng", "wgs_lat"), crs = 4326) %>%
  st_transform(crs = st_crs(sh_circle)) %>%
  st_intersection(sh_circle) %>%
  st_intersection(sh_pop)

### points sampling
set.seed(8584)
pointWeightSampling <- function(dt, n) {
  dt2 <- dt[sample.int(nrow(dt),
                       n,
                       replace = FALSE, prob = dt$population),]
  return(dt2)
}

N = 10000
list_name_id <- sample(N:(N * 9), 2 * N, replace = FALSE)

#
bj_amap_sample_list <-
  replicate(N, pointWeightSampling(amap_bj_poi, 2), simplify = FALSE)
names(bj_amap_sample_list) <- list_name_id[1:(length(list_name_id) / 2)]
bj_amap_sample_df <-
  do.call(rbind, Map(cbind, group = names(bj_amap_sample_list), bj_amap_sample_list))
#
sh_amap_sample_list <-
  replicate(N, pointWeightSampling(amap_sh_poi, 2), simplify = FALSE)
names(sh_amap_sample_list) <- list_name_id[(length(list_name_id) / 2+1):length(list_name_id)]
sh_amap_sample_df <-
  do.call(rbind, Map(cbind, group = names(sh_amap_sample_list), sh_amap_sample_list))
bj_amap_sample_df$poi_code <- as.character(sample(10000000:49999999, 20000, replace = FALSE))
sh_amap_sample_df$poi_code <- as.character(sample(50000000:99999999, 20000, replace = FALSE))


saveRDS(amap_bj_poi, 'data/amap_bj_poi.rds')
saveRDS(bj_amap_sample_list, 'data/bj_amap_sample_list.rds')
saveRDS(bj_amap_sample_df, 'data/bj_amap_sample_df.rds')
saveRDS(amap_sh_poi, 'data/amap_sh_poi.rds')
saveRDS(sh_amap_sample_list, 'data/sh_amap_sample_list.rds')
saveRDS(sh_amap_sample_df, 'data/sh_amap_sample_df.rds')

#
# # #点抽样是否符合预期
# ggplot() +
#   geom_spatraster_rgb(data = bj_basemap) +
#   geom_sf(
#     data = bj_poi_sample_df,
#     color = "#F16E67",
#     alpha = .3,
#     size = 1
#   ) +
#   stat_density_2d(
#     data = bj_poi_sample_df,
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
