setwd('/Users/sousekilyu/Documents/R/BeijingShanghai')
library(ggpubr)
library(sf)
library(ggplot2)
library(basemaps)
library(tidyterra)
library(terra)
library(magick)
library(MetBrewer)
library(colorspace)
library(glue)
library(stringr)
library(combinat)
library(osrm) # to get paths between points
library(purrr)
library(svMisc)
library(pipeR)
library(data.table)
library(risingCoord)
library(magrittr)
a = 6378245.0
f = 1 / 298.3
b = a * (1 - f)
ee = 1 - (b * b) / (a * a)
#setwd("~/Documents/R/BeijingShanghai")
#source("main/data_clean.R")

## 路径规划
u <- "https://routing.openstreetmap.de/"
options(osrm.server = u)

city = 'gz'
##
gz_circle <- st_read('data/广州城区.shp')
china_pop <- st_read('/Users/sousekilyu/Documents/Meta Data/人口数据/中国人口数据/中国人口数据（带城市聚合）.shp')
gz_circle <- st_transform(gz_circle, crs = st_crs(china_pop))
gz_pop <- st_intersection(china_pop, gz_circle)

amap_gz_poi <-
  fread('/Users/sousekilyu/Documents/Meta Data/高德poi/2022高德poi/广东省POI数据/广州市POI数据.csv') %>%
  rename('lng' = '经度', 'lat' = '纬度') %>%
  mutate(wgs_lng = gcj2wgs(lng, lat)$trans_lng,
         wgs_lat = gcj2wgs(lng, lat)$trans_lat,
  ) %>%
  sf::st_as_sf(coords = c("wgs_lng", "wgs_lat"), crs = 4326) %>%
  st_transform(crs = st_crs(gz_circle)) %>%
  st_intersection(gz_circle) %>%
  st_intersection(gz_pop)

### points sampling
set.seed(8584)
pointWeightSampling <- function(dt, n) {
  dt2 <- dt[sample.int(nrow(dt),
                       n,
                       replace = FALSE, prob = dt$population), ]
  return(dt2)
}

N = 10000
list_name_id <- paste0(city, sample(N:(N * 9), N*2, replace = FALSE))

#
gz_amap_sample_list <-
  replicate(N, pointWeightSampling(amap_gz_poi, 2), simplify = FALSE)
names(gz_amap_sample_list) <-
  list_name_id[1:(length(list_name_id) / 2)]
gz_amap_sample_df <-
  do.call(rbind, Map(cbind, group = names(gz_amap_sample_list), gz_amap_sample_list))
gz_amap_sample_df$poi_code <-
  paste0(city, as.character(sample(10000000:49999999, 20000, replace = FALSE)))
  

saveRDS(amap_gz_poi, glue('data/amap_{city}_poi.rds'))
saveRDS(gz_amap_sample_list, glue('data/{city}_amap_sample_list.rds'))
saveRDS(gz_amap_sample_df, glue('data/{city}_amap_sample_df.rds'))

##
######################
city_sample_list <- gz_amap_sample_list

## 直线距离
.dist <- lapply(city_sample_list, function(x) {
  st_distance(x[1,],
              x[2,])
})
.dist_4326 <- lapply(city_sample_list, function(x) {
  st_distance(st_transform(x[1,], crs = 4326),
              st_transform(x[2,], crs = 4326))
})
.city_dist_df <- .dist %>%
  {
    do.call(rbind, Map(cbind, group = names(.), .))
  } %>%
  data.frame() %>%
  dplyr::rename(dist_km = V2) %>%
  dplyr::mutate(dist_km = as.numeric(as.character(dist_km)) / 1000)
.city_dist_4326_df <- .dist_4326 %>%
  {
    data.frame(do.call(rbind, Map(cbind, group = names(.), .)))
  } %>%
  dplyr::rename(dist_4326_km = V2) %>%
  dplyr::mutate(dist_4326_km = as.numeric(as.character(dist_4326_km)) /
                  1000)
.city_dist_df2  <-  dplyr::left_join(.city_dist_df, .city_dist_4326_df)

###
saveRDS(.city_dist_df2, file = glue("data/{city}_amap_dis.rds"))

## 路径规划
# foot
.foot_dist <- list()
.car_dist <- list()
k = 1
j = 1
for (i in seq(1, 10000, 500)) {
  progress(i, length(city_sample_list))
  city_sample_list1 <- city_sample_list[i:(i + 499)]
  
  err <-
    try(.foot_dist_tmp <- lapply(city_sample_list1, function(x) {
      osrmRoute(src = x[1,],
                dst = x[2,],
                osrm.profile = "foot")
    }),
    TRUE)
  while (k <= 30 & inherits(err, 'try-error') == TRUE) {
    err <- try(.foot_dist_tmp <- lapply(city_sample_list1, function(x) {
      osrmRoute(src = x[1,],
                dst = x[2,],
                osrm.profile = "foot")
    }),
    TRUE)
    k = k + 1
    Sys.sleep(sample(5:30, 1))
  }
  .foot_dist <- append(.foot_dist, .foot_dist_tmp)
  
  err2 <-
    try(.car_dist_tmp <- lapply(city_sample_list1, function(x) {
      osrmRoute(src = x[1,],
                dst = x[2,],
                osrm.profile = "car")
    }),
    TRUE)
  while (j <= 30 & inherits(err2, 'try-error') == TRUE) {
    err2 <- try(.car_dist_tmp <- lapply(city_sample_list1, function(x) {
      osrmRoute(src = x[1,],
                dst = x[2,],
                osrm.profile = "car")
    }),
    TRUE)
    j = j + 1
    Sys.sleep(sample(5:30, 1))
  }
  .car_dist <- append(.car_dist, .car_dist_tmp)
}

.foot_dist_df <- .foot_dist %>%
  {
    do.call(rbind, Map(cbind, group = names(.), .))
  } %>%
  data.frame() %>%
  dplyr::rename(foot_dur = duration,
                foot_dist = distance) %>%
  dplyr::select(group, foot_dur, foot_dist, geometry)
.car_dist_df <- .car_dist %>%
  {
    do.call(rbind, Map(cbind, group = names(.), .))
  } %>%
  data.frame() %>%
  dplyr::rename(car_dur = duration,
                car_dist = distance) %>%
  dplyr::select(group, car_dur, car_dist, geometry)

####

saveRDS(.foot_dist_df, file = glue("data/{city}_amap_foot_dis.rds"))
saveRDS(.car_dist_df, file = glue("data/{city}_amap_car_dis.rds"))