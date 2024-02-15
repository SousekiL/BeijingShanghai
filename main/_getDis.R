library(osrm) # to get paths between points
library(purrr)
library(svMisc)
library(pipeR)

#setwd("~/Documents/R/BeijingShanghai")
#source("main/data_clean.R")

## 路径规划
u <- "https://routing.openstreetmap.de/"
options(osrm.server = u)

######################
bj_amap_sample_list <- readRDS('data/bj_amap_sample_list.rds')
sh_amap_sample_list <- readRDS('data/sh_amap_sample_list.rds')

######################
city_sample_list <- bj_amap_sample_list
city = 'bj'

## 直线距离
.dist <- lapply(city_sample_list, function(x) {
  st_distance(x[1, ],
              x[2, ])
})
.dist_4326 <- lapply(city_sample_list, function(x) {
  st_distance(st_transform(x[1, ], crs = 4326),
              st_transform(x[2, ], crs = 4326))
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
.city_dist_df %<>% left_join(.city_dist_4326_df)

###
saveRDS(.city_dist_df, file = glue("data/{city}_amap_dis.rds"))

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
      osrmRoute(src = x[1, ],
                dst = x[2, ],
                osrm.profile = "foot")
    }),
    TRUE)
  while (k <= 30 & inherits(err, 'try-error') == TRUE) {
    err <- try(.foot_dist_tmp <- lapply(city_sample_list1, function(x) {
      osrmRoute(src = x[1, ],
                dst = x[2, ],
                osrm.profile = "foot")
    }),
    TRUE)
    k = k + 1
    Sys.sleep(sample(5:30, 1))
  }
  .foot_dist <- append(.foot_dist, .foot_dist_tmp)
  
  err2 <-
    try(.car_dist_tmp <- lapply(city_sample_list1, function(x) {
      osrmRoute(src = x[1, ],
                dst = x[2, ],
                osrm.profile = "car")
    }),
    TRUE)
  while (j <= 30 & inherits(err2, 'try-error') == TRUE) {
    err2 <- try(.car_dist_tmp <- lapply(city_sample_list1, function(x) {
      osrmRoute(src = x[1, ],
                dst = x[2, ],
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

