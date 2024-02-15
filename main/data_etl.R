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
library(data.table)

source("~/Documents/R/BeijingShanghai/main/data_clean_map.R")
amap_bj_poi <- readRDS('data/amap_bj_poi.rds')
amap_sh_poi <- readRDS('data/amap_sh_poi.rds')

# route
bj_amap_sample_list <- readRDS('data/bj_amap_sample_list.rds')
bj_amap_sample_df <- readRDS('data/bj_amap_sample_df.rds')
sh_amap_sample_list <- readRDS('data/sh_amap_sample_list.rds')
sh_amap_sample_df <- readRDS('data/sh_amap_sample_df.rds')

bj_amap_dis <- readRDS("data/bj_amap_dis.rds")
sh_amap_dis <- readRDS("data/sh_amap_dis.rds")
gz_amap_dis <- readRDS("data/gz_amap_dis.rds")

bj_amap_car_dis <- readRDS("data/bj_amap_car_dis.rds")
sh_amap_car_dis <- readRDS("data/sh_amap_car_dis.rds")
gz_amap_car_dis <- readRDS("data/gz_amap_car_dis.rds")

bj_amap_foot_dis <- readRDS("data/bj_amap_foot_dis.rds")
sh_amap_foot_dis <- readRDS("data/sh_amap_foot_dis.rds")
gz_amap_foot_dis <- readRDS("data/gz_amap_foot_dis.rds")


bj_dis_by_car <- bj_amap_dis %>%
  left_join(bj_amap_car_dis) %>%
  mutate(city = "北京", by = "car") %>%
  st_as_sf()
sh_dis_by_car  <- sh_amap_dis %>%
  left_join(sh_amap_car_dis) %>%
  mutate(city = "上海", by = "car") %>%
  st_as_sf()
gz_dis_by_car  <- gz_amap_dis %>%
  left_join(gz_amap_car_dis) %>%
  mutate(city = "广州", by = "car") %>%
  st_as_sf()

dis_route_by_car <- rbind(bj_dis_by_car,
                          sh_dis_by_car) %>%
  mutate(detour_index = car_dist / dist_4326_km) %>%
  group_by(city) %>%
  mutate(detour_rank = dplyr::row_number(-detour_index)) %>%
  ungroup()
dis_route_by_car3 <- rbind(bj_dis_by_car,
                          sh_dis_by_car,
                          gz_dis_by_car) %>%
  mutate(detour_index = car_dist / dist_4326_km) %>%
  group_by(city) %>%
  mutate(detour_rank = dplyr::row_number(-detour_index)) %>%
  ungroup()


bj_dis_by_foot <- bj_amap_dis %>%
  left_join(bj_amap_foot_dis) %>%
  mutate(city = "北京", by = "foot") %>%
  st_as_sf()
sh_dis_by_foot  <- sh_amap_dis %>%
  left_join(sh_amap_foot_dis) %>%
  mutate(city = "上海", by = "foot") %>%
  st_as_sf()
gz_dis_by_foot  <- gz_amap_dis %>%
  left_join(gz_amap_foot_dis) %>%
  mutate(city = "广州", by = "foot") %>%
  st_as_sf()

dis_route_by_foot <- rbind(bj_dis_by_foot,
                           sh_dis_by_foot) %>%
  mutate(detour_index = foot_dist / dist_4326_km) %>%
  group_by(city) %>%
  mutate(detour_rank = dplyr::row_number(-detour_index)) %>%
  ungroup()
dis_route_by_foot3 <- rbind(bj_dis_by_foot,
                            sh_dis_by_foot,
                            gz_dis_by_foot) %>%
  mutate(detour_index = foot_dist / dist_4326_km) %>%
  group_by(city) %>%
  mutate(detour_rank = dplyr::row_number(-detour_index)) %>%
  ungroup()

### join 地名
## 所有poi点
poi_points_geo <- rbind(amap_bj_poi %>%
                          mutate(city = "北京") ,
                        amap_sh_poi %>%
                          mutate(city = "上海"))
poi_points <- poi_points_geo %>% st_drop_geometry()
## 被抽样中的路线对应的点
city_amap_sample_df <- rbind(
  bj_amap_sample_df %>%
    mutate(city = "北京"),
  sh_amap_sample_df %>%
    mutate(city = "上海")
)
city_amap_sample_df_s <- city_amap_sample_df %>%
  dplyr::group_by(city, group) %>%
  dplyr::summarise()

# #数据文件，非geo
# poi_amap_sample_df_geo <- rbind(
#   bj_amap_sample_df %>%
#     dplyr::select(group, 名称, poi_code) %>%
#     group_by(group) %>%
#     mutate(
#       city = "北京",
#       name2 = dplyr::dplyr::lead(name, n = 1),
#       rn = dplyr::row_number()
#     ) %>%
#     ungroup() %>%
#     dplyr::rename(osm_from = name,
#                   osm_to = name2) %>%
#     dplyr::filter(rn == 1) %>%
#     dplyr::select(group, city, osm_from, osm_to),
#   sh_amap_sample_df %>%
#     dplyr::select(group, 名称, poi_code) %>%
#     group_by(group) %>%
#     mutate(
#       city = "上海",
#       name2 = dplyr::dplyr::lead(name, n = 1),
#       rn = dplyr::row_number()
#     ) %>%
#     ungroup() %>%
#     dplyr::rename(osm_from = name,
#                   osm_to = name2) %>%
#     dplyr::filter(rn == 1) %>%
#     dplyr::select(group, city, osm_from, osm_to)
# )
# poi_sample_df <- poi_sample_df_geo %>% st_drop_geometry()
# dis_route_by_car_poi <-
#   left_join(dis_route_by_car, poi_sample_df)
# dis_route_by_foot_poi <-
#   left_join(dis_route_by_foot, poi_sample_df)


## 人口密度
pop_dt <- st_as_sf(rbind(
  data.frame(bj_pop_dt,
             city = "北京"),
  data.frame(sh_pop_dt,
             city = "上海")
))


### points to line
# 抽样点之间的直线连线

# city_str_lines <- rbind(
#   bj_amap_sample_df %>%
#     group_by(group) %>%
#     summarize() %>%
#     st_cast("LINESTRING") %>%
#     mutate(city = "北京") ,
#   sh_amap_sample_df %>%
#     group_by(group) %>%
#     summarize() %>%
#     st_cast("LINESTRING") %>%
#     mutate(city = "上海")
# )

library(stplanr) # https://github.com/ropensci/stplanr
.df <-  bj_amap_sample_df %>%
  dplyr::select(poi_code, group)
.tmp <- .df %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(
    poi_code1 = poi_code,
    poi_code2 = dplyr::lead(poi_code1, 1),
    id = paste0(poi_code1, ' ', poi_code2)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(poi_code2)) %>%
  dplyr::select(poi_code1, poi_code2, group) %>%
  st_drop_geometry()
bj_str_lines <- od2line(flow = .tmp, zones = .df)

.df <-  sh_amap_sample_df %>%
  dplyr::select(poi_code, group)
.tmp <- .df %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(
    poi_code1 = poi_code,
    poi_code2 = dplyr::lead(poi_code1, 1),
    id = paste0(poi_code1, ' ', poi_code2)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(poi_code2)) %>%
  dplyr::select(poi_code1, poi_code2, group) %>%
  st_drop_geometry()
sh_str_lines <- od2line(flow = .tmp, zones = .df)
city_str_lines <- rbind(bj_str_lines %>%
                          mutate(city = "北京") ,
                        sh_str_lines %>%
                          mutate(city = "上海"))
# # map_base_bj +
# #   tm_shape(sample_dt_lines[1:5,]) +
# #   tm_lines(col = "grey60",
# #            alpha = .5,
# #            size = .2)
