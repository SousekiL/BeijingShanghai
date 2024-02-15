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
library(risingCoord)
a = 6378245.0
f = 1 / 298.3
b = a * (1 - f)
ee = 1 - (b * b) / (a * a)


bj_pop <- st_read('data/bj_population_density.shp')
sh_pop <- st_read('data/sh_population_density.shp')

bj_circle <- st_read('data/北京五环.shp')
sh_circle <- st_read('data/上海外环.shp')

bj_circle <- st_transform(bj_circle, crs = st_crs(bj_pop))
sh_circle <- st_transform(sh_circle, crs = st_crs(sh_pop))

bj_circle_highway <- st_read('data/北京五环路网.shp') %>%
  st_transform(crs = st_crs(bj_pop)) %>%
  dplyr::filter(
    highway %in% c(
      "primary",
      "secondary",
      "trunk",
      "tertiary" ,
      "secondary_link",
      "trunk_link",
      "primary_link" ,
      "motorway"     ,
      "tertiary_link" ,
      "cycleway" ,
      "motorway_link"
    )
  )
sh_circle_highway <- st_read('data/上海外环路网.shp') %>%
  st_transform(crs = st_crs(sh_pop)) %>%
  dplyr::filter(
    highway %in% c(
      "primary",
      "secondary",
      "trunk",
      "tertiary" ,
      "secondary_link",
      "trunk_link",
      "primary_link" ,
      "motorway"     ,
      "tertiary_link" ,
      "cycleway" ,
      "motorway_link"
    )
  )


bj_highway <- st_read('data/北京路网.shp') %>%
  st_transform(crs = st_crs(bj_pop)) %>%
  dplyr::filter(
    highway %in% c(
      "primary",
      "secondary",
      "trunk",
      "tertiary" ,
      "secondary_link",
      "trunk_link",
      "primary_link" ,
      "motorway"     ,
      "tertiary_link" ,
      "cycleway" ,
      "motorway_link"
    )
  )
sh_highway <- st_read('data/上海路网.shp') %>%
  st_transform(crs = st_crs(sh_pop)) %>%
  dplyr::filter(
    highway %in% c(
      "primary",
      "secondary",
      "trunk",
      "tertiary" ,
      "secondary_link",
      "trunk_link",
      "primary_link" ,
      "motorway"     ,
      "tertiary_link" ,
      "cycleway" ,
      "motorway_link"
    )
  )
tj_highway <- st_read('data/天津路网.shp') %>%
  st_transform(crs = st_crs(bj_pop)) %>%
  dplyr::filter(
    highway %in% c(
      "primary",
      "secondary",
      "trunk",
      "tertiary" ,
      "secondary_link",
      "trunk_link",
      "primary_link" ,
      "motorway"     ,
      "tertiary_link" ,
      "cycleway" ,
      "motorway_link"
    )
  )

hz_highway <- st_read('data/杭州路网.shp') %>%
  st_transform(crs = st_crs(sh_pop)) %>%
  dplyr::filter(
    highway %in% c(
      "primary",
      "secondary",
      "trunk",
      "tertiary" ,
      "secondary_link",
      "trunk_link",
      "primary_link" ,
      "motorway"     ,
      "tertiary_link" ,
      "cycleway" ,
      "motorway_link"
    )
  )

## get city centre
bj_centre <- data.frame(name = "Beijing Centre",
                        geometry = st_sfc(st_point(c(12956614, 4852458)))) %>%
  st_as_sf(crs = st_crs(bj_pop))

# # 国贸
# bj_centre <- data.frame(name = "Beijing Centre",
#                         geometry = st_sfc(st_point(c(gcj2wgs(116.461775,39.909357)$trans_lng,
#                                                      gcj2wgs(116.461775,39.909357)$trans_lat)))) %>%
#   st_as_sf(crs = 4326) %>%
#   st_transform(crs = st_crs(bj_pop))


sh_centre <- data.frame(name = "Shanghai Centre",
                        geometry = st_sfc(st_point(c(13523683, 3664072)))) %>%
  st_as_sf(crs = st_crs(sh_pop))

# # 环贸iAPM
# sh_centre <- data.frame(name = "Shanghai Centre",
#                         geometry = st_sfc(st_point(c(gcj2wgs(121.458129,31.215404)$trans_lng,
#                                                      gcj2wgs(121.458129,31.215404)$trans_lat)))) %>%
#   st_as_sf(crs = 4326) %>%
#   st_transform(crs = st_crs(sh_pop))



tj_centre <- data.frame(name = "Tianjin Centre",
                        geometry = st_sfc(st_point(c(gcj2wgs(117.200565,39.126661)$trans_lng,
                                                     gcj2wgs(117.200565,39.126661)$trans_lat)))) %>%
  st_as_sf(crs = 4326) %>%
  st_transform(crs = st_crs(bj_pop))

hz_centre <- data.frame(name = "Hangzhou Centre",
                        geometry = st_sfc(st_point(c(gcj2wgs(120.163325,30.271001)$trans_lng,
                                                     gcj2wgs(120.163325,30.271001)$trans_lat)))) %>%
  st_as_sf(crs = 4326) %>%
  st_transform(crs = st_crs(sh_pop))
  
  
beijing <- st_read('data/北京市.shp') %>%
  st_transform(crs = st_crs(bj_pop))
shanghai <- st_read('data/上海市.shp') %>%
  st_transform(crs = st_crs(sh_pop))
tianjin <- st_read('data/天津市.shp') %>%
  st_transform(crs = st_crs(bj_pop))
hangzhou <- st_read('data/杭州市.shp') %>%
  st_transform(crs = st_crs(sh_pop))

# st_crop
bj_pop_dt <- st_intersection(bj_pop, bj_circle)
sh_pop_dt <- st_intersection(sh_pop, sh_circle)

### basemap
# https://jakob.schwalb-willmann.de/basemaps/
#get_maptypes()
bj_bbox <- st_bbox(bj_circle)
bj_basemap <- basemap_raster(bj_bbox, map_service = "osm", map_type = "streets")
#bj_basemap <- tmaptools::read_osm(bj_bbox, type = "osm")

bj_basemap <- rast(bj_basemap)

sh_bbox <- st_bbox(sh_circle)
sh_basemap <- basemap_raster(sh_bbox, map_service = "osm", map_type = "streets")
sh_basemap <- rast(sh_basemap)

bj_circle2 <- st_read('data/北京五环basemap.shp')
sh_circle2 <- st_read('data/上海外环basemap.shp')
bj_circle2 <- st_transform(bj_circle2, crs = st_crs(bj_pop))
sh_circle2 <- st_transform(sh_circle2, crs = st_crs(sh_pop))
bj_basemap2 <- basemap_raster(st_bbox(bj_circle2), map_service = "osm", map_type = "streets")
#tmaptools::read_osm(st_bbox(bj_circle2), type = "osm")
sh_basemap2 <- basemap_raster(st_bbox(sh_circle2), map_service = "osm", map_type = "streets")

bj_circle3 <- st_buffer(bj_circle, 2000)
sh_circle3 <- st_buffer(sh_circle, 2000)

# ### 获取poi和居民点信息
# library(osmdata)
# op = get("has_internet_via_proxy", environment(curl::has_internet))
# np = !is.null(curl::nslookup("r-project.org", error = FALSE))
# assign("has_internet_via_proxy", np, environment(curl::has_internet))
# bj_res <- bj_bbox %>%
#   opq(timeout = 60) %>%
#   add_osm_feature(key = "landuse", value = c("residential")) %>%
#   osmdata_sf()
