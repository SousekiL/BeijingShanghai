source('~/Documents/R/BeijingShanghai/main/data_clean_map.R')
## get density
city4_highway_density <- data.frame()
for (i in seq(1000, 20000, 100)) {
  svMisc::progress(i, 20000)
  
  .bj_centre_buffer <- st_buffer(bj_centre, i)
  .bj_centre_area <- .bj_centre_buffer %>%
    st_intersection(beijing) %>%
    st_area()
  .bj_centre_length <- bj_highway %>%
    st_intersection(.bj_centre_buffer) %>%
    dplyr::summarise() %>%
    st_length()
  .bj_centre_density <-
    (.bj_centre_length / 1000) / (.bj_centre_area / 1000000)
  
  .sh_centre_buffer <- st_buffer(sh_centre, i)
  .sh_centre_area <- .sh_centre_buffer %>%
    st_intersection(shanghai) %>%
    st_area()
  .sh_centre_length <- sh_highway %>%
    st_intersection(.sh_centre_buffer) %>%
    dplyr::summarise() %>%
    st_length()
  .sh_centre_density <-
    (.sh_centre_length / 1000) / (.sh_centre_area / 1000000)
  
  .tj_centre_buffer <- st_buffer(tj_centre, i)
  .tj_centre_area <- .tj_centre_buffer %>%
    st_intersection(tianjin) %>%
    st_area()
  .tj_centre_length <- tj_highway %>%
    st_intersection(.tj_centre_buffer) %>%
    dplyr::summarise() %>%
    st_length()
  .tj_centre_density <-
    (.tj_centre_length / 1000) / (.tj_centre_area / 1000000)
  
  .hz_centre_buffer <- st_buffer(hz_centre, i)
  .hz_centre_area <- .hz_centre_buffer %>%
    st_intersection(hangzhou) %>%
    st_area()
  .hz_centre_length <- hz_highway %>%
    st_intersection(.hz_centre_buffer) %>%
    dplyr::summarise() %>%
    st_length()
  .hz_centre_density <-
    (.hz_centre_length / 1000) / (.hz_centre_area / 1000000)
  
  .city_highway_density <- rbind(
    data.frame(
      buffer = i,
      city = "北京",
      area = .bj_centre_area,
      length = .bj_centre_length,
      density = .bj_centre_density
    ),
    data.frame(
      buffer = i,
      city = "上海",
      area = .sh_centre_area,
      length = .sh_centre_length,
      density = .sh_centre_density
    ),
    data.frame(
      buffer = i,
      city = "天津",
      area = .tj_centre_area,
      length = .tj_centre_length,
      density = .tj_centre_density
    ),
    data.frame(
      buffer = i,
      city = "杭州",
      area = .hz_centre_area,
      length = .hz_centre_length,
      density = .hz_centre_density
    )
  )
  
  city4_highway_density <-
    rbind(city4_highway_density, .city_highway_density)
}

saveRDS(city4_highway_density, 'data/city4_highway_density.rds')
