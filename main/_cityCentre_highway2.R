## get city centre

# # 国贸
bj_centre2 <- data.frame(name = "Beijing Centre",
                        geometry = st_sfc(st_point(c(gcj2wgs(116.461775,39.909357)$trans_lng,
                                                     gcj2wgs(116.461775,39.909357)$trans_lat)))) %>%
  st_as_sf(crs = 4326) %>%
  st_transform(crs = st_crs(bj_pop))



# # 环贸iAPM
sh_centre2 <- data.frame(name = "Shanghai Centre",
                        geometry = st_sfc(st_point(c(gcj2wgs(121.458129,31.215404)$trans_lng,
                                                     gcj2wgs(121.458129,31.215404)$trans_lat)))) %>%
  st_as_sf(crs = 4326) %>%
  st_transform(crs = st_crs(sh_pop))



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




city4_highway_density2 <- data.frame()
for (i in seq(5000, 20000, 100)) {
  svMisc::progress(i, 20000)
  
  .bj_centre_buffer <- st_buffer(bj_centre2, i)
  .bj_centre_area <- .bj_centre_buffer %>%
    st_intersection(beijing) %>%
    st_area()
  .bj_centre_length <- bj_highway %>%
    st_intersection(.bj_centre_buffer) %>%
    dplyr::summarise() %>%
    st_length()
  .bj_centre_density <-
    (.bj_centre_length / 1000) / (.bj_centre_area / 1000000)
  
  .sh_centre_buffer <- st_buffer(sh_centre2, i)
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
  
  city4_highway_density2 <-
    rbind(city4_highway_density2, .city_highway_density)
}

saveRDS(city4_highway_density2, 'data/city4_highway_density2.rds')
