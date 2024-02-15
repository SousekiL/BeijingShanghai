source('~/Documents/R/BeijingShanghai/main/data_clean_map.R')
## get density
bj_pop_sum <- bj_pop_dt %>%
  mutate(city = "北京") %>%
  group_by(city) %>%
  summarise(population_sum = sum(population))
sh_pop_sum <- sh_pop_dt %>%
  mutate(city = "上海") %>%
  group_by(city) %>%
  summarise(population_sum = sum(population))
.pop_sum <- rbind(bj_pop_sum, sh_pop_sum)

city_pop_density <- data.frame()
for (i in seq(1000, 25000, 1000)) {
  svMisc::progress(i, 25000)
  
  .bj_centre_buffer <- st_buffer(bj_centre, i)
  .bj_centre_pop <- bj_pop_dt %>%
    st_intersection(.bj_centre_buffer) %>%
    dplyr::summarise(population_sum = sum(population)) %>%
    st_drop_geometry()
  
  .sh_centre_buffer <- st_buffer(sh_centre, i)
  .sh_centre_pop <- sh_pop_dt %>%
    st_intersection(.sh_centre_buffer) %>%
    dplyr::summarise(population_sum = sum(population)) %>%
    st_drop_geometry()
  
  
  .city_pop_density <- rbind(
    data.frame(
      buffer = i,
      city = "北京",
      population = as.numeric(.bj_centre_pop)
    ),
    data.frame(
      buffer = i,
      city = "上海",
      population = as.numeric(.sh_centre_pop)
    )
  )
  
  city_pop_density <-
    rbind(city_pop_density, .city_pop_density)
}

bj_sh_pop_density <- city_pop_density %>%
  left_join(.pop_sum) 
saveRDS(bj_sh_pop_density, 'data/bj_sh_pop_density.rds')
