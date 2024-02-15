library(osrm) # to get paths between points
## Not run: 
library(sf)
apotheke.df <- read.csv(system.file("csv/apotheke.csv", package = "osrm"))
apotheke.sf <- st_read(system.file("gpkg/apotheke.gpkg", package = "osrm"),
                       quiet = TRUE
)
# Travel path between points
route1 <- osrmRoute(src = apotheke.sf[1, ], dst = apotheke.sf[16, ])
# Display paths
plot(st_geometry(route1))
plot(st_geometry(apotheke.sf[c(1, 16), ]), col = "red", pch = 20, add = TRUE)

# Return only duration and distance
route3 <- osrmRoute(
  src = apotheke.df[1, c("lon", "lat")],
  dst = apotheke.df[16, c("lon", "lat")],
  overview = FALSE
)
route3

# Using only coordinates
route4 <- osrmRoute(
  src = c(13.412, 52.502),
  dst = c(13.454, 52.592)
)
plot(st_geometry(route4))

# Using via points
route5 <- osrmRoute(loc = apotheke.sf[c(1, 2, 4, 3), ])
plot(st_geometry(route5), col = "red", lwd = 2)
plot(st_geometry(apotheke.sf[c(1, 2, 4, 3), ]), add = TRUE)

# Using a different routing server
u <- "https://routing.openstreetmap.de/routed-foot/"
route5 <- osrmRoute(apotheke.sf[1, ], apotheke.sf[16, ], osrm.server = u)
route5

# Using an open routing service with support for multiple modes
# see https://github.com/riatelab/osrm/issues/67
u <- "https://routing.openstreetmap.de/"
options(osrm.server = u)
route6 <- osrmRoute(apotheke.sf[1, ], apotheke.sf[16, ],
                    osrm.profile = "bike"
)
route7 <- osrmRoute(apotheke.sf[1, ], apotheke.sf[16, ],
                    osrm.profile = "car"
)
plot(st_geometry(route7), col = "green") # car
plot(st_geometry(route6), add = TRUE) # bike
plot(st_geometry(route5), col = "red", add = TRUE) # foot

## End(Not run)

u <- "https://routing.openstreetmap.de/"
options(osrm.server = u)

route_bike <- osrmRoute(
  src = c(116.492,39.872),
  dst = c(116.312,39.972),
  osrm.profile = "bike"
)
route_car <- osrmRoute(
  src = c(116.492,39.872),
  dst = c(116.312,39.972),
  osrm.profile = "car"
)
route_foot <- osrmRoute(
  src = c(116.492,39.872),
  dst = c(116.312,39.972),
  osrm.profile = "foot"
)

plot(st_geometry(route_bike), col = "green") # bike
plot(st_geometry(route_car), add = TRUE) # car
plot(st_geometry(route_foot), col = "red", add = TRUE) # foot
