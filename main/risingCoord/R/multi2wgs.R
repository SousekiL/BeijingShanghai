##' gcj2wgs
##'
##' GCJ-02 to WGS-48
##'
##' BD-09 formulated by the Chinese State Bureau of Mapping GIS coordinate system(Uesd by BaiduMap)
##' GCJ-02 formulated by the Chinese State Bureau of Mapping GIS coordinate system(Uesd byAmap/QQmap/Google map)
##' WGS-84 is an internationally adopted geocentric coordinate system
##' Referencing wandergis/coordTransform_py <\url{https://github.com/wandergis/coordTransform_py}>
##'
##' @param lng GCJ-02, BD or WGS longitude
##' @param lat GCJ-02, BD or WGS latitude
##' @return location
##' @author t.s.helianthus
##' @examples    location <- multi2wgs(lng=121.89080, lat = 31.9479, crs="GCJ")
##' @examples
##' lng <- c(121.394948, NA, 117.499596, 93.393923, 119.2943959)
##' lat <- c(25.2843745, NA, 25.4958935, 33.2949457, 31.5958333)
##' crs <- c("GCJ", "GCJ", "FALSE", "BD", "WGS")
##' location <- multi2wgs(lng = lng, lat = lat, crs = crs)
##' @export

multi2wgs <- function(lng, lat, crs) {
  crs <- toupper(crs)
  crs[grep("BD", crs)] <- "BD"
  crs[grep("GCJ", crs)] <- "GCJ"
  crs[grep("WGS", crs)] <- "WGS"

  lng <- as.numeric(lng)
  lat <- as.numeric(lat)
  lng[is.na(lng)] <- 0
  lat[is.na(lat)] <- 0
  crs[!crs %in% c("GCJ", "BD")] <- "WGS"

  oid <- seq(length(lng))

  crs_list <- c("BD", "GCJ", "WGS")

  nlng <- c()
  nlat <- c()
  noid <- c()
  for (c in seq_len(length(crs_list))) {
    lbs_crs <- crs_list[c]
    dl <- which(crs == lbs_crs)

    if (length(dl) > 0) {
      coord_fun <- get(paste0(tolower(lbs_crs), "2wgs"))

      otrans <- coord_fun(lng = lng[dl], lat = lat[dl])

      nlng <- c(nlng, otrans$trans_lng)
      nlat <- c(nlat, otrans$trans_lat)
      noid <- c(noid, oid[dl])
    }
  }

  trans_lng <- nlng[order(noid)]
  trans_lat <- nlat[order(noid)]

  re_coord <- not_china(lng, lat) & (!is.na(lng) | !is.na(lat))
  trans_lng[re_coord] <- lng[re_coord]
  trans_lat[re_coord] <- lat[re_coord]

  trans_lng[is.na(lng)] <- lng[is.na(lng)]
  trans_lat[is.na(lat)] <- lat[is.na(lat)]

  location <- list(trans_lng = trans_lng, trans_lat = trans_lat)

  return(location)
}