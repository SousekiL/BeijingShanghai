##' lbs2crs
##' get epsg code by longitude, only for Beijing 1954 / 3-degree Gauss-Kruger CM coordinate System now
##'
##' @param x longitude, you can also directly enter the spatial attribute data, and identify the center point coordinates through bbox
##' @return location_crs
##' @author t.s.helianthus
##' @examples lbs2crs(x = 87.68)
##' @export

lbs2crs <- function(x, degree = 3, sys = "beijing1954") {
    x_class <- class(x)
    sp_class <- c(
        "SpatialPolygonsDataFrame", "SpatialLinesDataFrame", "SpatialPointsDataFrame",
        "sp"
    )
    if (length(intersect(sp_class, as.character(x_class))) > 0) {
        mean_lng_d <- sapply(1:dim(x@data)[1], function(y) {
            m <- floor(mean(bbox(x[y, ])[1, ]))
        })
        x@data$mean_lng <- mean_lng_d
    } else {
        mean_lng_d <- x
    }

    x <- floor(mean_lng_d)
    crs_start <- 2422
    mean_start <- 74

    crs_out <- as.character(floor((mean_lng_d - mean_start) / degree) + crs_start)
    return(crs_out)

    ## @TODO, add more sys
}