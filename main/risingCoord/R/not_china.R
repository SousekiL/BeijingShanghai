##' not_china
##' 
##' if location out of china
##' 
##' Referencing wandergis/coordTransform_py <\url{https://github.com/wandergis/coordTransform_py}> 
##' 
##' @param lng query longitude
##' @param lat query latitude
##' @return out_of_china TRUE/FALSE
##' @author t.s.helianthus
##' @examples  out_of_china <- not_china(lng=121.89080, lat = 31.9479)
##' @export

not_china <- function(lng, lat) {
    out_of_china <- (lng < 72.004 | lng > 137.8347) | (lat < 0.8293 | lat > 55.8271)
    return(out_of_china)
}
