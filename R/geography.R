# geography.R --------------------------------------------------
#' Compute great circle distances
#'
#' Wraps \code{\link[geosphere]{distVincentyEllipsoid}} in tidy semantics to
#' compute and add the shortest distance between to points on a globe to a
#' table of coordinates.
#'
#' @param .data a tbl with coordinates in degrees
#' @param lat,long latitude and longitude columns (unquoted)
#' @param lat_from,long_from optional reference point coordinates
#' @param dist_col name of the output column
#' @param along if TRUE return cumulative distances along points
#' @param ... arguments passed on to distVincentyEllipsoid
#' @export
#' @examples
#' stations <- tibble(
#'   lat=c(50,55,60),
#'   long=c(180,180,180)
#' )
#'
#' # cumulative distance along transect stations
#' stations %>% distance
#'
#' # distances to almost Greenwich
#' stations %>%
#'   distance(lat_from=0, long_from=0, along=FALSE)
distance <- function(.data, lat = lat, long = long, lat_from = NULL, long_from = NULL, dist_col = "dist", along = TRUE, ...){
  if (!require(geosphere)){ stop("Depends on packages geosphere")}

 ll_vars <- ggworldmap:::vars_lat_long(names(.data), !! rlang::enquo(lat), !! rlang::enquo(long))

  ll <- as.matrix(.data[rev(ll_vars)])

  # TODO: has something to do with the interface of geosphere::distVincentyEllipsoid, could be fixed
  if(nrow(ll) == 2) stop('need at least 3 sets of coordinates to work')
  
  if(is.null(long_from) && is.null(lat_from)){
    dists <- geosphere::distVincentyEllipsoid(ll, ...)
  }else{
    dists <- geosphere::distVincentyEllipsoid(ll, c(long_from, lat_from), ...)
  }

  if(is.null(dist_col)) return(dists)
  # prepend NA in case of sequential distances
  if(length(dists) == nrow(.data) - 1) dists <- c(0, dists)

  if(along) dists <- cumsum(dists)
  
  .data[dist_col] <- dists
  .data
}
