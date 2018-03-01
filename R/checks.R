## file part of package ksharp
##
## Helper functions that perform quality checks on data
##



##' Perform checks that cluster and dist are compatible with each other
##'
##' (This is an internal function only)
##'
##' @param cluster vector with cluster assignments, must be named
##' @param dist dist or matrix, must match cluster
##'
##' @return matrix, a subset of dist
check.cluster.dist = function(cluster, dist) {

  if (class(dist)=="dist") {
    dist = as.matrix(dist)
  }  
  if (!class(cluster) %in% c("numeric", "integer")) {
    stop("cluster assignment must be numeric vector\n")
  }
  if (is.null(names(cluster))) {
    stop("silinfo cluster must have names\n")
  }
  if (is.null(rownames(dist)) | is.null(colnames(dist))) {
    stop("silinfo dist must have names\n")
  }
  
  ## check that all items in cluster are defined in dist
  common = intersect(names(cluster), rownames(dist))
  if (length(common) != length(cluster)) {
    stop("incompatible cluster and distance objects\n")
  }
  
  dist[names(cluster), names(cluster)]
}



##' performs checks on an input to make sure it is in the required range
##'
##' (This is an internal function only)
##'
##' @param x numeric value
##' @param range pair of minimum, maximum values
##' @param varname character, printed during stop()
##'
##' @return numeric x
check.numeric = function(x, range=c(0, 1), varname="threshold") {
  if (!class(x) %in% "numeric") {
    stop(varname, " must be a numeric\n")
  }
  if (x<range[1] | x>range[2]) {
    stop(varname, " must be in range [", range[1], ", ", range[2], "]\n")
  }
  x
}

