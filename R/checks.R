# package ksharp
# Helper functions that perform quality checks on data
#


#' stop with call.=FALSE
#'
#' @keywords internal
#' @noRd
#' @param ... character vector
#'
#' @return nothing, always ends with a stop()
stopCF = function(...) {
  x = paste0(paste(..., collapse=" "), "\n")
  stop(x, call.=FALSE)
}


#' check that cluster and dist are compatible with each other
#'
#' @keywords internal
#' @noRd
#' @importFrom methods is
#' @param cluster vector with cluster assignments, must be named
#' @param dist dist or matrix, must match cluster
#'
#' @return matrix, a subset of dist
check.cluster.dist = function(cluster, dist) {

  if (is(dist, "dist")) {
    dist = as.matrix(dist)
  }  
  if (!is(cluster, "numeric") & !is(cluster, "integer")) {
    stopCF("cluster assignment must be numeric vector")
  }
  if (is.null(names(cluster))) {
    stopCF("silinfo cluster must have names")
  }
  if (is.null(rownames(dist)) | is.null(colnames(dist))) {
    stopCF("silinfo dist must have names")
  }
  
  # check that all items in cluster are defined in dist
  common = intersect(names(cluster), rownames(dist))
  if (length(common) != length(cluster)) {
    stopCF("incompatible cluster and distance objects")
  }
  
  dist[names(cluster), names(cluster)]
}


#' check a numerical input to ensure it is in the required range
#'
#' @keywords internal
#' @noRd
#' @param x numeric value
#' @param range pair of minimum, maximum values
#' @param varname character, printed during stop()
#'
#' @return numeric x
check.numeric = function(x, range=c(0, 1), varname="threshold") {
  if (!is(x, "numeric")) {
    stopCF(varname, " must be numeric")
  }
  if (x<range[1] | x>range[2]) {
    stopCF(varname, " must be in range [", range[1], ", ", range[2], "]")
  }
  x
}

