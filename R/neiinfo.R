# package ksharp.R
# Calculation of measures of sharpness based on neighbors


#' Compute info on `neighbor widths`
#'
#' This function provides information on how well each data point
#' belongs to its cluster. For each query point, the function considers
#' n of its nearest neighbors. The neighbor widths are defined as the
#' fraction of those neighbors that belong to the same cluster as the
#' query point. These values are termed 'widths' in analogy to
#' silhouette widths, another measure of cluster membership.
#'
#' The function follows a similar signature as silinfo from this package.
#'
#' @export
#' @param cluster vector with assignments of data elements to clusters
#' @param dist distance object or matrix
#'
#' @return list with component widths. The wdiths object is a matrix
#' with one row per data item, wth column neighborhood holding the
#' sharpness value.
#'
#' @examples
#'
#' # construct a manual clustering of the iris dataset
#' iris.data = iris[, 1:4]
#' rownames(iris.data) = paste0("iris_", seq_len(nrow(iris)))
#' iris.dist = dist(iris.data)
#' iris.clusters = setNames(as.integer(iris$Species), rownames(iris.data))
#'
#' # compute neighbor-based sharpness widths
#' neiinfo(iris.clusters, iris.dist)
#' 
neiinfo = function(cluster, dist) {
  
  # check cluster and data
  dist = check.cluster.dist(cluster, dist)
  
  widths = matrix(0, ncol=3, nrow=length(cluster))
  rownames(widths) = names(cluster)
  colnames(widths) = c("cluster", "size", "neighborhood")
  
  cluster.split = split(names(cluster), cluster)
  
  for (i in names(cluster)) {
    icluster = cluster.split[[as.character(cluster[i])]]
    inei = names(sort(dist[i,], decreasing=FALSE))
    inei = inei[inei!=i]
    iself = vapply(inei, function(x) {
      x %in% icluster
    }, logical(1))
    ipure = which(!iself)[1]
    if (is.na(ipure)) {
      ipure = length(icluster)
    }
    widths[i, ] = c(cluster[i], length(icluster), ipure)
  }
  
  list(widths = widths)
}

