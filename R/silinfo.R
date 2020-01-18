# file from package ksharp.R
# Silhouette widths


##' Compute info on silhouette widths
#'
#' This function provides information on how well each data point
#' belongs to its cluster. For each query point, the function considers
#' the average distance to other members of the same cluster and the
#' average distance to members of another, nearest, cluster. The widths
#' are defined as the
#'
#' The function signature is very similar to cluster::silhouette but
#' the implementation has important differences. This implementation
#' requires both the dist object and and cluster vector must have names.
#' This prevents accidental assignment of silhouette widths to the wrong
#' elements. 
#'
#' @export
#' @param cluster vector with assignments of data elements to clusters
#' @param dist distance object or matrix
#' 
#' @return list, analogous to object within output from cluster::pam.
#' In particular, the list has a component widths. The widths object is
#' matrix with one row per data item, with column sil_width holding the
#' silhouette width. 
#'
#' @examples
#'
#' # construct a manual clustering of the iris dataset
#' iris.data = iris[, 1:4]
#' rownames(iris.data) = paste0("iris_", seq_len(nrow(iris.data)))
#' iris.dist = dist(iris.data)
#' iris.clusters = setNames(as.integer(iris$Species), rownames(iris.data))
#'
#' # compute sharpness values based on silhouette widths
#' silinfo(iris.clusters, iris.dist)
#' 
silinfo = function(cluster, dist) {

  # check cluster and data
  dist = check.cluster.dist(cluster, dist)
  
  # prepare matix for silhouette widths
  widths = matrix(0, ncol=3, nrow=length(cluster))
  colnames(widths) = c("cluster", "neighbor", "sil_width")
  rownames(widths) = names(cluster)
  
  cluster.split = split(names(cluster), cluster)
  
  for (i in names(cluster)) {
    # find average distance to all clusters
    ab = vapply(cluster.split, function(x) {
      result = sum(dist[i, x])
      denominator = length(x)
      if (i %in% x) {
        denominator = length(x)-1
      }
      result/denominator
    }, numeric(1))
    # find disances to:
    # a - all other points within same cluster,
    # b - all points in nearest cluster
    a = ab[as.character(cluster[i])]
    ab[as.character(cluster[i])] = NA
    whichb = names(ab)[which.min(ab)]
    b = ab[whichb]
    # record into output object
    widths[i, ] = c(cluster[i], as.numeric(whichb), (b-a)/max(b, a))
  }
  class(widths) = "silhouette"
  attr(widths, "Ordered") = TRUE

  result = list()
  result$widths = widths
  result$clus.avg.widths = vapply(cluster.split,
                                  function(x) {
                                    mean(widths[x, "sil_width"])
                                  }, numeric(1))
  result$avg.width = mean(widths[,3])
  class(result) = "silinfo"
  
  result
}

