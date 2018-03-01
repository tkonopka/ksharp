## file from package ksharp.R
##
## Calculation of neighbor widths and derivative metrics
##


##' Compute info on `neighbor widths`
##'
##' This function provides information on how well each data point belong to its
##' cluster. For each query point, the function considers n of its nearest neighbors.
##' The neighbor widths are defined as the fraction of those neighbors that belong to
##' the same cluster as the query point. (These values are termed 'widths' in
##' analogy to silhouette widths, another measure of cluster membership.)
##'
##' The function follows a similar signature as silinfo from this package.
##'
##' @param cluster vector with assignments of data elements to clusters
##' @param dist distance object or matrix
##'
##' @return list with component widths
##'
##' @export
neiinfo = function(cluster, dist) {
  
  ## check cluster and data
  dist = check.cluster.dist(cluster, dist)
  
  widths = matrix(0, ncol=3, nrow=length(cluster))
  rownames(widths) = names(cluster)
  colnames(widths) = c("cluster", "size", "neighborhood")
  
  cluster.split = split(names(cluster), cluster)
  
  for (i in names(cluster)) {
    icluster = cluster.split[[as.character(cluster[i])]]
    inei = names(sort(dist[i,], decreasing=FALSE))
    inei = inei[inei!=i]
    iself = sapply(inei, function(x) {
      x %in% icluster
    })
    ipure = which(!iself)[1]
    if (is.na(ipure)) {
      ipure = length(icluster)
    }
    widths[i, ] = c(cluster[i], length(icluster), ipure)
  }
  
  list(widths = widths)
}

