## file from package ksharp.R
##
## Calculation of cluster centers and distances
##




##' compute info on distances to medoids/centroids
##'
##' Analogous in structure to silinfo and neiinfo, it computes
##' a "widths" marix assessing how well each data point belongs
##' to its cluster. This measure is here the ratio of the distance
##' from the point to the nearest cluster center to the distance
##' from the point to its own cluster center.
##'
##' @param data matrix
##' @param cluster named vector
##' @param silwidths matrix with silhouette widths
##'
##' @return list with component widths
##'
##' @export
medinfo = function(cluster, data, silwidths) {

  datacenters = medoids(data, silwidths)
  d.all.centers = dist2centers(data, datacenters)

  widths = silwidths
  colnames(widths)[colnames(widths)=="sil_width"] = "med_ratio"
  
  ## for each item in dataset, identify ratio:
  ## (nearest distance to other cluster)/(distance to self cluster)
  ratios = sapply(rownames(widths), function(y) {
    d.centers = d.all.centers[y, ]
    ## fetch original cluster id
    yraw = as.character(cluster[y])
    ## fetch distances to its own cluster
    d.self = d.centers[yraw]
    ## fetch closest distance to another cluster
    d.centers[yraw] = NA
    d.other = min(d.centers, na.rm=T)
    d.other/d.self   
  })
  names(ratios) = names(cluster)
  widths[, "med_ratio"] = ratios
  class(widths) = NULL
  
  result = list(widths=widths)
  class(result) = "medinfo"
  
  result
}




##' get coordinates of cluster medoids
##'
##' (This is an internal functions)
##'
##' @param data matrix
##' @param silwidths matrix with silhouette widths
##'
##' @return matrix, each row has an element of data that represents its cluster
medoids = function(data, silwidths) {
  ## obtain ids of elements with maximal silhouette width
  silsplit = split(silwidths[, "sil_width"], silwidths[, "cluster"])
  silmedoid = sapply(silsplit, function(x) {
    names(which.max(x))
  })
  result = data[silmedoid,]
  rownames(result) = names(silmedoid)
  result
}




##' compute a matrix of distances from data elements to fixed points
##' (e.g. cluster centers)
##'
##' (This is an internal function only)
##'
##' @param data matrix with raw data
##' @param centers matrix with fewer rows than data each row meant to
##' capture center of a cluster
##'
##' @return matrix
dist2centers = function(data, centers) {

  datat = t(data)
  k = nrow(centers)
  
  ## build matrix with distances from items to cluster centers
  result = matrix(0, ncol=k, nrow=nrow(data))
  for (i in seq(k)) {
    temp = datat-centers[i,]
    result[,i] = sqrt(apply(temp*temp, 2, sum))
  }
  
  ## format result with samples names and cluster ids
  rownames(result) = rownames(data)
  colnames(result) = rownames(centers)
  result
}

