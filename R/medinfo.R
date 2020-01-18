# package ksharp.R
# compute measures of sharpness based on distances to medoids/centroids


#' compute info on distances to medoids/centroids
#'
#' Analogous in structure to silinfo and neiinfo, it computes
#' a "widths" matrix assessing how well each data point belongs
#' to its cluster. Here, this measure is the ratio of two distances:
#' in the numerator, the distance from the point to the nearest cluster center,
#' and in the denominator, from the point to its own cluster center.
#'
#' @export
#' @param cluster named vector
#' @param data matrix with raw data 
#' @param silwidths matrix with silhouette widths
#'
#' @return list with component widths. The widths object is a matrix
#' with one row per data item, with column med_ratio holding
#' the sharpness measure.
#'
#' @examples
#'
#' # construct a manual clustering of the iris dataset
#' iris.data = iris[, 1:4]
#' rownames(iris.data) = paste0("iris_", seq_len(nrow(iris.data)))
#' iris.dist = dist(iris.data)
#' iris.clusters = setNames(as.integer(iris$Species), rownames(iris.data))
#'
#' # compute sharpnessvalues based on medoids
#' iris.silinfo = silinfo(iris.clusters, iris.dist)
#' medinfo(iris.clusters, iris.data, iris.silinfo$widths)
#' 
medinfo = function(cluster, data, silwidths) {

  datacenters = medoids(data, silwidths)
  d.all.centers = dist2centers(data, datacenters)

  widths = silwidths
  colnames(widths)[colnames(widths)=="sil_width"] = "med_ratio"
  
  # for each item in dataset, identify ratio:
  # (nearest distance to other cluster)/(distance to self cluster)
  ratios = vapply(rownames(widths), function(y) {
    d.centers = d.all.centers[y, ]
    # fetch original cluster id
    yraw = as.character(cluster[y])
    # fetch distances to its own cluster
    d.self = d.centers[yraw]
    # fetch closest distance to another cluster
    d.centers[yraw] = NA
    d.other = min(d.centers, na.rm=TRUE)
    d.other/d.self   
  }, numeric(1))
  names(ratios) = names(cluster)
  widths[, "med_ratio"] = ratios
  class(widths) = NULL
  
  result = list(widths=widths)
  class(result) = "medinfo"
  
  result
}


#' get coordinates of cluster medoids
#'
#' @keywords internal
#' @noRd
#' @param data matrix
#' @param silwidths matrix with silhouette widths
#'
#' @return matrix, each row has an element of data that represents its cluster
medoids = function(data, silwidths) {
  # obtain ids of elements with maximal silhouette width
  silsplit = split(silwidths[, "sil_width"], silwidths[, "cluster"])
  silmedoid = vapply(silsplit, function(x) {
    names(which.max(x))
  }, character(1))
  result = data[silmedoid,]
  rownames(result) = names(silmedoid)
  result
}




#' compute a matrix of distances from data elements to fixed points
#' (e.g. cluster centers)
#'
#' @keywords internal
#' @noRd
#' @param data matrix with raw data
#' @param centers matrix with fewer rows than data each row meant to
#' capture center of a cluster
#'
#' @return matrix, each row captures distances from a data point
#' to each of the cluster centers
dist2centers = function(data, centers) {

  datat = t(data)
  k = nrow(centers)
  
  # build matrix with distances from items to cluster centers
  result = matrix(0, ncol=k, nrow=nrow(data))
  for (i in seq(k)) {
    temp = datat-as.numeric(centers[i,])
    result[,i] = sqrt(colSums(temp*temp))
  }
  
  # format result with samples names and cluster ids
  rownames(result) = rownames(data)
  colnames(result) = rownames(centers)
  result
}

