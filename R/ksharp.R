# package ksharp
#
# Thanks to @ashipunov for examples


#' sharpen a clustering
#'
#' Each data point in a clustering is assigned to a cluster, but some data
#' points may lie in ambiguous zones between two or more clusters, or far
#' from other points. Cluster sharpening assigns these border points into
#' a separate noise group, thereby creating more stark distinctions between
#' groups.
#'
#' Noise points are assigned to a group with cluster index 0. This is
#' analogous behavior to output produced by dbscan.
#'
#' @export
#' @param x clustering object; several types of inputs are acceptable,
#' including objects of class kmeans, pam, and self-made lists with a
#' component "cluster".
#' @param threshold numeric; the fraction of points to place in noise group
#' @param data matrix, raw data corresponding to clustering x; must be present
#' when sharpening for the first time or if data is not present within x.
#' @param method character, determines method used for sharpening
#' @param threshold.abs numeric; absolute-value of threshold for sharpening.
#' When non-NULL, this value overrides value in argument 'threshold'
#'
#' @return clustering object based on input x, with adjusted cluster
#' assignments and additional list components with sharpness measures.
#' Cluster assignments are placed in $cluster and excised data points
#' are given a cluster index of 0. Original cluster assignments are
#' saved in $cluster.original. Sharpness measures are stored in
#' components $silinfo, $medinfo, and $neiinfo, although these details
#' may change in future versions of the package.
#' 
#' @examples
#'
#' # prepare iris dataset for analysis
#' iris.data = iris[, 1:4]
#' rownames(iris.data) = paste0("iris_", seq_len(nrow(iris.data)))
#'
#' # cluster the dataset into three groups
#' iris.clustered = kmeans(iris.data, centers=3)
#' table(iris.clustered$cluster)
#' 
#' # sharpen the clustering by excluding 10% of the data points
#' iris.sharp = ksharp(iris.clustered, threshold=0.1, data=iris.data)
#' table(iris.sharp$cluster)
#' 
#' # visualize cluster assignments
#' iris.pca = prcomp(iris.data)$x[,1:2]
#' plot(iris.pca, col=iris$Species, pch=ifelse(iris.sharp$cluster==0, 1, 19))
#' 
ksharp = function(x, threshold=0.1, data=NULL,
                  method=c("silhouette", "neighbor", "medoid"),
                  threshold.abs=NULL) {
  
  method = match.arg(method)
  threshold = check.numeric(threshold)
  
  if (!is(x, "ksharp")) {
    # perhaps use data that is carried within x
    if (is.null(data) & "data" %in% names(x)) {
      data = x$data
    }     
    if ("cluster" %in% names(x)) {
      xclusters = x$cluster
    } else if ("clustering" %in% names(x)) {
      xclusters = x$clustering
    } else {    
      stop("object must have component $cluster or $clustering")
    }
    x = ksharp.prep(x, data, xclusters)
    class(x) = c("ksharp", class(x))
  } 
  
  # retrieve ids of noise points from an "info" object
  noise.ids = function(widths) {
    if (is.null(threshold.abs)) {
      threshold.abs = stats::quantile(widths[,3], p=threshold)
    }
    noise = widths[,3] < threshold.abs
    rownames(widths)[noise]
  }
  
  # update cluster assignments
  x$cluster = x$cluster.original
  if (method=="medoid") {
    x$cluster[noise.ids(x$medinfo$widths)] = 0
  } else if (method=="silhouette") {
    x$cluster[noise.ids(x$silinfo$widths)] = 0
  } else if (method=="neighbor") {
    x$cluster[noise.ids(x$neiinfo$widths)] = 0
  }
  
  x
}


#' augment a kmeans/clustering object with sharpness values
#' (silhouette widths, and other measures of sharpness)
#'
#' @keywords internal
#' @noRd
#' @param x object of class kmeans or cluster
#' @param data matrix with raw data, must match x
#' @param cluster named vector assigning items in data to clusters;
#' these clusters are meant to be the raw/original/unsharpened clusters
#'
#' @return object based on x, with additional elements holding
#' sharpness measures.
ksharp.prep = function(x, data, cluster) {

  if (is.null(data)) {
    stopCF("data cannot be null when running ksharp for first time")
  }
  if (is.null(rownames(data))) {
    stopCF("data rows must have names")
  }
  if (is.null(names(cluster))) {
    stopCF("cluster definitions must have names")
  }
  if (!identical(names(cluster), rownames(data))) {
    stopCF("rownames in data objects must match items in cluster object")
  }

  # compute distance matrix for data
  getdist = function() {
    if (is.null(datadist)) {
      return(stats::dist(data))
    }
    datadist
  }
  
  # add elements to x if they don't already exist
  datadist = NULL
  if (!"cluster.original" %in% names(x)) {
    x$cluster.original = cluster
  }
  if (!"silinfo" %in% names(x)) {
    datadist = getdist()
    x$silinfo = silinfo(cluster, datadist)
  }
  if (is.null(rownames(x$silinfo$widths))) {
    # this is a defensive re-compute of silinfo in case existing one
    # does not have item names encoded in it
    datadist = getdist()
    x$silinfo = silinfo(cluster, datadist)
  }
  if (!"medinfo" %in% names(x)) {
    x$medinfo = medinfo(cluster, data, x$silinfo$widths)
  }
  if (!"neiinfo" %in% names(x)) {
    datadist = getdist()
    x$neiinfo = neiinfo(cluster, datadist)
  }
  
  x
}

