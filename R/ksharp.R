## file from package ksharp
##
## Definition of main package function
##




##' sharpen a clustering
##'
##' Each data point in a clustering is assigned to a cluster, but some data points
##' may lie in ambiguous zones between two or more clusters. Cluster sharpening
##' assigns these border points into a separate noise group, thereby creating
##' more stark distinctions between the groupings that are left.
##'
##' Noise points are assigned to a group with cluster index 0. This is analogous
##' behavior to output produced by dbscan.
##'
##' @param x clustering object; several types of inputs are acceptable, including
##' objects of class kmeans, pam, and self-made lists with a component "cluster".
##' @param threshold numeric; the fraction of points to place in noise group
##' @param data matrix, raw data corresponding to clustering x; must be present
##' when sharpening for the first time or if data is not present within x.
##' @param method character, determines method used for sharpening
##'
##' @export
ksharp = function(x, threshold=0.1, data=NULL,
                   method=c("silhouette", "neighbor", "medoid")) {
  
  method = match.arg(method)
  threshold = check.numeric(threshold)
  
  if (!"ksharp" %in% class(x)) {
    ## perhaps infer data when present in x
    if (is.null(data) & "data" %in% names(x)) {
      data = x$data
    }     
    if ("cluster" %in% names(x)) {
      xclusters = x$cluster
    } else if ("clustering" %in% names(x)) {
      xclusters = x$clustering
    } else {    
      stop("object must be compatible with kmeans or pam\n")
    }
    x = ksharp.prep(x, data, xclusters)
    class(x) = c("ksharp", class(x))
  } 

  ## retrieve ids of noise points from an "info" object
  noise.ids = function(widths) {
    noise = widths[,3] < stats::quantile(widths[,3], p=threshold)
    rownames(widths)[noise]
  }
  
  ## update cluster assignments
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




##' augment a kmeans object with sharpness values
##'
##' (This is an internal function)
##'
##' @param x object of class kmeans or cluster
##' @param data matrix with raw data, must match x
##' @param cluster named vector assigning items in data to clusters;
##' these clusters are meant to be the raw/original/unsharpened clusters
##'
##' @return object based on x, with additional elements
ksharp.prep = function(x, data, cluster) {

  if (is.null(data)) {
    stop("data cannot be null when running ksharp for first time.")
  }
  if (is.null(rownames(data))) {
    stop("data rows must have names\n")
  }
  if (is.null(names(cluster))) {
    stop("cluster definitions must have names\n")
  }
  if (!identical(names(cluster), rownames(data))) {
    stop("rownames in data objects must match items in cluster object\n")
  }

  ## compute distance matrix for data
  getdist = function() {
    if (is.null(datadist)) {
      return(stats::dist(data))
    }
    datadist
  }
  
  ## add elements to x if they don't already exist
  datadist = NULL
  if (!"cluster.original" %in% names(x)) {
    x$cluster.original = cluster
  }
  if (!"silinfo" %in% names(x)) {
    datadist = getdist()
    x$silinfo = silinfo(cluster, datadist)
  }
  if (is.null(rownames(x$silinfo$widths))) {
    ## this is a defensive re-compute of silinfo in case existing one
    ## does not have item names encoded in it
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




