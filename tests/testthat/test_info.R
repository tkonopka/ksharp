# tests for ksharp silinfo.R, neiinfo.R


library(cluster)


# helper data object with manually picked data
infodata = matrix(c(-1.1,-1, -0.9,-1, -0.1,0,0.1,0, 0.9,1,1.1,1),
                  byrow=T, ncol=2)
rownames(infodata) = paste0("S", seq(nrow(infodata)))
colnames(infodata) = c("D1", "D2")
infodist = dist(infodata)
infoclusters = setNames(rep(c(1,2,3), each=2), rownames(infodata))
infopam = pam(infodata, 3)

# some alternative cluster assignments
singletons = setNames(c(1, rep(2, nrow(infodata)-1)), rownames(infodata))
withzeros = setNames(rep(c(0,2,3), each=2), rownames(infodata))
onecluster = setNames(rep(1, nrow(infodata)), rownames(infodata))


###############################################################################
# testing silinfo

test_that("compute silinfo matches from package cluster", {
  # get whole infosil and silhouette matrix
  infosil = silinfo(infoclusters, infodist)
  infowidths = as.matrix(infosil$widths)
  class(infowidths) = "matrix"
  attr(infowidths, "Ordered") = NULL
  # names should match
  expect_equal(names(infopam$silinfo), names(infosil))
  # content might not match exactly because of ordering of items
  ic = names(infoclusters)
  pamwidths = infopam$silinfo$widths[ic, ]
  # the infowidths matrix and numerics should matchs pam output
  expect_equal(pamwidths, infowidths)
})


test_that("silinfo gives errors when inputs and outputs don't match", {
  badclusters = infoclusters
  names(badclusters) = NULL
  expect_error(silinfo(badclusters, infodist))

  baddist2 = baddist1 = as.matrix(infodist)
  rownames(baddist1) = NULL
  rownames(baddist2) = NULL
  expect_error(silinfo(infoclusters, baddist1))
  expect_error(silinfo(infoclusters, baddist2))

  badnames = infodata
  rownames(badnames) = paste0("X", rownames(badnames))
  expect_error(silinfo(infoclusters, dist(badnames)))
})


test_that("silinfo gives errors when clusters are non-integers", {
  # numbers are required, because silinfo matrix has columns
  # with cluster, neighboring cluster, width
  # changing the cluster id to a non-number will change the matrix
  # into strings
  badclusters = paste0("C", infoclusters)
  names(badclusters) = names(infoclusters)
  expect_error(silinfo(badclusters, infodist))
})


test_that("silinfo gives same output on dist or matrix", {
  out1 = silinfo(infoclusters, infodist)
  out2 = silinfo(infoclusters, as.matrix(infodist))
  expect_equal(out1, out2)
})


test_that("silinfo gives same output on different order data", {
  # items in cluster and dist specified in same order
  c1 = infoclusters
  out1 = silinfo(c1, infodist)
  # items in cluster defined in different order
  c2 = rev(infoclusters)
  out2 = silinfo(c2, infodist)
  # here out1 and out2 should be equivalent, but might be ordered differently
  cnames = names(c1)
  expect_equal(out1$widths[cnames,], out2$widths[cnames,])
  expect_equal(out1$clust.avg.width, out2$clust.avg.width)
  expect_equal(out1$avg.width, out2$avg.width)
})


test_that("silinfo gives output even when cluster numbers include 0", {
  # items in cluster and dist specified in same order
  c1 = infoclusters
  out1 = silinfo(c1, infodist)
  c2 = infoclusters-1
  out2 = silinfo(c2, infodist)
  expect_equal(out1$widths[,3], out2$widths[,3])
})


test_that("silinfo works on clustering with singletons", {
  # items in cluster and dist specified in same order
  out1 = silinfo(singletons, infodist)
  expect_equal(dim(out1$widths), c(6, 3))
})


test_that("silinfo works on clustering with zeros", {
  # whether cluster ids are 1,2,3 or 0,2,3 - all should be equal 
  out1 = silinfo(infoclusters, infodist)
  out2 = silinfo(withzeros, infodist)
  expect_equal(out1$widths[,"sil_width"], out2$widths[,"sil_width"])
})


test_that("silinfo works on trivial clustering", {
  # whether cluster ids are 1,2,3 or 0,2,3 - all should be equal 
  out1 = silinfo(onecluster, infodist)
  expect_equal(sum(out1$widths[,"sil_width"]), length(onecluster))
})




###############################################################################
# testing neiinfo


test_that("neiinfo checks argument neighbors", {
  expect_error(neiinfo(infoclusters, infodist, NULL))
  expect_error(neiinfo(infoclusters, infodist, -1))
  # standard calculation of output size
  out1 = neiinfo(infoclusters, infodist)
  expect_equal(ncol(out1$widths), 3)
})


test_that("neiinfo gives right numerics", {
  # all clusters have size two, each neighborhood is of size two
  out = neiinfo(infoclusters, infodist)$widths
  expected1 = c(1,2,2)
  expect_equal(as.numeric(out["S1", ]), expected1)
  expected2 = c(2,2,2)
  expect_equal(as.numeric(out["S3", ]), expected2)
  expected3 = c(3,2,2)
  expect_equal(as.numeric(out["S6", ]), expected3)
})


test_that("neiinfo works on clustering with singletons", {
  out1 = neiinfo(singletons, infodist)
  expect_equal(dim(out1$widths), c(6, 3))
})


test_that("neiinfo works on clustering with zeros", {
  # whether cluster ids are 1,2,3 or 0,2,3 - all should be equal 
  out1 = neiinfo(infoclusters, infodist)
  out2 = neiinfo(withzeros, infodist)
  expect_equal(out1$widths[,"neighborhood"], out2$widths[,"neighborhood"])
  expect_equal(out1$widths[,"size"], out2$widths[,"size"])
})


test_that("neiinfo works on trivial clustering", {
  # whether cluster ids are 1,2,3 or 0,2,3 - all should be equal 
  out1 = neiinfo(onecluster, infodist)
  expect_equal(sum(out1$widths[,"neighborhood"]),
               length(onecluster)*length(onecluster))
})

