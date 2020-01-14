# tests for ksharp calculation


# small dataset for testing
K1select = c(1:30, 401:430)
K1 = as.matrix(kdata.1[K1select,1:2])

# precompute a kmeans object 
km2 = kmeans(K1, 2)
library(cluster)
kp2 = pam(K1, 2)


# create a self-made clustering object
selfmade = list(cluster = setNames(rep(c(1,2), each=30), rownames(K1)))



##############################################################################
# testing bad inputs


test_that("should give error on non-kmeans data", {
  expect_error(ksharp(K1), "must have component")
  expect_error(ksharp(1:10), "must have component")
})


test_that("should give error when data has no item names", {
  dd = K1
  rownames(dd) = NULL
  expect_error(ksharp(km2, data=dd), "names")
})


test_that("should give error when running first time without data", {
  expect_error(ksharp(km2), "null")
})


test_that("should give error when running on wrong data", {
  small = K1[1:4,]
  expect_error(ksharp(km2, data=small), "rownames")
})


test_that("should give error when running on wrong data", {
  temp = km2
  names(temp$cluster) = NULL
  expect_error(ksharp(temp, data=K1), "names")
})


test_that("should give error when threshold not in [0,1]", {
  expect_error(ksharp(km2, data=K1, threshold=-0.1), "threshold")
  expect_error(ksharp(km2, data=K1, threshold=1.5), "threshold")
  expect_error(ksharp(km2, data=K1, threshold=NULL), "threshold")
  expect_error(ksharp(km2, data=K1, threshold=NA), "threshold")
})




##############################################################################
# testing using kmeans input


test_that("should accept a numeric data frame", {
  i4 = iris[1:100, 1:4]
  rownames(i4) = paste0("I", 1:100)
  ik = kmeans(i4, 2)
  iks = ksharp(ik, data=i4)
  expect_is(iks, "ksharp")
  expect_is(iks, "kmeans")
})


test_that("should change class and add fields", {
  sharp2 = ksharp(km2, data=K1)
  expect_is(sharp2, "ksharp")
  expect_is(sharp2, "kmeans")
  
})


test_that("should add medinfo values based on distance to centers", {
  sharp2 = ksharp(km2, data=K1)
  # should add sharpness numeric
  expect_false("medinfo" %in% names(km2))
  expect_true("medinfo" %in% names(sharp2))  
  expect_equal(length(sharp2$medinfo), 1)
  expect_equal(nrow(sharp2$medinfo$widths), nrow(K1))
})


test_that("change cluster values", {
  sharp2 = ksharp(km2, threshold=0.4, data=K1)
  # should add matrices with distances to cluster centroids
  expect_equal(sort(unique(km2$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
})


test_that("can reset threshold based on ksharp objects", {
  sharp2a = ksharp(km2, threshold=0, data=K1)
  expect_equal(sort(unique(km2$cluster)), c(1,2))
  # here the sharpness threshold was too low to exclude anything
  expect_equal(sort(unique(sharp2a$cluster)), c(1,2))
  # raising the sharpness threshold can be done on a ksharp object
  sharp2b = ksharp(sharp2a, threshold=0.5)
  expect_equal(sort(unique(sharp2b$cluster)), c(0,1,2))
  # lowering sharpness threshold should work too
  sharp2c = ksharp(sharp2b, threshold=0)
  expect_equal(sort(unique(sharp2c$cluster)), c(1,2))
})


test_that("recompute silhouettes if needed", {
  sharp2 = ksharp(km2, threshold=0.3, data=K1, method="sil")
  # attempt to fool ksharp by using a prepared silhouette, but wrong format
  silB = sharp2$silinfo
  rownames(silB$widths) = NULL
  km2B = km2
  km2B$silinfo = silB
  sharp2B = ksharp(km2B, threshold=0.3, data=K1, method="sil")
  # silhouettes in sharp2B should be recomputed, i.e. have names
  expect_equal(sharp2$silinfo$widths, sharp2B$silinfo$widths)
})




##############################################################################
# using different methods


test_that("change clustering, method=silhouette", {
  sharp2 = ksharp(km2, threshold=0.3, data=K1, method="silhouette")
  # should add matrices with distances to cluster centroids
  expect_equal(sort(unique(km2$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
  numzeros = as.integer(table(sharp2$cluster)["0"])
  expect_equal(numzeros, 0.3*nrow(K1))
})


test_that("change clustering, method=medoid", {
  sharp2 = ksharp(km2, threshold=0.3, data=K1, method="medoid")
  # should add matrices with distances to cluster centroids
  expect_equal(sort(unique(km2$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
  numzeros = as.integer(table(sharp2$cluster)["0"])
  expect_equal(numzeros, 0.3*nrow(K1))
})


test_that("change clustering, method=neighbor", {
  sharp2 = ksharp(km2, threshold=0.3, data=K1, method="neighbor")
  # should add matrices with distances to cluster centroids
  expect_equal(sort(unique(km2$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
  numzeros = as.integer(table(sharp2$cluster)["0"])
  expect_equal(numzeros, 0.3*nrow(K1))
})




##############################################################################
# using relateive and absolute-value thresholds


test_that("absolute value thresholding", {
  sharp2rel = ksharp(kp2, threshold=0.3, data=K1, method="neighbor")
  sharp2abs = ksharp(kp2, threshold=0.3, data=K1, method="silhouette",
                     threshold.abs=0.6)
  # for this dataset, a silhouette value of 0.6 is high and should result
  # in more that 30% of points in noise group
  noise.rel = sum(sharp2rel$cluster==0)
  noise.abs = sum(sharp2abs$cluster==0)
  expect_gt(noise.abs, noise.rel)
})


test_that("absolute value thresholding ignores relative threshold", {
  abs1 = ksharp(kp2, threshold=0.1, data=K1, method="neighbor",
                threshold.abs=0.4)
  abs2 = ksharp(kp2, threshold=0.5, data=K1, method="neighbor",
                threshold.abs=0.4)
  abs3 = ksharp(kp2, threshold=0.9, data=K1, method="neighbor",
                threshold.abs=0.4)
  expect_equal(abs1$cluster, abs2$cluster)
  expect_equal(abs1$cluster, abs3$cluster)
})




###############################################################################
# using pam input


test_that("change pam cluster values", {
  sharp2 = ksharp(kp2, threshold=0.4, data=K1)
  # should add matrices with distances to cluster centroids
  expect_equal(sort(unique(kp2$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
})


test_that("sharpening on pam input requires no data", {
  sharp2 = ksharp(kp2, threshold=0.4)
  # should add matrices with distances to cluster centroids
  expect_equal(sort(unique(kp2$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
})




###############################################################################
# using self-made lists


test_that("change self-made cluster values", {
  # omitting data should give error
  expect_error(ksharp(selfmade, threshold=0.4))
  # with data, should succeed and alter clusters
  sharp2 = ksharp(selfmade, threshold=0.4, data=K1)
  # should add matrices with distances to cluster centroids
  expect_equal(sort(unique(selfmade$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
})


test_that("change self-made cluster values (omitting data)", {
  selfmade2 = selfmade
  selfmade2$data = K1
  sharp2 = ksharp(selfmade2, threshold=0.4)
  # should add matrices with distances to cluster centroids
  expect_equal(sort(unique(selfmade2$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
})

