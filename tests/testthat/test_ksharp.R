## tests for ksharp calculation

cat("\ntest_ksharp ")

## small dataset for testing
K1select = c(1:30, 401:430)
K1 = as.matrix(kdata.1[K1select,1:2])

## precompute a kmeans object 
km2 = kmeans(K1, 2)
library(cluster)
kp2 = pam(K1, 2)


## create a self-made clustering object
selfmade = list(cluster = setNames(rep(c(1,2), each=30), rownames(K1)))



###############################################################################
## testing bad inputs


test_that("should give error on non-kmeans data", {
  expect_error(ksharp(K1), "kmeans")
  expect_error(ksharp(1:10), "kmeans")
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



###############################################################################
## testing using kmeans input


test_that("should change class and add fields", {
  sharp2 = ksharp(km2, data=K1)
  expect_equal(class(sharp2), c("ksharp", "kmeans"))
})


test_that("should add medinfo values based on distance to centers", {
  sharp2 = ksharp(km2, data=K1)
  ## should add sharpness numeric
  expect_false("medinfo" %in% names(km2))
  expect_true("medinfo" %in% names(sharp2))  
  expect_equal(length(sharp2$medinfo), 1)
  expect_equal(nrow(sharp2$medinfo$widths), nrow(K1))
})


test_that("change cluster values", {
  sharp2 = ksharp(km2, threshold=0.4, data=K1)
  ## should add matrices with distances to cluster centroids
  expect_equal(sort(unique(km2$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
})


test_that("can reset threshold based on ksharp objects", {
  sharp2a = ksharp(km2, threshold=0, data=K1)
  expect_equal(sort(unique(km2$cluster)), c(1,2))
  ## here the sharpness threshold was too low to exclude anything
  expect_equal(sort(unique(sharp2a$cluster)), c(1,2))
  ## raising the sharpness threshold can be done on a ksharp object
  sharp2b = ksharp(sharp2a, threshold=0.5)
  expect_equal(sort(unique(sharp2b$cluster)), c(0,1,2))
  ## lowering sharpness threshold should work too
  sharp2c = ksharp(sharp2b, threshold=0)
  expect_equal(sort(unique(sharp2c$cluster)), c(1,2))
})


test_that("recompute silhouettes if needed", {
  sharp2 = ksharp(km2, threshold=0.3, data=K1, method="sil")
  ## attempt to fool ksharp by using a prepared silhouette, but wrong format
  silB = sharp2$silinfo
  rownames(silB$widths) = NULL
  km2B = km2
  km2B$silinfo = silB
  sharp2B = ksharp(km2B, threshold=0.3, data=K1, method="sil")
  ## silhouettes in sharp2B should be recomputed, i.e. have names
  expect_equal(sharp2$silinfo$widths, sharp2B$silinfo$widths)
})


###############################################################################
## testing using different methods


test_that("change clustering, method=silhouette", {
  sharp2 = ksharp(km2, threshold=0.3, data=K1, method="silhouette")
  ## should add matrices with distances to cluster centroids
  expect_equal(sort(unique(km2$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
  numzeros = as.integer(table(sharp2$cluster)["0"])
  expect_equal(numzeros, 0.3*nrow(K1))
})


test_that("change clustering, method=medoid", {
  sharp2 = ksharp(km2, threshold=0.3, data=K1, method="medoid")
  ## should add matrices with distances to cluster centroids
  expect_equal(sort(unique(km2$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
  numzeros = as.integer(table(sharp2$cluster)["0"])
  expect_equal(numzeros, 0.3*nrow(K1))
})


test_that("change clustering, method=neighbor", {
  sharp2 = ksharp(km2, threshold=0.3, data=K1, method="neighbor")
  ## should add matrices with distances to cluster centroids
  expect_equal(sort(unique(km2$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
  numzeros = as.integer(table(sharp2$cluster)["0"])
  expect_equal(numzeros, 0.3*nrow(K1))
})



###############################################################################
## testing using pam input


test_that("change pam cluster values", {
  sharp2 = ksharp(kp2, threshold=0.4, data=K1)
  ## should add matrices with distances to cluster centroids
  expect_equal(sort(unique(kp2$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
})


test_that("sharpening on pam input requires no data", {
  sharp2 = ksharp(kp2, threshold=0.4)
  ## should add matrices with distances to cluster centroids
  expect_equal(sort(unique(kp2$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
})




###############################################################################
## testing using self-made lists


test_that("change self-made cluster values", {
  ## omitting data should give error
  expect_error(ksharp(selfmade, threshold=0.4))
  ## with data, should succeed and alter clusters
  sharp2 = ksharp(selfmade, threshold=0.4, data=K1)
  ## should add matrices with distances to cluster centroids
  expect_equal(sort(unique(selfmade$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
})


test_that("change self-made cluster values (omitting data)", {
  selfmade2 = selfmade
  selfmade2$data = K1
  sharp2 = ksharp(selfmade2, threshold=0.4)
  ## should add matrices with distances to cluster centroids
  expect_equal(sort(unique(selfmade2$cluster)), c(1,2))
  expect_equal(sort(unique(sharp2$cluster)), c(0,1,2))
})


