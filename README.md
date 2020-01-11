# ksharp

R package for cluster sharpening

![Status](https://travis-ci.org/tkonopka/ksharp.svg?branch=master)
[![codecov](https://codecov.io/gh/tkonopka/ksharp/branch/master/graph/badge.svg)](https://codecov.io/gh/tkonopka/ksharp)


&nbsp;

Cluster sharpening is a procedure that adjusts an existing clustering in order to increase contrast between groups. In the example below, a dataset is sharpened at various levels. 

<img src="https://github.com/tkonopka/ksharp/blob/master/vignettes/ksharp-thresholds.png?raw=true" alt="cluster sharpening at thresholds from 0.05 to 0.4" width="600px"></img>

Package `ksharp` implements a general interface for cluster sharpening along with several algorithms. The package is compatible with clusterings produced by k-means, pam, dbscan, as well as cluster definitions produced manually.


## Installation

The package can be installed from github through devtools.

```{r}
library(devtools)
install_github("tkonopka/ksharp")
```



## Example

For a minimal example, consider a kmeans clustering of the iris dataset.

```{r}
iris.data = as.matrix(iris[, 1:4])
rownames(iris.data) = paste0("iris_", seq_len(nrow(iris.data)))
iris.clustered = kmeans(iris.data, centers=3)
table(iris.clustered$cluster)
```

By construction, the dataset is partitioned into three components, labeled 1, 2, 3. Sharpening involves amending the result by shifting some of the observation into a new group with index 0.

```{r}
library(ksharp)
iris.sharp.5 = ksharp(iris.clustered, threshold=0.05, data=iris.data)
table(iris.sharp.5$cluster)
iris.sharp.10 = ksharp(iris.clustered, threshold=0.1, data=iris.data)
table(iris.sharp.10$cluster)
```

See `help(ksharp)` for further usage settings. See the package vignette provides background information on cluster sharpening and further examples. 

