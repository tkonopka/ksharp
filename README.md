# ksharp

![Status](https://travis-ci.org/tkonopka/ksharp.svg?branch=master)
[![codecov](https://codecov.io/gh/tkonopka/ksharp/branch/master/graph/badge.svg)](https://codecov.io/gh/tkonopka/ksharp)


&nbsp;

Cluster sharpening is a procedure that adjusts an existing clustering in order to increase contrast between groups. In the example below, a dataset is sharpened at various levels. 

<img src="https://github.com/tkonopka/ksharp/blob/master/vignettes/ksharp-thresholds.png?raw=true" alt="cluster sharpening at thresholds from 0.05 to 0.4" width="600px"></img>

`ksharp` is an R package. It implements a general interface for cluster sharpening and implements several distinct algorithms. The package is compatible with processing clusterings produced by k-means, pam, dbscan, as well as cluster definitions produced manually.

The package vignette provides additional information on background and usage.



