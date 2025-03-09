# Cluster analysis of Elden Ring bosses

A cluster analysis of Elden Ring bosses (pre-DLC) based on enemy stats data
(Game version 1.10, New Game). Uses unsupervised random forests to get
proximity matrix and HDBSCAN for clustering.

## Reproducibility

First, restore the renv environment:

```r
renv::activate()
renv::restore()
```

Then, run the code in `clustering.R`.
