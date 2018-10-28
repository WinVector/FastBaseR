Comparing Vector Versus scalar Implementations of Conway's Game of Life
================

``` r
library("FastBaseR")
library("microbenchmark")
library("WVPlots")

life_step_scalar <- function(d) {
  nrow <- dim(d)[[1]]
  ncol <- dim(d)[[2]]
  dnext <- d
  for(i in seq_len(nrow)) {
    for(j in seq_len(ncol)) {
      pop <- 0
      if(i>1) {
        if(j>1) {
          pop <- pop + d[i-1, j-1]
        }
        pop <- pop + d[i-1, j]
        if(j<ncol) {
          pop <- pop + d[i-1, j+1]
        }
      }
      if(j>1) {
        pop <- pop + d[i, j-1]
      }
      if(j<ncol) {
        pop <- pop + d[i, j+1]
      }
      if(i<nrow) {
        if(j>1) {
          pop <- pop + d[i+1, j-1]
        }
        pop <- pop + d[i+1, j]
        if(j<ncol) {
          pop <- pop + d[i+1, j+1]
        }
      }
      dnext[i,j] <- (pop==3) | (d[i,j] & (pop>=2) & (pop<=3))
    }
  }
  dnext
}

set.seed(2352)
grid_size = 1000
d <- matrix(data = sample(c(TRUE, FALSE), grid_size*grid_size, replace = TRUE), 
            nrow = grid_size, ncol = grid_size)
```

``` r
d_next_scalar <- life_step_scalar(d)
d_next_vector <- life_step(d)
if(!isTRUE(all.equal(d_next_scalar, d_next_vector))) {
  stop("mismatch")
}
```

``` r
timing <- microbenchmark(
  life_step_scalar = life_step_scalar(d),
  life_step_vector = life_step(d),
  times = 10L)
```

``` r
print(timing)
```

    ## Unit: milliseconds
    ##              expr       min        lq      mean   median        uq
    ##  life_step_scalar 2465.5321 2484.6832 2528.1573 2488.318 2595.8243
    ##  life_step_vector  136.7932  142.3746  181.2697  149.575  257.7223
    ##       max neval cld
    ##  2700.931    10   b
    ##   266.507    10  a

``` r
tm <- data.frame(timing)
tm$method <- as.factor(tm$expr)
tm$seconds <- tm$time/1e+9
tm$method <- reorder(tm$method, tm$seconds)
ScatterBoxPlotH(tm, "seconds", "method",  "task timings")
```

![](Timing_files/figure-markdown_github/present-1.png)
