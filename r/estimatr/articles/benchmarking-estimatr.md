# Benchmarking estimatr

This document benchmarks the speed of `lm_robust` against other
estimators. Our performance is slightly better than base R when using
classical standard errors, but most of our improvements come when
estimating robust standard errors.

The times are milliseconds and are a median over 200 runs for all but
the CR2 case, which was taken on a sample of 50 runs, using the
`microbenchmark` package. This benchmarking was done on a 2017 MacBook
Air, with a 1.8 GHz Intel Core i5 CPU and 8 GB of memory.

## Linear regression

I test our speed in estimating coefficients, standard errors, and doing
inference on four different datasets (500 and 5000 observations; 5 and
50 covariates) and across several different specifications. Below I
preview the results comparing
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
to base R for fitting coefficients and a commonly used package for
robust standard errors, such as the `sandwich` package. In the two
largest datasets, our method is almost always faster and at worst is the
same as base R, and only with classical standard errors. When it comes
to the biggest gains, using
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
to get HC2 or Stata-like cluster-robust standard errors will roughly
halve your waiting time. If you want CR2 standard errors,
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
can reduce your run time by a factor of 10!

| N. Obs | N. Coefs | Estimator | Classical SEs | HC2 SEs | Stata clustered SEs | CR2 SEs |
|----|----|----|----|----|----|----|
| 500 | 5 | [`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md) | 7.9 | 7.9 | 7.6 | **10.6** |
|  |  | base + sandwich/clubSandwich | **2.3** | **5** | **6.7** | 45.9 |
|  |  |  |  |  |  |  |
| 5000 | 5 | [`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md) | 18.7 | **18.8** | **17.8** | **317** |
|  |  | base + sandwich/clubSandwich | **6.4** | 26 | 45.9 | 2121.6 |
|  |  |  |  |  |  |  |
| 500 | 50 | [`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md) | 24.3 | **28.7** | **29.5** | **269** |
|  |  | base + sandwich/clubSandwich | **17.5** | 43.9 | 69.5 | 404.3 |
|  |  |  |  |  |  |  |
| 5000 | 50 | [`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md) | **80.8** | **111.6** | **90.5** | **8464** |
|  |  | base + sandwich/clubSandwich | 88.5 | 287 | 480.7 | 2.510^{4} |

## Instrumental variables regression

| N. Obs | N. Coefs | Estimator | Classical SEs | HC0 SEs | CR2 SEs |  |
|----|----|----|----|----|----|----|
| 500 | 5 | [`estimatr::iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.md) | **8.2** | **8.9** | **11** |  |
|  |  | AER + ivpack/clubSandwich | 11.9 | 10.1 | 49.9 |  |
|  |  |  |  |  |  |  |
| 5000 | 5 | [`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md) | 17.8 | **18.9** | **156.9** |  |
|  |  | AER + ivpack/clubSandwich | **15.7** | 22.6 | 1814.1 |  |
|  |  |  |  |  |  |  |
| 500 | 50 | [`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md) | **25.8** | **26.1** | **80.9** |  |
|  |  | AER + ivpack/clubSandwich | 46.2 | 51.8 | 310.3 |  |
|  |  |  |  |  |  |  |
| 5000 | 50 | [`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md) | **85.4** | **93.9** | **2594.4** |  |
|  |  | AER + ivpack/clubSandwich | 144.7 | 316.4 | 1.610^{4} |  |

## Code that generated these data

To see the exact comparisons, see below.

    library(estimatr)
    library(microbenchmark)
    # Create some data sets of different sizes for testing below
    set.seed(42)
    data_size <- expand.grid(list(ns = c(500, 5000), ps = c(5, 50)))
    data_list <- lapply(
      1:nrow(data_size), 
      function(i) {
        n <- data_size$ns[i]
        p <- data_size$ps[i]
        y <- rnorm(n)
        X <- matrix(rnorm(n*p), n, p)
        return(data.frame(y, X))
      }
    )

First, we can look at the classical standard errors case. We do a few
more computations than `summary.lm` does, and have a bit more overhead.

    test_base <- lapply(data_list, function(dat) {
      mbo <- summary(microbenchmark(
        'lm_robust' = lm_robust(y ~ ., data = dat, se_type = "classical"),
        'base' = summary(lm(y ~ ., data = dat)),
        times = 200L
      ),
      unit = "ms")
      return(mbo[, c("expr", "median")])
    })

The following table has the median time in milliseconds across 50 runs
of each estimator for each of the different data sets.

| Estimator | N=500, P=5 | N=500, P=50 | N=5000, P=5 | N=500, P=50 |
|:----------|-----------:|------------:|------------:|------------:|
| lm_robust |          8 |          19 |          24 |          81 |
| base      |          2 |           6 |          18 |          88 |

However, the real speed gains come with robust standard errors. Let’s
compare `lm_robust` to getting “HC2” standard errors and doing inference
using them from the `coeftest` and `sandwich` packages.

    library(sandwich)
    library(lmtest)

    test_rob <- lapply(data_list, function(dat) {
      mbo <- summary(microbenchmark(
        'lm_robust' = lm_robust(y ~ ., data = dat, se_type = "HC2"),
        'lm + coeftest + sandwich' = {
          lmo <- lm(y ~ ., data = dat)
          coeftest(lmo, vcov = vcovHC(lmo, type = "HC2"))
        },
        times = 200L
      ),
      unit = "ms")
      return(mbo[, c("expr", "median")])
    })

| Estimator                | N=500, P=5 | N=500, P=50 | N=5000, P=5 | N=500, P=50 |
|:-------------------------|-----------:|------------:|------------:|------------:|
| lm_robust                |          8 |          19 |          29 |         112 |
| lm + coeftest + sandwich |          5 |          26 |          44 |         287 |

What about with Stata’s clustered standard errors using `tapply` and
`sandwich`?

    # Commonly used function attributed mostly to M. Arai replicating Stata 
    # clustered SEs in R using sandwich and lmtest packages
    cluster_robust_se <- function(model, cluster){
      M <- length(unique(cluster))
      N <- length(cluster)
      K <- model$rank
      dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
      uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
      rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
      rcse.se <- coeftest(model, rcse.cov)
      return(list(rcse.cov, rcse.se))
    }

    test_cl <- lapply(data_list, function(dat) {
      cluster <- sample(nrow(dat)/5, size = nrow(dat), replace = TRUE)
      mbo <- summary(microbenchmark(
        'lm_robust' = lm_robust(
          y ~ ., 
          data = dat, 
          clusters = cluster, 
          se_type = "stata"
        ),
        'lm + coeftest + sandwich' = {
          lmo <- lm(y ~ ., data = dat)
          cluster_robust_se(lmo, cluster)
        },
        times = 200L
      ),
      unit = "ms")
      return(mbo[, c("expr", "median")])
    })

| Estimator                | N=500, P=5 | N=500, P=50 | N=5000, P=5 | N=500, P=50 |
|:-------------------------|-----------:|------------:|------------:|------------:|
| lm_robust                |          8 |          18 |          30 |          90 |
| lm + coeftest + sandwich |          7 |          46 |          70 |         481 |

The original authors who came up with a generalized version of the CR2
errors and accompanying Satterthwaite-like corrected degrees of freedom
have their own package,
[`clubSandwich`](https://github.com/jepusto/clubSandwich), that provides
estimators for many methods. We show here how much faster our
implementation is for simple linear regression.

    library(clubSandwich)

    test_cr2 <- lapply(data_list, function(dat) {
      cluster <- sample(nrow(dat)/5, size = nrow(dat), replace = TRUE)
      mbo <- summary(microbenchmark(
        'lm_robust' = lm_robust(
          y ~ ., 
          data = dat,
          clusters = cluster, 
          se_type = "CR2"
        ),
        'lm + clubSandwich' = {
          lmo <- lm(y ~ ., data = dat)
          coef_test(lmo, vcov = vcovCR(lmo, cluster = cluster, type = "CR2"))
        },
        times = 50L
      ),
      unit = "ms")
      return(mbo[, c("expr", "median")])
    })

    knitr::kable(create_tab(test_cr2), col.names = col_names)

| Estimator         | N=500, P=5 | N=500, P=50 | N=5000, P=5 | N=500, P=50 |
|:------------------|-----------:|------------:|------------:|------------:|
| lm_robust         |         11 |         317 |         269 |        8464 |
| lm + clubSandwich |         46 |        2122 |         404 |       25266 |

What about for instrumental variables?

    library(AER)
    library(ivpack)
    library(clubSandwich)

    test_iv <- lapply(data_list, function(dat) {
      form <- as.formula(paste0(
        "y ~ ",
        paste(names(dat)[substr(names(dat), 1, 1) == "X"], collapse = " + "),
        " | ",
        paste(names(dat)[substr(names(dat), 1, 1) == "Z"], collapse = " + ")
      ))
      mbo <- summary(microbenchmark(
        'iv_robust' = iv_robust(
          form, 
          data = dat,
          se_type = "classical"
        ),
        'AER::ivreg' = {
          ivo <- summary(ivreg(form, data = dat))
        },
        times = 200L
      ),
      unit = "ms")
      return(mbo[, c("expr", "median")])
    })

    knitr::kable(create_tab(test_iv), col.names = col_names)

| Estimator  | N=500, P=5 | N=500, P=50 | N=5000, P=5 | N=500, P=50 |
|:-----------|-----------:|------------:|------------:|------------:|
| iv_robust  |          8 |          18 |          26 |          85 |
| AER::ivreg |         12 |          16 |          46 |         145 |

    test_iv_rob <- lapply(data_list, function(dat) {
      form <- as.formula(paste0(
        "y ~ ",
        paste(names(dat)[substr(names(dat), 1, 1) == "X"], collapse = " + "),
        " | ",
        paste(names(dat)[substr(names(dat), 1, 1) == "Z"], collapse = " + ")
      ))
      mbo <- summary(microbenchmark(
        'iv_robust' = iv_robust(
          form, 
          data = dat,
          se_type = "HC0"
        ),
        'AER::ivreg + ivpack::robust.se' = {
          ivo <- robust.se(ivreg(form, data = dat))
        },
        times = 200L
      ),
      unit = "ms")
      return(mbo[, c("expr", "median")])
    })

    knitr::kable(create_tab(test_iv_rob), col.names = col_names)

| Estimator | N=500, P=5 | N=500, P=50 | N=5000, P=5 | N=500, P=50 |
|:---|---:|---:|---:|---:|
| iv_robust | 9 | 19 | 26 | 94 |
| AER::ivreg + ivpack::robust.se | 10 | 23 | 52 | 316 |

    test_iv_cl <- lapply(data_list, function(dat) {
      cluster <- sample(nrow(dat)/5, size = nrow(dat), replace = TRUE)
      form <- as.formula(paste0(
        "y ~ ",
        paste(names(dat)[substr(names(dat), 1, 1) == "X"], collapse = " + "),
        " | ",
        paste(names(dat)[substr(names(dat), 1, 1) == "Z"], collapse = " + ")
      ))
      mbo <- summary(microbenchmark(
        'iv_robust' = iv_robust(
          form, 
          data = dat,
          clusters = cluster,
          se_type = "CR2"
        ),
        'AER::ivreg + clubSandwich' = {
          ivo <- clubSandwich::coef_test(ivreg(form, data = dat), cluster = cluster, vcov = "CR2")
        },
        times = 50L
      ),
      unit = "ms")
      return(mbo[, c("expr", "median")])
    })

    knitr::kable(create_tab(test_iv_cl), col.names = col_names)

| Estimator                 | N=500, P=5 | N=500, P=50 | N=5000, P=5 | N=500, P=50 |
|:--------------------------|-----------:|------------:|------------:|------------:|
| iv_robust                 |         11 |         157 |          81 |        2594 |
| AER::ivreg + clubSandwich |         50 |        1814 |         310 |       15589 |

    sessionInfo()
    #> R version 3.5.0 (2018-04-23)
    #> Platform: x86_64-apple-darwin15.6.0 (64-bit)
    #> Running under: macOS High Sierra 10.13.3
    #> 
    #> Matrix products: default
    #> BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
    #> LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
    #> 
    #> locale:
    #> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #>  [1] clubSandwich_0.3.2      ivpack_1.2             
    #>  [3] AER_1.2-5               survival_2.41-3        
    #>  [5] sandwich_2.4-0          lmtest_0.9-36          
    #>  [7] zoo_1.8-1               car_3.0-0              
    #>  [9] carData_3.0-1           RcppArmadillo_0.8.500.0
    #> [11] RcppEigen_0.3.3.4.0     microbenchmark_1.4-4   
    #> [13] estimatr_0.8.0         
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] zip_1.0.0         Rcpp_0.12.17      highr_0.6        
    #>  [4] compiler_3.5.0    pillar_1.2.3      cellranger_1.1.0 
    #>  [7] forcats_0.3.0     tools_3.5.0       digest_0.6.15    
    #> [10] evaluate_0.10.1   tibble_1.4.2      lattice_0.20-35  
    #> [13] texreg_1.36.23    rlang_0.2.1       openxlsx_4.1.0   
    #> [16] Matrix_1.2-14     curl_3.2          yaml_2.1.19      
    #> [19] haven_1.1.1       rio_0.5.10        stringr_1.3.1    
    #> [22] knitr_1.20        rprojroot_1.3-2   grid_3.5.0       
    #> [25] data.table_1.11.4 readxl_1.1.0      foreign_0.8-70   
    #> [28] rmarkdown_1.9     Formula_1.2-3     magrittr_1.5     
    #> [31] codetools_0.2-15  splines_3.5.0     backports_1.1.2  
    #> [34] htmltools_0.3.6   abind_1.4-5       stringi_1.2.2
