# Custom functions and DeclareDesign

The `declare_*` functions in **DeclareDesign** use functions in the
**fabricatr**, **randomizr**, and **estimatr** packages as defaults,
which work great for most designs. Sometimes, however, you might want to
write your own function. This advanced vignette declares a design using
only custom functions.

First, we’ll write custom functions for each of the steps in the design.
All functions must take a data.frame and return a data.frame, with the
exception of a population step, whose inputs can be anything but whose
output must be a data.frame.

``` r

# M: Model
custom_population <- function(N) {
  data.frame(u = rnorm(N))
}
custom_potential_outcomes <-
  function(data) {
    within(data,{
      Y_Z_0 <- u
      Y_Z_1 <- 0.25 + u
    })
  }

# I: Inquiry
custom_inquiry <- function(data, label) {
  data.frame(inquiry = label,
  inquiry = with(data, median(Y_Z_1 - Y_Z_0)))
}

# D: Data Strategy
custom_sampling <- function(data) {
     data$S <- rbinom(n = nrow(data),
            size = 1,
            prob = 0.1)
     data[data$S == 1, ]
}

custom_assignment <- function(data) {
  data$Z <- rbinom(n = nrow(data),
         size = 1,
         prob = 0.5)
  data
}

custom_reveal <- function(data){
  within(data, Y <- Y_Z_1 * Z + Y_Z_0 * (1 - Z))
}

# A: Answer strategy
custom_estimator <- function(data){
  data.frame(estimate = with(data, mean(Y)))
}
```

In order to declare the design, we pass each of the custom functions to
the `handler` argument of each declaration step:

``` r

design <- 
  declare_model(handler = custom_population, N = 100) + 
  declare_potential_outcomes(handler = custom_potential_outcomes) + 
  declare_inquiry(handler = custom_inquiry, label = "medianTE") + 
  declare_sampling(handler = custom_sampling) + 
  declare_assignment(handler = custom_assignment) + 
  declare_reveal(handler = custom_reveal) + 
  declare_estimator(handler = tidy_estimator(custom_estimator), 
                    inquiry = "medianTE")
```

    ## Warning in tidy_estimator(custom_estimator): tidy_estimator() has been
    ## deprecated. Please use label_estimator() instead.

``` r

head(draw_data(design))
```

    ##       u Y_Z_1 Y_Z_0 S Z    Y
    ## 7  1.51  1.76  1.51 1 0 1.51
    ## 24 1.21  1.46  1.21 1 0 1.21
    ## 35 0.50  0.75  0.50 1 0 0.50
    ## 50 0.66  0.91  0.66 1 0 0.66
    ## 64 1.40  1.65  1.40 1 1 1.65
    ## 69 0.92  1.17  0.92 1 1 1.17

``` r

run_design(design)
```

    ##    inquiry inquiry.1 estimator estimate
    ## 1 medianTE      0.25 estimator     0.11

This example used very simple custom functions, but this framework is
flexible enough to accommodate any design step that can be expressed as
a function of data that returns data.
