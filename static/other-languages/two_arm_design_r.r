library(DeclareDesign)

sample_size    <- 100
effect_size    <- 0.5
number_treated <- 50

two_arm_design <-
  
  # M: Model
  declare_population(
    N = sample_size, 
    U = rnorm(N),
    potential_outcomes(Y ~ effect_size * Z + U)
  ) +
  
  # I: Inquiry
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  
  # D: Data Strategy
  declare_assignment(m = number_treated) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  
  # A: Answer Strategy
  declare_estimator(Y ~ Z, model = lm_robust, estimand = "ATE")