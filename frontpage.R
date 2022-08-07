
library(tidyverse)
library(patchwork)
library(DeclareDesign)

ATE <- 0.1

design <- 
  declare_model(
    N = 100,
    U = rnorm(N),
    potential_outcomes(Y ~ ATE * Z + U)
  ) + 
  declare_inquiry(ATE = ATE) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_estimator(Y ~ Z, model = lm_robust)

