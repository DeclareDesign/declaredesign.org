# Randomized Response

Randomized response is a tool used in survey research to increase
subject privacy and mitigate potential bias, particularly when our
inquiries of interest pertain sensitive or controversial behavior or
beliefs. For example, researchers may be interested in measuring
participation in violence, engagement in illicit activities, or support
for the opposition in authoritarian contexts.

Let us consider a specific type of randomized response design in which
respondents are asked to use a randomization device, such as a die,
whose outcome is unobserved to the enumerator. Respondents are then
asked to answer “Yes” if the die shows numbers “1”, “2”, “3”, or “4” and
to answer truthfully if the die shows “5”, or “6.” Only the respondent
knows what they truly answered, so their anonymity is protected. The
researcher can never know the true answer provided by any particular
individual, but can back out an estimate of the rate of true responses
from what she knows about the probabilities that underly the
randomization mechanism.

Say, for example, we are interested in measuring the rate of intimate
partner violence (IPV) in a given locality. Our design compares
estimates from direct questions and questions using the randomized
response principle.

## Design Declaration

- **M**odel:

  Respondents are expected to answer the sensitive question with a
  probability $`p`$ (a known quantity). $`L_i`$ is a latent binary
  response to the sensitive question that equals 0 for respondent $`i`$
  if he or she does not engage in IPV and 1 if he or she does. This is
  defined in our design as the prevalence rate. $`Y_i`$ is the response
  observed by researchers from the randomized approach. $`D_i`$ is the
  response to the direct question of whether respondent engages in IPV.
  We expect respondents who engage in IPV to be inclined to misreport if
  asked to answer directly (we define this as the withholding rate).

- **I**nquiry:

  Our estimand is the rate of intimate partner violence in the studied
  locality, or the population mean of L. Formally, we expect
  $`Pr(Y_i = 1) = p + (1-p)Pr(L_i = 1)`$.

- **D**ata strategy:

  We collect survey data on a representative sample of 1,000 individuals
  and respondents are assigned to respond “Yes” with probability
  $`p = \frac{4}{6}`$ and truthfully with probability
  $`1 - p = \frac{1}{3}`$.

- **A**nswer strategy:

  We estimate our population rate of IPV in two ways: firstly by
  averaging the responses observed via direct question
  ($`\hat{\bar{L}} = \bar{D}`$), and secondly via randomized response:
  $`\hat{\bar{L}} = \frac{\bar{Y} - p}{1 - p}`$.

``` r

N <- 1000
prob_forced_yes <- 0.6
prevalence_rate <- 0.1
withholding_rate <- 0.5

population <- declare_population(N = N, sensitive_trait = draw_binary(prob = prevalence_rate, 
    N = N), withholder = draw_binary(prob = sensitive_trait * 
    withholding_rate, N = N), direct_answer = sensitive_trait - 
    withholder)
potential_outcomes <- declare_potential_outcomes(Y_Z_Yes = 1, 
    Y_Z_Truth = sensitive_trait)
estimand <- declare_inquiry(true_rate = mean(sensitive_trait))
assignment <- declare_assignment(Z = complete_ra(N, prob = prob_forced_yes, 
    conditions = c("Truth", "Yes")))
estimator_randomized_response <- declare_estimator(handler = label_estimator(function(data) with(data, 
    data.frame(estimate = (mean(Y) - prob_forced_yes)/(1 - 
        prob_forced_yes)))), inquiry = estimand, label = "Forced Randomized Response")
estimator_direct_question <- declare_estimator(handler = label_estimator(function(data) with(data, 
    data.frame(estimate = mean(direct_answer)))), inquiry = estimand, 
    label = "Direct Question")
randomized_response_design <- population + assignment + potential_outcomes + 
    estimand + declare_reveal(Y, Z) + estimator_randomized_response + 
    estimator_direct_question
randomized_response_design <- set_diagnosands(randomized_response_design, 
    declare_diagnosands(bias = mean(estimate - estimand)))
```

## Takeaways

``` r

diagnosis <- diagnose_design(randomized_response_design, sims = 25)
```

    ## Warning: We recommend you choose a number of simulations higher than 30.

| Design | Inquiry | Estimator | N Sims | Bias |
|:---|:---|:---|:---|:---|
| randomized_response_design | true_rate | Direct Question | 25 | -0.05 |
|  |  |  |  | (0.00) |
| randomized_response_design | true_rate | Forced Randomized Response | 25 | -0.00 |
|  |  |  |  | (0.00) |

Our diagnosis of the design indicates that randomized response yields an
unbiased estimate of the true rate of IPV in the study sample and offer
a better alternative to direct questions involving sensitive or
controversial inquiries.

## References

Blair, Graeme, Kosuke Imai, and Yang-Yang Zhou. 2015. “Design and
Analysis of the Randomized Response Technique.” *Journal of the American
Statistical Association* 110 (511): 1304–19.

Greenberg, Bernard G, Abdel-Latif A Abul-Ela, Walt R Simmons, and Daniel
G Horvitz. 1969. “The Unrelated Question Randomized Response Model:
Theoretical Framework.” *Journal of the American Statistical
Association* 64 (326): 520–39.

Warner, Stanley L. 1965. “Randomized Response: A Survey Technique for
Eliminating Evasive Answer Bias.” *Journal of the American Statistical
Association* 60 (309): 63–69.
