# Prepare model fits for stargazer

Prepare model fits for stargazer

## Usage

``` r
starprep(
  ...,
  stat = c("std.error", "statistic", "p.value", "ci", "df"),
  se_type = NULL,
  clusters = NULL,
  alpha = 0.05
)
```

## Arguments

- ...:

  a list of lm_robust or lm objects

- stat:

  either "std.error" (the default), "statistic" (the t-statistic),
  "p.value", "ci", or "df"

- se_type:

  (optional) if any of the objects are lm objects, what standard errors
  should be used. Must only be one type and will be used for all lm
  objects passed to starprep. See `commarobust` for more.

- clusters:

  (optional) if any of the objects are lm objects, what clusters should
  be used, if clusters should be used. Must only be one vector and will
  be used for all lm objects passed to starprep. See `commarobust` for
  more.

- alpha:

  (optional) if any of the objects are lm objects, what significance
  level should be used for the p-values or confidence intervals

## Value

a list of vectors of extracted statistics for stargazers

## Details

Used to help extract statistics from lists of model fits for stargazer.
Prefers lm_robust objects, but because `stargazer` does not work with
`lm_robust` objects, `starprep` can also take `lm` objects and calls
`commarobust` to get the preferred, robust statistics.

## Examples

``` r

library(stargazer)
#> 
#> Please cite as: 
#>  Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables.
#>  R package version 5.2.3. https://CRAN.R-project.org/package=stargazer 

lm1 <- lm(mpg ~ hp, data = mtcars)
lm2 <- lm(mpg ~ hp + wt, data = mtcars)

# Use default "HC2" standard errors
stargazer(lm1, lm2,
          se = starprep(lm1, lm2),
          p = starprep(lm1, lm2, stat = "p.value"),
          omit.stat = "f")
#> 
#> % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
#> % Date and time: Thu, Sep 03, 2026 - 23:27:49
#> \begin{table}[!htbp] \centering 
#>   \caption{} 
#>   \label{} 
#> \begin{tabular}{@{\extracolsep{5pt}}lcc} 
#> \\[-1.8ex]\hline 
#> \hline \\[-1.8ex] 
#>  & \multicolumn{2}{c}{\textit{Dependent variable:}} \\ 
#> \cline{2-3} 
#> \\[-1.8ex] & \multicolumn{2}{c}{mpg} \\ 
#> \\[-1.8ex] & (1) & (2)\\ 
#> \hline \\[-1.8ex] 
#>  hp & $-$0.068$^{***}$ & $-$0.032$^{***}$ \\ 
#>   & (0.015) & (0.008) \\ 
#>   & & \\ 
#>  wt &  & $-$3.878$^{***}$ \\ 
#>   &  & (0.688) \\ 
#>   & & \\ 
#>  Constant & 30.099$^{***}$ & 37.227$^{***}$ \\ 
#>   & (2.193) & (2.078) \\ 
#>   & & \\ 
#> \hline \\[-1.8ex] 
#> Observations & 32 & 32 \\ 
#> R$^{2}$ & 0.602 & 0.827 \\ 
#> Adjusted R$^{2}$ & 0.589 & 0.815 \\ 
#> Residual Std. Error & 3.863 (df = 30) & 2.593 (df = 29) \\ 
#> \hline 
#> \hline \\[-1.8ex] 
#> \textit{Note:}  & \multicolumn{2}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
#> \end{tabular} 
#> \end{table} 
# NB: We remove the F-stat because stargazer only can use original F-stat
# which uses classical SEs

# Use default "CR2" standard errors with clusters
stargazer(lm1, lm2,
          se = starprep(lm1, lm2, clusters = mtcars$carb),
          p = starprep(lm1, lm2, clusters = mtcars$carb, stat = "p.value"),
          omit.stat = "f")
#> 
#> % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
#> % Date and time: Thu, Sep 03, 2026 - 23:27:49
#> \begin{table}[!htbp] \centering 
#>   \caption{} 
#>   \label{} 
#> \begin{tabular}{@{\extracolsep{5pt}}lcc} 
#> \\[-1.8ex]\hline 
#> \hline \\[-1.8ex] 
#>  & \multicolumn{2}{c}{\textit{Dependent variable:}} \\ 
#> \cline{2-3} 
#> \\[-1.8ex] & \multicolumn{2}{c}{mpg} \\ 
#> \\[-1.8ex] & (1) & (2)\\ 
#> \hline \\[-1.8ex] 
#>  hp & $-$0.068$^{**}$ & $-$0.032$^{**}$ \\ 
#>   & (0.015) & (0.008) \\ 
#>   & & \\ 
#>  wt &  & $-$3.878$^{*}$ \\ 
#>   &  & (1.025) \\ 
#>   & & \\ 
#>  Constant & 30.099$^{***}$ & 37.227$^{***}$ \\ 
#>   & (2.304) & (2.807) \\ 
#>   & & \\ 
#> \hline \\[-1.8ex] 
#> Observations & 32 & 32 \\ 
#> R$^{2}$ & 0.602 & 0.827 \\ 
#> Adjusted R$^{2}$ & 0.589 & 0.815 \\ 
#> Residual Std. Error & 3.863 (df = 30) & 2.593 (df = 29) \\ 
#> \hline 
#> \hline \\[-1.8ex] 
#> \textit{Note:}  & \multicolumn{2}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
#> \end{tabular} 
#> \end{table} 

# Can also specify significance levels and different standard errors
stargazer(lm1, lm2,
          ci.custom = starprep(lm1, lm2, se_type = "HC3", alpha = 0.1, stat = "ci"),
          omit.stat = "f")
#> 
#> % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
#> % Date and time: Thu, Sep 03, 2026 - 23:27:49
#> \begin{table}[!htbp] \centering 
#>   \caption{} 
#>   \label{} 
#> \begin{tabular}{@{\extracolsep{5pt}}lcc} 
#> \\[-1.8ex]\hline 
#> \hline \\[-1.8ex] 
#>  & \multicolumn{2}{c}{\textit{Dependent variable:}} \\ 
#> \cline{2-3} 
#> \\[-1.8ex] & \multicolumn{2}{c}{mpg} \\ 
#> \\[-1.8ex] & (1) & (2)\\ 
#> \hline \\[-1.8ex] 
#>  hp & $-$0.068$^{***}$ & $-$0.032$^{***}$ \\ 
#>   & ($-$0.096, $-$0.040) & ($-$0.048, $-$0.016) \\ 
#>   & & \\ 
#>  wt &  & $-$3.878$^{***}$ \\ 
#>   &  & ($-$5.184, $-$2.572) \\ 
#>   & & \\ 
#>  Constant & 30.099$^{***}$ & 37.227$^{***}$ \\ 
#>   & (26.008, 34.189) & (33.439, 41.016) \\ 
#>   & & \\ 
#> \hline \\[-1.8ex] 
#> Observations & 32 & 32 \\ 
#> R$^{2}$ & 0.602 & 0.827 \\ 
#> Adjusted R$^{2}$ & 0.589 & 0.815 \\ 
#> Residual Std. Error & 3.863 (df = 30) & 2.593 (df = 29) \\ 
#> \hline 
#> \hline \\[-1.8ex] 
#> \textit{Note:}  & \multicolumn{2}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
#> \end{tabular} 
#> \end{table} 
```
