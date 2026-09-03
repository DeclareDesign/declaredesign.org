# Regression Tables with estimatr

Preparing regression tables with estimatr is possible with all of the
major r-to-LaTeX packages, including `texreg`, `modelsummary`,
`stargazer`, `xtable`, and `huxtable`.

## Setup

First we’ll load both `estimatr` and `magrittr` (for pipes).

``` r

library(estimatr)
library(magrittr)
```

## Texreg

Texreg operates directly on an `lm_robust` object. If you would like
standard errors instead of confidence intervals, use
`include.ci = FALSE`.

``` r

library(texreg)
fit <- lm_robust(mpg ~ disp, data = mtcars)
texreg(fit, include.ci = FALSE)
```

    ## 
    ## \begin{table}
    ## \begin{center}
    ## \begin{tabular}{l c}
    ## \hline
    ##  & Model 1 \\
    ## \hline
    ## (Intercept) & $29.60^{***}$ \\
    ##             & $(1.49)$      \\
    ## disp        & $-0.04^{***}$ \\
    ##             & $(0.01)$      \\
    ## \hline
    ## R$^2$       & $0.72$        \\
    ## Adj. R$^2$  & $0.71$        \\
    ## Num. obs.   & $32$          \\
    ## RMSE        & $3.25$        \\
    ## \hline
    ## \multicolumn{2}{l}{\scriptsize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}}
    ## \end{tabular}
    ## \caption{Statistical models}
    ## \label{table:coefficients}
    ## \end{center}
    ## \end{table}

## modelsummary

`modelsummary` operates directly on `lm_robust` objects:

``` r

library(modelsummary)
fit1 <- lm(mpg ~ disp, data = mtcars)
fit2 <- lm(mpg ~ hp, data = mtcars)
modelsummary(list(fit1, fit2))
```

To learn how to customize the output table and/or statistics, please
consult the `modelsummary` README file on Github:
<https://github.com/vincentarelbundock/modelsummary>

## Stargazer

Stargazer has to be *tricked* – there’s unfortunately no way around this
unless something in the stargazer package changes. The maintainer has
indicated that he has no active plans to update stargazer in the future.
First, run the regression using `lm`. Then you can pass the `lm` fits to
`starprep` a function which transforms the `lm` fits into `lm_robust`
fits and then prepare the appropriate statistic you requested. The
`starprep` function defaults to returning standard errors and uses
`lm_robust` defaults for standard errors (robust HC2 SEs are the default
in both `lm_robust`). Then you pass the `lm` fit to stargazer and then
pass `starprep(fit)` to the `se` argument in `stargazer`. This process
works great with multiple fits.

``` r

library(stargazer)
fit_1 <- lm(mpg ~ disp, data = mtcars)
fit_2 <- lm(mpg ~ hp, data = mtcars)
stargazer(fit_1, fit_2, se = starprep(fit_1, fit_2))
```

    ## 
    ## \begin{table}[!htbp] \centering 
    ##   \caption{} 
    ##   \label{} 
    ## \begin{tabular}{@{\extracolsep{5pt}}lcc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{2}{c}{\textit{Dependent variable:}} \\ 
    ## \cline{2-3} 
    ## \\[-1.8ex] & \multicolumn{2}{c}{mpg} \\ 
    ## \\[-1.8ex] & (1) & (2)\\ 
    ## \hline \\[-1.8ex] 
    ##  disp & $-$0.041$^{***}$ &  \\ 
    ##   & (0.005) &  \\ 
    ##   & & \\ 
    ##  hp &  & $-$0.068$^{***}$ \\ 
    ##   &  & (0.015) \\ 
    ##   & & \\ 
    ##  Constant & 29.600$^{***}$ & 30.099$^{***}$ \\ 
    ##   & (1.490) & (2.193) \\ 
    ##   & & \\ 
    ## \hline \\[-1.8ex] 
    ## Observations & 32 & 32 \\ 
    ## R$^{2}$ & 0.718 & 0.602 \\ 
    ## Adjusted R$^{2}$ & 0.709 & 0.589 \\ 
    ## Residual Std. Error (df = 30) & 3.251 & 3.863 \\ 
    ## F Statistic (df = 1; 30) & 76.513$^{***}$ & 45.460$^{***}$ \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Note:}  & \multicolumn{2}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

Below are some more examples, although we hide the output for brevity.

``` r

# Can also specify clusters and standard error type
stargazer(
  fit_1, fit_2,
  se = starprep(fit_1, fit_2, clusters = mtcars$cyl, se_type = "stata")
)

# Can also precompute robust objects to save computation time
# using `commarobust`
fit_1_r <- commarobust(fit_1)
fit_2_r <- commarobust(fit_2)
stargazer(fit_1, fit_2,
          se = starprep(fit_1_r, fit_2_r),
          p = starprep(fit_1_r, fit_2_r, stat = "p.value"))

# can also easily get robust confidence intervals
stargazer(fit_1, fit_2,
          ci.custom = starprep(fit_1_r, fit_2_r, stat = "ci"))
```

## xtable

`xtable` works directly on a data.frame, so we just have to prep the
`lm_robust` output with the `tidy` function.

``` r

library(xtable)
fit <- lm_robust(mpg ~ disp, data = mtcars)
fit %>% tidy %>% xtable()
```

    ## % latex table generated in R 4.6.1 by xtable 1.8-8 package
    ## % Thu Sep  3 23:28:54 2026
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rlrrrrrrrl}
    ##   \hline
    ##  & term & estimate & std.error & statistic & p.value & conf.low & conf.high & df & outcome \\ 
    ##   \hline
    ## 1 & (Intercept) & 29.60 & 1.49 & 19.86 & 0.00 & 26.56 & 32.64 & 30.00 & mpg \\ 
    ##   2 & disp & -0.04 & 0.01 & -7.97 & 0.00 & -0.05 & -0.03 & 30.00 & mpg \\ 
    ##    \hline
    ## \end{tabular}
    ## \end{table}

## huxtable

`huxtable`, too, works on a data.frame, so all we have to do is prep
with `tidy`.

``` r

library(huxtable)
fit <- lm_robust(mpg ~ disp, data = mtcars)
fit %>% tidy %>% hux() %>% print_latex()
```

    ## 
    ## 
    ## ```{=latex}
    ##  
    ##   \providecommand{\huxb}[2]{\arrayrulecolor[RGB]{#1}\global\arrayrulewidth=#2pt}
    ##   \providecommand{\huxvb}[2]{\color[RGB]{#1}\vrule width #2pt}
    ##   \providecommand{\huxtpad}[1]{\rule{0pt}{#1}}
    ##   \providecommand{\huxbpad}[1]{\rule[-#1]{0pt}{#1}}
    ## 
    ## \begin{table}[ht]
    ## \begin{centerbox}
    ## \begin{threeparttable}
    ##  \setlength{\tabcolsep}{0pt}
    ## \begin{tabular}{l l l l l l l l l}
    ## 
    ## 
    ## \hhline{}
    ## \arrayrulecolor{black}
    ## 
    ## \multicolumn{1}{!{\huxvb{0, 0, 0}{0}}l!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedright \hspace{6pt} term \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} estimate \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} std.error \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} statistic \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} p.value \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} conf.low \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} conf.high \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} df \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{l!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedright \hspace{6pt} outcome \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]
    ## 
    ## 
    ## \hhline{}
    ## \arrayrulecolor{black}
    ## 
    ## \multicolumn{1}{!{\huxvb{0, 0, 0}{0}}l!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedright \hspace{6pt} (Intercept) \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 29.6\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 1.49\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 19.9\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 8.19e-19 \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 26.6\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 32.6\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 30 \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{l!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedright \hspace{6pt} mpg \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]
    ## 
    ## 
    ## \hhline{}
    ## \arrayrulecolor{black}
    ## 
    ## \multicolumn{1}{!{\huxvb{0, 0, 0}{0}}l!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedright \hspace{6pt} disp \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} -0.0412 \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.00517 \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} -7.97 \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 6.74e-09 \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} -0.0518 \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} -0.0307 \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 30 \hspace{6pt}\huxbpad{6pt}} &
    ## \multicolumn{1}{l!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedright \hspace{6pt} mpg \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]
    ## 
    ## 
    ## \hhline{}
    ## \arrayrulecolor{black}
    ## \end{tabular}
    ## \end{threeparttable}\par\end{centerbox}
    ## 
    ## \end{table}
    ##  
    ## ```
