# Override environment via shim

Override environment via shim

## Usage

``` r
clone_dot_edit_env(dot, ..., to_replace = list(...))

clone_step_edit(step, ..., to_replace = list(...))

clone_design_edit(design, ..., to_replace = list(...))
```

## Examples

``` r
if (FALSE) { # \dontrun{
here_i_am <- "foo"
dot <- quo(here_i_am)
dot2 <- DeclareDesign:::clone_dot_edit_env(dot, here_i_am = "some_message", xyxyx = "bar")
rlang::eval_tidy(dot)
rlang::eval_tidy(dot2)
} # }
if (FALSE) { # \dontrun{
N <- 50

pop50 <- declare_model(N=N, noise=rnorm(N))
nrow(pop50())

pop100 <- DeclareDesign:::clone_step_edit(pop50, N=100)
nrow(pop100())
nrow(pop50())

} # }
```
