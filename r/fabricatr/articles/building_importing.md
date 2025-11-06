# Building and Importing Data

**fabricatr** is a package designed to help you imagine your data before
you collect it. While many solutions exist for creating simulated
datasets, **fabricatr** is specifically designed to make the creation of
realistic social science datasets easy. In particular, we need to be
able to imagine *correlated* data and *hierarchical* data.

## Basics

Using **fabricatr** begins by calling the function
[`fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md).
[`fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
can be used to create single-level of hierarchical data. There are three
main ways to call
[`fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md):

- Making a single-level dataset by specifying how many observations you
  would like
- Making a single-level dataset by importing data and optionally
  modifying it by creating new variables
- Making a hierarchical dataset.

## Single-level datasets from scratch

Making a single-level dataset begins with providing the argument `N`, a
number representing the number of observations you wish to create,
followed by a series of variable definitions. Variables can be defined
using any function you have access to in R. **fabricatr** provides
several simple functions for generating common types of data. These are
covered below. Functions that create subsequent variables can rely on
previously created variables, which ensures that variables can be
related to one another:

``` r
library(fabricatr)
my_data <- fabricate(N = 5, Y = runif(N), Y2 = Y * 5)
my_data
```

| ID  |    Y |  Y2 |
|:----|-----:|----:|
| 1   | 0.78 | 3.9 |
| 2   | 0.50 | 2.5 |
| 3   | 0.54 | 2.7 |
| 4   | 0.61 | 3.0 |
| 5   | 0.32 | 1.6 |

This simple example makes use of `R`’s built-in `runif` command. The
rest of the tutorial assumes a familiarity with `R` and its basic data
generating processes.

## Filling out observations.

`fabricate` is intended to make rectangular data frames: this means that
each variable added at a level needs to be the same length. Failure to
provide equal-length variables will result in an error. We provide a
convenient helper function, `recycle`, to help expand existing data to
fit the length of your level. Here, let’s use the existing `month`
variable from `R` to generate data using a month:

``` r
month_gdp <- fabricate(
  N = 20,
  month_name = recycle(month.abb),
  gdp_growth = rnorm(N, 0.5, 0.5)
)
```

`month.abb` contains the months of the year: \[“Jan”, “Feb”, “Mar”, …,
“Dec”\]. It is obvious that although we are asking for 20 observations,
there are only twelve months in the year. `recycle` will automatically
wrap the month text resulting in a data frame with the 12 months “Jan”
through “Dec”, followed by 8 months “Jan” through “Aug”.

## Single-level datasets using existing data

Instead of specifying the argument `N`, users can specify the argument
`data` to import existing datasets. Once a dataset is imported,
subsequent variables have access to `N`, representing the number of
observations in the imported data. This makes it easy to augment
existing data with simulations based on that data.

In this example, we make use of the `quakes` dataset, built into `R`,
which describes characteristics of earthquakes off the coast of Fiji.
The `mag` variable in this dataset contains the richter magnitude of the
earthquakes. We will expand this data to add variables modelling
hypothetical fatalities and insurance costs:

``` r
simulated_quake_data <- fabricate(
  data = quakes,
  fatalities = round(pmax(0, rnorm(N, mean = mag)) * 100),
  insurance_cost = fatalities * runif(N, 1000000, 2000000)
)
head(simulated_quake_data)
```

| lat | long | depth | mag | stations | fatalities | insurance_cost |
|----:|-----:|------:|----:|---------:|-----------:|---------------:|
| -20 |  182 |   562 | 4.8 |       41 |        494 |    564,736,181 |
| -21 |  181 |   650 | 4.2 |       15 |        390 |    414,044,159 |
| -26 |  184 |    42 | 5.4 |       43 |        596 |    708,701,956 |
| -18 |  182 |   626 | 4.1 |       19 |        293 |    567,537,664 |
| -20 |  182 |   649 | 4.0 |       11 |        487 |    920,442,976 |
| -20 |  184 |   195 | 4.0 |       12 |        319 |    436,673,133 |

Notice that variable creation calls are able to make reference to both
the variables in the imported data set, and newly created variables.
Also, function calls can be arbitrarily nested – the variable fatalities
uses several nested function calls.

## Hierarchical data

The most powerful use of **fabricatr** is to create hierarchical
(“nested”) data. In the example below, we create 5 countries, each of
which has 10 provinces. We also have covariates at the country level
(GDP per capita and life expectancy) and at the provincial level
(presence of natural resources, and presence of manufacturing industry):

``` r
country_data <-
  fabricate(
    countries = add_level(
      N = 5,
      gdp_per_capita = runif(N, min = 10000, max = 50000),
      life_expectancy = 50 + runif(N, 10, 20) + ((gdp_per_capita > 30000) * 10)
    ),
    provinces = add_level(
      N = 10,
      natural_resources = draw_binary(prob = 0.3, N = N),
      manufacturing = draw_binary(prob = 0.7, N = N)
    )
  )
head(country_data)
```

| countries | gdp_per_capita | life_expectancy | provinces | natural_resources | manufacturing |
|:----------|---------------:|----------------:|:----------|------------------:|--------------:|
| 1         |         40,451 |              73 | 01        |                 1 |             1 |
| 1         |         40,451 |              73 | 02        |                 0 |             1 |
| 1         |         40,451 |              73 | 03        |                 0 |             0 |
| 1         |         40,451 |              73 | 04        |                 1 |             1 |
| 1         |         40,451 |              73 | 05        |                 0 |             0 |
| 1         |         40,451 |              73 | 06        |                 0 |             1 |

Several things can be observed in this example. First, fabricate knows
that your second
[`add_level()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
command will be nested under the first level of data. Each level gets
its own ID variable, in addition to the variables you create. Second,
the meaning of the variable “N” changes. During the
[`add_level()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
call for countries, N is 5. During the
[`add_level()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
call for provinces, N is 10. And the resulting data, of course, has 50
observations.

Finally, the province-level variables are created using the
[`draw_binary()`](https://declaredesign.org/r/fabricatr/reference/draw_discrete.md)
function. This is a function provided by **fabricatr** to make
simulating discrete random variables simple. When you simulate your own
data, you can use **fabricatr**’s functions, R’s built-ins, or any
custom functions you wish.
[`draw_binary()`](https://declaredesign.org/r/fabricatr/reference/draw_discrete.md)
is explained in [our tutorial on variable generation using
**fabricatr**](https://declaredesign.org/r/fabricatr/articles/variable_generation.md)

## Adding hierarchy to existing data

**fabricatr** is also able to import existing data and nest hierarchical
data under it. This maybe be useful if, for example, you have existing
country-level data but wish to simulate data at lower geographical
levels for the purposes of an experiment you plan to conduct.

Imagine importing the country-province data simulated in the previous
example. Because
[`fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
returns a data frame, this simulated data can be re-imported into a
subsequent fabricate call, just like external data can be.

``` r
citizen_data <-
  fabricate(
    data = country_data,
    citizens = add_level(
      N = 10,
      salary = rnorm(
        N,
        mean = gdp_per_capita + natural_resources * 5000 + manufacturing * 5000,
        sd = 10000
      )
    )
  )
head(citizen_data)
```

| countries | gdp_per_capita | life_expectancy | provinces | natural_resources | manufacturing | citizens | salary |
|:----------|---------------:|----------------:|:----------|------------------:|--------------:|:---------|-------:|
| 1         |         40,451 |              73 | 01        |                 1 |             1 | 001      | 45,852 |
| 1         |         40,451 |              73 | 01        |                 1 |             1 | 002      | 56,768 |
| 1         |         40,451 |              73 | 01        |                 1 |             1 | 003      | 52,549 |
| 1         |         40,451 |              73 | 01        |                 1 |             1 | 004      | 46,428 |
| 1         |         40,451 |              73 | 01        |                 1 |             1 | 005      | 70,148 |
| 1         |         40,451 |              73 | 01        |                 1 |             1 | 006      | 64,771 |

In this example, we add a third level of data; for each of our 50
country-province observations, we now have 10 citizen-level
observations. Citizen-level covariates like salary can draw from both
the country-level covariate and the province-level covariate.

Notice that the syntax for adding a new nested level to existing data is
different than the syntax for adding new variables to the original
dataset.

## Modifying existing levels

Suppose you have hierarchical data, and wish to simulate variables at a
higher level of aggregation. For example, imagine you import a dataset
containing citizens within countries, but you wish to simulate
additional country-level variables. In **fabricatr**, you can do this
using the
[`modify_level()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
command.

Let’s use our country-province data from earlier:

``` r
new_country_data <-
  fabricate(
    data = country_data,
    countries = modify_level(average_temperature = runif(N, 30, 80))
  )

head(new_country_data)
```

| countries | gdp_per_capita | life_expectancy | provinces | natural_resources | manufacturing | average_temperature |
|:----------|---------------:|----------------:|:----------|------------------:|--------------:|--------------------:|
| 1         |         40,451 |              73 | 01        |                 1 |             1 |                  38 |
| 1         |         40,451 |              73 | 02        |                 0 |             1 |                  52 |
| 1         |         40,451 |              73 | 03        |                 0 |             0 |                  69 |
| 1         |         40,451 |              73 | 04        |                 1 |             1 |                  64 |
| 1         |         40,451 |              73 | 05        |                 0 |             0 |                  33 |
| 1         |         40,451 |              73 | 06        |                 0 |             1 |                  65 |

We can observe that the new variable is created at the level of
aggregation you chose – countries. Also, although N is not specified
anywhere,
[`modify_level()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
knows how large N should be based on the number of countries it finds in
the dataset. It is important, then, to ensure that the
[`modify_level()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
command is correctly assigned to the level of interest. We can also
modify more than one level.

Here, we modify our country-province-citizen data from above:

``` r
new_citizen_data <-
  fabricate(
    data = citizen_data,
    countries = modify_level(average_temperature = runif(N, 30, 80)),
    provinces = modify_level(
      conflict_zone = draw_binary(N, prob = 0.2 + natural_resources * 0.3),
      infant_mortality = runif(N, 0, 10) + conflict_zone * 10 +
        (average_temperature > 70) * 10
    ),
    citizens = modify_level(
      college_degree = draw_binary(N, prob = 0.4 - (0.3 * conflict_zone))
    )
  )
```

Before assessing what this tells us about
[`modify_level()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md),
let’s consider what the data simulated does. It creates a new variable
at the country level, for a country level average temperature.
Subsequently, it creates a province level binary indicator for whether
the province is an active conflict site. Provinces that have natural
resources are more likely to be in conflict in this simulation, drawing
on conclusions from literature on “resource curses”. The infant
mortality rate for the province is able to depend both on province level
data we have just generated, and country-level data: it is higher in
high-temperature areas (reflecting literature on increased disease
burden near the equator) and also higher in conflict zones. Citizen
access to education is also random, but depends on whether they live in
a conflict area.

There are a lot of things to learn from this example. First, it’s
possible to modify multiple levels. Any new variable created will
automatically propagate to the lower level data according – by setting
an average temperature for a country, all provinces, and all citizens of
those provinces, have the value for the country. Values created from one
[`modify_level()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
call can be used in subsequent variables of the same call, or subsequent
calls.

Again, we see the use of
[`draw_binary()`](https://declaredesign.org/r/fabricatr/reference/draw_discrete.md).
Using this function is covered in our tutorial on [generating discrete
random
variables](https://declaredesign.org/r/fabricatr/articles/variable_generation.md),
linked below.

## Averages within higher levels of hierarchy

A powerful feature of nested data and **fabricatr**’s setup is that
variable creating can access variables from higher in

You may want to include the mean value of a variable within a group
defined by a higher level of the hierarchy, for example the average
income of citizens within city. You can do this with
[`ave()`](https://rdrr.io/r/stats/ave.html), a built-in `R` command:

``` r
ave_example <- fabricate(
  cities = add_level(N = 2),
  citizens = add_level(
    N = 1:2, income = rnorm(N),
    income_mean_city = ave(income, cities)
  )
)
ave_example
```

| cities | citizens | income | income_mean_city |
|:-------|:---------|-------:|-----------------:|
| 1      | 1        |  -1.39 |            -1.39 |
| 2      | 2        |  -0.16 |             0.73 |
| 2      | 3        |   1.62 |             0.73 |

Here, we can create citizen-level data which relies on the data of other
citizens within the same city.
[`ave()`](https://rdrr.io/r/stats/ave.html) takes two arguments: first,
the name of the variable we are averaging on (in this case, `income`),
and second, the name of the level we are grouping by (in this case
`cities`). Other `R` functions which are able to group by variables to
compute statistics of interest are also compatible with **fabricatr**.

## Next Steps

You’ve seen **fabricatr**’s ability to generate single-level and
hierarchical data, which is enough to get you started on using the
package. From here, you can explore more about modeling the structure of
data by reading our [tutorial on panel and cross-classified
data](https://declaredesign.org/r/fabricatr/articles/cross_classified.md)
or [using **fabricatr** to bootstrap or resample hierarchical
data](https://declaredesign.org/r/fabricatr/articles/resampling.md). Or,
if you would like to learn about modeling specific variables using
**fabricatr**, you can read our tutorial on [common social science
variables](https://declaredesign.org/r/fabricatr/articles/common_social.md);
our technical manual on [generating discrete random
variables](https://declaredesign.org/r/fabricatr/articles/variable_generation.md);
or our guide on [using other data generation
packages](https://declaredesign.org/r/fabricatr/articles/other_packages.md)
with **fabricatr**.
