<!-- README.md is generated from README.Rmd. Please edit that file -->
R-package `ptable`
------------------

Overview
--------

The goal of the **ptable** package is to produce perturbation tables that can be used for applying noise to statistical tables.

### Information

This package is developed within the SGA `Open source tools for perturbative confidentiality methods`. The package is not yet tested or optimized but already contains the core-functionality to produce ptables for count data.

We have a first rough version with which interested users may play around. Feedback (via issues) with regards to bugs or features requests are very welcome as well as pull-requests.

### Installation

The package can directly be installed from `github`

``` r
devtools::install_github("sdcTools/ptable", build_vignette=FALSE)
```

### Usage

#### Load the package

To load the package `ptable` you have to call

``` r
## load the package
library(ptable)
```

#### Help

``` r
## help for specifying the perturbation parameters
?pt_create_pParams

## help for producing the perturbation table
?pt_create_pTable
```

#### Basic Example

This is a basic example which shows you how to solve a common problem with `D=3` (maximum deviation) and `V=0.7` (perturbation variance):

``` r
## basic example code
params <- pt_create_pParams(D=3, V=0.7)
ptable_destatis <- pt_create_pTable(params = params, type="destatis")
ptable_abs <- pt_create_pTable(params = params, type="abs")
```

#### Extended Example (prevent small frequencies)

To prevent small frequencies (e.g. 1s and 2s) in perurbed frequency tables, you can set the treshold parameter `js=...` when specifying the perturbation table (i.e. all positive integers less equal `js` are no target frequencies for the perturbation and, hence, blocked):

``` r
## extended example code
params <- pt_create_pParams(D=5, V=3, js=2)
ptable_destatis <- pt_create_pTable(params = params, type="destatis")
```

#### Extended Example (prevent small frequencies and pre-setting of probabilities)

Additionally, you can pre-set the probability that original frequencies won't be perturbed using the parameter `pstay=...`. In the following example the probability that values shouldn't be perturbed is pre-set to 50%:

``` r
## extended example code
params <- pt_create_pParams(D=5, V=3, js=2, pstay=0.5)
ptable_destatis <- pt_create_pTable(params = params, type="destatis")
```

However, a pre-set probability does neither hold for blocked frequencies (small frequencies such as 1s and 2s) nor for original zeroes. The probabilites for blocked frequencies are always `0` (i.e. they must be perturbed) and for original zeroes the probability is always `1` (i.e. zeroes won't be perturbed).

#### Plot the perturbation table and save the plot

``` r
## plot the ptable
fifi_plot(ptable_destatis)
## ... and save the plot as pdf
fifi_plot(ptable_destatis, file="graph.pdf")
```

#### Export the perturbation table (e.g. for importing the file in tauargus)

``` r
pt_export(ptable_destatis,file="Test")
```

### Dashboard

To use the dashboard call

``` r
## start the dashbaord
ptable()
```
