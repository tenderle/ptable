<!-- README.md is generated from README.Rmd. Please edit that file -->

## R-package `ptable`

<!-- badges: start -->

[![R-CMD-check](https://github.com/tenderle/ptable/workflows/R-CMD-check/badge.svg)](https://github.com/tenderle/ptable/actions)
[![Codecov test
coverage](https://codecov.io/gh/tenderle/ptable/branch/master/graph/badge.svg)](https://codecov.io/gh/tenderle/ptable?branch=master)
[![GitHub last
commit](https://img.shields.io/github/last-commit/tenderle/ptable.svg?logo=github)](https://github.com/tenderle/ptable/commits/master)
[![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/tenderle/ptable.svg?logo=github)](https://github.com/tenderle/ptable)
<!-- badges: end -->

## Overview

The goal of the **ptable** package is to produce perturbation tables
that can be used for applying noise to statistical tables.

### Information

This package is developed within the SGA
`Open source tools for perturbative confidentiality methods`. The
package is not yet tested or optimized but already contains the
core-functionality to produce ptables for count data.

We have a first rough version with which interested users may play
around. Feedback (via issues) with regards to bugs or features requests
are very welcome as well as pull-requests.

With Release 0.4.0 we offer an additional modification of ptables for
count data.

### Installation

The package can directly be installed from `github` using the `devtools`
package which must be installed on your system

``` r
if (!require("devtools")) install.packages("devtools")
library(devtools)

# update all packages
update.packages(ask=FALSE)

# finally install the ptable package directly from github
devtools::install_github("sdcTools/ptable", dependencies=c("Depends","Imports"), force=TRUE, build_opts="--build-vignettes")
```

If you experience a timeout due to a proxy server while downloading, one
can work around this issue by specifying the proxy-server using the
`hhtr` package:

``` r
httr::set_config(use_proxy(url="xxx.xxx.xxx.xxx, port=yy))
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

#### Help for modification of ptables

``` r
## help for specifying the perturbation parameters
?modify_cnt_ptable
```

#### Documentation

Once finished, the package will also contain a package **vignette**. The
unfinished introduction vignette can be looked at using the following
command:

``` r
pt_vignette()
```

#### Graphical User Interface (GUI)

For first time users and visual learners there is a GUI. The unfinished
dashboard can be started using the following command:

``` r
ptable()
```

The download buttons (for downloading the ptable and the graphics) only
works within a browser.

### News

#### To-Dos

-   Merge the two main functions `pt_create_pParams(...)` and
    `pt_create_pTable(...)` into one function
-   Allow for special cases: extended parameter setting, i.e. improved
    row-wise parameter settings
-   Add error codes
-   Improved `fifi_...`-functions
-   Add test environment
-   Improve codecov (higher coverage)
-   Improve accuracy of ptable by means of digits-functionality

#### Version 0.4.0

-   feature: new function `modify_cnt_ptable()` to generate modified
    ptables (for count/frequency tables)
-   update: minor code improvement

#### Version 0.3.4.

-   update: generic error message when constraints are violated
-   update: replaced `lattice` by `ggplot2` in density plot function
    `pt_plot_pD()`
-   feature: Github actions (code coverage and CMD check)

#### Version 0.3.3.

-   feature: new functions `create_cnt_table()` (for count/frequency
    tables) and `create_num_table()` (for numerical/magnitude tables)
    that both wrap `pt_create_pParams()` and `pt_create_pTable()`
-   code cleanup and improved documentation
-   updated vignette
-   Thx to @bernhard-da for the commits.

#### Version 0.3.2.

-   feature: generic function `plot` instead of `fifi_function`
-   feature: functions `pt_ex_cnts()` and `pt_ex_nums()` for quick
    examples
-   updated vignette
-   updated dashboard

#### Version 0.3.1.

-   minor updates (e.g. updated argument `step`)

#### Version 0.3.0

-   feature: ptables for magnitude tables
-   new arguments in function `pt_create_pParams()`: `icat`, `table`,
    `step`, `type`
-   updated arguments in function `pt_create_pPtable()`: argument `type`
    removed

#### Version 0.2.0

-   Prototype Version for Testing (Census)

#### Version 0.1.14

-   feature: new argument `SDCtool` of function `pt_export()`: either
    “TauArgus” or “SAS”
-   feature: vignette and new function `pt_vignette()` to show it
-   updated default for argument `pstay` in function
    `pt_create_pParams()`
-   updated default for argument `type` in function `pt_create_pTable()`

#### Version 0.1.12/13

-   Prepared for prototype testing

#### Version 0.1.11

-   Some minor updates of graphic outputs (e.g. font sizes)
-   Alternative allocation for abs implementation (argument
    `type="abs2"`) (intended to replace actual argument `type="abs"` and
    will be removed afterwards)

#### Version 0.1.10

-   feature: graphic output of transition matrix
    `fifi_plot(..., type="t")` (first version)
-   feature: graphic output of perturbation panel
    `fifi_plot(..., type="p")` (first version)

#### Version 0.1.9

-   Some updates of the dashboard (e.g. now directly opens in web
    browser, download buttons)

#### Version 0.1.8

-   Updated live output of the main function
    `pt_create_pTable(..., monitoring=FALSE, debugging=FALSE)` (i.e.,
    monitoring is for users, debugging for own purposes and will be
    removed in a stable version)  
-   Some first minor fixes of extended parameter setting (i.e. argument
    `pstay`)

#### Version 0.1.7

-   compatibility with R-package
    [**cellKey**](https://github.com/sdcTools/cellKey) (type of ptable:
    ‘abs’/‘destatis’ formatted ptable)

#### Version 0.1.6

-   feature: new function `pt_export()` to export ptable (csv format
    used by [**Tauargus**](https://github.com/sdcTools/tauargus))
-   feature: new argument of function `fifi_plot()` in order to save
    graphical pdf output

#### Version 0.1.5

-   bug-fixing: dashboard

#### Version 0.1.1 - 0.1.4

-   bug-fixing: CRAN compatibility and minor fixes (e.g. depends)

#### Version 0.1.0

-   first uploaded version
