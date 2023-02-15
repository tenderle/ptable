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
frequency count data in order to receive an higher level of protection

### Installation

#### Online

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

#### Offline

Alternatively you can download an archive (tar.gz) of the newest release
[here](https://github.com/sdcTools/ptable/releases) and install it
manually using

``` r
install.packages('C:/Users/.../Downloads/v0.4.0.tar.gz', repos=NULL, type='source')
```

### Usage

#### Load the package

To load the package `ptable` you have to call

``` r
## load the package
library(ptable)
```

#### Help & Documentation

``` r
## help file
?ptable::ptable_pkg
```

A detailed documentation is given in the **vignette**:

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

### Updates

The changelog is given [here](NEWS.md).

### To-Dos

- Allow for special cases: extended parameter setting, i.e. improved
  row-wise parameter settings
- Add more sophisticated error codes
- Update test environment
- Improve accuracy of ptable by means of digits-functionality
