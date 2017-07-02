
-   [sprawl - Spatial Processing (in) R: Amorphous Wrapper Library](#sprawl---spatial-processing-in-r-amorphous-wrapper-library)
    -   [Intro and rationale](#intro-and-rationale)
    -   [Package philosopy](#package-philosopy)
    -   [Installation](#installation)
    -   [Currently available functions and examples](#currently-available-functions-and-examples)
    -   [Contributing](#contributing)
    -   [Citation](#citation)

<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/IREA-CNR-MI/sprawl.svg?branch=master)](https://travis-ci.org/IREA-CNR-MI/sprawl) 
[![codecov](https://codecov.io/gh/IREA-CNR-MI/sprawl/branch/master/graph/badge.svg?token=0yWdr6gWG7)](https://codecov.io/gh/IREA-CNR-MI/sprawl)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)



sprawl - Spatial Processing (in) R: Amorphous Wrapper Library
=============================================================

Intro and rationale
-------------------

Support for spatial processing tasks is provided in `R` by several great packages, spanning from all-purpose packages providing generalized access to the main spatial data classes and corresponding processing methods (e.g., [`sp`](https://cran.r-project.org/web/packages/sp/index.html) and [`sf`](https://cran.r-project.org/web/packages/sf/index.html), [`raster`](https://cran.r-project.org/web/packages/raster/index.html) and [`rgdal`](https://cran.r-project.org/web/packages/rgdal/index.html) - providing functions for handling raster and vector spatial data -), to more "specialized" ones meant to allow conducting more specific processing tasks (e.g., [`geosphere`](https://cran.r-project.org/web/packages/geosphere/index.html), [`raster`](https://cran.r-project.org/web/packages/raster/index.html), [`landsat`](https://cran.r-project.org/web/packages/landsat/index.html), [`MODIStsp`](https://cran.r-project.org/web/packages/MODIStsp/index.html)), or to provide optimized/improved/easier solutions for methods already provided by the aforementioned general-purpose packages (e.g., [`velox`](https://cran.r-project.org/web/packages/velox/index.html), [`mapview`](https://cran.r-project.org/web/packages/mapview/index.html)) (Curated lists of some great packages ones can be found [here](https://cran.r-project.org/web/views/Spatial.html), [here](https://ropensci.org/blog/blog/2016/11/22/geospatial-suite) and [here](https://github.com/ropensci/maptools)).

This huge variability provides advanced `R` programmers with great flexibility for conducting simple or complex spatial data analyses, combining functionalities of different packages within custom custom scripts. At the same time, it may prove confusing for less skilled programmers, who may struggle with dealing with multiple packages, and don't know that a certain processing task may be more efficiently conducted by using less-known and sometimes difficult to find packages.

In this context, **`sprawl` aims to simplify the execution of some "common" spatial processing tasks** (e.g., reading/writing spatial datasets, extracting statistics from large datasets, etc.) **by providing a single and (hopefully) simpler access point to functionalities spread in the `R` packages ecosystem.**

Package philosopy
-----------------

The aforementioned broad objectives are accomplished in `sprawl` by:

1.  Providing simple **wrappers to common-use functions** for reading/writing patial data (e.g., `readvec` and `writevec` to read/write vector files from/to disk);
2.  Providing **optimized functions for performing some common-use processing tasks**, often exploiting functions provided by less-known packages (e.g., `maskraster`);
3.  Providing custom functions for performing more complex processing tasks (e.g., `comp_zonal`);
4.  Optimizing code execution speed by providing under-the-hood support for multi-core processing using for exmaple `foreach` looping and `raster::MakeCluster` functionality
5.  Simplifying access/further analysis of results, by carefully formatting processing outputs to easy to understand and use `R` objects.

Given this, `sprawl` is expected to be a broadly-aimed (and therefore rather "amorphous") and rapidly-changing package, which functionalities are expected to 1. gradually grow to accomodate more functions, and 2. be sometimes removed/changed as soon as similar or better functionalities start to be provided by different/more focused packages.

`sprawl` is currently developed and mainteined by personnel of the Milano section of CNR-IREA (Institute on Remote Sensing of Environment - National Research Council, Italy - <http://www.irea.cnr.it/en/>)

Installation
------------

You can install sprawl from github with:

``` r
# install.packages("devtools")
devtools::install_github("IREA-CNR-MI/sprawl")
```

Note that, given its scope, `sprawl` imports functions from several other packages, which need to be installed in your system. Since some of those have quite specific System Requirements, it's possible that you'll struggle a bit in installation due to unresolved dependencies. In that case, please have a look at the installation error messages to see which package has problems in installation and try to install it beforehand (consulting its specific CRAN pages or doing a stackoverflow search may help !).

Currently available functions and examples
------------------------------------------

The main functions available in `sprawl` will be generally documented \[here - coming soon\], along with common-usage examples.

Contributing
------------

We are open for contributions/help in improving `sprawl` and extend its functionalities. If you have a suggestion, you can report it in our \[issues\] page, or fork the repository and then make a pull request. (note that we plan to provide som "good-practices" instructions for contributing new functions in the future.)

Citation
--------

To cite package `sprawl` in publications use:

Lorenzo Busetto and Luigi Ranghetti (2017). sprawl: Spatial Processing (in) R: Amhorphous Wrapper Library. R package version 0.1.0.9000.
