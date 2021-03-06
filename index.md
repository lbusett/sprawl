
-   [sprawl: Spatial Processing (in) R: Amorphous Wrapper Library](#sprawl-spatial-processing-in-r-advanced-workflows-library)
    -   [NOTE: WORK IN PROGRESS !](#note-work-in-progress)
    -   [Installation](#installation)
    -   [Function Documentation](#function-documentation)
    -   [Examples of use](#examples-of-use)
    -   [Contributing](#contributing)
    -   [Authors](#authors)
    -   [Citation](#citation)

<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/lbusett/sprawl.svg?branch=master)](https://travis-ci.org/lbusett/sprawl) [![codecov](https://codecov.io/gh/lbusett/sprawl/branch/master/graph/badge.svg?token=0yWdr6gWG7)](https://codecov.io/gh/lbusett/sprawl) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

sprawl: Spatial Processing (in) R: Amorphous Wrapper Library
=============================================================

Support for spatial processing tasks is provided in `R` by several great packages, spanning from all-purpose packages providing generalized access to the main spatial data classes and corresponding processing methods (e.g., [`sp`](https://cran.r-project.org/web/packages/sp/index.html) and [`sf`](https://cran.r-project.org/web/packages/sf/index.html), [`raster`](https://cran.r-project.org/web/packages/raster/index.html) and [`rgdal`](https://cran.r-project.org/web/packages/rgdal/index.html) - providing functions for handling raster and vector spatial data -), to more "specialized" ones meant to allow conducting more specific processing tasks (e.g., [`geosphere`](https://cran.r-project.org/web/packages/geosphere/index.html), [`raster`](https://cran.r-project.org/web/packages/raster/index.html), [`landsat`](https://cran.r-project.org/web/packages/landsat/index.html), [`MODIStsp`](https://cran.r-project.org/web/packages/MODIStsp/index.html)), or to provide optimized/improved/easier solutions for methods already provided by the aforementioned general-purpose packages (e.g., [`velox`](https://cran.r-project.org/web/packages/velox/index.html), [`mapview`](https://cran.r-project.org/web/packages/mapview/index.html)) (Curated lists of some very good packages can be found [here](https://cran.r-project.org/web/views/Spatial.html), [here](https://ropensci.org/blog/blog/2016/11/22/geospatial-suite) and [here](https://github.com/ropensci/maptools)).

This huge variability provides advanced `R` programmers with great flexibility for conducting simple or complex spatial data analyses, by combining functionalities of different packages within custom scripts. At the same time, it may be confusing for less skilled programmers, who may struggle with dealing with multiple packages and don't know that a certain processing task may be more efficiently conducted by using less-known (and sometimes difficult to find) packages.

**`sprawl`** aims to simplify the execution of some spatial processing tasks by **providing a single and (hopefully) simpler access point to functionalities spread in the `R` packages ecosystem, introducing optimixed functions for executing frequently used processing tasks, and providing custom functions or workflows for the execution of more complex processing tasks** (See [here](https://lbusett.github.io/sprawl/articles/sprawl.html) for further details).

NOTE: WORK IN PROGRESS !
------------------------

If you stumble on this page, please be advised that `sprawl` is currently in an early stage of development. This means that things are going to change rapidly, and we do not guarantee that everything works as expected. Additionally, be warned that until a first official version is released, functions may change behaviour or even completely disappear while we test different approaches and solutions ! (besides this: we would be happy to have beta-testers !)

Installation
------------

You can install sprawl from git-hub with:

``` r
install.packages("devtools")
devtools::install_github("lbusett/sprawl")
```

<font size="2"> *Note that, given its rather broad scope, `sprawl` **imports functions from several other packages**. Since some of those have quite specific System Requirements, it's possible that you'll struggle a bit in installation due to unresolved dependencies. In that case, please have a look at the installation error messages to see which package has problems in installation and try to install it beforehand (consulting its specific CRAN pages or doing a stackoverflow search may help !).* </font>

Function Documentation
----------------------

The main functions available in `sprawl` are documented [here](https://lbusett.github.io/sprawl/reference/index.html), along with simple usage examples.

Examples of use
---------------

Worked-out examples of the more comlex `sprawl` functions and workflows can be found [here](https://lbusett.github.io/sprawl/docs/articles/examples.html)

Contributing
------------

We are open for contributions/help in improving `sprawl` and extend its functionalities. If you have a **suggestion for and additional functionality** please report it in our [issues](https://github.com/lbusett/sprawl/issues) page. If you **wish to contribute to `sprawl` development**, please see our \[Contributing page\] on github.

Authors
-------

`sprawl` is currently developed and maintained by Lorenzo Busetto and Luigi Ranghetti of CNR-IREA (Institute on Remote Sensing of Environment - National Research Council, Italy - <http://www.irea.cnr.it/en/>)

Citation
--------

To cite package `sprawl` in publications use:

Lorenzo Busetto and Luigi Ranghetti (2017). sprawl: Spatial Processing (in) R: Amorphous Wrapper Library. R package version 0.1.0.9000.

Please note that this project is released with a [Contributor Code of Conduct](https://lbusett.github.io/sprawl/articles/conduct.html). By participating in this project you agree to abide by its terms.
