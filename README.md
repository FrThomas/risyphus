# risyphus
Tools/utility functions to facilitate recurrent tasks with focus on
logistic regression, Cox regression, and studies/clinical trials with two groups.

This package contains functions that facilitate laborious steps
in most data analysis tasks, such as basic variable description,
compilation of baseline tables comparing two groups, odds-ratio tables
based on logistic regression, or hazard-ratio tables based on
Cox proportional hazards models. The name of this package is derived
from the Greek mythological figure Sisyphus (Sisyphus for R = risyphus),
who was doomed to roll a big boulder up a hill just to see it roll down
again ad infinitum. Somehow I related to the laborious task of keeping all
standard tables up to date in a developing project as Sisyphean, and it all
too often consumed too much time and energy devoted to a project.

My intention with this package is

1. to do standard tasks well,
1. to free time and resources for better statistical analyses, and
1. to facilitate reproducible computing.


Download/Installation
----
This version of [risyphus on GitHub](https://github.com/FrThomas/risyphus)
can be installed from R after installing the `remotes` package (has superseeded
this functionality previously provided by `devtools`).
```r
install.packages("remotes") # Will install from CRAN.

# Install and build vignettes (tested on macOS Mojave and Windows 10):
remotes::install_github("FrThomas/risyphus", build = TRUE, build_vignettes = TRUE,
                        build_opts = c("--no-resave-data", "--no-manual"),
                        force = TRUE)

# Note: Install without building vignettes (faster, but then only the help-function is available, not the vignettes):
remotes::install_github(repo="FrThomas/risyphus") # Will install from GitHub.

```

Available Help and Vignettes
-----
To have vignettes available, make sure you install risyphus in a way that
vignettes are actually build (see above).

See which vignettes are available (after install):
```r
vignette(package = "risyphus") # Gives list of available vignettes.
vignette("Introduction", package = "risyphus") # Shows the vignette "Introduction to risyphus".

```

If you use [RStudio](https://www.rstudio.com/products/RStudio/) (recommended!)
all vignettes and help files are available in the
"Files, Plots, Packages, Help, Viewer" pane, typically in the lower
right corner of your R Studio window: Click on Packages, scroll down/up and click
on risyphus to find your way around.

If you know the name of the function and would like to bring up the complete
description, the standard approach in R works, e.g., `help("BLtable")` will
bring up the help page for the function BLtable (make sure you have
executed `library("risyphus")` earlier so R has loaded the associated information).

The vignette *Introduction to risyphus* 
[`vignette("Introduction", package = "risyphus")`] contains a table with the
core functions available after installing risyphus.


Current Goals
-----
* Add flexibility to current functions.
* Robustify current functions.
* Add other recurring tasks.


Recent Additions and Changes
-----

* Vignette "Introduction to risyphus" added - requires that you install risyphus
in a way that vignettes are actually build (see above).


TO DO
-----
* BLtable, ORtable, HRtable: Make BLtable work with tibbles.
* BLtable: Refine handling of p-values (including no p-values)
* BLtable, ORtable, HRtable: Option to add information about missing data for each variable.
* Add variables to example data set to demonstrate ORtable and HRtable
(currently example code does not work due to lack of these variables)


History
-----
* 2019-09-20: Vignette *How to use risyphus: A simple example* added.
* 2019-08-30: Vignette *Introduction to risyphus* added.
* 2018-04-06: risyphus made publicly available on [GitHub](https://github.com/FrThomas/risyphus).
* 2017-02-12: Work on risyphus begins in a private GitHub repository.


*****

End of document

*****
