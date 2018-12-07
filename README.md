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
too often consumed too much time and energy devoted to a project. My
intention with this package is to do standard tasks well and to free time
and resources for better statistical analyses and to facilitate
reproducible computing.


Download/Installation
----
This version of [risyphus on GitHub](https://github.com/FrThomas/risyphus)
can be installed from R after installing the `devtools` package.
```r
install.packages("devtools") # Will install from CRAN.
devtools::install_github(repo="FrThomas/risyphus") # Will install from GitHub.
```

Current Goals
-----
* Add flexibility to current functions.
* Robustify current functions.
* Add other recurring tasks.


Recent additions and changes
-----
* ORtableGLMER: Function to compile table of odds ratios based on
generalized logistic regression with a provided ID for clustering.
* ContTable: Function to compile contingency tables.
* ONEtable: Function to compile table for one group (with or without information about missing entries).
* ORtable: Function to compile table of odds ratios based on logistic regression.
* HRtable: Function to compile table of hazard ratios based on Cox proportional 
hazards model.


TO DO
-----
* BLtable, ORtable, HRtable: Make BLtable work with tibbles.
* BLtable: Refine handling of p-values (including no p-values)
* BLtable, ORtable, HRtable: Option to add information about missing data for each variable.
* Add variables to example data set to demonstrate ORtable and HRtable
(currently example code does not work due to lack of these variables)


History
-----
* 2018-04-06: risyphus made publicly available on [GitHub](https://github.com/FrThomas/risyphus).
* 2017-02-12: Work on risyphus begins in a private GitHub repository.

