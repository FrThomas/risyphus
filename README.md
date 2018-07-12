# risyphus
Tools/utility functions for repetitive and laborious tasks for studies with typically two groups.


Current Goals
-----
* Add flexibility to current functions.
* Robustify current functions.
* Add other recurring tasks.


Recent additions and changes
-----
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
