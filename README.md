# risyphus
Tools/utility functions for repetitive and laborious tasks for studies with typically two groups.


Current Goals
-----
* Add flexibility to current functions.
* Robustify current functions.
* Expand for similar standard cases (ORs, HRs)


Recent additions and changes
-----
* ONEtable: Function to compile table for one group.
* ORtable: Function to compile table of odds ratios based on logistic regression.
* HRtable: Function to compile table of hazard ratios based on Cox proportional 
hazards model.



TO DO
-----
* BLtable: Make BLtable work with tibbles.
* BLtable: Refine handling of p-values (including no p-values)
* BLtable: Option for only one group.
* BLtable, ORtable, HRtable: Option to add information about missing data for each variable.
* Add variables to example data set to demonstrate ORtable and HRtable
(currently example code does not work due to lack of these variables)
