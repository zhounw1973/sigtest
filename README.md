# sigtest

## Installation
```R
install.packages("remotes")
remotes::install_github("zhounw1973/sigtest")

install.packages("devtools")
devtools::install_github("zhounw1973/sigtest")
```

## Description
we investigate a generic hypothesis testing problem that checks whether part of the covariates  does not contribute to the structural function,includes classic regression functions and the conditional average treatment effects as examples.  
For Study 1 under Scenario 1 and Scenario 2 in Section 5, you need
to run "test_cate_study1.R" by appropriately choosing "arg". when arg=="s1" , it corresponds to Scenario 1
while arg=="s2" implies Scenario 2. For Study 2 in Section 5, you need to run "test_miss_study2.R". 
You need to run "app_ACTG_test.R" for the real-data analysis in Section 6.
You need to run "app_twa_test.R" for the real-data analysis in Supplementary Materials.
