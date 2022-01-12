# counties-project

##Aim
Estimate the prevalence of sexual activity by age, sex and county in the U.S.

##Key scripts

**Data Processing scripts**
For surveys NSFG, ACS and YRBS (labelled 00a, 00b and 00c), these scripts take raw survey data (uploaded to Stanford med box) and process all variables to create final csv files (also available on Stanford med box) that are used for analysis.  Necessary adjustments will depend on whether we add or recode variables, or add years.

**01_data_description**
This is an exploratory script which will likely inform the first part of the results section, e.g. determining sample sizes, average characteristics. 

**Analysis scripts**
The scripts for national, county and state predictions (using NSFG, ACS and YRBS, respectively). 1. The national prediction script consists of the model to generate predictions (currently using logistic model). 
2. The county predictions script applies these predictions to the county level. 
3. The state predictions script (04_state_prediction.R) is not developed, but should be where we extrapolate trends and impute data for states without a current YRBS.

**05_state_raking**
This script 'adjusts' the county-level estimates to the state-level predictions. Currently this is only done for states with complete data. Raking could be redone in logit space.


