###########################################################################
#### README FILE						       ####
#### REPLICATION OF HOUSEHOLD EDUCATION GAPS AND GENDER ROLE ATTITUDES ####
#### FEBRUARY 1, 2021						       ####
###########################################################################

These files replicates: Giani, Marco, David Hope and Øyvind Skorge, 
"Household Education Gaps and Gender Role Attitudes". Political Science
Research and Methods.

Files included: 
	- utils.r
	- ESSround9.dta
	- 01_data.do
	- 02_data.R
	- 03_analyses.R
	- 04_graphs.R

Please put all files in the same folder and make sure 
to change the working directory in the scripts 01_data.do, 
02_data.R, 03_analyses.R, and 04_graphs.R.

To replicate the results reported in the paper and the appendix, please run
01_data.do, 02_data.R, 03_analyses.R, and 04_graphs.R in the specified order.

Before running 03_analyses.R make sure to install the "interflex" package 
for R (instructions provided in the script).

Due to cross-validation and bootstrap estimations, the analyses in 
03_analyses.R may require some time to run. Depending on the 
number of cores on your computer, you may want to change the cores setting
in the interflex functions, for instance by changing "v.cores <- 1" to 
"v.cores <- 4" and "v.parallel <- TRUE".


