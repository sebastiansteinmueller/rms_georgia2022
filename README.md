# RMS Georgia 2022
R-code to process and analyse data from the RMS Georgia 2022

## Workflow 
### rms_process_georgia2022.R
1. rms_process_georgia2022.R loads data from KoBo with robotoolbox package (see https://github.com/dickoa/robotoolbox for setup). 
Change variable koboid to your formid for other RMS datasets.
2. Data checks, merge between HH and individual data, create demographic variables and RBM indicator variables, e.g. WG disability indicators. See commented code for details. Merge created variables between HH data (object hh) and individual HH roster data (object s1)
3. Remove some individual identifiers (but the resulting datafile is not anonymised). 
4. Write hh and s1 objects to file rms_clean_georgia2022.RData in subfolder data. These are clean processed datafiles for further analysis. 

### rms_tables_georgia2022.R
1. Load data/rms_clean_georgia2022.RData from previous step 
2. Create srvyr objects to define sampling designs hh.design and ind.design with weights, clusters and stratification as required
3. With function rmstable in file rmsfunctions.R (sourced above) create tables with disaggregation variables gender, age (UNHCR age categories) and disability (WG disability identifier 3 from short question set). 
4. Combine tables and write to .xlsx file in output folder for further formatting. The tables are read column-wise (i.e. column % in one disaggregation variable value sum to 100% - e.g. for indicator core outcome 1.3, values across two indicator outcomes "Does not have documents or credentials" plus "Has documents or credentials" in column "Female" sum to 100%). low_ and upp_ columns for respective 95% lower and upper confidence interval bounds. 


### rmsfunctions.R
rmstable function to create table of indicator and other variables with headers gender, age, disability on a srvyr object. The tables are currently not flexible and the disaggregation variables need to be in the exact format as provided in the function: gender/sex variable with labels "Female", "Male"; age brackets with category labels "0-4", "5-11", "12-17", "18-24", "25-49", "50-59", "60+"; disability identifier variable with labels "Without disability", "With disability".

## Merging in rms_process_georgia2022.R
..* Merge s1a and s1b (both datasets contain HH roster information, one row per HH member) to a single HH member roster dataset s1. Merging variable: indid2 (see construction in code). This merging variable is removed in file rms_clean_georgia2022.RData since it contains names. 
..* Dataset hh contains HH level information, but also (with one adult person randomly selected per HH) individual level information including on many of the RBM indicators. Merge the HH roster information from file s1 for these selected individuals to hh with variable indid. 
..* Merge indicator values collected at HH level (e.g. clean cooking fuel) that are calculated on individual level (i.e. to all members of a HH) from hh to s1 with merging variables _parent_index (s1) and _index (hh). 
..* Merge some information on head of household (age, gender) and HH characteristics (disability identifier at HH level, HH size) from s1 to hh with merging variables _parent_index (s1) and _index (hh). 
