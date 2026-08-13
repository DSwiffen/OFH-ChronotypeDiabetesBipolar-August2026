# OurFutureHealth_ChronotypeAndDiabetesInBipolar_August2026

Does chronotype influence the odds of diabetes comorbidity in bipolar disorder?
This repository contains code used the analyse the interaction effects of chronotype on the association between type 2 diabetes and bipolar disorder. 
The analysis used data from the Our Future Health cohort (Data release 14), accessed August 2026.

Details of the code files are below:

01- cleaning.r 
Data is downloaded from DNANexus into the OFH trusted research environment (TRE). Data files are merged. 
Variables are then renamed and the mental health groups "Bipolar" and "No MHP" are created.
Cases are removed from further analysis due to i) not being able to assign them to either of the two groups, ii) night shift working, iii) missing data on chronotype, and iv) being assigned as "No MHP" but self-reporting medications for mental health. At each stage, missing data and total sample counts are created to be used in CONSORT diagram.
Outcome variable ("T2DM") created.
Predictor variable ("chronotype") created.

02- covariates.r
Covariates are created and missing or invalid responses for each covariate question are reassigned as NA.
Covariates include Age, Sex, Ethnicity, Smoking, Alcohol, Education, Activity, Medications, Index of Multiple Deprivation, BMI and Multimorbidity.
Covariates are combined into a baseline characteristics table, separated by sex in both groups. 

03- logreg.r
Logistic regression analysis is undertaken to generate odds ratios and 95% confidence intervals for main effects model excluding the interaction term and the interaction model including it. 
This is repeated for the whole sample and the sample stratified by sex. 
Likelihood ratios generate FDR-corrected p-values to determine whether the interaction model is a better fit than the main effects model. 

03b- logreg_additional_effects.r 
An optional code file that functions to sum the effect of bipolar on odds of type 2 diabetes with the additional effect of the chronotype interaction. 
This is written in long form using baseR to prevent the need to download a specific R package, as many of these are not currently available in the OFH TRE library.

04- complete_versus_incomplete_cases.r
The sample is stratified according to cases with a full complement of data in each included covariate and cases that have data missing in at least one covariate. 
Covariates for complete and incomplete cases are combined into a baseline characteristics table, separated by group. 

