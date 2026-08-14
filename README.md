# OFH-ChronotypeDiabetesBipolar-August2026

Analysis code for a study assessing the interaction effects of chronotype on the association between type 2 diabetes and bipolar disorder. 

PROJECT SUMMARY

The goal of this project was to explore whether chronotype modifies the association between bipolar disorder and type 2 diabetes and whether any effect differs between the sexes. To realise this, cases with bipolar disorder were compared to cases with no mental health problems in logistic regression models both with and without an interaction term (chronotype x mental health status) to generate odds ratios of type 2 diabetes. The regression analysis was adjusted for a range of relevant sociodemographic, lifestyle and health-related covariates.

ASSOCIATED PUBLICATION

Submitted.

CONTACT

For any questions, please contact the lead investigator, Dr Duncan Swiffen (dswiffen@ed.ac.uk).

DATA DICTIONARY

A full description of variables used in the analysis can be found in the Our Future Health data dictionary, at this link: https://research.ourfuturehealth.org.uk/data-and-cohort/

GENERAL APPROACH

Two mutually-exclusive groups - bipolar disorder and no mental health problems are created and baseline sociodemographic, lifestyle and health-related factors are compared between groups. Odds ratios are calculated via logistic regression models adjusted for relevant covariates. The first model includes both mental health status group and chronotype, whilst the second model includes a term for their interaction. These models were repeated on the whole sample and in the sample stratified by sex. Likelihood ratios comparing the two types of models were calculated. 

DESCRIPTION OF SCRIPTS

01- cleaning.r 
A data cleaning file.
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
A comparison of complete versus incomplete cases. 
The sample is stratified according to cases with a full complement of data in each included covariate and cases that have data missing in at least one covariate. 
Covariates for complete and incomplete cases are combined into a baseline characteristics table, separated by group. 

