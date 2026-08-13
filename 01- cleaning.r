### Aim: i) to rename variables to make them easier to use 
###     ii) to generate a sample to be used for further analysis by removing missing data or data that cannot be categorised into one of the two groups
###    iii) to count missing data and number of data points removed
###     vi) to create groups to be used in analysis


###############################################################################
############ DOWNLOAD, MERGE, RENAME AND CREATE FACTOR VARIABLES ##############
###############################################################################

## Download data from DNA Nexus in Python kernel ## 
%%bash
dx download "/sleep_bipolar/bipolar_chronotype_lsoa.csv"
dx download "/sleep_bipolar/bipolar_chronotype_clinic_measurements.csv"
dx download "/sleep_bipolar/bipolar_chronotype_questionnaire.csv"
dx download "/sleep_bipolar/bipolar_chronotype_participant.csv"
dx download "/sleep_bipolar/deprivation_index.csv"

## Switch to R kernel
# Load libraries
library(dplyr)

#Read data into R and merge together
participant <- read.table("bipolar_chronotype_participant.csv", sep=",", header=TRUE, quote = "\"")
questionnaire <- read.table("bipolar_chronotype_questionnaire.csv", sep=",", header=TRUE, quote = "\"")
clinic <- read.table("bipolar_chronotype_clinic_measurements.csv", sep=",", header=TRUE, quote = "\"")
LSOA <- read.table("bipolar_chronotype_lsoa.csv", sep=",", header=TRUE, quote = "\"")

part_ques <- full_join(participant, questionnaire, by = "pid")
clin_lsoa <- full_join(clinic, LSOA, by = "pid")
joined_data <- full_join(part_ques, clin_lsoa, by = "pid")

#Read in multiple deprivation score file
index <- read.table("deprivation_index.csv", sep=",", header=TRUE, quote = "\"")
#subset relevant columns from the index file and create "alldata"
index <- index[ , c(2, 6)] 
colnames(index) <- c("lsoa_at_reg", "multiple_deprivation_index")
alldata <- full_join(joined_data, index, by = "lsoa_at_reg")


## Make the column titles and data lower-case
alldata <- alldata %>%
  rename_with(tolower) %>%
  mutate(across(where(is.character), tolower))

# Label blanks as NA
alldata[alldata == ""] <- NA

## Rename columns
names(alldata)[names(alldata) == "diag_2_m"] <- "diag_sys"
names(alldata)[names(alldata) == "diag_cvd_1_m"] <- "diag_cvd"
names(alldata)[names(alldata) == "diag_endocr_1_m"] <- "diag_endo"
names(alldata)[names(alldata) == "smoke_tobacco_type_1_m"] <- "tobacco_even_once"
names(alldata)[names(alldata) == "smoke_reg_1_m"] <- "smoke_reg"
names(alldata)[names(alldata) == "alcohol_curr_1_1"] <- "alcohol_curr"
names(alldata)[names(alldata) == "activity_type_1_m"] <-"activity_type"
names(alldata)[names(alldata) == "demog_height_1_1"] <- "height_sr"
names(alldata)[names(alldata) == "demog_weight_1_1"] <- "weight_sr"
names(alldata)[names(alldata) == "demog_sex_2_1"] <- "sex"
names(alldata)[names(alldata) == "demog_ethnicity_1_1"] <- "ethnicity"
names(alldata)[names(alldata) == "consent_year"] <- "consent_year"
names(alldata)[names(alldata) == "birth_year"] <- "birth_year"
names(alldata)[names(alldata) == "edu_qual_1_m"] <- "edu_qual"
names(alldata)[names(alldata) == "housing_income_1_1"] <- "housing_income"
names(alldata)[names(alldata) == "work_status_2_m"] <-"work_status"
names(alldata)[names(alldata) == "diag_psych_1_m"] <- "diag_psych"
names(alldata)[names(alldata) == "medicat_1_m"] <- "meds_any"
names(alldata)[names(alldata) == "medicat_psych_1_m"] <- "meds_psych"
names(alldata)[names(alldata) == "sleep_chronotype_1_1"] <- "sleep_chronotype"
names(alldata)[names(alldata) == "work_nights_1_1"] <- "night_shifts"
names(alldata)[names(alldata) == "work_shifts_1_1"] <- "shift_work"
names(alldata)[names(alldata) == "lsoa_at_reg"] <- "lsoa"
names(alldata)[names(alldata) == "weight"] <- "weight_clinic"
names(alldata)[names(alldata) == "height"] <- "height_clinic"

## Convert to factors
# Convert columns to vectors
factorcols <- c(
  "diag_endo", "diag_psych", "diag_sys", "meds_any",
  "meds_psych", "sleep_chronotype", "sex", "ethnicity",
  "smoke_reg", "tobacco_even_once", "alcohol_curr", "housing_income", 
  "activity_type", "edu_qual","work_status", "shift_work", "night_shifts",
  "lsoa"
)
alldata[factorcols] <- lapply(alldata[factorcols], as.factor)
rm(factorcols)


###############################################################################
############# CREATE MUTUALLY EXCLUSIVE GROUPS: BIPOLAR AND NO_MHP ############
###############################################################################

###Create a variable from which groups can be created, with missing data removed
alldata <- alldata %>%
  mutate(mhp = case_when(
    grepl("bipolar", diag_psych) ~ "Bipolar",
    grepl("depression|anxiety|schizophrenia|schizoaffective disorder|post traumatic stress disorder|
        eating disorder|obsessive compulsive disorder|personality disorder|other|psychosis|body dysmorphia|
        premenstrual dysphoric disorder", diag_psych, ignore.case = TRUE) &
      !grepl("bipolar", diag_psych, ignore.case = TRUE) ~ "MHP not bipolar",
    grepl("do not know|prefer not to answer", diag_sys) ~ NA,
    is.na(diag_sys) ~ NA,
    diag_sys == "mental health problems" &
      (diag_psych %in% c("do not know","prefer not to answer","none of the above") | is.na(diag_psych)) ~ NA,
    TRUE ~ "No MHP"
  ))

### BIPOLAR DISORDER
# Create new column for Bipolar
alldata <- alldata %>%
  mutate(bipolar = case_when(mhp == "Bipolar" ~ TRUE,
                             TRUE ~ FALSE))

### NO MENTAL HEALTH PROBLEMS
## Aim: To create a group with no self-reported psychiatric diagnosis and who are not taking any medications for mental illness
# Create a new column for No MHP
alldata <- alldata %>%
  mutate(no_mhp = case_when(mhp == "No MHP" ~ TRUE,
                            TRUE ~ FALSE))
# Create new columns for psychiatric medications from meds_psych
alldata <- alldata %>% mutate(meds_psych_new = case_when(
   grepl("antidepressant|antipsychotic|lithium|valproic acid|other mood stabilising medication|pregabalin|
        benzodiazepine|blocker|other", meds_psych) ~ TRUE, #sleeping pills to be included in both groups
  grepl("do not know|prefer not to answer", meds_psych) ~ NA,
  is.na(meds_psych) ~ FALSE,
  grepl("none of the above", meds_psych) ~ FALSE,
  TRUE ~ FALSE
))


###############################################################################
########## REMOVE CASES THAT WILL NOT BE USED FOR FURTHER ANALYSIS ############
###############################################################################

## Count total N
all_n <- nrow(alldata)

## Count number of cases who did not answer the diagnosis question that generates groups
n_missing_diag <- sum(is.na(alldata$mhp) | alldata$mhp == "MHP not bipolar") 
                      
## Drop cases who did not answer the question that diagnosis generates groups and count N
alldata <- alldata %>%
  filter(!is.na(mhp)) %>%
  filter(!mhp == "MHP not bipolar")
diag_rm_n <- nrow(alldata)
# Count data by group
diag_rm_n_by_group <- table(alldata$mhp, useNA = "ifany")

## Count the number of data points to be removed due to night shifts or missing
n_night_shifts_or_no_answer <- sum(alldata$night_shifts %in% c("always","sometimes","usually","do not know",
                                                                                              "prefer not to answer"))
##Remove night shifts
alldata <- alldata %>%
  filter(is.na(night_shifts) | # NAs are kept in as only those who answered yes to previous questions are shown this question
    !night_shifts %in% c("always","sometimes","usually","do not know",
                         "prefer not to answer"))
diag_shift_rm_n <- nrow(alldata)
# Count data by group
diag_shift_rm_n_by_group <- table(alldata$mhp, useNA = "ifany")

## Count number missing from chronotype and drop cases with missing chronotype
missing_chronotype <- alldata %>%
  filter(sleep_chronotype %in% 
           c("do not know","prefer not to answer") |
           is.na(sleep_chronotype))
n_missing_chronotype <- nrow(missing_chronotype)
# Count data by group
missing_chronotype_by_group <- table(missing_chronotype$mhp, useNA = "ifany")

## Remove participants with missing chronotype
alldata <- alldata %>%
  filter(!sleep_chronotype %in%  c("do not know","prefer not to answer") &
           !is.na(sleep_chronotype))
diag_shift_chronotype_rm_n <- nrow(alldata)
# Count data by group
diag_shift_chronotype_rm_n_by_group <- table(alldata$mhp, useNA = "ifany")

## Create a group that identifies ppts who claimed not to have mental illness but who were prescribed regular psychotropics for their mental health
alldata$psych_meds_con <- ifelse (alldata$meds_psych_new == TRUE &
                                    alldata$no_mhp == TRUE,
                                  TRUE, FALSE)
## Count number of participants who claim not to have a mental illness but are prescribed regular psychotropics
test <- alldata %>% filter(psych_meds_con == TRUE)
no_mhp_psych_meds_n <- nrow(test)
# Remove ppts who claim not to have a mental illness but are taking psychotropic medications
alldata <- alldata[!alldata$psych_meds_con %in% TRUE, ]
# Count total with these participant removed
no_mhp_psych_meds_rm_n <- nrow(alldata)

# Count data by group
analysis_total_by_group <- table(alldata$mhp, useNA = "ifany") 


###############################################################################
################## CREATE OUTCOME VARIABLE: TYPE 2 DIABETES ###################
###############################################################################

# Create new column TRUE if diag_endo contains the string "Type 2 diabetes"
alldata <- alldata %>% 
  mutate(t2dm = case_when(
    grepl("type 2 diabetes", diag_endo) ~ TRUE,
    diag_sys == "do not know" ~ NA,
    diag_sys == "prefer not to answer" ~ NA,
    is.na(diag_sys) ~ NA,
    diag_sys == "endocrine" &
      (diag_endo %in% c("do not know","prefer not to answer") | is.na(diag_endo)) ~ NA,
    TRUE ~ FALSE))


###############################################################################
############# CREATE PREDICTOR VARIABLE: DIFFERENT CHRONOTYPES  ###############
###############################################################################

##Reassign sleep_chronotype
alldata <- alldata %>% mutate(chronotype = case_when(
  sleep_chronotype == "definitely an 'evening' person" ~ "definite evening",
  sleep_chronotype == "more an 'evening' than a 'morning' person" ~ "more evening than morning",
  sleep_chronotype == "more a 'morning' than 'evening' person" ~ "more morning than evening",
  sleep_chronotype == "definitely a 'morning' person" ~ "definite morning",
  TRUE ~ NA))


###############################################################################
########### CREATE AND STORE TABLES AND SAVE CLEAN CSV FOR ALLDATA ############
###############################################################################
sample <- rbind(all_n, n_missing_diag, diag_rm_n, n_night_shifts_or_no_answer, diag_shift_rm_n, n_missing_chronotype,
                diag_shift_chronotype_rm_n, no_mhp_psych_meds_n, no_mhp_psych_meds_rm_n)
groups_table <- rbind(diag_rm_n_by_group, diag_shift_rm_n_by_group, missing_chronotype_by_group, 
               diag_shift_chronotype_rm_n_by_group, analysis_total_by_group)

write.csv(sample, "sample_n_missing_removed.csv")
write.csv(groups_table, "groups_n_missing_removed.csv")
write.csv(alldata, "alldata_clean_00.csv")
write.csv(analysis_total_by_group, "analysis_total_by_group.csv")

## Switch to Python kernel
%%bash
dx upload "sample_n_missing_removed.csv"
dx upload "groups_n_missing_removed.csv"
dx upload "alldata_clean_00.csv"
dx upload "analysis_total_by_group.csv"
