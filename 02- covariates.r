### Aim:    i) to create covariates from the dataset
###        ii) to count number of missing data points per covariate
###       iii) to generate a baseline characteristics table comparing bipolar and MHP per covariate

## Start in Python kernel
%%bash
dx download "/sleep_bipolar/alldata_clean_00.csv"
# Load libraries
library(dplyr)
library(stringr)
library(tidyr)
# Read CSV created from 01- cleaning.r
alldata <- read.csv("alldata_clean_00.csv")

###############################################################################
##### COUNT MISSING DATA, DERIVE COVARIATES AND ASSIGN NAs APPROPRIATELY ######
###############################################################################

#### AGE ####
#Convert consent_year and birth_year variables to R-friendly year format
alldata$consent_year <- format(as.Date(strptime(alldata$consent_year, format = "%Y")), "%Y")
alldata$consent_year <- as.numeric(alldata$consent_year)
alldata$birth_year <- format(as.Date(strptime(alldata$birth_year, format = "%Y")), "%Y")
alldata$birth_year <- as.numeric(alldata$birth_year)
#Calculate age at point of consent from consent year and birth year
alldata$age <- alldata$consent_year - alldata$birth_year
#Bring age to front of dataframe
alldata <- alldata[,c(1,ncol(alldata),2:(ncol(alldata)-1))] 

#### SEX ####
# Create new sex variable with missing assigned as NA
alldata <- alldata %>% mutate(sex_clean = case_when(
  is.na(sex) ~ NA,
  sex %in% c("do not know" , "prefer not to answer") ~ NA,
  sex == "female" ~ "Female",
  sex == "male" ~ "Male",
  TRUE ~ NA
))

#### ETHNICITY ####
# Create new ethnicity variable with missing assigned as NA
alldata <- alldata %>% mutate(ethnicity_clean = case_when(
  is.na(ethnicity) ~ NA,
  ethnicity %in% c("do not know" , "prefer not to answer") ~ NA,
  grepl("bangladeshi|indian|pakistani", ethnicity) ~ "South Asian",
  grepl("african|carribean|any other Black", ethnicity) ~ "Black",
  grepl("white and asian|white and black african|white and black carribean|any other mixed multiple ethnic background", ethnicity) ~ "Mixed or multiple heritage ethnic background",
  grepl("british|gypsy|irish|polish|any other white background", ethnicity) ~ "White",
  TRUE ~ "Other" # Includes categories "Chinese", "Arab", "Other", "Any other Asian background"
))

#### SMOKING ####
# Create new smoking variable with missing assigned as NA
alldata <- alldata %>% mutate(smoking_clean = case_when(
  is.na(tobacco_even_once) ~ NA,
  tobacco_even_once %in% c("do not know" , "prefer not to answer") ~ NA,
  !tobacco_even_once %in% c("do not know", "prefer not to answer") & 
    !is.na(tobacco_even_once) & 
    (smoke_reg %in% c("do not know", "prefer not to answer")) ~ NA,
  grepl("i have not used any of these tobacco products", tobacco_even_once) ~ "Never regularly smoked cigarettes, cigars or tobacco pipes",
  grepl("i have not used any of these tobacco products|vaped|chewing tobacco|shisha", smoke_reg) ~ "Never regularly smoked cigarettes, cigars or tobacco pipes",
  grepl("cigarettes|cigars|tobacco pipe", smoke_reg) ~ "History of regular cigarette, cigar or tobacco pipe smoking"
))

#### ALCOHOL ####
# Create new alcohol variable with missing assigned as NA
alldata <- alldata %>% mutate(alcohol_clean = case_when(
  is.na(alcohol_curr) ~ NA,
  alcohol_curr %in% c("do not know" , "prefer not to answer") ~ NA,
  grepl("daily or almost daily|three or four times a week", alcohol_curr)  ~ "Drinks alcohol more frequently than once or twice per week",
  grepl("one to three times a month|special occasions only|once or twice a week", alcohol_curr) ~ "Drinks alcohol once or twice a week or less frequently", 
  TRUE ~ "Never drinks alcohol"
))

#### EDUCATION ####
# Create new degree variable with missing education assigned as NA
alldata <- alldata %>% mutate(edu_clean = case_when(
  is.na(edu_qual) ~ NA,
  edu_qual %in% c("do not know" , "prefer not to answer") ~ NA,
  grepl("college or university degree", edu_qual)  ~ "Degree",
  TRUE ~ "No degree"
))

#### ACTIVITY ####
# Create new activity variable with missing assigned as NA
alldata <- alldata %>% mutate(activity_clean = case_when(
  is.na(activity_type) ~ NA,
  activity_type %in% c("do not know" , "prefer not to answer") ~ NA,
  grepl("light diy|walking for pleasure|heavy diy|other exercises", activity_type)  ~ "Low to medium level physical activity",
  grepl("strenuous sports", activity_type)  ~ "High level physical activity",
  TRUE ~ "No physical activity"
))

#### MEDICATIONS ####
# Create new medications variable with missing assigned as NA
alldata <- alldata %>% mutate(meds_clean = case_when(
  is.na(meds_any) ~ NA,
  meds_any %in% c("do not know" , "prefer not to answer") ~ NA,
  !meds_any %in% c("do not know", "prefer not to answer") & 
    !is.na(meds_any) & 
    (meds_psych %in% c("do not know", "prefer not to answer")) ~ NA,
  grepl("sleeping pills", meds_psych) ~ "Taking sleeping medications",
  TRUE ~ "Not taking sleeping medications" 
))

#### MULTIPLE DEPRIVATION INDEX ####
# Create new deprivation index variable with missing assigned as NA
names(alldata)[names(alldata) == "multiple_deprivation_index"] <- "mdi"

alldata <- alldata %>% mutate(mdi_quintile = case_when(
  is.na(mdi) ~ NA,
  mdi %in% c("1", "2") ~ "1",
  mdi %in% c("3", "4") ~ "2",
  mdi %in% c("5", "6") ~ "3",
  mdi %in% c("7", "8") ~ "4",
  mdi %in% c("9", "10") ~ "5",
  TRUE ~ NA
))

#### BMI ####
# Create new height and weight variables from clinic data, using self-report only if clinic data is unavailable
alldata$height <- ifelse(is.na(alldata$height_clinic),
                         alldata$height_sr, alldata$height_clinic)
alldata$weight <- ifelse(is.na(alldata$weight_clinic),
                         alldata$weight_sr, alldata$weight_clinic)
#Calculate BMI. NAs automatically generated if either height or weight is missing
alldata$height <- alldata$height / 100 # convert to meters
alldata$bmi <- alldata$weight/alldata$height^2

#### MULTIMORBIDITY ####
## Count missing multimorbidity data
alldata <- alldata %>%
  mutate(missing_diag_var = 
           diag_sys%in% c("do not know","prefer not to answer")|
           is.na(diag_sys) |
           ((!diag_sys%in% c("do not know","prefer not to answer") &
               !is.na(diag_sys)) &
              (diag_endo %in% c("do not know", "prefer not to answer"))))

sum1 <- sum(alldata$missing_diag_var)
missing_diag_n <- data.frame(sum1)

missing <- alldata %>% filter(missing_diag_var == TRUE)
tab1 <- table(missing$mhp)
missing_diag_by_group <- as.data.frame(tab1)

tab2 <-table(missing$mhp, missing$sex)
missing_diag_by_groupxsex <- as.data.frame(tab2)

missing_multimorbidity <- bind_rows(
  Overall = missing_diag_n,
  missing_diag_by_group,
  missing_diag_by_groupxsex) %>%
  mutate(
    Freq = if_else(Freq < 10, "redacted", as.character(Freq)),
    Freq = if_else(
    !is.na(Var2) & Var1 == "No MHP" & Var2 == "male",
    "redacted",
    as.character(Freq)))


### Aim: Create new multimorbidity variable with missing data assigned as NA
# Create T2DM variable with missing data assigned as NA
alldata <- alldata %>% mutate(t2dm_clean = case_when(
  is.na(diag_sys) ~ NA,
  diag_sys %in% c("do not know" , "prefer not to answer") ~ NA,
  !diag_sys %in% c("do not know", "prefer not to answer") & 
    !is.na(diag_sys) & 
    (diag_endo %in% c("do not know", "prefer not to answer")) ~ NA,
  grepl("type 2 diabetes", diag_endo) ~ "t2dm",
  TRUE ~ "No t2dm" 
))
# Create endocrine diagnosis variable with T2DM assigned as NA
alldata <- alldata %>% mutate(diag_endo_clean = case_when(
  is.na(diag_endo) ~ NA,
  diag_endo %in% c("do not know", "prefer not to answer") ~ NA, 
  t2dm_clean == "t2dm" ~ NA,
  TRUE ~ "endo_diag_not_t2dm"
))
# Create new variables with each diagnostic group assigned a "1" if present and "0" if absent
alldata <- alldata %>% mutate(
  diag_joint = as.integer(grepl("joint problems", diag_sys)),
  diag_visual = as.integer(grepl("visual problems",diag_sys)),
  diag_heart = as.integer(grepl("heart or circulatory disease",diag_sys)),
  diag_digestive = as.integer(grepl("digestive system or liver problems", diag_sys)),
  diag_lung = as.integer(grepl("lung or respiratory problems", diag_sys)),
  diag_cancer = as.integer(grepl("cancer", diag_sys)),
  diag_blood = as.integer(grepl("blood disorders", diag_sys)),
  diag_pregnancy = as.integer(grepl("complications or difficulties in pregnancy or childbirth", diag_sys)),
  diag_urinary = as.integer(grepl("urinary system disorders", diag_sys)),
  diag_reproductive = as.integer(grepl("reproductive system problems", diag_sys)),
  diag_autoimmune = as.integer(grepl("autoimmune disorder", diag_sys)),
  diag_neuro = as.integer(grepl("neurological disorders", diag_sys)),
  diag_neurodev = as.integer(grepl("neurodevelopmental conditions", diag_sys)),
  diag_endo = as.integer(grepl("endo_diag_not_t2dm", diag_endo_clean)))
# Create new variable that counts number of diagnoses per participant
alldata <- alldata %>% mutate(diag_num = rowSums(
  select(., diag_joint, diag_visual, diag_heart, diag_digestive,
         diag_lung, diag_cancer, diag_blood, diag_pregnancy,
         diag_urinary, diag_reproductive, diag_autoimmune,
         diag_neuro, diag_neurodev, diag_endo)))
# Retain missing data points
alldata <- alldata %>%
  mutate(diag_num = if_else(missing_diag_var == TRUE, NA, diag_num))
# Convert to a new variable with multimorbidity scores ("0", "1-2" or ">2")
alldata <- alldata %>% mutate(multimorbidity = case_when(
  is.na(diag_num) ~ NA,
  diag_num == 0 ~ "0",
  diag_num == 1 ~ "1-2",
  diag_num == 2 ~ "1-2",
  diag_num >= 3 ~ ">3",
  TRUE ~ NA
))


#### Combine all missing_n into a single table
### Remove columns not needed
alldata <- alldata %>%
  select(-c(consent_year, birth_year, ethnicity, sex, sleep_chronotype, meds_psych, mdi,
            smoke_reg, night_shifts, diag_endo, diag_psych, shift_work,diag_cvd, diag_sys, 
            height_sr, weight_sr, edu_qual, housing_income, work_status, activity_type, alcohol_curr, tobacco_even_once, smoke_reg,
            meds_any, height_clinic, weight_clinic, lsoa, meds_psych_new, psych_meds_con,  
            height, weight, diag_endo, t2dm_clean, diag_endo_clean, diag_joint, diag_visual, diag_heart, diag_digestive, diag_lung,
            diag_cancer, diag_blood, diag_pregnancy, diag_urinary, diag_reproductive, diag_autoimmune, diag_neuro, diag_neurodev, diag_num))
### rename "clean" columns
alldata <- alldata %>%
  rename_with(~ str_remove(.x, "_clean$"))

#### Count total sample and by group
alldata_n <- nrow(alldata)
alldata_by_group <- table(alldata$mhp)
alldata_by_groupxsex <- table(alldata$mhp, alldata$sex)
sample <- bind_rows(
  Overall = alldata_n,
  By_group = alldata_by_group,
  By_groupxsex = alldata_by_groupxsex,
  .id = "category"
)

#### Convert T2DM to character variable
alldata <- alldata %>% mutate (t2dm_t = case_when(
  t2dm == TRUE ~ "TRUE",
  t2dm == FALSE ~ "FALSE",
  TRUE ~ NA
))

#### Create baseline characteristics tables
## Continuous variables
# Both sexes
cont <- alldata %>%
  mutate(mhp = as.character(mhp)) %>%
  bind_rows(mutate(alldata, mhp = "Overall")) %>%
  group_by(mhp) %>%
  summarise(
    across(c(age, bmi),
           list(
             median = ~ median(.x, na.rm = TRUE),
             q1     = ~ quantile(.x, 0.25, na.rm = TRUE),
             q3     = ~ quantile(.x, 0.75, na.rm = TRUE),
             missing = ~ sum(is.na(.x)),
             n = ~ sum(!is.na(.x)),
             nmedian = ~ {
               med <- round(median(.x, na.rm = TRUE), 0)
               sum(round(.x, 0) == med, na.rm = TRUE)
             },
             nQ1 = ~ {
               q1 <- round(quantile(.x, 0.25, na.rm = TRUE), 0)
               sum(round(.x, 0) == q1, na.rm = TRUE)
             },
             nQ3 = ~ {
               q3 <- round(quantile(.x, 0.75, na.rm = TRUE), 0)
               sum(round(.x, 0) == q3, na.rm = TRUE)
             }
           ),
           .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    -mhp,
    names_to = c("variable", "stat"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(
    value = paste0(round(median, 0), " [", round(q1, 0), "–", round(q3, 0), "]", "; missing = ", missing),
    Nmedian = paste0(nmedian),
    NQ1 = paste0(nQ1),
    NQ3 = paste0(nQ3),
    stat = "Median [IQR]; missing"
  ) %>%
  mutate(sex = "Both") %>%
  select(mhp, variable, sex, stat, value, n, Nmedian, NQ1, NQ3)
# Categorical variables
cat_vars <- c("sex", "ethnicity", "smoking", "alcohol", "edu", "mdi_quintile", "activity", "meds", "multimorbidity", "t2dm_t", "chronotype")

cat <- alldata %>%
  mutate(mhp = as.character(mhp)) %>%
  bind_rows(mutate(alldata, mhp = "Overall")) %>%
  pivot_longer(cols = all_of(cat_vars),
               names_to = "variable",
               values_to = "level") %>%
  tidyr::replace_na(list(level = "Missing")) %>%
  group_by(mhp, variable, level) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(mhp, variable) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    variable = paste0(variable, ": ", level),
    value = n,
    percentage = paste0("(", round(100 * pct, 1), ")"),
    stat = "N(%)"
  ) %>%
  select(mhp, variable, stat, value, percentage) %>%
  mutate(sex = "Both")




###Repeat for each sex category
## Males
# Continuous variables
all_males <- alldata %>% 
  filter(sex == "Male")

cont_m <- all_males %>%
  mutate(mhp = as.character(mhp)) %>%
  bind_rows(mutate(all_males, mhp = "Overall")) %>%
  group_by(mhp) %>%
  summarise(
    across(c(age, bmi),
           list(
             median = ~ median(.x, na.rm = TRUE),
             q1     = ~ quantile(.x, 0.25, na.rm = TRUE),
             q3     = ~ quantile(.x, 0.75, na.rm = TRUE),
             missing = ~ sum(is.na(.x)),
             n = ~ sum(!is.na(.x)),
             nmedian = ~ {
               med <- round(median(.x, na.rm = TRUE), 0)
               sum(round(.x, 0) == med, na.rm = TRUE)
             },
             nQ1 = ~ {
               q1 <- round(quantile(.x, 0.25, na.rm = TRUE), 0)
               sum(round(.x, 0) == q1, na.rm = TRUE)
             },
             nQ3 = ~ {
               q3 <- round(quantile(.x, 0.75, na.rm = TRUE), 0)
               sum(round(.x, 0) == q3, na.rm = TRUE)
             }
           ),
           .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    -mhp,
    names_to = c("variable", "stat"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(
    value = paste0(round(median, 0), " [", round(q1, 0), "–", round(q3, 0), "]", "; missing = ", missing),
    Nmedian = paste0(nmedian),
    NQ1 = paste0(nQ1),
    NQ3 = paste0(nQ3),
    stat = "Median [IQR]; missing"
  ) %>%
  mutate(sex = "Male") %>%
  select(mhp, variable, sex, stat, value, n, Nmedian, NQ1, NQ3)
# Categorical variables
cat_vars <- c("ethnicity", "smoking", "alcohol", "edu", "mdi_quintile", "activity", "meds", "multimorbidity", "t2dm_t", "chronotype")

cat_m <- all_males %>%
  mutate(mhp = as.character(mhp)) %>%
  bind_rows(mutate(all_males, mhp = "Overall")) %>%
  pivot_longer(cols = all_of(cat_vars),
               names_to = "variable",
               values_to = "level") %>%
  tidyr::replace_na(list(level = "Missing")) %>%
  group_by(mhp, variable, level) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(mhp, variable) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    variable = paste0(variable, ": ", level),
    value = n,
    percentage = paste0("(", round(100 * pct, 1), ")"),
    stat = "N(%)"
  ) %>%
  select(mhp, variable, stat, value, percentage) %>%
  mutate(sex = "Male")

## Females
# Continuous variables
all_females <- alldata %>% 
  filter(sex == "Female")

cont_f <- all_females %>%
  mutate(mhp = as.character(mhp)) %>%
  bind_rows(mutate(all_females, mhp = "Overall")) %>%
  group_by(mhp) %>%
  summarise(
    across(c(age, bmi),
           list(
             median = ~ median(.x, na.rm = TRUE),
             q1     = ~ quantile(.x, 0.25, na.rm = TRUE),
             q3     = ~ quantile(.x, 0.75, na.rm = TRUE),
             missing = ~ sum(is.na(.x)),
             n = ~ sum(!is.na(.x)),
             nmedian = ~ {
               med <- round(median(.x, na.rm = TRUE), 0)
               sum(round(.x, 0) == med, na.rm = TRUE)
             },
             nQ1 = ~ {
               q1 <- round(quantile(.x, 0.25, na.rm = TRUE), 0)
               sum(round(.x, 0) == q1, na.rm = TRUE)
             },
             nQ3 = ~ {
               q3 <- round(quantile(.x, 0.75, na.rm = TRUE), 0)
               sum(round(.x, 0) == q3, na.rm = TRUE)
             }
           ),
           .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    -mhp,
    names_to = c("variable", "stat"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(
    value = paste0(round(median, 0), " [", round(q1, 0), "–", round(q3, 0), "]", "; missing = ", missing),
    Nmedian = paste0(nmedian),
    NQ1 = paste0(nQ1),
    NQ3 = paste0(nQ3),
    stat = "Median [IQR]; missing"
  ) %>%
  mutate(sex = "Female") %>%
  select(mhp, variable, sex, stat, value, n, Nmedian, NQ1, NQ3)
# Categorical variables
cat_vars <- c("ethnicity", "smoking", "alcohol", "edu", "mdi_quintile", "activity", "meds", "multimorbidity", "t2dm_t", "chronotype")

cat_f <- all_females %>%
  mutate(mhp = as.character(mhp)) %>%
  bind_rows(mutate(all_females, mhp = "Overall")) %>%
  pivot_longer(cols = all_of(cat_vars),
               names_to = "variable",
               values_to = "level") %>%
  tidyr::replace_na(list(level = "Missing")) %>%
  group_by(mhp, variable, level) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(mhp, variable) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    variable = paste0(variable, ": ", level),
    value = n,
    percentage = paste0("(", round(100 * pct, 1), ")"),
    stat = "N(%)"
  ) %>%
  select(mhp, variable, stat, value, percentage) %>%
  mutate(sex = "Female")


#Combine baseline characteristics
baseline_cont <- bind_rows(cont, cont_m, cont_f)

baseline_cat_1 <- bind_rows(cat, cat_m, cat_f)
baseline_cat <- baseline_cat_1 %>%
  mutate(value = ifelse(value <10, "redacted", as.character(value)))%>%
  mutate(value = ifelse(mhp == "Bipolar" & sex == "Male" & variable == "activity: High level physical activity", "redacted", as.character(value))) %>%
  mutate(value = ifelse(mhp == "Bipolar" & sex == "Male" & variable == "ethnicity: Black", "redacted", as.character(value))) %>%
  mutate(value = ifelse(mhp == "Bipolar" & sex == "Female" & variable == "activity: Missing", "redacted", as.character(value))) %>%
  mutate(value = ifelse(mhp == "Bipolar" & sex == "Female" & variable == "ethnicity: Black", "redacted", as.character(value))) %>%
  mutate(value = ifelse(mhp == "Bipolar" & sex == "Male" & variable == "multimorbidity: 0", "redacted", as.character(value))) %>%
  mutate(percentage = ifelse(value == "redacted", "redacted", as.character(percentage)))

#Count complete cases
vars <- c("age", "bmi", "activity", "alcohol", "chronotype", "edu", "ethnicity", "mdi_quintile", "meds", "multimorbidity", "t2dm_t", "sex", "mhp", "smoking")
complete <- alldata %>%
  filter(complete.cases(across(all_of(vars))))
complete_n <- data.frame(nrow(complete))
complete_by_group <- as.data.frame(table(complete$mhp))
complete_by_group_sex <- as.data.frame(table(complete$mhp, complete$sex))
complete_cases <- bind_rows(
                      Overall = complete_n,
                      complete_by_group,
                      complete_by_group_sex)

#Count incomplete cases
incomplete <- alldata %>%
  filter(!complete.cases(across(all_of(vars))))
incomplete_n <- data.frame(nrow(incomplete))
incomplete_by_group <- as.data.frame(table(incomplete$mhp))
incomplete_by_group_sex <- as.data.frame(table(incomplete$mhp, incomplete$sex))
incomplete_cases <- bind_rows(
  Overall = incomplete_n,
  incomplete_by_group,
  incomplete_by_group_sex)

### Write CSV
write.csv(sample, "total_sample.csv")
write.csv(missing_multimorbidity, "missing_multimorbidity.csv")
write.csv(baseline_cont, "baseline_continuous.csv", row.names = FALSE)
write.csv(baseline_cat, "baseline_categorical.csv", row.names = FALSE)
write.csv(complete_cases, "complete_cases.csv", row.names = FALSE)
write.csv(incomplete_cases, "incomplete_cases.csv", row.names = FALSE)
write.csv(alldata, "alldata_clean_01.csv")

## Switch to Python kernel
%%bash
dx upload "total_sample.csv"
dx upload "missing_multimorbidity.csv"
dx upload "baseline_characteristics.csv"
dx upload "alldata_clean_01.csv"
