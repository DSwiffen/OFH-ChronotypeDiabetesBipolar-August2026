### Aim:  i) complete versus incomplete case comparison of baseline characteristics

## Start in Python kernel
%%bash
dx download "/sleep_bipolar/alldata_clean_01.csv"
# Load libraries
library(dplyr)
library(stringr)
library(tidyr)



##### CODE FOR COMPLETE CASES ONLY ######
#### Remove cases that have NA for any variable. This creates a "clean" sample that should contain no missing data points
# Read CSV created in 02- covariates.r
all_comp <- read.csv(
  "alldata_clean_01.csv",
  colClasses = c(mdi_quintile = "character",
                 t2dm_t = "character"))
comp <- all_comp[complete.cases(all_comp[, c("age", "sex", "bmi", "mdi_quintile","ethnicity", "smoking", "alcohol", "edu", "activity", "meds", "multimorbidity", "chronotype", "t2dm_t")]), ]
#### Create baseline characteristics tables for complete cases
## Continuous variables
# Both sexes
comp_cont <- comp %>%
  mutate(mhp = as.character(mhp)) %>%
  mutate(mdi_quintile = as.character(mdi_quintile)) %>%
  bind_rows(mutate(comp, mhp = "Overall")) %>%
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
    stat = "Median [IQR]; missing"
  ) %>%
  select(mhp, variable, stat, value, n, nmedian, nQ1, nQ3) %>%
  mutate(group = "Complete")
# Categorical variables
cat_vars <- c("sex", "ethnicity", "smoking", "alcohol", "edu", "activity", "meds", "multimorbidity", "mdi_quintile","chronotype", "t2dm_t")

comp_cat <- comp %>%
  mutate(mhp = as.character(mhp)) %>%
  mutate(mdi_quintile = as.character(mdi_quintile)) %>%
  bind_rows(mutate(comp, mhp = "Overall")) %>%
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
  mutate(group = "Complete")


##### CODE FOR INCOMPLETE CASES ONLY ######
#### Retain only cases that have NA for any variable
# Read CSV
all_incomp <- read.csv(
  "alldata_clean_01.csv",
  colClasses = c(mdi_quintile = "character",
                 t2dm_t = "character"))
incomp <- all_incomp[!complete.cases(all_incomp[, c("sex", "age", "bmi", "mdi_quintile", "ethnicity", "smoking", "alcohol", "edu", "activity", "meds", "multimorbidity", "chronotype", "t2dm_t")]), ]
#### Create baseline characteristics tables for complete cases
## Continuous variables
# Both sexes
incomp_cont <- incomp %>%
  mutate(mhp = as.character(mhp)) %>%
  mutate(mdi_quintile = as.character(mdi_quintile)) %>%
  bind_rows(mutate(incomp, mhp = "Overall")) %>%
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
    stat = "Median [IQR]; missing"
  ) %>%
  select(mhp, variable, stat, value, n, nmedian, nQ1, nQ3) %>%
  mutate(group = "Incomplete")
# Categorical variables
cat_vars <- c("sex", "ethnicity", "smoking", "alcohol", "edu", "activity", "meds", "multimorbidity", "mdi_quintile", "chronotype", "t2dm_t")

incomp_cat <- incomp %>%
  mutate(mhp = as.character(mhp)) %>%
  mutate(mdi_quintile = as.character(mdi_quintile)) %>%
  bind_rows(mutate(incomp, mhp = "Overall")) %>%
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
  mutate(group = "Incomplete")

#Combine baseline characteristics
comp_v_incomp_cont <- bind_rows(comp_cont, incomp_cont)

comp_v_incomp_cat_1 <- bind_rows(comp_cat, incomp_cat)
comp_v_incomp_cat <- comp_v_incomp_cat_1 %>%
  mutate(value = ifelse(value <10, "redacted", as.character(value))) %>%
  mutate(value = ifelse(mhp == "Bipolar" & group == "Incomplete" & variable == "activity: Missing", "redacted", as.character(value))) %>%
  mutate(value = ifelse(mhp == "Bipolar" & group == "Incomplete" & variable == "sex: Missing", "redacted", as.character(value))) %>%
  mutate(value = ifelse(mhp == "Bipolar" & group == "Incomplete" & variable == "sex: Male", "redacted", as.character(value))) %>%
  mutate(percentage = ifelse(value == "redacted", "redacted", as.character(percentage)))
  
#General total N and N by group for both complete and incomplete cases
total_comp <- nrow(comp)
total_incomp <- nrow(incomp)
comp_by_group <- table(comp$mhp)
incomp_by_group <- table(incomp$mhp)
#Combine the above
total_n_comp_v_incomp <- rbind(total_comp, total_incomp)
group_n_comp_v_incomp <- bind_rows(comp_by_group, incomp_by_group)

### Write CSV
write.csv(comp_v_incomp_cont, "continuous_complete_versus_incomplete_cases.csv", row.names = FALSE)
write.csv(comp_v_incomp_cat, "categorical_complete_versus_incomplete_cases.csv", row.names = FALSE)
write.csv(total_n_comp_v_incomp, "total_n_complete_versus_incomplete.csv", row.names = FALSE)
write.csv(group_n_comp_v_incomp, "group_n_complete_versus_incomplete.csv", row.names = FALSE)

## Switch to Python kernel
%%bash
dx upload "baseline_complete_versus_incomplete_cases.csv"
dx upload "total_n_complete_versus_incomplete.csv"
dx upload "group_n_complete_versus_incomplete.csv"
