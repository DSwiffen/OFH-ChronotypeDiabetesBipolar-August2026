### Aim:  i) Generate odds ratio for the relationship between chronotype and t2dm across people with bipolar and no_mhp using logistic regression
##            - Adjusted for a range of potential confounders
##            - Stratified according to sex
##       ii) Compare nested models with and without the interaction term using to determine whether the interaction term improves model fit


# Open Python kernel
%%bash
dx download "/sleep_bipolar/alldata_clean_01.csv"

# Switch to R kernel
# Read CSV created in 02- covariates.r
alldata <- read.csv("alldata_clean_01.csv")
# Load libraries
library(dplyr)
library(tidyr)


## Create a mhp_status variable
alldata <- alldata %>%
  mutate(mhp_status = ifelse(bipolar == TRUE, "bipolar", "no_mhp"))

## Convert covariates variables to factors and relevel
factorcols <- c("chronotype", "mhp_status", "sex", "ethnicity", "mdi_quintile", "edu", "activity", "smoking", "alcohol", "meds", "multimorbidity")
alldata[factorcols] <- lapply(alldata[factorcols], as.factor)
alldata$chronotype <- relevel(alldata$chronotype, ref = "definite morning")
alldata$sex <- relevel(alldata$sex, ref = "Male")
alldata$ethnicity <- relevel(alldata$ethnicity, ref = "White")
alldata$mdi_quintile <- relevel(alldata$mdi_quintile, ref = "5")
alldata$edu <- relevel(alldata$edu, ref = "Degree")
alldata$smoking <- relevel(alldata$smoking, ref = "Never regularly smoked cigarettes, cigars or tobacco pipes")
alldata$alcohol <- relevel(alldata$alcohol, ref = "Never drinks alcohol")
alldata$activity <- relevel(alldata$activity, ref = "High level physical activity")
alldata$meds <- relevel(alldata$meds, ref = "Not taking sleeping medications")
alldata$multimorbidity <- relevel(alldata$multimorbidity, ref = "0")
alldata$mhp_status <- relevel(alldata$mhp_status, ref = "no_mhp")

rm(factorcols)

## Convert relevant variables to numeric
numcols <- c("age", "bmi")
alldata[numcols] <- lapply(alldata[numcols], as.numeric)
rm(numcols)

#Create blank table
extract_or <- function(model, model_name) {
  coefs <- coef(summary(model))
  if (is.vector(coefs)) {
    coefs <- matrix(coefs, nrow = 1)
    colnames(coefs) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    rownames(coefs) <- names(coef(model))
  }
   coefs <- coefs[rownames(coefs) != "(Intercept)", , drop = FALSE]
  ci <- confint.default(model)
  ci <- ci[rownames(coefs), , drop = FALSE]
  data.frame(
    model = model_name,
    predictor = rownames(coefs),
    n = nobs(model),
    OR = exp(coefs[, "Estimate"]),
    lower = exp(ci[, 1]),
    upper = exp(ci[, 2]),
    p.value = coefs[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )
}


### Question 1
## What are the odds of type 2 diabetes in those with bipolar (versus no MHP) independent of chronotype,
## and what are the odds of type 2 diabetes in evening chronotypes (versus morning chronotype) independent of MHP status?
fit6 <- glm(t2dm ~ chronotype + mhp_status + age + I(age^2) + sex + ethnicity + smoking + alcohol +
              edu +  mdi_quintile + multimorbidity + meds + activity + bmi+ I(bmi^2),
            family = binomial(link="logit"),
            data = alldata)

### Question 2
## How much does chronotype influence the association between bipolar status and T2DM?
fit8 <- glm(t2dm ~ chronotype * mhp_status + age + I(age^2) + sex + ethnicity + smoking + alcohol +
              edu +  mdi_quintile + multimorbidity + meds + activity + bmi+ I(bmi^2),
            family = binomial(link="logit"),
            data = alldata)

### Question 3
## How much does chronotype influence the association between bipolar status and T2DM, stratified by sex?

#Filter female
female <- alldata %>%
  filter(sex == "Female")
# Estimate association of chronotype and mhp_status independent of each other in females
fit14 <- glm(t2dm ~ chronotype + mhp_status + age + I(age^2) + ethnicity + smoking + alcohol +
              edu +  mdi_quintile + multimorbidity + meds + activity + bmi+ I(bmi^2),
            family = binomial(link="logit"),
            data = female)

# Estimate the interaction effect of chronotype on type 2 diabetes in bipolar in females
fit16 <- glm(t2dm ~ chronotype * mhp_status + age + I(age^2)  + ethnicity + smoking + alcohol +
               edu +  mdi_quintile + multimorbidity + meds + activity + bmi+ I(bmi^2),
            family = binomial(link="logit"),
            data = female)


#Filter male
male <- alldata %>%
  filter(sex == "Male")
# Estimate association of chronotype and mhp_status independent of each other in males
fit18 <- glm(t2dm ~ chronotype + mhp_status + age + I(age^2) + ethnicity + smoking + alcohol +
              edu +  mdi_quintile + multimorbidity + meds + activity + bmi+ I(bmi^2),
            family = binomial(link="logit"),
            data = male)

# Estimate the interaction effect of chronotype on type 2 diabetes in bipolar in males
fit20 <- glm(t2dm ~ chronotype * mhp_status + age + I(age^2)  + ethnicity + smoking + alcohol +
               edu +  mdi_quintile + multimorbidity + meds + activity + bmi+ I(bmi^2),
             family = binomial(link="logit"),
             data = male)


#Generate output
output <- rbind(
  extract_or(fit6, "Q3_adjusted"),
  extract_or(fit8, "Q4_adjusted"),
  extract_or(fit14, "Q7_female_adjusted_independent"),
  extract_or(fit16, "Q7_female_adjusted_interaction"),
  extract_or(fit18, "Q7_male_adjusted_independent"),
  extract_or(fit20, "Q7_male_adjusted_interaction")
  )

#Create simplified output containing only predictor variables and cleaner outputs
output_filtered <- output %>%
  filter(grepl("chronotype|mhp_status", predictor))%>%
  mutate(OR_CI = paste0((round(OR,2)), " (", (round(lower,2)), "-", (round(upper,2)), ")"),) %>%
  mutate(adj.p.value_2d = round(adj.p.value,4))%>%
  mutate(question = case_when(
    grepl("Q3", model) ~ "chronotype+mhp_status vs T2DM",
    grepl("Q4", model) ~ "chronotype*mhp_status vs T2DM",
    grepl("_independent", model) ~ "chronotype+mhp_status vs T2DM by sex",
    grepl("_interaction", model) ~ "chronotype*mhp_status vs T2DM by sex",
    TRUE ~ NA)) %>%
  mutate(sex = case_when(
    grepl("Q7_male", model) ~ "Male",
    grepl("Q7_female", model) ~ "Female",
    TRUE ~ "Both"
  )) %>%
  mutate(sex = as.factor(sex)) %>%
  relocate(question, .before = model) %>%
  relocate(sex, .before = n) %>%
  mutate(adjust = case_when(
    grepl("adjusted", model) ~ "adjusted",
    TRUE ~ NA)) %>%
  relocate(adjust, .before = predictor) %>%
  select(-model)

output_filtered$predictor <- gsub("chronotype", "", output_filtered$predictor)
output_filtered$predictor <- gsub("mhp_status", "", output_filtered$predictor)

write.csv(output_filtered, "logreg.csv",row.names = FALSE)


### Compare main effects models with interaction models to generate p-values using likelihood ratios
## Whole sample
a1 <- anova(fit6, fit8, test = "LRT")
## Female participants
a2 <- anova(fit14, fit16, test = "LRT")
## Male participants
a3 <- anova(fit18, fit20, test = "LRT")

#Combine into single table
LR_table <- bind_rows(
  mutate(a1, sex = "Both"),
  mutate(a2, sex = "Female"),
  mutate(a3, sex = "Male")
) %>%
  mutate(model = rep(c("Main effects", "Interaction"), 3))
# Correct for multiple comparisons using FDR-correction
LR_table$adj.p.value <- NA

LR_table$adj.p.value[!is.na(LR_table$`Pr(>Chi)`)] <-
  p.adjust(
    LR_table$`Pr(>Chi)`[!is.na(LR_table$`Pr(>Chi)`)],
    method = "fdr"
  )
LR_table <- LR_table %>%
  select(c(Resid..Df, sex, model, adj.p.value)) %>%
  rename(N = Resid..Df)

write.csv(LR_table, "LR_output.csv",row.names = FALSE)


#Switch to Python kernel
%%bash
dx upload "logreg.csv"
dx upload "LR_output.csv"
