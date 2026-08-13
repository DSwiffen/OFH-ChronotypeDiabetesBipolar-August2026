#### Objective: Create a new logreg output in which the bipolar disorder coefficient is added to each chronotype output - do so manually to negate the need for a specific R package

### To run after 'logreg.r'

################ Whole sample - main effect (fit6) ######################
#generate objects
beta1 <- coef(fit6)
vc1 <- vcov(fit6)

#Bipolar disorder main effects
logOR_bipolar <-
  beta1["mhp_statusbipolar"]

SE_bipolar <- sqrt(vc1["mhp_statusbipolar", "mhp_statusbipolar"])

OR_bipolar <- exp(logOR_bipolar)

CI_bipolar <- c(
  lower = exp(logOR_bipolar - 1.96 * SE_bipolar),
  upper = exp(logOR_bipolar + 1.96 * SE_bipolar))

##Chronotype main effects
#More morning than evening
logOR_more_morning <- beta1["chronotypemore morning than evening"]

SE_more_morning <- sqrt(vc1["chronotypemore morning than evening", "chronotypemore morning than evening"])

OR_more_morning <- exp(logOR_more_morning)

CI_more_morning <- c(
  lower = exp(logOR_more_morning - 1.96 * SE_more_morning),
  upper = exp(logOR_more_morning + 1.96 * SE_more_morning))

#More evening than morning
logOR_more_evening <- beta1["chronotypemore evening than morning"]

SE_more_evening <- sqrt(vc1["chronotypemore evening than morning", "chronotypemore evening than morning"])

OR_more_evening <- exp(logOR_more_evening)

CI_more_evening <- c(
  lower = exp(logOR_more_evening - 1.96 * SE_more_evening),
  upper = exp(logOR_more_evening + 1.96 * SE_more_evening))

#Definite evening
logOR_definite_evening <- beta1["chronotypedefinite evening"]

SE_definite_evening <- sqrt(vc1["chronotypedefinite evening", "chronotypedefinite evening"])

OR_definite_evening <- exp(logOR_definite_evening)

CI_definite_evening <- c(
  lower = exp(logOR_definite_evening - 1.96 * SE_definite_evening),
  upper = exp(logOR_definite_evening + 1.96 * SE_definite_evening))



################ Whole sample - interaction model (fit8) ######################
#generate objects
beta2 <- coef(fit8)
vc2 <- vcov(fit8)

#Definite morning
logOR_definite_morning_int <-
  beta2["mhp_statusbipolar"]

SE_definite_morning_int <- sqrt(
  vc2["mhp_statusbipolar", "mhp_statusbipolar"])

OR_definite_morning_int <- exp(logOR_definite_morning_int)

CI_definite_morning_int <- c(
  lower = exp(logOR_definite_morning_int - 1.96 * SE_definite_morning_int),
  upper = exp(logOR_definite_morning_int + 1.96 * SE_definite_morning_int))

#More morning than evening
logOR_more_morning_int <-
  beta2["mhp_statusbipolar"] +
  beta2["chronotypemore morning than evening:mhp_statusbipolar"]

SE_more_morning_int <- sqrt(
  vc2["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc2["chronotypemore morning than evening:mhp_statusbipolar", "chronotypemore morning than evening:mhp_statusbipolar"] +
    2 * vc2["mhp_statusbipolar", "chronotypemore morning than evening:mhp_statusbipolar"])

OR_more_morning_int <- exp(logOR_more_morning_int)

CI_more_morning_int <- c(
  lower = exp(logOR_more_morning_int - 1.96 * SE_more_morning_int),
  upper = exp(logOR_more_morning_int + 1.96 * SE_more_morning_int))

#More evening than morning
logOR_more_evening_int <-
  beta2["mhp_statusbipolar"] +
  beta2["chronotypemore evening than morning:mhp_statusbipolar"]

SE_more_evening_int <- sqrt(
  vc2["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc2["chronotypemore evening than morning:mhp_statusbipolar", "chronotypemore evening than morning:mhp_statusbipolar"] +
    2 * vc2["mhp_statusbipolar", "chronotypemore evening than morning:mhp_statusbipolar"])

OR_more_evening_int <- exp(logOR_more_evening_int)

CI_more_evening_int <- c(
  lower = exp(logOR_more_evening_int - 1.96 * SE_more_evening_int),
  upper = exp(logOR_more_evening_int + 1.96 * SE_more_evening_int))

#Definite evening
logOR_definite_evening_int <-
  beta2["mhp_statusbipolar"] +
  beta2["chronotypedefinite evening:mhp_statusbipolar"]

SE_definite_evening_int <- sqrt(
  vc2["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc2["chronotypedefinite evening:mhp_statusbipolar", "chronotypedefinite evening:mhp_statusbipolar"] +
    2 * vc2["mhp_statusbipolar", "chronotypedefinite evening:mhp_statusbipolar"])

OR_definite_evening_int <- exp(logOR_definite_evening_int)

CI_definite_evening_int <- c(
  lower = exp(logOR_definite_evening_int - 1.96 * SE_definite_evening_int),
  upper = exp(logOR_definite_evening_int + 1.96 * SE_definite_evening_int))


############### Females - main effects (fit14) ####################
#generate objects
beta3 <- coef(fit14)
vc3 <- vcov(fit14)

#Bipolar disorder main effects
logOR_bipolar_f <-
  beta3["mhp_statusbipolar"]

SE_bipolar_f <- sqrt(vc3["mhp_statusbipolar", "mhp_statusbipolar"])

OR_bipolar_f <- exp(logOR_bipolar_f)

CI_bipolar_f <- c(
  lower = exp(logOR_bipolar_f - 1.96 * SE_bipolar_f),
  upper = exp(logOR_bipolar_f + 1.96 * SE_bipolar_f))

##Chronotype main effects
#More morning than evening
logOR_more_morning_f <- beta3["chronotypemore morning than evening"]

SE_more_morning_f <- sqrt(vc3["chronotypemore morning than evening", "chronotypemore morning than evening"])

OR_more_morning_f <- exp(logOR_more_morning_f)

CI_more_morning_f <- c(
  lower = exp(logOR_more_morning_f - 1.96 * SE_more_morning_f),
  upper = exp(logOR_more_morning_f + 1.96 * SE_more_morning_f))

#More evening than morning
logOR_more_evening_f <- beta3["chronotypemore evening than morning"]

SE_more_evening_f <- sqrt(vc3["chronotypemore evening than morning", "chronotypemore evening than morning"])

OR_more_evening_f <- exp(logOR_more_evening_f)

CI_more_evening_f <- c(
  lower = exp(logOR_more_evening_f - 1.96 * SE_more_evening_f),
  upper = exp(logOR_more_evening_f + 1.96 * SE_more_evening_f))

#Definite evening
logOR_definite_evening_f <- beta3["chronotypedefinite evening"]

SE_definite_evening_f <- sqrt(vc3["chronotypedefinite evening", "chronotypedefinite evening"])

OR_definite_evening_f <- exp(logOR_definite_evening_f)

CI_definite_evening_f <- c(
  lower = exp(logOR_definite_evening_f - 1.96 * SE_definite_evening_f),
  upper = exp(logOR_definite_evening_f + 1.96 * SE_definite_evening_f))



############## Females - interaction model (fit16) ######################
#generate objects
beta4 <- coef(fit16)
vc4 <- vcov(fit16)

#Definite morning
logOR_definite_morning_int_f <-
  beta4["mhp_statusbipolar"]

SE_definite_morning_int_f <- sqrt(
  vc4["mhp_statusbipolar", "mhp_statusbipolar"])

OR_definite_morning_int_f <- exp(logOR_definite_morning_int_f)

CI_definite_morning_int_f <- c(
  lower = exp(logOR_definite_morning_int_f - 1.96 * SE_definite_morning_int_f),
  upper = exp(logOR_definite_morning_int_f + 1.96 * SE_definite_morning_int_f))

#More morning than evening
logOR_more_morning_int_f <-
  beta4["mhp_statusbipolar"] +
  beta4["chronotypemore morning than evening:mhp_statusbipolar"]

SE_more_morning_int_f<- sqrt(
  vc4["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc4["chronotypemore morning than evening:mhp_statusbipolar", "chronotypemore morning than evening:mhp_statusbipolar"] +
    2 * vc4["mhp_statusbipolar", "chronotypemore morning than evening:mhp_statusbipolar"])

OR_more_morning_int_f <- exp(logOR_more_morning_int_f)

CI_more_morning_int_f <- c(
  lower = exp(logOR_more_morning_int_f - 1.96 * SE_more_morning_int_f),
  upper = exp(logOR_more_morning_int_f + 1.96 * SE_more_morning_int_f))

#More evening than morning
logOR_more_evening_int_f <-
  beta4["mhp_statusbipolar"] +
  beta4["chronotypemore evening than morning:mhp_statusbipolar"]

SE_more_evening_int_f <- sqrt(
  vc4["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc4["chronotypemore evening than morning:mhp_statusbipolar", "chronotypemore evening than morning:mhp_statusbipolar"] +
    2 * vc4["mhp_statusbipolar", "chronotypemore evening than morning:mhp_statusbipolar"])

OR_more_evening_int_f <- exp(logOR_more_evening_int_f)

CI_more_evening_int_f <- c(
  lower = exp(logOR_more_evening_int_f - 1.96 * SE_more_evening_int_f),
  upper = exp(logOR_more_evening_int_f + 1.96 * SE_more_evening_int_f))

#Definite evening
logOR_definite_evening_int_f <-
  beta4["mhp_statusbipolar"] +
  beta4["chronotypedefinite evening:mhp_statusbipolar"]

SE_definite_evening_int_f <- sqrt(
  vc4["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc4["chronotypedefinite evening:mhp_statusbipolar", "chronotypedefinite evening:mhp_statusbipolar"] +
    2 * vc4["mhp_statusbipolar", "chronotypedefinite evening:mhp_statusbipolar"])

OR_definite_evening_int_f <- exp(logOR_definite_evening_int_f)

CI_definite_evening_int_f <- c(
  lower = exp(logOR_definite_evening_int_f - 1.96 * SE_definite_evening_int_f),
  upper = exp(logOR_definite_evening_int_f + 1.96 * SE_definite_evening_int_f))



############### Males - main effects (fit18) ####################
#generate objects
beta5 <- coef(fit18)
vc5 <- vcov(fit18)

#Bipolar disorder main effects
logOR_bipolar_m <-
  beta5["mhp_statusbipolar"]

SE_bipolar_m <- sqrt(vc5["mhp_statusbipolar", "mhp_statusbipolar"])

OR_bipolar_m <- exp(logOR_bipolar_m)

CI_bipolar_m <- c(
  lower = exp(logOR_bipolar_m - 1.96 * SE_bipolar_m),
  upper = exp(logOR_bipolar_m + 1.96 * SE_bipolar_m))

##Chronotype main effects
#More morning than evening
logOR_more_morning_m <- beta5["chronotypemore morning than evening"]

SE_more_morning_m <- sqrt(vc5["chronotypemore morning than evening", "chronotypemore morning than evening"])

OR_more_morning_m <- exp(logOR_more_morning_m)

CI_more_morning_m <- c(
  lower = exp(logOR_more_morning_m - 1.96 * SE_more_morning_m),
  upper = exp(logOR_more_morning_m + 1.96 * SE_more_morning_m))

#More evening than morning
logOR_more_evening_m <- beta5["chronotypemore evening than morning"]

SE_more_evening_m <- sqrt(vc5["chronotypemore evening than morning", "chronotypemore evening than morning"])

OR_more_evening_m <- exp(logOR_more_evening_m)

CI_more_evening_m <- c(
  lower = exp(logOR_more_evening_m - 1.96 * SE_more_evening_m),
  upper = exp(logOR_more_evening_m + 1.96 * SE_more_evening_m))

#Definite evening
logOR_definite_evening_m <- beta5["chronotypedefinite evening"]

SE_definite_evening_m <- sqrt(vc5["chronotypedefinite evening", "chronotypedefinite evening"])

OR_definite_evening_m <- exp(logOR_definite_evening_m)

CI_definite_evening_m <- c(
  lower = exp(logOR_definite_evening_m - 1.96 * SE_definite_evening_m),
  upper = exp(logOR_definite_evening_m + 1.96 * SE_definite_evening_m))


############## Males - interaction model (fit20) ######################
#generate objects
beta6 <- coef(fit20)
vc6 <- vcov(fit20)

#Definite morning
logOR_definite_morning_int_m <-
  beta6["mhp_statusbipolar"]

SE_definite_morning_int_m <- sqrt(
  vc6["mhp_statusbipolar", "mhp_statusbipolar"])

OR_definite_morning_int_m <- exp(logOR_definite_morning_int_m)

CI_definite_morning_int_m <- c(
  lower = exp(logOR_definite_morning_int_m - 1.96 * SE_definite_morning_int_m),
  upper = exp(logOR_definite_morning_int_m + 1.96 * SE_definite_morning_int_m))

#More morning than evening
logOR_more_morning_int_m <-
  beta6["mhp_statusbipolar"] +
  beta6["chronotypemore morning than evening:mhp_statusbipolar"]

SE_more_morning_int_m<- sqrt(
  vc6["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc6["chronotypemore morning than evening:mhp_statusbipolar", "chronotypemore morning than evening:mhp_statusbipolar"] +
    2 * vc6["mhp_statusbipolar", "chronotypemore morning than evening:mhp_statusbipolar"])

OR_more_morning_int_m <- exp(logOR_more_morning_int_m)

CI_more_morning_int_m <- c(
  lower = exp(logOR_more_morning_int_m - 1.96 * SE_more_morning_int_m),
  upper = exp(logOR_more_morning_int_m + 1.96 * SE_more_morning_int_m))

#More evening than morning
logOR_more_evening_int_m <-
  beta6["mhp_statusbipolar"] +
  beta6["chronotypemore evening than morning:mhp_statusbipolar"]

SE_more_evening_int_m <- sqrt(
  vc6["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc6["chronotypemore evening than morning:mhp_statusbipolar", "chronotypemore evening than morning:mhp_statusbipolar"] +
    2 * vc6["mhp_statusbipolar", "chronotypemore evening than morning:mhp_statusbipolar"])

OR_more_evening_int_m <- exp(logOR_more_evening_int_m)

CI_more_evening_int_m <- c(
  lower = exp(logOR_more_evening_int_m - 1.96 * SE_more_evening_int_m),
  upper = exp(logOR_more_evening_int_m + 1.96 * SE_more_evening_int_m))

#Definite evening
logOR_definite_evening_int_m <-
  beta6["mhp_statusbipolar"] +
  beta6["chronotypedefinite evening:mhp_statusbipolar"]

SE_definite_evening_int_m <- sqrt(
  vc6["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc6["chronotypedefinite evening:mhp_statusbipolar", "chronotypedefinite evening:mhp_statusbipolar"] +
    2 * vc6["mhp_statusbipolar", "chronotypedefinite evening:mhp_statusbipolar"])

OR_definite_evening_int_m <- exp(logOR_definite_evening_int_m)

CI_definite_evening_int_m <- c(
  lower = exp(logOR_definite_evening_int_m - 1.96 * SE_definite_evening_int_m),
  upper = exp(logOR_definite_evening_int_m + 1.96 * SE_definite_evening_int_m))


### Generate p-values of each estimate ###
p_bipolar <- 2 * (1 - pnorm(abs(logOR_bipolar / SE_bipolar)))
p_more_morning <- 2 * (1 - pnorm(abs(logOR_more_morning / SE_more_morning)))
p_more_evening <- 2 * (1 - pnorm(abs(logOR_more_evening / SE_more_evening)))
p_definite_evening <- 2 * (1 - pnorm(abs(logOR_definite_evening / SE_definite_evening)))
p_definite_morning_int <- 2 * (1 - pnorm(abs(logOR_definite_morning_int / SE_definite_morning_int)))
p_more_morning_int <- 2 * (1 - pnorm(abs(logOR_more_morning_int / SE_more_morning_int)))
p_more_evening_int <- 2 * (1 - pnorm(abs(logOR_more_evening_int / SE_more_evening_int)))
p_definite_evening_int <- 2 * (1 - pnorm(abs(logOR_definite_evening_int / SE_definite_evening_int)))

p_bipolar_f <- 2 * (1 - pnorm(abs(logOR_bipolar_f / SE_bipolar_f)))
p_more_morning_f <- 2 * (1 - pnorm(abs(logOR_more_morning_f / SE_more_morning_f)))
p_more_evening_f <- 2 * (1 - pnorm(abs(logOR_more_evening_f / SE_more_evening_f)))
p_definite_evening_f <- 2 * (1 - pnorm(abs(logOR_definite_evening_f / SE_definite_evening_f)))
p_definite_morning_int_f <- 2 * (1 - pnorm(abs(logOR_definite_morning_int_f / SE_definite_morning_int_f)))
p_more_morning_int_f <- 2 * (1 - pnorm(abs(logOR_more_morning_int_f / SE_more_morning_int_f)))
p_more_evening_int_f <- 2 * (1 - pnorm(abs(logOR_more_evening_int_f / SE_more_evening_int_f)))
p_definite_evening_int_f <- 2 * (1 - pnorm(abs(logOR_definite_evening_int_f / SE_definite_evening_int_f)))

p_bipolar_m <- 2 * (1 - pnorm(abs(logOR_bipolar_m / SE_bipolar_m)))
p_more_morning_m <- 2 * (1 - pnorm(abs(logOR_more_morning_m / SE_more_morning_m)))
p_more_evening_m <- 2 * (1 - pnorm(abs(logOR_more_evening_m / SE_more_evening_m)))
p_definite_evening_m <- 2 * (1 - pnorm(abs(logOR_definite_evening_m / SE_definite_evening_m)))
p_definite_morning_int_m <- 2 * (1 - pnorm(abs(logOR_definite_morning_int_m / SE_definite_morning_int_m)))
p_more_morning_int_m <- 2 * (1 - pnorm(abs(logOR_more_morning_int_m / SE_more_morning_int_m)))
p_more_evening_int_m <- 2 * (1 - pnorm(abs(logOR_more_evening_int_m / SE_more_evening_int_m)))
p_definite_evening_int_m <- 2 * (1 - pnorm(abs(logOR_definite_evening_int_m / SE_definite_evening_int_m)))


## Calculate sample size of each model ##
n_fit6  <- nobs(fit6)
n_fit8  <- nobs(fit8)
n_fit14 <- nobs(fit14)
n_fit16 <- nobs(fit16)
n_fit18 <- nobs(fit18)
n_fit20 <- nobs(fit20)


### Combine all outputs in one table ###
logreg_summary1 <- data.frame(
  sex = c(
    rep("Both", 4),
    rep("Both", 4),
    rep("Female", 4),
    rep("Female", 4),
    rep("Male", 4),
    rep("Male", 4)
  ),
  model = c(
    rep("Main effects", 4),
    rep("Interaction", 4),
    rep("Main effects", 4),
    rep("Interaction", 4),
    rep("Main effects", 4),
    rep("Interaction", 4)
  ),
  chronotype = rep(
    c(
      "Definite morning",
      "More morning than evening",
      "More evening than morning",
      "Definite evening"
    ),
    6
  ),
  N = c(
    rep(n_fit6, 4),
    rep(n_fit8, 4),
    rep(n_fit14, 4),
    rep(n_fit16, 4),
    rep(n_fit18, 4),
    rep(n_fit20, 4)
  ),
  OR = c(
    # Whole sample - main effects
    OR_bipolar,
    OR_more_morning,
    OR_more_evening,
    OR_definite_evening,
    # Whole sample - interaction model
    OR_definite_morning_int,
    OR_more_morning_int,
    OR_more_evening_int,
    OR_definite_evening_int,
    # Females - main effects
    OR_bipolar_f,
    OR_more_morning_f,
    OR_more_evening_f,
    OR_definite_evening_f,
    # Females - interaction model
    OR_definite_morning_int_f,
    OR_more_morning_int_f,
    OR_more_evening_int_f,
    OR_definite_evening_int_f,
    # Males - main effects
    OR_bipolar_m,
    OR_more_morning_m,
    OR_more_evening_m,
    OR_definite_evening_m,
    # Males - interaction model
    OR_definite_morning_int_m,
    OR_more_morning_int_m,
    OR_more_evening_int_m,
    OR_definite_evening_int_m),
  CI_lower = c(
    # Whole sample - main effects
    CI_bipolar[grepl("^lower", names(CI_bipolar))],
    CI_more_morning[grepl("^lower", names(CI_bipolar))],
    CI_more_evening[grepl("^lower", names(CI_bipolar))],
    CI_definite_evening[grepl("^lower", names(CI_bipolar))],
    # Whole sample - interaction model
    CI_definite_morning_int[grepl("^lower", names(CI_bipolar))],
    CI_more_morning_int[grepl("^lower", names(CI_bipolar))],
    CI_more_evening_int[grepl("^lower", names(CI_bipolar))],
    CI_definite_evening_int[grepl("^lower", names(CI_bipolar))],
    # Females - main effects
    CI_bipolar_f[grepl("^lower", names(CI_bipolar))],
    CI_more_morning_f[grepl("^lower", names(CI_bipolar))],
    CI_more_evening_f[grepl("^lower", names(CI_bipolar))],
    CI_definite_evening_f[grepl("^lower", names(CI_bipolar))],
    # Females - interaction model
    CI_definite_morning_int_f[grepl("^lower", names(CI_bipolar))],
    CI_more_morning_int_f[grepl("^lower", names(CI_bipolar))],
    CI_more_evening_int_f[grepl("^lower", names(CI_bipolar))],
    CI_definite_evening_int_f[grepl("^lower", names(CI_bipolar))],
    # Males - main effects
    CI_bipolar_m[grepl("^lower", names(CI_bipolar))],
    CI_more_morning_m[grepl("^lower", names(CI_bipolar))],
    CI_more_evening_m[grepl("^lower", names(CI_bipolar))],
    CI_definite_evening_m[grepl("^lower", names(CI_bipolar))],
    # Males - interaction model
    CI_definite_morning_int_m[grepl("^lower", names(CI_bipolar))],
    CI_more_morning_int_m[grepl("^lower", names(CI_bipolar))],
    CI_more_evening_int_m[grepl("^lower", names(CI_bipolar))],
    CI_definite_evening_int_m[grepl("^lower", names(CI_bipolar))]
  ),
  CI_upper = c(
    # Whole sample - main effects
    CI_bipolar[grepl("^upper", names(CI_bipolar))],
    CI_more_morning[grepl("^upper", names(CI_bipolar))],
    CI_more_evening[grepl("^upper", names(CI_bipolar))],
    CI_definite_evening[grepl("^upper", names(CI_bipolar))],
    # Whole sample - interaction model
    CI_definite_morning_int[grepl("^upper", names(CI_bipolar))],
    CI_more_morning_int[grepl("^upper", names(CI_bipolar))],
    CI_more_evening_int[grepl("^upper", names(CI_bipolar))],
    CI_definite_evening_int[grepl("^upper", names(CI_bipolar))],
    # Females - main effects
    CI_bipolar_f[grepl("^upper", names(CI_bipolar))],
    CI_more_morning_f[grepl("^upper", names(CI_bipolar))],
    CI_more_evening_f[grepl("^upper", names(CI_bipolar))],
    CI_definite_evening_f[grepl("^upper", names(CI_bipolar))],
    # Females - interaction model
    CI_definite_morning_int_f[grepl("^upper", names(CI_bipolar))],
    CI_more_morning_int_f[grepl("^upper", names(CI_bipolar))],
    CI_more_evening_int_f[grepl("^upper", names(CI_bipolar))],
    CI_definite_evening_int_f[grepl("^upper", names(CI_bipolar))],
    # Males - main effects
    CI_bipolar_m[grepl("^upper", names(CI_bipolar))],
    CI_more_morning_m[grepl("^upper", names(CI_bipolar))],
    CI_more_evening_m[grepl("^upper", names(CI_bipolar))],
    CI_definite_evening_m[grepl("^upper", names(CI_bipolar))],
    # Males - interaction model
    CI_definite_morning_int_m[grepl("^upper", names(CI_bipolar))],
    CI_more_morning_int_m[grepl("^upper", names(CI_bipolar))],
    CI_more_evening_int_m[grepl("^upper", names(CI_bipolar))],
    CI_definite_evening_int_m[grepl("^upper", names(CI_bipolar))]
  ),
  p_value = c(
    # Whole sample - main effects
    p_bipolar,
    p_more_morning,
    p_more_evening,
    p_definite_evening,
    # Whole sample - interaction model
    p_definite_morning_int,
    p_more_morning_int,
    p_more_evening_int,
    p_definite_evening_int,
    # Females - main effects
    p_bipolar_f,
    p_more_morning_f,
    p_more_evening_f,
    p_definite_evening_f,
    # Females - interaction model
    p_definite_morning_int_f,
    p_more_morning_int_f,
    p_more_evening_int_f,
    p_definite_evening_int_f,
    # Males - main effects
    p_bipolar_m,
    p_more_morning_m,
    p_more_evening_m,
    p_definite_evening_m,
    # Males - interaction model
    p_definite_morning_int_m,
    p_more_morning_int_m,
    p_more_evening_int_m,
    p_definite_evening_int_m
  )
)

logreg_summary <- logreg_summary1 %>%
  mutate(
    chronotype = if_else(
      chronotype == "Definite morning" & model == "Main effects",
      "Bipolar",
      chronotype))

#### Objective: Create a new logreg output in which the bipolar disorder coefficient is added to each chronotype output - do so manually to negate the need for a specific R package

### To run after 'logreg.r'

################ Whole sample - main effect (fit6) ######################
#generate objects
beta1 <- coef(fit6)
vc1 <- vcov(fit6)

#Bipolar disorder main effects
logOR_bipolar <-
  beta1["mhp_statusbipolar"]

SE_bipolar <- sqrt(vc1["mhp_statusbipolar", "mhp_statusbipolar"])

OR_bipolar <- exp(logOR_bipolar)

CI_bipolar <- c(
  lower = exp(logOR_bipolar - 1.96 * SE_bipolar),
  upper = exp(logOR_bipolar + 1.96 * SE_bipolar))

##Chronotype main effects
#More morning than evening
logOR_more_morning <- beta1["chronotypemore morning than evening"]

SE_more_morning <- sqrt(vc1["chronotypemore morning than evening", "chronotypemore morning than evening"])

OR_more_morning <- exp(logOR_more_morning)

CI_more_morning <- c(
  lower = exp(logOR_more_morning - 1.96 * SE_more_morning),
  upper = exp(logOR_more_morning + 1.96 * SE_more_morning))

#More evening than morning
logOR_more_evening <- beta1["chronotypemore evening than morning"]

SE_more_evening <- sqrt(vc1["chronotypemore evening than morning", "chronotypemore evening than morning"])

OR_more_evening <- exp(logOR_more_evening)

CI_more_evening <- c(
  lower = exp(logOR_more_evening - 1.96 * SE_more_evening),
  upper = exp(logOR_more_evening + 1.96 * SE_more_evening))

#Definite evening
logOR_definite_evening <- beta1["chronotypedefinite evening"]

SE_definite_evening <- sqrt(vc1["chronotypedefinite evening", "chronotypedefinite evening"])

OR_definite_evening <- exp(logOR_definite_evening)

CI_definite_evening <- c(
  lower = exp(logOR_definite_evening - 1.96 * SE_definite_evening),
  upper = exp(logOR_definite_evening + 1.96 * SE_definite_evening))



################ Whole sample - interaction model (fit8) ######################
#generate objects
beta2 <- coef(fit8)
vc2 <- vcov(fit8)

#Definite morning
logOR_definite_morning_int <-
  beta2["mhp_statusbipolar"]

SE_definite_morning_int <- sqrt(
  vc2["mhp_statusbipolar", "mhp_statusbipolar"])

OR_definite_morning_int <- exp(logOR_definite_morning_int)

CI_definite_morning_int <- c(
  lower = exp(logOR_definite_morning_int - 1.96 * SE_definite_morning_int),
  upper = exp(logOR_definite_morning_int + 1.96 * SE_definite_morning_int))

#More morning than evening
logOR_more_morning_int <-
  beta2["mhp_statusbipolar"] +
  beta2["chronotypemore morning than evening:mhp_statusbipolar"]

SE_more_morning_int <- sqrt(
  vc2["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc2["chronotypemore morning than evening:mhp_statusbipolar", "chronotypemore morning than evening:mhp_statusbipolar"] +
    2 * vc2["mhp_statusbipolar", "chronotypemore morning than evening:mhp_statusbipolar"])

OR_more_morning_int <- exp(logOR_more_morning_int)

CI_more_morning_int <- c(
  lower = exp(logOR_more_morning_int - 1.96 * SE_more_morning_int),
  upper = exp(logOR_more_morning_int + 1.96 * SE_more_morning_int))

#More evening than morning
logOR_more_evening_int <-
  beta2["mhp_statusbipolar"] +
  beta2["chronotypemore evening than morning:mhp_statusbipolar"]

SE_more_evening_int <- sqrt(
  vc2["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc2["chronotypemore evening than morning:mhp_statusbipolar", "chronotypemore evening than morning:mhp_statusbipolar"] +
    2 * vc2["mhp_statusbipolar", "chronotypemore evening than morning:mhp_statusbipolar"])

OR_more_evening_int <- exp(logOR_more_evening_int)

CI_more_evening_int <- c(
  lower = exp(logOR_more_evening_int - 1.96 * SE_more_evening_int),
  upper = exp(logOR_more_evening_int + 1.96 * SE_more_evening_int))

#Definite evening
logOR_definite_evening_int <-
  beta2["mhp_statusbipolar"] +
  beta2["chronotypedefinite evening:mhp_statusbipolar"]

SE_definite_evening_int <- sqrt(
  vc2["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc2["chronotypedefinite evening:mhp_statusbipolar", "chronotypedefinite evening:mhp_statusbipolar"] +
    2 * vc2["mhp_statusbipolar", "chronotypedefinite evening:mhp_statusbipolar"])

OR_definite_evening_int <- exp(logOR_definite_evening_int)

CI_definite_evening_int <- c(
  lower = exp(logOR_definite_evening_int - 1.96 * SE_definite_evening_int),
  upper = exp(logOR_definite_evening_int + 1.96 * SE_definite_evening_int))


############### Females - main effects (fit14) ####################
#generate objects
beta3 <- coef(fit14)
vc3 <- vcov(fit14)

#Bipolar disorder main effects
logOR_bipolar_f <-
  beta3["mhp_statusbipolar"]

SE_bipolar_f <- sqrt(vc3["mhp_statusbipolar", "mhp_statusbipolar"])

OR_bipolar_f <- exp(logOR_bipolar_f)

CI_bipolar_f <- c(
  lower = exp(logOR_bipolar_f - 1.96 * SE_bipolar_f),
  upper = exp(logOR_bipolar_f + 1.96 * SE_bipolar_f))

##Chronotype main effects
#More morning than evening
logOR_more_morning_f <- beta3["chronotypemore morning than evening"]

SE_more_morning_f <- sqrt(vc3["chronotypemore morning than evening", "chronotypemore morning than evening"])

OR_more_morning_f <- exp(logOR_more_morning_f)

CI_more_morning_f <- c(
  lower = exp(logOR_more_morning_f - 1.96 * SE_more_morning_f),
  upper = exp(logOR_more_morning_f + 1.96 * SE_more_morning_f))

#More evening than morning
logOR_more_evening_f <- beta3["chronotypemore evening than morning"]

SE_more_evening_f <- sqrt(vc3["chronotypemore evening than morning", "chronotypemore evening than morning"])

OR_more_evening_f <- exp(logOR_more_evening_f)

CI_more_evening_f <- c(
  lower = exp(logOR_more_evening_f - 1.96 * SE_more_evening_f),
  upper = exp(logOR_more_evening_f + 1.96 * SE_more_evening_f))

#Definite evening
logOR_definite_evening_f <- beta3["chronotypedefinite evening"]

SE_definite_evening_f <- sqrt(vc3["chronotypedefinite evening", "chronotypedefinite evening"])

OR_definite_evening_f <- exp(logOR_definite_evening_f)

CI_definite_evening_f <- c(
  lower = exp(logOR_definite_evening_f - 1.96 * SE_definite_evening_f),
  upper = exp(logOR_definite_evening_f + 1.96 * SE_definite_evening_f))



############## Females - interaction model (fit16) ######################
#generate objects
beta4 <- coef(fit16)
vc4 <- vcov(fit16)

#Definite morning
logOR_definite_morning_int_f <-
  beta4["mhp_statusbipolar"]

SE_definite_morning_int_f <- sqrt(
  vc4["mhp_statusbipolar", "mhp_statusbipolar"])

OR_definite_morning_int_f <- exp(logOR_definite_morning_int_f)

CI_definite_morning_int_f <- c(
  lower = exp(logOR_definite_morning_int_f - 1.96 * SE_definite_morning_int_f),
  upper = exp(logOR_definite_morning_int_f + 1.96 * SE_definite_morning_int_f))

#More morning than evening
logOR_more_morning_int_f <-
  beta4["mhp_statusbipolar"] +
  beta4["chronotypemore morning than evening:mhp_statusbipolar"]

SE_more_morning_int_f<- sqrt(
  vc4["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc4["chronotypemore morning than evening:mhp_statusbipolar", "chronotypemore morning than evening:mhp_statusbipolar"] +
    2 * vc4["mhp_statusbipolar", "chronotypemore morning than evening:mhp_statusbipolar"])

OR_more_morning_int_f <- exp(logOR_more_morning_int_f)

CI_more_morning_int_f <- c(
  lower = exp(logOR_more_morning_int_f - 1.96 * SE_more_morning_int_f),
  upper = exp(logOR_more_morning_int_f + 1.96 * SE_more_morning_int_f))

#More evening than morning
logOR_more_evening_int_f <-
  beta4["mhp_statusbipolar"] +
  beta4["chronotypemore evening than morning:mhp_statusbipolar"]

SE_more_evening_int_f <- sqrt(
  vc4["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc4["chronotypemore evening than morning:mhp_statusbipolar", "chronotypemore evening than morning:mhp_statusbipolar"] +
    2 * vc4["mhp_statusbipolar", "chronotypemore evening than morning:mhp_statusbipolar"])

OR_more_evening_int_f <- exp(logOR_more_evening_int_f)

CI_more_evening_int_f <- c(
  lower = exp(logOR_more_evening_int_f - 1.96 * SE_more_evening_int_f),
  upper = exp(logOR_more_evening_int_f + 1.96 * SE_more_evening_int_f))

#Definite evening
logOR_definite_evening_int_f <-
  beta4["mhp_statusbipolar"] +
  beta4["chronotypedefinite evening:mhp_statusbipolar"]

SE_definite_evening_int_f <- sqrt(
  vc4["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc4["chronotypedefinite evening:mhp_statusbipolar", "chronotypedefinite evening:mhp_statusbipolar"] +
    2 * vc4["mhp_statusbipolar", "chronotypedefinite evening:mhp_statusbipolar"])

OR_definite_evening_int_f <- exp(logOR_definite_evening_int_f)

CI_definite_evening_int_f <- c(
  lower = exp(logOR_definite_evening_int_f - 1.96 * SE_definite_evening_int_f),
  upper = exp(logOR_definite_evening_int_f + 1.96 * SE_definite_evening_int_f))



############### Males - main effects (fit18) ####################
#generate objects
beta5 <- coef(fit18)
vc5 <- vcov(fit18)

#Bipolar disorder main effects
logOR_bipolar_m <-
  beta5["mhp_statusbipolar"]

SE_bipolar_m <- sqrt(vc5["mhp_statusbipolar", "mhp_statusbipolar"])

OR_bipolar_m <- exp(logOR_bipolar_m)

CI_bipolar_m <- c(
  lower = exp(logOR_bipolar_m - 1.96 * SE_bipolar_m),
  upper = exp(logOR_bipolar_m + 1.96 * SE_bipolar_m))

##Chronotype main effects
#More morning than evening
logOR_more_morning_m <- beta5["chronotypemore morning than evening"]

SE_more_morning_m <- sqrt(vc5["chronotypemore morning than evening", "chronotypemore morning than evening"])

OR_more_morning_m <- exp(logOR_more_morning_m)

CI_more_morning_m <- c(
  lower = exp(logOR_more_morning_m - 1.96 * SE_more_morning_m),
  upper = exp(logOR_more_morning_m + 1.96 * SE_more_morning_m))

#More evening than morning
logOR_more_evening_m <- beta5["chronotypemore evening than morning"]

SE_more_evening_m <- sqrt(vc5["chronotypemore evening than morning", "chronotypemore evening than morning"])

OR_more_evening_m <- exp(logOR_more_evening_m)

CI_more_evening_m <- c(
  lower = exp(logOR_more_evening_m - 1.96 * SE_more_evening_m),
  upper = exp(logOR_more_evening_m + 1.96 * SE_more_evening_m))

#Definite evening
logOR_definite_evening_m <- beta5["chronotypedefinite evening"]

SE_definite_evening_m <- sqrt(vc5["chronotypedefinite evening", "chronotypedefinite evening"])

OR_definite_evening_m <- exp(logOR_definite_evening_m)

CI_definite_evening_m <- c(
  lower = exp(logOR_definite_evening_m - 1.96 * SE_definite_evening_m),
  upper = exp(logOR_definite_evening_m + 1.96 * SE_definite_evening_m))


############## Males - interaction model (fit20) ######################
#generate objects
beta6 <- coef(fit20)
vc6 <- vcov(fit20)

#Definite morning
logOR_definite_morning_int_m <-
  beta6["mhp_statusbipolar"]

SE_definite_morning_int_m <- sqrt(
  vc6["mhp_statusbipolar", "mhp_statusbipolar"])

OR_definite_morning_int_m <- exp(logOR_definite_morning_int_m)

CI_definite_morning_int_m <- c(
  lower = exp(logOR_definite_morning_int_m - 1.96 * SE_definite_morning_int_m),
  upper = exp(logOR_definite_morning_int_m + 1.96 * SE_definite_morning_int_m))

#More morning than evening
logOR_more_morning_int_m <-
  beta6["mhp_statusbipolar"] +
  beta6["chronotypemore morning than evening:mhp_statusbipolar"]

SE_more_morning_int_m<- sqrt(
  vc6["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc6["chronotypemore morning than evening:mhp_statusbipolar", "chronotypemore morning than evening:mhp_statusbipolar"] +
    2 * vc6["mhp_statusbipolar", "chronotypemore morning than evening:mhp_statusbipolar"])

OR_more_morning_int_m <- exp(logOR_more_morning_int_m)

CI_more_morning_int_m <- c(
  lower = exp(logOR_more_morning_int_m - 1.96 * SE_more_morning_int_m),
  upper = exp(logOR_more_morning_int_m + 1.96 * SE_more_morning_int_m))

#More evening than morning
logOR_more_evening_int_m <-
  beta6["mhp_statusbipolar"] +
  beta6["chronotypemore evening than morning:mhp_statusbipolar"]

SE_more_evening_int_m <- sqrt(
  vc6["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc6["chronotypemore evening than morning:mhp_statusbipolar", "chronotypemore evening than morning:mhp_statusbipolar"] +
    2 * vc6["mhp_statusbipolar", "chronotypemore evening than morning:mhp_statusbipolar"])

OR_more_evening_int_m <- exp(logOR_more_evening_int_m)

CI_more_evening_int_m <- c(
  lower = exp(logOR_more_evening_int_m - 1.96 * SE_more_evening_int_m),
  upper = exp(logOR_more_evening_int_m + 1.96 * SE_more_evening_int_m))

#Definite evening
logOR_definite_evening_int_m <-
  beta6["mhp_statusbipolar"] +
  beta6["chronotypedefinite evening:mhp_statusbipolar"]

SE_definite_evening_int_m <- sqrt(
  vc6["mhp_statusbipolar", "mhp_statusbipolar"] +
    vc6["chronotypedefinite evening:mhp_statusbipolar", "chronotypedefinite evening:mhp_statusbipolar"] +
    2 * vc6["mhp_statusbipolar", "chronotypedefinite evening:mhp_statusbipolar"])

OR_definite_evening_int_m <- exp(logOR_definite_evening_int_m)

CI_definite_evening_int_m <- c(
  lower = exp(logOR_definite_evening_int_m - 1.96 * SE_definite_evening_int_m),
  upper = exp(logOR_definite_evening_int_m + 1.96 * SE_definite_evening_int_m))


### Generate p-values of each estimate ###
p_bipolar <- 2 * (1 - pnorm(abs(logOR_bipolar / SE_bipolar)))
p_more_morning <- 2 * (1 - pnorm(abs(logOR_more_morning / SE_more_morning)))
p_more_evening <- 2 * (1 - pnorm(abs(logOR_more_evening / SE_more_evening)))
p_definite_evening <- 2 * (1 - pnorm(abs(logOR_definite_evening / SE_definite_evening)))
p_definite_morning_int <- 2 * (1 - pnorm(abs(logOR_definite_morning_int / SE_definite_morning_int)))
p_more_morning_int <- 2 * (1 - pnorm(abs(logOR_more_morning_int / SE_more_morning_int)))
p_more_evening_int <- 2 * (1 - pnorm(abs(logOR_more_evening_int / SE_more_evening_int)))
p_definite_evening_int <- 2 * (1 - pnorm(abs(logOR_definite_evening_int / SE_definite_evening_int)))

p_bipolar_f <- 2 * (1 - pnorm(abs(logOR_bipolar_f / SE_bipolar_f)))
p_more_morning_f <- 2 * (1 - pnorm(abs(logOR_more_morning_f / SE_more_morning_f)))
p_more_evening_f <- 2 * (1 - pnorm(abs(logOR_more_evening_f / SE_more_evening_f)))
p_definite_evening_f <- 2 * (1 - pnorm(abs(logOR_definite_evening_f / SE_definite_evening_f)))
p_definite_morning_int_f <- 2 * (1 - pnorm(abs(logOR_definite_morning_int_f / SE_definite_morning_int_f)))
p_more_morning_int_f <- 2 * (1 - pnorm(abs(logOR_more_morning_int_f / SE_more_morning_int_f)))
p_more_evening_int_f <- 2 * (1 - pnorm(abs(logOR_more_evening_int_f / SE_more_evening_int_f)))
p_definite_evening_int_f <- 2 * (1 - pnorm(abs(logOR_definite_evening_int_f / SE_definite_evening_int_f)))

p_bipolar_m <- 2 * (1 - pnorm(abs(logOR_bipolar_m / SE_bipolar_m)))
p_more_morning_m <- 2 * (1 - pnorm(abs(logOR_more_morning_m / SE_more_morning_m)))
p_more_evening_m <- 2 * (1 - pnorm(abs(logOR_more_evening_m / SE_more_evening_m)))
p_definite_evening_m <- 2 * (1 - pnorm(abs(logOR_definite_evening_m / SE_definite_evening_m)))
p_definite_morning_int_m <- 2 * (1 - pnorm(abs(logOR_definite_morning_int_m / SE_definite_morning_int_m)))
p_more_morning_int_m <- 2 * (1 - pnorm(abs(logOR_more_morning_int_m / SE_more_morning_int_m)))
p_more_evening_int_m <- 2 * (1 - pnorm(abs(logOR_more_evening_int_m / SE_more_evening_int_m)))
p_definite_evening_int_m <- 2 * (1 - pnorm(abs(logOR_definite_evening_int_m / SE_definite_evening_int_m)))


## Calculate sample size of each model ##
n_fit6  <- nobs(fit6)
n_fit8  <- nobs(fit8)
n_fit14 <- nobs(fit14)
n_fit16 <- nobs(fit16)
n_fit18 <- nobs(fit18)
n_fit20 <- nobs(fit20)


### Combine all outputs in one table ###
logreg_summary1 <- data.frame(
  sex = c(
    rep("Both", 4),
    rep("Both", 4),
    rep("Female", 4),
    rep("Female", 4),
    rep("Male", 4),
    rep("Male", 4)
  ),
  model = c(
    rep("Main effects", 4),
    rep("Interaction", 4),
    rep("Main effects", 4),
    rep("Interaction", 4),
    rep("Main effects", 4),
    rep("Interaction", 4)
  ),
  chronotype = rep(
    c(
      "Definite morning",
      "More morning than evening",
      "More evening than morning",
      "Definite evening"
    ),
    6
  ),
  N = c(
    rep(n_fit6, 4),
    rep(n_fit8, 4),
    rep(n_fit14, 4),
    rep(n_fit16, 4),
    rep(n_fit18, 4),
    rep(n_fit20, 4)
  ),
  OR = c(
    # Whole sample - main effects
    OR_bipolar,
    OR_more_morning,
    OR_more_evening,
    OR_definite_evening,
    # Whole sample - interaction model
    OR_definite_morning_int,
    OR_more_morning_int,
    OR_more_evening_int,
    OR_definite_evening_int,
    # Females - main effects
    OR_bipolar_f,
    OR_more_morning_f,
    OR_more_evening_f,
    OR_definite_evening_f,
    # Females - interaction model
    OR_definite_morning_int_f,
    OR_more_morning_int_f,
    OR_more_evening_int_f,
    OR_definite_evening_int_f,
    # Males - main effects
    OR_bipolar_m,
    OR_more_morning_m,
    OR_more_evening_m,
    OR_definite_evening_m,
    # Males - interaction model
    OR_definite_morning_int_m,
    OR_more_morning_int_m,
    OR_more_evening_int_m,
    OR_definite_evening_int_m),
  CI_lower = c(
    # Whole sample - main effects
    CI_bipolar[grepl("^lower", names(CI_bipolar))],
    CI_more_morning[grepl("^lower", names(CI_bipolar))],
    CI_more_evening[grepl("^lower", names(CI_bipolar))],
    CI_definite_evening[grepl("^lower", names(CI_bipolar))],
    # Whole sample - interaction model
    CI_definite_morning_int[grepl("^lower", names(CI_bipolar))],
    CI_more_morning_int[grepl("^lower", names(CI_bipolar))],
    CI_more_evening_int[grepl("^lower", names(CI_bipolar))],
    CI_definite_evening_int[grepl("^lower", names(CI_bipolar))],
    # Females - main effects
    CI_bipolar_f[grepl("^lower", names(CI_bipolar))],
    CI_more_morning_f[grepl("^lower", names(CI_bipolar))],
    CI_more_evening_f[grepl("^lower", names(CI_bipolar))],
    CI_definite_evening_f[grepl("^lower", names(CI_bipolar))],
    # Females - interaction model
    CI_definite_morning_int_f[grepl("^lower", names(CI_bipolar))],
    CI_more_morning_int_f[grepl("^lower", names(CI_bipolar))],
    CI_more_evening_int_f[grepl("^lower", names(CI_bipolar))],
    CI_definite_evening_int_f[grepl("^lower", names(CI_bipolar))],
    # Males - main effects
    CI_bipolar_m[grepl("^lower", names(CI_bipolar))],
    CI_more_morning_m[grepl("^lower", names(CI_bipolar))],
    CI_more_evening_m[grepl("^lower", names(CI_bipolar))],
    CI_definite_evening_m[grepl("^lower", names(CI_bipolar))],
    # Males - interaction model
    CI_definite_morning_int_m[grepl("^lower", names(CI_bipolar))],
    CI_more_morning_int_m[grepl("^lower", names(CI_bipolar))],
    CI_more_evening_int_m[grepl("^lower", names(CI_bipolar))],
    CI_definite_evening_int_m[grepl("^lower", names(CI_bipolar))]
  ),
  CI_upper = c(
    # Whole sample - main effects
    CI_bipolar[grepl("^upper", names(CI_bipolar))],
    CI_more_morning[grepl("^upper", names(CI_bipolar))],
    CI_more_evening[grepl("^upper", names(CI_bipolar))],
    CI_definite_evening[grepl("^upper", names(CI_bipolar))],
    # Whole sample - interaction model
    CI_definite_morning_int[grepl("^upper", names(CI_bipolar))],
    CI_more_morning_int[grepl("^upper", names(CI_bipolar))],
    CI_more_evening_int[grepl("^upper", names(CI_bipolar))],
    CI_definite_evening_int[grepl("^upper", names(CI_bipolar))],
    # Females - main effects
    CI_bipolar_f[grepl("^upper", names(CI_bipolar))],
    CI_more_morning_f[grepl("^upper", names(CI_bipolar))],
    CI_more_evening_f[grepl("^upper", names(CI_bipolar))],
    CI_definite_evening_f[grepl("^upper", names(CI_bipolar))],
    # Females - interaction model
    CI_definite_morning_int_f[grepl("^upper", names(CI_bipolar))],
    CI_more_morning_int_f[grepl("^upper", names(CI_bipolar))],
    CI_more_evening_int_f[grepl("^upper", names(CI_bipolar))],
    CI_definite_evening_int_f[grepl("^upper", names(CI_bipolar))],
    # Males - main effects
    CI_bipolar_m[grepl("^upper", names(CI_bipolar))],
    CI_more_morning_m[grepl("^upper", names(CI_bipolar))],
    CI_more_evening_m[grepl("^upper", names(CI_bipolar))],
    CI_definite_evening_m[grepl("^upper", names(CI_bipolar))],
    # Males - interaction model
    CI_definite_morning_int_m[grepl("^upper", names(CI_bipolar))],
    CI_more_morning_int_m[grepl("^upper", names(CI_bipolar))],
    CI_more_evening_int_m[grepl("^upper", names(CI_bipolar))],
    CI_definite_evening_int_m[grepl("^upper", names(CI_bipolar))]
  ),
  p_value = c(
    # Whole sample - main effects
    p_bipolar,
    p_more_morning,
    p_more_evening,
    p_definite_evening,
    # Whole sample - interaction model
    p_definite_morning_int,
    p_more_morning_int,
    p_more_evening_int,
    p_definite_evening_int,
    # Females - main effects
    p_bipolar_f,
    p_more_morning_f,
    p_more_evening_f,
    p_definite_evening_f,
    # Females - interaction model
    p_definite_morning_int_f,
    p_more_morning_int_f,
    p_more_evening_int_f,
    p_definite_evening_int_f,
    # Males - main effects
    p_bipolar_m,
    p_more_morning_m,
    p_more_evening_m,
    p_definite_evening_m,
    # Males - interaction model
    p_definite_morning_int_m,
    p_more_morning_int_m,
    p_more_evening_int_m,
    p_definite_evening_int_m
  )
)

logreg_summary <- logreg_summary1 %>%
  mutate(
    chronotype = if_else(
      chronotype == "Definite morning" & model == "Main effects",
      "Bipolar",
      chronotype))

write.csv(logreg_summary, "logreg_summary.csv", row.names = FALSE)
