###### fully time dependent model, no age structure####
library(RMark)

# 1. Load data (from .inp file or dataframe)
data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)

# 2. Process multistate data (strata = states)
proc <- process.data(data, model = "Multistrata", 
                     strata.labels = c("1", "2"))  # 1 = nonbreeder, 2 = breeder

# 3. Generate default design data
ddl <- make.design.data(proc)

# 4. Constrain transitions from breeder (state "2") to nonbreeder (state "1") to zero
ddl$Psi$fix <- with(ddl$Psi, ifelse(stratum == "2" & tostratum == "1", 0, NA))

# 5. Define the model with time-dependent parameters
model.list <- list(
  S = list(formula = ~stratum * time),
  p = list(formula = ~stratum * time),
  Psi = list(formula = ~ time)
)

# 6. Fit the model
fit.constrained <- mark(
  data = proc,
  ddl = ddl,
  model.parameters = model.list,
  output = FALSE
)

# 7. View model results
summary(fit.constrained)

# save results
real_estimates = fit.constrained$results$real
real_estimates <- data.frame(Parameter=rownames(real_estimates), real_estimates)
library(writexl)
write_xlsx(real_estimates, "real_estimates.xlsx")



######Sgt.Pgt.PSIta#############
library(RMark)

# ---- 1. Read your input .inp file
data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)

# ---- 2. Process the data for multistate model
ms.processed <- process.data(data, model = "Multistrata", 
                             strata.labels = c("1","2"))  # 1=NB, 2=B

# ---- 3. Make design data
ddl <- make.design.data(ms.processed)

# ---- 4. Fix impossible transitions (2→1) to 0
ddl$Psi$fix[ddl$Psi$stratum == "2" & ddl$Psi$tostratum == "1"] <- 0

# ---- 5. Collapse ages into 4 bins (or use provided cutpoints)
ddl$Psi$ageclass <- cut(ddl$Psi$Age, 
                        breaks = c(0,2,3,4,100), 
                        labels = c("1","2","3","4"), right = FALSE)

# ---- 6. Model formula list
model.list1 <- list(
  S = list(formula = ~ stratum + time),         # Survival: state + time
  p = list(formula = ~ stratum + time),         # Recapture: state + time
  Psi = list(formula = ~ time + ageclass)       # Transition: time + age
)

# ---- 7. Fit the model
fit1 <- mark(ms.processed, ddl, model.parameters = model.list1)

# ---- 8. View results
summary(fit1)

# save results
real_estimates1 = fit1$results$real
real_estimates1 <- data.frame(Parameter=rownames(real_estimates1), real_estimates1)
library(writexl)
write_xlsx(real_estimates1, "real_estimates1.xlsx")



#####baseline model#####
library(RMark)

# Load data
data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)

# Process data
ms.processed <- process.data(data, model = "Multistrata", strata.labels = c("1","2"))
ddl <- make.design.data(ms.processed)

# Fix impossible transitions
ddl$Psi$fix[ddl$Psi$stratum=="2" & ddl$Psi$tostratum=="1"] <- 0

# Model
model.const <- list(
  S   = list(formula = ~stratum),
  p   = list(formula = ~stratum),
  Psi = list(formula = ~1)
)
fit.const <- mark(ms.processed, ddl, model.parameters = model.const)

# save results
real_estimates2 = fit.const$results$real
real_estimates2 <- data.frame(Parameter=rownames(real_estimates2), real_estimates2)
library(writexl)
write_xlsx(real_estimates2, "real_estimates2.xlsx")



#####simpler additive model-only psi has age effect#####
library(RMark)

data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)
ms.processed <- process.data(data, model = "Multistrata", strata.labels = c("1","2"))
ddl <- make.design.data(ms.processed)

# Fix breeder → nonbreeder to 0
ddl$Psi$fix[ddl$Psi$stratum=="2" & ddl$Psi$tostratum=="1"] <- 0

# Create age classes for NB→B transitions
ddl$Psi$ageclass <- cut(ddl$Psi$Age,
                        breaks = c(0,2,3,4,100),
                        labels = c("1","2","3","4"),
                        right = FALSE)

# Model
model.Psiage <- list(
  S   = list(formula = ~stratum + time),
  p   = list(formula = ~stratum + time),
  Psi = list(formula = ~ageclass)
)
fit.Psiage <- mark(ms.processed, ddl, model.parameters = model.Psiage)

# save results
real_estimates3 = fit.Psiage$results$real
real_estimates3 <- data.frame(Parameter=rownames(real_estimates3), real_estimates3)
library(writexl)
write_xlsx(real_estimates3, "real_estimates3.xlsx")

#####survival->age class for nonbreeders####
library(RMark)

data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)
ms.processed <- process.data(data, model = "Multistrata", strata.labels = c("1","2"))
ddl <- make.design.data(ms.processed)

# Fix breeder → nonbreeder to 0
ddl$Psi$fix[ddl$Psi$stratum=="2" & ddl$Psi$tostratum=="1"] <- 0

# NB survival: 2 age classes
ddl$S$ageclass2 <- cut(ddl$S$Age,
                       breaks = c(0,2,100),
                       labels = c("young","older"),
                       right = FALSE)

# NB→B transitions: 4 age classes
ddl$Psi$ageclass <- cut(ddl$Psi$Age,
                        breaks = c(0,2,3,4,100),
                        labels = c("1","2","3","4"),
                        right = FALSE)


# Model: NB survival depends on ageclass2, B survival varies by time
model.ageNB <- list(
  S   = list(formula = ~ I(stratum=="1")*ageclass2 + I(stratum=="2")*time),
  p   = list(formula = ~stratum + time),
  Psi = list(formula = ~ageclass)
)

fit.ageNB <- mark(ms.processed, ddl, model.parameters = model.ageNB)
<<<<<<< HEAD

# ---- 8. View results
summary(fit.ageNB)

# save results
real_estimatesNB = fit.ageNB$results$real
real_estimatesNB <- data.frame(Parameter=rownames(real_estimatesNB), real_estimatesNB)
library(writexl)
write_xlsx(real_estimatesNB, "real_estimatesNB.xlsx")

##### survival varying for nonbreeder and constant for breeders#####
library(RMark)

data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)
ms.processed <- process.data(data, model = "Multistrata", strata.labels = c("1","2"))
ddl <- make.design.data(ms.processed)

# Fix breeder → nonbreeder to 0
ddl$Psi$fix[ddl$Psi$stratum=="2" & ddl$Psi$tostratum=="1"] <- 0

# NB survival: 2 age classes
ddl$S$ageclass2 <- cut(ddl$S$Age,
                       breaks = c(0,2,100),
                       labels = c("young","older"),
                       right = FALSE)

# NB→B transitions: 4 age classes
ddl$Psi$ageclass <- cut(ddl$Psi$Age,
                        breaks = c(0,2,3,4,100),
                        labels = c("1","2","3","4"),
                        right = FALSE)


# Model: NB survival depends on ageclass2, B survival varies by time
model.ageNB2 <- list(
  S   = list(formula = ~I(stratum=="1")*(ageclass2 + time) + I(stratum=="2")*time),
  p   = list(formula = ~stratum + time),
  Psi = list(formula = ~ageclass)
)

fit.ageNB2 <- mark(ms.processed, ddl, model.parameters = model.ageNB2)

# save results
real_estimatesNB2 = fit.ageNB2$results$real
real_estimatesNB2 <- data.frame(Parameter=rownames(real_estimatesNB2), real_estimatesNB2)
library(writexl)
write_xlsx(real_estimatesNB2, "real_estimatesNB2.xlsx")

#####AICs for the above 6 models#####
all.models <- collect.models()
aic.table <- all.models$model.table

write_xlsx(aic.table, "AICc_results.xlsx")



