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



#####stratum,stratum,age CHECK SE!!!!!1#####
library(RMark)

data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)
ms.processed <- process.data(data, model = "Multistrata", strata.labels = c("1","2"))
ddl <- make.design.data(ms.processed)

# Fix breeder → nonbreeder to 0
ddl$Psi$fix[ddl$Psi$stratum=="2" & ddl$Psi$tostratum=="1"] <- 0

# Create age classes for NB→B transitions
ddl$Psi$ageclass <- cut(ddl$Psi$Age,
                        breaks = c(0,1,2,3,100),
                        labels = c("1","2","3","4"),
                        right = FALSE)

# Model
model.ssPsiage <- list(
  S   = list(formula = ~stratum ),
  p   = list(formula = ~stratum ),
  Psi = list(formula = ~ageclass)
)
fit.ssPsiage <- mark(ms.processed, ddl, model.parameters = model.ssPsiage)

# save results
real_estimates3ss = fit.ssPsiage$results$real
real_estimates3ss <- data.frame(Parameter=rownames(real_estimates3ss), real_estimates3ss)
library(writexl)
write_xlsx(real_estimates3ss, "real_estimates3ss.xlsx")
#####constant survival by state and psi ageclass#####
library(RMark)

data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)
ms.processed <- process.data(data, model = "Multistrata", strata.labels = c("1","2"))
ddl <- make.design.data(ms.processed)

# Fix breeder → nonbreeder to 0
ddl$Psi$fix[ddl$Psi$stratum=="2" & ddl$Psi$tostratum=="1"] <- 0

# Create age classes for NB→B transitions
ddl$Psi$ageclass <- cut(ddl$Psi$Age,
                        breaks = c(0,1,2,3,100),
                        labels = c("1","2","3","4"),
                        right = FALSE)

# Model
model.constantPsiage <- list(
  S   = list(formula = ~stratum),
  p   = list(formula = ~stratum + time),
  Psi = list(formula = ~ageclass)
)
fit.constantPsiage <- mark(ms.processed, ddl, model.parameters = model.constantPsiage)

# save results
real_estimates4 = fit.constantPsiage$results$real
real_estimates4 <- data.frame(Parameter=rownames(real_estimates4), real_estimates4)
library(writexl)
write_xlsx(real_estimates4, "real_estimates4.xlsx")
#####simpler additive model-only psi has age effect#####
library(RMark)

data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)
ms.processed <- process.data(data, model = "Multistrata", strata.labels = c("1","2"))
ddl <- make.design.data(ms.processed)

# Fix breeder → nonbreeder to 0
ddl$Psi$fix[ddl$Psi$stratum=="2" & ddl$Psi$tostratum=="1"] <- 0

# Create age classes for NB→B transitions
ddl$Psi$ageclass <- cut(ddl$Psi$Age,
                        breaks = c(0,1,2,3,100),
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

#####only NB survival varies#####
library(RMark)

data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)
ms.processed <- process.data(data, model = "Multistrata", strata.labels = c("1","2"))
ddl <- make.design.data(ms.processed)

# Fix breeder → nonbreeder to 0
ddl$Psi$fix[ddl$Psi$stratum=="2" & ddl$Psi$tostratum=="1"] <- 0

# NB survival: 2 age classes
# Step 1: Create ageclass2 as a character
ddl$S$ageclass2 <- as.character(cut(ddl$S$Age,
                                    breaks = c(0,1,100),
                                    labels = c("young","older"),
                                    right = FALSE))

# Step 2: Assign "none" to breeders
ddl$S$ageclass2[ddl$S$stratum == "2"] <- "none"

# Step 3: Convert back to factor with all three levels
ddl$S$ageclass2 <- factor(ddl$S$ageclass2,
                          levels = c("young","older","none"))

# NB→B transitions: 4 age classes
ddl$Psi$ageclass <- cut(ddl$Psi$Age,
                        breaks = c(0,1,2,3,100),
                        labels = c("1","2","3","4"),
                        right = FALSE)


# Model: NB survival depends on ageclass2, B survival varies by time
model.ageNB4 <- list(
  S   = list(formula = ~ I(stratum=="1")*ageclass2 + I(stratum=="2")),
  p   = list(formula = ~stratum + time),
  Psi = list(formula = ~ageclass)
)

fit.ageNB4 <- mark(ms.processed, ddl, model.parameters = model.ageNB4)

# ---- 8. View results
summary(fit.ageNB4)

# save results
real_estimatesNB4 = fit.ageNB4$results$real
real_estimatesNB4 <- data.frame(Parameter=rownames(real_estimatesNB4), real_estimatesNB4)
library(writexl)
write_xlsx(real_estimatesNB4, "real_estimatesNB4.xlsx")

#####survival for NB: age effect, B: only time####
library(RMark)

data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)
ms.processed <- process.data(data, model = "Multistrata", strata.labels = c("1","2"))
ddl <- make.design.data(ms.processed)

# Fix breeder → nonbreeder to 0
ddl$Psi$fix[ddl$Psi$stratum=="2" & ddl$Psi$tostratum=="1"] <- 0

# NB survival: 2 age classes
# Step 1: Create ageclass2 as a character
ddl$S$ageclass2 <- as.character(cut(ddl$S$Age,
                                    breaks = c(0,1,100),
                                    labels = c("young","older"),
                                    right = FALSE))

# Step 2: Assign "none" to breeders
ddl$S$ageclass2[ddl$S$stratum == "2"] <- "none"

# Step 3: Convert back to factor with all three levels
ddl$S$ageclass2 <- factor(ddl$S$ageclass2,
                          levels = c("young","older","none"))

# NB→B transitions: 4 age classes
ddl$Psi$ageclass <- cut(ddl$Psi$Age,
                        breaks = c(0,1,2,3,100),
                        labels = c("1","2","3","4"),
                        right = FALSE)


# Model: NB survival depends on ageclass2, B survival varies by time
model.ageNB <- list(
  S   = list(formula = ~ I(stratum=="1")*ageclass2 + I(stratum=="2")*time),
  p   = list(formula = ~stratum + time),
  Psi = list(formula = ~ageclass)
)

fit.ageNB <- mark(ms.processed, ddl, model.parameters = model.ageNB)

# ---- 8. View results
summary(fit.ageNB)

# save results
real_estimatesNB = fit.ageNB$results$real
real_estimatesNB <- data.frame(Parameter=rownames(real_estimatesNB), real_estimatesNB)
library(writexl)
write_xlsx(real_estimatesNB, "real_estimatesNB.xlsx")

##### survival for NB: time and age and their interactions, B: only time (Additive, can't be interactive)#####
library(RMark)

data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)
ms.processed <- process.data(data, model = "Multistrata", strata.labels = c("1","2"))
ddl <- make.design.data(ms.processed)

# Fix breeder → nonbreeder to 0
ddl$Psi$fix[ddl$Psi$stratum=="2" & ddl$Psi$tostratum=="1"] <- 0

# NB survival: 2 age classes
# Step 1: Create ageclass2 as a character
ddl$S$ageclass2 <- as.character(cut(ddl$S$Age,
                                    breaks = c(0,1,100),
                                    labels = c("young","older"),
                                    right = FALSE))

# Step 2: Assign "none" to breeders
ddl$S$ageclass2[ddl$S$stratum == "2"] <- "none"

# Step 3: Convert back to factor with all three levels
ddl$S$ageclass2 <- factor(ddl$S$ageclass2,
                          levels = c("young","older","none"))


# NB→B transitions: 4 age classes
ddl$Psi$ageclass <- cut(ddl$Psi$Age,
                        breaks = c(0,1,2,3,100),
                        labels = c("1","2","3","4"),
                        right = FALSE)


# Model: NB survival depends on ageclass2, B survival varies by time
model.ageNB2 <- list(
  S   = list(formula = ~stratum*time + I(stratum=="1")*(ageclass2)),
  p   = list(formula = ~stratum + time),
  Psi = list(formula = ~ageclass)
)

fit.ageNB2 <- mark(ms.processed, ddl, model.parameters = model.ageNB2)

# save results
real_estimatesNB2 = fit.ageNB2$results$real
real_estimatesNB2 <- data.frame(Parameter=rownames(real_estimatesNB2), real_estimatesNB2)
library(writexl)
write_xlsx(real_estimatesNB2, "real_estimatesNB2.xlsx")


#####survival only varies by time CHECK SE!!!!!! #####
library(RMark)

data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)
ms.processed <- process.data(data, model = "Multistrata", strata.labels = c("1","2"))
ddl <- make.design.data(ms.processed)

# Fix breeder → nonbreeder to 0
ddl$Psi$fix[ddl$Psi$stratum=="2" & ddl$Psi$tostratum=="1"] <- 0


# NB→B transitions: 4 age classes
ddl$Psi$ageclass <- cut(ddl$Psi$Age,
                        breaks = c(0,1,2,3,100),
                        labels = c("1","2","3","4"),
                        right = FALSE)


# Model: NB survival depends on ageclass2, B survival varies by time
#check standard errors of this !!!!!!!
model.ageNB5 <- list(
  S   = list(formula = ~I(stratum=="1")*time + I(stratum=="2")*time),
  p   = list(formula = ~stratum + time),
  Psi = list(formula = ~ageclass)
)

fit.ageNB5 <- mark(ms.processed, ddl, model.parameters = model.ageNB5)

# save results
real_estimatesNB5 = fit.ageNB5$results$real
real_estimatesNB5 <- data.frame(Parameter=rownames(real_estimatesNB5), real_estimatesNB5)
library(writexl)
write_xlsx(real_estimatesNB5, "real_estimatesNB5.xlsx")

##### NB2 but recapture has no time effect #####
library(RMark)

data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)
ms.processed <- process.data(data, model = "Multistrata", strata.labels = c("1","2"))
ddl <- make.design.data(ms.processed)

# Fix breeder → nonbreeder to 0
ddl$Psi$fix[ddl$Psi$stratum=="2" & ddl$Psi$tostratum=="1"] <- 0

# NB survival: 2 age classes
# Step 1: Create ageclass2 as a character
ddl$S$ageclass2 <- as.character(cut(ddl$S$Age,
                                    breaks = c(0,1,100),
                                    labels = c("young","older"),
                                    right = FALSE))

# Step 2: Assign "none" to breeders
ddl$S$ageclass2[ddl$S$stratum == "2"] <- "none"

# Step 3: Convert back to factor with all three levels
ddl$S$ageclass2 <- factor(ddl$S$ageclass2,
                          levels = c("young","older","none"))


# NB→B transitions: 4 age classes
ddl$Psi$ageclass <- cut(ddl$Psi$Age,
                        breaks = c(0,1,2,3,100),
                        labels = c("1","2","3","4"),
                        right = FALSE)


# Model: NB survival depends on ageclass2, B survival varies by time
model.ageNB3 <- list(
  S   = list(formula = ~stratum*time + I(stratum=="1")*(ageclass2)),
  p   = list(formula = ~stratum),
  Psi = list(formula = ~ageclass)
)

fit.ageNB3 <- mark(ms.processed, ddl, model.parameters = model.ageNB3)

# save results
real_estimatesNB3 = fit.ageNB3$results$real
real_estimatesNB3 <- data.frame(Parameter=rownames(real_estimatesNB3), real_estimatesNB3)
library(writexl)
write_xlsx(real_estimatesNB3, "real_estimatesNB3.xlsx")





#####NB2 with age*time CHECK SE!!!!!!######
library(RMark)

data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)
ms.processed <- process.data(data, model = "Multistrata", strata.labels = c("1","2"))
ddl <- make.design.data(ms.processed)

# Fix breeder → nonbreeder to 0
ddl$Psi$fix[ddl$Psi$stratum=="2" & ddl$Psi$tostratum=="1"] <- 0

# NB survival: 2 age classes
# Step 1: Create ageclass2 as a character
ddl$S$ageclass2 <- as.character(cut(ddl$S$Age,
                                    breaks = c(0,1,100),
                                    labels = c("young","older"),
                                    right = FALSE))

# Step 2: Assign "none" to breeders
ddl$S$ageclass2[ddl$S$stratum == "2"] <- "none"

# Step 3: Convert back to factor with all three levels
ddl$S$ageclass2 <- factor(ddl$S$ageclass2,
                          levels = c("young","older","none"))


# NB→B transitions: 4 age classes
ddl$Psi$ageclass <- cut(ddl$Psi$Age,
                        breaks = c(0,1,2,3,100),
                        labels = c("1","2","3","4"),
                        right = FALSE)


# Model: NB survival depends on ageclass2, B survival varies by time
# CHECK STANDARD ERRORS!!!!!!! 
model.ageNB6 <- list(
  S   = list(formula = ~I(stratum=="1")*(ageclass2 * time) + I(stratum=="2")*time),
  p   = list(formula = ~stratum + time),
  Psi = list(formula = ~ageclass)
)

fit.ageNB6 <- mark(ms.processed, ddl, model.parameters = model.ageNB6)

# save results
real_estimatesNB6 = fit.ageNB6$results$real
real_estimatesNB6 <- data.frame(Parameter=rownames(real_estimatesNB6), real_estimatesNB6)
library(writexl)
write_xlsx(real_estimatesNB6, "real_estimatesNB6.xlsx")

#####AICs for the above 6 models#####
all.models <- collect.models()
aic.table <- all.models$model.table

write_xlsx(aic.table, "AICc_results.xlsx")





