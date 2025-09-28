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
  Psi = list(formula = ~stratum * time)
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
install.packages("writexl")  # Only once
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
model.list <- list(
  S = list(formula = ~ stratum + time),         # Survival: state + time
  p = list(formula = ~ stratum + time),         # Recapture: state + time
  Psi = list(formula = ~ time + ageclass)       # Transition: time + age
)

# ---- 7. Fit the model
fit <- mark(ms.processed, ddl, model.parameters = model.list)

# ---- 8. View results
summary(fit)

### design matrix for specific params
# For survival (S)
head(ddl$S)

# For recapture (p)
head(ddl$p)

# For transition probabilities (Psi)
head(ddl$Psi)
#####constant model#####
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
  S   = list(formula = ~1),
  p   = list(formula = ~1),
  Psi = list(formula = ~1)
)
fit.const <- mark(ms.processed, ddl, model.parameters = model.const)
fit.const$results$AICc


#####S.P.PSIt#####
library(RMark)

data <- convert.inp("peregrine_multistate_final.inp", group.df = NULL)
ms.processed <- process.data(data, model = "Multistrata", strata.labels = c("1","2"))
ddl <- make.design.data(ms.processed)
ddl$Psi$fix[ddl$Psi$stratum=="2" & ddl$Psi$tostratum=="1"] <- 0

model.Psit <- list(
  S   = list(formula = ~1),
  p   = list(formula = ~1),
  Psi = list(formula = ~time)
)
fit.Psit <- mark(ms.processed, ddl, model.parameters = model.Psit)
fit.Psit$results$AICc
#####age model#####
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
  S   = list(formula = ~stratum),
  p   = list(formula = ~1),
  Psi = list(formula = ~ageclass)
)
fit.Psiage <- mark(ms.processed, ddl, model.parameters = model.Psiage)
fit.Psiage$results$AICc
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
  S   = list(formula = ~stratum + I(stratum=="1")*ageclass2 + I(stratum=="2")*time),
  p   = list(formula = ~stratum + time),
  Psi = list(formula = ~ageclass)
)

fit.ageNB <- mark(ms.processed, ddl, model.parameters = model.ageNB)
fit.ageNB$results$AICc
