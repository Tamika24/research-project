# fully time dependent model, no age structure
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



#######################
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