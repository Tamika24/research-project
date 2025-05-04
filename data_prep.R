install.packages("lubridate")
library(lubridate)
library(readxl)
library(dplyr)
library(tidyr)
#####------------------------
raw <- read_excel(
  "Peregrine ringing data sightings_1989-2024_13042025.xlsx",
  sheet = "Re-sightings - annual",
  skip = 3 # skip metadata rows
)

# 3. Tidy up: rename the ring ID and the year‐sighted columns,
#    and keep just those plus make an “obs” flag = 1
dat <- raw %>%
  rename(
    ring      = `ring number`,
    date_seen = `date sighted on territory`
  ) %>%
  # turn the POSIX date into an integer year
  mutate(year_seen = year(date_seen)) %>%
  select(ring, year_seen) %>%
  distinct() %>%              # one (ring, year) per bird-year
  mutate(obs = 1L)            # 1 = seen

# 4. Build a full table of (ring × all years in your study) and join...so even if a bird wasn't seen in a certain year that (ring, year) pair still exists...to be filled with 0s later
years <- 1997:2019

all_combos <- expand.grid(
  ring      = unique(dat$ring),
  year_seen = years
)

full <- all_combos %>%
  left_join(dat, by = c("ring", "year_seen")) %>%
  mutate(obs = replace_na(obs, 0L)) # If a bird was not seen in a year, obs is set to 0 using replace_na.

enc_hist <- full %>%
  arrange(ring, year_seen) %>%
  pivot_wider(
    names_from  = year_seen,
    values_from = obs,
    names_prefix = "yr",
    values_fill  = 0L
  )

# drop the ring column if you just want the matrix
ch_matrix <- as.matrix(enc_hist[,-1])
rownames(ch_matrix) <- enc_hist$ring

# Or, if you want a 0/1 string:
cap_hist <- enc_hist %>%
  mutate(
    caphist_str = apply(select(., starts_with("yr")), 1, paste0, collapse = "")
  )

# Optional: add frequency column (if each is unique, all freq = 1)
cap_hist_inp <- cap_hist %>%
  select(caphist_str) %>%
  mutate(freq = 1)

# Write to .inp file (no header, space-separated)
write.table(
  cap_hist_inp,
  file = "peregrine_data.inp",
  quote = FALSE,
  row.names = FALSE,
  col.names = FALSE,
  sep = " "
)


#####new year code-----------
# 1. Read the raw data
raw <- read_excel(
  "Peregrine ringing data sightings_1989-2024_13042025.xlsx",
  sheet = "Re-sightings - annual",
  skip  = 3
)

# 2. Compute an "Aug–Jul capture‐year" and flag sightings
dat <- raw %>%
  rename(
    ring      = `ring number`,
    date_seen = `date sighted on territory`
  ) %>%
  mutate(
    date_seen = as.Date(date_seen),
    # if month ≥ 8 (Aug–Dec) → same calendar year; else (Jan–Jul) → previous year
    cap_year = if_else(
      month(date_seen) >= 9,
      year(date_seen),
      year(date_seen) - 1L
    )
  ) %>%
  select(ring, cap_year) %>%
  distinct() %>%
  mutate(obs = 1L) %>%
  # keep only your 23 seasons 1997–2019
  filter(cap_year >= 1997, cap_year <= 2019)

# 3. Build every ring × Aug–Jul season from 1997 to 2019
years <- 1997:2019

all_combos <- expand.grid(
  ring     = unique(dat$ring),
  cap_year = years
)

full <- all_combos %>%
  left_join(dat, by = c("ring","cap_year")) %>%
  mutate(obs = replace_na(obs, 0L))

# 4. Pivot into your 0/1 encounter‐history (yr1990 … yr2019)
enc_hist <- full %>%
  arrange(ring, cap_year) %>%
  pivot_wider(
    names_from   = cap_year,
    values_from  = obs,
    names_prefix = "yr",
    values_fill  = 0L
  )

# 5. (Optional) Get a matrix or .inp file exactly as before:
ch_matrix <- as.matrix(enc_hist[,-1])
rownames(ch_matrix) <- enc_hist$ring

# if you need the .inp format:
caphist_inp <- enc_hist %>%
  mutate(caphist = apply(select(., starts_with("yr")), 1, paste0, collapse = "")) %>%
  select(caphist) %>%
  mutate(freq = 1)

write.table(
  caphist_inp,
  "peregrine_data_newyear.inp",
  quote     = FALSE,
  row.names = FALSE,
  col.names = FALSE,
  sep       = " "
)
