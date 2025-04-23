install.packages("lubridate")
library(lubridate)
library(readxl)
library(dplyr)
library(tidyr)

raw <- read_excel(
  "Peregrine ringing data sightings_1989-2024_13042025.xlsx",
  sheet = "Re-sightings - annual",
  skip = 3
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

# 4. Build a full table of (ring × all years in your study) and join
years <- 1990:2019

all_combos <- expand.grid(
  ring      = unique(dat$ring),
  year_seen = years
)

full <- all_combos %>%
  left_join(dat, by = c("ring", "year_seen")) %>%
  mutate(obs = replace_na(obs, 0L))

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
