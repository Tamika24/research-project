library(lubridate)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

#####------------------------
raw <- read_excel(
  "Peregrine ringing data sightings_1989-2024_13042025.xlsx",
  sheet = "Re-sightings - annual",
  skip = 3 # skip metadata rows
)

# rename the ring ID and the year‐sighted columns, and keep just those plus make an “obs” flag = 1
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

# Build a full table of (ring × all years in your study) and join...so even if a bird wasn't seen in a certain year that (ring, year) pair still exists...to be filled with 0s later
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
#Read the raw data
raw <- read_excel(
  "Peregrine ringing data sightings_1989-2024_13042025.xlsx",
  sheet = "Re-sightings - annual",
  skip  = 3
)

#Compute an "Aug–Jul capture‐year" and flag sightings
dat <- raw %>%
  rename(
    ring      = `ring number`,
    date_seen = `date sighted on territory`
  ) %>%
  mutate(
    date_seen = as.Date(date_seen),
    cap_year = if_else(
      month(date_seen) >= 8, #changed from 9 to 8 because we want aug-july not sept-aug
      year(date_seen),
      year(date_seen) - 1L
    )
  ) %>%
  select(ring, cap_year) %>%
  distinct() %>%
  mutate(obs = 1L) %>%
  # keep only your 23 seasons 1997–2019
  filter(cap_year >= 1997, cap_year <= 2019)

#Build every ring × Aug–Jul season from 1997 to 2019
years <- 1997:2019

all_combos <- expand.grid(
  ring     = unique(dat$ring),
  cap_year = years
)

full <- all_combos %>%
  left_join(dat, by = c("ring","cap_year")) %>%
  mutate(obs = replace_na(obs, 0L))

#Pivot into your 0/1 encounter‐history
enc_hist <- full %>%
  arrange(ring, cap_year) %>%
  pivot_wider(
    names_from   = cap_year,
    values_from  = obs,
    names_prefix = "yr",
    values_fill  = 0L
  )

#(Optional) Get a matrix or .inp file exactly as before:
ch_matrix <- as.matrix(enc_hist[,-1])
rownames(ch_matrix) <- enc_hist$ring

# if need the .inp format:
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
# build each line with a trailing semicolon
lines <- with(caphist_inp, paste0(caphist, " ", freq, " ;"))

# write out exactly those lines
writeLines(lines, "peregrine_data_newyear.inp")
#checking if any encounter histories are just zeros
caphist_inp %>%
  mutate(
    n1 = str_count(caphist, "1")          # count of 1’s in each string
  ) %>%
  summarise(
    all_zero    = sum(n1 == 0),               # histories with no sightings at all
    single_cap  = sum(n1 == 1),               # seen exactly once
    recaptured  = sum(n1  > 1),               # seen more than once
    total_birds = n()                         # total rows
  )
#no all zero entries, 34 single captures(no recapture), 145 recaptured so mark should be fine analysing this data


#checking if any years have all zeros
#count total sightings in each occasion
year_counts <- colSums(ch_matrix)

#see if any occasion has zero sightings
zero_years <- names(year_counts)[year_counts == 0]

year_counts
zero_years
#no years with just zeros so should be fine in mark

######merging the excel sheets#######
#we've been building histories only from the second sheet, treating resightings like our “captures.” But MARK and other CJS‐type models assume that occasion 1 for each animal is its release (i.e. when it was ringed).
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

# file path
path <- "Peregrine ringing data sightings_1989-2024_13042025.xlsx"

#Read & process the “All ringed” sheet (primary captures)
ring_events <- read_excel(path, sheet = "All ringed", skip = 3) %>%
  rename(
    ring       = `ring number`,
    date_event = `date ringed`
  ) %>%
  mutate(
    date_event = as.Date(date_event),
    # Aug–Jul “capture‐year”
    cap_year   = if_else(
      month(date_event) >= 8,
      year(date_event),
      year(date_event) - 1L
    ),
    obs = 1L
  ) %>%
  # keep only seasons 1997–2019
  filter(cap_year >= 1997, cap_year <= 2019) %>%
  select(ring, cap_year, obs)

#Read & process the “Re-sightings – annual” sheet (recaptures)
sight_events <- read_excel(path, sheet = "Re-sightings - annual", skip = 3) %>%
  rename(
    ring       = `ring number`,
    date_event = `date sighted on territory`
  ) %>%
  mutate(
    date_event = as.Date(date_event),
    cap_year   = if_else(
      month(date_event) >= 8,
      year(date_event),
      year(date_event) - 1L
    ),
    obs = 1L
  ) %>%
  filter(cap_year >= 1997, cap_year <= 2019) %>%
  select(ring, cap_year, obs)

#Combine captures and recaptures, but only for birds ringed ≥1997
dat_all <- bind_rows(
  ring_events,
  sight_events %>% filter(ring %in% ring_events$ring)
) %>%
  distinct()

#Build full bird × season grid and fill zeros
years <- 1997:2019
all_combos <- expand.grid(
  ring     = unique(dat_all$ring),
  cap_year = years
)

full <- all_combos %>%
  left_join(dat_all, by = c("ring", "cap_year")) %>%
  mutate(obs = replace_na(obs, 0L))

#Pivot to a 0/1 encounter history (yr1997 … yr2019)
enc_hist <- full %>%
  arrange(ring, cap_year) %>%
  pivot_wider(
    names_from   = cap_year,
    values_from  = obs,
    names_prefix = "yr",
    values_fill  = 0L
  )

#Optional: extract matrix and write .inp file
ch_matrix <- as.matrix(enc_hist[,-1])
rownames(ch_matrix) <- enc_hist$ring

caphist_inp <- enc_hist %>%
  mutate(
    caphist = apply(select(., starts_with("yr")), 1, paste0, collapse = "")
  ) %>%
  select(caphist) %>%
  mutate(freq = 1)

write.table(
  caphist_inp,
  "peregrine_merged_.inp",
  quote     = FALSE,
  row.names = FALSE,
  col.names = FALSE,
  sep       = " "
)
#Build each line with a trailing semicolon
lines <- with(caphist_inp, paste0(caphist, " ", freq, " ;"))

#Write exactly those lines to your .inp
writeLines(lines, "peregrine_merged_.inp")
#checking if there's any all zero entries
#Find which rows sum to zero
zero_rows <- which(rowSums(ch_matrix) == 0)

#Any all‐zero histories?
any_zero_histories <- length(zero_rows) > 0
any_zero_histories    # TRUE/FALSE

#See which birds they are
if (any_zero_histories) {
  rownames(ch_matrix)[zero_rows]
}
#no all zero histories here either so we didn't have to remove any
#####new age class histories#####
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

path <- "Peregrine ringing data sightings_1989-2024_13042025.xlsx"

#Process "All ringed"
ring_events <- read_excel(path, sheet = "All ringed", skip = 3) %>%
  rename(
    ring       = `ring number`,
    date_event = `date ringed`,
    age        = age              #one-letter codes: n, j, a OR juvenile
  ) %>%
  mutate(
    date_event = as.Date(date_event),
    cap_year   = if_else(month(date_event) >= 9,
                         year(date_event),
                         year(date_event) - 1L),
    obs        = 1L
  ) %>%
  #Drop juvenile rows entirely
  filter(age %in% c("n","a"),
         cap_year >= 1997, cap_year <= 2019) %>%
  select(ring, cap_year, obs, age)

#Process "Re-sightings – annual"
sight_events <- read_excel(path, sheet = "Re-sightings - annual", skip = 3) %>%
  rename(
    ring       = `ring number`,
    date_event = `date sighted on territory`
  ) %>%
  mutate(
    date_event = as.Date(date_event),
    cap_year   = if_else(month(date_event) >= 9,
                         year(date_event),
                         year(date_event) - 1L),
    obs        = 1L
  ) %>%
  filter(cap_year >= 1997, cap_year <= 2019,
         ring %in% ring_events$ring) %>%
  select(ring, cap_year, obs)

#Combine captures and recaptures
dat_all <- bind_rows(ring_events, sight_events) %>%
  distinct(ring, cap_year, obs)

#Expand to full bird × season
years <- 1997:2019
all_combos <- expand.grid(
  ring     = unique(dat_all$ring),
  cap_year = years
)

full <- all_combos %>%
  left_join(dat_all, by = c("ring","cap_year")) %>%
  mutate(obs = replace_na(obs, 0L))

#Pivot to wide & reattach age
enc_hist <- full %>%
  arrange(ring, cap_year) %>%
  pivot_wider(
    names_from   = cap_year,
    values_from  = obs,
    names_prefix = "yr",
    values_fill  = 0L
  ) %>%
  left_join(ring_events %>% distinct(ring, age), by = "ring")

#Build inp with 2 age class columns (no frequency column)
inp_age2 <- enc_hist %>%
  mutate(
    caphist = apply(select(., starts_with("yr")), 1, paste0, collapse = ""),
    nestling    = if_else(age == "n", 1L, 0L),
    adult       = if_else(age == "a", 1L, 0L)
  ) %>%
  select(caphist, nestling, adult)

#Write to .inp file
lines <- with(inp_age2, paste0(caphist, " ", nestling, " ", adult, " ;"))
writeLines(lines, "peregrine_age2class.inp")

#####data exploration#####
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(forcats)

# Point to your Excel file (put it in your working folder)
path <- "Peregrine ringing data sightings_1989-2024_13042025.xlsx"

# Season label: **Sep–Aug** year
season_year <- function(d) { d <- as.Date(d); ifelse(month(d) >= 9, year(d), year(d) - 1L) }

# Create output folder for figures/tables
if (!dir.exists("figs")) dir.create("figs")

#read & prep: ringed (captures)
ringed <- read_excel(path, sheet = "All ringed", skip = 3) %>%
  rename(
    ring      = `ring number`,
    date_ring = `date ringed`,
    age_raw   = age
  ) %>%
  mutate(
    ring      = as.character(ring) |> stringr::str_trim(),
    date_ring = as.Date(date_ring),
    cap_year  = season_year(date_ring),
    age = case_when(
      stringr::str_to_lower(age_raw) %in% c("n","nestling") ~ "nestling",
      stringr::str_to_lower(age_raw) %in% c("a","adult")    ~ "adult",
      stringr::str_to_lower(age_raw) %in% c("j","juvenile") ~ "juvenile",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(cap_year >= 1997, cap_year <= 2019) %>%
  select(ring, date_ring, cap_year, age)

#read & prep: re-sightings (encounters)
sight <- read_excel(path, sheet = "Re-sightings - annual", skip = 3) %>%
  rename(
    ring      = `ring number`,
    date_seen = `date sighted on territory`
  ) %>%
  mutate(
    ring     = as.character(ring) |> stringr::str_trim(),
    date_seen = as.Date(date_seen),
    sea_year  = season_year(date_seen)
  ) %>%
  # only keep re-sightings for birds ringed ≥ 1997
  filter(ring %in% ringed$ring,
         sea_year >= 1997, sea_year <= 2019) %>%
  select(ring, date_seen, sea_year)


# 0/1 encounters (distinct per bird-season)
enc01 <- sight %>% distinct(ring, sea_year) %>% mutate(obs = 1L)

#New birds ringed per season (1997–2019)
ringed_per_year <- ringed %>%
  count(cap_year, name = "n_ringed")

readr::write_csv(ringed_per_year, "figs/table_ringed_per_year.csv")

pA <- ggplot(ringed_per_year, aes(cap_year, n_ringed)) +
  geom_col() +
  labs(x = "Season (Sep–Aug)", y = "New birds ringed",
       title = "New peregrines ringed per season (1997–2019)") +
  theme_minimal(base_size = 12)

ggsave("figs/figA_ringed_per_season.png", pA, width = 7, height = 4.2, dpi = 300)

#total resightings per season
resightings_per_year <- sight %>%
  count(sea_year, name = "n_resightings")

readr::write_csv(resightings_per_year, "figs/table_resightings_per_year.csv")

pB <- ggplot(resightings_per_year, aes(sea_year, n_resightings)) +
  geom_col() +
  labs(x = "Season (Sep–Aug)", y = "Total re-sightings",
       title = "Re-sightings per season (1997–2019)") +
  theme_minimal(base_size = 12)

ggsave("figs/figB_resightings_per_season.png", pB, width = 7, height = 4.2, dpi = 300)

#unique individuals detected per season 
enc01 <- sight %>%
  distinct(ring, sea_year) %>%
  mutate(obs = 1L)

unique_seen_per_year <- enc01 %>%
  count(sea_year, name = "unique_birds_seen")

readr::write_csv(unique_seen_per_year, "figs/table_unique_birds_per_year.csv")

pC <- ggplot(unique_seen_per_year, aes(sea_year, unique_birds_seen)) +
  geom_line(linewidth = 1) + geom_point() +
  labs(x = "Season (Sep–Aug)", y = "Unique birds detected",
       title = "Unique individuals detected per season") +
  theme_minimal(base_size = 12)

ggsave("figs/figC_unique_birds_per_season.png", pC, width = 7, height = 4.2, dpi = 300)

#resighting frequency per individual
freq_per_bird <- enc01 %>%
  count(ring, name = "n_seasons_seen")

readr::write_csv(freq_per_bird, "figs/table_resightings_per_individual.csv")

pD <- ggplot(freq_per_bird, aes(n_seasons_seen)) +
  geom_histogram(binwidth = 1, boundary = -0.5, closed = "right") +
  labs(x = "Number of seasons an individual was seen",
       y = "Number of individuals",
       title = "Resighting frequency per bird") +
  theme_minimal(base_size = 12)

ggsave("figs/figD_hist_resightings_per_bird.png", pD, width = 6.5, height = 4.2, dpi = 300)

#Proportion of nestlings vs adults among birds ringed each season
age_mix <- ringed %>%
  count(cap_year, age) %>%
  mutate(age = dplyr::case_when(
    age %in% c("nestling","adult") ~ age,
    TRUE ~ "unknown"
  ))

readr::write_csv(age_mix, "figs/table_age_mix_ringed.csv")

pE <- ggplot(age_mix, aes(cap_year, n, fill = age)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Season (Sep–Aug)", y = "Proportion of new birds",
       fill = "Age at ringing",
       title = "Proportion nestlings vs adults among birds ringed") +
  theme_minimal(base_size = 12)

ggsave("figs/figE_age_mix_ringed.png", pE, width = 7, height = 4.2, dpi = 300)
