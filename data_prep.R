#####original#####
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

# Write to .inp file (no header, separated by space)
write.table(
  cap_hist_inp,
  file = "peregrine_data.inp",
  quote = FALSE,
  row.names = FALSE,
  col.names = FALSE,
  sep = " "
)
# Create lines with space-separated columns and semicolon at the end
cap_hist_lines <- apply(cap_hist_inp, 1, function(row) {
  paste(paste(row, collapse = " "), ";", sep = "")
})

# Write lines to file
writeLines(cap_hist_lines, "peregrine_data.inp")



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

# fixing input file for marked as young vs adult
library(dplyr)
library(tidyr)

inp_age2 <- enc_hist %>%
  mutate(
    caphist = apply(select(., starts_with("yr")), 1, paste0, collapse = ""),
    age_group = case_when(
      age == "n" ~ "nestling",
      age == "a" ~ "adult"
    )
  ) %>%
  count(caphist, age_group) %>%
  pivot_wider(names_from = age_group, values_from = n, values_fill = 0) %>%
  arrange(caphist)

lines <- with(inp_age2, paste0(caphist, " ", nestling, " ", adult, " ;"))
writeLines(lines, "peregrine_ya.inp")

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

#bar chart instead of histogram 
# --- Resighting frequency per individual ---
freq_per_bird <- enc01 %>%
  count(ring, name = "n_seasons_seen")

# --- Plot using bar chart ---
pD <- ggplot(freq_per_bird, aes(x = factor(n_seasons_seen))) +
  geom_bar(fill = "#0072B2", colour = "black", width = 0.8) +
  labs(
    x = "Number of seasons an individual was seen",
    y = "Number of individuals",
    title = "Resighting Frequency per Bird"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Save the figure
ggsave("figs/figD_bar_resightings_per_bird.png", pD, width = 6.5, height = 4.2, dpi = 300)

#####graphs#####
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

#Paste the MARK table here
txt <- "
Parameter                Estimate       Standard Error     Lower           Upper
--------------------------  --------------  --------------  --------------  --------------
1:Phi                    0.2103827       0.0822775       0.0916729       0.4129307
2:Phi                    0.2593466       0.1248829       0.0891837       0.5559913
3:Phi                    0.0939757       0.0482887       0.0330021       0.2396796
4:Phi                    0.1806899       0.0698479       0.0804361       0.3573408
5:Phi                    0.1823865       0.0822677       0.0703348       0.3967642
6:Phi                    0.2328873       0.0771354       0.1152358       0.4143971
7:Phi                    0.1716103       0.0658920       0.0770779       0.3394409
8:Phi                    0.1207834       0.0473888       0.0541836       0.2477974
9:Phi                    0.2545774       0.0629501       0.1512898       0.3955189
10:Phi                    0.2794889       0.0665216       0.1687575       0.4256706
11:Phi                    0.2528566       0.0762326       0.1330416       0.4273822
12:Phi                    0.1983190       0.0597339       0.1059088       0.3406411
13:Phi                    0.2460077       0.0679025       0.1373441       0.4007089
14:Phi                    0.1764726       0.0498571       0.0986041       0.2956641
15:Phi                    0.3095343       0.0747786       0.1842111       0.4709026
16:Phi                    0.1442638       0.0434806       0.0779419       0.2516198
17:Phi                    0.2191936       0.0747568       0.1065483       0.3978933
18:Phi                    0.1304439       0.0531389       0.0565050       0.2731264
19:Phi                    0.0895636       0.0333196       0.0422951       0.1797441
20:Phi                    0.1109391       0.0373469       0.0560759       0.2076696
21:Phi                    0.1400622       0.0803437       0.0422005       0.3758173
22:Phi                    0.0184352       0.0087959       0.0071917       0.0464346
23:Phi                    0.9287783       0.0595796       0.6905581       0.9870473
24:Phi                    0.7943599       0.1530281       0.3811393       0.9603624
25:Phi                    0.8914617       0.0857301       0.5912520       0.9790075
26:Phi                    0.8925616       0.0920100       0.5588736       0.9819742
27:Phi                    0.9187408       0.0629274       0.6842308       0.9833317
28:Phi                    0.8852569       0.0881558       0.5847143       0.9768922
29:Phi                    0.8364991       0.1133400       0.5020384       0.9629117
30:Phi                    0.9271081       0.0571503       0.7079705       0.9852353
31:Phi                    0.9352598       0.0499904       0.7412020       0.9864624
32:Phi                    0.9264916       0.0568942       0.7102583       0.9848034
33:Phi                    0.9020847       0.0719619       0.6510751       0.9784890
34:Phi                    0.9239608       0.0586100       0.7031510       0.9842105
35:Phi                    0.8886483       0.0837413       0.6030770       0.9766997
36:Phi                    0.9434886       0.0415532       0.7837437       0.9871651
37:Phi                    0.8626079       0.0985996       0.5514397       0.9697562
38:Phi                    0.9127009       0.0687483       0.6583452       0.9826763
39:Phi                    0.8481803       0.1079222       0.5194224       0.9665303
40:Phi                    0.7855765       0.1327367       0.4388011       0.9449536
41:Phi                    0.8229197       0.1207324       0.4781093       0.9593060
42:Phi                    0.8584732       0.1140226       0.4908118       0.9744712
43:Phi                    0.4115772       0.1955612       0.1256317       0.7729859
44:Phi                    0.8977876       0.0470110       0.7629240       0.9599588
45:Phi                    0.9202782       0.0471586       0.7660745       0.9760136
46:Phi                    0.7737257       0.0969124       0.5360649       0.9100650
47:Phi                    0.8790877       0.0505108       0.7412385       0.9485939
48:Phi                    0.8802962       0.0583519       0.7129749       0.9560854
49:Phi                    0.9091600       0.0359311       0.8101056       0.9591502
50:Phi                    0.8722769       0.0509116       0.7360562       0.9435830
51:Phi                    0.8191305       0.0608544       0.6693849       0.9101552
52:Phi                    0.9184262       0.0260507       0.8506400       0.9570031
53:Phi                    0.9274728       0.0230077       0.8673942       0.9615390
54:Phi                    0.9177428       0.0299312       0.8368468       0.9604251
55:Phi                    0.8907735       0.0342237       0.8036461       0.9420289
56:Phi                    0.9149384       0.0281413       0.8411435       0.9562363
57:Phi                    0.8759983       0.0365079       0.7852168       0.9317449
58:Phi                    0.9366244       0.0207161       0.8817567       0.9669854
59:Phi                    0.8475072       0.0423089       0.7452689       0.9134753
60:Phi                    0.9024834       0.0385419       0.7968569       0.9562066
61:Phi                    0.8318031       0.0613737       0.6767018       0.9211638
62:Phi                    0.7643225       0.0654810       0.6139720       0.8686428
63:Phi                    0.8044457       0.0557623       0.6725098       0.8917819
64:Phi                    0.8430007       0.0852639       0.6030140       0.9499513
65:Phi                    0.3823965       0.1013571       0.2107244       0.5894730
66:p                      0.1262407       0.0758343       0.0361897       0.3572985
67:p                      0.0828108       0.0445699       0.0277899       0.2219026
68:p                      0.1145643       0.0574929       0.0408628       0.2820993
69:p                      0.1155500       0.0510572       0.0467769       0.2580621
70:p                      0.1251603       0.0509120       0.0543829       0.2624829
71:p                      0.1565445       0.0561447       0.0746343       0.2992766
72:p                      0.1591344       0.0559857       0.0769351       0.3005607
73:p                      0.1452990       0.0512665       0.0703662       0.2763102
74:p                      0.2760895       0.0756575       0.1536975       0.4447281
75:p                      0.2081833       0.0582038       0.1162983       0.3443741
76:p                      0.2285787       0.0644004       0.1265076       0.3774204
77:p                      0.1231206       0.0387978       0.0649148       0.2211721
78:p                      0.1226483       0.0372452       0.0662363       0.2159918
79:p                      0.1370216       0.0409226       0.0745670       0.2383153
80:p                      0.1657569       0.0466701       0.0930033       0.2779806
81:p                      0.1604416       0.0504155       0.0840512       0.2846814
82:p                      0.1370902       0.0427864       0.0725220       0.2440202
83:p                      0.1227083       0.0402808       0.0629485       0.2255454
84:p                      0.3071894       0.0996640       0.1504410       0.5261167
85:p                      0.1644028       0.0666094       0.0706872       0.3372724
86:p                      0.4611696       0.5803406       0.0087223       0.9881305
87:p                      0.2858292       0.4387462       0.0058909       0.9643249
88:p                      0.6090326       0.1572863       0.2991303       0.8504249
89:p                      0.4932769       0.1334993       0.2546968       0.7349589
90:p                      0.5824693       0.1232594       0.3406363       0.7902274
91:p                      0.5848217       0.1066307       0.3732832       0.7691212
92:p                      0.6066892       0.0961387       0.4118639       0.7726084
93:p                      0.6667884       0.0814915       0.4937025       0.8041727
94:p                      0.6711032       0.0760139       0.5095449       0.8003005
95:p                      0.6470058       0.0757826       0.4888892       0.7783813
96:p                      0.8043834       0.0519097       0.6829407       0.8870062
97:p                      0.7392262       0.0543313       0.6199972       0.8312293
98:p                      0.7616065       0.0514792       0.6469773       0.8477725
99:p                      0.6022039       0.0606021       0.4797158       0.7131013
100:p                      0.6011538       0.0584145       0.4831967       0.7084336
101:p                      0.6312565       0.0576044       0.5131386       0.7354895
102:p                      0.6817573       0.0540853       0.5678946       0.7773782
103:p                      0.6732487       0.0624536       0.5415223       0.7823406
104:p                      0.6313914       0.0644447       0.4988666       0.7466672
105:p                      0.6012875       0.0675439       0.4647157       0.7237295
106:p                      0.8270081       0.0570038       0.6864593       0.9125776
107:p                      0.6796219       0.0897045       0.4861383       0.8262860
108:p                      0.9022278       0.2056507       0.0872944       0.9988781
109:p                      0.8118589       0.3271633       0.0608821       0.9965305
"

#Parse the table into tidy data

dat <- read_table2(txt, comment = "-", col_types = "cdddd") |>
  rename(Parameter = 1, estimate = 2, se = 3, lower = 4, upper = 5) |>
  mutate(
    idx = as.integer(str_extract(Parameter, "^[0-9]+")),
    par = str_extract(Parameter, "(?<=:)[A-Za-z]+")
  ) |>
  select(idx, par, estimate, lower, upper)

stopifnot(nrow(dat) == 109)

# Study has 23 occasions -> 22 intervals:
n_int <- 22
interval <- 1:n_int
year_lab <- 1997 + (interval - 1)   # change to whatever labels you prefer


#Survival (Phi) — re-labeled per your request
#   1..22  : MY first-year (Age1)
#   23..43 : MY juvenile (Age2) 
#   44..65 : Adult (shared across MY and MA; Age3+)
phi <- filter(dat, par == "Phi")

phi_juv <- phi |>
  filter(idx %in% 23:43) |>
  mutate(interval = 2:n_int,
         year     = year_lab[2:n_int],
         strata   = "Juvenile (Age 2)")

phi_adult <- phi |>
  filter(idx %in% 44:65) |>
  mutate(interval = interval,
         year     = year_lab,
         strata   = "Adult (Age 3+)")

# If you also want to keep the first-year curve, uncomment below:
phi_firstyear <- phi |>
   filter(idx %in% 1:22) |>
   mutate(interval = interval,
          year     = year_lab,
          strata   = "First-year (Age 1)")

phi_plot <- bind_rows(phi_juv, phi_adult, phi_firstyear)


# Detection p(t/t) by marking group
p <- filter(dat, par == "p")

p_my <- p |>
  filter(idx %in% 66:87) |>
  mutate(interval = interval,
         year     = year_lab,
         group    = "Marked as young")

p_ma <- p |>
  filter(idx %in% 88:109) |>
  mutate(interval = interval,
         year     = year_lab,
         group    = "Marked as adult")

p_all <- bind_rows(p_my, p_ma)


# Plots

base_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

#(A) Juvenile vs Adult survival on ONE plot 
gg_phi_both <- ggplot(phi_plot, aes(x = year, y = estimate,
                                    colour = strata, fill = strata)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
  labs(x = "Breeding season (start year)",
       y = "Apparent survival Φ",
       colour = NULL, fill = NULL,
       title = "Juvenile (Age 2) vs Adult (Age 3+) apparent survival") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank())

# Detection by marking group 
gg_p <- ggplot(p_all, aes(x = year, y = estimate, colour = group, shape = group)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
  labs(x = "Breeding season (start year)",
       y = "Detection probability p",
       colour = NULL, shape = NULL,
       title = "Detection probability by marking group") +
  base_theme +
  theme(legend.position = "top")

# Print
gg_phi_both
gg_p





txt <- "
Parameter                Estimate       Standard Error     Lower           Upper
--------------------------  --------------  --------------  --------------  --------------
  1:Phi                    0.1765477       0.0725121       0.0746387       0.3630146      
2:Phi                    0.1776978       0.0945207       0.0573307       0.4343392      
3:Phi                    0.0508721       0.0660097       0.0036631       0.4386372      
4:Phi                    0.1462244       0.7554963       0.1209111E-05   0.9999588      
5:Phi                    0.2709335       8.7349832       0.8474347E-38   1.0000000      
6:Phi                    0.2763160       0.1197986       0.1055487       0.5526587      
7:Phi                    0.1641489       0.0677178       0.0694579       0.3406705      
8:Phi                    0.1694442       0.8168836       0.2337598E-05   0.9999438      
9:Phi                    0.2599908       0.0648218       0.1536340       0.4047640      
10:Phi                    0.2750703       0.0651935       0.1666101       0.4186664      
11:Phi                    0.2762247       0.0837551       0.1437636       0.4645206      
12:Phi                    0.1014174       0.0625312       0.0285690       0.3022307      
13:Phi                    0.2137033       2.5880610       0.2107601E-13   1.0000000      
14:Phi                    0.1946663       0.0588282       0.1038224       0.3352614      
15:Phi                    0.9999937       0.0043172      0.8876572E-303   1.0000000      
16:Phi                    0.1078036       0.1093242       0.0128537       0.5285782      
17:Phi                    0.2666664       0.0926960       0.1255744       0.4793780      
18:Phi                    0.1594415       0.6621641       0.1181252E-04   0.9996718      
19:Phi                    0.0537128       0.0370254       0.0134311       0.1913699      
20:Phi                    0.0282921       0.0281961       0.0038852       0.1785431      
21:Phi                    0.0454540       0.0447034       0.0062808       0.2640332      
22:Phi                    0.3042186E-06   0.0013478      0.1692273E-314   1.0000000      
23:Phi                    1.0000000       0.5227441E-06   0.9999990       1.0000010      
24:Phi                    1.0000000       0.5528375E-06   0.9999989       1.0000011      
25:Phi                    0.9995650       0.8537150      0.1278367E-304   1.0000000      
26:Phi                    0.9927085       5.1075535      0.7573321E-306   1.0000000      
27:Phi                    0.8911602       28.729296      0.6091762E-251   1.0000000      
28:Phi                    0.7105884       0.2910217       0.1329205       0.9752016      
29:Phi                    1.0000000       0.5805014E-06   0.9999989       1.0000011      
30:Phi                    0.9952262       4.7765643      0.1159698E-305   1.0000000      
31:Phi                    1.0000000       0.2294371E-07   1.0000000       1.0000000      
32:Phi                    1.0000000       0.8064764E-08   1.0000000       1.0000000      
33:Phi                    1.0000000       0.2753725E-06   0.9999995       1.0000005      
34:Phi                    0.9998502       0.2776635      0.3714073E-304   1.0000000      
35:Phi                    0.9766130       11.823267      0.2322906E-306   1.0000000      
36:Phi                    1.0000000       0.3075649E-07   0.9999999       1.0000001      
37:Phi                    0.2476352       0.0630360       0.1449957       0.3898063      
38:Phi                    0.9996159       0.8950522      0.1447608E-304   1.0000000      
39:Phi                    0.8553645       0.2505071       0.1005250       0.9968147      
40:Phi                    0.9957192       4.1181204      0.1293888E-305   1.0000000      
41:Phi                    0.9999964       0.0162720      0.1558668E-302   1.0000000      
42:Phi                    0.9999996       0.0020282      0.1523201E-301   1.0000000      
43:Phi                    0.4094324E-07   0.1515196E-03  0.2277543E-315   1.0000000      
44:Phi                    1.0000000       0.1180064E-04   0.9999769       1.0000231      
45:Phi                    1.0000000       0.6354924E-04  0.2699696E-300   1.0000000      
46:Phi                    0.8623774       0.1471221       0.3556055       0.9861407      
47:Phi                    0.9290960       0.1364322       0.1844735       0.9986843      
48:Phi                    0.8125036       0.1338261       0.4364891       0.9603857      
49:Phi                    0.8499003       0.1140685       0.4953137       0.9702979      
50:Phi                    0.8805105       0.0890014       0.5840102       0.9747976      
51:Phi                    0.8533227       0.1000271       0.5484775       0.9653531      
52:Phi                    0.8264880       0.0837538       0.6025811       0.9373590      
53:Phi                    0.9245332       0.0553105       0.7214832       0.9830328      
54:Phi                    0.8629235       0.0554870       0.7151178       0.9404310      
55:Phi                    0.9572584       0.0436611       0.7344537       0.9945162      
56:Phi                    0.9310552       0.0603704       0.6812849       0.9884144      
57:Phi                    0.8300868       0.0662716       0.6604498       0.9246440      
58:Phi                    0.9578621       0.0578806       0.5776330       0.9973603      
59:Phi                    0.8879307       0.0638344       0.6926330       0.9653467      
60:Phi                    0.8434949       0.0703169       0.6548563       0.9386861      
61:Phi                    0.8078432       0.0847923       0.5903591       0.9246076      
62:Phi                    0.7520429       0.0805086       0.5654535       0.8760723      
63:Phi                    0.8775520       0.0666435       0.6800177       0.9602676      
64:Phi                    0.9382436       0.1452337       0.1004753       0.9995163      
65:Phi                    0.4990957       17.892173       0.1195995E-60   1.0000000      
66:p                      0.1888109       0.1714325       0.0252939       0.6761339      
67:p                      0.3126406       0.2640287       0.0393195       0.8348388      
68:p                      0.1282314E-08   0.1830994E-04  -0.3588620E-04   0.3588877E-04  
69:p                      0.1278601E-09   0.5456630E-06  -0.1069372E-05   0.1069627E-05  
70:p                      0.2343751E-08   0.7816193E-05  -0.1531739E-04   0.1532208E-04  
71:p                      0.2857098       0.1707456       0.0720132       0.6733868      
72:p                      0.5711304       0.2241407       0.1813695       0.8889466      
73:p                      0.3439350E-09   0.2849517E-05  -0.5584710E-05   0.5585398E-05  
74:p                      0.4910166       0.1443553       0.2372080       0.7495424      
75:p                      0.2272123       0.1156030       0.0748447       0.5165707      
76:p                      0.1206764       0.1134556       0.0165994       0.5273655      
77:p                      0.2062849E-10   0.2103209E-06  -0.4122084E-06   0.4122496E-06  
78:p                      0.1985013E-11   0.1738952E-07  -0.3408148E-07   0.3408545E-07  
79:p                      0.1048376       0.0995707       0.0144267       0.4837445      
80:p                      0.0192303       0.0190450       0.0027021       0.1242614      
81:p                      0.4610611E-11   0.2744588E-07  -0.5378932E-07   0.5379854E-07  
82:p                      0.3749975       0.1711624       0.1254048       0.7151525      
83:p                      0.2832172E-11   0.1489992E-07  -0.2920102E-07   0.2920668E-07  
84:p                      0.1039134E-10   0.6207666E-07  -0.1216599E-06   0.1216806E-06  
85:p                      0.2447939E-11   0.1879298E-07  -0.3683180E-07   0.3683670E-07  
86:p                      0.9999723       0.1126707      0.2008490E-303   1.0000000      
87:p                      0.3042186E-06   0.0013478      0.1692273E-314   1.0000000      
88:p                      0.6666641       0.2721650       0.1535077       0.9566289      
89:p                      0.6798471       0.1508494       0.3531228       0.8920139      
90:p                      0.3050649       0.1193675       0.1270964       0.5696176      
91:p                      0.7711833       0.1151890       0.4838953       0.9237518      
92:p                      0.4444444       0.1149320       0.2431523       0.6657859      
93:p                      0.6623844       0.0966501       0.4568217       0.8206887      
94:p                      0.7450335       0.0827118       0.5545011       0.8727750      
95:p                      0.3874365       0.0818223       0.2434540       0.5541948      
96:p                      0.8918910       0.0510488       0.7450745       0.9588257      
97:p                      0.7711510       0.0633150       0.6251884       0.8719169      
98:p                      0.7521357       0.0573857       0.6240357       0.8472712      
99:p                      0.7173227       0.0580439       0.5914982       0.8164199      
100:p                      0.6310275       0.0654832       0.4963458       0.7479807      
101:p                      0.5932207       0.0639531       0.4645216       0.7102799      
102:p                      0.6343846       0.0620439       0.5066936       0.7456171      
103:p                      0.7118647       0.0589618       0.5844423       0.8127347      
104:p                      0.6754013       0.0666253       0.5342148       0.7905694      
105:p                      0.5744686       0.0721190       0.4309116       0.7064794      
106:p                      0.6595749       0.0691184       0.5145103       0.7798415      
107:p                      0.8378377       0.0605974       0.6830984       0.9252840      
108:p                      0.6470583       0.1159004       0.4040611       0.8321357      
109:p                      0.6887401       24.690629       0.2027793E-97   1.0000000  
"
dat <- read_table2(txt, comment = "-", col_types = "cdddd") |>
  rename(Parameter = 1, estimate = 2, se = 3, lower = 4, upper = 5) |>
  mutate(
    idx = as.integer(str_extract(Parameter, "^[0-9]+")),
    par = str_extract(Parameter, "(?<=:)[A-Za-z]+")
  ) |>
  select(idx, par, estimate, lower, upper)

stopifnot(nrow(dat) == 109)

#Study design
n_int <- 22                       # 23 occasions → 22 intervals
interval <- 1:n_int
year_lab <- 1997 + (interval - 1) # adjust if first season differs

#Split Φ blocks 
phi <- filter(dat, par == "Phi")

phi_first <- phi |> filter(idx %in% 1:22) |>
  mutate(interval = interval,
         year = year_lab,
         stratum = "First-year (Age 1)")

phi_juv <- phi |> filter(idx %in% 23:43) |>
  mutate(interval = 2:n_int,
         year = year_lab[2:n_int],
         stratum = "Juvenile (Age 2)")

phi_adult <- phi |> filter(idx %in% 44:65) |>
  mutate(interval = interval,
         year = year_lab,
         stratum = "Adult (Age 3+)")

phi_all <- bind_rows(phi_first, phi_juv, phi_adult)

# Split p blocks 
p <- filter(dat, par == "p")

p_my <- p |> filter(idx %in% 66:87) |>
  mutate(interval = interval,
         year = year_lab,
         group = "Marked as young")

p_ma <- p |> filter(idx %in% 88:109) |>
  mutate(interval = interval,
         year = year_lab,
         group = "Marked as adult")

p_all <- bind_rows(p_my, p_ma)

#Plot theme
base_theme <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())


#Plots


#Survival (Juvenile vs Adult)
gg_phi_both <- phi_all |>
  filter(stratum %in% c("Juvenile (Age 2)", "Adult (Age 3+)")) |>
  ggplot(aes(x = year, y = estimate, color = stratum, fill = stratum)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0,1.05), breaks = seq(0,1,0.2)) +
  labs(x = "Breeding season (start year)",
       y = "Apparent survival Φ",
       color = NULL, fill = NULL,
       title = "Nestling (Age 1) vs Juvenile (Age 2) vs Adult (Age 3+) apparent survival") +
  base_theme

#Add first-year to same figure
gg_phi_all3 <- gg_phi_both +
  geom_ribbon(data = phi_first,
              aes(x = year, ymin = lower, ymax = upper, fill = "First-year (Age 1)"),
              alpha = 0.12, inherit.aes = FALSE) +
  geom_line(data = phi_first,
            aes(x = year, y = estimate, color = "First-year (Age 1)"),
            linewidth = 1, inherit.aes = FALSE) +
  geom_point(data = phi_first,
             aes(x = year, y = estimate, color = "First-year (Age 1)"),
             size = 2, inherit.aes = FALSE) +
  scale_color_manual(values = c("Adult (Age 3+)" = "#E64B35",
                                "Juvenile (Age 2)" = "#4DBBD5",
                                "First-year (Age 1)" = "#00A087")) +
  scale_fill_manual(values  = c("Adult (Age 3+)" = "#E64B35",
                                "Juvenile (Age 2)" = "#4DBBD5",
                                "First-year (Age 1)" = "#00A087"))

# Detection p by group
gg_p <- p_all |>
  ggplot(aes(x = year, y = estimate, color = group, shape = group)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, alpha = 0.6) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(limits = c(0,1.05), breaks = seq(0,1,0.2)) +
  labs(x = "Breeding season (start year)",
       y = "Detection probability p",
       color = NULL, shape = NULL,
       title = "Detection probability by marking group") +
  base_theme

# Print 
gg_phi_all3   # survival with all 3 strata
gg_p          # detection

##### Final multistate encounter histories for MARK #####
#Load the Excel sheets

path <- "Peregrine ringing data sightings_1989-2024_13042025.xlsx"

all_ringed <- read_excel(path, sheet = "All ringed", skip = 3) %>%
  rename(
    ring = `ring number`,
    date_ringed = `date ringed`,
    age = age
  ) %>%
  mutate(
    date_ringed = as.Date(date_ringed),
    year = if_else(month(date_ringed) >= 9,
                   year(date_ringed),
                   year(date_ringed) - 1L)
  ) %>%
  filter(year >= 1997, year <= 2019)

annual <- read_excel(path, sheet = "Re-sightings - annual", skip = 3) %>%
  rename(
    ring = `ring number`,
    date_sighted = `date sighted on territory`
  ) %>%
  mutate(
    date_sighted = as.Date(date_sighted),
    year = if_else(month(date_sighted) >= 9,
                   year(date_sighted),
                   year(date_sighted) - 1L)
  ) %>%
  filter(year >= 1997, year <= 2019)

dispersal <- read_excel(path, sheet = "Re-sightings - dispersal", skip = 2) %>%
  rename(
    ring = `ring number`,
    date_sighted = `date of sighting`,
    ringed_as = `ringed as`,
    loc_rel = `location relative to study area`
  ) %>%
  mutate(
    date_sighted = as.Date(date_sighted),
    year = if_else(month(date_sighted) >= 9,
                   year(date_sighted),
                   year(date_sighted) - 1L)
  ) %>%
  filter(year >= 1997, year <= 2019,
         ringed_as == "n",          # only birds ringed as nestlings
         loc_rel != "outside")      # exclude outside sightings


#Assign states
# State coding: 0 = not seen, 1 = non-breeder, 2 = breeder
# Initial capture (ringing) 
ring_events <- all_ringed %>%
  # Exclude juveniles entirely
  filter(!age %in% c("j","juvenile")) %>%
  mutate(
    state = case_when(
      age %in% c("n") ~ 1,   # nestlings = non-breeders
      age %in% c("a","adult") ~ 2,  # adults = breeders
      TRUE ~ NA_real_
    )
  ) %>%
  select(ring, year, state)
#check juvenile filtered out
table(ring_events$state, useNA = "ifany")
table(all_ringed$age, useNA = "ifany")        # still shows original, unfiltered
table(ring_events$state, useNA = "ifany")     # now only 1 (n) and 2 (a)

# Annual (territory sightings → breeders)
annual_events <- annual %>%
  mutate(state = 2) %>%
  select(ring, year, state)

# Dispersal (non-breeders)
dispersal_events <- dispersal %>%
  mutate(state = 1) %>%
  select(ring, year, state)


#Combine all events

events <- bind_rows(ring_events, annual_events, dispersal_events) %>%
  distinct(ring, year, .keep_all = TRUE)

years <- 1997:2019
all_combos <- expand.grid(ring = unique(events$ring), year = years)

full <- all_combos %>%
  left_join(events, by = c("ring","year")) %>%
  mutate(state = replace_na(state, 0))



#Build encounter histories

enc_hist <- full %>%
  arrange(ring, year) %>%
  tidyr::spread(key = year, value = state, fill = 0)

# collapse to history strings
inp <- enc_hist %>%
  mutate(
    caphist = apply(select(., -ring), 1, paste0, collapse = ""),
    line = paste0(caphist, " 1 ;")   # freq=1, add ;
  ) %>%
  pull(line)


# Write to .inp file

writeLines(inp, "peregrine_multistate_final.inp")



