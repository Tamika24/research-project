#####our results #####
# --- 1. Load Packages ---
library(readxl)
library(dplyr)
library(ggplot2)

# --- 2. Load Data ---
df <- read_excel("real_estimatesNB4.xlsx")

# --- 3. Prepare survival data ---
surv <- df %>%
  filter(grepl("^S", Parameter)) %>%
  mutate(
    Stratum = case_when(
      grepl("s1", Parameter) ~ "Nonbreeder",
      grepl("s2", Parameter) ~ "Breeder",
      TRUE ~ "Unknown"
    ),
    AgeClass = case_when(
      Stratum == "Breeder" ~ "Breeder",
      grepl("a0", Parameter) ~ "Young Nonbreeder",
      grepl("a1", Parameter) ~ "Older Nonbreeder",
      TRUE ~ "Constant"
    )
  ) %>%
  group_by(Stratum, AgeClass) %>%
  summarise(
    estimate = mean(estimate, na.rm = TRUE),
    lcl = mean(lcl, na.rm = TRUE),
    ucl = mean(ucl, na.rm = TRUE),
    .groups = "drop"
  )

# --- 4. Set factor order for plotting ---
surv$AgeClass <- factor(
  surv$AgeClass,
  levels = c("Young Nonbreeder", "Older Nonbreeder", "Breeder")
)

# --- 5. Plot with legend and colour distinction ---
plot_surv <- ggplot(surv, aes(x = AgeClass, y = estimate, colour = Stratum)) +
  geom_point(size = 4, position = position_dodge(0.3)) +
  geom_errorbar(
    aes(ymin = lcl, ymax = ucl),
    width = 0.15,
    position = position_dodge(0.3)
  ) +
  scale_colour_manual(
    values = c("Nonbreeder" = "#0072B2", "Breeder" = "#D55E00"),
    name = "Breeding Status"
  ) +
  labs(
    x = "Age Class",
    y = "Apparent Survival Probability (Φ)",
    title = "Estimated Apparent Survival by Age Class and Breeding State"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
  )

print(plot_surv)


#####newwww#####
psi <- df %>%
  filter(grepl("^Psi", Parameter)) %>%
  mutate(
    # Extract age class correctly
    AgeNum = as.numeric(gsub(".*a([0-9]+).*", "\\1", Parameter)),
    # Collapse everything ≥4 into "4+"
    AgeClass = ifelse(AgeNum >= 3, "Class 4+", paste0("Class ", AgeNum+1))
  ) %>%
  group_by(AgeClass) %>%
  summarise(
    estimate = mean(estimate, na.rm = TRUE),
    lcl = mean(lcl, na.rm = TRUE),
    ucl = mean(ucl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(AgeClass)

print(psi)  # should show only 4 rows (Class 0–3 and Class 4+)

# --- Plot Ψ neatly
plot_psi <- ggplot(psi, aes(x = AgeClass, y = estimate)) +
  geom_point(size = 4, colour = "#0072B2") +
  geom_errorbar(aes(ymin = lcl, ymax = ucl),
                width = 0.15, colour = "#0072B2") +
  labs(
    x = "Age Class",
    y = "Transition Probability (Ψ)",
    title = "Transition Probability from Nonbreeder to Breeder by Age Class"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 14)

print(plot_psi)

detect <- df %>%
  filter(grepl("^p", Parameter)) %>%
  mutate(
    Stratum = case_when(
      grepl("s1", Parameter) ~ "Nonbreeder",
      grepl("s2", Parameter) ~ "Breeder",
      TRUE ~ "Unknown"
    ),
    Year = as.numeric(gsub(".*t([0-9]+).*", "\\1", Parameter)) + 1996
  ) %>%
  filter(!is.na(Year))

# --- 6. Plot Detection ---
plot_detect <- ggplot(detect, aes(x = Year, y = estimate, colour = Stratum)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.25) +
  scale_colour_manual(values = c("Nonbreeder" = "#0072B2", "Breeder" = "#D55E00")) +
  labs(
    x = "Year",
    y = "Detection Probability (p)",
    title = "Annual Detection Probability by Breeding State"
  ) +
  theme_minimal(base_size = 14)

print(plot_detect)


ggsave("plot_survival.png", plot_surv, width = 7, height = 5, dpi = 300)
ggsave("plot_transition.png", plot_psi, width = 7, height = 5, dpi = 300)
ggsave("plot_detection.png", plot_detect, width = 7, height = 5, dpi = 300)


#####4th model#####
library(dplyr)
library(stringr)
library(ggplot2)
# --- 1. Load data ---
df <- read_excel("real_estimates3.xlsx")

# 1️⃣ Extract time (T1, T2...) and year
df <- df %>%
  mutate(
    time = as.numeric(str_extract(Parameter, "(?<=T|t)\\d+")),
    Year = 1996 + time
  )

# 2️⃣ Filter apparent survival parameters (S s)
surv <- df %>%
  filter(grepl("^S s", Parameter)) %>%
  mutate(
    Stratum = case_when(
      grepl(" s1 ", Parameter) ~ "Nonbreeder",
      grepl(" s2 ", Parameter) ~ "Breeder",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(time), !is.na(Stratum))

# 3️⃣ Plot (styled like the paper)
plot_surv <- ggplot(surv, aes(x = Year, y = estimate, colour = Stratum, shape = Stratum)) +
  geom_point(size = 2.5, position = position_dodge(0.3)) +
  geom_line(position = position_dodge(0.3), linewidth = 0.8) +
  geom_errorbar(
    aes(ymin = lcl, ymax = ucl),
    width = 0.15,
    position = position_dodge(0.3),
    linewidth = 0.5
  ) +
  scale_colour_manual(values = c("Nonbreeder" = "#0072B2", "Breeder" = "#D55E00")) +
  scale_shape_manual(values = c("Nonbreeder" = 16, "Breeder" = 17)) +
  labs(
    x = "Year",
    y = "Apparent Survival Probability (Φ)",
    title = "Annual Apparent Survival by Breeding State"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
  )

print(plot_surv)

# Collapse across time (if multiple years per class)
psi <- df %>%
  filter(grepl("^Psi", Parameter)) %>%
  mutate(AgeClass = as.numeric(str_extract(Parameter, "(?<=a)\\d+"))) %>%
  group_by(AgeClass) %>%
  summarise(
    estimate = mean(estimate, na.rm = TRUE),
    lcl = mean(lcl, na.rm = TRUE),
    ucl = mean(ucl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(AgeClassLabel = ifelse(AgeClass >= 4, "Class 4+", paste0("Class ", AgeClass)))

# Plot ψ
plot_psi <- ggplot(psi, aes(x = AgeClassLabel, y = estimate)) +
  geom_point(size = 3, colour = "#0072B2") +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.2, colour = "#0072B2") +
  labs(
    x = "Age Class",
    y = "Transition Probability (ψ)",
    title = "Transition Probability from Nonbreeder to Breeder by Age Class"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
  )

print(plot_psi)

detect <- df %>%
  filter(grepl("^p ", Parameter)) %>%      # only p estimates
  mutate(
    Stratum = case_when(
      grepl(" s1 ", Parameter) ~ "Nonbreeder",
      grepl(" s2 ", Parameter) ~ "Breeder",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Stratum)) %>%
  arrange(Stratum, Year)


plot_detect <- ggplot(detect, aes(x = Year, y = estimate, colour = Stratum, shape = Stratum)) +
  geom_point(size = 2.8, position = position_dodge(0.3)) +
  geom_line(linewidth = 1, position = position_dodge(0.3)) +
  geom_errorbar(
    aes(ymin = lcl, ymax = ucl),
    width = 0.15,
    position = position_dodge(0.3),
    linewidth = 0.5
  ) +
  scale_colour_manual(values = c("Nonbreeder" = "#0072B2", "Breeder" = "#D55E00")) +
  scale_shape_manual(values = c("Nonbreeder" = 16, "Breeder" = 17)) +
  labs(
    x = "Year",
    y = "Detection Probability (p)",
    title = "Annual Detection Probability by Breeding State"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
  )

print(plot_detect)

#####other psi with both breeders and non breeders#####
library(dplyr)
library(stringr)
library(ggplot2)


psi <- df %>%
  filter(grepl("^Psi ", Parameter, ignore.case = TRUE)) %>%
  mutate(
    # Transition direction
    Transition = case_when(
      grepl("s1 to2", Parameter) ~ "Nonbreeder → Breeder",
      grepl("s2 to1", Parameter) ~ "Breeder → Nonbreeder",
      TRUE ~ NA_character_
    ),
    # Extract time (e.g., t12 -> 12)
    time = as.numeric(str_extract(Parameter, "(?<=t)\\d+")),
    Year = 1996 + time,
    # Optional: extract age class from c#
    AgeClass = str_extract(Parameter, "(?<=c)\\d+")
  ) %>%
  filter(!is.na(Transition), !is.na(time)) %>%
  arrange(Transition, Year, AgeClass)


psi_summary <- psi %>%
  group_by(Transition, Year) %>%
  summarise(
    estimate = mean(estimate, na.rm = TRUE),
    lcl = mean(lcl, na.rm = TRUE),
    ucl = mean(ucl, na.rm = TRUE),
    .groups = "drop"
  )


plot_psi <- ggplot(psi_summary, aes(x = Year, y = estimate, colour = Transition, shape = Transition)) +
  geom_point(size = 2.8, position = position_dodge(0.3)) +
  geom_line(linewidth = 1, position = position_dodge(0.3)) +
  geom_errorbar(
    aes(ymin = lcl, ymax = ucl),
    width = 0.15,
    position = position_dodge(0.3),
    linewidth = 0.5
  ) +
  scale_colour_manual(values = c(
    "Nonbreeder → Breeder" = "#009E73",
    "Breeder → Nonbreeder" = "#CC79A7"
  )) +
  scale_shape_manual(values = c(
    "Nonbreeder → Breeder" = 16,
    "Breeder → Nonbreeder" = 17
  )) +
  labs(
    x = "Year",
    y = "Transition Probability (ψ)",
    title = "Annual Transition Probability Between Breeding States"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
  )

print(plot_psi)
