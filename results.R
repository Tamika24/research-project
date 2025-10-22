#####our results #####
library(readxl)
library(dplyr)
library(ggplot2)

# Load Data
df <- read_excel("real_estimatesNB4.xlsx")

# Prepare survival data
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

# Set factor order for plotting 
surv$AgeClass <- factor(
  surv$AgeClass,
  levels = c("Young Nonbreeder", "Older Nonbreeder", "Breeder")
)

# Plot with legend and colour distinction 
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


#####psi&dectection#####
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

# Plot Detection 
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
