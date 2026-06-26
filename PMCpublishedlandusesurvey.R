# DRACE 2024 — PMC (Potentially Mineralizable Carbon)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(ggplot2)

# PART 1 — Import, recode, and average the 3 reps per UID

pmc24 <- read.csv("PMC_DR2024_all_calcs_01July2025_complete.csv", stringsAsFactors = FALSE)

# Same abbreviation issue as ACE: "SH"/"RG" alongside spelled-out labels.
pmc24 <- pmc24 %>%
  mutate(type = recode(type, "SH" = "Soil_Health", "RG" = "Rotational_Grazing"))

# Average the 3 technical replicates per UID. na.rm = TRUE handles the
# few rows where one of the 3 reps came back NA (a failed reading),
# rather than discarding the whole UID.
pmc24_avg <- pmc24 %>%
  dplyr::rename(UID = sample.id, Farmer = farmer, Type = type) %>%
  group_by(UID, Farmer, Type) %>%
  summarise(ugC.g.day = mean(ugC.g.day, na.rm = TRUE), .groups = "drop")

# County isn't in this reduced table, but it's recoverable from the UID
# prefix (DRWI = Grant, DRMN = Fillmore), consistent with every other
# dataset in this project. Flagging this as inferred, not given directly.
pmc24_avg <- pmc24_avg %>%
  mutate(County = ifelse(grepl("^DRMN", UID), "Fillmore", "Grant"))

pmc24_avg$Type <- factor(
  pmc24_avg$Type,
  levels = c("Conventional", "Soil_Health", "Rotational_Grazing", "New_CRP", "Old_CRP")
)

# Same Field-ID fix as ACE/POXC: RLH_Inc and UW_AG each farm two
# different fields under two different management types.
pmc24_avg <- pmc24_avg %>%
  mutate(Field = paste(Farmer, Type, sep = "_"))

cat("UIDs after averaging:", nrow(pmc24_avg), "\n")
cat("Distinct fields:", n_distinct(pmc24_avg$Field), "(expect 31)\n")
stopifnot(n_distinct(pmc24_avg$Field) == 31)

# PART 2 — Mixed model
model_PMC     <- lmer(ugC.g.day ~ Type + (1 | Field), data = pmc24_avg)
model_PMC_log <- lmer(log(ugC.g.day) ~ Type + (1 | Field), data = pmc24_avg)

shapiro_raw <- shapiro.test(residuals(model_PMC))
shapiro_log <- shapiro.test(residuals(model_PMC_log))
cat("Shapiro-Wilk, raw-scale residuals:  p =", signif(shapiro_raw$p.value, 3), "\n")
cat("Shapiro-Wilk, log-scale residuals:  p =", signif(shapiro_log$p.value, 3), "\n")

use_log <- shapiro_log$p.value > shapiro_raw$p.value
final_model <- if (use_log) model_PMC_log else model_PMC
cat("Using", if (use_log) "LOG-transformed" else "RAW-scale", "model for inference.\n\n")

summary(final_model)
#emmeans
emm_PMC       <- emmeans(final_model, "Type")
emm_PMC_table <- summary(emm_PMC, type = "response")
cld_PMC       <- cld(emm_PMC, Letters = letters, adjust = "sidak")

print(emm_PMC_table)
print(cld_PMC)

mean_col <- if ("response" %in% names(emm_PMC_table)) "response" else "emmean"

PMC_summary <- as.data.frame(emm_PMC_table) %>%
  dplyr::select(Type, Mean = !!mean_col) %>%
  dplyr::left_join(
    as.data.frame(cld_PMC) %>% dplyr::select(Type, CLD = .group),
    by = "Type"
  )

print(PMC_summary)

## PART 3 — Plots

type_labels <- c(
  "Conventional"        = "Conventional Cropping & Tillage",
  "Soil_Health"         = "No Till and/or Cover Crops",
  "Rotational_Grazing"  = "Rotational Grazing",
  "New_CRP"             = "0-10 years CRP",
  "Old_CRP"             = "10+ years CRP"
)

fill_colors <- c(
  "Conventional"        = "#E69F00",
  "Soil_Health"         = "#56B4E9",
  "Rotational_Grazing"  = "#009E73",
  "New_CRP"             = "#F0E442",
  "Old_CRP"             = "#0072B2"
)

label_positions <- pmc24_avg %>%
  group_by(Type) %>%
  summarise(y_pos = max(ugC.g.day, na.rm = TRUE) * 1.05, .groups = "drop") %>%
  dplyr::left_join(PMC_summary %>% dplyr::select(Type, CLD), by = "Type")

p_overall <- ggplot(pmc24_avg, aes(x = Type, y = ugC.g.day, fill = Type)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  geom_text(
    data = label_positions,
    aes(x = Type, y = y_pos, label = CLD),
    inherit.aes = FALSE, size = 6, fontface = "bold", color = "black"
  ) +
  scale_x_discrete(labels = type_labels) +
  scale_fill_manual(values = fill_colors, guide = "none") +
  labs(
    title = "Potentially Mineralizable Carbon by Management",
    x = NULL,
    y = expression(paste(mu, "g C g"^-1, " day"^-1))
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 13, face = "bold", color = "black"),
    axis.text.y = element_text(size = 13, face = "bold", color = "black"),
    axis.title.y = element_text(size = 15, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 50)
  )
print(p_overall)
ggsave("PMC_overall_boxplot.png", p_overall, width = 10, height = 7, dpi = 300)

label_positions_county <- pmc24_avg %>%
  group_by(County, Type) %>%
  summarise(y_pos = max(ugC.g.day, na.rm = TRUE) * 1.05, .groups = "drop") %>%
  dplyr::left_join(PMC_summary %>% dplyr::select(Type, CLD), by = "Type")

p_county <- ggplot(pmc24_avg, aes(x = Type, y = ugC.g.day, fill = Type)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  geom_text(
    data = label_positions_county,
    aes(x = Type, y = y_pos, label = CLD),
    inherit.aes = FALSE, size = 5, fontface = "bold", color = "black"
  ) +
  facet_wrap(~ County) +
  scale_fill_manual(values = fill_colors, labels = type_labels, name = "Management") +
  labs(
    title = "Potentially Mineralizable Carbon by Management and County",
    x = NULL,
    y = expression(paste(mu, "g C g"^-1, " day"^-1))
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title.y = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
  )
print(p_county)
ggsave("PMC_by_county_boxplot.png", p_county, width = 12, height = 7, dpi = 300)

cat("\nSaved PMC_overall_boxplot.png and PMC_by_county_boxplot.png\n")

# ---- Metadata export ----------------------------------------------------
anova_PMC <- anova(final_model)

PMC_meta <- as.data.frame(emm_PMC_table) %>%
  dplyr::rename(Mean = if ("response" %in% names(emm_PMC_table)) "response" else "emmean") %>%
  dplyr::left_join(
    as.data.frame(cld_PMC) %>% dplyr::select(Type, CLD = .group),
    by = "Type"
  ) %>%
  mutate(
    Metric         = "PMC",
    Scale          = if (use_log) "log" else "raw",
    Shapiro_raw_p  = signif(shapiro_raw$p.value, 3),
    Shapiro_log_p  = signif(shapiro_log$p.value, 3),
    LMM_F          = round(anova_PMC$`F value`[1], 3),
    LMM_p          = signif(anova_PMC$`Pr(>F)`[1], 3)
  ) %>%
  dplyr::select(Metric, Scale, Shapiro_raw_p, Shapiro_log_p,
                LMM_F, LMM_p, Type, Mean, CLD)

write.csv(PMC_meta, "PMC_metadata.csv", row.names = FALSE)
cat("Saved PMC_metadata.csv\n")