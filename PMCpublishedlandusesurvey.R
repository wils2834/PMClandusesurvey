# DRACE 2024 — PMC (Potentially Mineralizable Carbon)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(ggplot2)

# PART 1 Import and average the 3 reps per UID

pmc24 <- read.csv("PMC_DR2024_all_calcs_01July2025_complete.csv", stringsAsFactors = FALSE)

# Same abbreviation issue as ACE: "SH"/"RG" alongside spelled-out labels.
pmc24 <- pmc24 %>%
  mutate(type = recode(type, "SH" = "Soil_Health", "RG" = "Rotational_Grazing"))

# Average the 3 technical replicates per UID. na.rm = TRUE handles the few rows where one of the 3 reps came back NA (a failed reading),
pmc24_avg <- pmc24 %>%
  dplyr::rename(UID = sample.id, Farmer = farmer, Type = type) %>%
  group_by(UID, Farmer, Type) %>%
  summarise(ugC.g.day = mean(ugC.g.day, na.rm = TRUE), .groups = "drop")

# County isn't in this reduced table, but it's recoverable from the UID
# prefix (DRWI = Grant, DRMN = Fillmore), consistent with every other dataset in this project
pmc24_avg <- pmc24_avg %>%
  mutate(County = ifelse(grepl("^DRMN", UID), "Fillmore", "Grant"))

pmc24_avg$Type <- factor(
  pmc24_avg$Type,
  levels = c("Conventional", "Soil_Health", "Rotational_Grazing", "New_CRP", "Old_CRP")
)

# Same Field-ID fix as ACE/POXC: RLH_Inc and UW_AG 
pmc24_avg <- pmc24_avg %>%
  mutate(Field = paste(Farmer, Type, sep = "_"))

cat("UIDs after averaging:", nrow(pmc24_avg), "\n")
cat("Distinct fields:", n_distinct(pmc24_avg$Field), "(expect 31)\n")
stopifnot(n_distinct(pmc24_avg$Field) == 31)

# PART 2 Mixed model
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

# PART 3  Plots

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
    axis.title.y = element_text(size = 21, face = "bold"),
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
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y  = element_text(size = 12, face = "bold", color = "black"),
    axis.title.y = element_text(size = 17, face = "bold"),
    strip.text   = element_text(size = 13, face = "bold"),
    plot.title   = element_text(size = 15, face = "bold", hjust = 0.5),
    legend.text  = element_text(size = 12, face = "bold", color = "black"),
    legend.title = element_text(size = 13, face = "bold", color = "black")
  )
print(p_county)
ggsave("PMC_by_county_boxplot.png", p_county, width = 12, height = 7, dpi = 300)

cat("\nSaved PMC_overall_boxplot.png and PMC_by_county_boxplot.png\n")

# ---- Metadata export ----------------------------------------------------
pairs(emm_PMC, adjust = "sidak")
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
                LMM_F, LMM_p, Type, Mean,SE, CLD)

write.csv(PMC_meta, "PMC_metadata.csv", row.names = FALSE)
cat("Saved PMC_metadata.csv\n")

# PART 4  Indexed (continuous management score) version
scores_full <- read.csv("C:\\Users\\swils\\Downloads\\01 Projects\\2024 CRP\\Landusesurveygithub\\managementoflandusesurvey2024\\soil_health_scores_2024_all_types.csv", stringsAsFactors = FALSE)

# farm_name -> lab-data Farmer name (RG/CRP rows only; Conventional/SH
# Farmer values in the CSV are already correct)
rg_crp_farmer_map <- c(
  "5G_Farms"           = "5G_Farms_LLC",
  "Cliff"              = "Cliff",
  "Cliff_2"            = "Cliff_2",
  "Collins"            = "Steve_Collins",
  "Derouin"            = "Michael_Derouin",
  "Eiken"              = "Kay_Eiken",
  "Eiken_2"            = "Kay_Eiken_2",
  "Haugen"             = "Bonnie_Haugen",
  "Lee"                = "Carter_Lee",
  "Lee_2"              = "Carter_Lee_2",
  "McLean"             = "Maurice_McLean",
  "Menne"              = "Kevin_Menne",
  "Mitchell"           = "Thomas_Mitchell",
  "Reichling_Brothers" = "Reichling_Brothers",
  "RLH_Inc"            = "RLH_Inc",
  "RLH_Inc_2"          = "RLH_Inc_2",
  "Schmidt"            = "Dennis_Schmidt",
  "UW_AG"              = "UW_AG",
  "UW_AG_2"            = "UW_AG_2"
)

# CRP contract-age split, from the CRP Fields sheet's "treatment" column
crp_type_map <- c(
  "Schmidt"             = "Old_CRP",
  "Reichling_Brothers"  = "Old_CRP",
  "McLean"              = "Old_CRP",
  "Eiken"               = "Old_CRP",
  "Eiken_2"             = "Old_CRP",
  "Derouin"             = "Old_CRP",
  "Collins"             = "New_CRP",
  "Menne"               = "New_CRP",
  "5G_Farms"            = "New_CRP",
  "Mitchell"            = "New_CRP",
  "RLH_Inc"             = "New_CRP",
  "RLH_Inc_2"           = "New_CRP"
)

scores_full <- scores_full %>%
  mutate(
    Farmer = ifelse(farm_name %in% names(rg_crp_farmer_map),
                    rg_crp_farmer_map[farm_name],
                    Farmer),
    Type = ifelse(Type == "CRP" & farm_name %in% names(crp_type_map),
                  crp_type_map[farm_name],
                  Type)
  )
scores_full <- scores_full %>%
  mutate(Type = recode(Type,
                       "CRP_New" = "New_CRP",
                       "CRP_Old" = "Old_CRP"))
cat("Corrected Farmer/Type for RG/CRP rows:\n")
print(scores_full %>% filter(Type %in% c("Rotational_Grazing","Old_CRP","New_CRP")) %>%
        dplyr::select(farm_name, Farmer, Type))

# Join management score onto the PMC data 
pmc24_indexed <- pmc24_avg %>%
  left_join(
    scores_full %>% dplyr::select(Farmer, Type, soil_health_score),
    by = c("Farmer", "Type")
  )

cat("\nRows with missing soil_health_score after join:",
    sum(is.na(pmc24_indexed$soil_health_score)), "\n")

#  Mixed model with continuous management score 

model_PMC_idx     <- lmer(ugC.g.day       ~ soil_health_score + (1 | Field), data = pmc24_indexed)
model_PMC_idx_log <- lmer(log(ugC.g.day)  ~ soil_health_score + (1 | Field), data = pmc24_indexed)

shapiro_idx_raw <- shapiro.test(residuals(model_PMC_idx))
shapiro_idx_log <- shapiro.test(residuals(model_PMC_idx_log))
cat("Indexed model - Shapiro-Wilk, raw-scale residuals:  p =", signif(shapiro_idx_raw$p.value, 3), "\n")
cat("Indexed model - Shapiro-Wilk, log-scale residuals:  p =", signif(shapiro_idx_log$p.value, 3), "\n")

use_log_idx <- shapiro_idx_log$p.value > shapiro_idx_raw$p.value
final_model_idx <- if (use_log_idx) model_PMC_idx_log else model_PMC_idx
cat("Using", if (use_log_idx) "LOG-transformed" else "RAW-scale", "model for indexed inference.\n\n")

summary(final_model_idx)
anova_PMC_idx <- anova(final_model_idx)
print(anova_PMC_idx)
library(MuMIn)
# Plot: scatter + fitted line

fixed_est   <- summary(final_model_idx)$coefficients["soil_health_score", "Estimate"]
fixed_p     <- anova_PMC_idx$`Pr(>F)`[1]
r2_marginal <- MuMIn::r.squaredGLMM(final_model_idx)[1, "R2m"]

sig_lab <- paste0("R\u00b2(marginal) = ", round(r2_marginal, 2),
                  "  (", ifelse(fixed_p < 0.001, "p < 0.001", paste0("p = ", round(fixed_p, 3))), ")")

plot_data <- pmc24_indexed %>%
  mutate(y_plot = if (use_log_idx) log(ugC.g.day) else ugC.g.day)

p_indexed <- ggplot(plot_data, aes(x = soil_health_score, y = y_plot)) +
  geom_smooth(method = "lm", color = "gray50", fill = "gray85",
              alpha = 0.3, linewidth = 0.8, se = TRUE) +
  geom_point(aes(color = Type, shape = County), size = 3.5, alpha = 0.9) +
  annotate("text", x = Inf, y = Inf, label = sig_lab,
           hjust = 1.05, vjust = 1.8, size = 4.2, color = "gray30", fontface = "italic") +
  scale_color_manual(values = fill_colors, labels = type_labels, name = "Management") +
  scale_shape_manual(values = c("Grant" = 16, "Fillmore" = 17), name = "County") +
  labs(
    title = "Potentially Mineralizable Carbon vs. Soil Health Management Score",
    x = "Soil Health Management Score  (higher = more favorable)",
    y = if (use_log_idx) expression(paste("log(", mu, "g C g"^-1, " day"^-1, ")")) else expression(paste(mu, "g C g"^-1, " day"^-1))
  ) +
  theme_bw() +
  theme(
    axis.text       = element_text(size = 12, face = "bold", color = "black"),
    axis.title      = element_text(size = 13, face = "bold"),
    plot.title      = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title    = element_text(size = 12, face = "bold"),
    legend.text     = element_text(size = 11),
    legend.position = "bottom",
    legend.key.size = unit(0.8, "lines")
  )

print(p_indexed)
ggsave("PMC_vs_soil_health_score.png", p_indexed, width = 8.5, height = 6.5, dpi = 300)

cat("\nSaved PMC_vs_soil_health_score.png\n")

# ---- Metadata export (indexed version) ------------------------------------

PMC_meta_indexed <- data.frame(
  Metric         = "PMC",
  Model          = "indexed (soil_health_score continuous)",
  Scale          = if (use_log_idx) "log" else "raw",
  Shapiro_raw_p  = signif(shapiro_idx_raw$p.value, 3),
  Shapiro_log_p  = signif(shapiro_idx_log$p.value, 3),
  Fixed_Estimate = round(fixed_est, 4),
  LMM_F          = round(anova_PMC_idx$`F value`[1], 3),
  LMM_p          = signif(fixed_p, 3),
  R2_marginal    = round(r2_marginal, 3)
)

write.csv(PMC_meta_indexed, "PMC_metadata_indexed.csv", row.names = FALSE)
cat("Saved PMC_metadata_indexed.csv\n")


# PART 5 — Field-averaged scatter (n=31, simple lm, ordinary R^2)

pmc_field_avg <- pmc24_indexed %>%
  group_by(Farmer, Type, County, soil_health_score) %>%
  summarise(PMC = mean(ugC.g.day, na.rm = TRUE), .groups = "drop")

lm_fit_avg  <- lm(PMC ~ soil_health_score, data = pmc_field_avg)
r2_avg      <- summary(lm_fit_avg)$r.squared
pval_avg    <- summary(lm_fit_avg)$coefficients[2, 4]
sig_lab_avg <- paste0("R\u00b2 = ", round(r2_avg, 2),
                      "  (", ifelse(pval_avg < 0.001, "p < 0.001", paste0("p = ", round(pval_avg, 3))), ")")

cat("Field-averaged PMC model: n =", nrow(pmc_field_avg),
    ", R2 =", round(r2_avg, 3), ", p =", signif(pval_avg, 3), "\n")

p_field_avg <- ggplot(pmc_field_avg, aes(x = soil_health_score, y = PMC)) +
  geom_smooth(method = "lm", color = "gray50", fill = "gray85",
              alpha = 0.3, linewidth = 0.8, se = TRUE) +
  geom_point(aes(color = Type, shape = County), size = 4, alpha = 0.9) +
  annotate("text", x = Inf, y = Inf, label = sig_lab_avg,
           hjust = 1.05, vjust = 1.8, size = 4.2, color = "gray30", fontface = "italic") +
  scale_color_manual(values = fill_colors, labels = type_labels, name = "Management") +
  scale_shape_manual(values = c("Grant" = 16, "Fillmore" = 17), name = "County") +
  labs(
    title = "Potentially Mineralizable Carbon by Management",
    x = "Soil Health Management Score  (higher = more favorable)",
    y = expression(bold(paste(mu, "g C g"^-1, " day"^-1)))
  ) +
  theme_bw() +
  theme(
    axis.text       = element_text(size = 12, face = "bold", color = "black"),
    axis.title      = element_text(size = 13, face = "bold"),
    plot.title      = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title    = element_text(size = 12, face = "bold"),
    legend.text     = element_text(size = 11),
    legend.position = "bottom",
    legend.key.size = unit(0.8, "lines")
  )

print(p_field_avg)
ggsave("PMC_field_avg_vs_score.png", p_field_avg, width = 8.5, height = 6.5, dpi = 300)
cat("Saved PMC_field_avg_vs_score.png\n")

