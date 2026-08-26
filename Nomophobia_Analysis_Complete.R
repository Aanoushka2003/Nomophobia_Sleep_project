# ============================================================
# Digital Dependence and Its Effect on Sleep Hygiene
# A Study on Nomophobia in University Students
# Author: Aanoushka Garg, Hindu College, University of Delhi
# Complete Analysis Script
# ============================================================

# ── Install packages (run once, skip if already installed) ──
install.packages("readxl")
install.packages("psych")
install.packages("pwr")
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("gridExtra")
install.packages("car")

# ── Load all libraries ───────────────────────────────────────
library(readxl)
library(psych)
library(pwr)
library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(car)

# ============================================================
# STEP 0: Load Data
# ============================================================
# Set your working directory to wherever your Excel file is
# e.g. setwd("C:/Users/YourName/Documents")

Biostatistics_Research <- read_excel("Biostatistics_Research.xlsx")

# Quick check
head(Biostatistics_Research)
str(Biostatistics_Research)
dim(Biostatistics_Research)  # should be 274 x 17

# ============================================================
# STEP 1: Compute Composite Scores
# ============================================================

# Phone Use Score (range: 3-15, higher = more phone dependency)
Biostatistics_Research$phone_use <- Biostatistics_Research$phone_night +
                                    Biostatistics_Research$phone_morning +
                                    Biostatistics_Research$phone_overuse

# Nomophobia Score (range: 4-20, higher = more nomophobia)
Biostatistics_Research$nomophobia_score <- Biostatistics_Research$nomophobia_notify +
                                           Biostatistics_Research$nomophobia_battery +
                                           Biostatistics_Research$nomophobia_internet +
                                           Biostatistics_Research$nomophobia_connected

# Sleep Quality Score (range: 3-15, higher = BETTER sleep)
# sleep_awake_rev is already reverse scored in Excel
Biostatistics_Research$sleep_score <- Biostatistics_Research$sleep_quality +
                                      Biostatistics_Research$sleep_latency +
                                      Biostatistics_Research$sleep_awake_rev

# Anxiety Score (range: 3-9, higher = more anxiety)



# Verify ranges
summary(Biostatistics_Research[, c("phone_use", "nomophobia_score",
                                    "sleep_score", "anxiety_score")])

# ============================================================
# STEP 2: Power Analysis
# ============================================================

# A priori power analysis for correlation (primary test)
pwr.r.test(
  r = 0.3,           # medium effect size
  sig.level = 0.05,
  power = 0.80,
  alternative = "two.sided"
)

# A priori power analysis for t-test (gender comparisons)
pwr.t.test(
  d = 0.5,           # medium Cohen's d
  sig.level = 0.05,
  power = 0.80,
  type = "two.sample",
  alternative = "two.sided"
)

# ============================================================
# STEP 3: Reliability Analysis (Cronbach's Alpha)
# ============================================================

# Phone Use Scale (3 items)
phone_items <- Biostatistics_Research[, c("phone_night",
                                           "phone_morning",
                                           "phone_overuse")]
alpha_phone <- alpha(phone_items)
cat("--- Phone Use Alpha ---\n")
print(alpha_phone$total)

# Nomophobia Scale (4 items)
nomo_items <- Biostatistics_Research[, c("nomophobia_notify",
                                          "nomophobia_battery",
                                          "nomophobia_internet",
                                          "nomophobia_connected")]
alpha_nomo <- alpha(nomo_items)
cat("\n--- Nomophobia Alpha ---\n")
print(alpha_nomo$total)

# Sleep Quality Scale (3 items)
sleep_items <- Biostatistics_Research[, c("sleep_quality",
                                           "sleep_latency",
                                           "sleep_awake_rev")]
alpha_sleep <- alpha(sleep_items)
cat("\n--- Sleep Quality Alpha ---\n")
print(alpha_sleep$total)

# Alpha if item dropped (for sleep scale justification)
cat("\n--- Sleep Alpha if Item Dropped ---\n")
print(alpha_sleep$alpha.drop)

# Inter-item correlations for sleep scale
cat("\n--- Sleep Inter-item Correlations ---\n")
print(cor(sleep_items))

# Anxiety Scale (3 items)
anxiety_items <- Biostatistics_Research[, c("anxiety_nervous",
                                             "anxiety_control",
                                             "anxiety_worry")]
alpha_anxiety <- alpha(anxiety_items)
cat("\n--- Anxiety Alpha ---\n")
print(alpha_anxiety$total)

# ============================================================
# STEP 4: Descriptive Statistics
# ============================================================

# Composite score descriptives
cat("--- Composite Score Descriptives ---\n")
score_descriptives <- describe(Biostatistics_Research[, c("phone_use",
                                                           "nomophobia_score",
                                                           "sleep_score",
                                                           "anxiety_score")])
print(score_descriptives)

# Gender distribution
cat("\n--- Gender Distribution ---\n")
gender_table <- table(Biostatistics_Research$Gender)
print(gender_table)
print(round(prop.table(gender_table) * 100, 2))

# Age summary
cat("\n--- Age Summary ---\n")
print(describe(Biostatistics_Research$Age))

# Score categories based on Appendix B cutoffs
# Phone Use: 3-6 Low, 7-11 Moderate, 12-15 High
Biostatistics_Research$phone_cat <- cut(Biostatistics_Research$phone_use,
                                         breaks = c(2, 6, 11, 15),
                                         labels = c("Low", "Moderate", "High"))

# Nomophobia: 4-8 Very Low, 9-14 Moderate, 15-20 High
Biostatistics_Research$nomo_cat <- cut(Biostatistics_Research$nomophobia_score,
                                        breaks = c(3, 8, 14, 20),
                                        labels = c("Very Low", "Moderate", "High"))

# Sleep Quality: 3-6 Poor, 7-11 Moderate, 12-15 Good
Biostatistics_Research$sleep_cat <- cut(Biostatistics_Research$sleep_score,
                                         breaks = c(2, 6, 11, 15),
                                         labels = c("Poor", "Moderate", "Good"))

# Anxiety: 3-5 Minimal, 6-7 Moderate, 8-9 High
Biostatistics_Research$anxiety_cat <- cut(Biostatistics_Research$anxiety_score,
                                           breaks = c(2, 5, 7, 9),
                                           labels = c("Minimal", "Moderate", "High"))

# Category frequencies
cat("\n--- Phone Use Categories ---\n")
print(table(Biostatistics_Research$phone_cat))
print(round(prop.table(table(Biostatistics_Research$phone_cat)) * 100, 2))

cat("\n--- Nomophobia Categories ---\n")
print(table(Biostatistics_Research$nomo_cat))
print(round(prop.table(table(Biostatistics_Research$nomo_cat)) * 100, 2))

cat("\n--- Sleep Quality Categories ---\n")
print(table(Biostatistics_Research$sleep_cat))
print(round(prop.table(table(Biostatistics_Research$sleep_cat)) * 100, 2))

cat("\n--- Anxiety Categories ---\n")
print(table(Biostatistics_Research$anxiety_cat))
print(round(prop.table(table(Biostatistics_Research$anxiety_cat)) * 100, 2))

# ============================================================
# STEP 5: Normality Testing (Shapiro-Wilk)
# ============================================================

sw_phone   <- shapiro.test(Biostatistics_Research$phone_use)
sw_nomo    <- shapiro.test(Biostatistics_Research$nomophobia_score)
sw_sleep   <- shapiro.test(Biostatistics_Research$sleep_score)
sw_anxiety <- shapiro.test(Biostatistics_Research$anxiety_score)

cat("Shapiro-Wilk Normality Tests\n")
cat("============================\n")
cat(sprintf("Phone Use:    W = %.4f, p = %.4f\n",
            sw_phone$statistic, sw_phone$p.value))
cat(sprintf("Nomophobia:   W = %.4f, p = %.4f\n",
            sw_nomo$statistic, sw_nomo$p.value))
cat(sprintf("Sleep Score:  W = %.4f, p = %.4f\n",
            sw_sleep$statistic, sw_sleep$p.value))
cat(sprintf("Anxiety:      W = %.4f, p = %.4f\n",
            sw_anxiety$statistic, sw_anxiety$p.value))

# Q-Q Plots
par(mfrow = c(2, 2))
qqnorm(Biostatistics_Research$phone_use,       main = "Q-Q Plot: Phone Use")
qqline(Biostatistics_Research$phone_use,       col = "red")
qqnorm(Biostatistics_Research$nomophobia_score, main = "Q-Q Plot: Nomophobia")
qqline(Biostatistics_Research$nomophobia_score, col = "red")
qqnorm(Biostatistics_Research$sleep_score,     main = "Q-Q Plot: Sleep Quality")
qqline(Biostatistics_Research$sleep_score,     col = "red")
qqnorm(Biostatistics_Research$anxiety_score,   main = "Q-Q Plot: Anxiety")
qqline(Biostatistics_Research$anxiety_score,   col = "red")
par(mfrow = c(1, 1))

# ============================================================
# STEP 6: Correlation Analysis (Spearman)
# Testing H01, H02, H03, H04
# ============================================================

cat("\nH01: Phone Use vs Sleep Quality\n")
cor_phone_sleep <- cor.test(Biostatistics_Research$phone_use,
                             Biostatistics_Research$sleep_score,
                             method = "spearman")
print(cor_phone_sleep)

cat("\nH02: Nomophobia vs Sleep Quality\n")
cor_nomo_sleep <- cor.test(Biostatistics_Research$nomophobia_score,
                            Biostatistics_Research$sleep_score,
                            method = "spearman")
print(cor_nomo_sleep)

cat("\nH03: Nomophobia vs Anxiety\n")
cor_nomo_anxiety <- cor.test(Biostatistics_Research$nomophobia_score,
                              Biostatistics_Research$anxiety_score,
                              method = "spearman")
print(cor_nomo_anxiety)

cat("\nH04: Anxiety vs Sleep Quality\n")
cor_anxiety_sleep <- cor.test(Biostatistics_Research$anxiety_score,
                               Biostatistics_Research$sleep_score,
                               method = "spearman")
print(cor_anxiety_sleep)

# Full correlation matrix
cat("\n--- Full Spearman Correlation Matrix ---\n")
cor_matrix <- corr.test(Biostatistics_Research[, c("phone_use",
                                                    "nomophobia_score",
                                                    "sleep_score",
                                                    "anxiety_score")],
                         method = "spearman")
print(cor_matrix$r)
cat("\n--- P-values ---\n")
print(cor_matrix$p)

# ============================================================
# STEP 7: Gender Comparisons (Mann-Whitney U)
# Testing H05 and H06
# ============================================================

# Keep only Female and Male (Others n=2, Prefer not to say n=3)
df_gender <- subset(Biostatistics_Research,
                    Gender %in% c("Female", "Male"))
cat("\nSample sizes after filtering:\n")
print(table(df_gender$Gender))

# H05: Gender vs Nomophobia
cat("\nH05: Gender vs Nomophobia (Mann-Whitney U)\n")
mw_nomo_gender <- wilcox.test(nomophobia_score ~ Gender,
                               data = df_gender,
                               exact = FALSE)
print(mw_nomo_gender)
cat("\nNomophobia means by gender:\n")
print(tapply(df_gender$nomophobia_score, df_gender$Gender, mean))
print(tapply(df_gender$nomophobia_score, df_gender$Gender, sd))

# H06: Gender vs Sleep Quality
cat("\nH06: Gender vs Sleep Quality (Mann-Whitney U)\n")
mw_sleep_gender <- wilcox.test(sleep_score ~ Gender,
                                data = df_gender,
                                exact = FALSE)
print(mw_sleep_gender)
cat("\nSleep Quality means by gender:\n")
print(tapply(df_gender$sleep_score, df_gender$Gender, mean))
print(tapply(df_gender$sleep_score, df_gender$Gender, sd))

# ============================================================
# STEP 8: Regression Analysis
# Testing H07
# ============================================================

# Simple regression: Phone Use -> Sleep Quality
cat("\nSimple Regression: Phone Use -> Sleep Quality\n")
model_phone <- lm(sleep_score ~ phone_use,
                  data = Biostatistics_Research)
summary(model_phone)

# Simple regression: Nomophobia -> Sleep Quality
cat("\nSimple Regression: Nomophobia -> Sleep Quality\n")
model_nomo <- lm(sleep_score ~ nomophobia_score,
                 data = Biostatistics_Research)
summary(model_nomo)

# Multiple regression: Phone Use + Nomophobia -> Sleep Quality
cat("\nMultiple Regression: Phone Use + Nomophobia -> Sleep Quality\n")
model_multiple <- lm(sleep_score ~ phone_use + nomophobia_score,
                     data = Biostatistics_Research)
summary(model_multiple)

# Extended model: + Anxiety
cat("\nExtended Regression: + Anxiety -> Sleep Quality\n")
model_extended <- lm(sleep_score ~ phone_use + nomophobia_score +
                       anxiety_score,
                     data = Biostatistics_Research)
summary(model_extended)

# Regression diagnostic plots
par(mfrow = c(2, 2))
plot(model_multiple, main = "Regression Diagnostics: Multiple Model")
par(mfrow = c(1, 1))

# VIF check for multicollinearity
cat("\nVariance Inflation Factors:\n")
print(vif(model_multiple))

# AIC model comparison
cat("\nModel Comparison (AIC):\n")
cat(sprintf("Simple (Phone only):      AIC = %.2f\n", AIC(model_phone)))
cat(sprintf("Simple (Nomophobia only): AIC = %.2f\n", AIC(model_nomo)))
cat(sprintf("Multiple (Phone + Nomo):  AIC = %.2f\n", AIC(model_multiple)))
cat(sprintf("Extended (+ Anxiety):     AIC = %.2f\n", AIC(model_extended)))

# ============================================================
# STEP 9: Visualisations
# ============================================================

# ── 9.1 Gender Pie Chart ─────────────────────────────────
gender_df <- as.data.frame(table(Biostatistics_Research$Gender))
colnames(gender_df) <- c("Gender", "Count")
gender_df$Percentage <- round(gender_df$Count /
                               sum(gender_df$Count) * 100, 1)
gender_df$Label <- paste0(gender_df$Gender, "\n",
                           gender_df$Percentage, "%")

ggplot(gender_df, aes(x = "", y = Count, fill = Gender)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 3.5) +
  scale_fill_manual(values = c("#E8A0BF", "#89CFF0",
                                "#B0E0A8", "#FFD580")) +
  labs(title = "Gender Distribution of Participants",
       subtitle = "n = 274") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,
                                  face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11))

# ── 9.2 Age Histogram ────────────────────────────────────
ggplot(Biostatistics_Research, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "#89CFF0",
                 color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(Age)),
             color = "#2C5F8A", linetype = "dashed",
             linewidth = 0.9) +
  annotate("text",
           x = mean(Biostatistics_Research$Age) + 1.5,
           y = 65,
           label = paste("Mean =",
                         round(mean(Biostatistics_Research$Age), 1)),
           color = "#2C5F8A", size = 3.8) +
  labs(title = "Age Distribution of Participants",
       subtitle = "n = 274",
       x = "Age", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5))

# ── 9.3 Score Category Bar Charts ────────────────────────
p1 <- ggplot(Biostatistics_Research,
             aes(x = phone_cat, fill = phone_cat)) +
  geom_bar(color = "white", alpha = 0.85) +
  geom_text(stat = "count",
            aes(label = paste0(after_stat(count), "\n(",
                round(after_stat(count) /
                      nrow(Biostatistics_Research) * 100, 1), "%)")),
            vjust = -0.3, size = 3.2) +
  scale_fill_manual(values = c("#B0E0A8", "#FFD580", "#FF8C8C")) +
  labs(title = "Phone Use Categories", x = "", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

p2 <- ggplot(Biostatistics_Research,
             aes(x = nomo_cat, fill = nomo_cat)) +
  geom_bar(color = "white", alpha = 0.85) +
  geom_text(stat = "count",
            aes(label = paste0(after_stat(count), "\n(",
                round(after_stat(count) /
                      nrow(Biostatistics_Research) * 100, 1), "%)")),
            vjust = -0.3, size = 3.2) +
  scale_fill_manual(values = c("#B0E0A8", "#FFD580", "#FF8C8C")) +
  labs(title = "Nomophobia Categories", x = "", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

p3 <- ggplot(Biostatistics_Research,
             aes(x = sleep_cat, fill = sleep_cat)) +
  geom_bar(color = "white", alpha = 0.85) +
  geom_text(stat = "count",
            aes(label = paste0(after_stat(count), "\n(",
                round(after_stat(count) /
                      nrow(Biostatistics_Research) * 100, 1), "%)")),
            vjust = -0.3, size = 3.2) +
  scale_fill_manual(values = c("#FF8C8C", "#FFD580", "#B0E0A8")) +
  labs(title = "Sleep Quality Categories", x = "", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

p4 <- ggplot(Biostatistics_Research,
             aes(x = anxiety_cat, fill = anxiety_cat)) +
  geom_bar(color = "white", alpha = 0.85) +
  geom_text(stat = "count",
            aes(label = paste0(after_stat(count), "\n(",
                round(after_stat(count) /
                      nrow(Biostatistics_Research) * 100, 1), "%)")),
            vjust = -0.3, size = 3.2) +
  scale_fill_manual(values = c("#B0E0A8", "#FFD580", "#FF8C8C")) +
  labs(title = "Anxiety Categories", x = "", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

grid.arrange(p1, p2, p3, p4, ncol = 2,
             top = "Score Category Distributions (n = 274)")

# ── 9.4 Correlation Heatmap ──────────────────────────────
cor_data <- Biostatistics_Research[, c("phone_use",
                                        "nomophobia_score",
                                        "sleep_score",
                                        "anxiety_score")]
colnames(cor_data) <- c("Phone Use", "Nomophobia",
                         "Sleep Quality", "Anxiety")
cor_matrix_viz <- cor(cor_data, method = "spearman")

ggcorrplot(cor_matrix_viz,
           method = "square",
           type = "lower",
           lab = TRUE,
           lab_size = 4.5,
           colors = c("#FF8C8C", "white", "#89CFF0"),
           title = "Spearman Correlation Matrix",
           ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold", size = 14))

# ── 9.5 Scatter Plots ────────────────────────────────────
s1 <- ggplot(Biostatistics_Research,
             aes(x = nomophobia_score, y = sleep_score)) +
  geom_point(alpha = 0.4, color = "#89CFF0", size = 1.8) +
  geom_smooth(method = "lm", color = "#2C5F8A",
              se = TRUE, linewidth = 1) +
  labs(title = "Nomophobia vs Sleep Quality",
       subtitle = "rho = -0.24, p < 0.001",
       x = "Nomophobia Score", y = "Sleep Quality Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,
                                     color = "#666666"))

s2 <- ggplot(Biostatistics_Research,
             aes(x = anxiety_score, y = sleep_score)) +
  geom_point(alpha = 0.4, color = "#E8A0BF", size = 1.8) +
  geom_smooth(method = "lm", color = "#8B1A4A",
              se = TRUE, linewidth = 1) +
  labs(title = "Anxiety vs Sleep Quality",
       subtitle = "rho = -0.35, p < 0.001",
       x = "Anxiety Score", y = "Sleep Quality Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,
                                     color = "#666666"))

s3 <- ggplot(Biostatistics_Research,
             aes(x = nomophobia_score, y = anxiety_score)) +
  geom_point(alpha = 0.4, color = "#B0E0A8", size = 1.8) +
  geom_smooth(method = "lm", color = "#2D6A2D",
              se = TRUE, linewidth = 1) +
  labs(title = "Nomophobia vs Anxiety",
       subtitle = "rho = +0.34, p < 0.001",
       x = "Nomophobia Score", y = "Anxiety Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,
                                     color = "#666666"))

s4 <- ggplot(Biostatistics_Research,
             aes(x = phone_use, y = sleep_score)) +
  geom_point(alpha = 0.4, color = "#FFD580", size = 1.8) +
  geom_smooth(method = "lm", color = "#8B6000",
              se = TRUE, linewidth = 1) +
  labs(title = "Phone Use vs Sleep Quality",
       subtitle = "rho = -0.21, p < 0.001",
       x = "Phone Use Score", y = "Sleep Quality Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,
                                     color = "#666666"))

grid.arrange(s1, s2, s3, s4, ncol = 2,
             top = "Key Relationships Between Study Variables")

# ── 9.6 Boxplots: Gender Comparisons ─────────────────────
df_gender <- subset(Biostatistics_Research,
                    Gender %in% c("Female", "Male"))

b1 <- ggplot(df_gender,
             aes(x = Gender, y = nomophobia_score, fill = Gender)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21,
               outlier.fill = "white") +
  scale_fill_manual(values = c("#E8A0BF", "#89CFF0")) +
  labs(title = "Nomophobia by Gender",
       subtitle = "p = 0.311 (ns)",
       x = "", y = "Nomophobia Score") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,
                                     color = "#666666"))

b2 <- ggplot(df_gender,
             aes(x = Gender, y = sleep_score, fill = Gender)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21,
               outlier.fill = "white") +
  scale_fill_manual(values = c("#E8A0BF", "#89CFF0")) +
  labs(title = "Sleep Quality by Gender",
       subtitle = "p = 0.720 (ns)",
       x = "", y = "Sleep Quality Score") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,
                                     color = "#666666"))

grid.arrange(b1, b2, ncol = 2,
             top = "Gender Comparisons (ns = not significant)")

# ============================================================
# END OF SCRIPT
# All 7 hypotheses tested. Analysis complete!
# ============================================================
