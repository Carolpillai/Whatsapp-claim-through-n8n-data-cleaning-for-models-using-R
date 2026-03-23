# HEALTH INSURANCE — DATA VISUALIZATION
# Dataset 1: Fraud Detection
# Dataset 2: Payout Prediction

required_packages <- c("ggplot2", "dplyr", "readr", "scales", "tidyr")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, dependencies = TRUE)
}
invisible(lapply(required_packages, install_if_missing))
invisible(lapply(required_packages, library, character.only = TRUE))

cat("✅ Packages loaded\n")


fraud_path  <- "C:/Users/Carol Pillai/Desktop/Research/model/health_insurance_fraud_india.csv"
payout_path <- "C:/Users/Carol Pillai/Desktop/Research/model/health_insurance_payout_prediction.csv"

fraud  <- read_csv(fraud_path,  show_col_types = FALSE)
payout <- read_csv(payout_path, show_col_types = FALSE)

names(fraud)  <- tolower(trimws(names(fraud)))
names(payout) <- tolower(trimws(names(payout)))

cat(sprintf("✅ Fraud dataset:  %d rows, %d columns\n", nrow(fraud),  ncol(fraud)))
cat(sprintf("✅ Payout dataset: %d rows, %d columns\n", nrow(payout), ncol(payout)))


theme_clean <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 11),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(colour = "grey30")
  )


fraud <- fraud %>%
  mutate(fraud_label = ifelse(is_fraud == 1, "Fraud", "Legitimate"))

c1 <- fraud %>%
  group_by(claim_type) %>%
  summarise(fraud_rate = mean(is_fraud) * 100, .groups = "drop")

p1 <- ggplot(c1, aes(x = reorder(claim_type, fraud_rate),
                     y = fraud_rate, fill = fraud_rate)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", fraud_rate)),
            hjust = -0.15, size = 4.5, fontface = "bold") +
  scale_fill_gradient(low = "#FFCC80", high = "#E65100") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  coord_flip() +
  labs(
    title    = "Fraud Rate by Claim Type",
    subtitle = "Which type of claim is most likely to be fraudulent?",
    x = NULL, y = "Fraud Rate (%)"
  ) + theme_clean

print(p1)
ggsave("chart1_fraud_by_claim_type.png", p1, width = 8, height = 5, dpi = 150)
cat("✅ Chart 1 done: Fraud Rate by Claim Type\n")



                        y = claim_amount_inr / 1e5,
                        fill = fraud_label)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.8,
               show.legend = FALSE, width = 0.5) +
  scale_fill_manual(values = c("Fraud" = "#EF5350", "Legitimate" = "#42A5F5")) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Claim Amount: Fraud vs Legitimate",
    subtitle = "Fraudulent claims are typically much higher in value",
    x = NULL, y = "Claim Amount (INR Lakhs)"
  ) + theme_clean

print(p2)
ggsave("chart2_claim_amount_fraud_vs_legit.png", p2, width = 7, height = 5, dpi = 150)
cat("✅ Chart 2 done: Claim Amount Fraud vs Legitimate\n")



p3 <- ggplot(payout, aes(x = claim_type,
                         y = approved_payout_inr / 1e5,
                         fill = claim_type)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.7,
               show.legend = FALSE, width = 0.55) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Approved Payout by Claim Type",
    subtitle = "Surgery & Emergency have the highest payouts — as expected",
    x = NULL, y = "Approved Payout (INR Lakhs)"
  ) + theme_clean

print(p3)
ggsave("chart3_payout_by_claim_type.png", p3, width = 8, height = 5, dpi = 150)
cat("✅ Chart 3 done: Payout by Claim Type\n")



c4 <- payout %>%
  summarise(
    `Smoker`       = mean(approved_payout_inr[is_smoker   == 1], na.rm = TRUE) / 1e5,
    `Non-Smoker`   = mean(approved_payout_inr[is_smoker   == 0], na.rm = TRUE) / 1e5,
    `Diabetic`     = mean(approved_payout_inr[is_diabetic == 1], na.rm = TRUE) / 1e5,
    `Non-Diabetic` = mean(approved_payout_inr[is_diabetic == 0], na.rm = TRUE) / 1e5
  ) %>%
  tidyr::pivot_longer(everything(),
                      names_to  = "group",
                      values_to = "avg_payout") %>%
  mutate(
    category = ifelse(group %in% c("Smoker", "Non-Smoker"), "Smoking Status", "Diabetes Status"),
    category = factor(category, levels = c("Smoking Status", "Diabetes Status"))
  )

p4 <- ggplot(c4, aes(x = group, y = avg_payout, fill = group)) +
  geom_col(show.legend = FALSE, width = 0.5, alpha = 0.85) +
  geom_text(aes(label = sprintf("INR %.1fL", avg_payout)),
            vjust = -0.4, size = 4.2, fontface = "bold") +
  scale_fill_manual(values = c(
    "Smoker"       = "#EF9A9A",
    "Non-Smoker"   = "#90CAF9",
    "Diabetic"     = "#FFCC80",
    "Non-Diabetic" = "#A5D6A7"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)), labels = comma) +
  facet_wrap(~category, scales = "free_x") +
  labs(
    title    = "Average Payout: Smokers vs Non-Smokers & Diabetics vs Non-Diabetics",
    subtitle = "Do high-risk patient profiles lead to higher claim payouts?",
    x = NULL, y = "Average Payout (INR Lakhs)"
  ) + theme_clean

print(p4)
ggsave("chart4_payout_smoker_diabetic.png", p4, width = 10, height = 5, dpi = 150)
cat("✅ Chart 4 done: Payout by Smoker & Diabetic\n")


cat("\n✅ All 4 charts complete!\n")
cat("  chart1_fraud_by_claim_type.png        — Fraud Detection\n")
cat("  chart2_claim_amount_fraud_vs_legit.png — Fraud Detection\n")
cat("  chart3_payout_by_claim_type.png        — Payout Prediction\n")
cat("  chart4_payout_smoker_diabetic.png      — Payout Prediction\n")
