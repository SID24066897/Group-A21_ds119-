
# Installing Packages
install.packages(c("tidyverse", "broom", "MASS", "ggrepel", "ggplot2", "reshape"))

# Import Libraries
library(tidyverse)
library(broom)
library(MASS)
library(ggrepel)
library(ggplot2)
library(reshape2)
options(scipen = 999)


# Dataset Loading
df <- read_csv("F1Drivers_Dataset.csv", show_col_types = FALSE)
names(df) <- str_trim(names(df))

cat("\n Data loaded. Rows:", nrow(df), "Columns:", ncol(df), "\n\n")


# Convert numeric columns and simple cleaning

numcols <- c("Race_Entries","Race_Starts","Pole_Positions","Race_Wins",
             "Podiums","Fastest_Laps","Points","Years_Active")
for (c in numcols) {
  if (c %in% names(df)) {
    df[[c]] <- as.numeric(df[[c]])
  }
}

df <- df %>% filter(!is.na(Race_Starts) & !is.na(Race_Wins))
cat("After removing missing starts/wins: Rows =", nrow(df), "\n\n")

# Derived columns (helpful)
df <- df %>%
  mutate(
    win_rate = if_else(Race_Starts > 0, Race_Wins / Race_Starts, NA_real_),
    start_rate = if_else(Race_Entries > 0, Race_Starts / Race_Entries, NA_real_)
  )


# Exploratory Data Analysis (EDA)
cat("Basic summary statistics (selected numeric cols)\n")

numeric_subset <- df[, numcols]

print(summary(numeric_subset))

# Count drivers with 0 wins
zero_wins_n <- sum(df$Race_Wins == 0, na.rm = TRUE)
cat("\nNumber of drivers with 0 wins:", zero_wins_n,
    "out of", nrow(df),
    sprintf("(%.1f%%)", 100 * zero_wins_n / nrow(df)), "\n\n")

# Mean and median starts/wins
cat("Mean Race_Starts:", round(mean(df$Race_Starts, na.rm = TRUE), 3), "\n")
cat("Median Race_Starts:", median(df$Race_Starts, na.rm = TRUE), "\n")
cat("Mean Race_Wins:", round(mean(df$Race_Wins, na.rm = TRUE), 3), "\n")
cat("Median Race_Wins:", median(df$Race_Wins, na.rm = TRUE), "\n\n")

# Exploratory Data Analysis (EDA) Plots
# Scatter plot with linear fit
p_scatter <- ggplot(df, aes(x = Race_Starts, y = Race_Wins)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "F1 drivers: Race Starts vs Race Wins",
       x = "Total Race Starts (count)",
       y = "Total Race Wins (count)") +
  theme_minimal()
print(p_scatter)
ggsave(filename = "scatter_starts_wins.png", plot = p_scatter, width = 8, height = 5, dpi = 300)

# Histogram of Race_Starts
p_hist_starts <- ggplot(df, aes(x = Race_Starts)) +
  geom_histogram(bins = 40) +
  labs(title = "Distribution of Total Race Starts", x = "Total Race Starts", y = "Number of drivers") +
  theme_minimal()
print(p_hist_starts)
ggsave(filename = "hist_starts.png", plot = p_hist_starts, width = 8, height = 4, dpi = 300)

# Histogram of Race_Wins
p_hist_wins <- ggplot(df, aes(x = Race_Wins)) +
  geom_histogram(bins = 40) +
  labs(title = "Distribution of Total Race Wins", x = "Total Race Wins", y = "Number of drivers") +
  theme_minimal()
print(p_hist_wins)
ggsave(filename = "hist_wins.png", plot = p_hist_wins, width = 8, height = 4, dpi = 300)


# labeled scatter for top 10 winners (for visual)
if ("Race_Wins" %in% names(df)) {
  top10_idx <- order(df$Race_Wins, decreasing = TRUE)[1:min(10, nrow(df))]
  top10 <- df[top10_idx, , drop = FALSE]
  p_scatter_lab <- p_scatter + geom_text_repel(data = top10, aes(label = Driver), size = 3)
  print(p_scatter_lab)
  ggsave(filename = "scatter_starts_wins_labeled.png", plot = p_scatter_lab, width = 9, height = 6, dpi = 300)
  cat("Saved: scatter_starts_wins_labeled.png (optional, labeled top10)\n\n")
}

# Correlation tests

pearson_test <- cor.test(df$Race_Starts, df$Race_Wins, method = "pearson")
spearman_test <- cor.test(df$Race_Starts, df$Race_Wins, method = "spearman")

cat("\nPearson correlation (starts vs wins):\n")
print(pearson_test)
cat("\nSpearman correlation (starts vs wins):\n")
print(spearman_test)
cat("\n")

# Linear model (descriptive only)
lm_mod <- lm(Race_Wins ~ Race_Starts, data = df)
print(summary(lm_mod))
cat("\nTidy coefficients (lm):\n")
print(broom::tidy(lm_mod))
cat("\n")

# Count modelling: Poisson and Negative Binomial
cat(" Poisson GLM \n")
pois_mod <- glm(Race_Wins ~ Race_Starts, data = df, family = poisson(link = "log"))
print(summary(pois_mod))

# Overdispersion check
dispersion <- sum(residuals(pois_mod, type = "pearson")^2, na.rm = TRUE) / df.residual(pois_mod)
cat("\nPoisson dispersion statistic (Pearson residuals^2 / df):", round(dispersion,4), "\n")
if (dispersion > 1.5) {
  cat("Dispersion > 1.5 indicates overdispersion. Fitting Negative Binomial model \n\n")
  nb_mod <- glm.nb(Race_Wins ~ Race_Starts, data = df)
  cat("=== Negative Binomial model summary ===\n")
  print(summary(nb_mod))
  cat("\nTidy coefficients (nb):\n")
  print(broom::tidy(nb_mod))
  # show exp(coef) interpretation
  cat("\nExponentiated coefficients (IRR interpretation):\n")
  print(exp(coef(nb_mod)))
}
cat("\n")

# Robustness: exclude top 3 winners and re-run correlations
cat("Robustness check: exclude top 3 winners\n")
top3_idx <- order(df$Race_Wins, decreasing = TRUE)[1:min(3, nrow(df))]
df_no_top3 <- df[-top3_idx, , drop = FALSE]
cat("Rows after removing top3:", nrow(df_no_top3), "\n")
pearson_no_top3 <- cor.test(df_no_top3$Race_Starts, df_no_top3$Race_Wins, method = "pearson")
spearman_no_top3 <- cor.test(df_no_top3$Race_Starts, df_no_top3$Race_Wins, method = "spearman")
cat("\nPearson (no top3):\n"); print(pearson_no_top3)
cat("\nSpearman (no top3):\n"); print(spearman_no_top3)
cat("\n")

# Correlation Heatmap
vars4 <- c("Race_Starts", "Race_Wins", "Podiums", "Points")
vars4 <- vars4[vars4 %in% names(df)]
if (length(vars4) < 2) {
  cat("Not enough numeric variables available for heatmap. vars4:",
      paste(vars4, collapse = ","), "\n")

} else {
  # Compute correlation & squared correlation
  mat <- cor(df[ , vars4], use = "pairwise.complete.obs", method = "pearson")
  mat_r2 <- mat^2
  cat("Correlation matrix (rounded):\n")
  print(round(mat, 3))
  print(round(mat_r2, 3))
  melted <- reshape2::melt(mat_r2)
  colnames(melted) <- c("Var1", "Var2", "R2")

  # Heatmap plot
  p_heat <- ggplot(melted, aes(x = Var1, y = Var2, fill = R2)) +
    geom_tile() +
    geom_text(aes(label = round(R2, 3)), color = "black", size = 4) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = "Squared correlation heatmap (R^2)",
         x = "", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p_heat)
  ggsave("heatmap_r2_4vars.png", p_heat, width = 6, height = 5, dpi = 300)
}
