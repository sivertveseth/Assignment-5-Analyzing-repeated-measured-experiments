# Last inn nødvendige pakker
library(exscidata)
library(tidyverse)
library(nlme)
library(ggplot2)
library(gridExtra)
data("strengthvolume"); data("dxadata")

# Forbered dataene
strengthvolume_legext <- strengthvolume %>%
  filter(exercise == "legext") %>%
  mutate(time = factor(time, levels = c("pre", "session1", "week2", "week5", "week9", "post")))

# Bygg modellen
model_legext <- lme(
  fixed = load ~ time * sets + sex,
  random = ~ 1 | participant,
  data = strengthvolume_legext,
  na.action = na.omit
)

# Undertrykk output av modelloppsummering
summary_modlegext <- summary(model_legext)

# Ekstraher residualer og predikerte verdier
residuals_legext <- residuals(model_legext)
fitted_values_legext <- fitted(model_legext)

# Lag plottene
qq_plot <- ggplot(data = data.frame(residuals = residuals_legext), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  ggtitle("Q-Q Plot") +
  theme_minimal()

resid_fitted_plot <- ggplot(data = data.frame(fitted = fitted_values_legext, residuals = residuals_legext),
                            aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Residualer vs. Predikerte Verdier") +
  xlab("Predikerte Verdier") +
  ylab("Residualer") +
  theme_minimal()

hist_plot <- ggplot(data = data.frame(residuals = residuals_legext), aes(x = residuals)) +
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  ggtitle("Histogram av Residualer") +
  xlab("Residualer") +
  ylab("Frekvens") +
  theme_minimal()

# Arranger plottene
grid.arrange(qq_plot, resid_fitted_plot, hist_plot, nrow = 2)


# Plotting
ggplot(strengthvolume_legext, aes(x = time, y = load, color = sets, shape = sex, group = interaction(sets, sex))) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) + 
  labs(
    title = "Utvikling av Muskelstyrke i Leg Extension", 
    x = "Tidspunkt", 
    y = "Muskelstyrke (kg)", 
    color = "Treningsvolum", 
    shape = "Kjønn"
  ) +
  theme_minimal()

# Utfør Box-Cox for `legext`
boxcox_model_legext <- boxcox(lm(load ~ time * sets + sex, data = strengthvolume_legext))
lambda_legext <- boxcox_model_legext$x[which.max(boxcox_model_legext$y)]

# Transformér `load` for `legext` basert på optimal lambda
strengthvolume_legext <- strengthvolume_legext %>%
  mutate(boxcox_load = (load^lambda_legext - 1) / lambda_legext)

# Log-transformasjon av `load` for `legext`
strengthvolume_legext <- strengthvolume_legext %>%
  mutate(log_load = log(load))

# Tilpass modellen med `log_load` for `legext`
model_log_legext <- lme(
  fixed = log_load ~ time * sets + sex,
  random = ~ 1 | participant,
  data = strengthvolume_legext,
  na.action = na.omit
)

# Ekstraher residualene fra den nye modellen
residuals_legext_log <- residuals(model_log_legext)

# Ekstraher predikerte verdier fra den nye modellen
fitted_values_legext_log <- fitted(model_log_legext)

# Q-Q plot av den nye modellen for `legext`
qqnorm(residuals_legext_log)
qqline(residuals_legext_log, col = "red")

# Opprett et datasett med residualer og predikerte verdier for `legext`
residuals_data_legext <- data.frame(
  Fitted = fitted_values_legext_log,
  Residuals = residuals_legext_log
)

# Plot residualene mot de predikerte verdiene for `legext`
ggplot(data = residuals_data_legext, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predikerte verdier", y = "Residualer", title = "Residualer vs. Predikerte Verdier for Leg Extension") +
  theme_minimal()

# Lag et histogram for residualene i legext-modellen
ggplot(data = residuals_data_legext, aes(x = Residuals)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(x = "Residualer", y = "Frekvens", title = "Histogram av Residualer for Leg Extension") +
  theme_minimal()

# Summary av den nye modellen
summary(model_log_legext)

# Modell for muskelstyrke i leg extension uten transformasjon
model_legext_untransformed <- lme(
  fixed = load ~ time * sets + sex,
  random = ~ 1 | participant,
  data = strengthvolume %>% filter(exercise == "legext"),
  na.action = na.omit
)

# Sammendrag av modellen
summary(model_legext_untransformed)
