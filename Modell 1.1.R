# Last inn dataene
library(exscidata)
library(tidyverse)
library(dplyr)
library(nlme)
data("strengthvolume"); data("dxadata")

# Filtrer datasettet for `legpress`
strengthvolume_legpress <- strengthvolume %>%
  filter(exercise == "legpress")

# Modell for muskelstyrke i legpress øvelsen uten manglende verdier
model_legpress <- lme(
  fixed = load ~ time * sets + sex,
  random = ~ 1 | participant,
  data = strengthvolume %>% filter(exercise == "legpress"),
  na.action = na.omit
)

summary(model_legpress)

# Ekstraher residualene fra modellen
residuals_legpress <- residuals(model_legpress)

# Ekstraher predikerte verdier fra modellen
fitted_values_legpress <- fitted(model_legpress)

# Q-Q plot
qqnorm(residuals_legpress)
qqline(residuals_legpress, col = "red")

library(MASS)

# Utfør Box-Cox for `legpress`
boxcox_model_legpress <- boxcox(lm(load ~ time * sets + sex, data = strengthvolume_legpress))
lambda_legpress <- boxcox_model_legpress$x[which.max(boxcox_model_legpress$y)]

# Transformér `load` for `legpress` basert på optimal lambda
strengthvolume_legpress <- strengthvolume_legpress %>%
  mutate(boxcox_load = (load^lambda_legpress - 1) / lambda_legpress)

# Log-transformasjon av `load` for `legpress`
strengthvolume_legpress <- strengthvolume_legpress %>%
  mutate(log_load = log(load))

# Endring av modellen med `log_load` for `legpress`
model_log_legpress <- lme(
  fixed = log_load ~ time * sets + sex,
  random = ~ 1 | participant,
  data = strengthvolume_legpress,
  na.action = na.omit
)

# Ekstraher residualene fra den nye modellen
residuals_legpress_log <- residuals(model_log_legpress)

# Ekstraher predikerte verdier fra den nye modellen
fitted_values_legpress_log <- fitted(model_log_legpress)

# Opprett et datasett med residualer og predikerte verdier for plottene
residuals_data <- data.frame(
  Fitted = fitted_values_legpress_log,
  Residuals = residuals_legpress_log
)

# Q-Q plot av den nye modellen
qqnorm(residuals_legpress_log)
qqline(residuals_legpress_log, col = "red")

# Plot residualene mot de predikerte verdiene
ggplot(data = residuals_data, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predikerte verdier", y = "Residualer", title = "Residualer vs. Predikerte Verdier") +
  theme_minimal()

# Lag et histogram for residualene
ggplot(data = residuals_data, aes(x = Residuals)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(x = "Residualer", y = "Frekvens", title = "Histogram av Residualer") +
  theme_minimal()
