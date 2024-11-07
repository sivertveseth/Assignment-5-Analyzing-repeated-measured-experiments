# Last inn dataene
library(exscidata)
library(tidyverse)
library(dplyr)
library(nlme)
library(MASS)
data("strengthvolume"); data("dxadata")

# Filtrer datasettet for `legext`
strengthvolume_legext <- strengthvolume %>%
  filter(exercise == "legext")

# Modell for muskelstyrke i legext øvelsen uten manglende verdier
model_legext <- lme(
  fixed = load ~ time * sets + sex,
  random = ~ 1 | participant,
  data = strengthvolume %>% filter(exercise == "legext"),
  na.action = na.omit
)

summary(model_legext)

# Ekstraher residualene fra modellen
residuals_legext <- residuals(model_legext)

# Ekstraher predikerte verdier fra modellen
fitted_values_legext <- fitted(model_legext)

# Q-Q plot
qqnorm(residuals_legext)
qqline(residuals_legext, col = "red")

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
