# Last inn dataene
library(exscidata)
library(tidyverse)
library(dplyr)
library(nlme)
library(MASS)
data("strengthvolume"); data("dxadata")

# Filtrer og fjern eventuelle duplikater i strengthvolume_legext
strengthvolume_legext_unique <- strengthvolume_legext %>%
  filter(time %in% c("pre", "post")) %>%
  distinct(participant, time, .keep_all = TRUE)

# Filtrer og fjern eventuelle duplikater i muskeltverr
muskeltverr_unique <- muskeltverr %>%
  filter(time %in% c("pre", "post")) %>%
  distinct(participant, time, .keep_all = TRUE)

# Kombiner datasettene ved hjelp av inner_join
combined_data <- strengthvolume_legext_unique %>%
  inner_join(muskeltverr_unique, by = c("participant", "time"))

# Pivotér dataene slik at pre- og post-verdiene kan beregnes for hver deltaker
combined_data_wide <- combined_data %>%
  pivot_wider(
    names_from = time,
    values_from = c(load, lean.mass)
  ) %>%
  # Beregn differansen mellom 'post' og 'pre' for styrke og muskeltverrsnitt
  mutate(
    delta_styrke = load_post - load_pre,
    delta_muskeltverr = lean.mass_post - lean.mass_pre
  )

# Kjør en lineær regresjon for å se om endring i muskeltverrsnitt påvirker endring i muskelstyrke
model_legext_muskeltverr <- lm(delta_styrke ~ delta_muskeltverr, data = combined_data_wide)

# Vis sammendraget av modellen
summary(model_legext_muskeltverr)

strengthvolume_legext_unique <- strengthvolume_legext %>%
  filter(time %in% c("pre", "post")) %>%
  distinct(participant, time, .keep_all = TRUE)

muskeltverr_unique <- muskeltverr %>%
  filter(time %in% c("pre", "post")) %>%
  distinct(participant, time, .keep_all = TRUE)

combined_data <- strengthvolume_legext_unique %>%
  inner_join(muskeltverr_unique, by = c("participant", "time"))

# Kombiner 'strengthvolume_legext' og 'muskeltverr'
combined_data <- strengthvolume_legext %>%
  inner_join(muskeltverr, by = c("participant", "time")) %>%
  filter(time %in% c("pre", "post"))

# Modell for leg extension og muskeltverrsnitt
model_legext_muskeltverr <- strengthvolume_legext %>%
  filter(time %in% c("pre", "post")) %>%
  pivot_wider(names_from = time, values_from = c(load, lean.mass)) %>%
  mutate(
    delta_styrke = load_post - load_pre,
    delta_muskeltverr = lean.mass_post - lean.mass_pre
  ) %>%
  lm(delta_styrke ~ delta_muskeltverr, data = .)

summary(model_legext_muskeltverr)

# Kombiner muskeltverrsnitt og muskelstyrke data
combined_data <- muskeltverr %>%
  filter(time %in% c("pre", "post")) %>%
  mutate(participant = as.character(participant), time = as.character(time)) %>%
  inner_join(
    strengthvolume %>% 
      filter(exercise %in% c("legpress", "legext"), time %in% c("pre", "post"))%>%
      mutate(participant = as.character(participant), time = as.character(time)),
    by = c("participant", "time")
  )

data <- combined_data %>% 
  group_by(participant) %>% 
  summarise(
    delta_styrke = muskelstyrke
  )
# Tilpass modellen for sammenhengen mellom muskelstyrke og muskeltverrsnitt
model_strength_vs_lean_mass <- lme(
  fixed = load ~ lean.mass * sets + sex + time,
  random = ~ 1 | participant,
  data = combined_data,
  na.action = na.omit
)

# Q-Q-plot av residualene for modell 3
qqnorm(residuals(model_strength_vs_lean_mass))
qqline(residuals(model_strength_vs_lean_mass), col = "red")

# Ekstraher residualer og predikerte verdier fra modellen
residuals_strength_vs_lean_mass <- residuals(model_strength_vs_lean_mass)
fitted_values_strength_vs_lean_mass <- fitted(model_strength_vs_lean_mass)

# Opprett datasett for plotting
residuals_data_strength_vs_lean_mass <- data.frame(
  Fitted = fitted_values_strength_vs_lean_mass,
  Residuals = residuals_strength_vs_lean_mass
)

# Residualplott for muskelstyrke vs. muskeltverrsnitt
ggplot(data = residuals_data_strength_vs_lean_mass, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predikerte verdier", y = "Residualer", title = "Residualer vs. Predikerte Verdier for Muskelstyrke vs. Muskeltverrsnitt") +
  theme_minimal()

# Lag et histogram for residualene i modell 3
ggplot(data = residuals_data_strength_vs_lean_mass, aes(x = Residuals)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(x = "Residualer", y = "Frekvens", title = "Histogram av Residualer for Muskelstyrke vs. Muskeltverrsnitt") +
  theme_minimal()

# Utfør Box-Cox for `load` for å finne optimal lambda
boxcox_model_strength_vs_lean_mass <- boxcox(lm(load ~ lean.mass * sets + sex + time, data = combined_data))
lambda_strength_vs_lean_mass <- boxcox_model_strength_vs_lean_mass$x[which.max(boxcox_model_strength_vs_lean_mass$y)]

# Transformér `load` basert på optimal lambda
combined_data <- combined_data %>%
  mutate(boxcox_load = ifelse(lambda_strength_vs_lean_mass == 0,
                              log(load),
                              (load^lambda_strength_vs_lean_mass - 1) / lambda_strength_vs_lean_mass))

# Tilpass modellen på nytt med transformert `load`
model_boxcox_strength_vs_lean_mass <- lme(
  fixed = boxcox_load ~ lean.mass * sets + sex + time,
  random = ~ 1 | participant,
  data = combined_data,
  na.action = na.omit
)

# Q-Q-plot av residualene for den Box-Cox-transformerte modellen
qqnorm(residuals(model_boxcox_strength_vs_lean_mass))
qqline(residuals(model_boxcox_strength_vs_lean_mass), col = "red")

# Ekstraher residualene og predikerte verdier fra den nye modellen
residuals_boxcox_strength_vs_lean_mass <- residuals(model_boxcox_strength_vs_lean_mass)
fitted_values_boxcox_strength_vs_lean_mass <- fitted(model_boxcox_strength_vs_lean_mass)

# Opprett datasett for plotting
residuals_data_boxcox_strength_vs_lean_mass <- data.frame(
  Fitted = fitted_values_boxcox_strength_vs_lean_mass,
  Residuals = residuals_boxcox_strength_vs_lean_mass
)

# Residualplott for transformert modell
ggplot(data = residuals_data_boxcox_strength_vs_lean_mass, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predikerte verdier", y = "Residualer", title = "Residualer vs. Predikerte Verdier for Transformert Modell") +
  theme_minimal()

# Lag et histogram for residualene i den transformerte modellen
ggplot(data = residuals_data_boxcox_strength_vs_lean_mass, aes(x = Residuals)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(x = "Residualer", y = "Frekvens", title = "Histogram av Residualer for Transformert Modell") +
  theme_minimal()
