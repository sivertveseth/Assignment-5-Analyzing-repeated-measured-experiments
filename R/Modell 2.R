# Last inn nødvendige pakker
library(exscidata)
library(tidyverse)
library(nlme)
library(ggplot2)
library(gridExtra)
data("dxadata")

# Forbered dataene
muskeltverr <- dxadata %>%
  select(participant:include, lean.left_leg, lean.right_leg) %>%
  pivot_longer(
    names_to = "leg",
    values_to = "lean.mass",
    cols = lean.left_leg:lean.right_leg
  ) %>%
  mutate(
    leg = if_else(leg == "lean.left_leg", "L", "R"),
    sets = if_else(multiple == leg, "multiple", "single"),
    time = factor(time, levels = c("pre", "post"))
  ) %>%
  select(participant, time, sex, include, sets, leg, lean.mass)

# Bygg modellen
model_muskel <- lme(
  fixed = lean.mass ~ time * sets + sex,
  random = ~ 1 | participant,
  data = muskeltverr,
  na.action = na.omit
)

# Undertrykk output av modelloppsummering
summary_modmuskel <- summary(model_muskel)

# Ekstraher residualer og predikerte verdier
residuals_muskeltverr <- residuals(model_muskel)
fitted_values_muskeltverr <- fitted(model_muskel)

# Lag plottene
# 1. Q-Q plot
qq_plot_muskeltverr <- ggplot(data = data.frame(sample = residuals_muskeltverr), aes(sample = sample)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  ggtitle("Q-Q Plot") +
  theme_minimal()

# 2. Residualer vs. Predikerte Verdier
residuals_plot_muskeltverr <- ggplot(data = data.frame(fitted = fitted_values_muskeltverr, residuals = residuals_muskeltverr),
                                     aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residualer vs. Predikerte Verdier") +
  xlab("Predikerte Verdier") +
  ylab("Residualer") +
  theme_minimal()

# 3. Histogram av Residualer
hist_plot_muskeltverr <- ggplot(data = data.frame(residuals = residuals_muskeltverr), aes(x = residuals)) +
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  ggtitle("Histogram av Residualer") +
  xlab("Residualer") +
  ylab("Frekvens") +
  theme_minimal()

# 4. Tomt plot eller legg til et fjerde plot hvis ønskelig
empty_plot <- ggplot() + 
  theme_void() + 
  ggtitle("")

# Kombiner plottene
grid.arrange(qq_plot_muskeltverr, residuals_plot_muskeltverr, hist_plot_muskeltverr, empty_plot, nrow = 2)



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

# Utfør Box-Cox for `lean.mass` for å finne optimal lambda
boxcox_model_lean_mass <- boxcox(lm(lean.mass ~ time * sets + sex, data = muskeltverr))
lambda_lean_mass <- boxcox_model_lean_mass$x[which.max(boxcox_model_lean_mass$y)]

# Transformér `lean.mass` basert på optimal lambda
muskeltverr <- muskeltverr %>%
  mutate(boxcox_lean_mass = (lean.mass^lambda_lean_mass - 1) / lambda_lean_mass)

# Log-transformasjon av `lean.mass` hvis optimal lambda er nær 0
muskeltverr <- muskeltverr %>%
  mutate(log_lean_mass = log(lean.mass))

# Tilpass modellen for muskeltverrsnitt (`lean.mass`) med log-transformert variabel
model_log_lean_mass <- lme(
  fixed = log_lean_mass ~ time * sets + sex,
  random = ~ 1 | participant,
  data = muskeltverr %>% filter(time %in% c("pre", "post")), # Vi bruker bare pre og post for sammenligning
  na.action = na.omit
)

# Q-Q-plot av residualene for modellen for muskeltverrsnitt
qqnorm(residuals(model_log_lean_mass))
qqline(residuals(model_log_lean_mass), col = "red")

# Ekstraher residualene og predikerte verdier fra modellen
residuals_lean_mass_log <- residuals(model_log_lean_mass)
fitted_values_lean_mass_log <- fitted(model_log_lean_mass)

# Opprett datasett for plotting
residuals_data_lean_mass <- data.frame(
  Fitted = fitted_values_lean_mass_log,
  Residuals = residuals_lean_mass_log
)

# Residualplott for muskeltverrsnitt
ggplot(data = residuals_data_lean_mass, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predikerte verdier", y = "Residualer", title = "Residualer vs. Predikerte Verdier for Muskeltverrsnitt") +
  theme_minimal()

# Lag et histogram for residualene i muskeltverrsnitt-modellen
ggplot(data = residuals_data_lean_mass, aes(x = Residuals)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(x = "Residualer", y = "Frekvens", title = "Histogram av Residualer for Muskeltverrsnitt") +
  theme_minimal()


