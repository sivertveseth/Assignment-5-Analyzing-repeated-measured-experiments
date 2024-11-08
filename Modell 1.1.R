# Last inn nødvendige pakker
library(exscidata)
library(tidyverse)
library(nlme)
library(ggplot2)
library(gridExtra)
library(grid)
data("strengthvolume"); data("dxadata")

# Forbered dataene
strengthvolume_legpress <- strengthvolume %>%
  filter(exercise == "legpress") %>%
  mutate(time = factor(time, levels = c("pre", "session1", "week2", "week5", "week9", "post")))

# Bygg modellen
model_legpress <- lme(
  fixed = load ~ time * sets + sex,
  random = ~ 1 | participant,
  data = strengthvolume_legpress,
  na.action = na.omit
)

# Undertrykk output av modelloppsummering
summary_modlegpress <- summary(model_legpress)

# Ekstraher residualer og predikerte verdier
residuals_legpress <- residuals(model_legpress)
fitted_values_legpress <- fitted(model_legpress)

# Lag plottene
# 1. Q-Q plot
qq_plot_legpress <- ggplot(data = data.frame(sample = residuals_legpress), aes(sample = sample)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  ggtitle("Q-Q Plot") +
  theme_minimal()

# 2. Residualer vs. Predikerte Verdier
residuals_plot_legpress <- ggplot(data = data.frame(fitted = fitted_values_legpress, residuals = residuals_legpress),
                                  aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residualer vs. Predikerte Verdier") +
  xlab("Predikerte Verdier") +
  ylab("Residualer") +
  theme_minimal()

# 3. Histogram av Residualer
hist_plot_legpress <- ggplot(data = data.frame(residuals = residuals_legpress), aes(x = residuals)) +
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  ggtitle("Histogram av Residualer") +
  xlab("Residualer") +
  ylab("Frekvens") +
  theme_minimal()

# 4. Eventuelt et tomt plot eller et ekstra plot (her legger vi til en tom tekst)
empty_plot <- ggplot() + 
  theme_void() + 
  ggtitle("")

# Kombiner plottene
grid.arrange(qq_plot_legpress, residuals_plot_legpress, hist_plot_legpress, empty_plot, nrow = 2)


# Lag grob-objekter fra hver av plott-funksjonene
qq_grob <- grid.grabExpr(qq_plot_legext())
residuals_grob <- grid.grabExpr(residuals_plot_legext())
hist_grob <- grid.grabExpr(hist_plot_legext())

# Kombiner og vis plottene i en layout
test <- grid.arrange(
  grobs = list(qq_grob, residuals_grob, hist_grob),
  nrow = 3
)


# Plotting
ggplot(strengthvolume_legext, aes(x = time, y = load, color = sets, shape = sex, group = interaction(sets, sex))) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2)  + 
  labs(
    title = "Utvikling av Muskelstyrke i Leg Extension", 
    x = "Tidspunkt", 
    y = "Muskelstyrke (kg)", 
    color = "Treningsvolum", 
    shape = "Kjønn"
  ) +
  theme_minimal()


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
