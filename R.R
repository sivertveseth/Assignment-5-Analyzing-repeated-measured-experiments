# Last inn dataene
library(exscidata)
data("strengthvolume"); data("dxadata")

# Undersøk de to datasettene
?dxadata
?strengthvolume

# Gjøre datasettet dxadata mer oversiktlig
library(tidyverse)

dxadata %>%
  select(participant:include, lean.left_leg, lean.right_leg) %>%
  pivot_longer(names_to = "leg", 
               values_to = "lean.mass", 
               cols = lean.left_leg:lean.right_leg) %>%
  mutate(leg = if_else(leg == "lean.left_leg", "L", "R"), 
         sets = if_else(multiple == leg, "multiple", "single")) %>%
  select(participant, time, sex, include, sets, leg, lean.mass) %>%
  print()

# Hent antropometriske data fra datasett
library(tidyverse)
library(knitr)
library(kableExtra)

baseline_tbl <- dxadata %>%
  select(participant:time, sex:weight, age, height) %>%  # Sørg for at disse kolonnene finnes
  filter(time == "pre") %>% 
  group_by(sex, include) %>% 
  summarise(
    N = n(),
    Alder = sprintf("%.1f (%.1f)", mean(age, na.rm = TRUE), sd(age, na.rm = TRUE)),
    Vekt = sprintf("%.1f (%.1f)", mean(weight, na.rm = TRUE), sd(weight, na.rm = TRUE)),
    Stature = sprintf("%.0f (%.0f)", mean(height, na.rm = TRUE), sd(height, na.rm = TRUE)),
    .groups = "drop"  # For å unngå advarsler om grupping
  ) %>% 
  # Konverter alle kolonnene til karakter for å unngå datatypeproblemer i pivot_longer
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = c(N, Alder, Vekt, Stature), names_to = "Variable", values_to = "Value") %>% 
  unite("sex_include", sex, include, sep = "_") %>% 
  pivot_wider(names_from = sex_include, values_from = Value)

# Tabell 
kable(baseline_tbl, row.names = FALSE, col.names = c("", "Ekskludert", "Inkludert", "Ekskludert", "Inkludert")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Kvinne" = 2, "Mann" = 2))



# Lager en dataframe med nødvendig info

data <- data.frame(
  Uke = 1:12,
  RM = c("10RM","10RM","8RM","8RM*","RM8","7RM*","7RM*","7RM*","7RM","7RM*","7RM","7RM"),
  Frekvens = c(2,3,2,3,2,3,3,3,2,3,3,2)
)

# Lager figur for å illustrere 
ggplot(data, aes(x = factor(Uke), y = Frekvens)) +
  geom_bar(stat = "identity", fill = "grey", color = "black", width = 0.6) +  
  geom_text(aes(label = RM), vjust = 0.5, color = "black", size = 5, 
            angle = 90, position = position_stack(vjust = 0.5)) +  # Plasser etiketter midt i søylen
  scale_y_continuous(breaks = 1:3, limits = c(0, 3)) +  # Justerer y-aksen for å gjøre søylene lavere
  labs(
    title = "Studieoversikt",
    x = "Uke",
    y = "Treningsfrekvens"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 1.2),  # Roterer y-akse-etiketten
    panel.grid.major = element_blank(),  # Fjerner gridlinjer for et rent utseende
    panel.grid.minor = element_blank())

# Dataframe som representerer treningsfrekvens og RM per uke
data <- data.frame(
  Uke = 1:12,
  RM = c("10RM", "10RM", "8RM", "8RM*", "8RM", "7RM*", "7RM", "7RM", "7RM*", "7RM", "7RM", "7RM"),
  Frekvens = c(2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1)
)

# Ukene der styrkemålingene ble gjort (x-markeringene)
måling_uker <- data.frame(
  Uke = c(1, 3, 5, 9, 12),  # Uke 1 representerer baseline
  Frekvens = 3.1,           # Plassering over søylene
  label = "x"               # Bruk "x" som markør
)

# Ukene der muskeltverrsnit ble gjort (sirkel-markeringene))
sirkel_uker <- data.frame(
  Uke = c(1, 12),          # Uke 1 og uke 12
  Frekvens = 3.1           # Plassering over søylene
)

# Søylediagram som illustrere studiedesign
ggplot(data, aes(x = factor(Uke), y = Frekvens)) +
  geom_bar(stat = "identity", fill = "grey", color = "black", width = 0.6) +  
  geom_text(aes(label = RM), vjust = 0.5, color = "black", size = 5, 
            angle = 90, position = position_stack(vjust = 0.5)) +  # Plasser RM-etiketter vertikalt midt i søylen
  geom_text(data = måling_uker, aes(x = factor(Uke), y = Frekvens, label = label), 
            color = "black", size = 6, vjust = -0.5, position = position_nudge(x = -0.1)) +  # Plasser "x" litt til venstre
  geom_point(data = sirkel_uker, aes(x = factor(Uke), y = Frekvens), 
             shape = 21, fill = "white", color = "black", size = 5, stroke = 1.5, 
             position = position_nudge(x = 0.1)) +  # Plasser sirkler litt til høyre
  scale_y_continuous(breaks = 1:3, limits = c(0, 3.5)) +  # Justerer y-aksen for å gi plass til markeringene
  labs(
    x = "Uke",
    y = "Treningsfrekvens"
  ) +
  theme_minimal()


# Tabell 
kable(baseline_tbl, row.names = FALSE, col.names = c("", "Ekskludert", "Inkludert", "Ekskludert", "Inkludert")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Kvinne" = 2, "Mann" = 2))


# Utregning av gjennomsnitt og SD for muskeltverrsnitt
library(dplyr)
library(knitr)
library(ggplot2)

# Dataprosessering - omstrukturering og kombinering av data for høyre og venstre ben
muskeltverr <- dxadata %>%
  select(participant:include, lean.left_leg, lean.right_leg) %>%
  pivot_longer(
    cols = lean.left_leg:lean.right_leg, 
    names_to = "leg", 
    values_to = "lean.mass"
  ) %>%
  mutate(
    leg = if_else(leg == "lean.left_leg", "L", "R"), 
    sets = if_else(multiple == leg, "multiple", "single")
  ) %>%
  select(participant, time, sex, include, sets, leg, lean.mass)

# Oppsummering av gjennomsnitt og standardavvik etter tid, treningsvolum og kjønn
muskeltverr_summary <- muskeltverr %>% 
  group_by(time, sets, sex) %>% 
  summarise(
    mean_lean_mass = mean(lean.mass, na.rm = TRUE),
    sd_lean_mass = sd(lean.mass, na.rm = TRUE),
    .groups = "drop"  # Fjerner grupperingen automatisk etter summarise
  )

# Filtrer til pre og post tidspunkter og kombiner kjønn for en enklere oversikt
muskeltverr_summary_pre_post <- muskeltverr_summary %>%
  filter(time %in% c("pre", "post")) %>%
  group_by(time, sets) %>%
  summarise(
    mean_lean_mass = mean(mean_lean_mass, na.rm = TRUE),
    sd_lean_mass = sqrt(mean(sd_lean_mass^2, na.rm = TRUE)),  # Kombinert SD ved å bruke kvadratet av SD
    .groups = "drop"
  ) %>%
  arrange(time, sets)  # Sortér etter tid og treningsvolum

# Lag en oversiktlig tabell
muskeltverr_summary_pre_post %>%
  kable(
    col.names = c("Tidspunkt", "Treningsvolum", "Gjennomsnittlig muskeltverrsnitt (g)", "Standardavvik (SD)"),
    caption = "Oppsummering av muskeltverrsnitt før og etter intervensjon (kombinert for kjønn, kun pre og post)"
  )

# Utregning av gjennomsnitt og SD for muskelstyrke

library(dplyr)

strength_summary <- strengthvolume %>% 
  filter(exercise %in% c("legpress", "legext")) %>% 
  group_by(time, sets, sex, exercise) %>% 
  summarise(mean_strength = mean(load, na.rm = TRUE),
            sd_strength = sd(load, na.rm = TRUE)) %>% 
  ungroup()

# Diagram av utviklingen i muskelstyrke

library(ggplot2)

## Endre navn på tidspunktene

strength <- strength_mean.sd %>%
  mutate(time = recode(time,
                       "pre" = "Pre",
                       "session1" = "Uke 1",
                       "week2" = "Uke 2",
                       "week5" = "Uke 5",
                       "week9" = "Uke 9",
                       "post" = "Post"),
         time = factor(time, levels = c("Pre", "Uke 1", "Uke 2", "Uke 5", "Uke 9", "Post")))

ggplot(strength, aes(x = time, y = mean_strength, color = sets, shape = sex, group = interaction(sets, sex, exercise))) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_strength - sd_strength, ymax = mean_strength + sd_strength), width = 0.2) +
  facet_wrap(~ exercise) +
  labs(x = "Tidspunkt", y = "Gjennomsnittlig muskelstyrke (kg)", color = "Treningsvolum", shape = "Kj\u00F8nn",
       title = "Utvikling i muskelstyrke for leg press og leg extension") +
  theme_minimal() +
  theme(legend.position = "bottom")

library(nlme)

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

# Bruk Box-Cox transformasjon for å finne optimal lambda-verdi
boxcox_model <- boxcox(lm(load ~ time * sets + sex, data = strengthvolume))
lambda <- boxcox_model$x[which.max(boxcox_model$y)]

# Transformér dataene med optimal lambda
strengthvolume <- strengthvolume %>%
  mutate(boxcox_load = (load^lambda - 1) / lambda)

# Log-transformasjon av variabelen `load` basert på Box Cox plottet
strengthvolume <- strengthvolume %>%
  mutate(log_load = log(load))

# Endring av modellen
model_log_legpress <- lme(
  fixed = log_load ~ time * sets + sex,
  random = ~ 1 | participant,
  data = strengthvolume %>% filter(exercise == "legpress"),
  na.action = na.omit
)

# Ekstraher residualene fra den nye modellen
residuals_legpress_log <- residuals(model_log_legpress)

# Ekstraher predikerte verdier fra den nye modellen
fitted_values_legpress_log <- fitted(model_log_legpress)

# Q-Q plot av nye modellen
qqnorm(residuals_legpress_log)
qqline(residuals_legpress_log, col = "red")

# Plot residualene mot de predikerte verdiene
ggplot(data = NULL, aes(x = fitted_values_legpress_log, y = residuals_legpress_log)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predikerte verdier", y = "Residualer", title = "Residualer vs. Predikerte Verdier") +
  theme_minimal()

# Lag et histogram for residualene
ggplot(data = NULL, aes(x = residuals_legpress_log)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(x = "Residualer", y = "Frekvens", title = "Histogram av Residualer") +
  theme_minimal()

library(broom)
library(broom.mixed)
library(knitr)
library(gt)

# Ekstraher oppsummeringer for hver modell
tidy_legext <- tidy(model_legext)
tidy_legpress <- tidy(model_legpress)
tidy_muskel <- tidy(model_muskel)

# Legg til en kolonne som angir modellnavnet for enkel referanse
tidy_legext$model <- "Model 1.1: Leg Extension"
tidy_legpress$model <- "Model 1.2: Leg Press"
tidy_muskel$model <- "Model 2: Muskel Tverrsnitt"

# Kombiner alle dataene i én dataframe
tidy_models <- rbind(tidy_legext, tidy_legpress, tidy_muskel)

# Sjekk de tilgjengelige "term" verdiene for å bekrefte navngivingen
unique(tidy_models$term)

# Filtrer ut kun de radene som er relevante for "(Intercept)", "timepost", og "timepost:setsingle"
tidy_models_filtered <- tidy_models %>%
  filter(term %in% c("(Intercept)", "timepost", "timepost:setssingle"))

# Velg ut de viktigste kolonnene å vise og legg til formatering for p-verdier med stjerner
tidy_models_summary <- tidy_models_filtered %>%
  dplyr::select(model, term, estimate, std.error, statistic, p.value) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 3),
    p.value = ifelse(p.value < 0.05, "0.05*", format.pval(p.value, digits = 3, nsmall = 3))
  )

# Bruk gt for å lage en tabell med de filtrerte resultatene
tidy_models_summary %>%
  gt() %>%
  tab_header(
    title = "Oppsummering av Modellene for Pre og Post",
    subtitle = "Inkludert Forskjeller Mellom Single og Multiple Sett"
  ) %>%
  cols_label(
    model = "Modell",
    term = "Parameter",
    estimate = "Estimert Koeffisient",
    std.error = "Standard Feil",
    statistic = "t-verdi",
    p.value = "p-verdi"
  ) %>%
  fmt_number(
    columns = c("estimate", "std.error", "statistic"),
    decimals = 3
  )




