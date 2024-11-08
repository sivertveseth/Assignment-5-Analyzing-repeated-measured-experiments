# Last inn i nødvendige pakkene
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