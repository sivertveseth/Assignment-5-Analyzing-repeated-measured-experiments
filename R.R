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
