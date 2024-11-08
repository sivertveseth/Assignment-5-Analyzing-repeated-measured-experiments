# Last inn nødvendige pakker
library(exscidata)
library(tidyverse)
library(nlme)
data("strengthvolume"); data("dxadata")

# Filtrer datasettet for leg extension-øvelsen og sett 'time' som en faktor med ønsket rekkefølge
strengthvolume_legext <- strengthvolume %>%
  filter(exercise == "legext") %>%
  mutate(time = factor(time, levels = c("pre", "session1", "week2", "week5", "week9", "post")))

# Modell for muskelstyrke i leg extension-øvelsen uten manglende verdier
model_legext <- lme(
  fixed = load ~ time * sets + sex,
  random = ~ 1 | participant,
  data = strengthvolume_legext,  # Bruker det oppdaterte datasettet
  na.action = na.omit
)

# Oppsummer modellresultatene
summary_modell_legext <- summary(model_legext)

# Hent ut estimatet og standardfeilen for 'post'
post_koeff_ext <- summary_modell_legext$tTable["timepost", "Value"]
post_se_ext <- summary_modell_legext$tTable["timepost", "Std.Error"]

# Beregn konfidensintervallet for post-estimatet
alpha <- 0.05
z_value <- qnorm(1 - alpha / 2)

lower_bound_ext <- post_koeff_ext - z_value * post_se_ext
upper_bound_ext <- post_koeff_ext + z_value * post_se_ext

konfidensintervall_post_ext <- c(lower_bound_ext, upper_bound_ext)

# Hent ut estimatet og standardfeilen for interaksjonen mellom 'timepost' og 'setssingle'
interaksjon_koeff_ext <- summary_modell_legext$tTable["timepost:setssingle", "Value"]
interaksjon_se_ext <- summary_modell_legext$tTable["timepost:setssingle", "Std.Error"]

# Beregn konfidensintervallet for interaksjonsestimatet
lower_bound_interaksjon_ext <- interaksjon_koeff_ext - z_value * interaksjon_se_ext
upper_bound_interaksjon_ext <- interaksjon_koeff_ext + z_value * interaksjon_se_ext

konfidensintervall_interaksjon_ext <- c(lower_bound_interaksjon_ext, upper_bound_interaksjon_ext)



# Last inn dataene
library(exscidata)
library(tidyverse)
library(nlme)
data("dxadata")

# Filtrer datasettet for leg press-øvelsen og sett 'time' som en faktor med ønsket rekkefølge
strengthvolume_legpress <- strengthvolume %>%
  filter(exercise == "legpress") %>%
  mutate(time = factor(time, levels = c("pre", "session1", "week2", "week5", "week9", "post")))

# Modell for muskelstyrke i legpress-øvelsen uten manglende verdier
model_legpress <- lme(
  fixed = load ~ time * sets + sex,
  random = ~ 1 | participant,
  data = strengthvolume_legpress,  # Bruker det oppdaterte datasettet
  na.action = na.omit
)

# Oppsummer modellresultatene
summary_modell_legpress <- summary(model_legpress)

# Hent ut estimatet og standardfeilen for 'post'
post_koeff_press <- summary_modell_legpress$tTable["timepost", "Value"]
post_se_press <- summary_modell_legpress$tTable["timepost", "Std.Error"]

# Beregn konfidensintervallet for post-estimatet
alpha <- 0.05
z_value <- qnorm(1 - alpha / 2)

lower_bound_press <- post_koeff_press - z_value * post_se_press
upper_bound_press <- post_koeff_press + z_value * post_se_press

konfidensintervall_post_press <- c(lower_bound_press, upper_bound_press)

# Hent ut estimatet og standardfeilen for interaksjonen mellom 'timepost' og 'setssingle'
interaksjon_koeff_press <- summary_modell_legpress$tTable["timepost:setssingle", "Value"]
interaksjon_se_press <- summary_modell_legpress$tTable["timepost:setssingle", "Std.Error"]

# Beregn konfidensintervallet for interaksjonsestimatet
lower_bound_interaksjon_press <- interaksjon_koeff_press - z_value * interaksjon_se_press
upper_bound_interaksjon_press <- interaksjon_koeff_press + z_value * interaksjon_se_press

konfidensintervall_interaksjon_press <- c(lower_bound_interaksjon_press, upper_bound_interaksjon_press)

# Last inn dataene
library(exscidata)
library(tidyverse)
library(nlme)
data("dxadata")

# Filtrer datasettet for muskeltverrsnitt og forbered dataene
muskeltverr <- dxadata %>%
  select(participant:include, lean.left_leg, lean.right_leg) %>%
  pivot_longer(names_to = "leg", 
               values_to = "lean.mass", 
               cols = lean.left_leg:lean.right_leg) %>%
  mutate(leg = if_else(leg == "lean.left_leg", "L", "R"), 
         sets = if_else(multiple == leg, "multiple", "single"),
         time = factor(time, levels = c("pre", "post"))) %>%
  select(participant, time, sex, include, sets, leg, lean.mass)

# Modell for muskeltverrsnitt uten manglende verdier
model_muskel <- lme(
  fixed = lean.mass ~ time * sets + sex,
  random = ~ 1 | participant,
  data = muskeltverr,
  na.action = na.omit
)

# Oppsummer modellen
summary_modell_muskel <- summary(model_muskel)

# Hent ut estimatet og standardfeilen for 'timepost'
post_koeff_muskel <- summary_modell_muskel$tTable["timepost", "Value"]
post_se_muskel <- summary_modell_muskel$tTable["timepost", "Std.Error"]

# Beregn konfidensintervallet for post-estimatet
alpha <- 0.05
z_value <- qnorm(1 - alpha / 2)

lower_bound_muskel <- post_koeff_muskel - z_value * post_se_muskel
upper_bound_muskel <- post_koeff_muskel + z_value * post_se_muskel

konfidensintervall_muskel <- c(lower_bound_muskel, upper_bound_muskel)

# Hent ut estimatet og standardfeilen for interaksjonen mellom 'timepost' og 'setssingle'
interaksjon_koeff_muskel <- summary_modell_muskel$tTable["timepost:setssingle", "Value"]
interaksjon_se_muskel <- summary_modell_muskel$tTable["timepost:setssingle", "Std.Error"]

# Beregn konfidensintervallet for interaksjonsestimatet
lower_bound_interaksjon_muskel <- interaksjon_koeff_muskel - z_value * interaksjon_se_muskel
upper_bound_interaksjon_muskel <- interaksjon_koeff_muskel + z_value * interaksjon_se_muskel

konfidensintervall_interaksjon_muskel <- c(lower_bound_interaksjon_muskel, upper_bound_interaksjon_muskel)