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
