#add weight to the species

#clean up 
rm(list = ls())

#create not in
#`` what you need for filtering special column names

'%ni%' <- Negate('%in%')

#libraries
library(tidyverse)

#get data
#https://wiley.figshare.com/articles/dataset/Full_Archive/3563457/1
sm <- read_csv("./inputs/meta/species_meta.csv")
amniote <- read_csv("inputs/meta/Amniote_Database_Aug_2015.csv")

#clean and trim both before join

sm <- sm %>%
  mutate(
    name = case_when(
      !is.na(use_name) ~ use_name,
      TRUE ~ scientific
    )
  ) %>% 
  dplyr::select(species,name)

amn <- amniote %>% 
  mutate(name = paste0(genus, " ", species),
         kg = adult_body_mass_g/1000
         ) %>% 
  dplyr::select(name, kg) %>% 
  filter(kg  > 0) #-0.999 means no data

#join

species_kg <- left_join(sm,amn) %>% 
  dplyr::select(-name)

species_kg %>% filter(is.na(kg))

write_csv(species_kg, "./inputs/meta/species_kg.csv")
saveRDS(species_kg, "./inputs/meta/species_kg.rds")



