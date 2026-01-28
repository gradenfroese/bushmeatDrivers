#prep inputs

##### clean up

rm(list = ls())

#create not in
#`` what you need for filtering special column names

'%ni%' <- Negate('%in%')

###load packages
library("plyr") #ALWAYS HAVE TO LOAD BEFORE DPLYR, OR ELSE IT MASKS DPLYR
library("tidyverse")
library("lubridate")
library("janitor")

#functions

source("./scripts/background/functions_PABBM.R")

#village meta

village_meta <- readRDS("./inputs/meta/village_meta.rds") %>% 
  mutate_if(is.character,funs(factor(.)))

capture <- left_join(readRDS("./inputs/meta/capture_rates_p2.rds"), village_meta) %>% 
  dplyr::select(village:km_mkk) %>% 
  arrange(desc(capture_rate))

unique(village_meta$p2_data)
unique(village_meta$p3_data)

cut_villages <- village_meta %>% 
  filter(!is.na(p2_data) | !is.na(p3_data)) %>% 
  pull(village)

village_meta <- village_meta %>% 
  filter(village %ni% cut_villages) %>% 
  dplyr::select(village:km_voisins)

#does capture rate (from phase 2 carto villages, see AJE) seem to change with distance? offtake? 
#data obs limited, but seems unrelated to distance, seems to increase with offtake
#(if it's true villages with higher offtake higher capture rate, diff between villages will seem smaller than they are...
#..conservative donc input into models...
#(if it was realted to distance, that could cause concern for relationship btwn distance and offtake)

plot(capture$capture_rate ~ capture$km_mkk, pch = 19)
cor(capture$capture_rate,capture$km_mkk)

plot(capture$capture_rate ~ capture$offtake_counted, pch = 19)
cor(capture$capture_rate,capture$offtake_counted)

#active households in each phase

read_eff <- function (x) {readRDS(x) %>% 
  filter(village %ni% cut_villages) %>% 
  mutate_if(is.character,funs(factor(.)))  %>% 
  group_by(village) %>%
  filter(session != min(session)) %>% #cut first session in each village
  ungroup() %>% 
  filter(session < 15) #last session was cut short in phase 2, phase 3 only has 14
  }

e2 <-  read_eff("./inputs/effort_transects_phase2.rds") 
e3 <-  read_eff("./inputs/effort_transects_phase3.rds") 

get_maison <- function (x) {
  
  x %>% 
    group_by(village) %>% 
    mutate(n_maison = ME + MPE) %>% 
    summarize(n_maison = median(n_maison))
}

median_maison <- left_join(
  get_maison(e2) %>% rename(n_maison_p2 = n_maison),
  get_maison(e3) %>% rename(n_maison_p3 = n_maison)
)


village_covs <- left_join(village_meta,median_maison)

#add yearly offtake

get_yearly <- function (phase_list, phase_tibble, phase_name) {
  

  pl <- readRDS(phase_list)
  
  tib_name <- deparse(substitute(phase_tibble))
  
  tmp1 <- pl[[tib_name]] %>% 
    dplyr::select(-phase,-n_offtake_total,-kg_offtake_total,-n_days)
  
  colnames(tmp1) <- c("village", paste0(colnames(tmp1)[2:7], phase_name))
  
  return(tmp1)
  
  
}


#all inputs before centering and scaling

village_covs <- left_join(
left_join(village_covs,get_yearly("./inputs/offtake/offtake_list_p2.rds", 
                                  offtake_yearly_p2, "_p2")),
get_yearly("./inputs/offtake/offtake_list_p3.rds", offtake_yearly_p3, "_p3")
)


join_covs <- function (x) {
  
  y <- village_covs %>% 
    dplyr::select(-n_offtake_yearly_lo_p2,-n_offtake_yearly_hi_p2,
                  -n_offtake_yearly_lo_p3,-n_offtake_yearly_hi_p3,
                  -kg_offtake_yearly_lo_p2,-kg_offtake_yearly_hi_p2,
                  -kg_offtake_yearly_lo_p3,-kg_offtake_yearly_hi_p3
                  )

  left_join(x,y)
  
}

daily_p2 <- join_covs(readRDS("./inputs/offtake/offtake_list_p2.rds")$offtake_daily_p2)
daily_p3 <- join_covs(readRDS("./inputs/offtake/offtake_list_p3.rds")$offtake_daily_p3)

gun_p2 <- join_covs(readRDS("./inputs/offtake/offtake_list_p2.rds")$offtakeF_p2)
gun_p3 <- join_covs(readRDS("./inputs/offtake/offtake_list_p3.rds")$offtakeF_p3)

trap_p2 <- join_covs(readRDS("./inputs/offtake/offtake_list_p2.rds")$offtakeP_p2)
trap_p3 <- join_covs(readRDS("./inputs/offtake/offtake_list_p3.rds")$offtakeP_p3)


village_covs
daily_p2
gun_p2 

village_covs

#standardize and center stuff

daily_offtake <- bind_rows(daily_p2,daily_p3)
gun_kg <- bind_rows(gun_p2,gun_p3)
trap_kg <- bind_rows(trap_p2,trap_p3)
village_covs

col_stdCS <- daily_offtake %>% 
  dplyr::select(km_mkk:kg_offtake_yearly_p3) %>% 
  colnames()

apply_stdCS <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols), ~ stdCS_pred(.x)))
}

daily_offtake_CS <- apply_stdCS(daily_offtake, col_stdCS)
gun_kg_CS <- apply_stdCS(gun_kg, col_stdCS)
trap_kg_CS <- apply_stdCS(trap_kg, col_stdCS)


village_covs_tmp <- village_covs %>% 
  dplyr::select(-n_offtake_yearly_lo_p2, -n_offtake_yearly_hi_p2,
                -n_offtake_yearly_lo_p3, -n_offtake_yearly_hi_p3,
                -kg_offtake_yearly_lo_p2, -kg_offtake_yearly_hi_p2,
                -kg_offtake_yearly_lo_p3, -kg_offtake_yearly_hi_p3,
                )

col_tmp <- colnames(village_covs_tmp)[-1]

vc_cs_tmp <- apply_stdCS(village_covs_tmp,col_tmp)
colnames(vc_cs_tmp) <- c("village", paste0(col_tmp, "_CS"))

village_covs_CS <- left_join(village_covs_tmp,vc_cs_tmp)

bmdr_inputs <- list(village_covs, daily_offtake, gun_kg,trap_kg)
names(bmdr_inputs) <- c("village_covs", "daily_offtake", "gun_kg", "trap_kg")

bmdr_inputs_CS <- list(village_covs_CS, daily_offtake_CS, gun_kg_CS,trap_kg_CS)
names(bmdr_inputs_CS) <- c("village_covs_CS", "daily_offtake_CS", "gun_kg_CS", "trap_kg_CS")

dvl <- function(tibble_list) { #drop village levels
  purrr::map(tibble_list, function(df) {
    df %>% dplyr::mutate(village = forcats::fct_drop(village))
  })
}

bmdr_inputs <- dvl(bmdr_inputs)
bmdr_inputs_CS <- dvl(bmdr_inputs_CS)

saveRDS(bmdr_inputs, "./inputs/offtake/bmdr_inputs.rds")
saveRDS(bmdr_inputs_CS, "./inputs/offtake/bmdr_inputs_CS.rds")

#graph raw relationships
#brute force copy n paste rather than looping to slow and think

colnames(village_covs)

#distance mkk increases offtake, consistent over time
plot(village_covs$n_offtake_yearly_p2 ~ village_covs$km_mkk, pch = 19, col = "indianred2")
points(village_covs$n_offtake_yearly_p3 ~ village_covs$km_mkk, pch = 19, col = "royalblue2")

#same with isolation...
plot(village_covs$n_offtake_yearly_p2 ~ village_covs$km_voisins, pch = 19, col = "indianred2")
points(village_covs$n_offtake_yearly_p3 ~ village_covs$km_voisins, pch = 19, col = "royalblue2")

#far weaker trend for n maison, especially in phase 3
plot(village_covs$n_offtake_yearly_p2 ~ village_covs$n_maison_p2, pch = 19, col = "indianred2")
points(village_covs$n_offtake_yearly_p3 ~ village_covs$n_maison_p3, pch = 19, col = "royalblue2")

#correlations between the above predictors?...pretty strong for km mkk and voisins (0.69)
plot(village_covs$km_mkk ~ village_covs$km_voisins, pch = 19)
cor(village_covs$km_mkk, village_covs$km_voisins)

#absent for n maison
plot(village_covs$km_mkk ~ village_covs$n_maison_p2, pch = 19)
plot(village_covs$km_mkk ~ village_covs$n_maison_p3, pch = 19)
plot(village_covs$km_voisins ~ village_covs$n_maison_p2, pch = 19)
plot(village_covs$km_voisins ~ village_covs$n_maison_p3, pch = 19)

#SAME WHEN WE LOOK AT KG ?
#MAISON BECOMES FAR MORE IMPORTANT

#distance mkk increases offtake, consistent over time
plot(village_covs$kg_offtake_yearly_p2 ~ village_covs$km_mkk, pch = 19, col = "indianred2")
points(village_covs$kg_offtake_yearly_p3 ~ village_covs$km_mkk, pch = 19, col = "royalblue2")

#same with isolation...
plot(village_covs$kg_offtake_yearly_p2 ~ village_covs$km_voisins, pch = 19, col = "indianred2")
points(village_covs$kg_offtake_yearly_p3 ~ village_covs$km_voisins, pch = 19, col = "royalblue2")

#now it seems same trend for n maison
plot(village_covs$kg_offtake_yearly_p2 ~ village_covs$n_maison_p2, pch = 19, col = "indianred2")
points(village_covs$kg_offtake_yearly_p3 ~ village_covs$n_maison_p3, pch = 19, col = "royalblue2")

#SAME TRENDS WITH DAILY? (yes)

#distance mkk increases offtake, consistent over time
plot(daily_p2$n_offtake ~ daily_p2$km_mkk, pch = 19, col = "indianred2")
points(daily_p3$n_offtake ~ daily_p3$km_mkk, pch = 19, col = "royalblue2")

#same with isolation...
plot(daily_p2$n_offtake ~ daily_p2$km_voisins, pch = 19, col = "indianred2")
points(daily_p3$n_offtake~ daily_p3$km_voisins, pch = 19, col = "royalblue2")

#far weaker trend for n maison, especially in phase 3
plot(daily_p2$n_offtake ~ daily_p2$n_maison_p2, pch = 19, col = "indianred2")
points(daily_p3$n_offtake ~ daily_p3$n_maison_p3, pch = 19, col = "royalblue2")

#distance mkk increases offtake, consistent over time
plot(daily_p2$kg_offtake ~ daily_p2$km_mkk, pch = 19, col = "indianred2")
points(daily_p3$kg_offtake ~ daily_p3$km_mkk, pch = 19, col = "royalblue2")

#same with isolation...
plot(daily_p2$kg_offtake ~ daily_p2$km_voisins, pch = 19, col = "indianred2")
points(daily_p3$kg_offtake~ daily_p3$km_voisins, pch = 19, col = "royalblue2")

#far weaker trend for n maison, especially in phase 3
plot(daily_p2$kg_offtake ~ daily_p2$n_maison_p2, pch = 19, col = "indianred2")
points(daily_p3$kg_offtake ~ daily_p3$n_maison_p3, pch = 19, col = "royalblue2")

#WHAT ABOUT MBMI ? (in realtion to distance, isolation, and n maison)
#no trend seen

plot(gun_p2$kg ~ gun_p2$km_mkk, pch = 19, col = "indianred2")
points(gun_p3$kg ~ gun_p3$km_mkk, pch = 19, col = "royalblue2")

plot(gun_p2$kg ~ gun_p2$km_voisins, pch = 19, col = "indianred2")
points(gun_p3$kg ~ gun_p3$km_voisins, pch = 19, col = "royalblue2")

plot(gun_p2$kg ~ gun_p2$n_maison_p2, pch = 19, col = "indianred2")
points(gun_p3$kg ~ gun_p3$n_maison_p3, pch = 19, col = "royalblue2")

plot(trap_p2$kg ~ trap_p2$km_mkk, pch = 19, col = "indianred2")
points(trap_p3$kg ~ trap_p3$km_mkk, pch = 19, col = "royalblue2")

plot(trap_p2$kg ~ trap_p2$km_voisins, pch = 19, col = "indianred2")
points(trap_p3$kg ~ trap_p3$km_voisins, pch = 19, col = "royalblue2")

plot(trap_p2$kg ~ trap_p2$n_maison_p2, pch = 19, col = "indianred2")
points(trap_p3$kg ~ trap_p3$n_maison_p3, pch = 19, col = "royalblue2")

#DOES PAST HUNTING PREDICT CURRENT HUNTING?
#seems to, though not perfectly (raw correlation 0.62 for n hunted, 0.56 for kg hunted)

plot(village_covs$n_offtake_yearly_p3 ~ village_covs$n_offtake_yearly_p2, pch = 19)
cor(village_covs$n_offtake_yearly_p3, village_covs$n_offtake_yearly_p2)

plot(village_covs$kg_offtake_yearly_p3 ~ village_covs$kg_offtake_yearly_p2, pch = 19)
cor(village_covs$kg_offtake_yearly_p3, village_covs$kg_offtake_yearly_p2)

#IS MBMI PREDICTED BY CURRENT OR PAST HUNTING? 
#does not seem to be at all

plot(gun_p3$kg~ gun_p3$n_offtake_yearly_p3, pch = 19)
plot(gun_p2$kg~ gun_p2$n_offtake_yearly_p2, pch = 19)
plot(gun_p3$kg~ gun_p3$n_offtake_yearly_p2, pch = 19)

plot(gun_p3$kg~ gun_p3$kg_offtake_yearly_p3, pch = 19)
plot(gun_p2$kg~ gun_p2$kg_offtake_yearly_p2, pch = 19)
plot(gun_p3$kg~ gun_p3$kg_offtake_yearly_p2, pch = 19)

plot(trap_p3$kg~ trap_p3$n_offtake_yearly_p3, pch = 19)
plot(trap_p2$kg~ trap_p2$n_offtake_yearly_p2, pch = 19)
plot(trap_p3$kg~ trap_p3$n_offtake_yearly_p2, pch = 19)

plot(trap_p3$kg~ trap_p3$kg_offtake_yearly_p3, pch = 19)
plot(trap_p2$kg~ trap_p2$kg_offtake_yearly_p2, pch = 19)
plot(trap_p3$kg~ trap_p3$kg_offtake_yearly_p2, pch = 19)

#which village hunt more or less over time

village_change <- village_covs %>% 
  mutate(change_n = n_offtake_yearly_p3 - n_offtake_yearly_p2,
         change_kg = kg_offtake_yearly_p3 - kg_offtake_yearly_p2,
         change_maison = n_maison_p3 - n_maison_p2
         ) %>% 
  dplyr::select(village, km_mkk,km_voisins,n_maison_p2,n_maison_p3, 
                change_maison, change_n,change_kg)

#what about changing kg?

#gun data there is enough for all villages
gun_change <- left_join(
gun_p3 %>% 
  group_by(village) %>% 
  summarize(n_p2 = n(),
            gun_kg_p3 = mean(kg)),
gun_p2 %>% 
  group_by(village) %>% 
  summarize(n_p3 = n(),
            gun_kg_p2 = mean(kg))
) %>% 
  mutate(change_gun_kg = gun_kg_p3 - gun_kg_p2) %>% 
  dplyr::select(village,n_p2, n_p3, change_gun_kg)

#trap data C3, C4,C5,E1,E9 not enough data
trap_change <- left_join(
  trap_p3 %>% 
    group_by(village) %>% 
    summarize(n_p2 = n(),
              trap_kg_p3 = mean(kg)),
  trap_p2 %>% 
    group_by(village) %>% 
    summarize(n_p3 = n(),
              trap_kg_p2 = mean(kg))
) %>% 
  mutate(change_trap_kg = trap_kg_p3 - trap_kg_p2) %>% 
  dplyr::select(village,n_p2, n_p3, change_trap_kg)

trap_change <- trap_change %>% 
  mutate(change_trap_kg = case_when(
    n_p2 < 30 | n_p3 < 30 ~ NA_real_,
    TRUE ~ change_trap_kg
  ))

kg_change <- left_join(gun_change %>% dplyr::select(village,change_gun_kg),
                       trap_change %>% dplyr::select(village,change_trap_kg))

kg_change

#kg change was minimal, except for villages C4 and E1

village_change <- left_join(village_change,kg_change) 
village_change

#is there a pattern to changes in hunting? 
#not apparent

plot(village_change$change_n ~ village_change$km_mkk, pch = 19)
plot(village_change$change_n ~ village_change$km_voisins, pch = 19)
plot(village_change$change_n ~ village_change$n_maison_p2, pch = 19)
plot(village_change$change_n ~ village_change$n_maison_p3, pch = 19)
plot(village_change$change_n ~ village_change$change_maison, pch = 19)
cor(village_change$change_n,village_change$change_maison)

#nor in mbmi

plot(village_change$change_gun_kg ~ village_change$km_mkk, pch = 19)
plot(village_change$change_gun_kg ~ village_change$km_voisins, pch = 19)
plot(village_change$change_gun_kg ~ village_change$n_maison_p2, pch = 19)
plot(village_change$change_gun_kg ~ village_change$n_maison_p3, pch = 19)
plot(village_change$change_gun_kg ~ village_change$change_maison, pch = 19)
cor(village_change$change_gun_kg,village_change$change_maison)

plot(village_change$change_trap_kg ~ village_change$km_mkk, pch = 19)
plot(village_change$change_trap_kg ~ village_change$km_voisins, pch = 19)
plot(village_change$change_trap_kg ~ village_change$n_maison_p2, pch = 19)
plot(village_change$change_trap_kg ~ village_change$n_maison_p3, pch = 19)
plot(village_change$change_trap_kg ~ village_change$change_maison, pch = 19)


plot(village_change$change_gun_kg ~ village_change$change_n, pch = 19)
plot(village_change$change_gun_kg ~ village_change$change_kg, pch = 19)

plot(village_change$change_trap_kg ~ village_change$change_n, pch = 19)
plot(village_change$change_trap_kg ~ village_change$change_kg, pch = 19)


