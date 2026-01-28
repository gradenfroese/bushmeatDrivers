#posterior predictions and plots

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
library("brms")
library("bayesplot")
library("patchwork")
library("colorspace")
library("scales")
library("grid")
library("cowplot")
library("jpeg")
library("imager")

#functions

source("./scripts/background/functions_PABBM.R")

#inputs

bmdr_inputs <- readRDS("./inputs/offtake/bmdr_inputs.rds")

fphase <- function(df) {
  if ("phase" %in% names(df)) {
    df %>% mutate(phase = factor(phase))
  } else {
    df
  }
}

bmdr_inputs <- lapply(bmdr_inputs, fphase)
village_covs <- bmdr_inputs$village_covs

daily <- bmdr_inputs$daily_offtake  
daily_p2 <- daily %>% filter(phase == 2)
daily_p3 <- daily %>% filter(phase == 3)

gun_kg <- bmdr_inputs$gun_kg 
gun_kg_p2 <- gun_kg %>% filter(phase == 2)
gun_kg_p3 <- gun_kg %>% filter(phase == 3)

trap_kg <- bmdr_inputs$trap_kg 
trap_kg_p2 <- trap_kg %>% filter(phase == 2)
trap_kg_p3 <- trap_kg %>% filter(phase == 3)

#yearly species
sm <- read_csv("./inputs/meta/species_meta.csv")

oys_p2 <- readRDS("./inputs/offtake/offtake_yearly_species_p2.rds")
oys_p3 <- readRDS("./inputs/offtake/offtake_yearly_species_p3.rds")
oysv_p2 <- readRDS("./inputs/offtake/offtake_yearly_species_village_p2.rds")
oysv_p3 <- readRDS("./inputs/offtake/offtake_yearly_species_village_p3.rds")

#overall counted offtake

length(unique(daily$village)) #12 villages
length(unique(daily$date)) #290 work days
sum(daily$n_offtake) #9224 hunted animals
sum(oys_p2$n_offtake_total) + sum(oys_p3$n_offtake_total) #9224 hunted animals
sm %>% filter(species %in% oys_p2$species | species %in% oys_p3$species) %>% nrow() #56 species
c(oys_p2$species,oys_p3$species) %>% unique() %>% length()  #56 species

#read fits

fits_folder <- "./outputs/fits"

fits <- list.files(fits_folder, pattern = "\\.rds$", full.names = TRUE)

for (f in fits) {
  obj_name <- tools::file_path_sans_ext(basename(f))
  assign(obj_name, readRDS(f))
}

all_fits <- setNames(
  lapply(fits, readRDS),
  tools::file_path_sans_ext(basename(fits))
)

lab_phase <- function (x) { #function you'll use later
  x %>% 
    mutate(phase_lab =  case_when(
      phase == "2" ~ "2019/2020",
      phase == "3" ~ "2024/2025")
    ) %>% 
    mutate(phase_lab = factor(phase_lab, levels = c("2019/2020", "2024/2025"))
    )
}

#ok let's get posterior predictions
#remind yourself different ways of doing this...

names(all_fits)

n_fits <- all_fits[c("n_p2", "n_p3")]
gkg_fits <-  all_fits[c("gkg_p2", "gkg_p3")]
tkg_fits <-  all_fits[c("tkg_p2", "tkg_p3")]

p2_pred <- posterior_epred(gkg_p2)
p2_pred2 <- fitted(gkg_p2)
p2_pred3 <- posterior_predict(gkg_p2)

mean(p2_pred)
as.numeric(quantile(p2_pred, 0.04))
as.numeric(quantile(p2_pred, 0.96))

mean(p2_pred2)
as.numeric(quantile(p2_pred2, 0.04))
as.numeric(quantile(p2_pred2, 0.96))

mean(p2_pred3)
as.numeric(quantile(p2_pred3, 0.04))
as.numeric(quantile(p2_pred3, 0.96))

mean(daily_p2$n_offtake)
as.numeric(quantile(daily_p2$kg_offtake, 0.04))
as.numeric(quantile(daily_p2$n_offtake, 0.96))

mean(gun_kg_p2$kg)
as.numeric(quantile(gun_kg_p2$kg, 0.04))
as.numeric(quantile(gun_kg_p2$kg, 0.96))

mean(trap_kg_p2$kg)
as.numeric(quantile(trap_kg_p2$kg, 0.04))
as.numeric(quantile(trap_kg_p2$kg, 0.96))

#predict for range of km

sort(village_covs$km_mkk)
sort(village_covs$n_maison_p2)
sort(village_covs$n_maison_p3)

range_km <- seq(10,60,5)
range_maison <- seq(5,65,5)

mean(village_covs$km_mkk); median(village_covs$km_mkk)
mean(village_covs$n_maison_p2); median(village_covs$n_maison_p2)
mean(village_covs$n_maison_p3); median(village_covs$n_maison_p3)

get_pp_mkk_maison <- function (choose_range, choose_phase, choose_model, choose_data) {
  
  phase_tmp <- choose_phase
  range_tmp <- choose_range
  data_tmp  <- choose_data
  model_tmp <- choose_model
  
  # output tibble
  tmp_tib <- tibble(
    phase = phase_tmp,
    range = range_tmp,
    u  = NA_real_,
    lo = NA_real_,
    hi = NA_real_
  )
  
  # initialize list
  nd_list <- vector("list", length(range_tmp))
  
  # decide WHICH predictor to vary:
  # identify by comparing object *names*, not by value
  is_km <- identical(choose_range, range_km)
  
  if (is_km) {
    
    # vary km_mkk
    for (i in seq_along(range_tmp)) {
      nd_list[[i]] <- data_tmp %>%
        mutate(km_mkk = range_tmp[i],
               n_maison_p2 = 30,
               n_maison_p3 = 30
               )
    }
    
  } else {
    
    # vary both maison variables
    for (i in seq_along(range_tmp)) {
      nd_list[[i]] <- data_tmp %>%
        mutate(km_mkk = 25,
          n_maison_p2 = range_tmp[i],
          n_maison_p3 = range_tmp[i]
        )
    }
  }
  
  # compute posterior predictions
  for (i in seq_along(range_tmp)) {
    
    pp_tmp <- posterior_epred(model_tmp, newdata = nd_list[[i]])
    
    tmp_tib$u[i]  <- mean(pp_tmp)
    tmp_tib$lo[i] <- quantile(pp_tmp, 0.04)
    tmp_tib$hi[i] <- quantile(pp_tmp, 0.96)
  }
  
  return(tmp_tib)
}

#function (choose_range, choose_phase, choose_model, choose_data) {
pp_mkk_p2 <- get_pp_mkk_maison(range_km,2,n_p2,daily_p2)
pp_mkk_p3 <- get_pp_mkk_maison(range_km,3,n_p3,daily_p3)
pp_maison_p2 <- get_pp_mkk_maison(range_maison,2,n_p2,daily_p2)
pp_maison_p3 <- get_pp_mkk_maison(range_maison,3,n_p3,daily_p3)

pp_gkg_mkk_p2 <- get_pp_mkk_maison(range_km,2,gkg_p2,gun_kg_p2)
pp_gkg_mkk_p3 <- get_pp_mkk_maison(range_km,3,gkg_p3,gun_kg_p3)
pp_gkg_maison_p2 <- get_pp_mkk_maison(range_maison,2,gkg_p2,gun_kg_p2)
pp_gkg_maison_p3 <- get_pp_mkk_maison(range_maison,3,gkg_p3,gun_kg_p3)

mean(gun_kg_p2$kg)
gun_kg_p2 %>% filter(km_mkk > 55) %>% pull(kg) %>% mean()
gun_kg_p2 %>% filter(km_mkk < 40) %>% pull(kg) %>% mean()
gun_kg_p3 %>% filter(km_mkk > 40) %>% pull(kg) %>% mean()
gun_kg_p3 %>% filter(km_mkk < 20) %>% pull(kg) %>% mean()

pp_tkg_mkk_p2 <- get_pp_mkk_maison(range_km,2,tkg_p2,trap_kg_p2)
pp_tkg_mkk_p3 <- get_pp_mkk_maison(range_km,3,tkg_p3,trap_kg_p3)
pp_tkg_maison_p2 <- get_pp_mkk_maison(range_maison,2,tkg_p2,trap_kg_p2)
pp_tkg_maison_p3 <- get_pp_mkk_maison(range_maison,3,tkg_p3,trap_kg_p3)

#calculate annual offtake from it

ulohi <- function (x) {x %>%   
  mutate(u_u = (u*365)/.53,
         u_lo = (u*365)/.6,
         u_hi = (u*365)/.4,
         lo_u = (lo*365)/.53,
         lo_lo = (lo*365)/.6,
         lo_hi = (lo*365)/.4,
         hi_u = (hi*365)/.53,
         hi_lo = (hi*365)/.6,
         hi_hi = (hi*365)/.4
         ) %>% 
  dplyr::select(-u,-lo,-hi) %>% 
  mutate(across(
    .cols = where(is.numeric) & !all_of(c("phase", "range")),
    .fns  = ~ round(.x, 0)
  )) 
  # rowwise() %>%
  # mutate(lo = min(c_across(!all_of(c("phase", "range")))),
  #        hi = max(c_across(!all_of(c("phase", "range")))),
  #        ) %>%
  # ungroup()
}

lab_phase <- function (x) {
  x %>% 
    mutate(phase_lab =  case_when(
      phase == "2" ~ "2019/2020",
      phase == "3" ~ "2024/2025")
    ) %>% 
    mutate(phase_lab = factor(phase_lab, levels = c("2019/2020", "2024/2025"))
    )
}

ulohi_mkk_p2 <- ulohi(pp_mkk_p2) %>% lab_phase()
ulohi_mkk_p3 <- ulohi(pp_mkk_p3) %>% lab_phase()
ulohi_maison_p2 <- ulohi(pp_maison_p2) %>% lab_phase()
ulohi_maison_p3 <- ulohi(pp_maison_p3) %>% lab_phase()

ulohi_mkk <- bind_rows(ulohi_mkk_p2,ulohi_mkk_p3)
ulohi_maison <- bind_rows(ulohi_maison_p2,ulohi_maison_p3)

ulohi_gkg_mkk <- bind_rows(pp_gkg_mkk_p2,pp_gkg_mkk_p3) %>% lab_phase()
ulohi_gkg_maison <- bind_rows(pp_gkg_maison_p2,pp_gkg_maison_p3) %>% lab_phase()

ulohi_tkg_mkk <- bind_rows(pp_tkg_mkk_p2,pp_tkg_mkk_p3) %>% lab_phase()
ulohi_tkg_maison <- bind_rows(pp_tkg_maison_p2,pp_tkg_maison_p3) %>% lab_phase()

#range animals hunted from 500 to 4000
min(ulohi_mkk$u_lo);max(ulohi_mkk$u_hi)
min(ulohi_maison$u_lo);max(ulohi_maison$u_hi)

#range avg. kg from 4 to 28
min(ulohi_gkg_mkk$lo);max(ulohi_gkg_mkk$hi)
min(ulohi_gkg_maison$lo);max(ulohi_gkg_maison$hi)

#round off for easy look at lo and hi

look_range <- function (x) {
  
  x %>% 
    mutate(lo = round(u_lo,-2), hi = round(u_hi,-2)) %>% 
    dplyr::select(phase,range, lo, hi)
}

lr_maison_p2 <- look_range(ulohi_maison_p2)
lr_maison_p3 <- look_range(ulohi_maison_p3)

lr_maison_p2
lr_maison_p3

max(lr_maison_p2$hi)/min(lr_maison_p2$hi)
max(lr_maison_p2$lo)/min(lr_maison_p2$lo)

max(lr_maison_p3$hi)/min(lr_maison_p3$hi)
max(lr_maison_p3$lo)/min(lr_maison_p3$lo)

lr_mkk_p2 <- look_range(ulohi_mkk_p2)
lr_mkk_p3 <- look_range(ulohi_mkk_p3)

lr_mkk_p2
lr_mkk_p3

max(lr_mkk_p2$hi)/min(lr_mkk_p2$hi)
max(lr_mkk_p2$lo)/min(lr_mkk_p2$lo)

max(lr_mkk_p3$hi)/min(lr_mkk_p3$hi)
max(lr_mkk_p3$lo)/min(lr_mkk_p3$lo)

lr_mkk_p2
lr_mkk_p3


#plot now
min(ulohi_tkg_mkk$lo);max(ulohi_tkg_mkk$hi)
min(ulohi_tkg_maison$lo);max(ulohi_tkg_maison$hi)

rgb(t(col2rgb("indianred2")), maxColorValue = 255)
rgb(t(col2rgb("royalblue2")), maxColorValue = 255)

plot_mkk_maison <- function (x) {
  
  ggplot(x,  aes(x=range, y=hi_hi,color = phase_lab)) +
    theme_classic()+
    geom_linerange(aes(ymin = u_lo, ymax = u_hi), size = 1, position = position_dodge(3)) +
    #geom_errorbar(aes(ymin=u_lo, ymax=u_hi),width= 0, position=position_dodge(2)) +
    scale_color_manual(values = c(`2019/2020` = "indianred2", `2024/2025` = "royalblue2")) +
    scale_y_continuous(limits =c(500, 4000), breaks = seq(500, 4000, 500))+
    labs(x = " \nDistance from Makokou (km)", y = "Yearly animals hunted\n",
         colour= "Period") +
    # scale_x_continuous(limits = c(8,62), breaks = seq(10,60,10)) +
    theme(#legend.position = "none",
      #legend.title = element_text(size=14),
      legend.title = element_blank(),
      legend.text = element_text(size=14),
      strip.background = element_blank(),
      strip.text = element_text(size=14),
      axis.line = element_blank(),
      axis.ticks = element_line(colour = "black",),
      axis.text=element_text(size=14, colour = "black"),
      legend.justification = "top",
      axis.title.x = element_text(size = 14),
      axis.title.y= element_text(size = 14),
      plot.title = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=1),
      plot.margin = unit(c(0.4, 0.2, 0.4, 0.2), "cm")
    )
  
}

ulohi_gkg_mkk

spp_kg <- readRDS("./inputs/meta/species_kg.rds") %>% 
  filter(species %in% c("CBL","CBA","CPE"))

plot_kg_mkk_maison <- function (x) {
  
  ggplot(x,  aes(x=range, y=hi,color = phase_lab)) +
    theme_classic()+
    geom_hline(yintercept=5, linetype="solid", color = "lightgray") +
    geom_hline(yintercept=15, linetype="solid", color = "lightgray") +
    #geom_linerange(aes(ymin = lo, ymax = hi), size = 1, position = position_dodge(3)) +
    geom_linerange(aes(ymin=lo, ymax=hi),position=position_dodge(3)) +
    #geom_errorbar(aes(ymin=lo, ymax=hi),width= 0, position=position_dodge(3)) +
    geom_point(aes(y = u, fill = phase_lab), shape = 21, color = "black", size = 3,
               position = position_dodge(3), show.legend = FALSE)+
    scale_fill_manual(values = c(`2019/2020` = "indianred2", `2024/2025` = "royalblue2")) +
    scale_color_manual(values = c(`2019/2020` = "indianred2", `2024/2025` = "royalblue2")) +
    scale_y_continuous(limits =c(4, 28), breaks = seq(5, 25, 5))+
    labs(x = " \nDistance from Makokou (km)", y = "kg/animal\n",
         colour= "Period") +
    scale_x_continuous(limits = c(4,66), breaks = seq(10,60,10))+
    theme(#legend.position = "none",
      legend.title = element_blank(),
      #legend.title = element_text(size=14),
      legend.text = element_text(size=14),
      strip.background = element_blank(),
      strip.text = element_text(size=14),
      axis.line = element_blank(),
      axis.ticks = element_line(colour = "black",),
      axis.text=element_text(size=14, colour = "black"),
      legend.justification = "top",
      axis.title.x = element_text(size = 14),
      axis.title.y= element_text(size = 14),
      plot.title = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=1),
      plot.margin = unit(c(0.4, 0.2, 0.4, 0.2), "cm")
    )
  
}

pl_gkg_maison <- plot_kg_mkk_maison(ulohi_gkg_maison) +
  labs(x = "Number of households\n", y = "kg/animal shot\n")+
  scale_x_continuous(limits = c(4,66), breaks = seq(10,60,10), position = "top")+
  #annotate("text", x = 35, y = 25, label = "GUN", size = 6, fontface = "bold")+
  #annotate("text", x = 8, y = 27, label = "(a)", size = 6, fontface = "bold")
  annotate("text", label = "(a)",size = 6, fontface = "bold",
           x = -Inf, y = Inf, hjust = -0.3, vjust = 1.4)

pl_gkg_mkk <- plot_kg_mkk_maison(ulohi_gkg_mkk) +
  scale_y_continuous(limits =c(4, 28), breaks = seq(5, 25, 5), position = "right")+
  labs(x = "Distance from Makokou (km)\n", y = "kg/animal shot\n")+
  scale_x_continuous(limits = c(4,66), breaks = seq(10,60,10), position = "top")+
  #annotate("text", x = 35, y = 25, label = "GUN", size = 6, fontface = "bold")+
  #annotate("text", x = 8, y = 27, label = "(b)", size = 6, fontface = "bold")
  annotate("text", label = "(b)",size = 6, fontface = "bold",
           x = -Inf, y = Inf, hjust = -0.3, vjust = 1.4)

pl_tkg_maison <- plot_kg_mkk_maison(ulohi_tkg_maison) +
  labs(x = "\nNumber of households", y = "kg/animal trapped\n")+
  #annotate("text", x = 35, y = 25, label = "TRAP", size = 6, fontface = "bold")+
  #annotate("text", x = 8, y = 27, label = "(c)", size = 6, fontface = "bold")
  annotate("text", label = "(c)",size = 6, fontface = "bold",
           x = -Inf, y = Inf, hjust = -0.3, vjust = 1.4)

pl_tkg_mkk <- plot_kg_mkk_maison(ulohi_tkg_mkk) +
  labs(x = "\nDistance from Makokou (km)", y = "kg/animal trapped\n")+
  scale_y_continuous(limits =c(4, 28), breaks = seq(5, 25, 5), position = "right")+
  #annotate("text", x = 35, y = 25, label = "TRAP", size = 6, fontface = "bold")+
  #annotate("text", x = 8, y = 27, label = "(d)", size = 6, fontface = "bold")
  annotate("text", label = "(d)",size = 6, fontface = "bold",
           x = -Inf, y = Inf, hjust = -0.3, vjust = 1.4)

fig_kg_mkk_maison  <- (pl_gkg_maison| pl_gkg_mkk |
                         pl_tkg_maison | pl_tkg_mkk) +
  plot_layout(ncol = 2, nrow = 2, guides = "collect", widths = c(1, 1)) &
  theme(legend.position = "right", legend.justification = "top")

fig_kg_mkk_maison

dev.off()

cairo_pdf(file = "./outputs/figs/fS1.pdf", width = 10, height =  8)

fig_kg_mkk_maison

dev.off()

range_km
range_maison

plot_maison <- plot_mkk_maison(ulohi_maison) +
  labs(x = "\nNumber of households")+
  scale_x_continuous(limits = c(4,66), breaks = seq(10,60,10)) +
  #annotate("text", x = 8, y = 4000, label = "(a)", size = 6, fontface = "bold")
  annotate("text", label = "(a)",size = 6, fontface = "bold",
           x = -Inf, y = Inf, hjust = -0.3, vjust = 1.4)

plot_mkk <- plot_mkk_maison(ulohi_mkk) +
  scale_x_continuous(limits = c(4,66), breaks = seq(10,60,10))+
  scale_y_continuous(limits =c(500, 4000), breaks = seq(500, 4000, 500), position = "right")+
  #annotate("text", x = 8, y = 4000, label = "(b)", size = 6, fontface = "bold")
  annotate("text", label = "(b)",size = 6, fontface = "bold",
           x = -Inf, y = Inf, hjust = -0.3, vjust = 1.4)

fig_mkk_maison  <- (plot_maison | plot_mkk) +
  plot_layout(guides = "collect", widths = c(1, 1)) &
  theme(legend.position = "right", legend.justification = "top")

fig_mkk_maison

dev.off()

cairo_pdf(file = "./outputs/figs/f1.pdf", width = 10, height =  8)

fig_mkk_maison

dev.off()

#get e_preds for all villages

n_fits
gkg_fits
tkg_fits

vil12 <- village_covs$village

pp_village <- function (choose_phase, choose_model, choose_data) {
  
  phase_tmp <- choose_phase
  model_tmp <- choose_model
  data_tmp <- choose_data
  
  # output tibble
  tmp_tib <- tibble(
    phase = phase_tmp,
    village = vil12,
    u  = NA_real_,
    lo = NA_real_,
    hi = NA_real_
  )
  
  # initialize list
  nd_list <- vector("list", length(vil12))
  
  # compute posterior predictions
  for (i in seq_along(vil12)) {
    
    nd_list[[i]] <- data_tmp %>% 
      filter(village == vil12[[i]])
    
    pp_tmp <- posterior_epred(model_tmp, newdata = nd_list[[i]])
    
    tmp_tib$u[i]  <- mean(pp_tmp)
    tmp_tib$lo[i] <- quantile(pp_tmp, 0.04)
    tmp_tib$hi[i] <- quantile(pp_tmp, 0.96)
  }
  
  return(tmp_tib)
  
}


#do it
pp_n_vil_p2 <- pp_village(2, n_p2,daily_p2) 
pp_n_vil_p3 <- pp_village(2, n_p3,daily_p3) 

pp_gkg_vil_p2 <- pp_village(2, gkg_p2,gun_kg_p2) 
pp_gkg_vil_p3 <- pp_village(3, gkg_p3,gun_kg_p3) 

pp_tkg_vil_p2 <- pp_village(2, tkg_p2,trap_kg_p2) 
pp_tkg_vil_p3 <- pp_village(3, tkg_p3,trap_kg_p3) 

#make test

vc_tmp_test <- village_covs %>% select(village, 
                                     u_obs_p2 = n_offtake_yearly_p2, 
                                     u_obs_p3 = n_offtake_yearly_p3)

n_vil_check <- function (x) {
  
  left_join(x %>% mutate(p_u = (u*365)/.53),vc_tmp_test) %>% 
    arrange(p_u)
  
}

kg_vil_check <- function (cpp,cdata) {
  
  tmp_pp <- cpp
  tmp_data <- cdata
  
  td2 <- tmp_data %>% 
    group_by(village) %>% 
    summarize(n = n(),
              u_obs = mean(kg))
  
  left_join(cpp, td2) %>% 
    mutate(wi = hi - lo) %>% 
    dplyr::select(phase,village,u_obs,u,n,wi,lo,hi) %>% 
    arrange(n)
  
}

#check

n_vil_check(pp_n_vil_p2)
n_vil_check(pp_n_vil_p3)

kg_vil_check(pp_gkg_vil_p2,gun_kg_p2)
kg_vil_check(pp_gkg_vil_p3,gun_kg_p3)

kg_vil_check(pp_tkg_vil_p2,trap_kg_p2)
kg_vil_check(pp_tkg_vil_p3,trap_kg_p3)

#plot

n_vil_ulohi <- village_covs %>% 
  dplyr::select(-contains(c("kg", "km", "maison"))) %>% 
  mutate(p_change = (n_offtake_yearly_p3 - n_offtake_yearly_p2) / n_offtake_yearly_p2 ) %>% 
  rename_with(~ str_replace(.x,
                            "n_offtake_yearly_p(\\d+)$",
                            "n_offtake_yearly_u_p\\1"
  )) %>%
  
  # Pivot longer
  pivot_longer(
    cols = starts_with("n_offtake_yearly"),
    names_to = c(".value", "phase"),
    names_pattern = "n_offtake_yearly_([^_]+)_p(\\d+)"
  ) %>%
  
  # Make phase numeric or factor as needed
  mutate(
    phase = as.numeric(phase)   # or as.factor(phase)
  ) %>%
  # Reorder columns
  select(village, phase, u, lo, hi, p_change) %>% 
  lab_phase()


max(n_vil_ulohi$hi)
min(n_vil_ulohi$lo)


n_vil_ulohi_pchange <- n_vil_ulohi %>% 
  mutate(village = fct_reorder(village, desc(p_change)))

n_vil_ulohi_pchange %>% arrange(desc(p_change)) %>% print(n = Inf)

plot_n_village <- function (x) {
  
  ggplot(x,  aes(x=village, y=hi,color = phase_lab)) +
    theme_classic()+
    geom_linerange(aes(ymin = lo, ymax = hi), size = 1, position = position_dodge(0.3)) +
    #geom_errorbar(aes(ymin=lo, ymax=hi),width= 0, position=position_dodge(0.2)) +
    #geom_linerange(aes(ymin=lo, ymax=hi),position=position_dodge(3)) +
    scale_color_manual(values = c(`2019/2020` = "indianred2", `2024/2025` = "royalblue2")) +
    scale_y_continuous(limits =c(300, 4100), breaks = seq(500, 4000, 500))+
    labs(x = "\nVillage", y = "Yearly animals hunted\n",
         colour= "Period") +
    # scale_x_continuous(limits = c(8,62), breaks = seq(10,60,10)) +
    theme(legend.position = "right",
      legend.justification = c("top"),
      legend.title = element_blank(),
      #legend.title = element_text(size=14),
      legend.text = element_text(size=14),
      strip.background = element_blank(),
      strip.text = element_text(size=14),
      axis.line = element_blank(),
      axis.ticks = element_line(colour = "black",),
      axis.text=element_text(size=14, colour = "black"),
      axis.title.x = element_text(size = 14),
      axis.title.y= element_text(size = 14),
      plot.title = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=1),
      plot.margin = unit(c(0.4, 0.2, 0.4, 0.2), "cm")
    )
  
}

nvt <- plot_n_village(n_vil_ulohi_pchange) +
  geom_vline(xintercept=1.5, linetype="solid", color = "black") +
  geom_vline(xintercept=8.5, linetype="solid", color = "black")

nvt

#
gkg_vil <- bind_rows(pp_gkg_vil_p2,pp_gkg_vil_p3) %>% lab_phase()
tkg_vil <- bind_rows(pp_tkg_vil_p2,pp_tkg_vil_p3) %>% lab_phase()

get_pchange <- function (x) {x %>% 
  lab_phase() %>% 
  select(village, phase, u) %>%
  tidyr::pivot_wider(
    names_from = phase,
    values_from = u,
    names_prefix = "u_p"
  ) %>%
  mutate(
    p_change = (u_p3 - u_p2) / u_p2
  ) %>% 
  dplyr::select(village,p_change)
  
}


gkg_vil <- left_join(gkg_vil, get_pchange(gkg_vil))
tkg_vil <- left_join(tkg_vil, get_pchange(tkg_vil))

peek_cor <- left_join(
left_join(
n_vil_ulohi %>% rename(u_n = u, pc_n = p_change),
gkg_vil %>% rename(u_gkg = u, pc_gkg = p_change),
by = c("village", "phase")
),
tkg_vil %>% rename(u_tkg = u, pc_tkg = p_change), by = c("village", "phase")
)

peek_cor_p2 <- peek_cor %>% filter(phase == 2)
peek_cor_p3 <- peek_cor %>% filter(phase == 3)

plot(peek_cor_p2$u_gkg ~ peek_cor_p2$u_n, pch = 19) + title(cor(peek_cor_p2$u_gkg,peek_cor_p2$u_n))
plot(peek_cor_p2$u_tkg ~ peek_cor_p2$u_n, pch = 19) + title(cor(peek_cor_p2$u_tkg,peek_cor_p2$u_n))
plot(peek_cor_p2$u_tkg ~ peek_cor_p2$u_gkg, pch = 19) + title(cor(peek_cor_p2$u_tkg,peek_cor_p2$u_gkg))

plot(peek_cor_p3$u_gkg ~ peek_cor_p3$u_n, pch = 19) + title(cor(peek_cor_p3$u_gkg,peek_cor_p3$u_n))
plot(peek_cor_p3$u_tkg ~ peek_cor_p3$u_n, pch = 19) + title(cor(peek_cor_p3$u_tkg,peek_cor_p3$u_n))
plot(peek_cor_p3$u_tkg ~ peek_cor_p3$u_gkg, pch = 19) + title(cor(peek_cor_p3$u_tkg,peek_cor_p3$u_gkg))

plot(peek_cor_p3$u_gkg ~ peek_cor_p2$u_n, pch = 19) + title(cor(peek_cor_p3$u_gkg,peek_cor_p2$u_n))
plot(peek_cor_p3$u_tkg ~ peek_cor_p2$u_n, pch = 19) + title(cor(peek_cor_p3$u_tkg,peek_cor_p2$u_n))
plot(peek_cor_p3$u_tkg ~ peek_cor_p2$u_gkg, pch = 19) + title(cor(peek_cor_p3$u_tkg,peek_cor_p2$u_gkg))

plot(peek_cor_p3$u_gkg ~ peek_cor_p2$pc_n, pch = 19) + title(cor(peek_cor_p3$u_gkg,peek_cor_p2$pc_n))
plot(peek_cor_p3$u_tkg ~ peek_cor_p2$pc_n, pch = 19) + title(cor(peek_cor_p3$u_tkg,peek_cor_p2$pc_n))

plot(peek_cor_p3$pc_gkg ~ peek_cor_p2$pc_n, pch = 19) + title(cor(peek_cor_p3$u_gkg,peek_cor_p2$pc_n))
plot(peek_cor_p3$pc_tkg ~ peek_cor_p2$pc_n, pch = 19) + title(cor(peek_cor_p3$u_tkg,peek_cor_p2$pc_n))

plot(peek_cor_p3$pc_gkg ~ peek_cor_p2$u_n, pch = 19) + title(cor(peek_cor_p3$u_gkg,peek_cor_p2$u_n))
plot(peek_cor_p3$pc_tkg ~ peek_cor_p2$u_n, pch = 19) + title(cor(peek_cor_p3$u_tkg,peek_cor_p2$u_n))

plot(peek_cor_p3$pc_gkg ~ peek_cor_p3$u_n, pch = 19) + title(cor(peek_cor_p3$u_gkg,peek_cor_p3$u_n))
plot(peek_cor_p3$pc_tkg ~ peek_cor_p3$u_n, pch = 19) + title(cor(peek_cor_p3$u_tkg,peek_cor_p3$u_n))

gkg_vil2 <- gkg_vil %>% 
  mutate(village = fct_reorder(village, desc(p_change)))

tkg_vil2 <- tkg_vil %>% 
  mutate(village = fct_reorder(village, desc(p_change)))

gkg_vil$village

max(gkg_vil$hi); max(tkg_vil$hi)
min(gkg_vil$lo); min(tkg_vil$lo)

gkg_fig <- plot_n_village(gkg_vil2)+
  labs(x = "Village\n", y = "FUSIL (kg/animal)\n") +
  scale_y_continuous(limits =c(2, 22), breaks = seq(5, 25, 5))+
  geom_vline(xintercept=4.5, linetype="solid", color = "black")+
  geom_vline(xintercept=11.5, linetype="solid", color = "black")+
  scale_x_discrete(position = "top")

gkg_fig 


tkg_fig <- plot_n_village(tkg_vil2)+
  labs(x = "\nVillage", y = "PIEGE (kg/animal)\n") +
  scale_y_continuous(limits =c(2, 22), breaks = seq(5, 25, 5))+
  geom_vline(xintercept=3.5, linetype="solid", color = "black")+
  geom_vline(xintercept=7.5, linetype="solid", color = "black")+
  scale_x_discrete(position = "bottom")

tkg_fig

fig_kg_time  <- (gkg_fig| tkg_fig ) +
  plot_layout(ncol = 1, nrow = 2, guides = "collect", widths = c(1, 1)) &
  theme(legend.position = "right", legend.justification = "top")

fig_kg_time

#estimate change exactly
pp_kg_change_village <- function (mtmp3,mtmp2,dt) {
  
  model_tmp3 <- mtmp3
  model_tmp2 <- mtmp2
  data_tmp <- dt
  
  # output tibble
  tmp_tib <- tibble(
    village = vil12,
    u  = NA_real_,
    lo = NA_real_,
    hi = NA_real_
  )
  
  #initialize list
  nd_list <- vector("list", length(vil12))

  # compute posterior predictions
  for (i in seq_along(vil12)) {

    nd_list[[i]] <- data_tmp %>% filter(village == vil12[[i]])

    pp_tmp3 <- posterior_epred(model_tmp3, newdata = nd_list[[i]])
    pp_tmp2 <- posterior_epred(model_tmp2, newdata = nd_list[[i]])
    pp_tmp32 <- pp_tmp3 - pp_tmp2

    tmp_tib$u[i]  <- mean(pp_tmp32)
    tmp_tib$lo[i] <- quantile(pp_tmp32, 0.04)
    tmp_tib$hi[i] <- quantile(pp_tmp32, 0.96)
  }
  
  return(tmp_tib)
  
}

gun_change <- pp_kg_change_village(gkg_p3, gkg_p2, village_covs)
trap_change <- pp_kg_change_village(tkg_p3,tkg_p2, village_covs)


#2019/2020
#Gun      

kg_change <- bind_rows(
  gun_change %>% mutate(guntrap = "Gun          "), #2019/2020 = 9 characters 
  trap_change %>% mutate(guntrap = "Trap        "),
) %>% 
  mutate(guntrap = factor(guntrap, levels = c("Gun          ",  "Trap        ")))

min(kg_change$lo) 
max(kg_change$hi) 

#order as in changing hunting

kg_change <- kg_change %>% 
  mutate(village = fct_relevel(village,  
                               levels(n_vil_ulohi_pchange$village)
                               )
         )

kg_change$village 

kgc_fig <- ggplot(kg_change,  aes(x=village, y=u,linetype = guntrap)) +
    theme_classic()+
    geom_hline(yintercept=0, linetype="solid", color = "gray") +
    #geom_errorbar(aes(ymin=lo, ymax=hi),color = "#9969A9", width= 0, position=position_dodge(0.2)) +
    geom_linerange(aes(ymin = lo, ymax = hi), color = "#9969A9", size = 1, position = position_dodge(0.3)) +
    geom_point(aes(y = u), fill = "#9969A9", shape = 21, color = "black", size = 4,
             position = position_dodge(0.3), show.legend = FALSE)+
    geom_vline(xintercept=1.5, linetype="solid", color = "black") +
    geom_vline(xintercept=8.5, linetype="solid", color = "black") +
    scale_linetype_manual(values = c("Gun          " = "solid", "Trap        " = "dotted")) +
    #scale_y_continuous(limits =c(-12, 12), breaks = seq(-12, 12, 4))+
    scale_y_continuous(
    limits = c(-12, 12),
    breaks = seq(-12, 12, 4),
    labels = function(x) gsub("-", "\u2013", x)
    )+
    labs(x = "\nVillage", y = "Change in kg/animal hunted\n") +
    theme(legend.position = "right",
      legend.justification = c("top"),
      legend.title = element_blank(),
      legend.text = element_text(size=14),
      strip.background = element_blank(),
      strip.text = element_text(size=14),
      axis.line = element_blank(),
      axis.ticks = element_line(colour = "black",),
      axis.text=element_text(size=14, colour = "black"),
      axis.title.x = element_text(size = 14),
      axis.title.y= element_text(size = 14),
      plot.title = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=1),
      plot.margin = unit(c(0.4, 0.2, 0.4, 0.2), "cm")
    )

n_change_fig <- nvt +
  scale_x_discrete(position = "top") +
  xlab("Village\n") +
  annotate("text", label = "(a)",size = 6, fontface = "bold",
           x = -Inf, y = Inf, hjust = -0.3, vjust = 1.4)

kg_change_fig <- kgc_fig +
  annotate("text", label = "(b)",size = 6, fontface = "bold", 
           x = -Inf, y = Inf, hjust = -0.2, vjust = 1.4)

village_change_fig <- (n_change_fig | kg_change_fig) +
  plot_layout(nrow = 2, ncol = 1) 

village_change_fig

dev.off()

cairo_pdf(file = "./outputs/figs/f2.pdf", width = 12, height =  12)

village_change_fig

dev.off()

#changes in species over time

#reload if you are jumping down here from the top
lab_phase <- function (x) { #function you'll use later
  x %>% 
    mutate(phase_lab =  case_when(
      phase == "2" ~ "2019/2020",
      phase == "3" ~ "2024/2025")
    ) %>% 
    mutate(phase_lab = factor(phase_lab, levels = c("2019/2020", "2024/2025"))
    )
}

#remind yourself how many villages
length(unique(oysv_p2$village))
length(unique(oysv_p3$village))
nrow(village_covs)
nvil <- nrow(village_covs)

#consider commonly hunted species as those with yearly offtake of at least 120 (avg. 10 per village)
p2_120 <- oys_p2 %>% filter(n_offtake_yearly_lo >= 120)
p3_120 <- oys_p3 %>% filter(n_offtake_yearly_lo >= 120)

p2_120 %>% filter(species %ni% p3_120$species) #TOR, CRN, AND CHV were common in p2 but not p3
p3_120 %>% filter(species %ni% p2_120$species) #nothing became common in p3 that was NOT in p2

#there are thus 20 common species
spp120 <- unique(c(p2_120$species, p3_120$species))

#these 20 species make up 94% of hunted animals

oys_n_p23 <- full_join(oys_p2 %>% select(species, n_p2 = n_offtake_total),
                     oys_p3 %>% select(species, n_p3 = n_offtake_total)) %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>% 
  mutate(n_p23 = n_p2 + n_p3)

spp120_n_p23 <- oys_n_p23 %>% filter(species %in% spp120)

sum(spp120_n_p23$n_p23)/sum(oys_n_p23$n_p23)*100

#only PCM and TOR drastically change, % change is same with 53%,40%, or 60% capture rates
spp_change <- left_join(
  oys_p2 %>% filter(species %in% spp120) %>% select(species,
                                                    u_p2 = n_offtake_yearly,
                                                    lo_p2 = n_offtake_yearly_lo,
                                                    hi_p2 = n_offtake_yearly_hi
                                                    ),
  oys_p3 %>% filter(species %in% spp120) %>% select(species,
                                                    u_p3 = n_offtake_yearly,
                                                    lo_p3 = n_offtake_yearly_lo,
                                                    hi_p3 = n_offtake_yearly_hi
                                                    )
  ) %>% 
  mutate(n_change_u = u_p3 - u_p2,
         n_change_lo = lo_p3 - lo_p2,
         n_change_hi = hi_p3 - hi_p2,
         p_change_u = (u_p3 - u_p2)/u_p2*100,
         p_change_lo = (lo_p3 - lo_p2)/lo_p2*100,
         p_change_hi = (hi_p3 - hi_p2)/hi_p2*100
  )  %>% 
  print(n= Inf)

#no other species have any overlap

spp_change %>% 
  mutate(ol23 = lo_p2 - hi_p3,
         ol32 = lo_p3 - hi_p2) %>% 
  filter(ol23 > 0 | ol32 > 0)

#species n change

get_n_change <- function (x) {
  
  x %>% 
    filter(species %in% spp120) %>% 
    dplyr::select(phase,species,
                  u = n_offtake_yearly,
                  lo = n_offtake_yearly_lo,
                  hi = n_offtake_yearly_hi
                  )
}

spp_n_change <- bind_rows(get_n_change(oys_p2),
                          get_n_change(oys_p3)
                          ) %>% 
  lab_phase()

#20 common species ranked by cumulative offtake

spp20 <- spp_n_change %>% 
  group_by(species) %>% 
  summarize(u23 = sum(u)) %>% 
  arrange(desc(u23)) %>% 
  pull(species)

spp_n_change <- spp_n_change %>% 
  mutate(species = factor(species, levels = spp20)) %>% 
  arrange(species)

#20 commonly hunted species with percent changes
#change total offtake to mean per village

spp20_change <- left_join(spp_n_change, 
                          spp_change %>% dplyr::select(species,p_change_u)
                          ) %>% 
  mutate(p_change = paste0(round(p_change_u,0),"%"),
         species = factor(species, levels = spp20),
         u = round(u/nvil,0),
         lo = round(lo/nvil,0),
         hi = round(hi/nvil,0)
         )

min(spp20_change$p_change_u)
max(spp20_change$p_change_u)

# Parameters for base color (#9969A9)
h <- 282   # hue for #9969A9
c <- 50    # chroma/saturation
l_range <- c(15, 90)  # darkest to lightest for your range

spp20_change <- spp20_change %>%
  mutate(
    lightness = rescale(p_change_u, to = l_range),
    color = hcl(h = h, c = c, l = lightness)
  )

#reverse factor order for plotting

spp20_change <- spp20_change %>% 
  mutate(species = fct_rev(species),
         phase_lab = fct_rev(phase_lab)
         )

spp20_change

##add common names for plot

spp20_names <- sm %>% 
  filter(species %in% spp120) %>% 
  dplyr::select(species,english) %>% 
  mutate(species = factor(species, levels = levels(spp20_change$species))) %>% 
  arrange(species)

spp20_change <- left_join(spp20_change, spp20_names)

#common species change plot

min(spp20_change$lo)
max(spp20_change$hi)

spp20_change %>% filter(species %in% c("PCM", "TOR"))

pos_change <- spp20_change %>%
  filter(p_change_u > 0) %>% 
  group_by(species) %>%
  slice_max(hi, n = 1) %>%
  ungroup() %>% 
  dplyr::select(species,hi,p_change_u,p_change)

neg_change <- spp20_change %>%
  filter(p_change_u < 0) %>% 
  group_by(species) %>%
  slice_min(lo, n = 1) %>%
  ungroup() %>% 
  dplyr::select(species,lo,p_change_u,p_change)

label_change <- function (x) {
  x %>%
    mutate(
      p_change = ifelse(
        as.numeric(sub("%", "", p_change)) > 0,
        paste0("+", p_change),
        paste0("–", sub("-", "", p_change))
      )
    )
}

pos_change <- label_change(pos_change)
neg_change <- label_change(neg_change)

pt <- c("PCM", "TOR")

pt_change <- neg_change %>% filter(species %in% pt)
neg_change <- neg_change  %>% filter(species %ni% pt)

spp20_change %>% filter(species %in% pt)

spp20_plot <- ggplot(spp20_change,  aes(x=species, y=hi,color = phase_lab)) +
  annotate("rect",xmin = 7.5, xmax = 8.5,ymin = -Inf, ymax = Inf,
           fill = "#9969A9", alpha = 0.2) +
  annotate("rect", xmin = 14.5, xmax = 15.5,ymin = -Inf, ymax = Inf,
           fill = "#9969A9", alpha = 0.2) +
  theme_classic()+ ##6A4379
  geom_linerange(aes(ymin = lo, ymax = hi), size = 1, position = position_dodge(0.3), key_glyph = "path") +
  geom_text(data = pos_change,aes(x = species,y = hi + 10, label = p_change),
    inherit.aes = FALSE,hjust = 0,size = 4, colour = "#9969A9")+
  geom_text(data = neg_change,aes(x = species,y = lo - 10, label = p_change),
            inherit.aes = FALSE,hjust = 1,size = 4, colour = "#9969A9")+
  geom_text(data = pt_change,aes(x = species,y = lo - 10, label = p_change),
            inherit.aes = FALSE, fontface= "bold", hjust = 1,size = 5, colour = "#6A4379")+
  # annotate(geom = ggtext::GeomRichText, x = "PCM", y = 500, fontface = "bold", size = 5,
  #          label = paste0(
  #            #"This is **bold**. ",
  #            "<span style='color: indianred2'>~70-100</span> to ",
  #            "<span style='color: royalblue2'>~30-40</span>"
  #          ),
  #          label.size = NA, fill = NA, label.color = NA, label.padding = grid::unit(0, "pt")
  #          )+
  # annotate(geom = ggtext::GeomRichText, x = "TOR", y = 500, fontface = "bold", size = 5,
  #          label = paste0(
  #            #"This is **bold**. ",
  #            "<span style='color: indianred2'>~20-40</span> to ",
  #            "<span style='color: royalblue2'>~10</span>"
  #          ),
  #          label.size = NA, fill = NA, label.color = NA
  #          )+
  # annotate("text", x = "PCM", y = 500,
  #          #y = 103+10, hjust = 0,
  #          label = "~70-100 to ~30-40",  size = 5,
  #          colour = "#6A4379", fontface = "bold")+
  annotate("text", x = "PCM", y = 300, hjust= 0,
           #y = 103+10, hjust = 0,
           label = "~70–100",  size = 5,
           colour = "indianred2", fontface = "bold")+
  annotate("text", x = "PCM", y = 500, hjust= 0,
           #y = 103+10, hjust = 0,
           label = "to",  size = 5,
           colour = "#6A4379", fontface = "bold")+
  annotate("text", x = "PCM", y = 560, hjust= 0,
           #y = 103+10, hjust = 0,
           label = "~30–40",  size = 5,
           colour = "royalblue2", fontface = "bold")+
  # annotate("text", x = "TOR", y = 500,
  #          #y = 35+10,  hjust = 0,
  #          label = "~20-40 to ~10", size = 5,
  #          colour = "#6A4379", fontface = "bold")+
  annotate("text", x = "TOR", y = 330, hjust= 0,
           #y = 103+10, hjust = 0,
           label = "~20–40",  size = 5,
           colour = "indianred2", fontface = "bold")+
  annotate("text", x = "TOR", y = 500, hjust= 0,
           #y = 103+10, hjust = 0,
           label = "to",  size = 5,
           colour = "#6A4379", fontface = "bold")+
  annotate("text", x = "TOR", y = 560, hjust= 0,
           #y = 103+10, hjust = 0,
           label = "~10",  size = 5,
           colour = "royalblue2", fontface = "bold")+
  scale_color_manual(values = c(`2019/2020` = "indianred2", `2024/2025` = "royalblue2")) +
  scale_y_continuous(limits =c(-100, 800), breaks = seq(0, 800, 200))+
  scale_x_discrete(labels =  rev(unique(spp20_change$english)))+
  coord_flip()+
  labs(x = "Species\n", y = "\nAverage yearly animals hunted/village",
       colour= "Period") +
  guides(color = guide_legend(reverse = TRUE))+
  theme(#legend.position = "none",
    #legend.title = element_text(size=14),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    axis.line = element_blank(),
    axis.ticks = element_line(colour = "black",),
    axis.text=element_text(size=14, colour = "black"),
    legend.justification = "top",
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 14),
    #axis.title.y= element_text(size = 14),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    plot.margin = unit(c(0.4, 0.2, 0.4, 0.2), "cm")
  )

spp20_plot

#add pics

# Read images (need to rotate pangolin cause metadata isn't read)
pcmPic <- imager::load.image("./inputs/pics/phataginusTricuspis1892.tiff")
pcmPic <- imager::imrotate(pcmPic, angle = 90)  
torPic <- imager::load.image("./inputs/pics/kinixysErosa1901.tiff")

spp20_plotPic <- ggdraw(spp20_plot) + 
  draw_image(pcmPic,
             x = 0.805, y =0.68, width = 0.18, 
             height = 0.25*dim(pcmPic)[2]/dim(pcmPic)[1])+
  draw_image(torPic,
             x = 0.805, y = 0.37, width = 0.18, 
             height = 0.25*dim(torPic)[2]/dim(torPic)[1])

dev.off()

cairo_pdf(file = "./outputs/figs/f3.pdf", width = 8, height =  8)

spp20_plotPic

dev.off()

#are the turtle and pangolin declines seen across villages?

pt_v_p2 <- oysv_p2 %>% filter(species %in% c("PCM", "TOR"))
pt_v_p3 <- oysv_p3 %>% filter(species %in% c("PCM", "TOR"))

pt_change <- left_join(
  pt_v_p2 %>% select(species, 
                    n_p2 = n_offtake_total,
                    u_p2 = n_offtake_yearly,
                    lo_p2 = n_offtake_yearly_lo,
                    hi_p2 = n_offtake_yearly_hi
                    ),
  pt_v_p3%>% select(species, 
                    n_p3 = n_offtake_total,
                    u_p3 = n_offtake_yearly,
                    lo_p3 = n_offtake_yearly_lo,
                    hi_p3 = n_offtake_yearly_hi)
  ) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  mutate(n_change_n = n_p3 - n_p2,
         n_change_u = u_p3 - u_p2,
         n_change_lo = lo_p3 - lo_p2,
         n_change_hi = hi_p3 - hi_p2,
         p_change_n = (n_p3 - n_p2)/n_p2*100,
         p_change_u = (u_p3 - u_p2)/u_p2*100,
         p_change_lo = (lo_p3 - lo_p2)/lo_p2*100,
         p_change_hi = (hi_p3 - hi_p2)/hi_p2*100
  )  

#pangolin declined lots in 7/12 villages (at least 10 less pangolins counted)
#minor declines in 2
#no change in 1
#minor increase in 2

pcm_change <- pt_change %>% 
  filter(species == "PCM") %>% 
  arrange(n_change_n)

pcm_change 

#it seems like % change is correlated with p2 offtake

plot(pcm_change$n_change_n ~ pcm_change$n_p2, pch = 19) + title(cor(pcm_change$n_change_n,pcm_change$n_p2))
plot(pcm_change$p_change_n ~ pcm_change$n_p2, pch = 19) + title(cor(pcm_change$p_change_n,pcm_change$n_p2))

#but this could be simply because villages with few p2 pangolins didn't have enough to to reduce
#and indeed trend completely vanishes when looking at the villages with >20 pangolins in p2
pcm_change20 <-  pcm_change %>% filter(n_p2 > 20)
plot(pcm_change20$p_change_n ~ pcm_change20$n_p2, pch = 19) + title(cor(pcm_change20$p_change_n,pcm_change20$n_p2))

#2 villages had not tortoise hunting either year it seems
#3 villages had major declines (at least 10 less tortoises counted)
#minor declines in 5
#no change in 1
#minor increase in 1

tor_change <- pt_change %>% 
  filter(species == "TOR") %>% 
  arrange(n_change_n)

tor_change

plot(tor_change$n_change_n ~ tor_change$n_p2, pch = 19) + title(cor(tor_change$n_change_n,tor_change$n_p2))
plot(tor_change$p_change_n ~ tor_change$n_p2, pch = 19) + title(cor(tor_change$p_change_n,tor_change$n_p2))

#en gros, both pangolin and tortoise declines are fairly common across village

#quick peek at how scoopable animals are across habitat, how killed, months

bm_p2 <- readRDS("./inputs/bushmeat_transects_phase2.rds")
bm_p3 <- readRDS("./inputs/bushmeat_transects_phase3.rds")

scoop <- bind_rows(bm_p2, bm_p3) %>% 
  filter(especes %in% c("CRN", "PCM", "TOR")) %>% 
  mutate(month = month(date))

#as we know, more pangolins, than tortoise, than crocs
scoop %>% 
  group_by(especes) %>% 
  summarize(n = n())

#cros are actually more shot than macheted
#pangolins are actually shot more than I expected, but still mostly macheted
#tortoise are slightly more scooped than trapped

scoop %>% 
  group_by(especes, `tue via`) %>% 
  summarize(n = n())

#cros are virtually always in swamp or river
#pangolin and tortoise are everywhere

scoop %>% 
  group_by(especes, `tue ou`) %>% 
  summarize(n = n())

#rough trends over months? (think march mushrooms for turtles)

scoop_month <- scoop %>% 
  group_by(especes, phase, month) %>% 
  summarize(n = n()) 

scoop_month %>% print(n = Inf)

pcm_m_p2 <- scoop_month %>% filter(especes == "PCM" & phase == 2)
pcm_m_p3 <- scoop_month %>% filter(especes == "PCM" & phase == 3)
tor_m_p2 <- scoop_month %>% filter(especes == "TOR" & phase == 2)
tor_m_p3 <- scoop_month %>% filter(especes == "TOR" & phase == 3)

#no patterns jump of the page for pangolin
plot(pcm_m_p2 $n ~ pcm_m_p2$month, pch = 19, col = "indianred2", ylim = c(0, 50))
points(pcm_m_p3 $n ~ pcm_m_p3$month, pch = 19, col = "royalblue2")

#for tortoise, march april and may were high in p2 but not p3
#november and october were high for both
plot(tor_m_p2 $n ~ tor_m_p2$month, pch = 19, col = "indianred2", ylim = c(0, 50))
points(tor_m_p3 $n ~ tor_m_p3$month, pch = 19, col = "royalblue2")

#how are common species used?

spp_use <- bind_rows(bm_p2, bm_p3) %>% 
  filter(especes %in% spp20 & `obtenu comment` %ni% c("D", "A")) %>% 
  rename(species = especes) %>% 
  group_by(species, but) %>% 
  summarize(n = n()) %>% 
  drop_na() %>% 
  mutate(p = (n/sum(n))*100)  

spp_m <- spp_use %>% filter(but == "M") %>% arrange(desc(p))
spp_s <- spp_use %>% filter(but == "S") %>% arrange(desc(p))

spp_m %>% print(n = Inf)
spp_s %>% print(n = Inf)

#life history of common species?
#pangolin are middle of the pack for most things, tortoise too when there is data (often missing)

amniote <- read_csv("inputs/meta/Amniote_Database_Aug_2015.csv")

smn <- sm %>%
  mutate(
    name = case_when(
      !is.na(use_name) ~ use_name,
      TRUE ~ scientific
    )
  ) %>% 
  filter(species %in% spp20) %>% 
  dplyr::select(species,name)

amn <- amniote %>% 
  mutate(name = paste0(genus, " ", species),
         kg = adult_body_mass_g/1000
  ) %>% 
  dplyr::select(name,kg,
                male_maturity_d, female_maturity_d,
                gestation_d, 
                litter_or_clutch_size_n, 
                litters_or_clutches_per_y, 
                inter_litter_or_interbirth_interval_y,
                longevity_y, maximum_longevity_y) 


spp20_life <- left_join(smn,amn) %>% 
  mutate(across(where(is.numeric), na_if, -999))

View(spp20_life)


#jed brodie x2 check model of changing offtake by village

v_p23 <- readRDS("./outputs/fits/v_p23.rds")

names(v_p23)

posts_v_p23 <- tibble(model = names(v_p23),
                      village = sub('.*\\_', '', names(v_p23)),
                      u = NA, lo = NA, hi = NA
                      )
  
for (i in seq_along(posts_v_p23$village)) {
  
  bp3_post_tmp <- as_draws_df(voff_list[[i]])$b_phase3
  posts_v_p23$u[[i]] <- mean(bp3_post_tmp)
  posts_v_p23$lo[[i]] <- as.numeric(quantile(bp3_post_tmp, 0.04))
  posts_v_p23$hi[[i]] <- as.numeric(quantile(bp3_post_tmp, 0.96))
}

posts_v_p23 <- posts_v_p23 %>% 
  arrange(desc(u)) 

p23_vl <- posts_v_p23$village

posts_v_p23 <- posts_v_p23 %>% 
  mutate(village = factor(village, levels = p23_vl))

posts_v_p23

v_p23_fig <- ggplot(posts_v_p23,  aes(x=village, y=u)) +
  theme_classic()+
  geom_hline(yintercept=0, linetype="solid", color = "gray") +
  #geom_errorbar(aes(ymin=lo, ymax=hi),color = "#9969A9", width= 0, position=position_dodge(0.2)) +
  geom_linerange(aes(ymin = lo, ymax = hi), color = "#9969A9", size = 1, position = position_dodge(0.3)) +
  geom_point(aes(y = u), fill = "#9969A9", shape = 21, color = "black", size = 4,
             position = position_dodge(0.3), show.legend = FALSE)+
  geom_vline(xintercept=1.5, linetype="solid", color = "black") +
  geom_vline(xintercept=8.5, linetype="solid", color = "black") +
  scale_y_continuous(limits =c(-1.5, 0.5), breaks = seq(-1, 0.5, 0.5),
                     labels = ~sub("-", "\u2212", .x)
                     )+
  # scale_y_continuous(
  #   limits = c(-12, 12),
  #   breaks = seq(-12, 12, 4),
  #   labels = function(x) gsub("-", "\u2013", x)
  # )+
  labs(x = "\nVillage", y = "\u03b2 of period (2024/2025 vs. 2019/2020) on daily recorded animal hunted\n") +
  theme(legend.position = element_blank(),
        #legend.position = "right",
        #legend.justification = c("top"),
        #legend.title = element_blank(),
        #legend.text = element_text(size=14),
        strip.background = element_blank(),
        strip.text = element_text(size=14),
        axis.line = element_blank(),
        axis.ticks = element_line(colour = "black",),
        axis.text=element_text(size=14, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y= element_text(size = 14),
        plot.title = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin = unit(c(0.4, 0.2, 0.4, 0.2), "cm")
  )

v_p23_fig

dev.off()

cairo_pdf(file = "./outputs/figs/fS2.pdf", width = 12, height =  8)

v_p23_fig

dev.off()

