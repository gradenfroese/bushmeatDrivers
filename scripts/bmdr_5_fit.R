#fit

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

#quick check on distributions of kg

hist(gun_kg$kg)
quantile(gun_kg$kg)

hist(trap_kg$kg)
quantile(trap_kg$kg)

#priors (for both gamma-poisson and gamma)

gp_priors <- c(
  #slopes
  prior(normal(0, 0.5), class = "b"),
  
  #intercept
  prior(normal(0, 0.5), class = "Intercept"),
  
  #village sd
  prior(exponential(2), class = "sd", group = "village"),
  
  #shape for disperion
  prior(exponential(2), class = "shape")
)

#hunting amount drivers
n_p2 <- brm(n_offtake ~ km_mkk + n_maison_p2 + (1 | village),
              family = negbinomial(),
              prior = gp_priors,
              data = daily_p2,
              chains = 4, iter = 2000, warmup = 1000,
              control = list(adapt_delta = 0.92),
              cores = getOption("mc.cores", 2),
              file = "./outputs/fits/n_p2"
              )

n_p3 <- brm(n_offtake ~ km_mkk + n_maison_p3 + (1 | village),
               family = negbinomial(),
               prior = gp_priors,
               data = daily_p3,
               chains = 4, iter = 2000, warmup = 1000,
               control = list(adapt_delta = 0.92),
               cores = getOption("mc.cores", 2),
               file = "./outputs/fits/n_p3"
               )

#sustainability (mbmi for shot and trapped animals)

gkg_p2 <- brm(kg ~ km_mkk + n_maison_p2 + (1 | village),
               family = Gamma(link = "log"),
               prior = gp_priors,
               data = gun_kg_p2,
               chains = 4, iter = 2000, warmup = 1000,
               control = list(adapt_delta = 0.92),
               cores = getOption("mc.cores", 2),
               file = "./outputs/fits/gkg_p2"
               )

gkg_p3 <- brm(kg ~ km_mkk + n_maison_p3 + (1 | village),
                family = Gamma(link = "log"),
                prior = gp_priors,
                data = gun_kg_p3,
                chains = 4, iter = 2000, warmup = 1000,
                control = list(adapt_delta = 0.92),
                cores = getOption("mc.cores", 2),
                file = "./outputs/fits/gkg_p3"
                )

tkg_p2 <- brm(kg ~ km_mkk + n_maison_p2 + (1 | village),
                family = Gamma(link = "log"),
                prior = gp_priors,
                data = trap_kg_p2,
                chains = 4, iter = 2000, warmup = 1000,
                control = list(adapt_delta = 0.92),
                cores = getOption("mc.cores", 2),
                file = "./outputs/fits/tkg_p2"
                )

tkg_p3 <- brm(kg ~ km_mkk + n_maison_p3 + (1 | village),
                family = Gamma(link = "log"),
                prior = gp_priors,
                data = trap_kg_p3,
                chains = 4, iter = 2000, warmup = 1000,
                control = list(adapt_delta = 0.92),
                cores = getOption("mc.cores", 2),
                file = "./outputs/fits/tkg_p3"
                )

#read them back in 

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


#parameter estimates is identical
#and rhats and ess are good

for (i in seq_along(all_fits)) {
  
  cat("\n\n===== Model:", names(all_fits)[i], "=====\n")
  
  s <- summary(all_fits[[i]])
  print(s)
}

#Visual MCMC diagnostics with bayesplot
#http://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html

#functions for NUTS 
available_mcmc(pattern = "_nuts_")

#extract posteriors and some other stuff
#keep em alone and in lists

post_list <- list()
pp_list <- list()
lp_list <- list()
npa_list <- list()

for(i in seq_along(all_fits))  {
  
  #loop through each fit
  tmp_fit <- all_fits[[i]]
  tmp_name <- names(all_fits)[[i]]
  
  #posteriors
  post_list[[i]] <- as.array(tmp_fit$fit)
  names(post_list)[i] <- tmp_name
  
  #posterior predictions
  pp_list[[i]] <- brms::posterior_predict(tmp_fit)
  names(pp_list)[i] <- tmp_name
  
  #log posteriors
  lp_list[[i]] <- log_posterior(tmp_fit)
  names(lp_list)[i] <- tmp_name
  
  #NUTS parameters
  npa_list[[i]] <- nuts_params(tmp_fit)
  names(npa_list)[i] <- tmp_name
  
}

##trace plots (mixing seems fine)
color_scheme_set("mix-brightblue-gray")


for (i in seq_along(all_fits)) {
  
  p <- mcmc_trace(post_list[[i]], np = npa_list[[i]]) + 
    xlab("Post-warmup iteration")+
    ggtitle(names(all_fits)[[i]])
  
  print(p)
  
}


#divergent transitions 

for (i in seq_along(all_fits)) {
  
  p <- mcmc_parcoord(post_list[[i]], pars = vars(-"lp__" ),np = npa_list[[i]])+
    xlab("Post-warmup iteration")+
    ggtitle(names(all_fits)[[i]])+
      theme(axis.text.x = element_blank())+
      xlab("Names of parameters removed for plot readability")
  
  print(p)
  
}

#posterior predictive checks
color_scheme_set("brightblue")

for (i in seq_along(all_fits)) {
  
  p <- pp_check(all_fits[[1]], ndraws = 100)+
    ggtitle(names(all_fits)[[i]])+
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p)
  
}

#prop zero
#exploratory modeling showed that poisson underestimates 0s by almost 20%
#gamma-poisson gets them nicely
prop_zero <- function(x) {sum(x == 0)/length(x)}

prop_zero(daily_p2$n_offtake)

for (i in seq_along(all_fits)) {
  
  p <- pp_check(all_fits[[i]],  type = "stat", stat = prop_zero) +
    ggtitle(names(all_fits)[[i]])+
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p)
}

#dispersion
#exploratory modeling showed that poission vastly underestimates, while gamma-poisson only slight overestimates
#gamma models vastly overestimate though

dispersion <- function(x) {var(x)/mean(x)}

dispersion (daily_p2$n_offtake)

for (i in seq_along(all_fits)) {
  
  p <- pp_check(all_fits[[i]],  type = "stat", stat = dispersion) +
    ggtitle(names(all_fits)[[i]])+
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p)
}


#jed brodie x2 check model of changing offtake by village

#prior with no varying intercept
gp_priors_novi <- gp_priors %>% filter(class != "sd")

vl <- levels(daily$village)

voff_list <- list()

for (i in seq_along(vl)) {
  
  daily_tmp <- daily %>% filter(village == vl[[i]])
  
  voff_list[[i]] <- brm(n_offtake ~ phase,
                        family = negbinomial(),
                        prior = gp_priors_novi,
                        data = daily_tmp,
                        chains = 4, iter = 2000, warmup = 1000,
                        control = list(adapt_delta = 0.92),
                        cores = getOption("mc.cores", 2)
                        )
  }


names(voff_list) <- paste0("n_p23_", vl)

saveRDS(voff_list, "./outputs/fits/v_p23.rds")

bp3_post <- as_draws_df(voff_list[[4]])$b_phase3
u <- mean(bp3_post)
lo <- as.numeric(quantile(bp3_post, 0.04))
hi <- as.numeric(quantile(bp3_post, 0.96))


