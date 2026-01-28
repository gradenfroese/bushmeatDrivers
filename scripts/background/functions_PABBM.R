###PABBM functions

#1 AB fixes mixed date schema (see in original hunts_gps_aggregate.csv)
date_fix = function(date_vector) {
  new_date = ifelse(((substr(date_vector, start = 1, stop = 2)=="20")&
                       (substr(date_vector, start = 7, stop = 8)!="19")),
                    format(as.Date(date_vector, "%y-%m-%d"),"%d-%m-%y"),
                    ifelse(((substr(date_vector, start = 1, stop = 2)=="19")&
                              (substr(date_vector, start = 7, stop = 8)!="20")),
                           format(as.Date(date_vector, "%y-%m-%d"),"%d-%m-%y"),
                           ifelse((date_vector=="20-02-19"),format(as.Date(date_vector, "%y-%m-%d"),"%d-%m-%y"),
                                  format(as.Date(date_vector, "%d-%m-%y"),"%d-%m-%y"))))
  return(as.vector(new_date))
}


#2 GF make a wordcloud of factors
#scale adapted to local name area, might be funky with other factors

wordcloud_factors <- function(x) {
  library(wordcloud)
  wordcloud(x,
          min.freq=1,
          color=alpha("darkseagreen", seq(0.4,1, 0.05)),
          scale=c(3,0.4))
}


#3 GF  when you want village levels for 9 phase 2 carto villages
#village levels given excluding E4 & E10 (not enough data)

village_carto_levels <- function () {
  
  village_levels <- paste0(c(rep("C",10),rep("E",10)),
                                        rep(seq(1:10),2))
  
  village_levels <- village_levels[village_levels %ni% c("E4","E10")]
}


#4 GF ggplot posterior density from tibble

plot_tibble_chain <- function(data, parameter) {
  library(rlang)
  data %>%  
 ggplot(aes(x= {{parameter}})) +
    geom_density(fill= "#69b3a2", color="black",
                 alpha=0.4)+
  scale_y_continuous(expand = c(0,0))
}

#5 GF plot offtake 

plot_offtake <- function (choose_df,predictor_col,lab_offtake,lab_predictor) {
  
input_df <- choose_df
  
  plot(jitter(input_df[,predictor_col] %>% pull()),
     jitter(input_df$offtake), pch = 19,
     ylim = range(input_df$offtake),
     col = alpha("black", 0.4),
     xlab = lab_predictor,
     ylab = lab_offtake)
points(jitter(input_df[,predictor_col] %>% pull()),
     jitter(input_df$IUCN), pch = 19, 
     col = alpha("tomato3", 0.3))
points(jitter(input_df[,predictor_col] %>% pull()),
     jitter(input_df$protected), pch = 19, 
     col = alpha("palegreen3", 0.2))
legend("topleft", legend=c("Total animals", "IUCN listed",
                            "Legally protected"),
         col=c(alpha("black", 0.9),
               alpha("tomato3", 0.8),
               alpha("palegreen3", 0.7)),
         pch=19,
         cex = 1.1,
         bty = 'n')
}

#6 GF plot biomass

plot_biomass <- function (choose_df, predictor_col, lab_biomass, lab_predictor) {
  
   input_df <- choose_df
   
   plot(jitter(input_df[,predictor_col] %>% pull()),
     jitter(input_df$kg), pch = 19,
     col = alpha("black", 0.4),
     xlab = lab_predictor,
     ylab = lab_biomass)
  
}

#7 GF plot money (FCFA sold for)

plot_money <- function (choose_df, predictor_col, lab_money, lab_predictor) {
  
   input_df <- choose_df
   
   plot(jitter(input_df[,predictor_col] %>% pull()),
     jitter(input_df$money), pch = 19,
     col = alpha("black", 0.4),
     xlab = lab_predictor,
     ylab = lab_money)
  
}

#8 GF factor single plot violin

violin_one <- function (df,x,y) {
  library(rlang)
  ggplot(df, aes(x={{x}}, y={{y}})) + 
  geom_violin(trim = FALSE, size = 1)+
  geom_jitter(position=position_jitter(0.2))
  
}

#9 GF factor plot violin for looping over x and y
violin_loop_xy <- function (df,x,y,title) {
  library(rlang)
  ggplot(df, aes(x=.data[[{{x}}]], y=.data[[{{y}}]])) + 
  geom_violin(trim = FALSE, size = 1)+
  geom_jitter(position=position_jitter(0.2))+
  ylab(y)+
  xlab(x)+
  ggtitle(title)+
  #ggtitle(deparse(substitute(df)))+
  theme(plot.title = element_text(hjust = 0.5))
  
}

#10 GF make list with names function from stack exchange

listN <- function(...){
    anonList <- list(...)
    names(anonList) <- as.character(substitute(list(...)))[-1]
    anonList
}

#11 GF standardize (center and scale) predictor variable

stdCS_pred <- function (predictor) {
  (predictor - mean(predictor, na.rm = T)) / 
    sd(predictor, na.rm = T)
}

#11b GF backtransform

BT_stdCS <- function (value, sdv, meanv) {
  
  value*sdv + meanv
  
}

##12 extract text from within brackets from stack exchange

text_within_brackets <- function(x) {
  sub(".*\\[(.*)\\].*", "\\1", x, perl=TRUE)}

##13 save list components as global objects from stack exchange

unpack_list <- function (your_list) {
  
  lapply(seq_along(your_list),
         function(i) assign(names(your_list)[i],
                            your_list[[i]], envir = .GlobalEnv)) 
  
}

#14 GF proportion of zeros with na.rm = TRUE

prop_zero_na.rm <- function(x) {sum(x == 0, na.rm = TRUE)/length(x)}

##15 GF get brms effects

get_brms_ef <- function (ef_object, ef_type, vgroup) {
  
em <- 'Failed to choose "fixef" or "ranef" in 2nd argument
Ensure as well that 1st argument is ranef() or fixef() of your model fit
& that if using "ranef", your 3rd argument the grouping variable is in quotes (e.g., "village")'
  
  library(dplyr)
  library(magrittr)
  
  if (ef_type != "fixef" & ef_type != "ranef") {
    cat(em)
    x <- NULL
  }
  
  if (ef_type == "fixef") {
    
    tmp_cnames <- rownames(ef_object)
    
    x <- as_tibble(ef_object) %>% 
      dplyr::mutate(group = NA,
                    coef = as_factor(tmp_cnames)) %>% 
      dplyr::select(group,coef, everything())
    
  }
  
  if (ef_type == "ranef") {
    
    x <- ef_object %>% 
      magrittr::extract2(vgroup)
    
    tmp_gnames <- names(x[,1,1])
    tmp_cnames <- names(x[1,1,])
    tmp_names <- tibble(g = rep(tmp_gnames, length(tmp_cnames)),
                        c = rep(tmp_cnames, each = length(tmp_gnames)))
    
    array_list <- list()
    
    for (i in seq_along(tmp_cnames)) {
      
      array_list[[i]] <- as_tibble(x[,,i]) 
      
    }
    
    x <- bind_rows(array_list) %>% 
      mutate(group = as.factor(tmp_names$g),
             coef = as.factor(tmp_names$c)) %>% 
      select(group,coef,everything())
    
  }
  
  colnames(x) <- c("group", "coef",
                   "estimate", "error",
                   "q2.5", "q97.5")
  
  return(x)
  
}

##16 GF summarize brms posteriors

brms_posts <- function (rstring,pstring,bstring,mfit, lowq, hiq) {
  
  postib <- tibble(response = as.factor(rstring), 
                   predictor = factor(pstring, levels = pstring), 
                   brms_variable = factor(bstring, levels = bstring),
                   mean = NA, lui = NA, uui = NA )
  
  for (i in seq_along(bstring)) {
    
    tmp_post <- as_draws_df(mfit) %>% 
      pull(bstring[i]) 
    
    postib$mean[i] <- mean(tmp_post)
    postib$lui[i] <- as.numeric(quantile(tmp_post, lowq))
    postib$uui[i] <- as.numeric(quantile(tmp_post, hiq))
    
    
  }

  return(postib)
  
}

##17 GF recode motivation facotrs

make_motivation_EN <- function (x) {
  
  x %>% 
    dplyr::mutate(why_hunt = recode_factor(why_hunt, 
                                    "manger et vendre" = "both",
                                    vendre = "sell",
                                    manger = "eat",
                                    ceremonie = "ceremony",
                                    #plantation = "plantation", same word
                                    participer = "participate",
                                    vivre = "live",
                                    "manger et participer" = "eat and participate",
                                    #command = "command", same word
                                    "manger et ceremonie" = "eat and ceremony",
                                    "manger et plantation" = "eat and plantation")
    )
}

##18 GF compare proportion of factors in raw to simulated data
raw_sim_facomp <- function (rawdf,simdf, group) {
  
  tr <- rawdf %>%
    group_by({{group}}) %>%
    summarize(phunt_raw = n()/nrow(rawdf)) %>%
    arrange(desc(phunt_raw))
  
  sr <- simdf %>%
    group_by({{group}}) %>%
    summarize(phunt_sim = n()/nrow(simdf)) %>%
    arrange(desc(phunt_sim))
  
  left_join(tr,sr) %>%
    arrange(desc(phunt_sim))
  
}

#19 GF compare histograms of continuous predictors in raw to simulated data
raw_sim_hcomp <- function (df1,df2,pred) {
  
  x1 <- df1 %>% pull({{pred}})
  x2 <- df2 %>% pull({{pred}})
  
  h1 <- hist(x1, main = "raw", xlab = pred, col=rgb(0,0,1,1/4))
  h2 <- hist(x2, main = "sim", xlab = pred, col=rgb(1,0,0,1/4))
  plot(h2, col=rgb(1,0,0,1/4), xlab = pred,
       main = "salmon = sim\nlight purple = raw\ndark purple = overlap") #sim
  plot(h1, col=rgb(0,0,1,1/4), add = T) #raw 
  
}

##20 GF get 50% UIs for contrasting posterior predictions across factors
##this is for when you used posterior_predict() in brms

contrast_ui_pp <- function (m1,m2) {
  
  tibble(
    mean = mean(colMeans(m1 - m2)),
    mui25 = mean(apply((m1 - m2), 2 , quantile , probs = 0.25, na.rm = TRUE )),
    mui75 = mean(apply((m1 - m2), 2 , quantile , probs = 0.75, na.rm = TRUE )),
    uim25 = quantile(colMeans(m1- m2), 0.25),
    uim75 = quantile(colMeans(m1 - m2), 0.75)
  )
  
}

##21 GF get 95% UIs for posterior predictions using fitted() in brms

contrast_ui <- function (m1,m2) {
  
  full_diff <- as_tibble(m1 - m2)
  
  summ_diff <- full_diff %>% 
    summarize(mean = mean(Estimate),
           lmui = mean(Q2.5), 
           umui = mean(Q97.5)
           ) %>% 
    mutate(luim = quantile(full_diff$Estimate, prob = 0.025),
           uuim = quantile(full_diff$Estimate, prob = 0.975))
  
  return(summ_diff )
}

##22 GF tidybayes::mean_qui contrasts 

tidybayes_contrasts <- function(m1, m2, choose_width) {
  
  tidybayes::mean_qi(m1 - m2, .width = choose_width) %>% 
    rename(mean = y, lui = ymin, uui = ymax, width = .width) %>% 
    select(-c(.point,.interval))
  
}

##23 GF get 92 or 89% intervals summary of data for brms posterior predictions using fitted

exp_summary_fitted92 <- function(model, data, response) {
  
  as_tibble(fitted(model, resp = response,
                   newdata = data, probs = c(.04, .96))) %>% 
    rename(mean = Estimate, error = Est.Error, lui = Q4,uui = Q96) %>% 
    bind_cols(data) %>% 
    mutate(expmean = exp(mean)) %>% 
    select(mean,expmean,everything())
  
}

exp_summary_fitted89 <- function(data, response) {
  
  as_tibble(fitted(gsem, resp = response,
                   newdata = data, probs = c(.045, .945))) %>% 
    rename(mean = Estimate, error = Est.Error, lui = Q4.5, uui = Q94.5) %>% 
    bind_cols(data) %>% 
    mutate(expmean = exp(mean)) %>% 
    select(mean,expmean,everything())
  
}

##24 GF summarize raw data 95%

summ_raw <- function(df,resp) {
  
  mtmp <- mean(df %>% pull(!!sym(resp)), na.rm = T)
  
  df %>%
    group_by(village) %>% 
    summarize(model = "raw data",
              n_hunts = n(),
              mean = mean(!!sym(resp), na.rm = T),
              sd = sd(!!sym(resp), na.rm = T),
              se = mean/sqrt(n_hunts),
              ci95 = se*1.96,
              q2.5 = mean - ci95,
              q97.5 = mean + ci95,
              gmean = mtmp, 
              diff = mean - mtmp
    )
}

##24 GF summarize raw data 92%
#z-score for 92% CIs 1.75
#resp in "", group not

summ_raw_flex <- function(df,resp, g, z) {
  
  mtmp <- mean(df %>% pull(!!sym(resp)), na.rm = T)
  
  df %>%
    group_by({{g}}) %>% 
    summarize(model = "raw data",
              resp = resp,
              n_hunts = n(),
              mean = mean(!!sym(resp), na.rm = T),
              sd = sd(!!sym(resp), na.rm = T),
              se = mean/sqrt(n_hunts),
              ci = se*z,
              lci = mean - ci,
              uci = mean + ci,
              gmean = mtmp, 
              diff = mean - mtmp
    )
}

##25 GF get long posterior from brms giving that posterior_samples() is deprecated
#model fit, variable in brms, variable name, nchains

posterior_long <- function(mf,nch, vb,vn) {
  as_tibble(mf[,,vb]) %>% 
    pivot_longer(1:nch, names_to = vn)
}

##26 GF function for abs and percent change (with log link regressions)
##mf model fit,  nch n chains, cui choose your ui, ex 92 (not .92)
##vn1 and vn2 variable names in brms to contrast

change_link <- function(mf,nch,cui,vn1,vn2) {
  
  library(posterior)
  
  #posteriors in array
  ada <- posterior::as_draws_array(mf)
  
  #uis
  lui <- (100-cui)/200
  uui <- 1-(100-cui)/200
  
  #changes long
  chl <- tibble(
    pre = as_tibble(ada[,,vn1]) %>% 
      pivot_longer(1:nch) %>% 
      pull(value) %>% 
      exp(),
    post = as_tibble(ada[,,vn2]) %>% 
      pivot_longer(1:nch) %>% 
      pull(value) %>% 
      exp(),
    abs_diff = post - pre,
    per_diff = (post - pre)/pre*100 #%>% round(0)
  )
  
  chw <- tibble(form = as.character(mf$formula)[1],
                dist = as.character(mf$family)[1],
                #link = as.character(mf$family)[2], doesn't shwo multiple
                ui = cui,
                mean_abs = mean(chl$abs_diff),
                lui_abs = quantile(chl$abs_diff,lui),
                uui_abs = quantile(chl$abs_diff,uui),
                width_abs = abs(uui_abs -lui_abs),
                mean_per = mean(chl$per_diff),
                lui_per = quantile(chl$per_diff,lui),
                uui_per = quantile(chl$per_diff,uui),
                width_per = abs(uui_per -lui_per),
  ) %>% 
    mutate(across(contains("abs"), ~ round(.x,1)),
           across(contains("per"), ~ round(.x,0))
           )
  
  return(chw)
}

##27 GF function for abs and percent change (with logit link zi regressions)
##mf model fit,  nch n chains, cui choose your ui, ex 92 (not .92)
##vn1 and vn2 variable names in brms to contrast

change_wzi <- function(mf,nch,cui,vn1,vn2,vzi1,vzi2) {
  
  library(posterior)
  
  ada <- posterior::as_draws_array(mf)
  
  #uis
  lui <- (100-cui)/200
  uui <- 1-(100-cui)/200
  
  #changes long
  chl <- tibble(
    pre_cnt = as_tibble(ada[,,vn1]) %>% 
      pivot_longer(1:nch) %>% 
      pull(value) %>% 
      exp(),
    pre_zi_nomi = as_tibble(ada[,,vzi1]) %>% 
      pivot_longer(1:nch) %>% 
      pull(value) %>% 
      exp(),
    pre_zi = pre_zi_nomi/(1 + pre_zi_nomi),
    pre = pre_cnt*(1-pre_zi),
    post_cnt = as_tibble(ada[,,vn2]) %>% 
      pivot_longer(1:nch) %>% 
      pull(value) %>% 
      exp(),
    post_zi_nomi = as_tibble(ada[,,vzi2]) %>% 
      pivot_longer(1:nch) %>% 
      pull(value) %>% 
      exp(),
    post_zi = post_zi_nomi/(1 + post_zi_nomi),
    post = post_cnt*(1-post_zi),
    abs_diff = post - pre,
    per_diff = (post - pre)/pre*100 #%>% round(0)
  )
  
  chw <- tibble(form = as.character(mf$formula)[1],
                dist = as.character(mf$family)[1],
                #link = as.character(mf$family)[2], #doesnt show multiple
                ui = cui,
                mean_abs = mean(chl$abs_diff),
                lui_abs = quantile(chl$abs_diff,lui),
                uui_abs = quantile(chl$abs_diff,uui),
                width_abs = abs(uui_abs -lui_abs),
                mean_per = mean(chl$per_diff),
                lui_per = quantile(chl$per_diff,lui),
                uui_per = quantile(chl$per_diff,uui),
                width_per = abs(uui_per -lui_per)
  ) %>% 
    mutate(across(contains("abs"), ~ round(.x,1)),
           across(contains("per"), ~ round(.x,0))
    )
  
  return(chw)
}

##28 check bayes plot graphical checks

bp_quick <- function (x) {
  
  tmp_fit <- x
  
  postarray_tmp_fit <- as.array(tmp_fit)
  lp_tmp_fit <- log_posterior(tmp_fit) 
  np_tmp_fit <- nuts_params(tmp_fit)
  
  #diagnostics 
  p1 <- mcmc_trace(postarray_tmp_fit)
  p2 <- mcmc_parcoord(postarray_tmp_fit, np = np_tmp_fit)
  p3 <- mcmc_pairs(postarray_tmp_fit, np = np_tmp_fit)
  p4 <- mcmc_nuts_divergence(np_tmp_fit, lp_tmp_fit)
  p5 <- mcmc_nuts_energy(np_tmp_fit)
  p6 <- mcmc_rhat(rhat(tmp_fit)) + yaxis_text(hjust = 1)
  p7 <- mcmc_neff(neff_ratio(tmp_fit)) + yaxis_text(hjust = 1)
  
  #ppc
  p8 <- pp_check(tmp_fit, ndraws = 100)
  prop_zero <- function(x) mean(x == 0)
  
  p9 <- ppc_stat(y = tmp_fit$data[1] %>% pull(1),
                 posterior_epred(tmp_fit)[1:1000,],
                 stat = "mean")
  
  p10 <- ppc_stat(y = tmp_fit$data[1] %>% pull(1),
                  posterior_epred(tmp_fit)[1:1000,],
                  stat = "prop_zero")
  
  check_plots <- list(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
  return(check_plots)
  
}



##ALTERNATIVE APPROACH TO function to get abs and percent change
##believe this is WRONG !!
##DELETE LATER

# variables(offp_ps_E6)
# 
# offp_ps_E6_pred <- brms_posts("Daily hunted animals",
#                               c("Pre-management", "Post-management"),
#                               c("b_periodpre", "b_periodpost"),
#                               offp_ps_E6, 0.04, 0.96) %>%  
#   mutate(across(mean:uui, ~ exp(.x))) %>% 
#   mutate(model = factor("offp_ps_E6")) %>% 
#   dplyr::select(model,everything())
# 
# ui_change_factors <- function (x,k) {
#   
#   xem <- 'Ensure  1st argument is brms model fit without quotes and that 2nd argument is long or wide, one word WITH quotes'
#   
#   xpost <- x %>% filter(predictor == "Post-management")
#   xpre <- x %>% filter(predictor == "Pre-management")
#   
#   post_ui <- xpost %>% dplyr::select(mean,lui,uui)
#   pre_ui <- xpre %>% dplyr::select(mean,lui,uui)
#   
#   xchange_long <- bind_rows(round(post_ui - pre_ui,2),
#                             round((post_ui - pre_ui) / pre_ui*100,0)
#   ) %>% 
#     mutate(model = x %>% pull(model),
#            response = x %>% pull(response),
#            type_change = factor(c("Absolute", "Percent"))
#     ) %>% 
#     dplyr::select(model,response,type_change, everything())
#   
#   xchange_wide <-pivot_wider(xchange_long,
#                              names_from = type_change,
#                              values_from = c(mean,lui,uui)) %>%
#     dplyr::select(model,response,
#                   mean_abs = mean_Absolute,
#                   lui_abs = lui_Absolute,
#                   uui_abs = uui_Absolute,
#                   mean_per = mean_Percent,
#                   lui_per = lui_Percent,
#                   uui_per = uui_Percent
#     )
#   
#   if (k == quote(long)) {return(xchange_long)}
#   else if (k == quote(wide)) {return(xchange_wide)}
#   else{print(xem)}
#   
# }


###BELOW ARE FUNCTIONS WRITTEN BY AB FOR RUNNING THE PABBM ITSELF

### load posterior data
load_posteriors = function() {
  ### loads brms posterior data
  
  gmv <<- readRDS("./outputs/model_fits/mvgmv.rds")
  tmv <<- readRDS("./outputs/model_fits/mvtmv.rds")
}


### loading empirical data
load_emp_data = function() {
  ### loads actual hunt data
  
  gun_data <<-
    readRDS("./inputs/cartography/derived/bayes_hunts_inputs_gun.rds")
  trap_data <<-
    readRDS("./inputs/cartography/derived/bayes_hunts_inputs_trap.rds")
  village_info <<-
    readRDS("./inputs/cartography/derived/village_info.rds")
  
  
}

### extract gun info
extract_gun_posterior = function() {
  ### extracts the posteriors
  ### for gun
  
  # vars
  b_double_zero <<- as_tibble(posterior_samples(gmv)) %>% pull(b_offtake_d_double_zero_brought)
  b_chevrotine <<- as_tibble(posterior_samples(gmv)) %>% pull(b_offtake_d_chevrotine_brought)
  b_max_distance_gun <<- as_tibble(posterior_samples(gmv)) %>% pull(b_offtake_d_max_km_village)
  
  # intercept
  gun_intercepts <<- as_tibble(posterior_samples(gmv)) %>% select(contains("offtake"),
                                                                  -contains(c("hunter", "cor", "b_", "sd")))
  
  
  
}


### extract trap info
extract_trap_posterior = function() {
  ### same a extract gun
  
  b_traps_days <<- as_tibble(posterior_samples(tmv)) %>% pull(b_offtake_d_traps_days)
  b_traps_checked <<- as_tibble(posterior_samples(tmv)) %>% pull(b_offtake_d_traps_checked)
  b_max_distance_trap <<- as_tibble(posterior_samples(tmv)) %>% pull(b_offtake_d_max_km_village)
  
  trap_intercepts <<- as_tibble(posterior_samples(tmv)) %>% select(contains("offtake"),
                                                                   -contains(c("hunter", "cor", "b_", "sd")))
  
  
}

### ABM run
will_hunt = function(agent, s) {
  ### determines if an agent will
  ### hunt at this time step
  
  # probability the person hunts
  agent$does_hunt = rbinom(n = 1, size = 1, p = as.numeric(agent$p_hunt))
  agent$step = s
  return(agent)
}

allowed_hunt = function(agent, h_tracker, m_hunt) { #m_hunt = max # hunts per week
  ### checks the tracking table to 
  ### see if a hunter is allowed to 
  ### hunt
  
  # get info
  id = as.numeric(agent$id)
  
  #boolean statement on whether first row is 0 in h_tracker
  #i.e., T/F if h_tracker already exists
  bool_1 = dim(h_tracker)[1] == 0
  
  # if trackers are both empty, or agent not in trackers
  if((bool_1 == T) | (id %in% h_tracker$id == F)) {
    return(agent)
  }
  
  # row access of specific hunter in dataframe
  row_num_h = which(h_tracker$id == id)
  
  # see how many times the've hunted that week
  h_count_this_week = h_tracker[row_num_h, "count_this_week"] #count_this_week
  
  # if(m_hunt == 0) {
  #   agent$does_hunt = 0 
  # } 
  # 
  if((h_count_this_week >= m_hunt) | (m_hunt == 0)) { #0 DOES NOT WORK
    agent$does_hunt = 0 
  } 
  
  return(agent)
}

update_off_days = function(agent, h_tracker, days) {
  ### need to update the days
  ### for hunters who did not
  ### hunt, so they can be reset
  
  # get hunter id
  id = as.numeric(agent$id)
  row_num = which(h_tracker$id == id)
  
  # if h_tracker is empty, or agent not in tracker
  if (dim(h_tracker)[1] == 0  | id %in% h_tracker$id == F) {
    return(h_tracker)
  }
  
  # still update the day and week
  else{
    h_tracker[row_num, "curr_day"]  = days
    h_tracker[row_num, "week"]  = floor((days-1)/7) + 1 #GF -1 to days
    
    # reset the count_this_week
    if (mod(days, 7) == 0) {
      h_tracker[row_num, "count_this_week"] = 0
      h_tracker[row_num, "allowed_hunt"] = 1
    }
  }
  return(h_tracker)
}

z_score = function(x, vec) {
  ### calculates z-score
  
  return((x - mean(vec, na.rm = T)) / sd(vec, na.rm = T))
}

does_gun = function(agent, gun_dataset) {
  ### calculates offtake of a
  ### gun hunter
  
  # subset intercept and data by village
  v_id = as.character(agent[1, "village"])
  
  
  g_data = subset(gun_dataset, village == as.character(v_id))
  g_inter = pull(gun_intercepts[, v_id])
  
  
  if (agent$hunt_method == "gun" || agent$curr_method == "gun") {
    # sample max_km_gun
    gun_dist = g_data[g_data$guntrap == "gun", ]
    
    agent$max_km = sample(na.omit(gun_dist$max_km_village), 1)
    agent$max_km_std = z_score(agent$max_km, gun_dist$max_km_village)
  }
  
  # sample total_double_zero
  agent$double_zero_brought = sample(na.omit(g_data$double_zero_brought), 1)
  agent$double_zero_brought_std = z_score(agent$double_zero_brought, g_data$double_zero_brought)
  
  # sample total_chevrotine
  agent$chevrotine_brought = sample(na.omit(g_data$chevrotine_brought), 1)
  agent$chevrotine_brought_std = z_score(agent$chevrotine_brought, g_data$chevrotine_brought)
  
  # fixing NaN
  agent[is.na(agent)] = 0
  
  # calculate offtake
  agent$offtake_gun = exp(
    sample(na.omit(g_inter), 1) +
      sample(b_max_distance_gun, 1) * agent$max_km_std +
      sample(b_double_zero, 1) * agent$double_zero_brought_std +
      sample(b_chevrotine, 1) * agent$chevrotine_brought_std
  )
  
  
  if (agent$curr_method == "both") {
    return(agent)
  }
  
  # add current method
  agent$curr_method = "gun"
  agent$total_offtake = agent$offtake_gun
  
  return(agent)
}

does_trap = function(agent, trap_dataset) {
  ### calculates offtake of a
  ### trap hunter
  
  # subset intercept and data by village
  v_id = as.character(agent[1, "village"])
  
  t_data = subset(trap_data, village == as.character(v_id))
  t_inter = pull(trap_intercepts[, v_id])
  
  
  if (agent$hunt_method == "trap" || agent$curr_method == "trap") {
    # sample max_km_trap
    trap_dist = t_data[t_data$guntrap == "trap", ]
    
    agent$max_km = sample(na.omit(trap_dist$max_km_village), 1)
    agent$max_km_std = z_score(agent$max_km, trap_dist$max_km_village)
    #agent$max_km_traps = sample(na.omit(trap_dist$max_km_village), 1)
    #agent$max_km_traps_std = z_score(agent$max_km_traps, trap_dist$max_km_village)
  }
  
  # sample traps_checked
  agent$traps_checked = sample(na.omit(t_data$traps_checked), 1)
  agent$traps_checked_std = z_score(agent$traps_checked, t_data$traps_checked)
  
  
  # sample trap_days
  agent$traps_days = sample(na.omit(t_data$traps_days), 1)
  agent$traps_days_std = z_score(agent$traps_days, t_data$traps_days)
  
  # fixing NaN
  agent[is.na(agent)] = 0
  
  # calculate offtake
  agent$offtake_traps = exp(
    sample(na.omit(t_inter), 1) +
      sample(b_traps_checked, 1) * agent$traps_checked_std +
      sample(b_max_distance_trap, 1) * agent$max_km_std +
      sample(b_traps_days, 1) * agent$traps_days_std
  )
  
  if (agent$curr_method == "both") {
    return(agent)
  }
  
  # add current method
  agent$curr_method = "trap"
  agent$total_offtake = agent$offtake_traps
  
  return(agent)
}

both_decision = function(g, t, b) {
  ### samples probabilities of
  ### hunting methods for the
  ### both hunter
  
  s = sample(c(rep("gun", g), rep("trap", t), rep("both", b)), 1)
  return(s)
}

does_both = function(agent, gun_dataset, trap_dataset) {
  ### calculates offtake of a
  ### hunter who does both, or
  ### sends them to only gun /
  ### trap hunt
  
  # get prob values of hunt method
  p_g = round(as.double(agent$p_gun) * 100, 0)
  p_t = round(as.double(agent$p_trap) * 100, 0)
  p_b = round(as.double(agent$p_both) * 100, 0)
  
  # returns a decision for hunt method
  method = both_decision(p_g, p_t, p_b)
  
  if (method == "gun") {
    agent$curr_method = "gun"
    agent = does_gun(agent, gun_dataset)
    return(agent)
  }
  
  if (method == "trap") {
    agent$curr_method = "trap"
    agent = does_trap(agent, trap_dataset)
    return(agent)
  }
  
  if (method == "both") {
    # add current method
    agent$curr_method = "both"
    
    v_id = as.character(agent[1, "village"])
    b_data = rbind(gun_data, trap_data)
    b_data = subset(b_data, village == as.character(v_id))
    
    # calculate distance
    both_dist = b_data[b_data$guntrap == "both",]
    agent$max_km = sample(na.omit(both_dist$max_km_village), 1)
    agent$max_km_std = z_score(agent$max_km, both_dist$max_km_village)
    
    # add current method
    agent$curr_method = "both"
    
    # sends agent to both gun and trap
    agent = does_trap(does_gun(agent, gun_dataset), trap_dataset)
    
    # total offtake
    offtake_subset = agent %>% select(contains("offtake"))
    new_offtake = as.numeric(rowSums(offtake_subset))
    agent$total_offtake = new_offtake
    
    return(agent)
  }
}

fix_inter_names = function(data) {
  ### cleans intercept names
  ### from brms output
  
  # regex is the worst
  p = "[A-Za-z]+[:digit:]+"
  
  # cleans names
  for (i in seq(1, length(data), 1)) {
    c_name = colnames(gun_intercepts)[i]
    new_name = str_extract(c_name , p)
    colnames(data)[i] = new_name
  }
  
  return(data)
}

null_to_zero = function(df_hunters) {
  ### makes null frames into 0 x 0
  ### df
  
  if(is.null(df_hunters) == T) {
    z = data.frame()
    return(z)
  }
  
  return(df_hunters)
}

combine = function(g_hunters, t_hunters, b_hunters) {
  ### combines multiple hunter
  ### data frames into single
  ### output
  
  # trap and gun missing
  if (nrow(t_hunters) == 0 & nrow(g_hunters) == 0) {
    print("g and t are missing")
    return(b_hunters)
  }
  
  # both and trap missing
  if (nrow(b_hunters) == 0 & nrow(t_hunters) == 0) {
    print("b and t are missing")
    return(g_hunters)
  }
  
  # both missing
  if (nrow(b_hunters) == 0) {
    print("b is missing")
    a_hunters = full_join(g_hunters, t_hunters)
    return(a_hunters)
  }
  
  # trap missing
  if (nrow(t_hunters) == 0) {
    print("t is missing")
    a_hunters = full_join(g_hunters, b_hunters)
    return(a_hunters)
  }
  
  # gun missing
  if (nrow(g_hunters) == 0) {
    print("g is missing")
    a_hunters = full_join(t_hunters, b_hunters)
    return(a_hunters)
  }
  
  # combine all full frames
  a_hunters = full_join(g_hunters, t_hunters)
  a_hunters = rbind(a_hunters, b_hunters)
  
  return(a_hunters)
  
}

collect_hunt = function(agent, tracker, days) { #this used to have max_val which I cut as wasn't in function
  ### stores the hunt instance 
  ### of each hunter and
  ### updates after every
  ### hunt
  
  # get hunter id
  id = as.numeric(agent$id)
  row_num = which(tracker$id == id)
  
  # if tracker is empty or a new hunter has appeared
  if(dim(tracker)[1] == 0 | id %in% tracker$id == F) {
    new_instance = data.frame('id' = id, 'total_count' = 1, 'count_this_week' = 1, 
                              'curr_day' = days, 'week' = floor((days-1)/7) + 1, 'allowed_hunt' = 1)
    tracker = rbind(tracker, new_instance)
  }
  
  # a hunter id already exists
  else {
    row_num = which(tracker$id == id)
    tracker[row_num, "total_count"]  = tracker[row_num, "total_count"] + 1
    tracker[row_num, "count_this_week"]  = tracker[row_num, "count_this_week"] + 1
    tracker[row_num, "curr_day"]  = days
    tracker[row_num, "week"]  = floor((days-1)/7) + 1 #GF changed -1 to days
    the_week = tracker[row_num, "week"]
  }
  return(tracker)
}

max_offtake_per_hunt = function(agent, max_val) {
  ### sets the offtake
  ### of the hunt to the
  ### max no. per hunt
  
  if(as.numeric(agent$total_offtake) > max_val) {
    agent$total_offtake = max_val
    print("maxed")
  }
  
  return(agent)
}

##IS_MAX_HUNT() NO LONGER USED - FOR WEEKLY OFFTAKE
#THE ABOVE IS WRONG

# is_max_hunt = function(agent, tracker, max_val) {
#   ### each hunter is checked
#   ### to see if they have not
#   ### maxed out their hunts
#   ### per week
# 
#   # if agent doesnt want to hunt, exit now
#   if(agent$does_hunt == 0) {
#     return(agent)
#   }
# 
#   id = as.numeric(agent$id)
# 
#   if(id %in% tracker$id == T) {
#     row_num = which(tracker$id == id)
#     the_week = tracker[row_num, "week"]
# 
#     if((tracker[row_num, "count_this_week"]) >= max_val) {
#       agent$does_hunt = 0
#       print("ive maxed hunted for the week")
#     }
#   }
#   return(agent)
# }

##COLLECT_OFFTAKE() NO LONGER USED - FOR WEEKLY OFFTAKE 


# collect_offtake = function(agent, tracker, max_val) {
#   ### stores the offtake 
#   ### of each hunter and
#   ### updates after every
#   ### hunt
#   
#   id = as.numeric(agent$id)
#   offtake_subset = agent %>% select(contains("offtake"))
#   new_offtake = as.numeric(rowSums(offtake_subset))
#   
#   # if tracker is empty or a new hunter has appeared
#   if(dim(tracker)[1] == 0 | id %in% tracker$id == F) {
#     new_instance = data.frame('id' = id, 'total_offtake' = new_offtake, 'allowed_hunt' = 1)
#     tracker = rbind(tracker, new_instance)
#   }
#   
#   # a hunter id already exists
#   else {
#     row_num = which(tracker$id == id)
#     tracker[row_num, "total_offtake"]  = tracker[row_num, "total_offtake"] + new_offtake
#     
#     # if hunter goes over limit, then cant hunt
#     if(tracker[row_num, "total_offtake"] >= max_val) {
#       tracker[row_num, "allowed_hunt"] = 0
#     }
#      
#   }
#   return(tracker)
# }


##IS_MAX_OFFTAKE() NO LONGER USED - FOR WEEKLY OFFTAKE 

# is_max_offtake = function(agent, tracker, max_off) {
#   ### each hunter is checked
#   ### to see if they have hit 
#   ### the max number of offtake
#   ### for a given week
#   
#   # if agent doesnt want to hunt, exit now
#   if(agent$does_hunt == 0) {
#     return(agent)
#   }
#   
#   id = as.numeric(agent$id)
#   
#   # checks to see if hunter has maxed offtake for the week
#   if(id %in% tracker$id == T) {
#     row_num = which(tracker$id == id)
#     
#     if((tracker[row_num, "total_offtake"]) >= max_off) {
#       agent$does_hunt = 0
#       print("ive maxed offtake")
#     }
#   }
#   return(agent)
# }


