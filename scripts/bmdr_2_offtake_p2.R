#take paraecologist transects and calculate offtake
#here applied to phase 2 data

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

#read data
#(first, remove villages you don't have (robust) data for both 2019-2020 and 2024-2025)
village_meta <- readRDS("./inputs/meta/village_meta.rds") 

unique(village_meta$p2_data)
unique(village_meta$p3_data)

cut_villages <- village_meta %>% 
  filter(!is.na(p2_data) | !is.na(p3_data)) %>% 
  pull(village)

village_meta <- village_meta %>% 
  filter(village %ni% cut_villages) 

bushmeat <-  readRDS("./inputs/bushmeat_transects_phase2.rds") %>% 
  filter(village %ni% cut_villages) %>% 
  mutate_if(is.character,funs(factor(.)))  

effort <-  readRDS("./inputs/effort_transects_phase2.rds") %>% 
  filter(village %ni% cut_villages) %>% 
  mutate_if(is.character,funs(factor(.)))  

kg <- readRDS("./inputs/meta/species_kg.rds")

#peek for funky species (ele because of kg)

bushmeat %>% filter(especes %in% c("GHA", "BON", "BUF", "HYL", "FGA"))
bushmeat %>% filter(especes%in% c("COG", "PLQ")) #change to CPE and PCM if exist
bushmeat %>% filter(especes == "ELE") 

#phase 2, 2 elephants hunted

bushmeat <- bushmeat %>% 
  filter(especes != "ELE")

#step 1
#create a copy of the bushmeat data to manipulate
##why do this? because we need to change nombre NA to 1

bushmeat_off <- bushmeat

####now let's check if we have any NA's in terms of nombre
##if YES we need to deal with them because we count things below
#we remain conservative when assigning numbers to unknowns...
#...our lowest possible number is 1, so this is what will assign the unknown NAs

table(is.na(bushmeat_off$nombre)) 

bushmeat_off$nombre[is.na(bushmeat_off$nombre)] <- 1

#there should now be no TRUE
table(is.na(bushmeat_off$nombre)) 

###see how much bushmeat you have each session and also with only entire animals
#and (new in 2025) shot and trapped...fusil et piege

bushmeatE <- bushmeat_off %>%
  filter(etat=="E" | etat=="ED" | etat=="EF")

bushmeatF <- bushmeat_off %>%
  filter(`tue via` == "F")

bushmeatP <- bushmeat_off %>%
  filter(`tue via` == "P")

#create empty list of appropriate length for offtake per session

bushmeat_session_list <- vector("list", length(unique(bushmeat_off$session)))

for (i in seq_along(unique(bushmeat_off$session))) {

  #populate the list
  bushmeat_session_list[[i]] <- bushmeat_off %>%
    filter(session == i)

  #name it
  names(bushmeat_session_list)[i] <- paste0("bushmeat", i)

  #extract the objects with their names
  assign(names(bushmeat_session_list)[i],
         bushmeat_session_list[[i]])

}

###create a string of unique events of all bushmeat

unique_events <- unique(paste0(bushmeat_off$village, "_", 
                               bushmeat_off$date, "_",
                               bushmeat_off$especes))

unique_eventsE <- unique(paste0(bushmeatE$village, "_", 
                                bushmeatE$date, "_",
                                bushmeatE$especes))

unique_eventsF <- unique(paste0(bushmeatF$village, "_", 
                               bushmeatF$date, "_",
                               bushmeatF$especes))

unique_eventsP <- unique(paste0(bushmeatP$village, "_", 
                                bushmeatP$date, "_",
                                bushmeatP$especes))


#create empty list of appropriate length for unique events
unique_events_session_list <- vector("list", length(bushmeat_session_list)) 

for (i in seq_along(unique(bushmeat_off$session))) {
  
  #populate it
  unique_events_session_list[[i]] <- unique(paste0(bushmeat_session_list[[i]]$village, "_", 
                                                   bushmeat_session_list[[i]]$date, "_",
                                                   bushmeat_session_list[[i]]$especes))
  
  #name it 
  names(unique_events_session_list)[i] <- paste0("unique_events", i)
  
  #extract the objects with their names
  assign(names(unique_events_session_list)[i],
         unique_events_session_list[[i]])
  
}

#now make a dataframe in which later you can check your algorithim's logic with

#nrows is number of sessions + 2 (total & E)
logic_check <- data.frame(matrix(ncol = 3, nrow = length(unique(bushmeat_off$session)) + 2))
colnames(logic_check) <- c("observations", "unique events", "animals")
rownames(logic_check) <- c("all", "entire", paste0("session", unique(bushmeat_off$session)))

#use a mapping function to fill in the observations


logic_check$observations <- c(nrow(bushmeat_off), nrow(bushmeatE),
                              unique(bushmeat_off$session) %>%
                                map(function (x) nrow(bushmeat_session_list[[x]])) %>%
                                unlist())

#same same for unique events
logic_check$'unique events' <- c(length(unique_events), length(unique_eventsE),
                                 unique(bushmeat_off$session) %>%
                                   map(function (x) length(unique_events_session_list[[x]])) %>%
                                   unlist())


###lastly let's check if we currently have any NA's in terms of etat
#because there were E/EF/ED with nombre > 1...
#...that I thus changed the etat to NA in the data cleaning
#this is no problem! clearly NAs wil eventually arise so need to incorporate them after...
#...by reactiving code in the loop

table(is.na(bushmeat_off$etat)) 

#step2
#MAKE YOUR OFFTAKE FUNCTION
#Chris Beirne & I wrote this together, he really drove the beginning
#b stands for your bushmeat choice, and u for your unique events
#note I have a script with fake data trying really hard to break this...
#...and it holds up, always delivering the estimate we expect

generate_offtake <- function(b,u) {
  
  ###create an empty list to fill up  
  tmp_take <- list()
  
  
  for (i in seq_along(u))
  {
    
    tmp <- b[b$village == strsplit(u[i], "_")[[1]][1] &
               b$date == strsplit(u[i], "_")[[1]][2] &
               b$especes == strsplit(u[i], "_")[[1]][3],]
    
    
    ###active this code if you have NAs!
    
    na.tmp <- tmp[is.na(tmp$etat)==T,]
    tmp <- tmp[is.na(tmp$etat)==F,]
    
    
    ##the count starts at 0
    
    count <- 0
    
    ##make your estimated animals from moitie et gigots called MIG
    #first just count your total moities and gigots
    
    tmpMI <- sum((tmp[tmp$etat=="MI" | tmp$etat=="MID" |  tmp$etat=="MIF",])$nombre)
    tmpG <- sum((tmp[tmp$etat=="G" | tmp$etat=="GD" |  tmp$etat=="GF",])$nombre)
    
    #step A: make all your gigots possible as moitie
    #(2 moities make a gigot, and if there is an extra, you toss it out)
    sa <- floor(tmpG/2)
    
    #step B: keep your extra gigot (if you have one) on the side
    #(the difference 'picks' up the gigot you tossed out above)
    sb <- ceiling(tmpG/2) - floor(tmpG/2)
    
    #step C: make as many betes as possible...
    #by using your total moities (including the gigots you just converted)
    #which excludes your extra gigot and moitie (if you have them)
    #(two moities make a bete, and if there is an extra you toss it out)
    sc <- floor((tmpMI + sa)/2)
    
    #step D: keep your extra moitie (if you have one) on the side
    #(the difference 'picks' up the moitie you tossed out above)
    sd <- ceiling((tmpMI + sa)/2) - floor((tmpMI + sa)/2)
    
    #step E: determine whether you have one additional bete..
    ##due to either an extra gigot, extra moitie, or both
    ##(you may not have one)
    ##you divide by two and round up so that 0 + 0, 0 + 1, and 1 + 1 all equal 1
    se <- ceiling(sb/2 + sd/2)
    
    #create MIG!! estimated betes from moties and gigots together
    ##add the many betes as possible to your potential additional bete
    MIG <- sc + se 
    
    ###all entire animals (regardless of state) add 1 to the count
    ##if you filter bushmeat to ONLY include E, ED, and EF..
    ##..and then comment the logic dealing with other etats...
    ##...the length of offtake must equal the length of the filtered bushmeat
    
    if(length(tmp$etat[tmp$etat=="E" | tmp$etat=="ED" |  tmp$etat=="EF"])>0)
    {
      #the below lines was before E was sometimes > 1 in phase 1
      #count <- count + nrow(tmp[tmp$etat=="E" | tmp$etat=="ED" |  tmp$etat=="EF", ])
      count <- count + sum(tmp$nombre[tmp$etat=="E" | tmp$etat=="ED" |  tmp$etat=="EF"])
    }
    
    ###if there is one or more head, and this number is greater than or equal to...
    ###the number of skins, &
    ###the number of animals estimated via MIG
    ##then we add the number of heads to the count
    
    if(sum(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) > 0 &
       sum(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) >= 
       sum(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"]) &
       sum(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) >= MIG
    ) 
    {
      count <- count + sum(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"])
    }
    
    ###if then is one or more skins & more than heads, &
    #more or equal than the number of animals estimated via MIG
    ###we add the skins
    
    if(sum(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"]) > 0 &
       sum(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"]) > 
       sum(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) &
       sum(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"]) >= MIG
    ) 
    {
      count <- count + sum(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"])
    }
    
    ###if then is one or more MIG & more than heads, &
    #more than skins, &
    ##we add MIG
    
    if(MIG > 0 &
       MIG > sum(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) &
       MIG > sum(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"])
    )
    {
      count <- count + MIG
    }
    
    
    ##if there are no heads or skins or halves or gigots...
    ###...but there are some morceaux we simply add 1
    
    if(length(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) == 0 &
       length(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"]) == 0 & 
       length(tmp$nombre[tmp$etat=="MI" | tmp$etat=="MID" |  tmp$etat=="MIF"]) == 0 &
       length(tmp$nombre[tmp$etat=="G" | tmp$etat=="GD" |  tmp$etat=="GF"]) == 0 &
       length(tmp$nombre[tmp$etat %in% c( "MR", "MRF", "MRD")]) > 0  
    ) 
    {
      count <- count + 1
    }
    
    ##if we have no heads, halves, or bits, but DO have etat = NA, we add 1 to our count
    #reactive this code if you have NAs
    ##I don't understand why we put the tmp <- na.tmp???
    
    if(length(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) == 0 &
       length(tmp$nombre[tmp$etat=="P" | tmp$etat=="PD" |  tmp$etat=="PF"]) == 0 &
       MIG == 0 &
       length(tmp$nombre[tmp$etat %in% c("MR", "MRF", "MRD")]) == 0 &
       nrow(na.tmp) > 0  )
    {
      count <- count + 1
      tmp <- na.tmp
    }
    
    if(length(tmp$nombre[tmp$etat=="T" | tmp$etat=="TD" |  tmp$etat=="TF"]) == 0 &
       length(tmp$nombre[tmp$etat=="MI" | tmp$etat=="MID" |  tmp$etat=="MIF"]) == 0 &
       length(tmp$nombre[tmp$etat %in% c("G", "GF", "GD",
                                         "MR", "MRF", "MRD",
                                         "P", "PF", "PD")]) == 0 &
       nrow(na.tmp) > 0  )
    {
      count <- count + 1
      tmp <- na.tmp
    }
    
    ####fill up your offtake!
    
    tmp_take[[i]] <- data.frame(village=rep(tmp$village[1], times=count),
                                date=tmp$date[1],
                                uniqueID=tmp$uniqueID[1],
                                session=tmp$session[1],
                                especes=tmp$especes[1])
    
    
    
  }
  
  return(tmp_take)
  
}

##make your offtakes

offtake <- do.call("rbind", generate_offtake(bushmeat_off, unique_events))

offtakeE <- do.call("rbind", generate_offtake(bushmeatE, unique_eventsE))

offtakeF <- do.call("rbind", generate_offtake(bushmeatF, unique_eventsF))

offtakeP <- do.call("rbind", generate_offtake(bushmeatP, unique_eventsP))


####check how well your offtake worked with the logic check table

logic_check$animals <- logic_check$animals <- c(nrow(offtake), nrow(offtakeE),
                                                unique(bushmeat_off$session) %>%
                                                  map(function (x) nrow(offtake %>% filter(session == x))) %>%
                                                  unlist())

logic_check

#session sums must equal all

identical(
logic_check %>% 
  filter(rownames(logic_check) == "all") %>% 
  colSums(),

logic_check %>% 
  filter(rownames(logic_check) %ni%  c("entire","all")) %>% 
  colSums()
)


#obs and animals must be equal in entire
identical(
  (logic_check %>%
     filter(rownames(logic_check) == "entire"))$observations,
  (logic_check %>%
     filter(rownames(logic_check) == "entire"))$animals
)

#P and F seem good

nrow(offtakeF)
nrow(bushmeatF)
nrow(bushmeatF %>% filter(etat == "E"))

nrow(offtakeP)
nrow(bushmeatP)
nrow(bushmeatP %>% filter(etat == "E"))

#clean and save offtake

cleanoff <- function (x) {
  tmp <- as_tibble(x) %>% 
    dplyr::rename(species = especes) %>% 
    mutate_if(is.character,funs(factor(.)))
  
  left_join(tmp,kg) %>% 
    group_by(village) %>%
    filter(session != min(session)) %>% #cut first session in each village
    ungroup() %>% 
    filter(session < 15) #last session was cut short in phase 2, phase 3 only has 14
  
  }

offtake <- cleanoff(offtake)
offtakeF <- cleanoff(offtakeF)
offtakeP <- cleanoff(offtakeP)

offtake
offtakeF
offtakeP

#get offtake daily

effort <- effort %>% 
  group_by(village) %>%
  filter(session != min(session)) %>% #cut first session in each village
  ungroup() %>% 
  filter(session < 15) #last session was cut short in phase 2, phase 3 only has 14

god <- function (x) {
  
  left_join(
    effort %>% 
      dplyr::select(uniqueID, village,session,date) %>% 
      dplyr::distinct(),
    left_join(x,kg) %>% 
      dplyr::group_by(uniqueID) %>% 
      dplyr::summarize(n_offtake = n(),
                kg_offtake = sum(kg)
      )
  ) %>% 
    dplyr::mutate(across(everything(), ~replace_na(., 0))) %>% 
    dplyr::select(village,date,uniqueID,session,n_offtake,kg_offtake)
  
}

offtake_daily <- god(offtake)

identical(nrow(offtake),sum(offtake_daily$n_offtake))


offtake_daily

#annual offtake

gao <- function (x) {
  
  left_join(
    x %>% 
      dplyr::group_by(village) %>% 
      dplyr::summarize(n_offtake_total = sum(n_offtake),
                kg_offtake_total = sum(kg_offtake),
      ),
    x %>% 
      dplyr::group_by(village) %>% 
      dplyr::summarize(n_days = n())
  ) %>% 
    dplyr::mutate(
      n_offtake_yearly = (n_offtake_total*365)/(n_days*0.53), #53% from aje 2022
      n_offtake_yearly_lo = (n_offtake_total*365)/(n_days*0.6),
      n_offtake_yearly_hi = (n_offtake_total*365)/(n_days*0.4),
      kg_offtake_yearly = (kg_offtake_total*365)/(n_days*0.53),
      kg_offtake_yearly_lo = (kg_offtake_total*365)/(n_days*0.6),
      kg_offtake_yearly_hi = (kg_offtake_total*365)/(n_days*0.4)
    ) %>% 
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 0)))
  
}

offtake_yearly <- gao(offtake_daily)


#summarize

offtake
offtakeF
offtakeP
offtake_daily
offtake_yearly

tbl_names <- c(
  "offtake",
  "offtakeF",
  "offtakeP",
  "offtake_daily",
  "offtake_yearly"
)

# Create a list of modified tibbles with phase = 2
offtake_list_p2 <- tbl_names %>%
  set_names(paste0(tbl_names, "_p2")) %>%   # <- give new names with _p2
  map(~ {
    get(.) %>%                              # get the tibble by name
      mutate(phase = 2) %>% 
      dplyr::select(phase,everything())
  })

saveRDS(offtake_list_p2, "./inputs/offtake/offtake_list_p2.rds")

#yearly offtake by species
#first assign all CMN to CBA,CPE,CFN,or CVB

cmn <- c("CPE", "CBA", "CFN", "CVB")

propCMN <- offtake %>% 
  mutate_if(is.character,funs(factor(.)))  %>% 
  filter(species %in% cmn) %>% 
  droplevels() %>%
  count(species, name = "n") %>%
  mutate(prop = n / sum(n)) %>%
  dplyr::select(species, prop) %>% 
  arrange(desc(prop))

propCMN
sum(propCMN$prop)

set.seed(123) 

idx <- which(offtake$species == "CMN")

offtakeCMN <- offtake

offtakeCMN$species[idx] <- sample(
  as.character(propCMN$species),  
  size = length(idx),
  replace = TRUE,
  prob = propCMN$prop
)
#check 

identical(nrow(offtake),nrow(offtakeCMN))
identical(nrow(offtake %>% filter(species == "CBL")),nrow(offtakeCMN %>% filter(species == "CBL")))
identical(nrow(offtake %>% filter(species == "ATH")),nrow(offtakeCMN %>% filter(species == "ATH")))

offtake %>% filter(species == "CMN")
offtakeCMN %>% filter(species == "CMN")

identical(
nrow(offtake %>% filter(species %in% cmn)) + nrow(offtake %>% filter(species == "CMN")),
nrow(offtakeCMN %>% filter(species %in% cmn))
)

check_cmn <- function (x) {
  
  x %>% 
    filter(species %in% cmn) %>% 
    dplyr::group_by(species) %>% 
    summarize(n = n())
}

cmn_added <- left_join(
check_cmn(offtake) %>% rename(n_raw = n),
check_cmn(offtakeCMN) %>% rename(n_cor = n)
) %>% 
  mutate(n_added = n_cor - n_raw,
         p_added = n_added/nrow(offtake %>% filter(species == "CMN"))
         ) %>% 
  arrange(desc(n_added))

cmn_added <- left_join(cmn_added,propCMN) %>% 
  rename(p_obs = prop)

cmn_added

#all good, proceed

offtake_yearly_species_p2 <- offtakeCMN %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarize(n_offtake_total = n(),
            n_days = length(unique(offtake_daily$date)),
            n_offtake_yearly = (n_offtake_total*365)/(n_days*0.53), #53% from aje 2022
            n_offtake_yearly_lo = (n_offtake_total*365)/(n_days*0.6),
            n_offtake_yearly_hi = (n_offtake_total*365)/(n_days*0.4)
            ) %>% 
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 0))) %>% 
  dplyr::arrange(desc(n_offtake_yearly)) %>% 
  dplyr::mutate(phase = 2) %>% 
  dplyr::select(phase,everything())

saveRDS(offtake_yearly_species_p2, "./inputs/offtake/offtake_yearly_species_p2.rds")

#and by species by village

offtake_yearly_species_village_p2 <- offtakeCMN %>% 
  dplyr::group_by(village,species) %>% 
  dplyr::summarize(n_offtake_total = n(),
                   n_days = length(unique(offtake_daily$date)),
                   n_offtake_yearly = (n_offtake_total*365)/(n_days*0.53), #53% from aje 2022
                   n_offtake_yearly_lo = (n_offtake_total*365)/(n_days*0.6),
                   n_offtake_yearly_hi = (n_offtake_total*365)/(n_days*0.4)
  ) %>% 
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 0))) %>% 
  dplyr::arrange(village,desc(n_offtake_yearly)) %>% 
  dplyr::mutate(phase = 2) %>% 
  dplyr::select(phase,everything())

saveRDS(offtake_yearly_species_village_p2, "./inputs/offtake/offtake_yearly_species_village_p2.rds")

