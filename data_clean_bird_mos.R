##########################################################################################
### Load and clean results from previously run model for Bird to Mosquito transmission ###
##########################################################################################

## If the Stan model has been run previously and the user does not want to rerun it, load data
if (load_stan_bird_mos_res == TRUE & file.exists("saved_output/bird_to_mosquito_transmission.Rds")) {

bird_mos_rds            <- readRDS("saved_output/bird_to_mosquito_transmission.Rds")
bird_mos_model_out_summ <- bird_mos_rds[[1]]
bird_mos_pred           <- bird_mos_rds[[2]]
samps_bird_mos          <- bird_mos_rds[[3]]
rm(bird_mos_rds)

samps_bird_mos <- rbind(samps_bird_mos[,1,], samps_bird_mos[,2,], samps_bird_mos[,3,], samps_bird_mos[,4,])

} else {

mos_bird_trans  <- read.csv("data/mos_bird_trans.csv", header = TRUE)

## Data associated with bird to mosquito transmission
vectcomp <- mos_bird_trans %>% filter(Host_to_Mosquito == "Y" & Time_Series == "N")

## Needed to reset factors so that they are a continuous seq of numbers when converted to numeric for Stan model
vectcomp <- droplevels(vectcomp)

# BRADY NOTE: for some reason, vector_species and citation are not properly
# converted to factors  so when you do as_numeric, it messes up. if this doesn't work
# (or even if it does), try making those 2 columns into factors which should fix any issue and work as intended



## Stan model data
bird_mos.data <- 
  with(vectcomp, 
     list(
      "N"        = nrow(vectcomp)
    , "N_Samp"   = Sample_Size
    , "N_Inf"    = Number_Infected
    , "N_CIT"    = length(unique(Citation))
    , "N_VS"     = length(unique(Vector_Species))
    , "Samp_Max" = max(Sample_Size)
    , "Inf_Max"  = max(Number_Infected)
    , "LD"       = Log_Dose
    , "Temp"     = Temperature_C
    , "VS"       = as.numeric(factor(Vector_Species)) #first NA thing
    , "CIT"      = as.numeric(factor(Citation)) #second NA thing
    )
    )

## Run model
bird_mos_model_out <- stan(
   file     = "stan/Bird_to_Mosquito.stan"
  , data    = bird_mos.data
  , iter    = 14000
  , thin    = 4
  , warmup  = 4000
  , refresh = max(14000/100, 1)
  , control = list(max_treedepth = 16, adapt_delta = .90)
  , chains  = 4)

## Pleasant way to look at convergence of the model
# launch_shinystan(bird_mos_model_out)

# BRADY NOTE: I MANAGED TO GET UP TO HERE BUT THE NEXT FEW LINES I DO NOT UNDERSTAND

# detach("package:tidyr", unload = TRUE) #unloads so extract works but this is not useful if we say rstan::extract
samps_bird_mos          <- rstan::extract(bird_mos_model_out, permuted = FALSE)
library(tidyr)
library(broom) 
tidy_bird_mos           <- broom::tidy(samps_bird_mos) # changed from bird_mos_model_out
bird_mos_model_out_summ <- rstan::summary(bird_mos_model_out) # change summary to rstan::summary
bird_mos_pred           <- bird_mos_model_out_summ[["summary"]]

saveRDS(list(bird_mos_model_out_summ, bird_mos_pred, samps_bird_mos)
  , file = "saved_output/bird_to_mosquito_transmission.Rds")

samps_bird_mos <- rbind(samps_bird_mos[,1,], samps_bird_mos[,2,], samps_bird_mos[,3,], samps_bird_mos[,4,])

}
