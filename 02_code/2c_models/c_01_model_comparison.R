rm(list = ls())
#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

#1.Set groups for model comparison; can make this iterative, but seems unnecessary 
#as we aren't trying to find the marginally best fitting model for each specific model. We want to identify the best
#fitting model that works for all subgroups and subcauses - for this reason, stick with 'overall' and 'any'. 
subcauses <- causes[1]
subgroups <- "overall"
types <- 'pop_expo'
thresholds <- 'any'

#2a.Load and join data for model comparison 
mort_data <- readRDS(paste0(outcome.data.folder, subcauses, '_', subgroups, '_tidy_data.rds')) 

flood_by_expo_type <- read_csv(paste0(exposure.data.folder, "flood_pop_data_by_thresh_type.csv")) %>% 
  dplyr::select(-c(state)) %>% 
  filter(expo_type == types)


joined_data <- left_join(mort_data, flood_by_expo_type, by = c("fips", "year", "month")) %>% 
  mutate_at(c(10:13,16,18), ~replace_na(.,0)) %>% #4/16 note: check these column positions 
  mutate_at(c(14:15,17), ~replace_na(.,"none"))

#2b.Prep data to run in model 
model_data <- joined_data %>% filter(expo_threshold %in% c(thresholds, "none")) %>% 
  mutate_at(c("fips"), as.factor) %>% 
  mutate_at(c("month"), as.factor) %>% 
  group_by(fips) %>% 
  arrange(-desc(month)) %>% 
  arrange(-desc(year)) %>% 
  rename(lag_0 = flood_occur) %>% 
  mutate(lag_1 = lagpad(lag_0,1),
         lag_2 = lagpad(lag_0,2),
         lag_3 = lagpad(lag_0,3),
         lag_4 = lagpad(lag_0,4),
         lag_5 = lagpad(lag_0,5),
         lag_6 = lagpad(lag_0,6)) %>% 
  mutate(stratum = as.factor(month:fips))

#3a.Set values to test for model comparison
model_names <- c("basic", "flood_days") 
hyper_prior_opts <- c("pc.prec", "loggamma")
family_opts <- c("poisson", "nbinomial2")
num_df <- c(2,3,4)

#3b.Run model comparison and output table of dic,waic,cpo, and marginal likelihood for each model 

for (hpo in hyper_prior_opts){
  
  print(hpo)
  
  for (fo in family_opts){
    print(fo)
    
    for (dfs in num_df){
      print(dfs)
      
      for (name in model_names){
        print(name)
        
        if(name == "basic"){
       
        rough_model <-inla(formula = deaths ~
                            ns(year, df = dfs) +
                            lag_0 + lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + 
                            f(stratum, model='iid', hyper = list(prec = list(prior = hpo, param = c(1, hyper_value)))),
                          family = fo,
                          data = model_data,
                          E = pop,
                          control.compute = list(dic=TRUE, waic = TRUE, cpo = TRUE, openmp.strategy="pardiso.parallel"),
                          control.predictor = list(link = 1),
                          control.inla = list(int.strategy = 'eb', strategy = 'gaussian'))
        
        full_model <- inla(formula = deaths ~
                             ns(year, df = dfs) +
                             lag_0 + lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + 
                             f(stratum, model='iid', hyper = list(prec = list(prior = hpo, param = c(1, hyper_value)))),
                           family = fo,
                           data = model_data,
                           E = pop,
                           control.compute = list(config=TRUE,dic=TRUE, waic = TRUE, cpo = TRUE, openmp.strategy="pardiso.parallel"),
                           control.predictor = list(link = 1),
                           control.inla = list(diagonal = 0),
                           control.mode = list(result = test_rough, restart = TRUE))
        
        }
        grid_model_output <- create_model_comp_table(full_model)
        
        if(exists("mod_comp")){
          mod_comp <- bind_rows(mod_comp, grid_model_output)
        }else{
          mod_comp <- grid_model_output
        }
        
        if(name == "flood_days"){
          
          rough_model <-inla(formula = deaths ~
                              ns(year, df = dfs) +
                              flood_days_max + 
                              lag_0 + lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + 
                              f(stratum, model='iid', hyper = list(prec = list(prior = hpo, param = c(1, hyper_value)))),
                            family = fo,
                            data = model_data,
                            E = pop,
                            control.compute = list(dic=TRUE, waic = TRUE, cpo = TRUE, openmp.strategy="pardiso.parallel"),
                            control.predictor = list(link = 1),
                            control.inla = list(int.strategy = 'eb', strategy = 'gaussian'))
          
          
          full_model <- inla(formula = deaths ~
                               ns(year, df = dfs) +
                               flood_days_max + 
                               lag_0 + lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + 
                               f(stratum, model='iid', hyper = list(prec = list(prior = hpo, param = c(1, hyper_value)))),
                             family = fo,
                             data = model_data,
                             E = pop,
                             control.compute = list(config=TRUE,dic=TRUE, waic = TRUE, cpo = TRUE, openmp.strategy="pardiso.parallel"),
                             control.predictor = list(link = 1),
                             control.inla = list(diagonal = 0),
                             control.mode = list(result = test_rough, restart = TRUE))
        
        }
        
        grid_model_output <- create_model_comp_table(full_model)
        
        if(exists("mod_comp")){
          mod_comp <- bind_rows(mod_comp, grid_model_output)
        }else{
          mod_comp <- grid_model_output
        }
        }
      }
    }
  }
return(mod_comp)

write_csv(mod_comp, paste0(output.folder, subcauses,"_model_comparison_table.csv"))
