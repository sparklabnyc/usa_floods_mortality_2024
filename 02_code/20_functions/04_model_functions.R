#1.Function to run Bayesian conditional quasi-Poisson model 
run_model_function <- function(causes, groups, expo_types,expo_thresholds, flood_types){
  
  for (subcauses in causes){
    print(subcauses)
    
    for (subgroups in groups){
      print(subgroups)
      
      mort_data <- readRDS(paste0(outcome.data.folder, subcauses, '_', subgroups, '_tidy_data.rds')) #load mortality data 
      
      for (types in expo_types){
        print(types)
        
        flood_by_expo_type <- flood_data %>% filter(expo_type == types)  #filter flood data to specific exposure type
        
        for (thresholds in expo_thresholds){
          print(thresholds)
          
          flood_by_threshold <- flood_by_expo_type %>% filter(expo_threshold == thresholds)
          
          for(flood_cats in flood_types){
            print(flood_cats)
            flood_by_cat <- flood_by_threshold %>% filter(flood_cat %in% c("none", flood_cats)) |> 
              rename(state_name = "state")
            unique(flood_by_cat$flood_cat)
          
          model_data <- left_join(mort_data, flood_by_cat, by = c("fips", "year", "month")) %>% 
            filter(!is.na(lag_0)) #this removes fips that never have a flood (according to GFD)
          
          #define model form
          model_form <- deaths ~
            ns(year, df = num_df) +
            f(inla.group(max_mean_temp, n = 5), model = 'rw2', hyper = list(prec = list(prior = "loggamma", param = c(1,hyper_value)))) + 
            lag_0 + lag_1 + lag_2 + lag_3 + 
            f(stratum, model="iid", hyper = list(prec = list(prior = "loggamma", param = c(1, hyper_value)))) 
          
          #run rough model - why?
          rough_model <-inla(safe = TRUE, formula = model_form,
                             family = "poisson",
                             data = model_data,
                             E = pop,
                             control.compute = list(dic=TRUE, openmp.strategy="pardiso.parallel"),
                             control.predictor = list(link = 1),
                             control.inla = list(diagonal = 1000, int.strategy = 'eb', strategy = 'gaussian')) #this runs but generates a seemingly meaningful warning
          
          #run full model
          full_model <- inla(safe = TRUE, formula = model_form,
                             family = 'poisson',
                             data = model_data,
                             E = pop,
                             control.compute = list(config=TRUE,dic=TRUE,openmp.strategy="pardiso.parallel"),
                             control.predictor = list(link = 1),
                             control.inla = list(diagonal = 0),
                             control.mode = list(result = rough_model, restart = TRUE))
          
          #output model results as dataframe 
          output_df <- full_model$summary.fixed %>% as.data.frame() %>% 
            mutate(cause = subcauses,
                   group = subgroups,
                   type = types,
                   threshold = thresholds,
                   flood_cat = flood_cats)
          
          #join model result dataframes throughout iteration 
          if(exists("results_df")){
            results_df <- bind_rows(results_df, output_df)
          }else{
            results_df <- output_df
          }
        }
      }
    }
    }
  }
  return(results_df)
}


