rm(list = ls())
#0a.Declare root directory, folder location and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))


#1a.Load flood data
flood_data <- read_csv(paste0(exposure.data.folder, "flood_pop_data_with_lags_type.csv"), 
                       col_types = cols(stratum = readr::col_factor())) %>%  
  mutate(fips = as.character(fips)) 

#1b.Set categories for iteration 
causes <- c( "Cardiovascular diseases", "Cancers", "Respiratory diseases",
             "Injuries", "Neuropsychiatric conditions", "Infectious and parasitic diseases") 
groups <- c("overall")
expo_types <- c("pop_expo")
expo_thresholds <- c("any", '1_pert',"25_pert", "50_pert", "75_pert")
flood_types <- c("Heavy rain", "Snowmelt", "Tropical cyclones", "Ice jams and dam breaks") #all_floods


#2.Function to create 'all cause' mortality by combining cause-specific mortality; DO NOT NEED TO RUN - combined dataset in outcome data folder 
load_data_function <- function(causes, groups){
  
  for (subcauses in causes){
    print(subcauses)
    
    for (subgroups in groups){
      print(subgroups)
      
      mort_data <- readRDS(paste0(outcome.data.folder, subcauses, '_', subgroups, '_tidy_data.rds')) #load mortality data 
      
      
      if(exists("results_df")){
        results_df <- bind_rows(results_df, mort_data)
      }else{
        results_df <- mort_data
      }
    }
  }
  return(results_df)
}

combo_mort_data <- load_data_function(causes,groups)
model_data <- combo_mort_data |> dplyr::select(-mort_rate) |> 
  group_by(state, fips, year, month, model_group, pop) |> 
  summarize(deaths = sum(deaths)) |> 
  mutate(cause ='All')

#write_feather(model_data, paste0(outcome.data.folder, 'all_cause_mort.feather'))
#Need to figure this out
n_distinct(combo_mort_data$fips) #there should be 678,240 pop values (3140 fips, 12 months, 18 years)
test <- combo_mort_data |> ungroup() |> dplyr::select(year, month, fips, pop) |> distinct() |> group_by(year, month, fips) |> tally()
sum(test$n)

#3.Run. model with all cause mortaltiy 
hyper_value <- 0.001 #hyperparameter value
num_df <- 54 #number of degrees of freedom for year term
groups <- 'all'

run_model_function_all <- function(groups, expo_types,expo_thresholds, flood_types){
  
  
  for (subgroups in groups){
    print(subgroups)
    
    mort_data <- model_data <- read_feather(paste0(outcome.data.folder, subgroups,'_cause_mort.feather'))
    
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
            mutate(cause = 'all',
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
  return(results_df)
}

model_results <- run_model_function_all(groups,expo_types,expo_thresholds,flood_types) |> 
  rownames_to_column()
#model_results %>% write_csv(paste0(model.output.folder, "model_results_all_mort_causes.csv"))

