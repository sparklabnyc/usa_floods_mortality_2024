create_model_comp_table <- function(data){
  data <- full_model
  
  dic <- data$dic$dic
  waic <- data$waic$waic
  cpo <- sum(data$cpo$cpo)
  marg_lik <- data$mlik[1]
  
  test.grid <- expand_grid(
    dic = c(dic),
    waic = c(waic),
    cpo = c(cpo),
    marg_lik = c(marg_lik)) %>% 
    mutate(model_name = paste0(hpo,'_',fo,'_',dfs, '_', name)) 
  
  return(test.grid)
}
