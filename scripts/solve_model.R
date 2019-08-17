solve_model <- function(model, shocks, alpha_min = 1e-4, alpha_max = 1e-1){
  
  sol_bln <- solve_emr_block(
    model = model,
    alpha_min = alpha_min,
    alpha_max = alpha_max
  )
  
  results_bln <- map(sol_bln$variables, ~{
    if(!is.null(dim(.x))){
      memisc::to.data.frame(.x, as.vars = 0, name = "value_bln")    
    } else {
      enframe(.x, name = "region", value = "value_bln")
    }
  })
  
  model_cfl <- model
  model_cfl$params <- shocks
  
  sol_cfl <- solve_emr_block(
    model = model_cfl,
    alpha_min = alpha_min,
    alpha_max = alpha_max
  )
  
  results_cfl <- map(sol_cfl$variables, ~{
    if(!is.null(dim(.x))){
      memisc::to.data.frame(.x, as.vars = 0, name = "value_cfl")    
    } else {
      enframe(.x, name = "region", value = "value_cfl")
    }
  })
  
  final_results <- map(names(results_cfl), ~{
    suppressMessages(
      left_join(results_bln[[.x]], results_cfl[[.x]])
    )%>% 
      mutate(change = value_cfl/value_bln - 1)
  })
  
  names(final_results) <- names(results_bln)
  
  final_results
  
}