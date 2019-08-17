armington <- function(data, eta = -0.5, mu = 3, sig = 3){
  
  lambda <- trade_data %>% 
    group_by(destination) %>% 
    mutate(
      lambda = (1 + t)^sig * value/(sum((1 + t) * value))
    ) %>% 
    select(source, destination, lambda)
  
  regions <- trade_data %>% 
    pull(source) %>% 
    unique()
  
  sets <- list(
    source = regions,
    destination = regions
  )
  
  params <- list()

  params[["t"]] <- create_param(
    value = trade_data %>% 
      select(source, destination, t),
    indexes = sets[c('source', 'destination')],
    desc = "Tariff level"
  )
  
  params[["lambda"]] <- create_param(
    value = lambda,
    indexes = sets[c('source', 'destination')],
    desc = "Taste parameter"
  )
  
  params[["eta"]] <- create_param(
    value = eta,
    indexes = "eta",
    desc = "Demand price elasticity"
  )
  
  params[["mu"]] <- create_param(
    value = mu,
    indexes = "mu",
    desc = "Supply price elasticity"
  )
  
  params[["sig"]] <- create_param(
    value = sig,
    indexes = "sig",
    desc = "Elasticity of substitution between varieties"
  )
  
  params[["Q0"]] <- create_param(
    value = trade_data %>% 
      group_by(destination) %>% 
      summarise(value = sum(value * (1 + t))),
    indexes = sets["destination"],
    desc = "Initial total demand"
  )
  
  params[["P0"]] <- create_param(
    value = 1,
    indexes = sets["destination"],
    desc = "Initial price index"
  )
  
  params[["Y0"]] <- create_param(
    value = trade_data %>% 
      group_by(source) %>% 
      summarise(value = sum(value)),
    indexes = sets["source"],
    desc = "Initial total supply"
  )
  
  params[["c0"]] <- create_param(
    value = 1,
    indexes = sets["source"],
    desc = "Initial marginal cost"
  )
  
  variables <- list()
  equations <- list()
  
  variables[["Q"]] <- create_variable(
    value = params$Q0$value,
    indexes = sets["destination"],
    type = "defined",
    desc = "Total demand"
  )
  
  equations[["E_Q"]] <- create_equation(
    "Q[r] = Q0[r] * (P[r]/P0[r])^eta",
    indexes = "r in destination",
    type = "defining",
    desc = "Total demand"
  )
  
  variables[["Y"]] <- create_variable(
    value = params$Y0$value,
    indexes = sets["source"],
    type = "defined",
    desc = "Total supply"
  )
  
  equations[["E_Y"]] <- create_equation(
    "Y[r] = Y0[r] * (c[r]/c0[r])^mu",
    indexes = "r in source",
    type = "defining",
    desc = "Total supply"
  )
  
  variables[["P"]] <- create_variable(
    value = params$P0$value,
    indexes = sets["destination"],
    type = "defined",
    desc = "Composite price index"
  )
  
  equations[["E_P"]] <- create_equation(
    "P[s] = sum(lambda[,s] * ((1+t[,s]) * c[])^(1-sig))^(1/(1-sig))",
    indexes = "s in destination",
    type = "defining",
    desc = "Composite price index"
  )
  
  variables[["q"]] <- create_variable(
    value = trade_data %>% 
      select(source, destination, value),
    indexes = sets[c("source", "destination")],
    type = "defined",
    desc = "import quantity by source and destination"
  )
  
  equations[["E_q"]] <- create_equation(
    "q[r,s] = lambda[r,s] * ((1+t[r,s]) * c[r]/P[s])^(-sig)  * Q[s]",
    indexes = c("r in source", "s in destination"),
    type = "defining",
    desc = "Import quantity by source and destination"
  )
  
  variables[["c"]] <- create_variable(
    value = params$c0$value,
    indexes = sets["source"],
    type = "undefined",
    desc = "Composite input unit cost"
  )
  
  equations[["E_c"]] <- create_equation(
    "Y0[r] * (c[r]/c0[r])^mu - sum(q[r,])",
    indexes = 'r in source',
    type = "mcc",
    desc = "Market clearing condition"
  )
  
  armington <- list(
    sets = sets,
    params = params,
    variables = variables,
    equations = equations
  )
  
}