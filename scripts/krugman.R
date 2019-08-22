krugman <- function(data, eta = -0.5, mu = 3, sig = 3){
  
  trade_data <- trade_data %>% 
    mutate(N0 = 5) %>% 
    group_by(source) %>% 
    mutate(fk = sum(value)/(N0 * sig)) %>% 
    ungroup() %>% 
    mutate(
      p0 = (1 + t)/(1 - 1/sig),
      q0 = ((1 + t) * value)/(p0 * N0)
    ) %>% 
    group_by(destination) %>% 
    mutate(lambda = (q0 * (p0)^sig)/sum((1 + t) * value)) %>% 
    #group_by(destination) %>% 
    #summarise(P = sum(lambda * N0 * p0^(1-sig))^(1/(1-sig)))
    ungroup()

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
    value = trade_data %>% 
      select(source, destination, lambda),
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
  
  params[["p0"]] <- create_param(
    value = trade_data %>% 
      select(source, destination, p0),
    indexes = sets[c('source', 'destination')],
    desc = "Benchmark firm price"
  )
  
  params[["q0"]] <- create_param(
    value = trade_data %>% 
      select(source, destination, q0),
    indexes = sets[c('source', 'destination')],
    desc = "Benchmark firm quantity"
  )
  
  params[["N0"]] <- create_param(
    value = trade_data %>% 
      select(source, N0) %>% 
      distinct(),
    indexes = sets[c('source')],
    desc = "Benchmark number of firms"
  )
  
  params[["fk"]] <- create_param(
    value = trade_data %>% 
      select(source, fk) %>% 
      distinct(),
    indexes = sets[c('source')],
    desc = "Fixed costs"
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
    "P[s] = sum(lambda[,s] * N[] * p[,s]^(1-sig))^(1/(1-sig))",
    indexes = "s in destination",
    type = "defining",
    desc = "Composite price index"
  )
  
  variables[["q"]] <- create_variable(
    value = params$q0$value,
    indexes = sets[c("source", "destination")],
    type = "defined",
    desc = "import quantity by source and destination"
  )
  
  equations[["E_q"]] <- create_equation(
    "q[r,s] = lambda[r,s] * (p[r,s]/P[s])^(-sig) * Q[s]",
    indexes = c("r in source", "s in destination"),
    type = "defining",
    desc = "Import quantity by source and destination"
  )
  
  variables[["c"]] <- create_variable(
    value = params$c0$value,
    indexes = sets["source"],
    type = "defined",
    desc = "Composite input unit cost"
  )
  
  equations[["E_c"]] <- create_equation(
    "c[r] = sum(p[r,] * q[r,]/(1+t[r,]))/(sig * fk[r])",
    indexes = 'r in source',
    type = "defining",
    desc = "Composite input unit cost"
  )
  
  variables[["p"]] <- create_variable(
    value = params$p0$value,
    indexes = sets[c("source", "destination")],
    type = "undefined",
    desc = "Firm price"
  )
  
  equations[["E_p"]] <- create_equation(
    "p[r,s] - ((1 + t[r,s]) * c[r])/(1 - 1/sig)",
    indexes = c('r in source', 's in destination'),
    type = "mcc",
    desc = "Firm price"
  )
  
  variables[["N"]] <- create_variable(
    value = params$N0$value,
    indexes = sets[c("source")],
    type = "undefined",
    desc = "Number of firms"
  )
  
  equations[["E_N"]] <- create_equation(
    "N[r] - Y[r]/(fk[r] + sum(q[r,]))",
    indexes = c('r in source'),
    type = "mcc",
    desc = "Number of firms"
  )
  
  list(
    sets = sets,
    params = params,
    variables = variables,
    equations = equations
  )
  
}
