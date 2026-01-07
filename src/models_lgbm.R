src/models_lgbm.R
# Entrenamiento final: modelo puntual y modelos cuantiles

params_point <- list(
  objective = "regression",
  metric = "rmse",
  learning_rate = 0.05,
  num_leaves = 15,
  max_depth = 4,
  min_data_in_leaf = 6,
  feature_fraction = 0.9
)

train_final_lgb <- function(df_feat_imp, params = params_point, nrounds = 300) {
  
  df_tr <- prep_train_set(df_feat_imp)
  X <- as.matrix(df_tr %>% dplyr::select(-date, -product, -y))
  y <- df_tr$y
  
  dtrain <- lightgbm::lgb.Dataset(X, label = y)
  
  lightgbm::lgb.train(
    params  = params,
    data    = dtrain,
    nrounds = nrounds,
    verbose = -1
  )
}

train_quantile_models <- function(df_feat_imp,
                                  alphas = c(0.1, 0.5, 0.9),
                                  nrounds = 350) {
  
  df_tr <- prep_train_set(df_feat_imp)
  X <- as.matrix(df_tr %>% dplyr::select(-date, -product, -y))
  y <- df_tr$y
  
  dtrain <- lightgbm::lgb.Dataset(X, label = y)
  
  models <- list()
  
  for (a in alphas) {
    
    params_q <- list(
      objective = "quantile",
      metric = "quantile",
      alpha = a,
      learning_rate = 0.05,
      num_leaves = 15,
      max_depth = 4,
      min_data_in_leaf = 6,
      feature_fraction = 0.9
    )
    
    models[[as.character(a)]] <- lightgbm::lgb.train(
      params  = params_q,
      data    = dtrain,
      nrounds = nrounds,
      verbose = -1
    )
  }
  
  models
}