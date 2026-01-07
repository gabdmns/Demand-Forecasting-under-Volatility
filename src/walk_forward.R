# src/walk_forward.R
# Walk-forward (expanding window) + metricas

walk_forward_lgb <- function(df_feat_imp, min_train = 18, params, nrounds = 250) {
  
  df_tr <- prep_train_set(df_feat_imp)
  n <- nrow(df_tr)
  
  if (n <= (min_train + 1)) {
    stop("Muy pocos datos tras features. Reduce min_train o reduce lags/rolls.")
  }
  
  errors <- tibble::tibble()
  
  for (i in seq(min_train, n - 1)) {
    
    train_i <- df_tr[1:i, ]
    test_i  <- df_tr[i + 1, ]
    
    X_train <- as.matrix(train_i %>% dplyr::select(-date, -product, -y))
    y_train <- train_i$y
    
    X_test  <- as.matrix(test_i %>% dplyr::select(-date, -product, -y))
    y_test  <- test_i$y
    
    dtrain <- lightgbm::lgb.Dataset(data = X_train, label = y_train)
    
    model_i <- lightgbm::lgb.train(
      params  = params,
      data    = dtrain,
      nrounds = nrounds,
      verbose = -1
    )
    
    pred <- as.numeric(predict(model_i, X_test))
    
    errors <- dplyr::bind_rows(
      errors,
      tibble::tibble(date = as.Date(test_i$date), actual = y_test, pred = pred)
    )
  }
  
  metrics <- c(
    RMSE = Metrics::rmse(errors$actual, errors$pred),
    MAE  = Metrics::mae(errors$actual, errors$pred),
    MAPE = Metrics::mape(errors$actual, errors$pred)
  )
  
  list(errors = errors, metrics = metrics)
}