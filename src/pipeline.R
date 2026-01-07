# src/forecast_quantiles.R
# Forecast recursivo de cuantiles (P10/P50/P90)
# blindado: mismas columnas del training, mismos ordenes

forecast_6m_quantiles <- function(df_hist, qmodels, feature_cols, h = 6) {
  
  df_hist <- df_hist %>%
    dplyr::mutate(
      date    = as.Date(date),
      product = as.character(product),
      y       = as.numeric(y)
    ) %>%
    dplyr::arrange(date)
  
  prod <- df_hist$product[1]
  
  future_dates <- seq.Date(
    from = max(df_hist$date) %m+% lubridate::months(1),
    by = "month",
    length.out = h
  )
  
  out_all <- tibble::tibble(date = as.Date(future_dates))
  
  for (a in names(qmodels)) {
    
    colname <- dplyr::case_when(
      a == "0.1" ~ "P10",
      a == "0.5" ~ "P50",
      a == "0.9" ~ "P90",
      TRUE ~ paste0("P", a)
    )
    
    preds <- numeric(length(future_dates))
    df_ext <- df_hist %>% dplyr::select(date, product, y)
    
    for (j in seq_along(future_dates)) {
      
      d <- as.Date(future_dates[j])
      
      df_tmp <- dplyr::bind_rows(
        df_ext,
        tibble::tibble(date = d, product = prod, y = NA_real_)
      ) %>%
        dplyr::arrange(date)
      
      feat_raw <- make_ts_features_adaptive(df_tmp)
      feat_imp <- impute_features(feat_raw)
      
      feat_row <- feat_imp %>% dplyr::filter(date == d)
      
      if (nrow(feat_row) != 1) {
        stop("feat_row invalida (nrow != 1) para fecha: ", d)
      }
      
      # forzar mismas columnas que el training
      missing_cols <- setdiff(feature_cols, colnames(feat_row))
      if (length(missing_cols) > 0) {
        for (mc in missing_cols) feat_row[[mc]] <- 0
      }
      
      X_new <- feat_row %>%
        dplyr::select(dplyr::all_of(feature_cols)) %>%
        as.matrix()
      
      if (!is.matrix(X_new) || nrow(X_new) != 1 || any(is.na(dim(X_new)))) {
        stop("X_new invalido para fecha: ", d)
      }
      
      yhat <- as.numeric(predict(qmodels[[a]], X_new))
      preds[j] <- yhat
      
      df_ext <- dplyr::bind_rows(
        df_ext,
        tibble::tibble(date = d, product = prod, y = yhat)
      ) %>% dplyr::arrange(date)
    }
    
    out_all[[colname]] <- preds
  }
  
  out_all %>% dplyr::select(date, P10, P50, P90)
}
5) src/pipeline.R (orquestador)
r
Copiar código
# src/pipeline.R
# Pipeline completo por producto (features + wf + entrenar + forecast)

run_product_pipeline <- function(df_long, prod_name, h = 6,
                                 min_train = 18,
                                 params = params_point,
                                 nrounds_wf = 250,
                                 nrounds_point = 300,
                                 nrounds_q = 350) {
  
  df_prod <- df_long %>%
    dplyr::filter(product == prod_name) %>%
    dplyr::select(date, product, y) %>%
    dplyr::mutate(
      date    = as.Date(date),
      product = as.character(product),
      y       = as.numeric(y)
    ) %>%
    dplyr::arrange(date)
  
  df_feat_raw <- make_ts_features_adaptive(df_prod)
  df_feat_imp <- impute_features(df_feat_raw)
  
  feature_cols <- df_feat_imp %>%
    dplyr::select(-date, -product, -y) %>%
    colnames()
  
  wf <- walk_forward_lgb(df_feat_imp,
                         min_train = min_train,
                         params = params,
                         nrounds = nrounds_wf)
  
  model_point <- train_final_lgb(df_feat_imp, params = params, nrounds = nrounds_point)
  qmodels <- train_quantile_models(df_feat_imp, alphas = c(0.1, 0.5, 0.9), nrounds = nrounds_q)
  
  fc_q <- forecast_6m_quantiles(df_prod, qmodels, feature_cols = feature_cols, h = h) %>%
    dplyr::mutate(product = prod_name) %>%
    dplyr::relocate(product, .before = date)
  
  list(
    metrics = wf$metrics,
    errors = wf$errors,
    forecast = fc_q,
    model_point = model_point,
    qmodels = qmodels,
    feature_cols = feature_cols
  )
}