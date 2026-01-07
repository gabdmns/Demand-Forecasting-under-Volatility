# src/features.R
# Feature engineering 

make_ts_features_adaptive <- function(df_prod,
                                      lag_set  = c(1, 2, 3, 6, 12),
                                      roll_set = c(3, 6, 12)) {
  
  out <- df_prod %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      date    = as.Date(date),
      product = as.character(product),
      y       = as.numeric(y),
      
      # estacionalidad ciclica
      m     = lubridate::month(date),
      sin_m = sin(2*pi*m/12),
      cos_m = cos(2*pi*m/12)
    )
  
  # lags
  for (k in lag_set) {
    out[[paste0("lag", k)]] <- dplyr::lag(out$y, k)
  }
  
  # rollmeans over lag1 
  if (!"lag1" %in% names(out)) out$lag1 <- dplyr::lag(out$y, 1)
  for (w in roll_set) {
    out[[paste0("roll", w)]] <- zoo::rollmean(out$lag1, w, fill = NA, align = "right")
  }
  
  # protected shocks
  denom_l1 <- out$lag1
  out$shock_1 <- dplyr::if_else(is.na(denom_l1) | denom_l1 == 0, NA_real_,
                                abs(out$y - denom_l1) / abs(denom_l1))
  
  dy <- out$y - denom_l1
  out$shock_3 <- zoo::rollapply(
    dplyr::if_else(is.na(denom_l1) | denom_l1 == 0, NA_real_, abs(dy) / abs(denom_l1)),
    width = 3, FUN = mean, fill = NA, align = "right"
  )
  
  out %>%
    dplyr::select(
      date, product, y,
      sin_m, cos_m,
      dplyr::starts_with("lag"),
      dplyr::starts_with("roll"),
      shock_1, shock_3
    )
}

# Impute  NA

impute_features <- function(df_feat) {
  
  num_cols <- df_feat %>%
    dplyr::select(-date, -product) %>%
    names()
  
  flags <- df_feat %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(num_cols),
                                ~ as.integer(is.na(.)),
                                .names = "{.col}_NA")) %>%
    dplyr::select(date, product, dplyr::ends_with("_NA"))
  
  filled <- df_feat %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(num_cols),
                                ~ dplyr::if_else(is.na(.), 0, .)))
  
  filled %>%
    dplyr::left_join(flags, by = c("date", "product"))
}

prep_train_set <- function(df_feat_imp) {
  df_feat_imp %>% dplyr::filter(!is.na(y))
}