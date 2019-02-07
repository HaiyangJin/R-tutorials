round_df <- function (df, decimals = 4) {
  
  numeric_columns <- sapply(df, mode) == 'numeric'
  df[numeric_columns] <-  round(df[numeric_columns], decimals)
  return(df)
} 