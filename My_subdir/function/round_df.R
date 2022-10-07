# Round all the decimals to N "digits" in a data frame
round_df <- function(df, digits) {
  # check one column is decimal
  nums <- vapply(df, function(x) is.numeric(x) && (x %% 1 != 0), FUN.VALUE = logical(1))
  # round the decimals
  df[,nums] <- round(df[,nums], digits = digits)
  (df)
}