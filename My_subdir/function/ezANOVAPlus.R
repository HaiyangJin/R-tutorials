# create a function called ezANOVAPlus to calculate the partial eta squared and ICs
ezANOVAPlus <- function(df) {
  # This function adds the partial eta squares and CIs to the reuslts of ezANOVA.
  # This function requires library(ez) and library(MBEss).
  
  # calculate the partial eta Squares for the ANOVA results
  df$ANOVA$parEtaSqu <- df$ANOVA$SSn/(df$ANOVA$SSn+df$ANOVA$SSd)
  
  # calculate the CIs
  lowEtaSqu <- c()  # lower eta Squared
  uppEtaSqu <- c()  # upper eta Squared
  for (cR in 1:nrow(df$ANOVA)) {
    Lims <- conf.limits.ncf(F.value = df$ANOVA$F[cR], conf.level = 0.95, 
                            df.1 <- df$ANOVA$DFn[cR], df.2 <- df$ANOVA$DFd[cR])
    Lower.lim <- Lims$Lower.Limit/(Lims$Lower.Limit + df.1 + df.2 + 1)
    Upper.lim <- Lims$Upper.Limit/(Lims$Upper.Limit + df.1 + df.2 + 1)
    if (is.na(Lower.lim)) {Lower.lim <- 0}
    if (is.na(Upper.lim)) {Upper.lim <- 1}
    lowEtaSqu <- c(lowEtaSqu,Lower.lim)
    uppEtaSqu <- c(uppEtaSqu,Upper.lim)
  }
  df$ANOVA$lowEtaSqu <- lowEtaSqu  # partialetasquared.lower
  df$ANOVA$uppEtaSqu <- uppEtaSqu  # partialetasquared.upper
  return(df)
}