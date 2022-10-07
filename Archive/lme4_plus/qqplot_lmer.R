qqplot_lmer <- function(lmm) {
  # This code aims to run the qqplot for the resisuals of lmm (from lme4)
  # Haiyang Jin (https://haiyangjin.github.io/)
  
  # Usage: qqplot_lmer(lmm)
  
  df <- data.frame(resi = residuals(lmm))
  
  g <- {
    ggplot(df, aes(sample = resi)) +
      stat_qq() +
      stat_qq_line()
  }
  return(g)
}