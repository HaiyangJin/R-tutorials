# Create a function called ezStatsPlus to calculate the SE, CI, hi(gh), lo(w).
ezStatsPlus <- function(df){
  df %>%
    mutate(SE = SD / sqrt(N)) %>%  # add standard error
    mutate(hi = Mean + SE) %>% #
    mutate(lo = Mean - SE) %>%
    mutate(CI = SE * qt(0.975,N)) %>%  # calculation 1 
    mutate(ezCI = FLSD / sqrt(2))   # calculation 2
}