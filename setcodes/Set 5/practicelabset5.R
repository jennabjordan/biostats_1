# Laboratory --------------------------------------------------------------
library(tidyverse)

#1
df_h0 <- read_csv("data_raw/data_plant_height.csv")
df_h0

set.seed(100)

#sample of 50
mu_50 <- ub_var50 <- NULL

for (i in 1:100) {
  
  df_50 <- df_h0 %>% 
    sample_n(size = 50) # random samples of 50 individuals
  
  mu_50[i] <- mean(df_50$height)
  ub_var50[i] <- var(df_50$height)
}

print(mu_50)
print(ub_var50)

#sample of 100
mu_100 <- ub_var100 <- NULL

for (i in 1:100) {
  
  df_100 <- df_h0 %>% 
    sample_n(size = 100) # random samples of 100 individuals
  
  mu_100[i] <- mean(df_100$height)
  ub_var100[i] <- var(df_100$height)
}

print(mu_100)
print(ub_var100)

