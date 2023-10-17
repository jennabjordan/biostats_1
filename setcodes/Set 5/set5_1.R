#' ---
#' title: "Set 5"
#' output: html_document
#' date: 2023-09-12
#' author: Jenna Jordan
#' ---

#+ message = FALSE, warning = FALSE
library(tidyr)
library(tidyverse)


# 2.1 ---------------------------------------------------------------------

h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h1 <- tibble(plant_id = 1:10, # a vector from 1 to 10 by 1
                height = h, # height
                unit = "cm") # unit
df_h1

# nrow() returns the number of rows
# i.e., nrow(.) counts the number of rows in df_h1
df_h1 <- df_h1 %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h1)

#different set of 10
h <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)


# 2.2 ---------------------------------------------------------------------

# load csv data on R
df_h0 <- read_csv("data_raw/data_plant_height.csv")
  #hypothetical true garden data (not possible in real life)

# show the first 10 rows
print(df_h0)

#Summary Statistics
mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0) #variation

print(mu)
print(sigma2) #variation

#subsample within dataset
df_i <- df_h0 %>% 
  sample_n(size = 10) # size specifies the number of rows to be selected randomly

print(df_i)

#attempt 2
df_i <- df_h0 %>% 
  sample_n(size = 10)

print(df_i)

# subsample replicates (for reproducibility)
set.seed(3)

mu_i <- var_i <- NULL # create empty objects or box

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
}
print(mu_i)
print(var_i)


#histogram

library(patchwork)

df_sample <- tibble(mu_hat = mu_i, var_hat = var_i)

# histogram for mean
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

# histogram for variance
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

# layout vertically
# possible only if "patchwork" is loaded
g_mu / g_var


#use to remove bias
set.seed(3)

# redo simulations ---- "ub = un-bias"
mu_i <- var_i <- var_ub_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
  var_ub_i[i] <- var(df_i$height)
}

#create un-bias histograms
# draw histograms ----
df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i,
                    var_ub_hat = var_ub_i)

# histogram for mu
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

# histogram for variance
# scale_x_continuous() adjusts scale in x-axis
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

# histogram for unbiased variance
g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

g_mu / g_var / g_var_ub

# Laboratory --------------------------------------------------------------

#1
df_h0 <- read_csv("data_raw/data_plant_height.csv")
df_h0

#Sample Size 50
mu_50 <- ub_var50 <- NULL

for (i in 1:100) {
  
  df_50 <- df_h0 %>% 
    sample_n(size = 50) # random samples of 10 individuals
  
  mu_50[i] <- mean(df_50$height)
  ub_var50[i] <- var(df_50$height)
}

print(mu_50)
print(ub_var50)

#Sample Size 100
mu_100 <- ub_var100 <- NULL

for (i in 1:100) {
  
  df_100 <- df_h0 %>% 
    sample_n(size = 100) # random samples of 100 individuals
  
   mu_100[i] <- mean(df_100$height)
   ub_var100[i] <- var(df_100$height)
}

print(mu_100)
print(ub_var100)

#Histograms
library(patchwork)

df_50_histogram <- tibble(mu_hat_50 = mu_50, ubvar_hat_50 = ub_var50)

# histogram for mu_50 & ub_var50
g_mu_50 <- df_50_histogram %>% 
  ggplot(aes(x = mu_hat_50)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

g_var_50 <- df_50_histogram %>% 
  ggplot(aes(x = ubvar_hat_50)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)


# histogram for mu_100 & ub_var100

df_100_histogram <- tibble(mu_hat_100 = mu_100, ubvar_hat_100 = ub_var100)

g_mu_100 <- df_100_histogram %>% 
  ggplot(aes(x = mu_hat_100)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

g_var_100 <- df_100_histogram %>% 
  ggplot(aes(x = ubvar_hat_100)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

#2
df_h10 <- df_h0 %>% 
  filter(height >= 10)

#Sample Size 50
mu1_50 <- ub1_var50 <- NULL

for (i in 1:100) {
  
  dfh10_50 <- df_h10 %>% 
    sample_n(size = 50) # random samples of 10 individuals
  
  mu1_50[i] <- mean(dfh10_50$height)
  ub1_var50[i] <- var(dfh10_50$height)
}

print(mu1_50)
print(ub1_var50)

#Sample Size 100
mu1_100 <- ub1_var100 <- NULL

for (i in 1:100) {
  
  dfh10_100 <- df_h10 %>% 
    sample_n(size = 100) # random samples of 100 individuals
  
  mu1_100[i] <- mean(dfh10_100$height)
  ub1_var100[i] <- var(dfh10_100$height)
}

print(mu1_100)
print(ub1_var100)
