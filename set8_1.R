#' ---
#' title: "Set 8"
#' output: html_document
#' date: 2023-10-3
#' author: Jenna Jordan
#' ---

#+ message = FALSE, warning = FALSE
library(tidyverse)
df_anova <- read_csv(here::here("data_raw/data_fish_length_anova.csv"))

distinct(df_anova, lake)

df_anova %>%
  distinct(lake)

unique(df_anova$lake)

df_anova %>%
  ggplot(aes(x = lake,
             y = length)) +
  geom_violin(draw_quantiles = 0.5,
              fill = "cyan",
              alpha = 0.6) +
  geom_jitter(width = 0.2, 
              height = 0, 
              alpha = 0.5)

#anova in R

fit <- aov(formula = length ~ lake, 
    data = df_anova)
fit
summary(fit)

#whole mean

mu <- mean(df_anova$length)

#behind anova

x_a <- df_anova %>%
  filter (lake == "a") %>%
  pull(length)

x_b <- df_anova %>%
  filter (lake == "b") %>%
  pull(length)

x_c <- df_anova %>%
  filter (lake == "c") %>%
  pull(length)

mu_a <- mean(x_a)
mu_b <- mean(x_b)
mu_c <- mean(x_c)

mu_a; mu_b; mu_c

## between-group variability
#calculate sum squared

ss_ga <- (mu_a - mu)^2 * length(x_a)
ss_gb <- (mu_b - mu)^2 * length(x_b)
ss_gc <- (mu_c - mu)^2 * length(x_c)

ss_ga; ss_gb; ss_gc

ss_g <- ss_ga + ss_gb + ss_gc
ss_g

#tidyverse language
# estimate overall mean
mu <- mean(df_anova$length)

# estimate group means and sample size each
df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length), # mean for each group
            dev_g = (mu_g - mu)^2, # squared deviation for each group
            n = n()) # sample size for each group

print(df_g)

ss_g <- df_g %>% 
  mutate(ss = dev_g * n) %>%
  pull(ss) %>%
  sum()

print(df_g)

## within-group variability

dev_ia <- (x_a - mu_a)^2

dev_ib <- (x_b - mu_b)^2

dev_ic <- (x_c - mu_c)^2

#sum i

sum(dev_ia)
sum(dev_ib)
sum(dev_ic)

#summation across groups

ss_w <- sum(dev_ia) + sum(dev_ib) + sum(dev_ic)

#tidyverse version of within-group
df_i <- df_anova %>% 
  group_by(lake) %>% 
  mutate(mu_g = mean(length)) %>% # use mutate() to retain individual rows
  ungroup() %>% 
  mutate(dev_i = (length - mu_g)^2) # deviation from group mean for each fish
df_i

#convert variability to SD
var_g <- ss_g / (3-1) #3 lakes minus 1
nrow(df_anova) #number of total individuals - number of groups
var_w <- ss_w / (150 - 3)

f_value <- var_g / var_w #calculation for f value

ss_g; ss_w
var_g; var_w
f_value
summary(fit)

# f distribution

x <- seq(0, 10, by = 0.1)
y <- df(x, df1 = 3-1, df2 = 150 -3) #df1 = df of groups, #df2 = df of individuals

tibble(x = x, y = y) %>% 
  ggplot(aes(x = x,
             y = y)) + 
  geom_line() + # F distribution
  geom_vline(xintercept = f_value, # observed F-statistic
             color = "darkgreen",
             linetype = "dashed") 

# pf() estimate the probability of less than q
# Pr(F0 > F) is 1 - Pr(F0 < F)
p_value <- 1 - pf(q = f_value, df1 = n_g - 1, df2 = nrow(df_anova) - n_g)
print(p_value)
