#' ---
#' title: "Set 10"
#' output: html_document
#' date: 2023-10-19
#' author: Jenna Jordan
#' ---

#+ message = FALSE, warning = FALSE
library(tidyverse)

df_fl <- read_csv(here::here("data_raw/data_fish_length.csv"))
print(df_fl)

m <- lm(length ~ lake,
        data = df_fl)
m

# calculate mean for group a = alpha
mu_a <- mean(df_fl$length[df_fl$lake == "a"])
mu_a

#calculate mean for group b
mu_b <- mean(df_fl$length[df_fl$lake == "b"])
mu_b

#difference between two means = beta
b <- mu_b - mu_a


# T-Test Comparison -------------------------------------------------------
summary(m)

##apply t-test
lakea <- df_fl %>%
  filter(lake == "a") %>%
  pull(length)
lakeb <- df_fl %>%
  filter(lake == "b") %>%
  pull(length)
t.test(lakeb, lakea, var.equal = TRUE)


# ANOVA Comparison --------------------------------------------------------

df_anova <- read_csv(here::here("data_raw/data_fish_length_anova.csv"))

m_anova <- lm(length ~ lake,
   data = df_anova)
summary(m_anova)

df_mu <- df_anova %>%
  group_by(lake) %>%
  summarize(mu = mean(length)) %>%
  arrange(lake) %>%
  mutate(diff = mu - mu[1])

v_mu <- pull(df_mu, mu)
names(v_mu) <- pull(df_mu, lake)
v_mu

# compare with anova
m_aov <- aov(length ~ lake,
    data = df_anova)
summary(m_aov)
summary(m_anova)


# Combine Different Predictors --------------------------------------------

iris <- as_tibble(iris)

m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)
summary(m_iris)
