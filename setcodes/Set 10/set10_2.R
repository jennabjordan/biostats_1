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
m_iris
summary(m_iris)

## Predictions

n_rep <- 100  #number of new data points for prediction
x <- seq(min(iris$Petal.Width), max(iris$Petal.Width), length = n_rep) 

coef(m_iris)[1] + coef(m_iris)[2] * x

new_x <- rep(x, n_distinct(iris$Species)) #repeats x three times (for each species)

new_sp <- rep(unique(iris$Species), each = length(x))

df_pred <- tibble(Petal.Width = new_x,
                  Species = new_sp)

y_pred <- predict(m_iris, newdata = df_pred)


#df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
#                                        max(iris$Petal.Width),
#                                       length = n_rep),
#                                   n_distinct(iris$Species)), #repeats x three times for each species
#                 Species = rep(unique(iris$Species), #makes 100 names for each species                                each = n_rep))  #made 300 predictor data points#

df_pred <- df_pred %>%
  mutate(y_hat = predict(m_iris, newdata = df_pred))

g0 <- iris %>%
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_hat))

# Laboratory --------------------------------------------------------------

## 7.3.1 Normality Assumption ----------------------------------------------

m_residuals <- m_iris$residuals #difference between predicted values and actual values

#or used resid(m_iris) #pulls residual values

shapiro.test(m_residuals) #null = residuals are normally distributed

#plot(m_iris)

## 7.3.2 Model Interpretation ----------------------------------------------------

g0

setosa_int <- m_iris$coefficients[1] 
versicolor_int <- m_iris$coefficients[1] + m_iris$coefficients[3] 
virginia_int <- m_iris$coefficients[1] + m_iris$coefficients[4] 

setosa_int; versicolor_int; virginia_int

## Other Way ---
#b <- coef(m_iris)
#i_setosa <- b[1]
#i_versicolor <- b[1] + b[3]
#i_virginica <- b[1] + b[4]


## 7.3.3 Alternative Model -------------------------------------------------

a_iris <- lm(Petal.Length ~ Petal.Width,
             data = iris)
a_iris
summary(a_iris)

p_rep <- 100
df_p <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                     max(iris$Petal.Width),
                                     length = p_rep))) %>%
  mutate(y_hat = predict(a_iris, newdata = .))


g1 <- iris %>%
  ggplot(aes(x = Petal.Width,
             y = Petal.Length)) +
  geom_point() +
  geom_line(data = df_p,
            aes(y = y_hat)) #redefine y values, if not it will refer to Petal.Length

library(patchwork)
g0 | g1

#assumptions behind models can change your data 
#proper models are critical!

