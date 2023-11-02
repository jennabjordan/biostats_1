#' ---
#' title: "Set 11"
#' output: html_document
#' date: 2023-10-26
#' author: Jenna Jordan
#' ---

#+ message = FALSE, warning = FALSE
library(tidyverse)

df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))
print(df_count)

#linear model - not the best model
m_normal <- lm(count ~ nitrate, 
               data= df_count)
m_normal

#draw line

coef(m_normal)
a <- coef(m_normal)[1]
b <- coef(m_normal)[2]

df_count %>%
  ggplot(aes(y = count,
             x = nitrate)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = a,
              slope = b) #not a good line of fit

#generalized linear model - poisson distribution

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")
m_pois
summary(m_pois)

theta <- coef(m_pois)
se <- sqrt(diag(vcov(m_pois))) 
z_value <- theta / se

#glm prediction

df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
                                max(df_count$nitrate),
                                length = 100)) %>%
  mutate(y_pois = predict(m_pois, newdata = .) %>%
           exp(),
         y_norm = predict(m_normal, newdata = .))
df_pred

df_count %>%
  ggplot(aes(y = count,
             x = nitrate)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pois),
            color = "red") + #better fitted line
  geom_line(data = df_pred,
            aes(y = y_norm),
            linetype = "dashed")

# Mussel Egg Data

df_mussel <- read.csv(here::here("data_raw/data_mussel.csv")) %>%
  mutate(prop_fert = n_fertilized / n_examined) #proportion of fertilized eggs
df_mussel

#visualize proportion vs density

df_mussel %>%
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() +
  labs(y = "Proportion of Fertilized Eggs",
       x = "Mussel Density",
       title = "Mussel Density Compared to the Proportion of Fertilized Eggs")

# Binomial Distribution 

df_test <- tibble(logit_p = seq(-10, 10, length = 100), #logit_p = log of the parameters; guarantees values are 0.0 to 1.0
                  p = exp(logit_p) / (1 + exp(logit_p)))
df_test

df_test %>%
  ggplot(aes(x = logit_p,
             y = p)) +
  geom_point() +
  geom_line(color = "slateblue1") +
  labs(x = "log(P / 1 - P)",
       y = "P")

#use binomial glm

m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density, #first part is response variable ~ explanatory variable
               data = df_mussel,
               family = "binomial")

m_binom
summary(m_binom)

## make prediction

df_pred <- tibble(density = seq(min(df_mussel$density),
                                max(df_mussel$density),
                                length = 100)) %>%
  mutate(logit_y_hat = predict(m_binom, newdata = .),
          y_hat = exp(logit_y_hat) / (1 + exp(logit_y_hat))) #or use (boot::inv.logit(logit_y_hat))
df_pred

#plot fertilization prop data vs. density
#overlay the predicted values from the model

df_mussel %>%
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() +
  geom_line(data = df_pred, 
            aes(y = y_hat), 
            color = "hotpink") 

#GLM Framework

# Laboratory --------------------------------------------------------------

#+ message = FALSE, warning = FALSE
library(tidyverse)

df_vpart <- read.csv(here::here("data_raw/data_vpart.csv"))
print(df_vpart)

##GLM Exercise

m_glm <- glm(n_sp ~ distance + cat_area + hull_area,
             data = df_vpart,
             family = "poisson")
summary(m_glm)
coef(m_glm) #cannot compare values (km, km^2, etc.) must standardize scale

mean <- mean(df_vpart$n_sp) # only 1 parameter that dictates mean and variance meaning mean = variance
var <- var(df_vpart$n_sp)
mean; var

## standardize unit values to compare explanatory variable values


scaled_d <- scale(df_vpart$distance, scale = TRUE)
   #divides variable by own standard deviation and eliminates unit

#mean(df_vpart$distance)
#z2 <- (df_vpart$distance) / sd(df_vpart$distance)
#sd(z2)

scaled_c <- scale(df_vpart$cat_area, scale = TRUE)

scaled_h <- scale(df_vpart$hull_area, scale = TRUE)


df_vpart <- df_vpart %>%
  mutate(scaled_distance = scaled_d,
         scaled_cat_area = scaled_c,
         scaled_hull_area = scaled_h) 

#approach 2 for this code^ to replace numbers within data.frame
#df_vpart <- df_vpart %>%
  #mutate(across(.cols = c("distance", "cat_area", "hull_area"),
  #.fns = function(x) c(scale(x)))


##GLM using scaled data

scaled_glm <- glm(n_sp ~ scaled_distance + scaled_cat_area + scaled_hull_area,
                  data = df_vpart,
                  family = "poisson")
scaled_glm
summary(scaled_glm)

# data points do not change, only x-axis scale
plot(n_sp ~ distance, df_vpart)
plot(n_sp ~ scaled_distance, df_vpart)

summary(m_glm)
summary(scaled_glm)
#p-value does not change
