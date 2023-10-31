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

df_mussel <- read.csv(here::here("data.raw/data_mussel.csv"))



# Laboratory --------------------------------------------------------------


