#' ---
#' title: "Set 9"
#' output: html_document
#' date: 2023-10-12
#' author: Jenna Jordan
#' ---

#+ message = FALSE, warning = FALSE
library(tidyverse)
df_algae <- read_csv(here::here("data_raw/data_algae.csv"))
print(df_algae)

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point(color = "rosybrown4")

#fit linear regression to algae biomass

fit <- lm(biomass ~ conductivity, 
          data = df_algae)

fit
summary(fit)

#estimate parameters
coef(fit)
alpha <- coef(fit)[1]
beta <- coef(fit)[2]

df_algae %>%
  ggplot(aes(y = biomass, 
             x = conductivity)) +
  geom_point(color = "rosybrown4") +
  geom_abline(intercept = alpha, 
              slope = beta,
              color = "snow4")

#get residuals

#vector# y_hat <- alpha + beta * df_algae$conductivity

df_algae <- df_algae %>%
  mutate (y_hat = alpha + beta * conductivity)

#standard error estimate
summary(fit)
theta <- coef(fit)
se <- sqrt(diag(vcov(fit)))
t_value <- theta / se
print(t_value)

#calculate p-value
# for intercept
# (1 - pt(t_value[1], df = 48)) calculates pr(t > t_value[1])
# pt(-t_value[1], df = 48) calculates pr(t < -t_value[1])
p_alpha <- (1 - pt(t_value[1], df = 48)) + pt(-t_value[1], df = 48)

# for slope
p_beta <- (1 - pt(t_value[2], df = 48)) + pt(-t_value[2], df = 48)

print(p_alpha)
print(p_beta)

#residual
eps <- sd(resid(fit))

#Visualization
# add error column
df_algae <- df_algae %>% 
  mutate(eps = eps)

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point(color = "mediumpurple4") +
  geom_abline(intercept = alpha,
              slope = beta,
              color = "mediumpurple4") + 
  geom_segment(aes(x = conductivity, # start-coord x
                   xend = conductivity, # end-coord x
                   y = biomass, # start-coord y
                   yend = biomass - eps), # end-coord y
               linetype = "dashed",
               color = "sienna2")


# Laboratory --------------------------------------------------------------
##6.4.1
iris

df_setosa <- as.data.frame(filter(iris, Species == "setosa"))
df_versicolor <- as.data.frame(filter(iris, Species == "versicolor"))
df_virginica <- as.data.frame(filter(iris, Species == "virginica"))

print(df_setosa)
print( df_versicolor)
print(df_virginica)

#setosa analysis
setosadata <- lm(Sepal.Width ~ Petal.Width, 
          data = df_setosa)

setosadata
summary(setosadata)

#versicolor analysis
versicolordata <- lm(Sepal.Width ~ Petal.Width, 
                 data = df_versicolor)

versicolordata
summary(versicolordata)

#virginica analysis
virginicadata <- lm(Sepal.Width ~ Petal.Width, 
                     data = df_virginica)

virginicadata
summary(virginicadata)


##6.4.2
#additional explanatory variable for each species

#setosa analysis
setosadata2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length, 
                 data = df_setosa)

setosadata2
summary(setosadata)
summary(setosadata2)

#versicolor analysis
versicolordata2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length, 
                     data = df_versicolor)

versicolordata
versicolordata2
summary(versicolordata)
summary(versicolordata2)

#virginica analysis
virginicadata2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length, 
                    data = df_virginica)

virginicadata
virginicadata2
summary(virginicadata)
summary(virginicadata2)

