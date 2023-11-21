#' ---
#' title: "Set 13"
#' output: html_document
#' date: 2023-11-14
#' author: Jenna Jordan
#' ---

#+ message = FALSE, warning = FALSE
library(tidyverse)


# In-Class ----------------------------------------------------------------

set.seed(1) #keeps random numbers the same

#generate simulated data
##sample size
n <- 100

## parameters
b <- c(0.1, 0.5)

## hypothetical explanatory variable

x1 <- rnorm(n = n, mean = 0, sd = 1)

##design matrix

X <- model.matrix(~x1) #intercept # place holder and explanatory value

## matrix multiplication (%*%) 

mu <- drop(X %*% b) #expected value of y & drop() is used to vectorize the matrix

#plot(mu ~ x1) 

## create response variable with error

y <- rnorm(n = n, mean = mu, sd = 0.5)

plot(y ~ x1)
abline(0.1, 0.5)

df0 <- tibble(y = y, x1 = x1) 

df0%>%
  ggplot(aes(y = y, 
             x = x1)) +
  geom_point() 

# assess model performace
## R Squared

## true model
m1 <- lm(y ~ x1, data = df0)
summary(m1)

## add useless predictor
df0 <- df0 %>%
  mutate(x2 = rnorm(n = n))

m2 <- lm(y ~ x1 + x2, df0) #r-squared value increases as predictors are added (x2, x3, etc.)
summary(m2)
#use 

# Likelihood Ratio Test (LR)

logLik(m1)
logLik(m2)

#shows comparison between test m1 and test m2
anova(m1, m2, test = "Chisq") #p-value shows adding useless predictor is not significant

m0 <- lm(y ~ 1, data = df0)
m0

anova(m0, m1, test = "Chisq") #p-value is significant so null model (model 1) is rejected

#AIC (model assessment based on model capacity)
## good results = low AIC

AIC(m1)
AIC(m2) #higher because x2 is useless parameter


# Laboratory --------------------------------------------------------------

library(palmerpenguins)
penguins_raw <- read_csv(here::here("data_raw/penguins_raw.csv"))
penguins_raw

#10.3.1 Format Penguin Data
##1 - clean up data set
#make column names lowercase
penguins_raw <- rename_with(penguins_raw, tolower)

#replace white space
colnames(penguins_raw) <- (gsub(" ", "_", colnames(penguins_raw)))

#remove unit notations
#???

##2 - Clutch Completion

penguins_raw$clutch_completion <- ifelse(penguins_raw$clutch_completion == "Yes", 1, 0)

#3
penguins_raw$species[penguins_raw$species == "Adelie Penguin (Pygoscelis adeliae)"] <- "Adelie"
penguins_raw$species[penguins_raw$species == "Chinstrap penguin (Pygoscelis antarctica)"] <- "Chinstrap"
penguins_raw$species[penguins_raw$species == "Gentoo penguin (Pygoscelis papua)"] <- "Gentoo"

#4 - drop NA
penguins_raw <- drop_na(penguins_raw)

#10.3.2 Analyze Penguin Data
  
data <- penguins_raw %>%
  select(species, `culmen_length_(mm)`, `culmen_depth_(mm)`, `flipper_length_(mm)`, `body_mass_(g)`, sex, clutch_completion)


