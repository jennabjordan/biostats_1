#' ---
#' title: "Set 4"
#' output: html_document
#' date: 2023-09-05
#' author: Jenna Jordan
#' ---

#+ message = FALSE, warning = FALSE
library(tidyr)
library(tidyverse)

# Central Tendencies ------------------------------------------------------

# Arithmetic Mean 
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)
x
y

n_x <- length (x)
sum_x <- sum(x)
mu_x <- sum_x / n_x
print (mu_x)

mu_y <- sum(y) / length(y)
print(mu_y)

print(mu_x)
print(mu_y)

#or

mu_x; mu_y


# Geometric Mean 

prod_x <- prod(x) #product of vector x
n_x <- length(x)
geo_mu_x <- prod_x^(1 / n_x)
print(geo_mu_x)

geo_mu_y <- prod(y)^(1/length(y))
print(geo_mu_y)

geo_mu_x; geo_mu_y


# Median

x <- sort(x)  #small to large
index <- (length(x) + 1) / 2   #length is odd number
med_x <- x[index]
print(med_x)

y <- sort(y)
med_y <- y[(length(y) + 1) / 2]
print(med_y)

print(median(x))
print(median(y))


# Variance ---------------------------------------------------------------

sqd_x <- (x-mean(x))^2  #squared deviance
sqd_x
sum_sqd_x <- sum(sqd_x)
var_x <- sum_sqd_x / length (x)
print(var_x)

var_y <- sum((y-mean(y))^2 / length(y))
print(var_y)

#Standard Deviation

sd_x <- sqrt(var_x)  #square root function = sqrt()
print(sd_x)

sd_y <- sqrt(var_y)
print(sd_y)


# Coefficient of Variation (CV)

cv_x <- sd_x / mean(x)
print(cv_x)

cv_y <- sd_y / mean(y)
print(cv_y)


# IQR
## 25th and 75th percentiles as xl and xh (l = low value, h = high value)

x_l <- quantile(x, 0.25)
x_h <- quantile(x, 0.75)
x_l; x_h
iqr_x <- abs(x_l - x_h)  #abs() = absolute value
print(iqr_x)

y_q <- quantile (y, c(0.25, 0.75))  #use vector to concise code
iqr_y <- abs(y_q[1] - y_q[2])   #[1] = 25th percentile & [2] = 75th
print(iqr_y)


# MAD

ad_x <- abs(x - median(x))   #abs() = absolute value
ad_x
mad_x <- median(ad_x)
print(mad_x)

mad_y <- median(abs(y - median(y)))
print(mad_y)


# MAD/Median

mm_x <- mad_x / median(x)
print(mm_x)

mm_y <- mad_y / median(y)
print(mm_y)


# Laboratory --------------------------------------------------------------

#'
#' title: "Laboratory Exercise: set4_1"
#' author: Jenna Jordan
#'

#1:
z <- exp(rnorm(n=1000, mean = 0, sd = 0.1))
z

#Arithmetic mean
print(mean(z))

#Geometric Mean
geo_mu_z <- prod(z)^(1/length(z))
print(geo_mu_z)

#Median
z <- sort(z)  #smallest to largest
med_z <- z[(length(z) + 1) / 2]
print(med_z)
print(median(z))

#2:
#Histogram
   #use tibble, ggplot, geom_histogram

z <- exp(rnorm(n=1000, mean = 0, sd = 0.1))
df1 <- tibble(z, 1:1000)
df1

ggplot(data = df1,
       mapping = aes(x = z)) + 
geom_histogram(binwidth = 0.04) +
  geom_vline(xintercept = mean(z), linetype = 'dashed', color = 'darkblue') +
  geom_vline(xintercept = prod(z)^(1/length(z)), color = 'purple') +
  geom_vline(xintercept = median(z), color = 'green')

#4:
z_rev <- -z +max(z) + 0.1
print(z_rev)

df2 <- tibble(z_rev)
df2

ggplot(data = df2, 
       mapping = aes(x = z_rev)) +
  geom_histogram(binwidth = 0.03) +
  geom_vline(xintercept = mean(z_rev), linetype = 'dashed', color = 'darkblue') +
  geom_vline(xintercept = prod(z_rev)^1/length(z_rev), color = 'purple') +
  geom_vline(xintercept = median(z_rev), color = 'green')

