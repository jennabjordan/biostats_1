#' ---
#' title: "Set 7"
#' output: html_document
#' date: 2023-09-26
#' author: Jenna Jordan
#' ---

#+ message = FALSE, warning = FALSE
library(tidyverse)

df_fl <- read_csv(here::here("data_raw/data_fish_length.csv"))
print(df_fl)

# unique returns unique values as a vector
## specifying lake column
unique(df_fl$lake)

# distinct returns unique values as a tibble
## finds unique ID & keeps it in tibble form
distinct(df_fl, lake)

# group mean and sd
df_fl_mu <- df_fl %>% 
  group_by(lake) %>% # group operation to compare between a & b
  summarize(mu_l = mean(length), # summarize by mean()
            sd_l = sd(length)) # summarize with sd()

# plot
# geom_jitter() plot data points with scatter
# geom_segment() draw lines
# geom_point() draw points
df_fl %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_jitter(width = 0.1, # scatter width
              height = 0, # scatter height (no scatter with zero)
              alpha = 0.25) + # transparency of data points
  geom_segment(data = df_fl_mu, # switch data frame
               aes(x = lake,
                   xend = lake,
                   y = mu_l - sd_l,
                   yend = mu_l + sd_l)) +
  geom_point(data = df_fl_mu, # switch data frame
             aes(x = lake,
                 y = mu_l),
             size = 3) +
  labs(x = "Lake", # x label
       y = "Fish Body Length") # y label


# Test Statistic ----------------------------------------------------------

# take another look at df_fl_mu
print(df_fl_mu)

# pull mu_l from tibble as vector
v_mu <- df_fl_mu %>% 
  pull(mu_l)

# lake a
print(v_mu[1])

# lake b
print(v_mu[2])

# difference (by 2 centimeters)
v_mu[1] - v_mu[2]

# group mean, variance, and sample size
df_t <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            var_l = var(length), # summarize with sd()
            n = n()) # count number of rows per group

print(df_t)

# pull values as a vector
v_mu <- pull(df_t, mu_l) #mean
v_var <- pull(df_t, var_l) #variance
v_n <- pull(df_t, n) #sample size 

var_p <- ((v_n[1] - 1)/(sum(v_n) - 2)) * v_var[1] +
  ((v_n[2] - 1)/(sum(v_n) - 2)) * v_var[2]

t_value <- (v_mu[1] - v_mu[2]) / sqrt(var_p * ((1 / v_n[1]) + (1 / v_n[2])))

print(t_value)


# Null Hypothesis ---------------------------------------------------------

# produce 500 values from -5 to 5 with equal interval
x <- seq(-5, 5, length = 500) #example of possible fish body size difference

# probability density [represented by dt] of t-statistics with df = sum(v_n) - 2
y <- dt(x, df = sum(v_n) - 2) 

# draw figure (under null hypothesis conditions)
tibble(x, y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  labs(y = "Probability Density",
       x = "T-Statistic")

# draw entire range (shows extreme values)
tibble(x, y) %>% #tibble based on x & y vector
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_vline(xintercept = t_value,
             color = "salmon") + # t_value is the observed t_value
  geom_vline(xintercept = abs(t_value),
             color = "salmon") + # t_value is the observed t_value
  labs(y = "Probability Density",
       x = "t-statistic") 

# calculate area under the curve from -infinity to t_value
pr_below <- pt(q = t_value, df = sum(v_n) - 2)

# calculate area under the curve from abs(t_value) to infinity
pr_above <- 1 - pt(q = abs(t_value), df = sum(v_n) - 2)

# p_value
p_value <- pr_below + pr_above
print(p_value)


# t-test in R -------------------------------------------------------------
df_fl

x <- df_fl %>%
  filter(lake == "a") %>%  # subset lake a
  pull(length)

y <- df_fl %>%
  filter(lake == "b") %>% # subset lake b
  pull(length)

t.test(x, y, var.equal = TRUE)


# Laboratory --------------------------------------------------------------
## 4.4.1
xs <- rnorm(n = 10, mean = 10, sd = 5)
ys <- rnorm(n = 10, mean = 12, sd = 5)

t.test(xs, ys, var.equal = TRUE)

xl <- rnorm(n = 100, mean = 10, sd = 5)
yl <- rnorm(n = 100, mean = 12, sd = 5)

t.test(xl, yl, var.equal = TRUE)

## xs vs ys: p-value = 0.5791 [not stat significant >0.05]
## xl vs yl: p-value = 0.000666 [stat significant <0.05]

## 4.4.2
a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)
b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

df <- data.frame(group = c("a1", "a2", "b1", "b2"),
                 values = c(a1, a2, b1, b2))
print(df)
  
df_musd <- df %>%
  group_by(group) %>%
  summarize(mu = mean(values),
             sd = mean(values))
df_musd


x <- df %>%
  filter(group == "a1") %>%
  pull(values)

y <- df %>%
  filter(group == "a2") %>%
  pull(values)

t.test(x, y, var.equal = FALSE)

x1 <- df %>%
  filter(group == "b1") %>%
  pull(values)

y1 <- df %>%
  filter(group == "b2") %>%
  pull(values)

t.test(x1, y1, var.equal = FALSE)

#a1 vs a2: p-value = 0.07398 [not stat significant > 0.05]
#b1 vs b2: p-value = 0.6531 [not stat significant > 0.05]

## 4.4.3 - Bonus Exercise

a3 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a4 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

mua3 <- mean(a3)
var_a3 <- var(a3)
mua4 <- mean(a4)
var_a4 <- var(a4)
n_a3 <- length(a3)
n_a4 <- length(a4)
n_a <- length(a3) + length(a4)

mua3; mua4; var_a3; var_a4
n_a3; n_a4; n_a

var_a <- ((n_a3 - 1)/(sum(n_a) - 2)) * var_a3 + 
            ((n_a4 - 1)/(sum(n_a) - 2)) * var_a4

t_value_a <- (mua3 - mua4) / sqrt((var_a3 / n_a3) + (var_a4 / n_a4))

num <- (var_a3 / n_a3) +  (var_a4 / n_a4)

d.f.a <- ((num^2) / (var_a3/n_a3) / ((n_a3 - 1) + (var_a4)/n_a4) / (n_a4 - 1))

p_value <- (1 - pt(t_value_a, df = d.f.a)) *2
  

#compare results using formula & code
t_value_a; d.f.a; p_value
t.test(a3, a4)
