#' ---
#' title: "Set 6"
#' output: html_document
#' date: 2023-09-19
#' author: Jenna Jordan
#' ---

#+ message = FALSE, warning = FALSE
library(tidyverse)

# 3.1 Continuous Variable ------------------------------------------------

#Probability Density (is y axis)

# load csv data on R
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1) + # specify binwidth
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean


## PDF to Frequency Distribution

# vector of x values
# seq() generate min to max values with specified numbers of elements or interval
# the following produce 100 elements
# not real data, hypothetical based on min and max
x <- seq(min(df_h0$height), max(df_h0$height), length = 100)

# calculate probability density
mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)
#pd = probability density
#dnorm = distribution normal
#x = point at which we want to measure probability density (height of histogram)

# figure
tibble(y = pd, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line() + # draw lines
  labs(y = "Probability Density") # re-label
# y is dnorm or probabiliy density


## convert to ACTUAL probability
# probability of x < 10
p10 <- pnorm(q = 10, mean = mu, sd = sigma)
print(p10)

# probability of x < 20
p20 <- pnorm(q = 20, mean = mu, sd = sigma)
print(p20)

# probability of 10 < x < 20
p20_10 <- p20 - p10
print(p20_10)


##Estimates compare to Frequency Data

x_min <- floor(min(df_h0$height)) # floor takes the integer part of the value
x_max <- ceiling(max(df_h0$height)) # ceiling takes the next closest integer
bin <- seq(x_min, x_max, by = 1) # each bin has 1cm
bin

p <- NULL # empty object for probability
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) - pnorm(bin[i], mean = mu, sd = sigma)
}
#calculates probability for *each* number between min and max (numbers in index)

# data frame for probability
# bin: last element [-length(bin)] was removed to match length since only 35 not 36
# expected frequency in each bin is "prob times sample size"
df_prob <- tibble(p, bin = bin[-length(bin)]) %>% 
  mutate(freq = p * nrow(df_h0))
df_prob
#multiply total number of observations by probability = expected frequency

##overlay -- expectation and observation
df_h0 %>% 
  ggplot(aes(x = height)) +        #makes histogram using df_h0
  geom_histogram(binwidth = 1) +   #using a different data frame: df_prob (points)
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "salmon") +   #adding line to points of expected frequency points
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "salmon")


# 3.2 Discrete Variable -----------------------------------------------------------

#discrete (y axis = actual probability)

df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))
print(df_count)

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # define binwidth
                 center = 0) # relative position of each bin is in center
  #gaps due to discrete data

# vector of x values
# create a vector of 0 to 10 with an interval one
# must be integer of > 0
x <- seq(0, 10, by = 1)
x

# calculate probability mass
lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)
lambda_hat
pm

# figure
tibble(y = pm, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "dashed") + # draw dashed lines
  geom_point() + # draw points
  labs(y = "Probability",
       x = "Count") # re-label


##change y-axis from probability to frequency

df_prob <- tibble(x = x, y = pm) %>% 
  mutate(freq = y * nrow(df_count)) # prob x sample size

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5,
                 center = 0) +
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob,
             aes(x = x,
                 y = freq))


# Laboratory --------------------------------------------------------------

#+ message = FALSE, warning = FALSE
library(tidyverse)

# 3.4.1 ---
set.seed(100)

df <- data.frame(rand.number = rnorm(n = 50, mean = 20, sd = 2))

mu <- mean(df$rand.number)
sigma <- sd(df$rand.number)
pd <- dnorm(x, mean = mu, sd = sigma)

x_min <- floor(min(df$rand.number)) #need floor to define bin by integer
x_max <- ceiling(max(df$rand.number))
bin <- seq(x_min, x_max, by = 1)

p <- NULL

 for(i in 1:(length(bin) - 1)) {
   p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) - pnorm(bin[i], mean = mu, sd = sigma) #calculating area under curve for each index
 }

df_prob <- tibble (p, bin = bin[-length(bin)]) %>%
  mutate(freq = p * nrow(df))

df %>%
  ggplot(aes(x = rand.number)) +
  geom_histogram(binwidth = 1, center = 0) +
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
              color = "deeppink") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "deeppink")

# 3.4.2 ---

pnumber <- rpois(n = 1000, lambda = 5)

lambda_h <- mean(pnumber)


bin_dfp <- seq(floor(min(pnumber)), ceiling(max(pnumber)), by = 1)

pm <- dpois(bin_dfp, lambda = lambda_h)
                
df_prob_p <- tibble(x = bin_dfp, y = pm) %>%
  mutate(freq = y * length(pnumber))

tibble(z = pnumber) %>% 
  ggplot(aes(x = z)) +
  geom_histogram(binwidth = 0.5,
                 center = 0) +
  geom_line(data = df_prob_p,
            aes(x = bin_dfp,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob_p,
             aes(x = bin_dfp,
                 y = freq))
