#' ---
#' title: "Set 12"
#' output: html_document
#' date: 2023-11-09
#' author: Jenna Jordan
#' ---

#+ message = FALSE, warning = FALSE
library(tidyverse)

#calculate the probability of y = 3
#with lambda = 3.5

dpois(3, lambda = 3.5)

#use formula to confirm
(p <- (3.5^3 * exp(-3.5)) / factorial(3))

#possible lambda values
lambda <- seq(0, 10, by = 0.01)

df_pr <- tibble(y = 3,
                lambda = lambda,
                pr = dpois(3, lambda = lambda)) #pr = probability
df_pr

#draw figure, y = pr, x = lambda 
## shows what value of lambda best represents the data

df_pr %>%
  ggplot(aes(x = lambda,
         y = pr)) +
  geom_line() + 
  labs(y = "Pr(k = 3")

#find best value
df_pr %>%
  arrange(desc(pr)) #arranges values to show highest value (at peak)
                    #best value is when lambda = 3

#calculate probability of observing y = c(3, 2, 5)

pr <- dpois(c(3, 2, 5), lambda = 3) #probability of observing each data point seperately 

prod(pr) #likelihood of observing all three data points

# find the best lambda for y = c(3, 2, 5)

likelihood <- NULL
for (i in 1:length(lambda)) {
  pr0 <- dpois(c(3, 2, 5), lambda = lambda[i])
  likelihood[i] <- prod(pr0)
}


df_pr2 <- tibble(lambda = lambda, pr = likelihood) 
df_pr2

df_pr2 %>%
  ggplot(aes(x = lambda,
             y = pr)) +
  geom_line() +
  geom_point(y = 0.00534, x = 3.33)

df_pr2 %>%
  arrange(desc(pr))

#find the pest value of lambda using df_pr2


df_pr2 %>%
  arrange(desc(pr)) %>%
  slice(1) %>%    #retrieves row 1
  pull(lambda)

mean(c(3, 2, 5))


# Laboratory --------------------------------------------------------------

#9.3.1 Binomial Distribution

y <- c(2, 2, 0, 0, 3, 1, 3, 3, 4, 3)

p <- seq(from = 0, to = 1, by = 0.01)

N <- 10

for(i in 1:length(p)) {
  lh[i] <- prod(dbinom(y, prob = p[i], size = 10))
}

df_lh <- tibble(p = p, lh = lh)

df_lh %>%
  ggplot(aes(x = p,
             y = lh)) +
  geom_line() +
  geom_point(x = 0.21, y = 0.0000000421)

p_best <- df_lh %>%
  arrange(desc(lh)) %>%
  slice(1) %>%
  pull(p)

## sapply version (can also use)
##lh0 <- sapply(p, function(x) prod(dbnom(y, prob = x, size = 10)))

N * p_best
mean(y)

