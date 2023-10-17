#' ---
#' title: "Set 2: R product 2"
#' output: html_document
#' date: 2023-08-24
#' author: Jenna Jordan
#' ---

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2movies)


# Exercise 1 --------------------------------------------------------------

# 1a
movies %>% 
  mutate(centerversion = rating - mean(rating))

# 1b
filter(movies, year >= 2000)

# 1c
select(movies, c('title', 'year', 'budget', 'length', 'rating', 'votes'))

# 1d
colnames(movies)[3] = "length_in_min"


# Exercise 2 --------------------------------------------------------------

meanbudget <- movies %>%
  group_by(year) %>%
  summarize(budget_m = mean(budget, na.rm = TRUE))

# Exercise 3 --------------------------------------------------------------

longer = tibble(movies = 1:10,
              'rating' = rnorm(10),
              'votes' = rnorm(10))

tidy_dataset <- longer %>%
  pivot_longer(cols = c('votes', 'rating'))
    names_to = ''
    values_to = 'votes'
print(tidy_dataset)


# Exercise 4 --------------------------------------------------------------

filter(movies, year >= 1998)

select(movies, c('mpaa', 'Action', 'Drama'))

mpaa_action <- movies %>%
  group_by(mpaa, Drama) %>%
  summarize(avg_rating = mean(rating, na.rm = TRUE))
print(mpaa_action)  
