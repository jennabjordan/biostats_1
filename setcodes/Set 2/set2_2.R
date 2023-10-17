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
movies_2000 <- movies %>%
filter(movies, year >= 2000)

# 1c
movies_sel <- movies %>%
  select(movies, c('title', 'year', 'budget', 'length', 'rating', 'votes'))

# 1d
movies_rename %>%
  colnames(movies)[3] = "length_in_min"

  #or: rename(length_in_min = length)

# Exercise 2 --------------------------------------------------------------

meanbudget <- movies %>%
  group_by(year) %>%
  summarize(budget_m = mean(budget, na.rm = TRUE))

# Exercise 3 --------------------------------------------------------------

longer <- tibble(id = 1:10,
              'x' = rnorm(10),
              'y' = rnorm(10))

longer %>%
  pivot_longer(cols = c(x, y))
               names_to = 'var'
              values_to = 'votes'
print(tidy_dataset)


# Exercise 4 --------------------------------------------------------------

filter(movies, year >= 1990)

select(movies, c('mpaa', 'Action', 'Drama'))

mpaa_action <- movies %>%
  group_by(mpaa, Drama) %>%
  summarize(avg_rating = mean(rating, na.rm = TRUE))
print(mpaa_action)  
