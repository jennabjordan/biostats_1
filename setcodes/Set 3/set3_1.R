#' ---
#' title: "Set 3: R product 1"
#' output: html_document
#' date: 2023-08-29
#' author: Jenna Jordan
#' ---

library(tidyverse)
library(ggplot2movies)
data('movies')


# Point -------------------------------------------------------------------

# basic plot
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)) +
  geom_point()

#to add color by species
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width, 
             color = Species)) +
  geom_point()


# Line --------------------------------------------------------------------

# sample data
df0 <- tibble(x = rep(1:50, 3),
              y = x * 2)

# basic plot
df0 %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
# add points to line using:
  geom_point()

print(df0)

# Histogram ---------------------------------------------------------------

# basic plot; bins = 30 by default
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram()

# change bin width
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5)

# change number of bins
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(bins = 50)


# Boxplot -----------------------------------------------------------------

# basic function for boxplot
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length)) +
  geom_boxplot()

# change fill by "Species"
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot()

# change boarder color
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot(color = "darkgrey")


# Density plot ------------------------------------------------------------

iris %>%
  ggplot(aes(x = Sepal.Length)) +
  geom_density(fill = "salmon",
               color = "salmon",
               alpha = 0.5)

# Facet -------------------------------------------------------------------

#facet_wrap
iris %>%
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)) +
  geom_point() +
  facet_wrap(facets = ~ Species,
             nrow = 2,
             ncol = 2)
#facet_grid: column = site, row = species
iris %>%
  mutate(site = sample(letters[1:2], nrow(iris), replace = TRUE)) %>%
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)) +
  geom_point() +
  facet_grid(rows = vars(Species),
             cols = vars(site)) +
  theme(panel.grid = element_blank())
