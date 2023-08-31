#' ---
#' title: "Set 3: R product 2"
#' output: html_document
#' date: 2023-08-31
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



# Extra Exercise Day 2 ----------------------------------------------------------
#Q1: create 10 by 10 matrix, elements may be random numbers
m_1 <- matrix(rnorm(100), nrow = 10, ncol = 10)
m_1

#Q2: calculate mean for each row
meanrow_m_1 <- rowMeans(m_1)
rowMeans(m_1)

#or
other_way_row_mean <- NULL
for (i in 1:10) {
  other_way_row_mean[i] <- mean(m_1[i, ])
}
other_way_row_mean

#Q3: create a tibble with three columns of numbers, and one column of letters (a-z)
# # rows is 100 and letters use sample(letters, 100, replace = TRUE)
library(tidyverse)
df0 <- tibble(x1 = rnorm (100),
       x2 = rpois(100, lambda = 5),
       x3 = rnorm (100),
       letter = sample(letters, 100, replace = TRUE))

#Q4: get sums & number of observations for each letter group
# # use tidyverse function to get them and output must be tibble format
#observation shows how many are in each row
df0 %>%
  group_by(letter) %>%
  summarize(sum_x1 = sum(x1),
            sum_x2 = sum(x2),
            sum_x3 = sum(x3),
            n_observation = n())

#Q5: use pivot_wider
#refer to Tidyverse - reshape section

#Q6: use the following data frame df_na, remove rows with NA
# # use drop_na(x1) in pipe
df_na <- df0 %>%
  mutate(x1 = ifelse(x1 < 0, NA, x1), #overriding x1 column by using if else function
         x2 = ifelse(x2 > 3, NA, x2)) 
print(df_na)

df_na %>%
 drop_na(x1, x2)  #removes row with 'NA' included in data

#ex.
#x <- c(-1, 0, 1)
#ifelse(x < 0, NA, x) 
