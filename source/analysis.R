library(tidyverse)
library(stringr)
library(ggplot2)
library(usmap)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
# total population incarcerated by year
#----------------------------------------------------------------------------#

#df <- get_data()

df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", nrows=-1)

male_jail_pop_2018 <- df %>%
  filter(year == 2018) %>%
  filter(male_jail_pop == max(male_jail_pop)) %>%
  pull(male_jail_pop)

male_juvenile_jail_pop_2018 <- df %>%
  filter(year == 2018) %>%
  filter(male_juvenile_jail_pop == max(male_juvenile_jail_pop)) %>%
  pull(male_juvenile_jail_pop)

female_jail_pop_2018 <- df %>%
  filter(year == 2018) %>%
  filter(female_jail_pop == max(female_jail_pop)) %>%
  pull(median(female_jail_pop))

# I decided to look at how the total male jail population compared to female population.
# I wanted to see how the highest number compared with each other.
# I was also curious to see how the highest juvenile population compared to the total
# and if it was a significant value based on the total.
# There are three different variables created (male_jail_pop_2018, male_juvenile_jail_pop_2018, and female_jail_pop_2018)
# which represent the values mentioned above.


## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# This function creates a data frame that helps look into the U.S prison population
# It is derived from the original data frame from the Vera Institute
# This data frame is grouped by year and then the sum of total jail population is found for each year
# The function returns the adjusted data frame
get_year_jail_pop <- function() {
  get_year_jail_pop_df <- df %>%
    group_by(year) %>%
    summarise(year_jail_population = sum(total_jail_pop, na.rm = TRUE))
return(get_year_jail_pop_df)   
}

# This function plots the year and jail population for each year in a bar chart
# It takes the data frame created in the get_year_jail_pop function and makes a chart
# using ggplot functions
# The function returns the graph made
plot_jail_pop_for_us <- function()  {
  plot_graph <- ggplot(data = get_year_jail_pop()) + 
    geom_col(mapping = aes(x=year, y=year_jail_population)) +
    labs(title = "Increase of Jail Population in U.S (1970-2018)", x = "Year", y = "Total Jail Population",
         caption = "This graph shows the increase (and slight decrease) in jail population from 1970 to 2018 in the United States.")
  return(plot_graph)   
} 

# Graph is plotted
plot_jail_pop_for_us()

# Summary Paragraph: This graphs helps us answer the question:
# What trends are we seeing in terms of the number of people incarcerated within the US over the past 5 decades?
# We can see that the total number of incarcerated individuals in the United States has increased
# significantly since the 1970s. The population seemed to have grown in the 70s and slightly decreased in the 80s.
# However, it picked up rapidly since the 90s and the value has stabalized within the last 5 or so years.

#----------------------------------------------------------------------------#

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

# This function creates a data frame that helps look into the prison population for each state passed in the parameter
# The parameter accepts a vector of state abbreviations that user would like to the data of in the graph
# It is derived from the original data frame from the Vera Institute
# This data frame is grouped by state and then the sum of total jail population is found for each state
# The function returns the adjusted data frame
get_jail_pop_by_states <- function(states) {
  get_jail_pop_by_states_df <- df %>%
#    filter(str_contains(state, states, logic = "or")) %>%
    filter(state == states) %>%
    group_by(year, state) %>%
    summarise(jail_pop_by_states = sum(total_jail_pop, na.rm = TRUE)) %>%
  return(get_year_jail_pop_df)  
  
}

# This function plots the state and jail population for each year in a line graph
# It will create a different line (with a different color) for each state passed in the parameter
# The parameter accepts a vector of state abbreviations that user would like to the data of in the graph
# It takes the data frame created in the get_jail_pop_by_states function and makes a chart
# using ggplot functions
# The function returns the graph made
plot_jail_pop_by_states <- function(states) {
  plot_graph <- ggplot(data = get_jail_pop_by_states(states), aes(x = year, y = jail_pop_by_states, color = state)) + 
    geom_line() +
    geom_point() +
    labs(title = "Growth of Prison Population by State (1970-2018)", x = "Year", y = "Jail Population",
         caption = "This graph shows trend in jail population from 1970 to 2018 for each state passed in the vector")
  return(plot_graph)
}

data <- get_jail_pop_by_states(c("WA", "OR", "CA"))

# Graph is plotted
plot_jail_pop_by_states(c("WA", "OR", "CA"))

# Summary Paragraph: This graphs helps us answer the question:
# What trends are we seeing in terms of the number of people incarcerated in US states the past 5 decades?
# It seems states relatively follow the same trend as the US: Increase in the 70s, slight decrease in the 80s
# rapid growth until stable level in 2010s. However, it is important to note that states with a higher population
# showed this tend at a more extreme level compared to states with a smaller population. 


## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>

# This function creates a data frame that helps look into any sex-based discrimination in jail across the US in 2018
# It is derived from the original data frame from the Vera Institute
# This data frame is filtered by state and then mutated to find
# the number of males and females that are incarcerated based on the population of the county
# The function returns the adjusted data frame
get_jail_prop_by_sex <- function() {
  get_jail_prop_by_sex_df <- df %>%
    filter(year == 2018) %>%
    mutate(jail_prop_male = male_jail_pop / male_pop_15to64) %>%
    mutate(jail_prop_female = female_jail_pop / female_pop_15to64) %>%
  return(get_year_jail_pop_df)  
  
}


# This function plots the proportion of males incarcerated by county population
# to proportion of females incarcerated by county population in a scatter plot
# It takes the data frame created in the get_jail_prop_by_sex function and makes a plot
# using ggplot functions
# The function returns the graph made
plot_ail_prop_by_sex <- function() {
  plot_graph <- ggplot(data=get_jail_prop_by_sex()) + 
    geom_point(mapping = aes(x=jail_prop_male, y=jail_prop_female)) +
    labs(title = "Proportion of Males Incarcerated vs Females by County (2018)", x = "Proportion of Males Incarcerated", y = "Proportion of Females Incarcerated",
         caption = "This graph shows the proportion of males incarcerated compared to females in 2018 for each county")
  return(plot_graph)
  
}

# Graph is plotted
plot_ail_prop_by_sex()

# Summary Paragraph: This graphs helps us answer the question:
# Are we seeing any inequalities in terms of the proportion of males being incarcerated compared to females in 2018?
# We can clearly see that in most cases, the proportion of males being incarcerated seems to be higher than the number of females
# This shows that there does, in fact, seem to be a difference
# However, this data is based on a few assumptions
# such as males and females are involved in criminal activity at the same rate.
#----------------------------------------------------------------------------#



## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>

# This function creates a data frame that helps look into race (specifically black) discrimination in jail in US States in 2018
# It is derived from the original data frame from the Vera Institute
# This data frame is filtered by year (2018) and grouped by state
# to find proportion of blacks in jail (using the summarise function)
# The function returns the adjusted data frame
get_prop_black_jail_state_2018 <- function() {
  get_prop_black_jail_state_2018_df <- df %>%
    filter(year == 2018) %>%
    group_by(state) %>%
    summarise(prop_black_jail_state_2018 = sum(black_jail_pop, na.rm = TRUE) / sum(black_pop_15to64, na.rm = TRUE)) %>%
  return(get_prop_black_jail_state_2018_df)   
}

# This function shows the proportion of blacks incarcerated by state population through a map
# It takes the data frame created in the get_prop_black_jail_state_2018 function and makes a colored (blue) map
# using ggplot functions
# The function returns the graph made
plot_prop_black_jail_state_2018 <- function()  {
  plot_graph <- plot_usmap(data = get_prop_black_jail_state_2018(), values = "prop_black_jail_state_2018", color = "blue") + 
    scale_fill_continuous(
      low = "white", high = "blue", name = "Proportion of Incarcerated Blacks", label = scales::comma) +
    labs(title = "Proportion of Incarcerated Blacks by State (2018)",
         caption = "This graph shows the proportion of incarcerated Blacks by state in 2018") +
#    theme_minimal()
    theme(legend.position = "right")
return(plot_graph)
}

# Graph is plotted
plot_prop_black_jail_state_2018()

# Summary Paragraph: This graphs helps us answer the question:
# Are we seeing any inequalities in terms of the proportion of blacks being incarcerated by state recently?
# In other words, what trends are we seeing in terms of the number of black people incarcerated in US states in 2018?
# This graph shows that there is a disparity between states in terms of number of black individuals being incarcerated.
# We can see that Wyoming is incarcerating more Black individuals compared to other states.
# Montana, Louisiana, Kentucky, and West Virginia seem to be high as well.


#----------------------------------------------------------------------------#

## Load data frame ---- 


