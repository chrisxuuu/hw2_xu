# Your Name: Chris Xu
# Course: PHB 228, Assignment 2
# Date: Apr. 9, 2025
# Description: This script provides solutions for Homework 2, focusing on R
# data structures, mapping functions, and version control concepts.

# Part 1: Version Control Setup
# See the Github Repo at: https://github.com/chrisxuuu/hw2_xu.
# Load Libraries.
library(palmerpenguins)
library(purrr)
library(dplyr)
library(ggplot2)

# Part 2: Data Structures
# List Operations.
# Get Unique Species.
unique_species <- unique(penguins$species)
print(unique_species)

# Separate the species by type.
species_dfs <- split(penguins, penguins$species, drop = FALSE)

# Sample Size Attribute.
attributes(species_dfs)$sample_size <- sapply(species_dfs, function(x) nrow(x))

# We can access the sample size attribute by the following:
attributes(species_dfs)$sample_size

# Matrix of numeric measurements for penguins.
penguins_mtx <- as.matrix(penguins[, c(
  "bill_length_mm", "bill_depth_mm",
  "flipper_length_mm", "body_mass_g"
)])
penguins_df <- as.data.frame(penguins[, c(
  "bill_length_mm", "bill_depth_mm",
  "flipper_length_mm", "body_mass_g"
)])
str(penguins_mtx)
# num [1:344, 1:4] 39.1 39.5 40.3 NA 36.7 39.3 38.9 39.2 34.1 42 ...
# - attr(*, "dimnames")=List of 2
#  ..$ : NULL
#  ..$ : chr [1:4] "bill_length_mm" "bill_depth_mm" "flipper_length_mm" "body_mass_g"
str(penguins_df)
#' data.frame':   344 obs. of  4 variables:
# $ bill_length_mm   : num  39.1 39.5 40.3 NA 36.7 39.3 38.9 39.2 34.1 42 ...
# $ bill_depth_mm    : num  18.7 17.4 18 NA 19.3 20.6 17.8 19.6 18.1 20.2 ...
# $ flipper_length_mm: int  181 186 195 NA 193 190 181 195 193 190 ...
# $ body_mass_g      : int  3750 3800 3250 NA 3450 3650 3625 4675 3475 4250 ...

# In the matrix str() output, we can see that the matrix is a 2 dimensional object
# filled with num. Since there are no row names, the first attribute is NULL. The
# second attribute contains the names of the columns from the subsetted original
# dataframe.
# In the dataframe str() output, we can see that it is a data.frame object. The
# dimensions are displayed using obs. and variables counts. Furthermore, the
# difference is that the flipper_length_mm and body_mass_g are stored as integers (int)
# rather than numerics. (num), while bill_length_mm and bill_depth_mm are still numerics.
# This is different from the matrix, where everything is stored as numerics. The
# as.matrix() function coerced everything into numerics.

# If I were to fit a statistical model onto the dataset, I would prefer to use the
# data.frame structure. If I use this dataset to demostrate a mathematical algorithm
# that requires operations such as matrix multiplication, inversion, etc, I would
# use the matrix structure. One operation would be some analysis of population structure
# of the measurements.

# Copy-on-Modify
x <- 1:5
y <- x
y[3] <- 10
print(x)
# [1] 1 2 3 4 5
print(y)
# [1]  1  2 10  4  5

# In R, if we modify y, x remains the same while y changes. If we were to do
# this in Python:
# x = [1, 2, 3, 4, 5]
# y = x
# y[2] = 10
# x
# [1, 2, 10, 4, 5]
# y
# [1, 2, 10, 4, 5]

# Both x and y are changes. Python is able to allow for true references in
# memory, while R copies the variable data and stores them seperately.

# Part 3: Map Functions
# Base R Functions.
# Mean of bill_length_mm, bill_depth_mm, flipper_length_mm, and body_mass_g.
lapply(as.list(penguins_df), function(x) mean(x, na.rm = TRUE))
# Mean of body_mass_g by species.
tapply(penguins, penguins$species, function(x) mean(x$body_mass_g, na.rm = TRUE))
# Mean of body_mass_g by species and sex.
# Filter for penguins without NA for sex.
penguins_complete_sex <- penguins[!is.na(penguins$sex), ]
# Create indicator for species with sex.
penguins_complete_sex$species_sex <- paste(
  penguins_complete_sex$species,
  penguins_complete_sex$sex
)
tapply(
  penguins_complete_sex, penguins_complete_sex$species_sex,
  function(x) mean(x$body_mass_g, na.rm = TRUE)
)
# lapply() outputs a names list with the mean of each column in the data.frame.
# tapply() outputs an numeric array with names stored in the attribute of
# the corresponding type of mean (by species and by species and sex).

# Purrr Map Functions
# Rewrite using map_dbl()
library(dplyr)
# Mean of bill_length_mm, bill_depth_mm, flipper_length_mm, and body_mass_g.
map_dbl(as.list(penguins_df), function(x) mean(x, na.rm = TRUE))
# Mean of body_mass_g by species.
penguins %>%
  split(.$species) %>%
  map_dbl(~ mean(.$body_mass_g, na.rm = TRUE))
# Mean of body_mass_g by species and sex.
penguins %>%
  filter(!is.na(sex)) %>%
  mutate(species_sex = paste(species, sex)) %>%
  split(.$species_sex) %>%
  map_dbl(~ mean(.$body_mass_g, na.rm = TRUE))

# Ratio of bill_length_mm to bill_depth_mm.
lapply(species_dfs, function(species_data) {
  map2(
    species_data$bill_length_mm, species_data$bill_depth_mm,
    function(x, y) x / y
  )
})
# I prefer using the Purrr map functions, especially combined with the dplyr
# pipe. It allows for more readable code. Also, it follows a more natural order of logic
# to me and reduces some verbosity in code.

# Practical Application
# Function to create each histogram.
create_histogram <- function(data, var_name) {
  # Remove NA observations in var_name.
  data <- data[!is.na(data[[var_name]]), ]
  ggplot(data, aes(x = get(var_name), fill = species)) +
    geom_histogram(bins = 20) +
    facet_wrap(~species, ncol = 1) +
    labs(
      title = paste("Histogram of", gsub("_", " ", var_name)),
      x = gsub("_", " ", var_name),
      y = "Count"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}
# Run create_histogram() on each numeric variable (subsetted in penguins_df earlier)
hist_plts <- map(colnames(penguins_df), ~ create_histogram(
  penguins_df %>% mutate(species = penguins$species), .x
))
# Output all 4 combined plots in a 2x2 grid (will be 12 in total)
do.call(gridExtra::grid.arrange, c(hist_plts, ncol = 2))

# The above implementation is more efficient than writing separate ggplot functions
# for each plot because it is much more condense and easier to debug. Since the
# only thing that is changing is the column variable name, there will be less
# lines of duplicated code. Furthermore, if an error occurs (or a need to change
# the plot), we only have to fix the create_histogram() rather than applying the
# same fix manually 4 times.

# Part 4: Memory Management
# Original code
original_func <- function() {
  result <- numeric(0)
  for (i in 1:10000) {
    result <- c(result, i^2)
  }
  return(result)
}
system.time(original_func())
#   user  system elapsed
#  0.090   0.036   0.128
new_func <- function() {
  result <- 1:10000
  result <- result^2
  return(result)
}
system.time(new_func())
#   user  system elapsed
#      0       0       0

# The new function is faster because I used vectorization to square 1:10000 numbers.
# So, it only writes the final result once to the memory, rather than writing to
# the memory after each calculation 10000 times. The line `result <- c(result, i^2)`
# has to write to memory for every number in 1:10000.

# Data Structure Selection
# a. I would use a data.frame to store this data because this may involve different
#    types of data, such as character for patient ID's and numeric for blood pressure
#    regions.
# b. I would use a matrix to store this data. All entries would be numeric, and
#    doing linear algebra, such as finding the inverse of the correlation matrix,
#    will be easier.
# c. I would use a named list to store all the fitted models because statistical
#    models are complex lists with attributes that will only fit in a list of lists
#    while having easy access through double brackets.
# d. I would use a data.frame with columns latitude and longitude to store this
#    data. Each row would represent a coordinate. I use this structure because
#    it would clearly state which numbers correspond to latitude and which to
#    longitude. Furthermore, the restriction of data.frame (each column have the
#    same length) will work in favor as it ensures each coordinate will have both
#    latitude and longitude. Finally, it can be easy to plot using various plotting
#    packages such as ggplot.
