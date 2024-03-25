
library(tidyverse)
library(dslabs)

# From: Intro to Data Science
# by Rafael A. Irizarry
# http://rafalab.dfci.harvard.edu/dsbook-part-1/

### Section 17 String Processing ####

# In section 17.7, we deal with a dataset of self-reported heights collected from a sample of students:
heights %>% as_tibble() # cleaned version of the data
reported_heights %>% as_tibble() # the original survey data (pre-cleaning)
# note that the cleaned version has had some of the rows removed already
# the original survey data has the height info stored within strings
# different respondents (students) formatted/entered their height differently... we have heterogeneity in units, format/punctuation, as well as obvious errors etc 

# In this script, we'll use stringr functions to clean up reported_heights
# the desired output is numerical data on each student's height, in consistent measurement units (let's go w/ inches), with values we know are impossible replaced w/ NA

# This is a process... we'll do as well as we can!

# I'm going to build it up step-by-step, and use a github repo to demo the process...

reported_heights$height
# To start, let's build something that handles the diversity of cases we get in a small subset, say, the first 150 entries.
reported_heights$height[1:150]

# Can we describe some of the heterogeneity here?
# 1. whole (integer) numbers between XX-XX that can be assumed as human heights in inches. what XX-XX? 48-84 seems reasonable (4 feet - 7 feet)
# 2. whole (integer) numbers that are between 4-7, that we assume are heights given in feet
  # 2b. decimal numbers between 4-7, that we assume are `feet.inches`
  # 2c. numbers with an intervening space, and the first one is is an integter between 4-7, that we assume are `feet inches`
# 3. decimal numbers that are between 48-84, that we assume are given in inches
# 4. whole (integer) or decimal numbers that are between 120ish - 210ish, that we assume are heights in cm
# 5. really big numbers that we assume are errors
# 6. numbers with ' separator that can be converted to feet and inches
  # 6b. numbers with , separator that can be converted to feet and inches
# ...

reported_heights[1:150,]






