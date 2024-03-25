
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
# this is a process... we'll do as well as we can
# I'm going to build it up step-by-step, and use a github repo to demo the process...





