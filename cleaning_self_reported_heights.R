
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

n_samp <- nrow(reported_heights)

output_df <- reported_heights[1:n_samp,] %>% 
  mutate(height = str_replace(height, '\'', '\' ')) %>% 
  mutate(height = str_replace(height, ',', ' ')) %>%  # to handle ',' as foot,inches sep 
  mutate(height = str_replace(height, '  ', ' ')) %>% # let's get spaces as separators
  pull(height) %>% 
  str_split_fixed(., pattern = ' ', n = Inf) %>% # then carve at the spaces
  data.frame() %>% 
  mutate(part_1_num = as.numeric(str_remove_all(X1, pattern = '[^0-9.]'))) %>% 
  mutate(part_2_num = as.numeric(str_remove_all(X2, pattern = '[^0-9.]'))) %>% 
  rename('part_1' = X1, 'part_2' = X2) %>% 
  mutate(height_inches = case_when(
    part_1_num > 48 & part_1_num <= 84 ~ part_1_num,
    part_1_num > 120 & part_1_num <= 215 ~ part_1_num / 2.54,
    part_1_num > 4 & part_1_num <= 7 ~ part_1_num * 12,
    part_1_num > 1.2 & part_1_num <= 2 & is.na(part_2_num) ~ part_1_num * 100 / 2.54, # let's assume a v. small number is m
    TRUE ~ NA
  )) %>% 
  mutate(height_inches = ifelse(part_2 != '' & part_2_num <= 12, height_inches + part_2_num, height_inches)) %>% 
  mutate(decimal_as_inches = part_1_num > 4 & part_1_num <= 7 & (as.character(round((part_1_num - floor(part_1_num)), digits = 2)) %in% as.character(round(seq(0.1, 0.9, by = 0.1), digits = 2)))) %>% # note the ambiguity, is 5.3s a 5'3"? I will assume as much.
  # this is clunky due to floating point
  mutate(height_inches = ifelse(decimal_as_inches == T, floor(part_1_num) * 12 + 10 * (part_1_num %% floor(part_1_num)), height_inches)) %>% # extra step for the decimal_as_inches format
  mutate(orig = reported_heights[1:n_samp, 'height']) %>% 
  mutate(gender = reported_heights[1:n_samp, 'sex']) %>% 
  select(gender, orig, height_inches)
  
# inspect the output to see if it makese sense
# let's list some cases that need to be handled still:
# row 40 is feet.inches (pretty common format I think)... DONE

# let's take a look at the distribution for the first 150 after cleaning, then expand up to more rows...

(mytitle <- str_c('initial n = ', nrow(output_df), sep = ''))
myplot <- output_df %>% 
  ggplot() + theme_bw() +
  geom_jitter(aes(x = height_inches, y = gender, color = gender), width = 0, height = 0.4) +
  labs(x = 'Height (inches)', y = '') +
  ggtitle(mytitle) +
  geom_vline(xintercept = c(54, 84), linetype = 'dotted')
myplot
ggsave(filename = 'cleaned_heights.pdf', plot = myplot, width = 6, height = 4, units = 'in')
# some extreme values... let's continue...

# a few more errors to clean up (below)
# one thing we can see is that we have some really extreme values below 4.5 feet and at 7 feet
# to decide how to handle those, we can look into how probable it is to have this many values at that extreme in a sample this size

output_df %>% as_tibble() %>% select(height_inches, orig) %>% print(n = Inf) # scan for other cases
output_df %>% as_tibble %>% select(height_inches, orig) %>% 
  slice(c(320, 332, 372))

# row 194 5.25 might *actually* be 5 and 1/4 feet 
# row 231, 242, 292 similarly values like 5.5 may not be 5'5", but rather 5'6"

# we assume people don't give their fractional height in feet if the fraction is something harder to intuit, like 10ths or 5ths. but they might give their fractional height in feet if it's x.25, x.5, x.75...

# rows 332, 492, '5.11' should be 71, not 61.3 NOT DONE YET

# row 320 '5.1' should be 61, why is it 61.2 ?? 
# ok, it's a floating point thing, so that may be a poor way to handle. DONE NOW
# row 372 '6.1' should be 73, not 73.2

# row 66 is oddball/uncommon might actually be 5'11"
# row 187 oddball '5 feet and 8.11 inches' not handled
# row 311 is oddball (in words)... these are safely handled as NAs for now, which is good

# row 448 "2'33" is nonsense/shouldn't be handled (should be NA I think) this is a weird one because they might actually be 2 feet + 33 inches which is possible, but still odd and probably shouldn't be handled DONE NOW

# row 363 that person might be 7 foot and that could be real... let's change to >= to be a just bit more permissive for the extreme heights DONE
# similarly, row 376, 214 might be real for cm height (just over 7 feet) DONE NOW

# row 288 '6,8' is not 68, but rather I think it's 6'8" DONE NOW
# and also row 357 '5,4' should be 64 not 54
# and also row 466 '5,8' should be 68 not 58



