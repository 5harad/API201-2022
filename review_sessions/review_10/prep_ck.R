# Prepare Card and Krueger data

# Load packages
library(tidyverse)
library(haven)

# Download data
download.file("http://davidcard.berkeley.edu/data_sets/njmin.zip", 
              destfile = "data/njmin.zip")

unzip("data/njmin.zip", exdir = "data")
codebook <- read_lines(file = "data/codebook")

variable_names <- codebook %>%
   `[`(8:59) %>%
   `[`(-c(5, 6, 13, 14, 32, 33)) %>%
   str_sub(1, 13) %>%
   str_squish() %>%
   str_to_lower()

dataset <- read_table("data/public.dat", col_names = FALSE)

dataset <- dataset %>%
   select(-X47) %>%
   `colnames<-`(., variable_names) %>%
   mutate_all(as.numeric) %>%
   mutate(sheet = as.character(sheet))

# Clean
fastfood <- dataset %>% 
   mutate(restaurant_id = row_number(),
          fte_pre = empft + nmgrs + (0.5 * emppt),
          fte_post = empft2 + nmgrs2 + (0.5 * emppt2),
          state = ifelse(state == 0, "PA", "NJ"),
          ) %>% 
   select(restaurant_id, state, fte_pre, fte_post) 
   
# Save data
write_dta(fastfood, "fastfood.dta")

