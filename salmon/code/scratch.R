library(tidyverse)
library(here)


# seine data are in long format, including all fish spp...I want to retain that long
# format, sum CHN counts for each seine--including 0's when no CHN are caught in
# a given seine

long <-
  tibble(sample_id = c(1,1,1,2,2,3,3,3),
         temp = c(20, 20, 20, 18, 18, 19, 19, 19),
         organism_code = c("CHN", "CHN", "SACPIK", "SACPIK", "SACPIK", "WESMOS", "SACPIK", "CHN"),
         count = c(2,1,1,1,3,6,1,1)) %>%
  print()

wider <-
  long %>% pivot_wider(
  # group by and retain 'temp'
  id_cols = c(sample_id, temp),
  # name new columns
  names_from = organism_code,
  # values from match
  values_from = count,
  # value if no match
  values_fill = 0,
  # add up multiple matches
  values_fn = sum) %>%
  print()

# note that this 'long' version retains the counts (including 0's) for all spp in each sample_id
long_again <-
  wider %>%
  pivot_longer(
    cols = CHN:WESMOS,
    names_to = "organism_code",
    values_to = "count"
  ) %>%
  print()

Petes_data <- here("~/Library/CloudStorage/OneDrive-CaliforniaDepartmentofWaterResources/3-Projects/09b-first flush/data files PN")

# standardize function -----------
standardize <- function(x, center = FALSE, norm = FALSE) {
  if(!is.numeric(x)) {
    stop('ERROR: x must be numeric')
  }
  if(center) {
    x <- x - mean(x)
  }
  if(norm) {
    x <- x/sd(x)
  }
  return(x)
}

test_df <- c(10, 12, 15, 18, 20)
standardize(test_df) # leaves original unchanged bc NULL = FALSE
standardize(test_df, TRUE, TRUE) # test_df centered and normalized

test2_df <- c(10, 12, 15, 18, "wonky")
standardize(test2_df) # leaves original unchanged bc NULL = FALSE
standardize(test2_df, TRUE, TRUE)

# using 'ifelse' (genus for falcon is 'Falco')
isfalcon <-
  ifelse(speciestable$genus == 'Falco',
         'falcon',
         'not a falcon'
         )

dat <- data.frame(x = 1, y = 2)


# Load the necessary library
library(dplyr)

# Create a sample tibble
data <- tibble(
  id = 1:4,
  value = c(10, 20, 30, 40)
)

# Use transmute() to create a new variable 'value_doubled' and keep only it and 'id'
# (Note: if 'id' wasn't included in the function call, it would also be dropped)
result_transmute <- data %>%
  transmute(
    id = id,             # Keep the 'id' variable as is
    value = value * 2    # Create a new 'value' and drop the old one
  )

# Print the results
print(data)
print(result_transmute)
