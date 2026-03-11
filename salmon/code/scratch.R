library(tidyverse)
library(here)

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
