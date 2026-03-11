# load libraries
library(tidyverse)
library(here)

# create a list to loop through
dfs <- list(
  trawl_76_01 = dt1,
  trawl_02_25 = dt2,
  seine_76_25 = dt3,
  taxonomy  = dt4,
  sample_sites = dt5
)

for (nm in names(dfs)) {
  write.csv(dfs[[nm]],
            # may need to change this path eventually
            file = paste0("salmon/data/raw/", nm, ".csv"),
            row.names = FALSE)
}

# these data files are all too large to save on git so it should be ignored and only saved locally
