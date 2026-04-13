library("RODBC")
library("tidyverse")

# Functions -------------

match_LAD_runID <- function(data, date, size) {
  result <- data %>%
    filter(
      yday == !!date,
      min <= !!size,
      max >= !!size
    ) %>%
    pull(runID)
  if (length(result) == 0) {
    return(NA)
  } else {
    return(result)
  }
}

# Load files ---------------
lad_model <- read_tsv("data/delta_lad_long_WY2024 1 (1).txt") %>%
  mutate(
    runID = str_extract(cohort, "^[a-zA-Z]+"),
    year = 2024, # set all year fields to 2024, note year is not used for lookup
    date = `year<-`(date, 2024),
    yday = yday(date)
  )

# Connect to Microsoft Access DB
salvage_db <- "data/Salvage_data_FTP.accdb"
channel <- odbcConnectAccess2007(salvage_db)

unmarked_query <- "
SELECT
  s.SampleDate,
  s.SampleTime,
  d.IDNumber,
  l.ForkLength,
  l.LengthFrequency,
  d.DNA_Run,
  d.Probability,
  b.BuildingCode
FROM
  (((Length l
  LEFT JOIN DNAandCWTRun AS d ON l.LengthRowID = d.LengthRowID)
  LEFT JOIN Catch AS c ON c.CatchRowID = l.CatchRowID)
  LEFT JOIN Building AS b ON b.BuildingRowID = c.BuildingRowID)
  LEFT JOIN Sample AS s ON s.SampleRowID = b.SampleRowID
WHERE
  l.AdiposeClip = 0 AND
  c.OrganismCode = 1 AND
  d.CWT_Run IS NULL
"



unmarked_chinook <- sqlQuery(channel, unmarked_query) %>%
  as_tibble() %>%
  mutate(
    datetime = paste(SampleDate, SampleTime),
    SampleTime = lubridate::ymd_hms(datetime, tz = "America/Los_Angeles"),
    facility = case_when(
      BuildingCode == "NS" ~ "SWP",
      BuildingCode == "OS" ~ "SWP",
      BuildingCode == "F" ~ "CVP",
      TRUE ~ "Unknown"
    ),
    yday = yday(SampleTime),
    LAD = map2_chr(yday, ForkLength, ~match_LAD_runID(lad_model, .x, .y))
  )

wr_lad <- unmarked_chinook %>%
  filter(
    !is.na(DNA_Race),
    LAD == "w"
  ) %>%
  mutate(
    year = year(datetime),
    water_year = ifelse(month(datetime) >= 10, year(datetime) + 1, year(datetime)),
  ) %>%
  group_by(water_year) %>%
  summarize(
    total_wrlad = n(),
    genetic_wrlad = sum(DNA_Race == "W", na.rm = TRUE)
  ) %>%
  mutate(
    percent_genetic_wrlad = genetic_wrlad / total_wrlad * 100,
  )
wr_lad_plottable <- wr_lad %>%
  rename(
    LAD = total_wrlad,
    Genetic = genetic_wrlad
  ) %>%
  select(-percent_genetic_wrlad) %>%
  pivot_longer(
    cols = c(LAD, Genetic),
    names_to = "Run ID Method",
    values_to = "count"
  )

wr_lad_bar_chart <- ggplot(
  data = wr_lad_plottable,
  aes(x = factor(water_year), y = count, fill = factor(`Run ID Method`))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Putative Winter Chinook Salvaged by Water Year",
    x = "Water Year",
    y = "Count",
    fill = "Run ID Method"
  ) +
  scale_fill_manual(values = c("LAD" = "blue", "Genetic" = "orange")) +
  theme_classic()
wr_lad_bar_chart
ggsave(
  filename = "figures/wr_lad_bar_chart.png",
  plot = wr_lad_bar_chart,
  width = 5,
  height = 3,
  dpi = 300
  )
# Close the ODBC connection
odbcClose(channel)
