# Data Validation on 2025chcmp data

library(scoutR) # devtools::install_github("gkrotkov/scoutR")

data <- readr::read_csv("shinyapp/data_files/dchamp/data.csv")

event_code <- "2025chcmp"

tba <- event_matches(event_code, match_type = "qual")

#########################
#### Coverage Checks ####
#########################

# Do we have data for each team, in each match they were scheduled for?
schedule <- qual_schedule(event_code)
schedule <- schedule |>
    tidyr::pivot_longer(red1:blue3) |>
    dplyr::mutate(
        alliance = substr(name, 1, nchar(name) - 1), 
        team = value
    ) |>
    dplyr::select(
        match_number, alliance, team
    )

############################
#### Correctness Checks ####
############################
