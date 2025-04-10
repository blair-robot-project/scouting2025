# Data Validation on 2025chcmp data

library(scoutR) # devtools::install_github("gkrotkov/scoutR")

data <- readr::read_csv("shinyapp/data_files/dchamp/data.csv")

event_code <- "2025chcmp"

tba <- event_matches(event_code, match_type = "qual")

#########################
#### Coverage Checks ####
#########################

# Do we have data for each team, in each match they were scheduled for?
schedule <- qual_schedule(event_code) |>
    tidyr::pivot_longer(red1:blue3) |>
    dplyr::mutate(
        robot = case_when(
            name == "red1" ~ "R1",
            name == "red2" ~ "R2",
            name == "red3" ~ "R3",
            name == "blue1" ~ "B1",
            name == "blue2" ~ "B2",
            name == "blue3" ~ "B3"
        ),
        team = substr(value, 4, nchar(value)),
        match = match_number
    ) |>
    dplyr::select(
        match, robot, team
    )

############################
#### Correctness Checks ####
############################    


