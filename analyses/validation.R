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
        robot = dplyr::case_when(
            name == "red1" ~ "R1",
            name == "red2" ~ "R2",
            name == "red3" ~ "R3",
            name == "blue1" ~ "B1",
            name == "blue2" ~ "B2",
            name == "blue3" ~ "B3"
        ),
        team = value,
        match = match_number,
        string = paste(match_number, robot, value, sep = "_")
    ) |>
    dplyr::select(
        match, robot, team, string
    )

data$string <- paste(data$match, data$robot, data$team, sep = "_")

temp1 <- rep(NA, nrow(data)) #35 datas don't exist in schedule
for (i in seq_len(nrow(data))){
    temp1[i] = !(data$string[i] %in% schedule$string)
}

temp2 <- rep(NA, nrow(data)) #47 schedules don't exist in data
for (i in seq_len(nrow(schedule))){
    temp2[i] = !(schedule$string[i] %in% data$string)
}

cat("Here is a list of all the data rows we have that are not scheduled: ",
    data$string[which(temp1)])

cat("Here is a list of all the scheduled matches we did not cover in our data: ",
    schedule$string[which(temp2)])

############################
#### Correctness Checks ####
############################ 



