# CHCMP Prescouting Data

library(scoutR) # devtools::install_github("gkrotkov/scoutR")
library(tidyverse)

tangibles <- event_season_history("2025chcmp")

# grabbing tangibles based on their season history so far
tangibles <- tangibles %>%
    mutate(deep_pct = endGame_DeepCage / n_matches_count, 
           mobility_pct = autoLine_Yes / n_matches_count, 
           n_matches = n_matches_count)

# what else do I want to see? perhaps some EPAs and OPRs
get_epa <- function(key){
    result <- team_sb(key, event = "2025chcmp") |>
        purrr::pluck("epa", "breakdown") |>
        as_tibble()
    result$id <- key
    result <- result |>
        dplyr::select(id, dplyr::everything())
    return(result)
}

epas <- lapply(prescout$id, get_epa)
epas <- reduce(epas, rbind)

epas <- epas |>
    mutate(total_epa = total_points, 
           auto_epa = auto_points, 
           teleop_epa = teleop_points, 
           endgame_epa = endgame_points, 
           coral_epa = total_coral_points, 
           algae_epa = total_algae_points, 
           game_pieces_epa = total_game_pieces)

prescout <- merge(tangibles, epas) |>
    select(id, n_matches, deep_pct, mobility_pct, total_epa, auto_epa, 
           teleop_epa, endgame_epa, coral_epa, 
           algae_epa, game_pieces_epa)