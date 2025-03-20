library(scoutR) # devtools::install_github("gkrotkov/scoutR")
library(ggplot2) # install.packages("ggplot2")

#' @TODO remember to write test cases, start building my "utils.R"

#####################
#### Winner Plot ####
#####################

rm(list = ls())

seed_finish_thru_wk <- function(yr, wk, finish){
    wks <- 1:wk
    lst <- lapply(wks, week_seed_finish_table, year = yr, 
                  finish = finish, size = 8)
    result <- purrr::reduce(lst, combine_tbls)
    dummy <- rep(0, 8)
    names(dummy) <- 1:8
    result <- combine_tbls(dummy, result)
    result <- round(result / sum(result), digits = 2)
    return(result)
}

# Driver

years <- c(2012:2019, 2022:2025)
result <- lapply(years, seed_finish_thru_wk, wk = 3, finish = "Winner")
names(result) <- years

# reconstruct years into a df (credit to ChatGPT for the do.call/rbind idea)
df <- do.call(rbind, lapply(names(result), function(name){
    data.frame(
        year = name, 
        seed = names(result[[name]]), 
        pct = result[[name]]
    )
}))

rownames(df) <- 1:nrow(df)

# Viz

ggplot(df, aes(x = year, y = pct, color = seed)) + 
    geom_point() + 
    geom_line() + 
    labs(title = "#1 Seeds are crushing in 2025",
         subtitle = "Cumulative through week 3, 8-alliance events only",
         x = "Year", y = "Tournament Win Percentage", 
         color = "Seed", caption = "Data from frc-events via TBA") +
    theme_bw() + 
    scale_y_continuous(labels = scales::percent, limits = c(0, 1))

###################
#### Finalists ####
###################

# Driver

years <- c(2012:2019, 2022:2025)
result <- lapply(years, seed_finish_thru_wk, wk = 3, finish = "Finalist")
names(result) <- years

df <- do.call(rbind, lapply(names(result), function(name){
    data.frame(
        year = name, 
        seed = names(result[[name]]), 
        pct = result[[name]]
    )
}))

rownames(df) <- 1:nrow(df)

# Viz

ggplot(df, aes(x = year, y = pct, color = seed)) + 
    geom_point() + 
    geom_line() +
    labs(title = "Reefscape has a lotta 2-seeds as finalists, like Rapid React",
         subtitle = "Cumulative through week 3, 8-alliance events only",
         x = "Year", y = "Finalist Finish Percentage", 
         color = "Seed", caption = "Data from frc-events via TBA") +
    theme_bw() + 
    scale_y_continuous(labels = scales::percent, limits = c(0, 1))

####################################
#### Playoff Advancement Points ####
####################################

rm(list = ls())

event_finish_playoff_pts <- function(key){
    df <- event_alliances(key)
    if (is.null(df)) return(0) # since we're combining with "+", 0 is null
    df <- df %>%
        mutate(
            playoff_pts = case_when(
                finish == "Winner" ~ 30, 
                finish == "Finalist" ~ 20,
                finish == "3rd" ~ 13,
                finish == "4th" ~ 7,
                finish == "Semifinalist" ~ 10, 
                .default = 0
            )
        )
    return(df$playoff_pts)
}

seed_playoff_pts_thru_wk <- function(yr, wk){
    wks <- (1:wk) - 1 # TBA 0-indexes weeks, so we subtract one
    keys <- events(yr, official = TRUE) %>%
        dplyr::filter(week %in% wks) %>%
        dplyr::pull(key)
    result <- rep(0, 8)
    names(result) <- 1:8
    for (i in seq_along(keys)){
        result <- result + event_finish_playoff_pts(keys[i])
    }
    # return the result as a percentage of available playoff pts
    return(round(result / sum(result), digits = 2))
}

years <- c(2012:2019, 2022:2025)
result <- lapply(years, seed_playoff_pts_thru_wk, wk = 3)
names(result) <- years

df <- do.call(rbind, lapply(names(result), function(name){
    data.frame(
        year = name, 
        seed = names(result[[name]]), 
        pct = result[[name]]
    )
}))

rownames(df) <- 1:nrow(df)

ggplot(df, aes(x = year, y = pct, fill = seed)) + 
    geom_area(aes(group = seed), position = "fill") + 
    labs(title = "Playoff Advancement Points Distribution", 
         subtitle = "Cumulative through week 3",
         x = "Year", y = "Percent of Playoff Advancement Points", 
         caption = "Data from frc-events via TBA") +
    theme_bw() + 
    scale_y_continuous(labels = scales::percent, limits = c(0, 1))

