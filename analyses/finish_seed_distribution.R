library(scoutR) # devtools::install_github("gkrotkov/scoutR")

#' @TODO remember to write test cases, start building my "utils.R"

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

#####################
#### Driver Code ####
#####################

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

#############
#### Viz ####
#############

library(ggplot2) # install.packages("ggplot2")

ggplot(df, aes(x = year, y = pct, color = seed, group = seed)) + 
    geom_line() + 
    geom_point() + 
    labs(title = "#1 Seeds are crushing in 2025",
         subtitle = "Cumulative through week 3, 8-alliance events only",
         x = "Year", y = "Tournament Win Percentage", 
         color = "Seed", caption = "Data from frc-events via TBA") +
    theme_bw() + 
    scale_y_continuous(labels = scales::percent, limits = c(0, 1))

# Ok, what about finalists? 3rd/4th(/semifinalist)?
# Finalist is relatively easy, but the others I'll have to leave for a few 
# days from now.

################
#### Driver ####
################

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

#############
#### Viz ####
#############

ggplot(df, aes(x = year, y = pct, color = seed, group = seed)) + 
    geom_line() + 
    geom_point() + 
    labs(title = "Reefscape has a lotta 2-seeds as finalists, like Rapid React",
         subtitle = "Cumulative through week 3, 8-alliance events only",
         x = "Year", y = "Finalist Finish Percentage", 
         color = "Seed", caption = "Data from frc-events via TBA") +
    theme_bw() + 
    scale_y_continuous(labels = scales::percent, limits = c(0, 1))
