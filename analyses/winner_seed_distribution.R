library(scoutR) # devtools::install_github("gkrotkov/scoutR")

#' @TODO remember to write test cases, start building my "utils.R"

winner_seed_pct_thru_wk <- function(yr, wk){
    wks <- 1:wk
    lst <- lapply(wks, week_winning_seed_table, year = yr, size = 8)
    result <- purrr::reduce(lst, combine_tbls)
    dummy <- rep(0, 8)
    names(dummy) <- 1:8
    result <- combine_tbls(dummy, result)
    result <- round(result / sum(result), digits = 2)
    return(result)
}

years <- c(2012:2019, 2022:2025)
result <- lapply(years, winner_seed_pct_thru_wk, 3)
names(result) <- years

# reconstruct years into a df (credit to ChatGPT for the do.call/rbind idea)
df <- do.call(rbind, lapply(names(result), function(name){
    data.frame(
        year = name, 
        seed = names(result[[name]]), 
        win_pct = result[[name]]
    )
}))

rownames(df) <- 1:nrow(df)

library(ggplot2) # install.packages("ggplot2")

ggplot(df, aes(x = year, y = win_pct, color = seed, group = seed)) + 
    geom_line() + 
    geom_point() + 
    labs(title = "#1 Seeds are crushing in 2025",
         subtitle = "Cumulative through week 3, 8-alliance events only",
         x = "Year", y = "Tournament Win Percentage", 
         color = "Seed", caption = "Data from frc-events via TBA") +
    theme_bw() + 
    scale_y_continuous(labels = scales::percent, limits = c(0, 1))
