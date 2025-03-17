library(scoutR)

#' Combine Tables
#' 
#' Combines two tables, numerical vectors of integer counts. Assumes that 
#' order() is a meaningful call on the names of the tables.
#' @param tbl1 First table to be combined
#' @param tbl2 Second table to be combined
#' @export
#' @TODO remember to write test cases, start building my "utils.R"
combine_tbls <- function(tbl1, tbl2){
    unq <- unique(c(names(tbl1), names(tbl2)))
    # using 0 bc we will combine with addition
    pads <- rep(0, length(unq) - length(tbl1))
    names(pads) <- setdiff(unq, names(tbl1))
    tbl1 <- c(tbl1, pads)
    tbl1 <- tbl1[order(names(tbl1))]
    # using 0 bc we will combine with addition
    pads <- rep(0, length(unq) - length(tbl2))
    names(pads) <- setdiff(unq, names(tbl2))
    tbl2 <- c(tbl2, pads)
    tbl2 <- tbl2[order(names(tbl2))]
    return(tbl1 + tbl2)
}

winner_seeds_thru_wk <- function(yr, wk){
    wks <- 1:wk
    lst <- lapply(wks, week_winning_seed_table, yr)
    return(purrr::reduce(lst, combine_tbls))
}

years <- c(2010:2019, 2022:2025)

winner_seeds_thru_wk(2024, 3)
