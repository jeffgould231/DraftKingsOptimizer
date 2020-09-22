library(lpSolve)
library(tidyverse)

week2 <- read_csv("ETRweek2Projections.csv")

week2 <- week2 %>%
  rename(projection = `DK Projection`,
         position = `DK Position`,
         proj_own = `DK Ownership`,
         salary = `DK Salary`) %>%
  mutate(proj_own = as.numeric(str_extract(proj_own, "[:digit:]+")),
         salary = as.numeric(str_extract(salary, "[:digit:]+"))) %>%
  mutate(max_own = 20,
         in_lineups = 0,
         exclude = 0) %>%
  drop_na() %>%
  mutate(max_own = case_when(
    Player == "Josh Allen" ~ 40,
    Player == "Chris Herndon" ~ 5,
    TRUE ~ max_own
  ))

working_data <- week2

lineup_portfolio <- data.frame(QB = c(), RB1 = c(), RB2 = c(), WR1 = c(), 
                               WR2 = c(), WR3 = c(), TE = c(), FLEX = c(), DST = c())

for (lineup_num in 1:150) {
  
  obj <- working_data$projection
  
  if(lineup_num > 5){
    working_data <- working_data %>%
      mutate(exclude = ifelse(100*in_lineups / lineup_num >= max_own, 1, exclude))
  }
  
  
  mat <- matrix(c(
    working_data$salary,
    as.numeric(working_data$position == "QB"),
    as.numeric(working_data$position == "RB"),
    as.numeric(working_data$position == "WR"),
    as.numeric(working_data$position == "TE"),
    as.numeric(working_data$position == "DST"),
    working_data$proj_own,
    working_data$in_lineups,
    working_data$exclude
  ),
  byrow = TRUE, ncol = nrow(working_data))
  
  dir <- c("<=", "==", "==", "==", "==", "==", "<=", "<", "==")
  rhs <- c(50000, 1, 3, 3, 1, 1, 125, lineup_num, 0)
  
  lp_trans <- lp("max", obj, mat, dir, rhs, compute.sens = 1, all.bin = T)
  
  return_lineup <- working_data %>%
    mutate(in_lineup = lp_trans$solution) %>%
    filter(in_lineup == 1) %>%
    group_by(position) %>%
    mutate(posRank = rank(-salary, ties.method = "first")) %>%
    ungroup() %>%
    mutate(position = case_when(
      position %in% c("RB", "WR") ~ str_c(position, posRank),
      TRUE ~ position
    )) %>%
    select(Player, position) %>%
    pivot_wider(names_from = position, values_from = Player) %>%
    select(QB, RB1, RB2, WR1, WR2, WR3, TE, FLEX = RB3, DST)
  
  
  working_data$in_lineups = working_data$in_lineups + lp_trans$solution
  working_data$exclude = 0
  
  lineup_portfolio <- bind_rows(lineup_portfolio, return_lineup)
  
}


