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
    Player == "Derrick Henry" ~ 10,
    TRUE ~ max_own
  ))

working_data <- week2

lineup_portfolio <- data.frame(QB = c(), RB1 = c(), RB2 = c(), WR1 = c(), 
                               WR2 = c(), WR3 = c(), TE = c(), FLEX = c(), DST = c())
obj <- working_data$projection

mat <- matrix(c(
    working_data$salary,
    as.numeric(working_data$position == "QB"), ## QB constraint, must roster 1 QB
    as.numeric(working_data$position == "RB"), ## RB constraint, can roster 2-3 RBs, but for now lock at 3
    as.numeric(working_data$position == "WR"), ## WR constraint, can roster 3-4 WRs, but for now lock at 3
    as.numeric(working_data$position == "TE"), ## TE constraint, can roster 1-2 TEs, but best to roster 1
    as.numeric(working_data$position == "DST"), ## DST constraint, must roster 1 DST
    working_data$proj_own ## projected field ownership constraint
  ),
  byrow = TRUE, ncol = nrow(working_data))

overlap <- 3

tictoc::tic()
for (lineup_num in 1:150) {
  
  if(lineup_num > 5){## If a player is above our desired ownership level, exclude them from this round of rosters
    working_data <- working_data %>%
      mutate(exclude = ifelse(100*in_lineups / lineup_num >= max_own, 1, exclude)) 
  }
  
  dir <- c("<=", "==", "==", "==", "==", "==", "<=")
  rhs <- c(50000, 1, 3, 3, 1, 1, 125) ## Salary Cap, QB, RB, WR, TE, DST, Ownership
  
  if(lineup_num >= 2){ ## This step is to prevent any two lineups from having more than `overlap` players in common
    dir <- c(dir, rep("<=", lineup_num-1)) ##  and append the lineups to our constrain matrix
    rhs <- c(rhs, rep(overlap, lineup_num-1))
    mat <- rbind(mat, lp_trans$solution)
  }
  mat <- rbind(mat, working_data$exclude) ## Add excluded players to our constraints
  dir <- c(dir, "==")
  rhs <- c(rhs, 0)
  
  
  lp_trans <- lp("max", obj, mat, dir, rhs, compute.sens = 1, all.bin = T) ## Solve for maximum projections
  
  return_lineup <- working_data %>%  ### Take solution vector, transform into a human readable lineup
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
  
  
  working_data$in_lineups = working_data$in_lineups + lp_trans$solution ## Update rostership numbers
  working_data$exclude = 0 ## reset exculsion to 0
  
  lineup_portfolio <- bind_rows(lineup_portfolio, return_lineup) ## add current lineups to lineup portfolio
  
  mat <- mat[-nrow(mat),] ## remove exclusion row from constraint matrix
  
}
tictoc::toc()

