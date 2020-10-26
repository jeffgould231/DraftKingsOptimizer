
library(lpSolve)
library(tidyverse)

make_lineups <- function(qb, n_lineups = 75, overlap = 3, flex_eligible = c("RB", "WR"),
                         stack_size = 1, rb_in_stack1 = FALSE, run_it_back = T, exclude_players = NULL, 
                         max_proj_ownership = 135,
                         secondary_stack = NULL, secondary_probs = NULL, rb_in_stack2 = FALSE){
  
  set.seed(420)
  
  slate <- read_csv("Weekly DraftKings Main Slate Projections.csv") %>%
    rename(projection = `DK Projection`,
           position = `DK Position`,
           proj_own = `DK Ownership`,
           salary = `DK Salary`) %>%
    mutate(proj_own = parse_number(proj_own),
           salary = parse_number(salary)) %>%
    #mutate(salary = as.numeric(str_remove(salary, "\\,"))) %>%
    mutate(in_lineups = 0,
           exclude = 0) %>%
    drop_na() %>%
    mutate(max_own = case_when(
      position == "TE" ~ 15,
      position == "WR" ~ 18,
      position == "DST" ~ 15,
      position == "RB" ~ 30,
      position == "QB" ~ 30
    )) %>% mutate(max_own = case_when(
      Player == "Derrick Henry" ~ 25,
      Player == "Phillip Lindsay" ~ 15,
      Player == "Alexander Mattison" ~ 20,
      Player == "Marquise Brown" ~ 10,
      Player == "Adrian Peterson" ~ 5,
      Player == "Adam Humphries" ~ 5,
      TRUE ~ max_own
    )) %>%
    mutate(game = ifelse(Team <= Opponent, str_c(Team, Opponent), str_c(Opponent,Team)),
           store_projection = projection) 
  
  lineup_portfolio_display <- data.frame(QB = c(), RB1 = c(), RB2 = c(), WR1 = c(), 
                                         WR2 = c(), WR3 = c(), TE = c(), FLEX = c(), DST = c(), Proj. = c())
  
  lineup_portfolio_export <- data.frame(QB = c(), RB1 = c(), RB2 = c(), WR1 = c(), 
                                        WR2 = c(), WR3 = c(), TE = c(), FLEX = c(), DST = c(), Proj. = c())
  
  
  if(length(secondary_stack) == 1){
    if(secondary_stack == "None"){secondary_stack = NULL}
  }else if("None" %in% secondary_stack){
    secondary_stack = secondary_stack[secondary_stack != "None"]
  }
  
  slate <- slate %>%
    filter(!(position == "QB" & Player != qb)) %>%
    mutate(exclude = case_when(
      Player %in% exclude_players ~ 1,
      TRUE ~ exclude
    ))
  
  qb_team <- slate[slate$position == "QB",]$Team
  
  slate <- slate %>%
    mutate(qb_stack = case_when(
      position %in% c("WR", "TE") & Team == qb_team ~ 1, 
      Team == qb_team & rb_in_stack1 == TRUE & position == "RB" ~ 1,
      TRUE ~ 0),
      run_back = ifelse(position %in% c("RB", "WR", "TE") & Opponent == qb_team, 1, 0)) %>%
    filter(!(position == "DST" & (Opponent == qb_team | Team == qb_team)))%>%
    mutate(max_own = case_when(
      Player == qb ~ 100,
      qb_stack == 1 ~ 75,
      run_back == 1 ~ 75,
      TRUE ~ max_own
    ))
  
  exclusion_vector <- slate$exclude
  obj <- slate$projection
  
  mat <- matrix(c(
    slate$salary,
    as.numeric(slate$position == "QB"), ## QB constraint, must roster 1 QB
    as.numeric(slate$position == "RB"), ## RB constraint for minimum of 2 RBs
    as.numeric(slate$position == "RB"), ## RB constraint for maximum of 3 RBs
    as.numeric(slate$position == "WR"), ## WR constraint for minimum of 3 WRs
    as.numeric(slate$position == "WR"), ## WR constraint for maximum of 4 WRs
    as.numeric(slate$position %in% c("RB", "WR", "TE")), ## exactly 7 RBs +WRs + TEs
    as.numeric(slate$position == "TE"), ## TE constraint for minimum of 1 TE
    as.numeric(slate$position == "TE"), ## TE constraint for maximum of 2 TEs
    as.numeric(slate$position == "DST"), ## DST constraint, must roster 1 DST
    slate$proj_own,## projected field ownership constraint
    slate$qb_stack,
    slate$run_back
  ),
  byrow = TRUE, ncol = nrow(slate))
  
  for (lineup_num in 1:n_lineups) {
    
    slate <- slate %>%
      mutate(projection = case_when(
        position == "DST" ~ rnorm(1, mean = store_projection, sd = 1.5) %>% round(1),
        TRUE ~ store_projection
      ))
    
    if(lineup_num >= 4){## If a player is above our desired ownership level, exclude them from this round of rosters
      slate <- slate %>%
        mutate(exclude = ifelse(100*in_lineups / lineup_num >= max_own, 1, exclude)) 
    }
    
    dir <- c("<=", "==", ">=", "<=", ">=", "<=", "==", ">=", "<=", "==", "<=", ">=", ">=")
    
    if(length(stack_size) > 1){stack_size = sample(stack_size, 1)}
    
    rhs <- c(50000, 1, 2, 2 + as.numeric("RB" %in% flex_eligible),
             3, 3 + as.numeric("WR" %in% flex_eligible), 7,
             1, 1+ as.numeric("TE" %in% flex_eligible), 1, 
             max_proj_ownership, stack_size, as.numeric(run_it_back))
    ## Salary Cap, QB, RB, RB, WR, WR, TE, TE, DST, Ownership, qb_stack, run back
    
    if(lineup_num >= 2){ ## This step is to prevent any two lineups from having more than `overlap` players in common
      dir <- c(dir, rep("<=", lineup_num-1)) ##  and append the lineups to our constrain matrix
      rhs <- c(rhs, rep(overlap, lineup_num-1))
      mat <- rbind(mat, lp_sol$solution)
    }
    
    mat <- rbind(mat, slate$exclude) ## Add excluded players to our constraints
    dir <- c(dir, "==")
    rhs <- c(rhs, 0)
    
    if(!is.null(secondary_stack)){
      if(is.null(secondary_probs)) {
        secondary_probs <- rep(1/length(secondary_stack), length(secondary_stack))
      }
      game_stack_2 = sample(secondary_stack, 1, prob = secondary_probs)
      slate <- slate %>%
        mutate(second_stack = case_when(
          game == game_stack_2 & position %in% c("WR", "TE") ~ 1,
          #game == game_stack_2 & rb_in_stack2 == T & position == "RB" ~ 1,
          TRUE ~ 0
        ))
      mat <- rbind(mat, slate$second_stack)
      dir <- c(dir, "==")
      rhs <- c(rhs, 2)
    }
    
    
    lp_sol <- lp("max", obj, mat, dir, rhs, compute.sens = 1, all.bin = T) ## Solve for maximum projections
    
    return_lineup_display <- slate %>%  ### Take solution vector, transform into a human readable lineup
      mutate(in_lineup = lp_sol$solution) %>%
      filter(in_lineup == 1) %>%
      group_by(position) %>%
      mutate(posRank = rank(-salary, ties.method = "first")) %>%
      ungroup() %>%
      mutate(position = case_when(
        position == "RB" & posRank <=2 ~ str_c(position, posRank),
        position == "WR" & posRank <=3 ~ str_c(position, posRank),
        position == "RB"& posRank == 3 ~ "FLEX",
        position == "WR" & posRank == 4 ~ "FLEX",
        position == "TE" & posRank == 2 ~ "FLEX",
        TRUE ~ position
      )) %>%
      select(Player, position) %>%
      pivot_wider(names_from = position, values_from = Player) %>%
      select(QB, RB1, RB2, WR1, WR2, WR3, TE, FLEX, DST)
    
    return_lineup_export <- slate %>%  ### Take solution vector, transform into a human readable lineup
      mutate(in_lineup = lp_sol$solution) %>%
      filter(in_lineup == 1) %>%
      mutate(Player = str_c(Player, " (", DKSlateID, ")")) %>%
      group_by(position) %>%
      mutate(posRank = rank(-salary, ties.method = "first")) %>%
      ungroup() %>%
      mutate(position = case_when(
        position == "RB" & posRank <=2 ~ str_c(position, posRank),
        position == "WR" & posRank <=3 ~ str_c(position, posRank),
        position == "RB"& posRank == 3 ~ "FLEX",
        position == "WR" & posRank == 4 ~ "FLEX",
        position == "TE" & posRank == 2 ~ "FLEX",
        TRUE ~ position
      )) %>%
      select(Player, position) %>%
      pivot_wider(names_from = position, values_from = Player) %>%
      select(QB, RB1, RB2, WR1, WR2, WR3, TE, FLEX, DST)
    
    require(magrittr)
    return_lineup_display$Proj. = slate %>%  ### Take solution vector, transform into a human readable lineup
      mutate(in_lineup = lp_sol$solution) %>%
      filter(in_lineup == 1) %$% sum(store_projection)
    
    slate$in_lineups = slate$in_lineups + lp_sol$solution ## Update rostership numbers
    slate$exclude = exclusion_vector ## reset exculsion to 0
    
    lineup_portfolio_display <- bind_rows(lineup_portfolio_display, return_lineup_display) %>% 
      distinct() ## add current lineups to lineup portfolio
    
    lineup_portfolio_export <- bind_rows(lineup_portfolio_export, return_lineup_export) %>% 
      distinct() 
    
    if(!is.null(secondary_stack)){
      mat <- mat[1:(nrow(mat) - 2),]
    }else{
      mat <- mat[1:(nrow(mat) - 1),]
    }
    ## remove exclusion row and secondary game stack from constraint matrix
    
    # dir <- c(dir, "<=") ##  and append the lineups to our constrain matrix
    # rhs <- c(rhs, overlap)
    # mat <- rbind(mat, lp_sol$solution)
    #lineup_num = lineup_num + 1
    #lineup_num <- nrow(lineup_portfolio)
    
  }
  
  return(export = lineup_portfolio_export)
  
}



qbs <- sample(
  c(rep("Kyler Murray", 15), rep("Russell Wilson", 25), 
    rep("Deshaun Watson", 25), rep("Justin Herbert", 15), rep("Josh Allen", 10),
    rep("Patrick Mahomes", 15), rep("Ryan Tannehill", 15), rep("Cam Newton", 15), rep("Matt Ryan", 15))
)



require_one_of <- c("Melvin Gordon III", "Antonio Gibson", "Latavius Murray", 
                    "D'Andre Swift", "Tee Higgins", "DJ Moore", "Joshua Kelley", "Tyler Boyd",
                    "A.J. Green")

n_lineups = 150
overlap = 3
flex_eligible = c("RB", "WR")
stack_size = 2
rb_in_stack1 = T
run_it_back = T
exclude_players = c("Panthers", "Broncos", "Texans", "Raiders")
max_proj_ownership = 145
secondary_stack = c("ARISEA", "CARNO", "GBHOU", "ATLDET", "PITTEN")
secondary_probs = NULL
rb_in_stack2 = TRUE
  
  set.seed(420)
  
  slate <- read_csv("Weekly DraftKings Main Slate Projections.csv") %>%
    rename(projection = `DK Projection`,
           position = `DK Position`,
           proj_own = `DK Ownership`,
           salary = `DK Salary`) %>%
    filter(!(str_detect(Player, "N/A"))) %>%
    mutate(proj_own = parse_number(proj_own),
           salary = parse_number(salary)) %>%
    #mutate(salary = as.numeric(str_remove(salary, "\\,"))) %>%
    mutate(in_lineups = 0,
           exclude = 0) %>%
    drop_na() %>%
    mutate(max_own = case_when(
      position == "TE" ~ 15,
      position == "WR" ~ 22.5,
      position == "DST" ~ 15,
      position == "RB" ~ 32.5,
      position == "QB" ~ 50
    ),
    Player = case_when(
      position == "DST" ~ str_c(Player, position, sep = " "),
      TRUE ~ Player
    )) %>%
    mutate(max_own = case_when(
      Player == "Giovani Bernard" ~ 15,
      Player == "Jamaal Williams" ~ 12.5,
      Player == "Gabriel Davis" ~ 7.5,
      TRUE ~ max_own
    ),
    require_one = case_when(
      Player %in% require_one_of ~ 1,
      TRUE ~ 0
    )) %>%
    mutate(game = ifelse(Team <= Opponent, str_c(Team, Opponent), str_c(Opponent,Team)),
           store_projection = projection) %>%
    filter(!(position == "QB" & !(Player %in% qbs)))
  
  
  
  lineup_portfolio_display <- data.frame(QB = c(), RB1 = c(), RB2 = c(), WR1 = c(), 
                                         WR2 = c(), WR3 = c(), TE = c(), FLEX = c(), DST = c(), Proj. = c())
  
  lineup_portfolio_export <- data.frame(QB = c(), RB1 = c(), RB2 = c(), WR1 = c(), 
                                        WR2 = c(), WR3 = c(), TE = c(), FLEX = c(), DST = c(), Proj. = c())

  qb <- qbs[1]  
  
  slate_work <- slate 
  
  qb_team <- slate_work[slate_work$Player == qb,]$Team
  
  slate_work <- slate_work %>%
    mutate(qb_stack = case_when(
      position %in% c("WR", "TE") & Team == qb_team ~ 1, 
      Team == qb_team & rb_in_stack1 == TRUE & position == "RB" ~ 1,
      TRUE ~ 0),
      run_back = ifelse(position %in% c("WR", "TE") & Opponent == qb_team, 1, 0)) %>%
    #filter(!(position == "QB" & !(Player %in% qbs))) %>%
    mutate(exclude = case_when(
      Player %in% exclude_players ~ 1,
      (position == "DST" & (Opponent == qb_team | Team == qb_team)) ~ 1,
      TRUE ~ exclude
    ))
  
  
  slate_work <- slate_work %>%
    mutate(projection = case_when(
      #position == "DST" ~ rnorm(1, mean = store_projection, sd = 1.5) %>% round(1),
      position == "DST" ~ rexp(1, 1/store_projection) %>% round(1),
      TRUE ~ store_projection
    ))
  
  mat <- matrix(c(
    slate_work$salary,
    as.numeric(slate_work$Player == qb),
    as.numeric(slate_work$position == "QB"), ## QB constraint, must roster 1 QB
    as.numeric(slate_work$position == "RB"), ## RB constraint for minimum of 2 RBs
    as.numeric(slate_work$position == "RB"), ## RB constraint for maximum of 3 RBs
    as.numeric(slate_work$position == "WR"), ## WR constraint for minimum of 3 WRs
    as.numeric(slate_work$position == "WR"), ## WR constraint for maximum of 4 WRs
    as.numeric(slate_work$position %in% c("RB", "WR", "TE")), ## exactly 7 RBs +WRs + TEs
    as.numeric(slate_work$position == "TE"), ## TE constraint for minimum of 1 TE
    as.numeric(slate_work$position == "TE"), ## TE constraint for maximum of 2 TEs
    as.numeric(slate_work$position == "DST"), ## DST constraint, must roster 1 DST
    slate_work$proj_own,## projected field ownership constraint
    slate_work$qb_stack,
    slate_work$run_back,
    slate_work$require_one
  ),
  byrow = TRUE, ncol = nrow(slate_work))
  
for (lineup_num in 1:n_lineups){ 
    
  qb <- qbs[lineup_num]  
    
  run_it_back = ifelse(qb %in% c("Patrick Mahomes", "Josh Allen"), sample(c(TRUE, FALSE), 1, prob = c(2/3, 1/3)), TRUE)
  
  slate_work <- slate 
  
  qb_team <- slate_work[slate_work$Player == qb,]$Team
  
  slate_work <- slate_work %>%
    mutate(qb_stack = case_when(
      position %in% c("WR", "TE") & Team == qb_team ~ 1, 
      Team == qb_team & rb_in_stack1 == TRUE & position == "RB" ~ 1,
      TRUE ~ 0),
      run_back = ifelse(position %in% c("WR", "TE") & Opponent == qb_team, 1, 0)) %>%
    #filter(!(position == "QB" & !(Player %in% qbs))) %>%
    mutate(exclude = case_when(
      Player %in% exclude_players ~ 1,
      (position == "DST" & (Opponent == qb_team | Team == qb_team)) ~ 1,
      TRUE ~ exclude
    )) %>%
    mutate(run_back = case_when(
      Player == "Melvin Gordon" & qb == "Patrick Mahomes" ~ 1,
      TRUE ~ run_back
    ))
  

    slate_work <- slate_work %>%
      mutate(projection = case_when(
        #position == "DST" ~ rnorm(1, mean = store_projection, sd = 1.5) %>% round(1),
        position == "DST" ~ rexp(1, 1/store_projection) %>% round(1),
        TRUE ~ store_projection
      ))
    
    if(lineup_num >= 10){## If a player is above our desired ownership level, exclude them from this round of rosters
      slate_work <- slate_work %>%
        mutate(exclude = ifelse(100*in_lineups / lineup_num >= max_own, 1, exclude)) 
    }
    
    exclusion_vector <- slate_work$exclude
    obj <- slate_work$projection
    

    
    mat[2, ] = as.numeric(slate_work$Player == qb)
    mat[13, ] = slate_work$qb_stack
    mat[14, ] = slate_work$run_back
    
    dir <- c("<=", "==", "==", ">=", "<=", ">=", "<=", "==", ">=", "<=", "==", "<=", ">=", ">=", ">=")
    rhs <- c(50000, 1, 1, 2, 2 + as.numeric("RB" %in% flex_eligible),
             3, 3 + as.numeric("WR" %in% flex_eligible), 7,
             1, 1+ as.numeric("TE" %in% flex_eligible), 1, 
             max_proj_ownership, stack_size, as.numeric(run_it_back), 1)
    ## Salary Cap, QB, RB, RB, WR, WR, TE, TE, DST, Ownership, qb_stack, run back
    
    if(lineup_num >= 2){ ## This step is to prevent any two lineups from having more than `overlap` players in common
      dir <- c(dir, rep("<=", lineup_num-1)) ##  and append the lineups to our constrain matrix
      rhs <- c(rhs, rep(overlap, lineup_num-1))
      mat <- rbind(mat, lp_sol$solution)
    }
    
    mat <- rbind(mat, slate_work$exclude) ## Add excluded players to our constraints
    dir <- c(dir, "==")
    rhs <- c(rhs, 0)
    
    if(!is.null(secondary_stack)){
      if(is.null(secondary_probs)) {
        secondary_probs <- rep(1/length(secondary_stack), length(secondary_stack))
      }
      game_stack_2 = sample(secondary_stack, 1, prob = secondary_probs)
      
      if(rb_in_stack2 == T){
        slate_work <- slate_work %>%
          mutate(second_stack = case_when(
            game == game_stack_2 & position %in% c("RB", "WR", "TE") ~ 1,
            TRUE ~ 0
          ))
      }
      
      if(rb_in_stack2 == F){
        slate_work <- slate_work %>%
          mutate(second_stack = case_when(
            game == game_stack_2 & position %in% c("RB", "WR", "TE") ~ 1,
            TRUE ~ 0
          ))
      }
      
      # slate_work <- slate_work %>%
      #   mutate(second_stack = case_when(
      #     game == game_stack_2 & position %in% c("WR", "TE") ~ 1,
      #     game == game_stack_2 & rb_in_stack2 == T & position == "RB" ~ 1,
      #     TRUE ~ 0
      #   ))
      
      mat <- rbind(mat, slate_work$second_stack)
      dir <- c(dir, ">=")
      rhs <- c(rhs, 2)
    }
    
    
    lp_sol <- lp("max", obj, mat, dir, rhs, compute.sens = 1, all.bin = T) ## Solve for maximum projections
    
    return_lineup_display <- slate_work %>%  ### Take solution vector, transform into a human readable lineup
      mutate(in_lineup = lp_sol$solution) %>%
      filter(in_lineup == 1) %>%
      group_by(position) %>%
      mutate(posRank = rank(-salary, ties.method = "first")) %>%
      ungroup() %>%
      mutate(position = case_when(
        position == "RB" & posRank <=2 ~ str_c(position, posRank),
        position == "WR" & posRank <=3 ~ str_c(position, posRank),
        position == "RB"& posRank == 3 ~ "FLEX",
        position == "WR" & posRank == 4 ~ "FLEX",
        position == "TE" & posRank == 2 ~ "FLEX",
        TRUE ~ position
      )) %>%
      select(Player, position) %>%
      pivot_wider(names_from = position, values_from = Player) %>%
      select(QB, RB1, RB2, WR1, WR2, WR3, TE, FLEX, DST)
    
    return_lineup_export <- slate_work %>%  ### Take solution vector, transform into a human readable lineup
      mutate(in_lineup = lp_sol$solution) %>%
      filter(in_lineup == 1) %>%
      mutate(Player = str_c(Player, " (", DKSlateID, ")")) %>%
      mutate(Player = str_remove(Player, " DST")) %>%
      group_by(position) %>%
      mutate(posRank = rank(-salary, ties.method = "first")) %>%
      ungroup() %>%
      mutate(position = case_when(
        position == "RB" & posRank <=2 ~ str_c(position, posRank),
        position == "WR" & posRank <=3 ~ str_c(position, posRank),
        position == "RB"& posRank == 3 ~ "FLEX",
        position == "WR" & posRank == 4 ~ "FLEX",
        position == "TE" & posRank == 2 ~ "FLEX",
        TRUE ~ position
      )) %>%
      select(Player, position) %>%
      pivot_wider(names_from = position, values_from = Player) %>%
      select(QB, RB1, RB2, WR1, WR2, WR3, TE, FLEX, DST)
    
    require(magrittr)
    return_lineup_display$Proj. = slate_work %>%  ### Take solution vector, transform into a human readable lineup
      mutate(in_lineup = lp_sol$solution) %>%
      filter(in_lineup == 1) %$% sum(store_projection)
    
    slate$in_lineups = slate$in_lineups + lp_sol$solution ## Update rostership numbers
    slate_work$exclude = exclusion_vector ## reset exculsion to 0
    
    lineup_portfolio_display <- bind_rows(lineup_portfolio_display, return_lineup_display) %>% 
      distinct() ## add current lineups to lineup portfolio
    
    lineup_portfolio_export <- bind_rows(lineup_portfolio_export, return_lineup_export) %>% 
      distinct() 
    
    if(!is.null(secondary_stack)){
      mat <- mat[1:(nrow(mat) - 2),]
    }else{
      mat <- mat[1:(nrow(mat) - 1),]
    }
    ## remove exclusion row and secondary game stack from constraint matrix
    
    # dir <- c(dir, "<=") ##  and append the lineups to our constrain matrix
    # rhs <- c(rhs, overlap)
    # mat <- rbind(mat, lp_sol$solution)
    #lineup_num = lineup_num + 1
    #lineup_num <- nrow(lineup_portfolio)
    
}



RB1s <- lineups %>%
  group_by(RB1) %>%
  summarise(rosters = n()) %>%
  rename(RB = RB1)
RB2s <- lineups %>%
  group_by(RB2) %>%
  summarise(rosters = n()) %>%
  rename(RB = RB2)
RB3s <- lineups %>%
  group_by(FLEX) %>%
  summarise(rosters = n()) %>%
  rename(RB = FLEX)
RBs <- bind_rows(RB1s, RB2s, RB3s) %>%
  group_by(RB) %>%
  summarise(rosters = sum(rosters)) %>%
  mutate(ownership = rosters / 150)

WR1s <- lineups %>%
  group_by(WR1) %>%
  summarise(rosters = n()) %>%
  rename(WR = WR1)
WR2s <- lineups %>%
  group_by(WR2) %>%
  summarise(rosters = n()) %>%
  rename(WR = WR2)
WR3s <- lineups %>%
  group_by(WR3) %>%
  summarise(rosters = n()) %>%
  rename(WR = WR3)
WR3s <- lineups %>%
  group_by(FLEX) %>%
  summarise(rosters = n()) %>%
  rename(WR = FLEX)
WRs <- bind_rows(WR1s, WR2s, WR3s) %>%
  group_by(WR) %>%
  summarise(rosters = sum(rosters)) %>%
  mutate(ownership = rosters / 150)

TEs <- lineups %>%
  group_by(TE) %>%
  summarise(rosters = n()) 

lineups_final <- lineups %>%
  filter(!(str_detect(FLEX, "Breida|Darrell Daniels")), !(str_detect(WR3, "Cole Beasley"))) %>%
  filter(!(str_detect(WR3, "Robby Anderson|Curtis Samuel") & str_detect(WR2, "DJ Moore|Robby Anderson"))) %>%
  filter(!(str_detect(FLEX, "Robby Anderson|Curtis Samuel") & str_detect(WR3, "DJ Moore|Robby Anderson"))) %>%
  filter(!(str_detect(WR3, "John Brown") & str_detect(TE, "Knox|Kroft"))) %>%
  filter(!(str_detect(QB, "Dak Prescott") & str_detect(RB1, "Ezekiel Elliott")))

write_csv(lineup_portfolio_display, "DK_week7_entries_display.csv")
write_csv(lineup_portfolio_export, "DK_week7_entries.csv")
