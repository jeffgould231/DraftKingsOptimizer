make_lineups <- function(qb, n_lineups = 75, overlap = 4, flex_eligible = c("RB", "WR"),
                         stack_size = 1, rb_in_stack1 = FALSE, run_it_back = T, exclude_players = NULL, 
                         max_proj_ownership = 125,
                         secondary_stack = NULL, secondary_probs = NULL, rb_in_stack2 = FALSE){
  
  slate <- read_csv("Weekly DraftKings Main Slate Projections.csv") %>%
    rename(projection = `DK Projection`,
           position = `DK Position`,
           proj_own = `DK Ownership`,
           salary = `DK Salary`) %>%
    mutate(proj_own = as.numeric(str_extract(proj_own, "[:digit:]+")),
           salary = as.numeric(str_extract(salary, "[:digit:]+"))) %>%
    mutate(in_lineups = 0,
           exclude = 0) %>%
    drop_na() %>%
    mutate(max_own = case_when(
      position == "TE" ~ 15,
      position == "WR" ~ 25,
      position == "DST" ~ 15,
      position == "RB" ~ 40,
      position == "QB" ~ 30
    ),
    Player = case_when(
      position == "DST" ~ str_c(Player, position, sep = " "),
      TRUE ~ Player
    )) %>%
    mutate(game = ifelse(Team <= Opponent, str_c(Team, Opponent), str_c(Opponent,Team)),
           store_projection = projection)
  
  lineup_portfolio <- data.frame(QB = c(), RB1 = c(), RB2 = c(), WR1 = c(), 
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
           run_back = ifelse(position %in% c("WR", "TE") & Opponent == qb_team, 1, 0)) %>%
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
          game == game_stack_2 & rb_in_stack2 == T & position == "RB" ~ 1,
          TRUE ~ 0
        ))
      mat <- rbind(mat, slate$second_stack)
      dir <- c(dir, "==")
      rhs <- c(rhs, 2)
    }
    
    
    lp_sol <- lp("max", obj, mat, dir, rhs, compute.sens = 1, all.bin = T) ## Solve for maximum projections
    
    return_lineup <- slate %>%  ### Take solution vector, transform into a human readable lineup
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
    
    require(magrittr)
    return_lineup$Proj. = slate %>%  ### Take solution vector, transform into a human readable lineup
      mutate(in_lineup = lp_sol$solution) %>%
      filter(in_lineup == 1) %$% sum(store_projection)
    
    slate$in_lineups = slate$in_lineups + lp_sol$solution ## Update rostership numbers
    slate$exclude = exclusion_vector ## reset exculsion to 0
    
    lineup_portfolio <- bind_rows(lineup_portfolio, return_lineup) %>% 
      distinct() ## add current lineups to lineup portfolio
    
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
  
  return(lineup_portfolio)
  
}


# View(make_lineups(qb = "Kyler Murray", slate = Projections, stack_size = 2, rb_in_stack1 = T,
#              secondary_stack = c("BUFMIA", "ATLDAL"), secondary_probs = c(0.4, 0.6))
# )
