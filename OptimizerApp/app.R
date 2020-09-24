#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lpSolve)
library(tidyverse)

source("OptimizerFunction.R")
Projections <- read_csv("ETRweek2Projections.csv")%>%
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
    mutate(game = ifelse(Team <= Opponent, str_c(Team, Opponent), str_c(Opponent,Team)))

qb_list <- Projections %>%
    filter(position == "QB") %>%
    select(Player)

exclude_list <- Projections %>%
    filter(position != "QB") %>%
    select(Player)

secondary_games <- Projections %>%
    select(game) %>% unique()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Contested Catch DraftKings Lineups"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #includeMarkdown("instructions.md"),
            selectInput("quarterback", "Choose QB (required):", choices = c(qb_list)),
            sliderInput("stack_size", "Choose # of WRs/TEs to stack:", value = 1, min = 0, max = 2),
            checkboxInput("run_it_back", "Stack with an opposing WR/TE?"),
            selectInput("second_stack", "Choose a secondary game stack (optional):", 
                        choices = c("None", secondary_games), selected = "None", multiple = TRUE),
            sliderInput("overlap", "Maximum Number of Players lineups can share:", value = 4, min = 2, max = 6),
            numericInput("number_lineups", "How many lineups to make:", min = 10, max = 75, value = 20),
            checkboxGroupInput("stack_eligible", "Positions to Play in Flex:", 
                               choices = c("RB", "WR", "TE"), selected = c("RB", "WR"), inline = T),
            sliderInput("max_ownership", "Max Projected Ownership: ", value = 150, min = 100, max = 200, step = 5),
            selectInput("exclude_players", "Choose Players To Exclude:", choices = exclude_list,
                        multiple = TRUE),
            submitButton("RUN"),
            width = 2
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            DT::dataTableOutput("lineup_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$lineup_table <- DT::renderDataTable({
        make_lineups(qb = input$quarterback, n_lineups = input$number_lineups, slate = Projections,
                     overlap = input$overlap, flex_eligible = input$stack_eligible,
                     stack_size = input$stack_size, run_it_back = input$run_it_back, 
                     exclude_players = input$exclude_players, max_proj_ownership = input$max_ownership,
                     secondary_stack = input$second_stack)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
