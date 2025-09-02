#Load required libraries
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(shinyWidgets)
library(tidyverse)
library(shinythemes)

blair_red <- "#a7000a"
in_rstudio <- requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()

default_linear_weights <- data.frame(
    team = 0,
    l1_cycle_tele = 0,
    l2_cycle_tele = 0,
    l3_cycle_tele = 0,
    l4_cycle_tele = 0,
    coral_cycle_tele = 10,
    coral_cycle_auto = 14,
    net_cycle = 20,
    proc_cycle = 8,
    algae = 0,
    total_pts_mean = 0,
    tele_pts_mean = 0,
    auto_pts_mean = 0,
    endgame_pts_mean = 20,
    algae_remove_pct = 3,
    intaked_algae_from_ground_pct = 9,
    move_pct = 0,
    dead_times = 0,
    dead_pct = -15,
    driver_rating_mean = 7,
    defense_rating_mean = 0
)

#UI
ui <- fluidPage(
    navbarPage(theme = shinytheme("yeti"),  
               "449 Scouting",
               tabPanel("Event Summary",
                        fluidRow(
                            column(12,
                                plotOutput("picklist_graph"),
                                plotOutput("long_column_output", height = "750px"),
                                DTOutput("picklist_table")
                                )
                            )
                        ),
               
               tabPanel("Auto-Picklisting",
                        fluidRow(
                            column(12, 
                                   actionButton("open_weights", "Adjust Weights", class = "btn btn-primary"),
                                   downloadButton("download_picklist", "Download Picklist", class = "btn btn-success"),
                                   DTOutput("weighted_picklist_table")
                            )
                        )
               ),
             
               tabPanel("Alliance/Match",
                        sidebarLayout(
                            sidebarPanel(
                              #Selection between match number or entering 6 teams
                                radioButtons("match_or_teams", "Select One of the Following", choices = c("Match Number", "Select 6 Teams", "2 Alliances")),
                                conditionalPanel(
                                    condition = "input.match_or_teams == 'Match Number'",
                                    selectInput("match_num", "Match Number", choices = NULL)
                                ),
                                conditionalPanel(
                                    condition = "input.match_or_teams == 'Select 6 Teams'",
                                    pickerInput("red_teams", "Red Alliance Teams", choices = NULL, multiple = TRUE, options = list(maxOptions = 3)),
                                    pickerInput("blue_teams", "Blue Alliance Teams", choices = NULL, multiple = TRUE, options = list(maxOptions = 3))
                                    ),
                                conditionalPanel(
                                    condition = "input.match_or_teams == '2 Alliances'",
                                    pickerInput("red_alliance", "Red Alliance", choices = NULL, multiple = FALSE, options = list(maxOptions = 1)),
                                    pickerInput("blue_alliance", "Blue Alliance", choices = NULL, multiple = FALSE, options = list(maxOptions = 1))                                
                                    ),
                                actionButton("generate_graph", "Generate Graphs", class = "btn btn-primary"),
                                #imageOutput("field_image_output")
                                ),
                            mainPanel(
                                uiOutput("score_prediction"),
                                plotOutput("alliance_box_plot_output"),
                                plotOutput("alliance_tele_coral_graph_output"),
                                plotOutput("alliance_auto_coral_graph_output"),
                                plotOutput("alliance_algae_bar_graph_output"),
                                plotOutput("alliance_endgame_graph_output"),
                                plotOutput("alliance_all_graph_output"),
                                DTOutput("alliance_table")
                                )
                            )
                        ),
               
               tabPanel("Previous Matches",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("match_num_PREVIOUS", "Match Number", choices = NULL),
                            ),
                            mainPanel(
                                plotOutput("MATCH_alliance_box_plot_output"),
                                plotOutput("MATCH_alliance_tele_coral_graph_output"),
                                plotOutput("MATCH_alliance_auto_coral_graph_output"),
                                plotOutput("MATCH_alliance_algae_bar_graph_output"),
                                plotOutput("MATCH_alliance_endgame_graph_output"),
                                plotOutput("MATCH_alliance_all_graph_output"),
                                DTOutput("MATCH_alliance_table")
                            )
                        )
               ),
               
               tabPanel("Compare Teams",
                        sidebarLayout(
                            sidebarPanel(
                                pickerInput("teams_selected", "Select Team", choices = NULL, multiple = TRUE, options = list(maxOptions = 2)),
                                selectInput("compare_teams_graph", "Choose Graph", choices = c("Points Large Bar Graph", "Comments", "Dead", "Progress Over Time")),
                                conditionalPanel(
                                    condition = "input.compare_teams_graph == 'Progress Over Time'",
                                    selectInput("compare_teams_progress_graph", "Choose Graph", choices = c("Points", "Driver Ranking"))
                                ),
                            ),
                            mainPanel(
                                plotOutput("compare_teams_graph_output"),
                                h3("Data Frame"),
                                DTOutput("compare_teams_data_row"),
                            )
                        )
               ),
                
                tabPanel("Single Team",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("team_select", "Select Team", choices = NULL),
                            selectInput("team_graph", "Choose Graph", choices = c("Overall Points Box Plot", "Auto Bar Graph", "Tele Bar Graph", "Endgame Bar Graph", "Comments", "Problems", "Match History", "Driver Rating History", "Cycle History")),
                            uiOutput("team_image_output")
                            ),
                        mainPanel(
                            plotOutput("team_graph_output"),
                            DTOutput("team_data_row"),
                            if (in_rstudio){
                                h3("Comments")
                                DTOutput("comments_list")
                            },
                            if (!in_rstudio) {
                                tagList(
                                    uiOutput("login_ui"),
                                    uiOutput("login_status"),
                                    uiOutput("comments_ui")
                                )
                            },
                            h3("Past raw data"),
                            DTOutput("past_team_table")
                            )
                        )
                    ),
                tabPanel("Scouts",
                    fluidRow(
                        plotlyOutput("scouter_graph_output", width = "100%", height = "500px"),
                        plotlyOutput("yapp_graph_output"),
                        plotOutput("high_streak_output")
                        )
                    )
               ),
    #actionButton("check", "CHECK DATA", style="simple", size="sm", color = "warning"),
    #actionButton("schedule", "CHECK SCHEDULE", style="simple", size="sm", color = "warning"),
    actionButton("vagle", "VAGLE DATA", style="simple", size="sm", color = "warning"),
    actionButton("mdsev", "MDSEV DATA", style="simple", size="sm", color = "warning"),
    actionButton("dchamp", "DCHAMP DATA", style="simple", size="sm", color = "warning"),
    actionButton("newton", "NEWTON DATA", style="simple", size="sm", color = "warning"),
    actionButton("all_data", "ALL DATA", style="simple", size="sm", color = "warning"),
    actionButton("vacri", "CRI DATA", style="simple", size="sm", color = "warning")
    )

#Server
server <- function(input, output, session) {
    data_dir <- "data_files"
    
    #Need to set data to Reactive Values to be able to change them later
    raw <- reactiveVal()
    match_schedule <- reactiveVal()
    teams <- reactiveVal()
    alliances <- reactiveVal()
    #scouts <- reactiveVal()
    event_code <- "all_data"
    
    #Helper Function
    load_event_data <- function(event) {
        raw(read.csv(file.path(data_dir, event, "data.csv")))
        match_schedule(read.csv(file.path(data_dir, event, "schedule.csv")))
        teams(read.csv(file.path(data_dir, event, "teams.csv")))
        alliances(read.csv(file.path(data_dir, event, "alliances.csv")))
        event_code <- event
        #scouts(read.csv(file.path(data_dir, event, "approved_scouters.csv")))
    }
    
    load_event_data("all_data")
    addResourcePath("images", "images") #needed to have images show up (don't delete)
    
    observe({
        req(match_schedule())
        updateSelectInput(session, "match_num", choices = unique(match_schedule()$Match))
        updateSelectInput(session, "match_num_PREVIOUS", choices = unique(match_schedule()$Match))
    })
    
    observe({
        req(teams())
        updatePickerInput(session, "red_teams", choices = unique(teams()$team))
        updatePickerInput(session, "blue_teams", choices = unique(teams()$team))
        updatePickerInput(session, "teams_selected", choices = unique(teams()$team))
        updateSelectInput(session, "team_select", choices = unique(teams()$team))
    })
    
    observe({
        req(alliances())
        updatePickerInput(session, "red_alliance", choices = unique(alliances()$Alliance))
        updatePickerInput(session, "blue_alliance", choices = unique(alliances()$Alliance))
    })
    
    #Password logic
    correct_password <- "0322"
    user_logged_in <- reactiveVal(in_rstudio)
    
    output$login_ui <- renderUI({
        if (!user_logged_in()) {
            tagList(
                passwordInput("password", "Enter password to access comments: "),
                actionButton("login", "Login")
            )
        }
    })
    
    observeEvent(input$login, {
        if (input$password == correct_password) {
            user_logged_in(TRUE)
        } else {
            user_logged_in(FALSE)
        }
    })
    
    output$login_status <- renderUI({
        req(input$login)
        if (user_logged_in()) {
            tags$p(style = "color: green;", "Access granted.")
        } else {
            tags$p(style = "color: red;", "Incorrect password.")
        }
    })
    
    output$comments_ui <- renderUI({
        req(user_logged_in())
        DTOutput("comments_list")
    })
    
    #Dataframe Calculations
    mldf <- reactive({
        req(raw())
        
        raw() %>% 
            mutate(
            auto_coral_l1_pts = auto_coral_L1_num * 3,
            auto_coral_l2_pts = auto_coral_L2_num * 4, 
            auto_coral_l3_pts = auto_coral_L3_num * 6, 
            auto_coral_l4_pts = auto_coral_L4_num * 7, 
            move_pts = move * 3,
            net_pts = robot_net_score * 4, 
            net_missed = robot_net_miss * 4,
            processor_value = proc_score * 2.5, 
            coral_l1_pts = coral_L1_num * 2,
            coral_l2_pts = coral_L2_num * 3,
            coral_l3_pts = coral_L3_num * 4,
            coral_l4_pts = coral_L4_num * 5,
            
            tele_coral_total = (coral_L1_num + coral_L2_num + coral_L3_num + coral_L4_num),
            auto_coral_total = (auto_coral_L1_num + auto_coral_L2_num + auto_coral_L3_num + auto_coral_L4_num),
            
            coral_tele_success_rate_raw = ifelse((tele_coral_total + tele_missed) == 0, tele_coral_total / (tele_coral_total + tele_missed), 0),
            coral_auto_success_rate_raw = ifelse((auto_coral_total + auto_coral_missed) == 0, auto_coral_total / (auto_coral_total + auto_coral_missed), 0),
            
            net_success_rate_raw = ifelse((robot_net_score + robot_net_miss)==0, 0, robot_net_score / (robot_net_score + robot_net_miss)),
            
            endgame_pts = case_when(
                ending == "P" ~ 2, 
                ending == "S" ~ 6, 
                ending == "D" ~ 12, 
                .default = 0
            ),
            
            dead = case_when(
                dead == "de" ~ 1,
                dead == "ba" ~ 0,
                dead == "di" ~ 0,
                dead == "t" ~ 0,
                dead == "cs" ~ 0,
                .default = 0
            ),
            
            total_pts = move_pts + auto_coral_l1_pts + auto_coral_l2_pts + 
                auto_coral_l3_pts + auto_coral_l4_pts + coral_l1_pts + coral_l2_pts + 
                coral_l3_pts + coral_l4_pts + net_pts + processor_value + endgame_pts,
            
            #auto
            total_auto_pts = auto_coral_l1_pts + auto_coral_l2_pts + 
                auto_coral_l3_pts + auto_coral_l4_pts + move_pts,
            #something
            
            #tele
            total_algae_pts = net_pts + processor_value,
            
            total_tele_coral_pts =  coral_l1_pts + coral_l2_pts + 
                coral_l3_pts + coral_l4_pts,
            
            total_tele_pts =  total_tele_coral_pts +total_algae_pts,
            
            #algae
            #algae_net_shots = sum(net_pts + net_missed)
        )
    })
    
    past_raw_team_data <- reactive({
        req(mldf())
        
        mldf() %>% 
            group_by(team) %>%
        summarize(
            "M#" = match,
            "Move %" = as.character(move),
            "Auto coral cycle" = auto_coral_L1_num + auto_coral_L2_num  + auto_coral_L3_num + auto_coral_L4_num,
            "Auto pts+3" = total_auto_pts,
            
            "Tele coral cycle" = coral_L1_num + coral_L2_num + coral_L3_num + coral_L4_num,
            "Tele coral miss" = tele_missed,
            
            L1 = coral_L1_num,
            L2 = coral_L2_num,
            L3 = coral_L3_num,
            L4 = coral_L4_num,
            
            Net = robot_net_score,
            "Net miss" = robot_net_miss, 
            
            Proc = proc_score,
            Endgame = ending,
            De_reef = as.character(robot_reef_removal),
            
            "Tele pts" = total_tele_pts,
            "Total pts" = total_pts,
            
            
            Dead = paste(dead, "/", 1),
            Fouls = fouls,
            "Driver Rate" = driver,
            "Defense Rate" = defense
        )
    })
    
    consolidated_team_data <- reactive({
        req(mldf())
        
        mldf() %>%
        group_by(team) %>%
        summarize(
            
            l1_cycle_tele = round(mean(coral_L1_num), digits = 2),
            l2_cycle_tele = round(mean(coral_L2_num), digits = 2),
            l3_cycle_tele = round(mean(coral_L3_num), digits = 2),
            l4_cycle_tele = round(mean(coral_L4_num), digits = 2),
            
            
            coral_cycle_tele = round(sum(l1_cycle_tele+l2_cycle_tele+l3_cycle_tele+l4_cycle_tele), digits = 2),
            
            coral_tele_success_rate = round(mean(coral_tele_success_rate_raw, digits = 2)),
            
            coral_cycle_auto = round(sum(mean(auto_coral_L1_num) + mean(auto_coral_L2_num) + mean(auto_coral_L3_num) + mean(auto_coral_L4_num)), digits = 2),
            
            coral_auto_success_rate = round(mean(coral_auto_success_rate_raw, digits = 2)),
            
            n_corals = round(coral_cycle_tele + coral_cycle_auto, digits = 2),
            
            #algae cycle
            net_cycle = round(mean(robot_net_score), digits = 2),
            proc_cycle = round(mean(proc_score), digits = 2),
            
            net_accuracy = round(mean(net_success_rate_raw, digits = 2)),
            
            algae = round((sum(net_cycle+proc_cycle)),digits = 2),
            cycles = round(n_corals + algae, digits = 2),
            algae_ground_pct = round(sum(robot_algae_picked, na.rm = TRUE) / n(), digits = 2),
            
            #total points
            total_pts_mean = round(mean(total_pts, na.rm = TRUE), digits =2), 
            
            #teleop
            tele_pts_mean = round(mean(total_tele_pts, na.rm = TRUE), digits =2),
            
            #auto
            auto_pts_mean = round(mean(total_auto_pts, na.rm = TRUE), digits =2), 
            
            #endgame
            endgame_pts_mean =round(mean(endgame_pts, na.rm = TRUE), digits =2),
                
            #put in climb success rate
            
            #algae remove data
            algae_remove_pct = round(sum(c(robot_reef_removal))/n(), digits = 2),
            algae_ground_intake_pct = round(sum(c(robot_algae_picked))/n(), digits = 2),
            
            move_pct = round(sum(c(move))/n(), digits = 2),
            
            dead_times = paste(sum(c(dead)),"/",n()),
            
            dead_pct = round(sum(c(dead))/n(), digits = 2),
            
            driver_rating_mean = round(mean(driver[driver != 0], na.rm = TRUE), digits =2),
            defense_rating_mean = round(mean(defense[defense != 0], na.rm = TRUE), digits =2)
        )
    })
    
    #Event Summary Tab
    #BUBBLE LOGIC
    bubble_graph <- function(raw) {
        bubble <- raw %>%
        group_by(team)%>%
        summarise(
            match = n(),
            total_coral_cycles = sum(
                auto_coral_L1_num + auto_coral_L2_num + auto_coral_L3_num + 
                    auto_coral_L4_num + coral_L1_num + coral_L2_num + 
                    coral_L3_num + coral_L4_num
                ) / n(),
            
            total_algae_cycles = sum(
                robot_net_score + proc_score
            ) / n(),
            
            endgame_score = sum(ifelse(ending =="D", 12, 
                                ifelse(ending =="S",6,
                                ifelse(ending =="P", 2, 0))))/n()
            
        )
        ggplot(bubble, aes(x = total_coral_cycles, y = total_algae_cycles, 
            labs(title = "Teams Performance Summary", 
                 x = "Coral Cycles (auto + teleop)", 
                 y = "Algae Cycles", 
                 size = "Endgame")+
            theme_bw()
    }
    
    #LARGE SCORING SUMMARY
    long_column <- function(raw) {
        column4 <- raw %>%
            group_by(team) %>%
            summarise(
                match = n(),
                auto_coral_L1 = sum(auto_coral_L1_num*3)/n(),
                auto_coral_L2 = sum(auto_coral_L2_num*4)/n(),
                auto_coral_L3 = sum(auto_coral_L3_num*6)/n(),
                auto_coral_L4 = sum(auto_coral_L4_num*7)/n(),
                move_pts = sum(move*3)/n(),
                tele_coral_L1 = sum(coral_L1_num*2)/n(),
                tele_coral_L2 = sum(coral_L2_num*3)/n(),
                tele_coral_L3 = sum(coral_L3_num*4)/n(),
                tele_coral_L4 = sum(coral_L4_num*5)/n(),
                robot_net_score = sum(robot_net_score*4)/n(),
                robot_proc_score = sum(proc_score*2.5)/n(),
                endgame_score = sum(ifelse(ending =="D", 12,
                                    ifelse(ending =="S", 6,
                                    ifelse(ending =="P", 2, 0)))
                                    )/n(),
                
                avg_score = auto_coral_L1 + auto_coral_L2 + auto_coral_L3 + auto_coral_L4 + move_pts +
                    tele_coral_L1 + tele_coral_L2 + tele_coral_L3 + tele_coral_L4 +
                    robot_net_score + robot_proc_score +
                    endgame_score
            ) %>%
            
            pivot_longer(cols = c(auto_coral_L1, auto_coral_L2, auto_coral_L3, auto_coral_L4, move_pts,
                                  tele_coral_L1, tele_coral_L2, tele_coral_L3, tele_coral_L4,
                                  robot_net_score, robot_proc_score,
                                  endgame_score), 
                         names_to = "level", 
                         values_to = "score")
        
        # Sort data by avg_score first to get unique teams in order
        unique_teams <- column4 %>%
            select(team, avg_score) %>%
            distinct() %>%
            arrange(desc(avg_score))
        
        # Use the unique sorted teams to create a proper factor
        column4$team <- factor(column4$team, 
                               levels = unique_teams$team, 
                               ordered = TRUE)
        column4$level_score <- column4$score  # Simplified since case_when wasn't changing values
        
        ggplot(column4, aes(x = team, y = level_score, fill = level, text = paste("Average Score: ", avg_score))) + 
            geom_bar(position = "stack", stat = "identity") + 
            labs(title = "Scoring Summary", 
                 x = "Team", y = "Total Score with Coral", fill = "Level") +
            scale_fill_manual(values = c("plum1",
                                         "plum2",
                                         "plum3",
                                         "plum4",
                                         "#FFF68F",
                                         "#FFC156", 
                                         "olivedrab3",
                                         "springgreen4", 
                                         "steelblue2",
                                         "steelblue3",
                                         "steelblue",
                                         "steelblue4"),
                              labels = c("auto_coral_L1" = "Auto Coral L1", 
                                         "auto_coral_L2" = "Auto Coral L2",
                                         "auto_coral_L3" = "Auto Coral L3", 
                                         "auto_coral_L4" = "Auto Coral L4",
                                         "move_pts" = "Move",
                                         "tele_coral_L1" = "Tele Coral L1", 
                                         "tele_coral_L2" = "Tele Coral L2", 
                                         "tele_coral_L3" = "Tele Coral L3", 
                                         "tele_coral_L4" = "Tele Coral L4", 
                                         "endgame_score" = "Endgame", 
                                         "robot_net_score" = "Net", 
                                         "robot_proc_score" = "Processor")
            )+
            theme_bw()+
            coord_flip()
    }
    
    check <- function(raw){
        double_check <- raw[FALSE,]
        for (i in 1:(nrow(raw))){
            if(!(is.na(raw[i, 4])) & length(grep(raw[i, 4], teams))==0){
                double_check <- rbind(double_check, raw[i,])
            }
            for (j in (i):nrow(raw)){
                if (!is.na(raw[i, 2]) & !is.na(raw[j, 2]) & raw[i,2] == raw[j,2] & 
                    !is.na(raw[i, 4]) & !is.na(raw[j, 4]) & raw[i,4] == raw[j,4] &
                    i!=j){
                    double_check <- rbind(double_check, raw[i,])
                    double_check <- rbind(double_check, raw[j,])
                }
            }
        }
        return(double_check)
    }
    
    schedule <- function(raw, match_schedule){
        #find our next match
        #find our next next match
        #want all teams from next next match (to scout)
        #want friend teams from next next match (to organize)
        priority_matches <- data.frame(matrix(ncol = 3, nrow = 0))
        colnames(priority_matches) = c("match", "team", "role")

        current_match = max(raw[,2])
        next_match = which(match_schedule == 449, arr.ind = TRUE)[,1]
        next_match = sort(next_match)
        temp_1 = next_match[next_match>current_match][1]
        temp_2 = next_match[next_match>current_match][2]
        if(is.na(temp_1) | is.na(temp_2)){ return(priority_matches)}
        teams_next_next_match = match_schedule[temp_2,]
        for (team in teams_next_next_match){
            #browser()
            if (team != 449 & team!=temp_2){
                team_next_next_match = which(match_schedule == team, arr.ind = TRUE)[,1]
                team_next_next_match = sort(team_next_next_match)
                team_temp_1 = team_next_next_match[team_next_next_match>current_match][1]
                team_temp_2 = team_next_next_match[team_next_next_match>current_match][2]
                #figuring out which matches needed to scout
                if (team_temp_1 > current_match & team_temp_1 < temp_1){
                    priority_matches[nrow(priority_matches)+1,] = c(team_temp_1, team, "to scout")
                }
                
                #figuring out which match can talk
                if (team_temp_1 > temp_1 & team_temp_1 < temp_2){
                    priority_matches[nrow(priority_matches)+1,] = c(team_temp_1, team, "can talk after")
                }
            }
        }
        return(priority_matches)
    }
    
    

    #UI Event Summary Rendering Plots
    output$long_column_output <- renderPlot({
        #Bubble graph logic
        long_column(raw())
    })
    
    output$picklist_graph <- renderPlot({
        #Picklisting graph logic
        bubble_graph(raw())
    })
    
    output$picklist_table <- renderDT({
        #Picklisting table logic
        datatable(consolidated_team_data(), options = list(dom = "ft", lengthChange = FALSE, rowNames = FALSE, scrollX = TRUE, scrollY = 500, pageLength = nrow(consolidated_team_data())))
    })

    #UI Checking Data
    observeEvent(input$check, {
        showModal(modalDialog(
            h4(
                renderText("Problematic data will be shown below.")
            ),
            DTOutput("checked_data")
        ))
    })
    
    output$checked_data <- renderDT({
        errors <- check(raw)
        datatable(errors, options = list(dom = "ft", lengthChange = FALSE, rowNames = FALSE, scrollX = TRUE, scrollY = 500, pageLength = nrow(errors)))
    })
    
    #UI Showing Priority Schedule
    observeEvent(input$schedule, {
        showModal(modalDialog(
            h4(
                renderText("Priority Matches/Teams to Scout Below")
            ),
            DTOutput("priority_schedule")
        ))
    })
    
    output$priority_schedule <- renderDT({
        temp <- schedule(raw(), match_schedule())
        datatable(temp, options = list(dom = "ft", lengthChange = FALSE, rowNames = FALSE, scrollX = TRUE, scrollY = 500, pageLength = nrow(temp)))
    })
    
    
    observeEvent(input$vagle, {
        load_event_data("vagle")
    })
    observeEvent(input$mdsev, {
        load_event_data("mdsev")
    })
    observeEvent(input$dchamp, {
        load_event_data("dchamp")
    })
    observeEvent(input$newton, {
        load_event_data("newton")
    })
    observeEvent(input$all_data, {
        load_event_data("all_data")
    })
    observeEvent(input$vacri, {
        load_event_data("vacri")
    })
    
    #Alliance/Match Tab
    #ALL GRAPH GENERATING FUNCTIONS------------------------------------------------------------------
    #BOXPLOT GRAPH
    
    predict_scores <- function(raw, red_alliance = c(params$red1, params$red2, params$red3), 
                               blue_alliance = c(params$blue1, params$blue2, params$blue3)) {
        scores <- raw %>%
            filter(team %in% c(red_alliance, blue_alliance)) %>%
            mutate(team = factor(team, c(red_alliance, blue_alliance))) %>%
            mutate(total_score = 
                    (coral_L1_num*2) + (coral_L2_num*3) + 
                    (coral_L3_num*4) + (coral_L4_num*5) + 
                    (auto_coral_L1_num*3) + (auto_coral_L2_num*4)+ 
                    (auto_coral_L3_num*5)+ (auto_coral_L4_num*7) +
                    (robot_net_score*4) + (proc_score*6) + 
                    ifelse(ending =="D", 12, ifelse(ending=="S",6,ifelse(ending=="P", 2, 0))) + 
                    (move*3)
                ) %>%
            group_by(team) %>%
            summarise(mean_score = mean(total_score),
                      mean_proc = mean(proc_score))
        
        red_alliance_score = sum(ifelse(scores$team %in% red_alliance, scores$mean_score, 0)) + 
                             sum(ifelse(scores$team %in% blue_alliance, scores$mean_proc*4, 0))
        blue_alliance_score = sum(ifelse(scores$team %in% blue_alliance, scores$mean_score, 0)) +
                             sum(ifelse(scores$team %in% red_alliance, scores$mean_proc*4, 0))
        
        
        paste0("Predicted Scores: ", 
               "<span style='color:red;'>", round(red_alliance_score, digits = 0), 
               "<span style='color:black;'>", " - ", 
               "<span style='color:blue;'>", round(blue_alliance_score, digits = 0))
    }
    
    boxplot_graph_alliance <- function(raw, red_alliance = c(params$red1, params$red2, params$red3), 
                                       blue_alliance = c(params$blue1, params$blue2, params$blue3)) {
        boxplot <- raw %>%
            filter(team %in% c(blue_alliance,red_alliance)) %>%
            mutate(team = factor(team, c(blue_alliance,red_alliance)))%>%
            
            mutate(total_coral_score = 
                       (coral_L1_num*2) + (coral_L2_num*3) + 
                       (coral_L3_num*4) + (coral_L4_num*5) + 
                       (auto_coral_L1_num*3) + (auto_coral_L2_num*4)+ 
                       (auto_coral_L3_num*5)+ (auto_coral_L4_num*7),
                   
                   total_algae_score = 
                       (robot_net_score*4) + (proc_score*2.5),
                   
                   total_endgame_score = 
                       ifelse(ending =="D", 12, ifelse(ending=="S",6,ifelse(ending=="P", 2, 0))),
                   
                   total_misc_score = 
                       (move * 3)
            )
        
        boxplot$total = boxplot$total_algae_score + boxplot$total_coral_score+boxplot$total_endgame_score+boxplot$total_misc_score
        
        ggplot(boxplot,aes(x = total, y = team))+    
            geom_boxplot(position = "dodge2", fill = "azure2") +
            stat_boxplot(geom = "errorbar") + 
            stat_summary(fun = mean, geom="point", size=3, color="orange")+
            labs(title = "Total points scored",x = "Points", y = "Team")+
            theme_bw()+
            theme(
                axis.text.y = element_text(color = ifelse(levels(boxplot$team) %in% 
                                                              red_alliance, "red", "blue"),
                                           size = 15)
            )
    }
    
    #CORAL LEVEL GRAPH ALLIANCE
    tele_coral_alliance <- function(raw, red_alliance = c(params$red1, params$red2, params$red3), 
                                    blue_alliance = c(params$blue1, params$blue2, params$blue3)) {
        
        bar <- raw %>%
            filter(team %in% c(red_alliance, blue_alliance)) %>%
            mutate(team = factor(team, levels = c(red_alliance, blue_alliance))) %>%
            group_by(team) %>%
            summarize(
                l1 = mean(coral_L1_num),
                l2 = mean(coral_L2_num),
                l3 = mean(coral_L3_num),
                l4 = mean(coral_L4_num)
            ) %>%
            pivot_longer(cols = c(l4, l3, l2, l1), 
                         names_to = "level", 
                         values_to = "coral_num")
        
        bar$level_score <- case_when(
            bar$level == "l1" ~ bar$coral_num*2, 
            bar$level == "l2" ~ bar$coral_num*3, 
            bar$level == "l3" ~ bar$coral_num*4, 
            bar$level == "l4" ~ bar$coral_num*5
        )
        
        ggplot(bar, aes(x = factor(team), y = level_score, fill = level)) + 
            geom_bar(position = "stack", stat = "identity", 
                     color = ifelse(bar$team %in% red_alliance, "red", "blue"),
                     size = 0.5) + 
            labs(title = "Tele Coral Summary", 
                 x = "Team", y = "Coral score", fill = "Level") +
            scale_fill_manual(values=c("lightskyblue","royalblue1","royalblue3","navy")) +
            theme_bw()
        }
    
    #CORAL AUTO  
    auto_coral_alliance <- function(raw, red_alliance = c(params$red1, params$red2, params$red3), 
                                    blue_alliance= c(params$blue1, params$blue2, params$blue3)) {
        
        auto <- raw %>%
            filter(team %in% c(red_alliance, blue_alliance)) %>%
            mutate(team = factor(team, c(red_alliance,blue_alliance) )) %>%
            
            group_by(team) %>%
            summarize(
                matches = n(),
                autol1 = mean((auto_coral_L1_num*3)),
                autol2 = mean(auto_coral_L2_num*4),
                autol3 = mean(auto_coral_L3_num*6),
                autol4 = mean(auto_coral_L4_num*7),
                move_pts = mean(move*3)
            )%>%
            
            pivot_longer(cols = c(autol1,autol2,autol3,autol4,move_pts), 
                         names_to = "level", 
                         values_to = "points")
        
        
        ggplot(auto, aes(x = factor(team), y = points, fill = level)) + 
            geom_bar(position = "stack", stat = "identity", 
                     color = ifelse(auto$team %in% red_alliance, "red", "blue"),
                     size = 0.5) + 
            labs(title = "Move + Auto Coral Summary", 
                 x = "Team", y = "Coral Scored Points", fill = "Level") +
            scale_fill_manual(values=c("#f2cbfe","plum2","plum3","plum4", "#FFD700"))+
            theme_bw() 
    }
    
    
    #  ALGAE NET AND PROC GRAPH
    algae_bar <- function(raw, red_alliance = c(params$red1, params$red2, params$red3),
                          blue_alliance= c(params$blue1, params$blue2, params$blue3)) {
        
        algae <- raw %>%
            filter(team %in% c(red_alliance, blue_alliance)) %>%
            mutate(team = factor(team, c(red_alliance, blue_alliance))) %>%
            group_by(team) %>%
            summarize(mathes = n(),
                      net = sum(robot_net_score, na.rm = TRUE)/n(),
                      proc = sum(proc_score, na.rm = TRUE)/n()) %>%
            
            pivot_longer(cols = c(net, proc), 
                         names_to = "type", 
                         values_to = "points")
        
        
        ggplot(algae, aes(x = team, y = points, fill = type)) + 
            geom_bar(position = "stack", stat = "identity", 
                     color = ifelse(algae$team %in% red_alliance, "red", "blue"),
                     size = 0.8
            ) + 
            labs(title = "Algae Points Summary", 
                 x = "Team", y = "Algae Points", fill = "Place")+
            
            
            scale_fill_manual(values=c("#008B8B","darkslategray2"))  +
            theme_bw() 
    }
    
    
    #ENDGAME BAR GRAPH
    endgame_graph <- function(raw, red_alliance = c(params$red1, params$red2, params$red3),
                              blue_alliance= c(params$blue1, params$blue2, params$blue3)) {
        
        end <- raw %>%
            filter(team %in% c(red_alliance, blue_alliance)) %>%
            mutate(team = factor(team, c(red_alliance,blue_alliance) )) %>%
            
            group_by(team, ending) %>%
            summarise(count = n())
        
        end$points <- recode(end$ending,  "P" = 2, "S" = 6, "D" = 12, "No" = 0)
        
        #compute the total end game score from all matches
        end$total_points <- end$count * end$points
        
        end$ending <- factor(end$ending, levels = c("D", "S","P","No"))
        
        ggplot(end, aes(x = factor(team), y = total_points, fill = ending)) +
            geom_bar(position= "stack", stat = "identity", 
                     color = ifelse(end$team %in% red_alliance, "red", "blue"),
                     size = 0.8) +
            
            labs(title = "Endgame Score",
                 x = "Team",
                 y = "Points") +
            scale_fill_manual(
                values = c("D" = "#BF9B2E", "S" = "#FBC901", "P" = "#FFF68F"),
                labels = c("D" = "Deep", "S" = "Shallow", "P" = "Park", "ending" = "Cage")
            ) +
            theme_bw() 
    }
    
    output$field_image_output <- renderImage({
        img_src <- paste0("images/reefscapeField.png")  #Path to the image
        no_img_available_src <- paste0("images/", "no_image_available", ".jpg")
        
        #Check if the image file exists
        if (file.exists(img_src)) {
            return(list(
                src = img_src,
                contentType = "image/png",
                width = 380,
                height = 750,
                alt = paste("Field Image for Reefscape"),
                style="display: block; margin-left: auto; margin-right: auto;"
            ))
        } else {
            return(list(
                src = no_img_available_src,
                contentType = "image/jpg",
                width = 350,
                height = 350,
                alt = paste("No Image Available"),
                style="display: block; margin-left: auto; margin-right: auto;"
            ))
        }
    }, deleteFile = FALSE)
    
    
    
    #ALL BAR GRAPH
    long_column_alliance <- function(raw, red_alliance = c(params$red1, params$red2, params$red3),
                                     blue_alliance= c(params$blue1, params$blue2, params$blue3)) {
        
        column4 <- raw %>%
            filter(team %in% c(red_alliance, blue_alliance)) %>%
            mutate(team = factor(team, c(red_alliance,blue_alliance) )) %>%
            group_by(team) %>%
            summarise(
                match = n(),
                auto_coral_L1 = sum(auto_coral_L1_num*3)/n(),
                auto_coral_L2 = sum(auto_coral_L2_num*4)/n(),
                auto_coral_L3 = sum(auto_coral_L3_num*6)/n(),
                auto_coral_L4 = sum(auto_coral_L4_num*7)/n(),
                move_pts = sum(move*3)/n(),
                tele_coral_L1 = sum(coral_L1_num*2)/n(),
                tele_coral_L2 = sum(coral_L2_num*3)/n(),
                tele_coral_L3 = sum(coral_L3_num*4)/n(),
                tele_coral_L4 = sum(coral_L4_num*5)/n(),
                robot_net_score = sum(robot_net_score*4)/n(),
                robot_proc_score = sum(proc_score*2.5)/n(),
                
                endgame_score = sum(ifelse(ending =="D", 12,
                                    ifelse(ending =="S", 6,
                                    ifelse(ending =="P", 2, 0)))
                                    )/n(),
                
                avg_score = auto_coral_L1 + auto_coral_L2 + auto_coral_L3 + auto_coral_L4 + move_pts +
                    tele_coral_L1 + tele_coral_L2 + tele_coral_L3 + tele_coral_L4 +
                    robot_net_score + robot_proc_score +
                    endgame_score
            ) %>%
            
            pivot_longer(cols = c(auto_coral_L1, auto_coral_L2, auto_coral_L3, auto_coral_L4, move_pts,
                                  tele_coral_L1, tele_coral_L2, tele_coral_L3, tele_coral_L4,
                                  robot_net_score, robot_proc_score,
                                  endgame_score), 
                         names_to = "level", 
                         values_to = "score")
        
        ggplot(column4, aes(x = factor(team), y = score, fill = level)) + 
            geom_bar(position = "stack", stat = "identity") + 
            labs(title = "Scoring Summary", 
                 x = "Team", y = "Total Score with Coral", fill = "Level") +
            scale_fill_manual(values = c("plum1","plum2","plum3","plum4",
                                         "#FFF68F","#FFC156","olivedrab3","springgreen4", 
                                         "steelblue2","steelblue3","steelblue","steelblue4"),
                              labels = c("auto_coral_L1" = "Auto Coral L1", 
                                         "auto_coral_L2" = "Auto Coral L2",
                                         "auto_coral_L3" = "Auto Coral L3", 
                                         "auto_coral_L4" = "Auto Coral L4",
                                         "move_pts" = "Move",
                                         "tele_coral_L1" = "Tele Coral L1", 
                                         "tele_coral_L2" = "Tele Coral L2", 
                                         "tele_coral_L3" = "Tele Coral L3", 
                                         "tele_coral_L4" = "Tele Coral L4", 
                                         "endgame_score" = "Endgame", 
                                         "robot_net_score" = "Net", 
                                         "robot_proc_score" = "Processor")
            )+
            theme_bw()+
            coord_flip()+
            theme( 
                axis.text.y = element_text(size = 15)
            )
    }
    
    
    
    output$field_image_output <- renderImage({
        img_src <- paste0("images/reefscapeField.png")  #Path to the image
        no_img_available_src <- paste0("images/", "no_image_available", ".jpg")
        
        #Check if the image file exists
        if (file.exists(img_src)) {
            return(list(
                src = img_src,
                contentType = "image/png",
                width = 380,
                height = 750,
                alt = paste("Field Image for Reefscape"),
                style="display: block; margin-left: auto; margin-right: auto;"
            ))
        } else {
            return(list(
                src = no_img_available_src,
                contentType = "image/jpg",
                width = 350,
                height = 350,
                alt = paste("No Image Available"),
                style="display: block; margin-left: auto; margin-right: auto;"
            ))
        }
    }, deleteFile = FALSE)
    
    
    #Server logic for generating alliance graph
    observeEvent(input$generate_graph, {
        #Backend team splitting based on user input
        if (input$match_or_teams == "Match Number") {
            match_num <- input$match_num
            
            #Get the specific row first
            match_row <- match_schedule()[match_schedule()$Match == match_num, ]
            
            #Extract teams as vectors
            selected_red_teams <- c(
                match_row$R1,
                match_row$R2,
                match_row$R3
            )
            
            selected_blue_teams <- c(
                match_row$B1,
                match_row$B2,
                match_row$B3
            )
            
            print("Selected match number:")
            print(match_num)
            print("Red teams:")
            print(selected_red_teams)
            print("Blue teams:")
            print(selected_blue_teams)
            
            matches_happened <- sort(unique(mldf()$match))
            
            if (match_num %in% matches_happened) {
                output$alliance_box_plot_output <- renderPlot({
                    match_data <- mldf() %>% filter(match == match_num)
                    
                    match_teams <- unique(match_data$team)
                    
                    match_row <- match_schedule()[match_schedule()$Match == match_num, ]
                    
                    red_teams <- c(match_row$R1, match_row$R2, match_row$R3)
                    blue_teams <- c(match_row$B1, match_row$B2, match_row$B3)
                    
                    team_match_data <- match_data %>%
                        group_by(team) %>%
                        summarize(
                            l1_cycle_tele = coral_L1_num,
                            l2_cycle_tele = coral_L2_num,
                            l3_cycle_tele = coral_L3_num,
                            l4_cycle_tele = coral_L4_num,
                            
                            coral_cycle_tele = sum(l1_cycle_tele, l2_cycle_tele, l3_cycle_tele, l4_cycle_tele),
                            coral_cycle_auto = sum(auto_coral_L1_num, auto_coral_L2_num, auto_coral_L3_num, auto_coral_L4_num),
                            
                            net_cycle = robot_net_score,
                            proc_cycle = proc_score,
                            algae = sum(net_cycle, proc_cycle),
                            
                            total_pts_mean = total_pts,
                            
                            tele_pts_mean = total_tele_pts,
                            
                            auto_pts_mean = total_auto_pts,
                            
                            endgame_pts_mean = endgame_pts,
                            
                            algae_remove_pct = as.numeric(robot_reef_removal),
                            move_pct = as.numeric(move),
                            dead_times = paste(dead, "/", 1),
                            dead_pct = as.numeric(dead),
                            driver_rating_mean = driver,
                            defense_rating_mean = defense
                        )
                    
                    match_points <- match_data %>%
                        mutate(
                            total_coral_score =
                                (coral_L1_num*2) + (coral_L2_num*3) +
                                (coral_L3_num*4) + (coral_L4_num*5) +
                                (auto_coral_L1_num*3) + (auto_coral_L2_num*4)+
                                (auto_coral_L3_num*5)+ (auto_coral_L4_num*7),
                            
                            total_algae_score =
                                (robot_net_score*4) + (proc_score*2.5),
                            
                            total_endgame_score =
                                ifelse(ending =="D", 12, ifelse(ending=="S",6,ifelse(ending=="P", 2, 0))),
                            
                            total_misc_score =
                                (move * 3)
                        )
                    
                    match_points$total <- match_points$total_algae_score + match_points$total_coral_score +
                        match_points$total_endgame_score + match_points$total_misc_score
                    
                    
                    boxplot_graph_alliance(raw(), selected_red_teams, selected_blue_teams)
                    
                    #base_plot +
                    #   geom_point(data = match_points,
                    #               aes(x = total, y = factor(team, levels = c(selected_red_teams, selected_blue_teams))),
                    #               color = "red", size = 4)
                })
            } 
        } else if (input$match_or_teams == "Select 6 Teams"){
            selected_red_teams <- input$red_teams
            selected_blue_teams <- input$blue_teams
            
            #Check for fewer than 3 teams for either alliance
            if (length(selected_red_teams) != 3 || length(selected_blue_teams) != 3) {
                showNotification("Please select exactly 3 teams for both the red and blue alliances.", type = "error")
                return()
            }
            
            #Check for duplicate teams
            all_selected_teams <- c(selected_red_teams, selected_blue_teams)
            if (length(unique(all_selected_teams)) != length(all_selected_teams)) {
                showNotification("Duplicate teams are not allowed across alliances. Please select unique teams.", type = "error")
                return()
            }
        } else if (input$match_or_teams == "2 Alliances"){
            red_alliance <- input$red_alliance
            blue_alliance <- input$blue_alliance
            
            #Get the rows
            red_alliance_row <- alliances()[alliances()$Alliance == red_alliance,]
            blue_alliance_row <- alliances()[alliances()$Alliance == blue_alliance,]
            
            #Extract teams as vectors
            selected_red_teams <- c(
                red_alliance_row$Captain,
                red_alliance_row$Pick.1,
                red_alliance_row$Pick.2,
                red_alliance_row$Pick.3
            )
            
            selected_blue_teams <- c(
                blue_alliance_row$Captain,
                blue_alliance_row$Pick.1,
                blue_alliance_row$Pick.2,
                blue_alliance_row$Pick.3
            )
        }
        
        output$score_prediction <- renderText({
            predict_scores(raw(), selected_red_teams, selected_blue_teams)
        })
        
        output$alliance_box_plot_output <- renderPlot({
            boxplot_graph_alliance(raw(), selected_red_teams, selected_blue_teams)
        })
        
        output$alliance_tele_coral_graph_output <- renderPlot({
            tele_coral_alliance(raw(), selected_red_teams, selected_blue_teams)
        })
        
        output$alliance_auto_coral_graph_output <- renderPlot({
            auto_coral_alliance(raw(), selected_red_teams, selected_blue_teams)
        }) 
        
        output$alliance_algae_bar_graph_output <- renderPlot({
            algae_bar(raw(), selected_red_teams, selected_blue_teams)
        }) 
        
        
        output$alliance_endgame_graph_output <- renderPlot({
            endgame_graph(raw(), selected_red_teams, selected_blue_teams)
        })
        
        
        output$alliance_all_graph_output <- renderPlot({
            long_column_alliance(raw(), selected_red_teams, selected_blue_teams)
        })
        
        
        output$alliance_table <- renderDT({
            alliance_dt_subset <- consolidated_team_data()[consolidated_team_data()$team %in% c(selected_red_teams, selected_blue_teams), ]
            #team_data_row <- consolidated_team_data()[consolidated_team_data()$team == selected_team]
            datatable(alliance_dt_subset, options = list(dom = "ft", lengthChange = FALSE, rowNames = FALSE, scrollX = TRUE, scrollY = 500, pageLength = nrow(alliance_dt_subset)))
        })
        
    })
    
    #PREVIOUS MATCH TAB
    observe({
        matches_happened <- sort(unique(mldf()$match))
        updateSelectInput(session, "match_num_PREVIOUS", choices = matches_happened)
    })
    
    observeEvent(input$match_num_PREVIOUS, {
        selected_match <- input$match_num_PREVIOUS
        
        match_data <- mldf() %>% filter(match == selected_match)
        match_teams <- unique(match_data$team)
        
        match_row <- match_schedule()[match_schedule()$Match == selected_match, ]
        
        red_teams <- c(match_row$R1, match_row$R2, match_row$R3)
        blue_teams <- c(match_row$B1, match_row$B2, match_row$B3)
        
        team_match_data <- match_data %>%
            group_by(team) %>%
            summarize(
                l1_cycle_tele = coral_L1_num,
                l2_cycle_tele = coral_L2_num,
                l3_cycle_tele = coral_L3_num,
                l4_cycle_tele = coral_L4_num,
                
                coral_cycle_tele = sum(l1_cycle_tele, l2_cycle_tele, l3_cycle_tele, l4_cycle_tele),
                coral_cycle_auto = sum(auto_coral_L1_num, auto_coral_L2_num, auto_coral_L3_num, auto_coral_L4_num),
                
                net_cycle = robot_net_score,
                proc_cycle = proc_score,
                algae = sum(net_cycle, proc_cycle),
                
                total_pts_mean = total_pts,
                
                tele_pts_mean = total_tele_pts,
                
                auto_pts_mean = total_auto_pts,
                
                endgame_pts_mean = endgame_pts,
                
                algae_remove_pct = as.numeric(robot_reef_removal),
                move_pct = as.numeric(move),
                dead_times = paste(dead, "/", 1),
                dead_pct = as.numeric(dead),
                driver_rating_mean = driver,
                defense_rating_mean = defense
            )
        output$MATCH_alliance_box_plot_output <- renderPlot({
            base_plot <- boxplot_graph_alliance(raw(), red_teams, blue_teams)
            
            match_points <- match_data %>%
                mutate(
                    total_coral_score =
                        (coral_L1_num*2) + (coral_L2_num*3) +
                        (coral_L3_num*4) + (coral_L4_num*5) +
                        (auto_coral_L1_num*3) + (auto_coral_L2_num*4)+
                        (auto_coral_L3_num*5)+ (auto_coral_L4_num*7),

                    total_algae_score =
                        (robot_net_score*4) + (proc_score*2.5),

                    total_endgame_score =
                        ifelse(ending =="D", 12, ifelse(ending=="S",6,ifelse(ending=="P", 2, 0))),

                    total_misc_score =
                        (move * 3)
                )

            match_points$total <- match_points$total_algae_score + match_points$total_coral_score +
                match_points$total_endgame_score + match_points$total_misc_score

            #base_plot +
                #geom_point(data = match_points,
                #           aes(x = total, y = factor(team, levels = c(red_teams, blue_teams))),
                #           color = "red", size = 4) +
                #labs(title = paste("Total points scored - Match", selected_match))
            
            #print(match_points$total)
            base_plot
        })
        
        output$MATCH_alliance_tele_coral_graph_output <- renderPlot({
            tele_coral_graph <- tele_coral_alliance(match_data, red_teams, blue_teams)
            tele_coral_graph + labs(title = paste("Tele Coral Summary - Match", selected_match))
        })
        
        output$MATCH_alliance_auto_coral_graph_output <- renderPlot({
            auto_coral_graph <- auto_coral_alliance(match_data, red_teams, blue_teams)
            auto_coral_graph + labs(title = paste("Move + Auto Coral Summary - Match", selected_match))
        })
        
        output$MATCH_alliance_algae_bar_graph_output <- renderPlotly({
            algae_graph <- algae_bar(match_data, red_teams, blue_teams)
            algae_graph + labs(title = paste("Algae Points Summary - Match", selected_match))
        })
        
        output$MATCH_alliance_endgame_graph_output <- renderPlot({
            endgame_plot <- endgame_graph(match_data, red_teams, blue_teams)
            endgame_plot + labs(title = paste("Endgame Score - Match", selected_match))
        })
        
        output$MATCH_alliance_all_graph_output <- renderPlot({
            all_graph <- long_column_alliance(match_data, red_teams, blue_teams)
            all_graph + labs(title = paste("Scoring Summary - Match", selected_match))
        })
        
        output$MATCH_alliance_table <- renderDT({
            datatable(team_match_data, options = list(dom = "ft", lengthChange = FALSE, rowNames = FALSE, scrollX = TRUE, scrollY = 500, pageLength = nrow(team_match_data)))
        })
    })
    
    #Compare Teams Tab
    #GRAPH GEN LOGIC-------------------------------------------
    
    #LARGE BAR GRAPH
    compare_teams_large_bar_graph <- function(raw, selected_teams){
        compare_bar_graph <- raw %>%
            filter(team %in% selected_teams) %>%
            group_by(team) %>%
            summarise(
                match = n(),
                auto_coral_L1 = sum(auto_coral_L1_num*3)/n(),
                auto_coral_L2 = sum(auto_coral_L2_num*4)/n(),
                auto_coral_L3 = sum(auto_coral_L3_num*6)/n(),
                auto_coral_L4 = sum(auto_coral_L4_num*7)/n(),
                move_pts = sum(move*3)/n(),
                tele_coral_L1 = sum(coral_L1_num*2)/n(),
                tele_coral_L2 = sum(coral_L2_num*3)/n(),
                tele_coral_L3 = sum(coral_L3_num*4)/n(),
                tele_coral_L4 = sum(coral_L4_num*5)/n(),
                robot_net_score = sum(robot_net_score*4)/n(),
                robot_proc_score = sum(proc_score*2.5)/n(),
                endgame_score = sum(ifelse(ending =="D", 12,
                                           ifelse(ending =="S", 6,
                                                  ifelse(ending =="P", 2, 0)))
                )/n(),
                
                avg_score = auto_coral_L1 + auto_coral_L2 + auto_coral_L3 + auto_coral_L4 + move_pts +
                    tele_coral_L1 + tele_coral_L2 + tele_coral_L3 + tele_coral_L4 +
                    robot_net_score + robot_proc_score +
                    endgame_score
            ) %>%
            
            pivot_longer(cols = c(auto_coral_L1, auto_coral_L2, auto_coral_L3, auto_coral_L4, move_pts,
                                  tele_coral_L1, tele_coral_L2, tele_coral_L3, tele_coral_L4,
                                  robot_net_score, robot_proc_score,
                                  endgame_score), 
                         names_to = "level", 
                         values_to = "score")
        
        ggplot(compare_bar_graph, aes(x = factor(team), y = score, fill = level)) + 
            geom_bar(position = "stack", stat = "identity") + 
            labs(title = "Scoring Summary", 
                 x = "Team", y = "Total Score with Coral", fill = "Level") +
            scale_fill_manual(values = c("plum1","plum2","plum3","plum4",
                                         "#FFF68F","#FFC156","olivedrab3","springgreen4", 
                                         "steelblue2","steelblue3","steelblue","steelblue4"),
                              labels = c("auto_coral_L1" = "Auto Coral L1", 
                                         "auto_coral_L2" = "Auto Coral L2",
                                         "auto_coral_L3" = "Auto Coral L3", 
                                         "auto_coral_L4" = "Auto Coral L4",
                                         "move_pts" = "Move",
                                         "tele_coral_L1" = "Tele Coral L1", 
                                         "tele_coral_L2" = "Tele Coral L2", 
                                         "tele_coral_L3" = "Tele Coral L3", 
                                         "tele_coral_L4" = "Tele Coral L4", 
                                         "endgame_score" = "Endgame", 
                                         "robot_net_score" = "Net", 
                                         "robot_proc_score" = "Processor")
            )+
            theme_bw()
    }
    
    #COMMENTS
    compare_teams_comments <- function(raw, selected_teams){
        comments2 <- raw %>%
            group_by(team)%>%
            filter(team %in% selected_teams)%>%
            mutate(team = as.factor(team))%>%
            summarise(
                wobbly = length(grep("2", comments)),
                wiffs = length(grep("4", comments)),
                `wobbly manip` = length(grep("1", comments)),
                `bad intake` = length(grep("5", comments)),
                defense = length(grep("0", comments)),
                `xtra long climb` = length(grep("6", comments))
            )%>%
            
            pivot_longer(cols = c(wobbly,
                                  wiffs, 
                                  `wobbly manip`,
                                  `bad intake`,
                                  defense,
                                  `xtra long climb`), 
                         names_to = "com", 
                         values_to = "level")
        
        team_colors <- setNames(hue_pal()(length(levels(comments2$team))), levels(comments2$team))
        ggplot(comments2, aes(x = com, y = level, fill = team)) + 
            geom_bar(position = "stack", stat = "identity")+
            labs(title = "Comments Summary", 
                 x = "Issues", y = "Frequency") +
            scale_fill_manual(values = team_colors) +
            theme_bw()
    }
    
    #PROBLEMS
    compare_teams_problems <- function(raw, selected_teams){
        problems2 <- raw %>%
            group_by(team)%>%
            filter(team %in% selected_teams)%>%
            mutate(team = as.factor(team))%>%
            summarise(
                coral_stuck = length(grep("cs", dead)),
                algae_beach = length(grep("ba", dead)),
                disabled = length(grep("di", dead)),
                `dead` = length(grep("de", dead)),
                tipped = length(grep("t", dead))
            )%>%
            
            pivot_longer(cols = c(coral_stuck,
                                  algae_beach, 
                                  disabled,
                                  `dead`,
                                  tipped), 
                         names_to = "labels", 
                         values_to = "count")
        
        team_colors <- setNames(hue_pal()(length(levels(problems2$team))), levels(problems2$team))
        ggplot(problems2, aes(x = labels, y = count, fill = team)) + 
            geom_bar(position = "stack", stat = "identity") + 
            labs(title = "Dead Summary", 
                 x = "Issues", y = "Frequency") +
            scale_fill_manual(values = team_colors) +
            theme_bw()
    }
    
    #POINTS OVER TIME
    compare_teams_past_points <- function(raw, selected_teams){
        past_match_points <- raw %>%
            group_by(team)%>%
            filter(team %in% selected_teams)%>%
            mutate(team = as.factor(team))%>%
            summarize(
                team = team,
                match = match,
                total_score = 
                    move*3 +
                    auto_coral_L1_num*3 + auto_coral_L2_num*4 + auto_coral_L3_num*6 + auto_coral_L4_num*7 +
                    robot_net_score*4 + proc_score*2.5 +
                    coral_L1_num*2 + coral_L2_num*3 + coral_L3_num*4 + coral_L4_num*5 +
                    ifelse(ending =="D", 12, ifelse(ending=="S",6,ifelse(ending=="P", 2, 0))),
            )
        team_colors <- setNames(hue_pal()(length(levels(past_match_points$team))), levels(past_match_points$team))
        ggplot(past_match_points, aes(x = match, y = total_score, color = team, group = team)) + 
            geom_line(size = 1) +
            geom_point(size = 2) + 
            scale_fill_manual(values = team_colors) +
            scale_y_continuous(limits = c(0, max(past_match_points$total_score, na.rm = TRUE))) +
            scale_x_continuous(breaks = unique(past_match_points$match)) +
            labs(title = "Points Over Time", x = "Match", y = "Total Score", color = "Team") +
            theme_minimal()
    }
    
    #DRIVER OVER TIME
    compare_teams_past_driver <- function(raw, selected_teams){
        past_match_points <- raw %>%
            group_by(team)%>%
            filter(team %in% selected_teams)%>%
            mutate(team = as.factor(team))%>%
            summarize(
                team = team,
                match = match,
                driver = driver
            )
        team_colors <- setNames(hue_pal()(length(levels(past_match_points$team))), levels(past_match_points$team))
        ggplot(past_match_points, aes(x = match, y = driver, color = team, group = team)) + 
            geom_line(size = 1) +
            geom_point(size = 2) + 
            scale_fill_manual(values = team_colors) +
            scale_y_continuous(limits = c(0, 5, na.rm = TRUE)) +
            scale_x_continuous(breaks = unique(past_match_points$match)) +
            labs(title = "Driver Ranking Over Time", x = "Match", y = "Driver Ranking", color = "Team") +
            theme_minimal()
    }
    
    output$compare_teams_data_row <- renderDT({
        #Reasoning behind this is I found it hard to read by continuously scrolling horizantally
        #so I made it vertical. Only problem is that team is no longer the column name...
        selected_teams <- input$teams_selected
        filtered_data <- consolidated_team_data()[consolidated_team_data()$team %in% selected_teams,]
        filtered_data <- filtered_data[, !names(filtered_data) %in% "team"]
        flipped_data <- as.data.frame(t(filtered_data))
        colnames(flipped_data) <- selected_teams
        datatable(flipped_data, options = list(dom = "ft", lengthChange = FALSE, rowNames = FALSE, scrollX = TRUE, scrollY = 500, pageLength = nrow(flipped_data)))
    })
    
    
    #Single Team Tab
    #GRAPH GEN LOGIC-------------------------------------------
    
    #BOX PLOT
    boxplot_graph_single <- function(raw, team_num) {
        
        boxplot <- raw %>%
            filter(team == team_num) %>%
            mutate(total_coral_score = 
                       (coral_L1_num*2) + (coral_L2_num*3) + 
                       (coral_L3_num*4) + (coral_L4_num*5) + 
                       (auto_coral_L1_num*3) + (auto_coral_L2_num*4)+ 
                       (auto_coral_L3_num*5)+ (auto_coral_L4_num*7),
                   
                   total_algae_score = 
                       (robot_net_score*4) + (proc_score*2.5),
                   
                   total_endgame_score = 
                       ifelse(ending =="D", 12, ifelse(ending=="S",6,ifelse(ending=="P", 2, 0))),
                   
                   total_misc_score = 
                       (move*3)
            )
        
        boxplot$total = boxplot$total_algae_score + boxplot$total_coral_score + boxplot$total_endgame_score + boxplot$total_misc_score
        
        ggplot(boxplot, aes(x = total, y = as.character(team))) +    
            geom_boxplot(position = "dodge2", fill = "azure2") +
            stat_boxplot(geom = "errorbar") + 
            stat_summary(fun = mean, geom = "point", size = 5, color = "orange") +
            labs(title = paste("Total points scored by Team", team_num),
                 x = "Points", 
                 y = "Team") +
            theme_bw() 
        #coord_fixed(ratio = 20)
    }
    
    #TELE LEVEL
    tele_graph_single <- function(raw, team_num) {
        
        bar <- raw %>%
            filter(team == team_num) %>%
            group_by(team) %>%
            summarize(
                L1 = mean(coral_L1_num),
                L2 = mean(coral_L2_num),
                L3 = mean(coral_L3_num),
                L4 = mean(coral_L4_num),
                Net = mean(robot_net_score),
                Proc = mean(proc_score)
            ) %>%
            pivot_longer(cols = c(L4, L3, L2, L1, Net, Proc), 
                         names_to = "type", 
                         values_to = "points")
        
        bar$points_score <- case_when(
            bar$type == "L1" ~ bar$points*2, 
            bar$type == "L2" ~ bar$points*3, 
            bar$type == "L3" ~ bar$points*4, 
            bar$type == "L4" ~ bar$points*5,
            bar$type == "Net" ~ bar$points*4,
            bar$type == "Proc" ~ bar$points*6
        )
        
        p <- ggplot(bar, aes(x = factor(team), y = points_score, fill = type)) + 
            geom_bar(position = "stack", stat = "identity", width = 0.3) + 
            labs(title = paste("Level Summary for Team", team_num), 
                 x = "Team", y = "Tele score", fill = "Points") +
            scale_fill_manual(values=c("lightskyblue","royalblue1","royalblue3","navy", "#008B8B","darkslategray2")) +
            theme_bw()
        
        return(p)
    }
    
    #AUTO CORAL
    auto_graph_single <- function(raw, team_num) {
        auto <- raw %>%
            filter(team == team_num) %>%
            group_by(team) %>%
            summarize(
                matches = n(),
                autoMove = mean(move),
                autol1 = mean(auto_coral_L1_num),
                autol2 = mean(auto_coral_L2_num),
                autol3 = mean(auto_coral_L3_num),
                autol4 = mean(auto_coral_L4_num)
            ) %>%
            pivot_longer(cols = c(autoMove, autol1, autol2, autol3, autol4), 
                         names_to = "type", 
                         values_to = "level")
        
        auto$level_score <- case_when(
            auto$type == "autoMove" ~ auto$level*3,
            auto$type == "autol1" ~ auto$level*3,
            auto$type == "autol2" ~ auto$level*4,
            auto$type == "autol3" ~ auto$level*6,
            auto$type == "autol4" ~ auto$level*7
        )
        
        ggplot(auto, aes(x = factor(team), y = level_score, fill = type)) + 
            geom_bar(position = "stack", stat = "identity", width = 0.3) + 
            labs(title = paste("Level Summary for Team", team_num), 
                 x = "Team", y = "Level score", fill = "Level") +
            scale_fill_manual(values=c("plum1","plum2","plum3","plum4", "#FFC156"),
                              labels=c("autoMove" = "Move",
                                       "autol1" = "Auto L1", "autol2" = "Auto L2", "autol3" = "Auto L3", "autol4" = "Auto L4")
            ) +
            theme_bw()
    }
    
    #ALGAE POINTS GRAPH
    algae_bar_single <- function(raw, team_num) {
        algae <-  raw %>%
            filter(team == team_num) %>%
            group_by(team, ending) %>%
            summarize(mathes = n(),
                      net = sum(robot_net_score, na.rm = TRUE)/n(),
                      proc = sum(proc_score, na.rm = TRUE)/n()) %>%
            
            pivot_longer(cols = c(net, proc), 
                         names_to = "type", 
                         values_to = "points")
        
        ggplot(algae, aes(x = factor(team), y = points, fill = type)) + 
            geom_bar(position = "stack", stat = "identity", width = 0.3)+ 
            labs(title = "Algae Points Summary", 
                 x = "Team", y = "Algae Points", fill = "Place")+
            scale_fill_manual(values=c("#008B8B","darkslategray2")) 
    }
    
    #END GAME
    endgame_graph_single <- function(raw, team_num) {
        end <- raw %>%
            filter(team == team_num) %>%
            group_by(team, ending) %>%
            summarise(count = n())
        
        end$points <- recode(end$ending, "P" = 2, "S" = 6, "D" = 12, "No" = 0)
        
        end$total_points <- end$count * end$points
        
        end$ending <- factor(end$ending, levels = c("D", "S", "P", "No"))
        
        ggplot(end, aes(x = factor(team), y = total_points, fill = ending)) +
            geom_bar(position = "stack", stat = "identity", width = 0.3) +
            labs(title = paste("Endgame Score for Team", team_num),
                 x = "Team",
                 y = "Points") +
            scale_fill_manual(
                values = c("D" = "#BF9B2E", "S" = "#FBC901", "P" = "#FFF68F"),
                labels = c("D" = "Deep", "S" = "Shallow", "P" = "Park", "ending" = "Cage")
            ) +
            theme_bw() 
    }
    
    #COMMENTS TABLE
    comment_table_single <- function(raw, team_num){
        comments <- raw %>%
            group_by(team)%>%
            filter(team==team_num)%>%
            summarise(
                wobbly = length(grep("2", comments)),
                wiffs = length(grep("4", comments)),
                `wobbly manip` = length(grep("1", comments)),
                `bad intake` = length(grep("5", comments)),
                defense = length(grep("0", comments)),
                `xtra long climb` = length(grep("6", comments))
            )%>%
            
            pivot_longer(cols = c(wobbly,
                                  wiffs, 
                                  `wobbly manip`,
                                  `bad intake`,
                                  defense,
                                  `xtra long climb`), 
                         names_to = "com", 
                         values_to = "level")
        ggplot(comments, aes(x = com, y = level)) + 
            geom_bar(position = "stack", stat = "identity", fill = "#EE3B3B") + 
            labs(title = "Comments Summary", 
                 x = "Issues", y = "Frequency") +
            theme_bw()
    }
    
    #PROBLEMS TABLE
    problem_table_single <- function(raw, team_num){
        problem <- raw %>%
            group_by(team)%>%
            filter(team==team_num)%>%
            summarise(
                coral_stuck = length(grep("cs", dead)),
                algae_beach = length(grep("ba", dead)),
                disabled = length(grep("di", dead)),
                `dead` = length(grep("de", dead)),
                tipped = length(grep("t", dead))
                )%>%
            
            pivot_longer(cols = c(coral_stuck,
                                  algae_beach, 
                                  disabled,
                                  `dead`,
                                  tipped), 
                         names_to = "labels", 
                         values_to = "count")
        ggplot(problem, aes(x = labels, y = count)) + 
            geom_bar(position = "stack", stat = "identity", fill = "#EE3B3B") + 
            labs(title = "Dead Summary", 
                 x = "Issues", y = "Frequency") +
            theme_bw()
    }
    
    #PAST MATCH HISTORY (fixed)
    single_team_match_history_bar_graph <- function(raw, selected_team){
        team_data <- raw %>%
            filter(team == selected_team) %>%
            mutate(
                auto_coral_L1 = auto_coral_L1_num * 3,
                auto_coral_L2 = auto_coral_L2_num * 4,
                auto_coral_L3 = auto_coral_L3_num * 6,
                auto_coral_L4 = auto_coral_L4_num * 7,
                move_pts = move * 3,
                tele_coral_L1 = coral_L1_num * 2,
                tele_coral_L2 = coral_L2_num * 3,
                tele_coral_L3 = coral_L3_num * 4,
                tele_coral_L4 = coral_L4_num * 5,
                robot_net_score = robot_net_score * 4,
                robot_proc_score = proc_score * 2.5,
                endgame_score = ifelse(ending == "D", 12,
                                       ifelse(ending == "S", 6,
                                              ifelse(ending == "P", 2, 0)))
            ) %>%
            # Use match number for each bar
            mutate(match = factor(match)) %>%
            # Keep match_num and all the score columns
            select(match, auto_coral_L1, auto_coral_L2, auto_coral_L3, auto_coral_L4, move_pts,
                   tele_coral_L1, tele_coral_L2, tele_coral_L3, tele_coral_L4,
                   robot_net_score, robot_proc_score, endgame_score) %>%
            # Reshape data for ggplot
            pivot_longer(cols = c(auto_coral_L1, auto_coral_L2, auto_coral_L3, auto_coral_L4, move_pts,
                                  tele_coral_L1, tele_coral_L2, tele_coral_L3, tele_coral_L4,
                                  robot_net_score, robot_proc_score, endgame_score), 
                         names_to = "level", 
                         values_to = "score")
        
        color_values <- list(
            "auto_coral_L1" = "plum1",
            "auto_coral_L2" = "plum2",
            "auto_coral_L3" = "plum3", 
            "auto_coral_L4" = "plum4",
            "endgame_score" = "#FFF68F", 
            "move_pts" = "#FFC156", 
            "robot_net_score" = "olivedrab3",
            "robot_proc_score" = "springgreen4", 
            "tele_coral_L1" = "steelblue1", 
            "tele_coral_L2" = "steelblue2",
            "tele_coral_L3" = "steelblue3", 
            "tele_coral_L4" = "steelblue4"
        )
        
        label_values <- list(
            "auto_coral_L1" = "Auto Coral L1", 
            "auto_coral_L2" = "Auto Coral L2",
            "auto_coral_L3" = "Auto Coral L3", 
            "auto_coral_L4" = "Auto Coral L4",
            "move_pts" = "Move",
            "tele_coral_L1" = "Tele Coral L1", 
            "tele_coral_L2" = "Tele Coral L2", 
            "tele_coral_L3" = "Tele Coral L3", 
            "tele_coral_L4" = "Tele Coral L4", 
            "endgame_score" = "Endgame", 
            "robot_net_score" = "Net", 
            "robot_proc_score" = "Processor"
        )
        
        ggplot(team_data, aes(x = match, y = score, fill = level)) + 
            geom_bar(position = "stack", stat = "identity") + 
            labs(title = paste("Match History for Team", selected_team), 
                 x = "Match Number", y = "Score Breakdown", fill = "Scoring Element") +
            scale_fill_manual(values = unlist(color_values),
                              labels = unlist(label_values)) +
            theme_bw() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                plot.title = element_text(size = 14, face = "bold")
            )
           # geom_text(aes(label = score), size = 3, hjust = 0.2, vjust = 2, position = "stack")
        
    }
    
    #PAST MATCH RAW TEAM DATA
    output$past_team_table <- renderDT({
        selected_team <- input$team_select
        team_past_data <- past_raw_team_data()[past_raw_team_data()$team == selected_team, ]
        datatable(team_past_data, options = list(dom = "ft", lengthChange = FALSE, rowNames = FALSE, scrollX = TRUE, scrollY = 500, pageLength = nrow(team_past_data)))
    })
    
    #PAST DRIVER GRAPH
    previous <- function(raw, team_num){
        past <- raw %>%
            group_by(team)%>%
            filter(team==team_num)%>%
            summarize(
                match = match,
                total_score = move*3 +
                    auto_coral_L1_num*3 +
                    auto_coral_L2_num*4 +
                    auto_coral_L3_num*6 +
                    auto_coral_L4_num*7 +
                    robot_net_score*4 +
                    proc_score*2.5 +
                    coral_L1_num*2 +
                    coral_L2_num*3 +
                    coral_L3_num*4 +
                    coral_L4_num*5 +
                    ifelse(ending =="D", 12, ifelse(ending=="S",6,ifelse(ending=="P", 2, 0))),
                driver = driver,
                defense = defense
            )
        ggplot(past, aes(match)) + 
            geom_line(aes(y=driver), color = blair_red) +
            geom_line(aes(y=defense), color = "blue") +
            scale_y_continuous(limits = c(1, 5)) +
            scale_x_continuous(breaks=past$match)
    }
    
    cycle_history <- function(raw, team_num){
        past <- raw %>%
            group_by(team)%>%
            filter(team==team_num)%>%
            summarize(
                match = match,
                total_score = move*3 +
                    auto_coral_L1_num*3 +
                    auto_coral_L2_num*4 +
                    auto_coral_L3_num*6 +
                    auto_coral_L4_num*7 +
                    robot_net_score*4 +
                    proc_score*2.5 +
                    coral_L1_num*2 +
                    coral_L2_num*3 +
                    coral_L3_num*4 +
                    coral_L4_num*5 +
                    ifelse(ending =="D", 12, ifelse(ending=="S",6,ifelse(ending=="P", 2, 0))),
                driver = driver,
                defense = defense,
                coral_tele_cycles = coral_L1_num + coral_L2_num + coral_L3_num + coral_L4_num,
                algae_tele_cycles = robot_net_score + proc_score
            )
        ggplot(past, aes(match)) + 
            geom_line(aes(y=coral_tele_cycles), color = blair_red) +
            geom_line(aes(y=algae_tele_cycles), color = "blue") +
            geom_line(aes(y=coral_tele_cycles+algae_tele_cycles), color = "black") +
            scale_y_continuous() +
            scale_x_continuous(breaks=past$match) + ylab("Coral (red) || Algae (blue) || Total (black) Cycles")
    }
    
    team_comments <- reactive({
        comments_data <- raw() %>%
            filter(team == input$team_select) %>%
            select(commentOpen) 
        
        if (length(comments_data) > 0) {
            return(comments_data)
        } else {
            return(data.frame())
        }
    })
    
    output$comments_list <- renderDT({
        comments <- team_comments()
        })
    
    #SINGLE TEAM CHOICE LOGIC
    output$team_graph_output <- renderPlot({
        selected_team <- input$team_select
        
        if (input$team_graph == "Overall Points Box Plot"){
            boxplot_graph_single(raw(), selected_team)
            } 
        else if (input$team_graph == "Auto Bar Graph"){
            auto_graph_single(raw(), selected_team)
            } 
        else if (input$team_graph == "Tele Bar Graph"){
            tele_graph_single(raw(), selected_team)
            } 
        else if (input$team_graph == "Endgame Bar Graph"){
            endgame_graph_single(raw(), selected_team)
            } 
        else if (input$team_graph == "Comments"){
            comment_table_single(raw(), selected_team)
        } 
        else if (input$team_graph == "Problems"){
            problem_table_single(raw(), selected_team)
        } 
        else if (input$team_graph == "Match History"){
            single_team_match_history_bar_graph(raw(), selected_team)
        }
        else if (input$team_graph == "Driver Rating History"){
            previous(raw(), selected_team)
        } 
        else if (input$team_graph == "Cycle History"){
            cycle_history(raw(), selected_team)
        } 
    })
    
    
    #COMPARE TEAMS CHOICE LOGIC
    output$compare_teams_graph_output <- renderPlot({
        selected_teams <- input$teams_selected
        
        if (input$compare_teams_graph == "Points Large Bar Graph"){
            compare_teams_large_bar_graph(raw(), selected_teams)
        } 
        else if (input$compare_teams_graph == "Comments"){
            compare_teams_comments(raw(), selected_teams)
        }
        else if (input$compare_teams_graph == "Dead"){
            compare_teams_problems(raw(), selected_teams)
        }
        else if (input$compare_teams_progress_graph == "Points"){
            compare_teams_past_points(raw(), selected_teams)
        }
        else if (input$compare_teams_progress_graph == "Driver Ranking"){
            compare_teams_past_driver(raw(), selected_teams)
        }
    })
    
    output$scouter_graph_output <- renderPlotly({
        scouter_graph_output(raw())
        })
    
    output$yapp_graph_output <- renderPlotly({
        yapp_graph_output(raw())
    })
    
    output$high_streak_output <- renderPlot({
        high_streak_output(raw())
    })
    
    output$team_image_output <- renderUI({
        #browser()
        teamnum <- input$team_select
        if (event_code == "all_data") {
            #browser()
            img_src <- paste0("images/vacri_img/", teamnum, ".png")
            img_vagle <- paste0("images/vagle_img/", teamnum, ".png")
            img_mdsev <- paste0("images/mdsev_img/", teamnum, ".png")
            img_dchamp <- paste0("images/dchamp_img/", teamnum, ".png")
            img_newton <- paste0("images/newton_img/", teamnum, ".png")
            img_vacri <- paste0("images/vacri_img/", teamnum, ".png")
            
            if (file.exists(img_vagle)) img_src <- img_vagle
            if (file.exists(img_mdsev)) img_src <- img_mdsev
            if (file.exists(img_dchamp)) img_src <- img_dchamp
            if (file.exists(img_newton)) img_src <- img_newton
            if (file.exists(img_vacri)) img_src <- img_vacri
        }
        else {img_src <- paste0("images/", event_code, "_img", "/", teamnum, ".png")}
        no_img_available_src <- paste0("images/", "no_image_available", ".jpg")
        
        #Check if the image file exists
        if (file.exists(img_src)) {
            tags$img(src = img_src, alt = paste("Robot Image for Team", teamnum), style = "width: 100%; height: auto; display: block; margin: 0 auto;")
            #return(list(
            #    src = img_src,
            #    contentType = "image/png",
            #    width = 350,
            #    height = 350,
            #    alt = paste("Robot Image for Team", teamnum),
            #    style="display: block; margin-left: auto; margin-right: auto;"))
        } else {
            tags$img(src = no_img_available_src, alt = paste("No Image Available"), style = "width: 100%; height: auto; display: block: margin: 0 auto;")
            #return(list(
            #    src = no_img_available_src,
            #    contentType = "image/jpg",
            #    width = 350,
            #    height = 350,
            #    alt = paste("No Image Available"),
            #    style="display: block; margin-left: auto; margin-right: auto;"))
        }
    })
    
    output$team_field_output <- renderImage({
        teamnum <- input$team_select
        img_src <- paste0("images/", teamnum, ".png")  #Path to the image
        no_img_available_src <- paste0("images/", "no_image_available", ".jpg")
        
        #Check if the image file exists
        if (file.exists(img_src)) {
            return(list(
                src = img_src,
                contentType = "image/png",
                width = 350,
                height = 350,
                alt = paste("Robot Image for Team", teamnum),
                style="display: block; margin-left: auto; margin-right: auto;"
            ))
            #tags$img(src = img_src, alt = paste("Robot Image for Team", teamnum), height = "300px")
            } 
        else {
            return(list(
                src = no_img_available_src,
                contentType = "image/jpg",
                width = 350,
                height = 350,
                alt = paste("No Image Available"),
                style="display: block; margin-left: auto; margin-right: auto;"
                ))
            }
    }, deleteFile = FALSE)
    
    output$team_data_row <- renderDT({
        selected_team <- input$team_select
        team_data_row <- consolidated_team_data()[consolidated_team_data()$team == selected_team, ]
        datatable(team_data_row, options = list(scrollX = TRUE, pageLength = 1, dom = 't'))
    })
    
    #SCOUTERS
    scouter_graph_output <- function(raw){
        scout_df <- raw %>%
            mutate(
                scout = toupper(scout),
                scout = trimws(scout),
                scout = gsub("[^[:alnum:]]", "", scout)
            ) %>%
            group_by(scout) %>%
            summarise(count = n()) %>%
            arrange(desc(count))
        
        scout_df$scout <- factor(scout_df$scout, levels = scout_df$scout)
        
        temp <- ggplot(scout_df, aes(x = `scout`, y = count, text = paste("Scout: ", scout, "|| Matches Scouted:", count))) + 
            geom_bar(position = "stack", stat = "identity", fill = "coral3") + 
            labs(title = "Scout Summary", 
                 x = "Scouts", y = "Times Scouted") +
            theme_bw()
        
        ggplotly(temp, tooltip = "text")
    }
    
    yapp_graph_output <- function(raw){
        yapp_df <- raw %>%
            mutate(
                scout = toupper(scout),
                scout = trimws(scout),
                scout = gsub("[^[:alnum:]]", "", scout)
            ) %>%
            group_by(scout) %>%
            summarize(yapp = nchar(commentOpen)) %>%
            summarize(yapp = mean(yapp)) %>%
            arrange(desc(yapp))
        
        yapp_df$scout <- factor(yapp_df$scout, levels = yapp_df$scout)
        
        temp <- ggplot(yapp_df, aes(x = `scout`, y = yapp, text = paste("Scout: ", scout, "|| Words Yapped:", yapp))) + 
            geom_bar(position = "stack", stat = "identity", fill = "steelblue") + 
            labs(title = "Yapp Summary", 
                 x = "Scouts", y = "Length of Comments") +
            theme_bw()
        
        ggplotly(temp, tooltip = "text")
    }
    
    high_streak_output <- function(raw){
        current_match = max(raw$match)
        all_matches <- 1:current_match
        streak_df <- raw %>%
            mutate(
                scout = toupper(scout),
                scout = trimws(scout),
                scout = gsub("[^[:alpha:]]", "", scout)
            ) %>%
            group_by(scout) %>%
            summarise(
                scouted_matches = list(unique(match))
            ) %>%
            rowwise()%>% #Holy sheet rowwise my GOAT
            mutate(
                missed_matches = list(setdiff(all_matches, scouted_matches)),
                streak = current_match - max(missed_matches)
            ) %>%
            #arrange(desc(streak))%>% #help why not work
            filter(streak>0)

        ggplot(streak_df, aes(x = `scout`, streak)) + 
            geom_bar(position = "stack", stat = "identity", fill = "chartreuse2") + 
            labs(title = "Current Streak", 
                 x = "Scouts", y = "Matches") +
            theme_bw()
    }
    
    #AUTO PICKLISTING
    weights_data <- reactiveVal(default_linear_weights)
    
    normalize_column <- function(x) {
        if (sd(x, na.rm = TRUE) == 0) {
            return(rep(0, length(x)))
        }
        
        #min-max normalization
        normalized <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
        
        normalized[is.nan(normalized)] <- 0
        
        return(normalized)
    }
    
    calculate_team_scores <- reactive({
        #get current weights
        current_weights <- weights_data()
        
        #create a copy of data to work with
        team_data <- consolidated_team_data()
        
        #these cols are genkey not useful but will see
        numeric_cols <- setdiff(names(team_data), c("team", "dead_times"))
        
        #create normalized version of the data
        normalized_data <- team_data
        
        for (col in numeric_cols) {
            normalized_data[[col]] <- normalize_column(team_data[[col]])
        }
        
        #calc weighted scores
        team_scores <- team_data[, "team", drop = FALSE]
        team_scores$team_score <- 0
        
        for (col in numeric_cols) {
            if (col %in% names(current_weights)) {
                weight_val <- current_weights[[col]]
                team_scores$team_score <- team_scores$team_score + (normalized_data[[col]] * weight_val)
            }
        }
        
        team_scores <- merge(team_scores, team_data, by = "team")
        
        #sort by team score in descending order
        team_scores <- team_scores[order(-team_scores$team_score), ]
        
        return(team_scores)
    })
    
    output$weighted_picklist_table <- renderDT({
        team_scores <- calculate_team_scores()
        
        team_scores$rank <- 1:nrow(team_scores)
        team_scores$team_score <- round(team_scores$team_score, 2)
        
        #reorder columns to show rank and score first
        cols_order <- c("rank", "team", "team_score")
        remaining_cols <- setdiff(names(team_scores), cols_order)
        team_scores <- team_scores[, c(cols_order, remaining_cols)]
        
        #datatable
        datatable(team_scores, 
                  options = list(
                      pageLength = 15,
                      dom = 'ftip',
                      scrollX = TRUE
                  ),
                  rownames = FALSE) %>%
            formatStyle('team_score',
                        background = styleColorBar(c(0, max(team_scores$team_score)), 'lightblue'),
                        backgroundSize = '100% 90%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center')
    })
    
    #create weights modal UI
    weights_modal <- function() {
        modalDialog(
            title = "Adjust Team Weighting Factors",
            size = "l",
            
            fluidRow(
                column(6,
                       sliderInput("weight_l1_cycle_tele", "L1 Coral Tele", min = -20, max = 20, value = weights_data()$l1_cycle_tele, step = 1),
                       sliderInput("weight_l2_cycle_tele", "L2 Coral Tele", min = -20, max = 20, value = weights_data()$l2_cycle_tele, step = 1),
                       sliderInput("weight_l3_cycle_tele", "L3 Coral Tele", min = -20, max = 20, value = weights_data()$l3_cycle_tele, step = 1),
                       sliderInput("weight_l4_cycle_tele", "L4 Coral Tele", min = -20, max = 20, value = weights_data()$l4_cycle_tele, step = 1),
                       sliderInput("weight_coral_cycle_tele", "Total Coral Tele", min = -20, max = 20, value = weights_data()$coral_cycle_tele, step = 1),
                       sliderInput("weight_coral_cycle_auto", "Total Coral Auto", min = -20, max = 20, value = weights_data()$coral_cycle_auto, step = 1),
                       sliderInput("weight_net_cycle", "Net Cycle", min = -20, max = 20, value = weights_data()$net_cycle, step = 1),
                       sliderInput("weight_proc_cycle", "Processor Cycle", min = -20, max = 20, value = weights_data()$proc_cycle, step = 1),
                       sliderInput("weight_algae", "Total Algae", min = -20, max = 20, value = weights_data()$algae, step = 1),
                       sliderInput("weight_total_pts_mean", "Total Points Mean", min = -20, max = 20, value = weights_data()$total_pts_mean, step = 1)
                ),
                column(6,
                       sliderInput("weight_tele_pts_mean", "Teleop Points Mean", min = -20, max = 20, value = weights_data()$tele_pts_mean, step = 1),
                       sliderInput("weight_auto_pts_mean", "Auto Points Mean", min = -20, max = 20, value = weights_data()$auto_pts_mean, step = 1),
                       sliderInput("weight_endgame_pts_mean", "Endgame Points Mean", min = -20, max = 20, value = weights_data()$endgame_pts_mean, step = 1),
                       sliderInput("weight_algae_remove_pct", "Algae Removal %", min = -20, max = 20, value = weights_data()$algae_remove_pct, step = 1),
                       sliderInput("weight_intaked_algae_from_ground_pct", "Algae Ground Intake %", min = -20, max = 20, value = weights_data()$algae_remove_pct, step = 1),
                       sliderInput("weight_move_pct", "Move %", min = -20, max = 20, value = weights_data()$move_pct, step = 1),
                       sliderInput("weight_dead_pct", "Dead %", min = -20, max = 20, value = weights_data()$dead_pct, step = 1),
                       sliderInput("weight_driver_rating_mean", "Driver Rating", min = -20, max = 20, value = weights_data()$driver_rating_mean, step = 1),
                       sliderInput("weight_defense_rating_mean", "Defense Rating", min = -20, max = 20, value = weights_data()$defense_rating_mean, step = 1)
                )
            ),
            
            footer = tagList(
                modalButton("Cancel"),
                actionButton("reset_weights", "Reset to Default", class = "btn-warning"),
                actionButton("apply_weights", "Apply Weights", class = "btn-primary")
            )
        )
    }
    
    #open weights modal
    observeEvent(input$open_weights, {
        showModal(weights_modal())
    })
    
    observeEvent(input$reset_weights, {
        #update all slider values to default
        updateSliderInput(session, "weight_l1_cycle_tele", value = default_linear_weights$l1_cycle_tele)
        updateSliderInput(session, "weight_l2_cycle_tele", value = default_linear_weights$l2_cycle_tele)
        updateSliderInput(session, "weight_l3_cycle_tele", value = default_linear_weights$l3_cycle_tele)
        updateSliderInput(session, "weight_l4_cycle_tele", value = default_linear_weights$l4_cycle_tele)
        updateSliderInput(session, "weight_coral_cycle_tele", value = default_linear_weights$coral_cycle_tele)
        updateSliderInput(session, "weight_coral_cycle_auto", value = default_linear_weights$coral_cycle_auto)
        updateSliderInput(session, "weight_net_cycle", value = default_linear_weights$net_cycle)
        updateSliderInput(session, "weight_proc_cycle", value = default_linear_weights$proc_cycle)
        updateSliderInput(session, "weight_algae", value = default_linear_weights$algae)
        updateSliderInput(session, "weight_total_pts_mean", value = default_linear_weights$total_pts_mean)
        updateSliderInput(session, "weight_tele_pts_mean", value = default_linear_weights$tele_pts_mean)
        updateSliderInput(session, "weight_auto_pts_mean", value = default_linear_weights$auto_pts_mean)
        updateSliderInput(session, "weight_endgame_pts_mean", value = default_linear_weights$endgame_pts_mean)
        updateSliderInput(session, "weight_algae_remove_pct", value = default_linear_weights$algae_remove_pct)
        updateSliderInput(session, "weight_move_pct", value = default_linear_weights$move_pct)
        updateSliderInput(session, "weight_dead_pct", value = default_linear_weights$dead_pct)
        updateSliderInput(session, "weight_driver_rating_mean", value = default_linear_weights$driver_rating_mean)
        updateSliderInput(session, "weight_defense_rating_mean", value = default_linear_weights$defense_rating_mean)
    })
    
    #apply updated weights
    observeEvent(input$apply_weights, {
        new_weights <- data.frame(
            team = 0,
            l1_cycle_tele = input$weight_l1_cycle_tele,
            l2_cycle_tele = input$weight_l2_cycle_tele,
            l3_cycle_tele = input$weight_l3_cycle_tele,
            l4_cycle_tele = input$weight_l4_cycle_tele,
            coral_cycle_tele = input$weight_coral_cycle_tele,
            coral_cycle_auto = input$weight_coral_cycle_auto,
            net_cycle = input$weight_net_cycle,
            proc_cycle = input$weight_proc_cycle,
            algae = input$weight_algae,
            total_pts_mean = input$weight_total_pts_mean,
            tele_pts_mean = input$weight_tele_pts_mean,
            auto_pts_mean = input$weight_auto_pts_mean,
            endgame_pts_mean = input$weight_endgame_pts_mean,
            algae_remove_pct = input$weight_algae_remove_pct,
            intaked_algae_from_ground_pct = input$weight_intaked_algae_from_ground_pct,
            move_pct = input$weight_move_pct,
            dead_pct = input$weight_dead_pct,
            driver_rating_mean = input$weight_driver_rating_mean,
            defense_rating_mean = input$weight_defense_rating_mean
        )
        
        weights_data(new_weights)
        removeModal()
    })
    
    
    #download picklist
    output$download_picklist <- downloadHandler(
        filename = function() {
            paste("team-picklist-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(calculate_team_scores(), file, row.names = FALSE)
        }
    )
}
shinyApp(ui, server)