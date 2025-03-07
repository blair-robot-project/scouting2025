#Load required libraries
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(tidyverse)
library(shinythemes)

data_dir <- "data files"
data_file <- paste0(data_dir, "/match_data_glen_allen.csv")
event_schedule_file <- paste0(data_dir, "/2025vagle_match_info.csv")
teams_file <- paste0(data_dir, "/teams_glen_allen.csv")

#Load team data and event schedule
raw <- read.csv(data_file)
match_schedule <- read.csv(event_schedule_file, fill = TRUE)
teams <- read.csv(teams_file)

mldf <- raw %>%
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
    endgame_pts = case_when(
      ending == "P" ~ 2, 
      ending == "S" ~ 6, 
      ending == "D" ~ 12, 
      .default = 0
    ),
    
    total_pts = move_pts + auto_coral_l1_pts + auto_coral_l2_pts + 
      auto_coral_l3_pts + auto_coral_l4_pts + coral_l1_pts + coral_l2_pts + 
      coral_l3_pts + coral_l4_pts + net_pts + processor_value + endgame_pts,
    
    #auto
    total_auto_pts = auto_coral_l1_pts + auto_coral_l2_pts + 
      auto_coral_l3_pts + auto_coral_l4_pts + move_pts,
    
    
    #tele
    total_algae_pts = net_pts + processor_value,
    
    total_tele_coral_pts =  coral_l1_pts + coral_l2_pts + 
      coral_l3_pts + coral_l4_pts,
    
    total_tele_pts =  total_tele_coral_pts +total_algae_pts,
    
    #algae
    algae_net_shots = sum(net_pts + net_missed)
    
    
  )

consolidated_team_data <- mldf %>%
  group_by(team) %>%
  summarize(
    
    #total points
    total_pts_mean = round(mean(total_pts, na.rm = TRUE), digits =2), 
    total_pts_median = round(median(total_pts, na.rm = TRUE), digits =2),
    total_pts_sd = round(sd(total_pts, na.rm = TRUE), digits =2), 
    total_pts_max = round(max(total_pts, na.rm = TRUE), digits =2),
    
    
    #coral
    coral_pts_mean = round(mean(coral_l1_pts + coral_l2_pts + coral_l3_pts + 
                                  coral_l4_pts, na.rm = TRUE), digits =2), 
    
    l1_pts_mean = round(mean(coral_l1_pts, na.rm = TRUE), digits = 2), 
    l2_pts_mean = round(mean(coral_l2_pts, na.rm = TRUE), digits = 2), 
    l3_pts_mean = round(mean(coral_l3_pts, na.rm = TRUE), digits = 2), 
    l4_pts_mean = round(mean(coral_l4_pts, na.rm = TRUE), digits = 2), 
    #% in levels
    l1_pct_mean = round(mean(coral_l1_pts, na.rm = TRUE)/coral_pts_mean*100, digits = 2), 
    l2_pct_mean = round(mean(coral_l2_pts, na.rm = TRUE)/coral_pts_mean*100, digits = 2), 
    l3_pct_mean = round(mean(coral_l3_pts, na.rm = TRUE)/coral_pts_mean*100, digits = 2), 
    l4_pct_mean = round(mean(coral_l4_pts, na.rm = TRUE)/coral_pts_mean*100, digits = 2), 
    
    
    #teleop
    tele_pts_mean = round(mean(total_tele_pts, na.rm = TRUE), digits =2), 
    tele_pts_median = round(median(total_tele_pts, na.rm = TRUE), digits =2), 
    tele_pts_sd = round(sd(total_tele_pts, na.rm = TRUE), digits = 2), 
    tele_pts_max = round(max(total_tele_pts, na.rm = TRUE), digits = 2), 
    
    
    #auto
    auto_pts_mean = round(mean(total_auto_pts, na.rm = TRUE), digits =2 ), 
    auto_pts_median = round(median(total_auto_pts, na.rm = TRUE), digits = 2), 
    auto_pts_sd = round(sd(total_auto_pts, na.rm = TRUE), digits = 2),
    auto_pts_max = round(max(total_auto_pts, na.rm = TRUE),digits =2),  
    auto_move = round(mean(move_pts, na.rm = TRUE), digits = 2),
    
    
    #algae
    algae_pts_mean =round(mean(net_pts + processor_value, na.rm = TRUE), digits=2),
    algae_pts_median = round(median(net_pts + processor_value, na.rm = TRUE), digits = 2),
    algae_pts_sd = round(sd(net_pts + processor_value, na.rm = TRUE), digits =2),
    algae_pts_max = round(max(net_pts + processor_value, na.rm = TRUE), digits =2), 
    
    #endgame
    endgame_pts_mean =round( mean(endgame_pts, na.rm = TRUE), digits =2),
    endgame_pts_median = round(median(endgame_pts, na.rm = TRUE), digits =2), 
    endgame_pts_sd = round(sd(endgame_pts, na.rm = TRUE), digits =2),
    endgame_pts_max = round(max(endgame_pts, na.rm = TRUE), digits =2),
    
    
    #you would put many more calculations here!
    #algae net pct
  #algae remove data?
    
    algae_remove_pct = sum(c(robot_reef_removal))/n()
    
  )

#UI
ui <- fluidPage(
  navbarPage(theme = shinytheme("sandstone"),  
             "449 Scouting",
    tabPanel("Picklisting",
             #sidebarLayout(
             #  sidebarPanel(
             #    selectInput("picklist_metric", "Choose Graph", choices = c("Bubble Graph", "Other Graphs")),
             #    actionButton("graph_btn", "Graph", class = "btn btn-primary")
             #  ),
             #  mainPanel(
             #    plotOutput("picklist_graph"),
             #    DTOutput("picklist_table")
             #  )
             #)
             fluidRow(
               column(12,
                      plotOutput("picklist_graph"),
                      plotOutput("long_column_output"),
                      DTOutput("picklist_table")
               )
             )
    ),
    
    tabPanel("Alliance/Match",
             sidebarLayout(
               sidebarPanel(
                 #Selection between match number or entering 6 teams
                 radioButtons("match_or_teams", "Select Match Number or 6 Teams", choices = c("Match Number", "Select 6 Teams")),
                 conditionalPanel(
                   condition = "input.match_or_teams == 'Match Number'",
                   selectInput("match_num", "Match Number", choices = unique(match_schedule$Match))
                 ),
                 conditionalPanel(
                   condition = "input.match_or_teams == 'Select 6 Teams'",
                   pickerInput("red_teams", "Red Alliance Teams", choices = unique(teams$team), multiple = TRUE, options = list(maxOptions = 3)),
                   pickerInput("blue_teams", "Blue Alliance Teams", choices = unique(teams$team), multiple = TRUE, options = list(maxOptions = 3))
                 ),
                 #selectInput("alliance_graph", "Choose Graph", choices = c("Overall Points Box Plot", "Coral Level Bar Graph", "Coral Auto + Tele Bar Graph", "Endgame Bar Graph")),
                 actionButton("generate_graph", "Generate Graphs", class = "btn btn-primary")
               ),
               mainPanel(
                 plotOutput("alliance_box_plot_output"),
                 plotOutput("alliance_coral_graph_output"),
                 plotOutput("alliance_tele_auto_graph_output"),
                 plotOutput("alliance_endgame_graph_output"),
                 DTOutput("alliance_table")
               )
             )
    ),
    
    tabPanel("Single Team",
             sidebarLayout(
               sidebarPanel(
                 selectInput("team_select", "Select Team", choices = unique(teams$team)),
                 selectInput("team_graph", "Choose Graph", choices = c("Overall Points Box Plot", "Coral Level Bar Graph", "Coral Auto + Tele Bar Graph", "Endgame Bar Graph")),
                 imageOutput("team_image_output")
              ),
               mainPanel(
                 plotOutput("team_graph_output"),
                 DTOutput("team_data_row")
               )
             )
    ),
    
    tabPanel("Scouters",
             #sidebarLayout(
            #   sidebarPanel(
            #     selectInput("Scouter_Select", "Select Scouter", choices = unique(raw$scout)),
                 #selectInput("team_graph", "Choose Graph", choices = c("Overall Points Box Plot", "Coral Level Bar Graph", "Coral Auto + Tele Bar Graph", "Endgame Bar Graph")),
                 #imageOutput("team_image_output")
            #   ),
               mainPanel(
                 plotOutput("scouter_graph_output")
               )
          )
  )
)

#Server
server <- function(input, output, session) {
  
  #Picklisting Tab Graph and Table
  #BUBBLE LOGIC
  bubble_graph <- function(raw) {
    
    bubble <- raw%>%
      group_by(team)%>%
      summarise(
        match = n(),
        total_coral_score = sum(
          (auto_coral_L1_num*3) + (auto_coral_L2_num*4) + (auto_coral_L3_num*6) + (auto_coral_L4_num*7) + 
            (coral_L1_num*2) + (coral_L2_num*3) + (coral_L3_num*4)+ (coral_L4_num*5)+move*3)/n(),
        
        total_algae_score = sum(
          (robot_net_score*4) + (proc_score*2.5))/n(),
        
        endgame_score = sum(
          ifelse(ending =="D", 12, ifelse(ending =="S",6,ifelse(ending =="P", 2, 0))))/n(),
        avg_score = total_algae_score+total_coral_score+endgame_score
      )
    
    ggplot(bubble, aes(x=total_coral_score, y=total_algae_score, size = endgame_score)) +
      geom_point( color = "lightblue2")+
      geom_text( aes(label=team, vjust = 1.7 ))+
      labs(title = "Teams Performance Summary", 
           x = "Auto + Tele Coral Points", y = "Total algae Points", fill = "Auto+End", size = "Endgame")
    
  }
  
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
    
    ggplot(column4, aes(x = team, y = level_score, fill = level)) + 
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
  
  output$long_column_output <- renderPlot({
    #Bubble graph logic
    long_column(raw)
  })
  
  output$picklist_graph <- renderPlot({
    #Bubble graph logic
    bubble_graph(raw)
  })
  
  output$picklist_table <- renderDT({
    datatable(consolidated_team_data, options = list(dom = "ft", lengthChange = FALSE, rowNames = FALSE, scrollX = TRUE, scrollY = 500, pageLength = nrow(consolidated_team_data)))
  })
  
  #observeEvent(input$graph_btn, {
  #  if (input$picklist_metric == "Bubble Graph") {
  #    output$picklist_graph <- renderPlot({
  #      #Bubble graph logic
  #      bubble_graph(raw)
  #    })
  #  }
  #  output$picklist_table <- renderDT({
  #    datatable(consolidated_team_data)
  #  })
  #})
  #
  
  
  #Alliance/Match Tab
  #ALL GRAPH GENERATING FUNCTIONS------------------------------------------------------------------
  #BOXPLOT GRAPH
  
  boxplot_graph_alliance <- function(raw, red_alliance = c(params$red1, params$red2, params$red3), 
                                     blue_alliance = c(params$blue1, params$blue2, params$blue3)) {
    
    boxplot <- raw %>%
      filter(team %in% c(red_alliance, blue_alliance)) %>%
      mutate(team = factor(team, c(red_alliance,blue_alliance) ))%>%
      
      mutate(total_coral_score = 
               (coral_L1_num*2) + (coral_L2_num*3) + 
               (coral_L3_num*4) + (coral_L4_num*5) + 
               (auto_coral_L1_num*3) + (auto_coral_L2_num*4)+ 
               (auto_coral_L3_num*5)+ (auto_coral_L4_num*7),
             
             total_algae_score = 
               (robot_net_score*4) + (proc_score*2.5),
             
             total_endgame_score = 
               ifelse(ending =="D", 12, ifelse(ending=="S",6,ifelse(ending=="P", 2, 0)))
      )
    
    boxplot$total = boxplot$total_algae_score + boxplot$total_coral_score+boxplot$total_endgame_score
    
    
    ggplot(boxplot,aes(x = total, y = team))+    
      geom_boxplot(position = "dodge2", fill = "azure2") +
      stat_boxplot(geom = "errorbar") + 
      stat_summary(fun = mean, geom="point", size=3, color="orange")+
      labs(title = "Total points scored",x = "Points", y = "Team")+
      theme_bw()
  }
  
  #CORAL LEVEL GRAPH ALLIANCE
  bar_graph_alliance <- function(raw, red_alliance = c(params$red1, params$red2, params$red3), 
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
    
    p <- ggplot(bar, aes(x = factor(team), y = level_score, fill = level)) + 
      geom_bar(position = "stack", stat = "identity") + 
      labs(title = "Level Summary", 
           x = "Team", y = "Coral score", fill = "Level") +
      scale_fill_manual(values=c("lightskyblue","royalblue1","royalblue3","navy")) +
      theme_bw()
    
    return(p)
  }
  
  #CORAL TELE AUTO
  tele_auto_graph <- function(raw, red_alliance = c(params$red1, params$red2, params$red3), blue_alliance= c(params$blue1, params$blue2, params$blue3)) {
    
    auto_tele <- raw %>%
      filter(team %in% c(red_alliance, blue_alliance)) %>%
      mutate(team = factor(team, c(red_alliance,blue_alliance) )) %>%
      
      group_by(team) %>%
      summarize(
        matches = n(),
        
        l1 = mean(coral_L1_num),
        l2 = mean(coral_L2_num),
        l3 = mean(coral_L3_num),
        l4 = mean(coral_L4_num),
        autol1 = mean(auto_coral_L1_num),
        autol2 = mean(auto_coral_L2_num),
        autol3 = mean(auto_coral_L3_num),
        autol4 = mean(auto_coral_L4_num)
        
      )%>%
      
      pivot_longer(cols = c(l4, l3, l2,l1,autol1,autol2,autol3,autol4), 
                   names_to = "level", 
                   values_to = "coral_num")
    auto_tele$level_score <- case_when(
      auto_tele$level == "l1" ~ auto_tele$coral_num*2, 
      auto_tele$level == "l2" ~ auto_tele$coral_num*3, 
      auto_tele$level == "l3" ~ auto_tele$coral_num*4, 
      auto_tele$level == "l4" ~ auto_tele$coral_num*5,
      auto_tele$level == "autol1" ~ auto_tele$coral_num*3,
      auto_tele$level == "autol2" ~ auto_tele$coral_num*4,
      auto_tele$level == "autol3" ~ auto_tele$coral_num*6,
      auto_tele$level == "autol4" ~ auto_tele$coral_num*7
      
    )
    
    ggplot(auto_tele, aes(x = factor(team), y = (level_score), fill = level)) + 
      geom_bar(position = "stack", stat = "identity") + 
      labs(title = "Level Summary", 
           x = "Team", y = "Coral score", fill = "Level") +
      scale_fill_manual(values=c("plum1","plum2","plum3","plum4","steelblue2","steelblue3","steelblue","steelblue4"))+
      
      theme_bw() 
  }
  
  #ENDGAME BAR GRAPH
  endgame_graph <- function(raw, red_alliance = c(params$red1, params$red2, params$red3), blue_alliance= c(params$blue1, params$blue2, params$blue3)) {
    
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
      geom_bar(position= "stack", stat = "identity") +
      
      labs(title = "Endgame Score",
           x = "Team",
           y = "Points") +
      scale_fill_manual(
        values = c("D" = "springgreen4", "S" = "olivedrab3", "P" = "#FFF68F"),
        labels = c("D" = "Deep", "S" = "Shallow", "P" = "Park", "ending" = "Cage")
      )
  }
  

  
  #Server logic for generating alliance graph
  observeEvent(input$generate_graph, {
    #Backend team splitting based on user input
    if (input$match_or_teams == "Match Number") {
      match_num <- input$match_num

      #Get the specific row first
      match_row <- match_schedule[match_schedule$Match == match_num, ]

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
    } else {
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
    }

    output$alliance_box_plot_output <- renderPlot({
      boxplot_graph_alliance(raw, selected_red_teams, selected_blue_teams)
    })
    
    output$alliance_coral_graph_output <- renderPlot({
        bar_graph_alliance(raw, selected_red_teams, selected_blue_teams)
      })
    
    output$alliance_tele_auto_graph_output <- renderPlot({
      tele_auto_graph(raw, selected_red_teams, selected_blue_teams)
      }) 
    
    output$alliance_endgame_graph_output <- renderPlot({
      endgame_graph(raw, selected_red_teams, selected_blue_teams)
      })
    
    output$alliance_table <- renderDT({
      alliance_dt_subset <- consolidated_team_data[consolidated_team_data$team %in% c(selected_red_teams, selected_blue_teams), ]
      #team_data_row <- consolidated_team_data[consolidated_team_data$team == selected_team]
      datatable(alliance_dt_subset, options = list(scrollX = TRUE, dom = 't'))
    })
    
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
               ifelse(ending =="D", 12, ifelse(ending=="S",6,ifelse(ending=="P", 2, 0)))
      )
    
    boxplot$total = boxplot$total_algae_score + boxplot$total_coral_score + boxplot$total_endgame_score
    
    ggplot(boxplot, aes(x = total, y = as.character(team))) +    
      geom_boxplot(position = "dodge2", fill = "azure2") +
      stat_boxplot(geom = "errorbar") + 
      stat_summary(fun = mean, geom = "point", size = 1, color = "orange") +
      labs(title = paste("Total points scored by Team", team_num),
           x = "Points", 
           y = "Team") +
      theme_bw() 
      #coord_fixed(ratio = 20)
  }
  
  #CORAL LEVEL
  bar_graph_single <- function(raw, team_num) {
    
    bar <- raw %>%
      filter(team == team_num) %>%
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
    
    p <- ggplot(bar, aes(x = factor(team), y = level_score, fill = level)) + 
      geom_bar(position = "stack", stat = "identity", width = 0.3) + 
      labs(title = paste("Level Summary for Team", team_num), 
           x = "Team", y = "Coral score", fill = "Level") +
      scale_fill_manual(values=c("lightskyblue","royalblue1","royalblue3","navy")) +
      theme_bw()
    
    return(p)
  }
  
  #TELE AUTO CORAL
  tele_auto_graph_single <- function(raw, team_num) {
    
    auto_tele <- raw %>%
      filter(team == team_num) %>%
      group_by(team) %>%
      summarize(
        matches = n(),
        
        l1 = mean(coral_L1_num),
        l2 = mean(coral_L2_num),
        l3 = mean(coral_L3_num),
        l4 = mean(coral_L4_num),
        autol1 = mean(auto_coral_L1_num),
        autol2 = mean(auto_coral_L2_num),
        autol3 = mean(auto_coral_L3_num),
        autol4 = mean(auto_coral_L4_num)
      ) %>%
      pivot_longer(cols = c(l4, l3, l2, l1, autol1, autol2, autol3, autol4), 
                   names_to = "level", 
                   values_to = "coral_num")
    
    auto_tele$level_score <- case_when(
      auto_tele$level == "l1" ~ auto_tele$coral_num*2, 
      auto_tele$level == "l2" ~ auto_tele$coral_num*3, 
      auto_tele$level == "l3" ~ auto_tele$coral_num*4, 
      auto_tele$level == "l4" ~ auto_tele$coral_num*5,
      auto_tele$level == "autol1" ~ auto_tele$coral_num*3,
      auto_tele$level == "autol2" ~ auto_tele$coral_num*4,
      auto_tele$level == "autol3" ~ auto_tele$coral_num*6,
      auto_tele$level == "autol4" ~ auto_tele$coral_num*7
    )
    
    ggplot(auto_tele, aes(x = factor(team), y = level_score, fill = level)) + 
      geom_bar(position = "stack", stat = "identity", width = 0.3) + 
      labs(title = paste("Level Summary for Team", team_num), 
           x = "Team", y = "Coral score", fill = "Level") +
      scale_fill_manual(values=c("plum1","plum2","plum3","plum4",
                                 "steelblue2","steelblue3","steelblue","steelblue4")) +
      theme_bw() 
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
        values = c("D" = "springgreen4", "S" = "olivedrab3", "P" = "#FFF68F"),
        labels = c("D" = "Deep", "S" = "Shallow", "P" = "Park", "ending" = "Cage")
      )
  }
  
  output$team_graph_output <- renderPlot({
    selected_team <- input$team_select
    
    if (input$team_graph == "Overall Points Box Plot"){
      
      boxplot_graph_single(raw, selected_team)
    } else if (input$team_graph == "Coral Level Bar Graph"){
      bar_graph_single(raw, selected_team)
    } else if (input$team_graph == "Coral Auto + Tele Bar Graph"){
      tele_auto_graph_single(raw, selected_team)
    } else if (input$team_graph == "Endgame Bar Graph"){
      endgame_graph_single(raw, selected_team)
    }
  })
  
  output$scouter_graph_output <- renderPlot({
    #selected_team <- input$team_select
    scouter_graph_output(raw)
  })
  
  output$team_image_output <- renderImage({
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
  
  output$team_data_row <- renderDT({
    selected_team <- input$team_select
    team_data_row <- consolidated_team_data[consolidated_team_data$team == selected_team, ]
    datatable(team_data_row, options = list(scrollX = TRUE, pageLength = 1, dom = 't'))
  })
  
  #SCOUTERS
  scouter_graph_output <- function(raw){
    scout_df <- raw %>%
      group_by(scout) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    scout_df$scout <- factor(scout_df$scout, levels = scout_df$scout)
    
    ggplot(scout_df, aes(x = `scout`, count)) + 
      geom_bar(position = "stack", stat = "identity", fill = "coral3") + 
      labs(title = "Scouter Summary", 
           x = "Scouters", y = "Times Scouted") +
    theme_bw()
      
  }
}
shinyApp(ui, server)