#Load required libraries
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(tidyverse)
library(shinythemes)

data_dir <- "data files"
data_file <- paste0(data_dir, "/Severn/data.csv")
event_schedule_file <- paste0(data_dir, "/Severn/schedule.csv")
teams_file <- paste0(data_dir, "/Severn/teams.csv")

#Load team data and event schedule
raw <- read.csv(data_file)
# raw <- transform(raw_raw,
#                  match = as.numeric(match),
#                  team = as.numeric(team),
#                  show = as.logical(show),
#                  move = as.logical(move),
#                  auto_coral_L1_num = as.numeric(auto_coral_L1_num),
#                  auto_coral_L2_num = as.numeric(auto_coral_L2_num),
#                  auto_coral_L3_num = as.numeric(auto_coral_L3_num),
#                  auto_coral_L4_num = as.numeric(auto_coral_L4_num),
#                  robot_reef_removal = as.logical(robot_reef_removal),
#                  robot_algae_picked = as.logical(robot_algae_picked),
#                  robot_net_score = as.numeric(robot_net_score), 
#                  robot_net_miss	= as.numeric(robot_net_miss),
#                  proc_score = as.numeric(proc_score),
#                  coral_L1_num = as.numeric(coral_L1_num),
#                  coral_L2_num = as.numeric(coral_L2_num),
#                  coral_L3_num = as.numeric(coral_L3_num),
#                  coral_L4_num = as.numeric(coral_L4_num),
#                  tele_missed = as.numeric(tele_missed),
#                  climb_attempt = as.logical(climb_attempt),
#                  climb_time = as.logical(climb_time),
#                  fouls = as.numeric(fouls),
#                  defense = as.numeric(driver),
#                  driver = as.numeric(driver),
#                  comments = as.character(comments))
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
        #something
    
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
        #coral cycle
        # l1_cycle = round(mean(auto_coral_L1_num + coral_L1_num), digits = 2),
        # l2_cycle = round(mean(auto_coral_L2_num + coral_L2_num), digits = 2),
        # l3_cycle = round(mean(auto_coral_L3_num + coral_L3_num), digits = 2),
        # l4_cycle = round(mean(auto_coral_L4_num + coral_L4_num), digits = 2),
        # coral_cycle = round(sum(l1_cycle+l2_cycle+l3_cycle+l4_cycle), digits = 2),
        
        l1_cycle_tele = round(mean(coral_L1_num), digits = 2),
        l2_cycle_tele = round(mean(coral_L2_num), digits = 2),
        l3_cycle_tele = round(mean(coral_L3_num), digits = 2),
        l4_cycle_tele = round(mean(coral_L4_num), digits = 2),
        
        coral_cycle_tele = round(sum(l1_cycle_tele+l2_cycle_tele+l3_cycle_tele+l4_cycle_tele), digits = 2),
        coral_cycle_auto = round(sum(mean(auto_coral_L1_num) + mean(auto_coral_L2_num) + mean(auto_coral_L3_num) + mean(auto_coral_L4_num)), digits = 2),

        #algae cycle
        net_cycle = round(mean(robot_net_score), digits = 2),
        proc_cycle = round(mean(proc_score), digits = 2),
        algae = round((sum(net_cycle+proc_cycle)),digits = 2),
    
        #total points
        #total_pts_mean = round(mean(total_pts, na.rm = TRUE), digits =2), 
        total_pts_median = round(median(total_pts, na.rm = TRUE), digits =2),
        #total_pts_sd = round(sd(total_pts, na.rm = TRUE), digits =2), 
        #total_pts_max = round(max(total_pts, na.rm = TRUE), digits =2),
    
    
        #------------don't think these are necessary for the table bc they are in graphs -shriyan
        #coral
        #coral_pts_mean = round(mean(coral_l1_pts + coral_l2_pts + coral_l3_pts + coral_l4_pts, na.rm = TRUE), digits =2), 
    
        #------------don't think these are necessary for the table bc they are in graphs -shriyan
        # l1_pts_mean = round(mean(coral_l1_pts, na.rm = TRUE), digits = 2), 
        # l2_pts_mean = round(mean(coral_l2_pts, na.rm = TRUE), digits = 2), 
        # l3_pts_mean = round(mean(coral_l3_pts, na.rm = TRUE), digits = 2), 
        # l4_pts_mean = round(mean(coral_l4_pts, na.rm = TRUE), digits = 2), 
        # #% in levels
        # l1_pct_mean = round(mean(coral_l1_pts, na.rm = TRUE)/coral_pts_mean*100, digits = 2), 
        # l2_pct_mean = round(mean(coral_l2_pts, na.rm = TRUE)/coral_pts_mean*100, digits = 2), 
        # l3_pct_mean = round(mean(coral_l3_pts, na.rm = TRUE)/coral_pts_mean*100, digits = 2), 
        # l4_pct_mean = round(mean(coral_l4_pts, na.rm = TRUE)/coral_pts_mean*100, digits = 2), 
    
        #teleop
        #tele_pts_mean = round(mean(total_tele_pts, na.rm = TRUE), digits =2), 
        tele_pts_median = round(median(total_tele_pts, na.rm = TRUE), digits =2), 
        #tele_pts_sd = round(sd(total_tele_pts, na.rm = TRUE), digits = 2), 
        #tele_pts_max = round(max(total_tele_pts, na.rm = TRUE), digits = 2), 
    
        #auto
        #auto_pts_mean = round(mean(total_auto_pts, na.rm = TRUE), digits =2 ), 
        auto_pts_median = round(median(total_auto_pts, na.rm = TRUE), digits = 2), 
        #auto_pts_sd = round(sd(total_auto_pts, na.rm = TRUE), digits = 2),
        #auto_pts_max = round(max(total_auto_pts, na.rm = TRUE),digits =2),  
        #auto_move = round(mean(move_pts, na.rm = TRUE), digits = 2),
    
        #algae
        #algae_pts_mean =round(mean(net_pts + processor_value, na.rm = TRUE), digits=2),
        #algae_pts_median = round(median(net_pts + processor_value, na.rm = TRUE), digits = 2),
        #algae_pts_sd = round(sd(net_pts + processor_value, na.rm = TRUE), digits =2),
        #algae_pts_max = round(max(net_pts + processor_value, na.rm = TRUE), digits =2), 
        
        #endgame
        #endgame_pts_mean =round( mean(endgame_pts, na.rm = TRUE), digits =2),
        endgame_pts_median = round(median(endgame_pts, na.rm = TRUE), digits =2), 
        #endgame_pts_sd = round(sd(endgame_pts, na.rm = TRUE), digits =2),
        #endgame_pts_max = round(max(endgame_pts, na.rm = TRUE), digits =2),
        
        #you would put many more calculations here!
        #algae net pct
        #algae remove data?
    
        algae_remove_pct = round(sum(c(robot_reef_removal))/n(), digits = 2),
        move_pct = round(sum(c(move))/n(), digits = 2),
        driver_rating_mean = round(mean(driver[driver != 0], na.rm = TRUE), digits =2 ),
        defense_rating_mean = round(mean(defense[defense != 0], na.rm = TRUE), digits =2 )
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
                                #selectInput("alliance_graph", "Choose Graph", choices = c("Overall Points Box Plot", "Auto Level Bar Graph", "Tele Bar Graph", "Endgame Bar Graph")),
                                actionButton("generate_graph", "Generate Graphs", class = "btn btn-primary"),
                                imageOutput("field_image_output")
                                ),
                            mainPanel(
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
             
                tabPanel("Single Team",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("team_select", "Select Team", choices = unique(teams$team)),
                            selectInput("team_graph", "Choose Graph", choices = c("Overall Points Box Plot", "Auto Bar Graph", "Tele Bar Graph", "Endgame Bar Graph", "Comments")),
                            imageOutput("team_image_output")
                            ),
                        mainPanel(
                            plotOutput("team_graph_output"),
                            DTOutput("team_data_row"),
                            h3("Comments"),
                            DTOutput("comments_list")
                            )
                        )
                    ),
                tabPanel("Scouters",
                    #sidebarLayout(
                    #   sidebarPanel(
                    #     selectInput("Scouter_Select", "Select Scouter", choices = unique(raw$scout)),
                    #selectInput("team_graph", "Choose Graph", choices = c("Overall Points Box Plot", "Coral Level Bar Graph", "Auto + Tele Bar Graph", "Endgame Bar Graph")),
                    #imageOutput("team_image_output")
                    #   ),
                    fluidRow(
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
            total_coral_score = sum((auto_coral_L1_num*3) + 
                                    (auto_coral_L2_num*4) + 
                                    (auto_coral_L3_num*6) + 
                                    (auto_coral_L4_num*7) + 
                                    (coral_L1_num*2) +
                                    (coral_L2_num*3) + 
                                    (coral_L3_num*4) +
                                    (coral_L4_num*5) 
                                    )/n(),
                
            total_coral_cycle =sum(auto_coral_L1_num +
                                       auto_coral_L2_num + 
                                       auto_coral_L3_num + 
                                       auto_coral_L4_num +
                                       coral_L1_num +
                                       coral_L2_num + 
                                       coral_L3_num + 
                                       coral_L4_num)/n(),
            
            endgame_score = sum(ifelse(ending =="D", 12, 
                                ifelse(ending =="S",6,
                                ifelse(ending =="P", 2, 0))))/n()
            
        )
        ggplot(bubble, aes(x=total_coral_score, y=total_coral_cycle, 
                           size = endgame_score)) +
            geom_point( color = "lightblue2")+
            geom_text( aes(label=team, vjust = 1.7 ))+
            labs(title = "Teams Performance Summary", 
                 x = "Auto + Tele Coral Points", y = "Auto + Tele Coral Cycles", 
                 size = "Endgame")+
            theme_bw()
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
        
        p <- ggplot(bar, aes(x = factor(team), y = level_score, fill = level)) + 
            geom_bar(position = "stack", stat = "identity", 
                     color = ifelse(bar$team %in% red_alliance, "red", "blue"),
                     size = 0.5) + 
            labs(title = "Tele Coral Summary", 
                 x = "Team", y = "Coral score", fill = "Level") +
            scale_fill_manual(values=c("lightskyblue","royalblue1","royalblue3","navy")) +
            theme_bw()
        
        return(p)
    }
    
    #CORAL  AUTO  
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
                values = c("D" = "springgreen4", "S" = "olivedrab3", "P" = "#FFF68F"),
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
        
        
        output$alliance_tele_coral_graph_output <- renderPlot({
            tele_coral_alliance(raw, selected_red_teams, selected_blue_teams)
        })
        
        output$alliance_auto_coral_graph_output <- renderPlot({
            auto_coral_alliance(raw, selected_red_teams, selected_blue_teams)
        }) 
        
        output$alliance_algae_bar_graph_output <- renderPlot({
            algae_bar(raw, selected_red_teams, selected_blue_teams)
        }) 
        
        
        output$alliance_endgame_graph_output <- renderPlot({
            endgame_graph(raw, selected_red_teams, selected_blue_teams)
        })
        
        
        output$alliance_all_graph_output <- renderPlot({
            long_column_alliance(raw, selected_red_teams, selected_blue_teams)
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
                values = c("D" = "springgreen4", "S" = "olivedrab3", "P" = "#FFF68F"),
                labels = c("D" = "Deep", "S" = "Shallow", "P" = "Park", "ending" = "Cage")
            ) +
            theme_bw() 
    }
    
    #COMMENTS TABLE
    comment_table_single <- function(raw, team_num){
        comments <- raw%>%
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
    
    
    team_comments <- reactive({
        comments_data <- raw %>%
            filter(team == input$team_select) %>%
            select(commentOpen)  
        if (length(comments_data) > 0) {
            print(comments_data)
            return(comments_data)
        } else {
            return(data.frame())
        }
    })
    
    output$comments_list <- renderDT({
        comments <- team_comments()
        })
    
    output$team_graph_output <- renderPlot({
        selected_team <- input$team_select
        
        if (input$team_graph == "Overall Points Box Plot"){
            boxplot_graph_single(raw, selected_team)
            } 
        else if (input$team_graph == "Auto Bar Graph"){
            auto_graph_single(raw, selected_team)
            } 
        else if (input$team_graph == "Tele Bar Graph"){
            tele_graph_single(raw, selected_team)
            } 
        else if (input$team_graph == "Endgame Bar Graph"){
            endgame_graph_single(raw, selected_team)
            } 
        else if (input$team_graph == "Comments"){
            comment_table_single(raw, selected_team)
            } 
        })
    
    output$scouter_graph_output <- renderPlot({
        scouter_graph_output(raw)
        })
    
    output$team_image_output <- renderImage({
        teamnum <- input$team_select
        img_src <- paste0("images/Severn/", teamnum, ".png")  #Path to the image
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