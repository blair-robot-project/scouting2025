library(tidyverse)

raw <- read_csv("Shiny App Files/data files/match_data_glen_allen.csv")

# manually copied
epas <- c(19.8, 26.3, 21.7, 54.4, 17, 20.6, 24.1, 6.2, 28.4, 19.2, 17.6, 21.8, 
          57.1, 25.7, 44.4, 6.4, 13.1, 14.5, 4.8, 22.7, 2.6, 13.5, 11.6, 17.3, 
          33.5, 19.3, 26.5, 41.8, 26.1, 14.9, 14.7, 22.9, 11.6, 13.9)

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

tldf <- mldf %>%
  group_by(team) %>%
  summarize(scouted = mean(total_pts))

tldf$epa <- epas
tldf$diff <- tldf$scouted - tldf$epa

ggplot(tldf, aes(x = epa, y = scouted)) + 
  geom_point() + 
  geom_abline(col = "red", lty = 2) + 
  theme_bw() + 
  labs(title = "EPA against Scouted Data (2025vagle)", 
       x = "EPA", y = "Scouted Data (pts/match)")

ggplot(tldf, aes(x = epa, y = diff)) + 
  geom_point() + 
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  theme_bw() + 
  labs(title = "EPA Residuals Plot (2025vagle)", 
       x = "EPA", y = "Error in pts/match")
