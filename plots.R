library(tidyverse)

kohli_ODI_pdata  <- kohli_ODI %>%
  mutate(innings = row_number()) %>%
  
  # Removing innings where the game was abandoned or Kohli did not bat
  
  subset(kohli_ODI$Runs != "TDNB" & kohli_ODI$Runs != "DNB") %>%
  
  # Removing the asterisk from the "Runs" column, the "Dismissal" column 
  # captures the not-outs already
  
  mutate(Runs = gsub("\\*", "", Runs))

kohli_ODI_pdata$Runs <- as.numeric(as.character(kohli_ODI_pdata$Runs))

kohli_ODI_plot <- kohli_ODI_pdata %>%
  ggplot(aes(x = innings, 
             y = Runs)) +
  geom_point(color = "red", size = 2) + 
  geom_segment(aes(x= innings, xend= innings, y=0, yend= Runs), 
                   color = "dark blue") +
  scale_x_continuous(labels = scales::number_format(accuracy = 1),
                     n.breaks = 10) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     n.breaks = 15) +
  labs(title = "ODI Runs Scored by Virat Kohli, by innings",
       x = "Innings number",
       caption = "Source: ESPNCricinfo" )
  
ggsave("kohli_ODI_plot.png", kohli_ODI_plot)

