library(tidyverse)

kohli_ODI_pdata  <- kohli_ODI %>%
  mutate(innings = row_number()) %>%
  
  # Removing innings where the game was abandoned or Kohli did not bat
  
  subset(kohli_ODI$Runs != "TDNB" & kohli_ODI$Runs != "DNB") %>%
  
  # Removing the asterisk from the "Runs" column, the "Dismissal" column 
  # captures the not-outs already
  
  mutate(Runs = gsub("\\*", "", Runs))

kohli_ODI_pdata$Runs <- as.numeric(as.character(kohli_ODI_pdata$Runs))

kohli_ODIruns_plot <- kohli_ODI_pdata %>%
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
       caption = "Source: ESPNCricinfo")
  
ggsave("kohli_ODIruns_plot.png", kohli_ODIruns_plot)


# The same template can be used for other players, such as Rohit Sharma

sharma_ODI_pdata  <- sharma_ODI %>%
  mutate(innings = row_number()) %>%
  subset(sharma_ODI$Runs != "TDNB" & sharma_ODI$Runs != "DNB") %>%
  mutate(Runs = gsub("\\*", "", Runs))

sharma_ODI_pdata$Runs <- as.numeric(as.character(sharma_ODI_pdata$Runs))

sharma_ODIruns_plot <- sharma_ODI_pdata %>%
  ggplot(aes(x = innings, 
             y = Runs)) +
  geom_point(color = "red", size = 2) + 
  geom_segment(aes(x= innings, xend= innings, y=0, yend= Runs), 
               color = "dark blue") +
  scale_x_continuous(labels = scales::number_format(accuracy = 1),
                     n.breaks = 10) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     n.breaks = 15) +
  labs(title = "ODI Runs Scored by Rohit Sharma, by innings",
       x = "Innings number",
       caption = "Source: ESPNCricinfo")

ggsave("sharma_ODIruns_plot.png", sharma_ODIruns_plot)


# Now calculating the cumulative strike-rate over the course of a batsman's 
# innings, first replacing any stray hyphens with 0

kohli_ODI_pdata$SR[kohli_ODI_pdata$SR== "-"] <- 0

kohli_ODI_pdata <- kohli_ODI_pdata %>%
  
  # Strike rate is calculated as runs scored divided by balls faced, * 100
  
  mutate(cumulative_SR = cumsum(kohli_ODI_pdata$Runs)/cumsum(kohli_ODI_pdata$BF) * 100)

# Now we can create a line-plot charting the evolution of Strike Rate over the course
# of a batshman's innings

kohli_ODISR_plot <- kohli_ODI_pdata %>%
  ggplot(aes(x=innings, y=cumulative_SR)) +
  geom_line(color = "red") +
  labs(title = "ODI Strike Rate of Virat Kohli, by innings",
       x = "Innings number",
       y = "Strike Rate",
       caption = "Source: ESPNCricinfo")

ggsave("kohli_ODISR_plot.png", kohli_ODISR_plot)

# We can repeat the template for any other batsman

sharma_ODI_pdata$SR[sharma_ODI_pdata$SR== "-"] <- 0

sharma_ODI_pdata <- sharma_ODI_pdata %>%
  mutate(cumulative_SR = cumsum(sharma_ODI_pdata$Runs)/cumsum(sharma_ODI_pdata$BF) * 100)

sharma_ODISR_plot <-sharma_ODI_pdata %>%
  ggplot(aes(x=innings, y=cumulative_SR)) +
  geom_line(color = "red") +
  labs(title = "ODI Strike Rate of Rohit Sharma, by innings",
       x = "Innings number",
       y = "Strike Rate",
       caption = "Source: ESPNCricinfo")

ggsave("sharma_ODISR_plot.png", sharma_ODISR_plot)
