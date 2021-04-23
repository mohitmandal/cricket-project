library (cricketr)
library (tidyverse)

kohli <- getPlayerDataTT(profile = 253802, 
                         dir = "raw_data",
                         file = "kohli_T20.csv",
                         type = "batting")


sharma <- getPlayerDataTT(profile = 34102, 
                          dir = "raw_data",
                          file = "sharma_T20.csv",
                          type = "batting")

batsman <- c("kohli", "sharma")

kohli_pdata  <- kohli %>%
  mutate(innings = row_number()) %>%
  
  # Removing innings where the game was abandoned or Kohli did not bat
  
  subset(kohli$Runs != "TDNB" & kohli$Runs != "DNB") %>%
  
  # Removing the asterisk from the "Runs" column, the "Dismissal" column 
  # captures the not-outs already
  
  mutate(Runs = gsub("\\*", "", Runs))

# Need to convert the Runs and BF columns into numeric variables

kohli_pdata$Runs <- as.numeric(as.character(kohli_pdata$Runs))
kohli_pdata$BF <- as.numeric(as.character(kohli_pdata$BF))

kohli_runs_plot <- kohli_pdata %>%
  ggplot(aes(x = innings, 
             y = Runs)) +
  geom_point(color = "red", size = 2) + 
  geom_segment(aes(x= innings, xend= innings, y=0, yend= Runs), 
                   color = "dark blue") +
  scale_x_continuous(labels = scales::number_format(accuracy = 1),
                     n.breaks = 10) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     n.breaks = 15) +
  labs(title = "T20i Runs Scored by Virat Kohli, by innings",
       x = "Innings number",
       caption = "Source: ESPNCricinfo")
  

# The same template can be used for other players, such as Rohit Sharma

sharma_pdata  <- sharma %>%
  mutate(innings = row_number()) %>%
  subset(sharma$Runs != "TDNB" & sharma$Runs != "DNB") %>%
  mutate(Runs = gsub("\\*", "", Runs))

sharma_pdata$Runs <- as.numeric(as.character(sharma_pdata$Runs))
sharma_pdata$BF <- as.numeric(as.character(sharma_pdata$BF))

sharma_runs_plot <- sharma_pdata %>%
  ggplot(aes(x = innings, 
             y = Runs)) +
  geom_point(color = "red", size = 2) + 
  geom_segment(aes(x= innings, xend= innings, y=0, yend= Runs), 
               color = "dark blue") +
  scale_x_continuous(labels = scales::number_format(accuracy = 1),
                     n.breaks = 10) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     n.breaks = 15) +
  labs(title = "T20i Runs Scored by Rohit Sharma, by innings",
       x = "Innings number",
       caption = "Source: ESPNCricinfo")


# Now calculating the cumulative strike-rate over the course of a batsman's 
# innings, first replacing any stray hyphens with 0

kohli_pdata$SR[kohli_pdata$SR== "-"] <- 0

kohli_pdata <- kohli_pdata %>%
  
  # Strike rate is calculated as runs scored divided by balls faced, * 100
  
  mutate(cumulative_SR = cumsum(kohli_pdata$Runs)/cumsum(kohli_pdata$BF) * 100)

# Now we can create a line-plot charting the evolution of Strike Rate over the course
# of a batshman's innings

kohli_SR_plot <- kohli_pdata %>%
  ggplot(aes(x=innings, y=cumulative_SR)) +
  geom_line(color = "red") +
  labs(title = "T20i Strike Rate of Virat Kohli, by innings",
       x = "Innings number",
       y = "Strike Rate",
       caption = "Source: ESPNCricinfo")

# We can repeat the template for any other batsman

sharma_pdata$SR[sharma_pdata$SR== "-"] <- 0

sharma_pdata <- sharma_pdata %>%
  mutate(cumulative_SR = cumsum(sharma_pdata$Runs)/cumsum(sharma_pdata$BF) * 100)

sharma_SR_plot <-sharma_pdata %>%
  ggplot(aes(x=innings, y=cumulative_SR)) +
  geom_line(color = "red") +
  labs(title = "T20i Strike Rate of Rohit Sharma, by innings",
       x = "Innings number",
       y = "Strike Rate",
       caption = "Source: ESPNCricinfo")

