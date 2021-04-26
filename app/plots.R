library (cricketr)
library (tidyverse)

# The library "cricketr" is extremely useful. It makes it easy to scrape data
# on individual players and teams from the website ESPNCricinfo.

kohli <- getPlayerDataTT(profile = 253802, 
                         dir = "raw_data",
                         file = "kohli_T20.csv",
                         type = "batting")


sharma <- getPlayerDataTT(profile = 34102, 
                          dir = "raw_data",
                          file = "sharma_T20.csv",
                          type = "batting")

dhawan <- getPlayerDataTT(profile = 28235, 
                          dir = "raw_data",
                          file = "sharma_T20.csv",
                          type = "batting")

klrahul <- getPlayerDataTT(profile = 422108, 
                           dir = "raw_data",
                           file = "sharma_T20.csv",
                           type = "batting")

india <- getTeamData(teamName = "India",
                     dir = "raw_data",
                     file = "India_T20.csv",
                     matchType = "T20")


# Creating a vector of all the batsmen selected, so I can call upon it in my
# app.R file

batsman <- c("kohli", "sharma", "dhawan", "klrahul")


# My analysis begins here. First I need to clean the data a bit.

kohli_pdata  <- kohli %>%
  mutate(innings = row_number()) %>%
  
  # Removing innings where the game was abandoned or Kohli did not bat
  
  subset(kohli$Runs != "TDNB" & kohli$Runs != "DNB") %>%
  
  # Removing the asterisk from the "Runs" column, the "Dismissal" column 
  # captures the not-outs already
  
  mutate(Runs = gsub("\\*", "", Runs))

# Need to convert the Runs, BF (Balls Faced) columns into numeric variables

kohli_pdata$Runs <- as.numeric(as.character(kohli_pdata$Runs))
kohli_pdata$BF <- as.numeric(as.character(kohli_pdata$BF))

# I can now create a plot of runs scored, by innings

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
  

# The same template can be used for other players. I will repeat the
# same method for Rohit Sharma, Shikhar Dhawan, and KL Rahul

# Rohit Sharma

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

# Shikhar Dhawan

dhawan_pdata  <- dhawan %>%
  mutate(innings = row_number()) %>%
  subset(dhawan$Runs != "TDNB" & dhawan$Runs != "DNB") %>%
  mutate(Runs = gsub("\\*", "", Runs))

dhawan_pdata$Runs <- as.numeric(as.character(dhawan_pdata$Runs))
dhawan_pdata$BF <- as.numeric(as.character(dhawan_pdata$BF))

dhawan_runs_plot <- dhawan_pdata %>%
  ggplot(aes(x = innings, 
             y = Runs)) +
  geom_point(color = "red", size = 2) + 
  geom_segment(aes(x= innings, xend= innings, y=0, yend= Runs), 
               color = "dark blue") +
  scale_x_continuous(labels = scales::number_format(accuracy = 1),
                     n.breaks = 10) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     n.breaks = 15) +
  labs(title = "T20i Runs Scored by Shikhar Dhawan, by innings",
       x = "Innings number",
       caption = "Source: ESPNCricinfo")

# KL Rahul

klrahul_pdata  <- klrahul %>%
  mutate(innings = row_number()) %>%
  subset(klrahul$Runs != "TDNB" & klrahul$Runs != "DNB") %>%
  mutate(Runs = gsub("\\*", "", Runs))

klrahul_pdata$Runs <- as.numeric(as.character(klrahul_pdata$Runs))
klrahul_pdata$BF <- as.numeric(as.character(klrahul_pdata$BF))

klrahul_runs_plot <- klrahul_pdata %>%
  ggplot(aes(x = innings, 
             y = Runs)) +
  geom_point(color = "red", size = 2) + 
  geom_segment(aes(x= innings, xend= innings, y=0, yend= Runs), 
               color = "dark blue") +
  scale_x_continuous(labels = scales::number_format(accuracy = 1),
                     n.breaks = 10) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     n.breaks = 15) +
  labs(title = "T20i Runs Scored by KL Rahul, by innings",
       x = "Innings number",
       caption = "Source: ESPNCricinfo")

# Now calculating the cumulative strike-rate over the course of a batsman's 
# innings, first replacing any stray hyphens with 0

kohli_pdata$SR[kohli_pdata$SR== "-"] <- 0

kohli_pdata <- kohli_pdata %>%
  
  # Strike rate is calculated as runs scored divided by balls faced, * 100
  
  mutate(cumulative_SR = cumsum(kohli_pdata$Runs)/cumsum(kohli_pdata$BF) * 100)

# Now we can create a line-plot charting the evolution of Strike Rate over the course
# of a batsman's innings

kohli_SR_plot <- kohli_pdata %>%
  ggplot(aes(x=innings, y=cumulative_SR)) +
  geom_line(color = "red") +
  labs(title = "T20i Strike Rate of Virat Kohli, by innings",
       x = "Innings number",
       y = "Strike Rate",
       caption = "Source: ESPNCricinfo")

# We can repeat the template for our other batsman

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

# Shikhar Dhawan

dhawan_pdata$SR[dhawan_pdata$SR== "-"] <- 0

dhawan_pdata <- dhawan_pdata %>%
  mutate(cumulative_SR = cumsum(dhawan_pdata$Runs)/cumsum(dhawan_pdata$BF) * 100)

dhawan_SR_plot <-dhawan_pdata %>%
  ggplot(aes(x=innings, y=cumulative_SR)) +
  geom_line(color = "red") +
  labs(title = "T20i Strike Rate of Shikhar Dhawan, by innings",
       x = "Innings number",
       y = "Strike Rate",
       caption = "Source: ESPNCricinfo")

# KL Rahul

klrahul_pdata$SR[klrahul_pdata$SR== "-"] <- 0

klrahul_pdata <- klrahul_pdata %>%
  mutate(cumulative_SR = cumsum(klrahul_pdata$Runs)/cumsum(klrahul_pdata$BF) * 100)

klrahul_SR_plot <- klrahul_pdata %>%
  ggplot(aes(x=innings, y=cumulative_SR)) +
  geom_line(color = "red") +
  labs(title = "T20i Strike Rate of KL Rahul, by innings",
       x = "Innings number",
       y = "Strike Rate",
       caption = "Source: ESPNCricinfo")

library(rstanarm)
library(ggdist)

# Does it make sense to do a model of each batsman separately? Let me try it out
# with Virat Kohli's statistics

fit_test <- stan_glm(Runs ~ BF + Pos,
                     data = kohli_pdata,
                     refresh = 0,
                     seed = 71)

print(fit_test, digits = 4)

# Probably not. The MAD_SD values are really large and there aren't really 
# enough data points to have a robust posterior. 
# Instead, let us try to construct a model for average runs scored by combinations
# of opening batsman together.

# First, we need to add a new column making sure we can differentiate 
# the data from each other, selecting for the relevant variables

kohli_mdata <- kohli_pdata %>%
  mutate(batsman = "kohli") %>%
  select(Runs, BF, Pos, batsman)

sharma_mdata <- sharma_pdata %>%
  mutate(batsman = "sharma") %>%
  select(Runs, BF, Pos, batsman)

dhawan_mdata <- dhawan_pdata %>%
  mutate(batsman = "dhawan") %>%
  select(Runs, BF, Pos, batsman)

klrahul_mdata <- klrahul_pdata %>%
  mutate(batsman = "klrahul") %>%
  select(Runs, BF, Pos, batsman)

# Now we can bring the data together

all_batsmen <- full_join(kohli_mdata, sharma_mdata) %>%
  full_join(dhawan_mdata) %>%
  full_join(klrahul_mdata)

# Positions 1 and 2 are interchangeable (both are openers, only depends on who
# takes strike)

all_batsmen$Pos[all_batsmen$Pos == "1" | all_batsmen$Pos == "2"] <- "opening"

# Now we can refit our model, with this new data, and then create a posterior 
# probability distribution. I am using posterior_predict() because we are 
# predicting future values based on the data we already have available.

# fit_1 <- stan_glm(Runs ~ BF + Pos + batsman,
#                  data = all_batsmen,
#                  refresh = 0,
#                  seed = 71)

# print(fit_1, digits = 4)

# These values don't make much intuitive sense. So, let's take some simplifying
# assumptions while building our model. Let's only take Positions 1 and 2, for
# the opening batsmen. Furthermore, let's assume that the Balls Faced by each 
# batsman is 36 (or 6 overs). 
# Question: assuming an opening partnership of 12 overs, how many runs would each
# combination be expected to score?

# Pos = c("opening")
# BF = 36
# batsman = unique(all_batsmen$batsman)

# newobs <- expand_grid(Pos, BF, batsman) %>%
#  mutate(names = paste(batsman, Pos, sep = "_"))

# model_data <- posterior_predict(fit_1,
#                                newdata = newobs) %>% 
#  as_tibble() %>%
#  set_names(newobs$names)

# write_rds(model_data, "model.rds")

model_data <- read_rds("model.rds")

# Great. Now, we need to make a number of plots based on this information. First,
# let's just plot the average runs scored per batsman as an opener.

plot_1 <- model_data %>%
  rename(`Virat Kohli` = `kohli_opening`,
         `Rohit Sharma` = `sharma_opening`,
         `Shikhar Dhawan` = `dhawan_opening`,
         `KL Rahul` = `klrahul_opening`) %>%
  pivot_longer(names_to = "batsman",
               values_to = "values",
               cols = `Virat Kohli`:`KL Rahul`) %>%
  ggplot(aes(x = values, y = fct_reorder(batsman, values))) +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, .width = c(0.95, 1)))),
               show.legend = FALSE) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1),
                     n.breaks = 10) +
  labs(title = "Expected Runs as Opener for India's Top Batsmen",
       subtitle = "KL Rahul has the highest projected runs, though all four are within a close range of values",
       x = "Average Runs Scored Per Innings (assuming 36 balls faced)",
       y = "Name of Batsman",
       caption = "Source: ESPNCricinfo")

# We can also construct a plot to visualize the best opening combinations

plot_2 <- model_data %>%
  mutate(`Kohli + Sharma` = `kohli_opening` + `sharma_opening`,
         `Kohli + Dhawan` = `kohli_opening` + `dhawan_opening`,
         `Kohli + Sharma` = `kohli_opening` + `klrahul_opening`,
         `Sharma + KL Rahul` = `sharma_opening` + `klrahul_opening`,
         `Sharma + Dhawan` = `sharma_opening` + `dhawan_opening`,
         `Dhawan + KL Rahul` = `dhawan_opening` + `klrahul_opening`) %>%
  pivot_longer(names_to = "combination",
               values_to = "values",
               cols = `Kohli + Sharma`:`Dhawan + KL Rahul`) %>%
  ggplot(aes(x = values, y = fct_reorder(combination, values))) +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, .width = c(0.95, 1)))),
               show.legend = FALSE) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1),
                     n.breaks = 10) +
  labs(title = "Expected Runs per Opening Combination for India",
       subtitle = "Sharma + KL Rahul is the settled combination, but Kohli + Sharma is a close second",
       x = "Runs Scored per Innings
       (assuming 36 balls faced by each batsman)",
       y = "Name of Batsmen",
       caption = "Source: ESPNCricinfo")

