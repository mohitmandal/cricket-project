library(cricketr)
library(tidyverse)
library(ggthemes)
library(ggdist)

# DATA WRANGLING AND CLEANING

# The library "cricketr" is extremely useful. It makes it easy to scrape data
# on individual players and teams from the website ESPNCricinfo. Credits to
# Tinniam V. Ganesh!

# I don't need to save the data to a CSV file as such, I can just save them to
# tibbles, but the 'file' part of the function is required.

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
                          file = "dhawan_T20.csv",
                          type = "batting")

klrahul <- getPlayerDataTT(profile = 422108, 
                           dir = "raw_data",
                           file = "klrahul_T20.csv",
                           type = "batting")

# I want to create a plot to orient the reader to the different formats of cricket
# on the home page. For that, I will need to scrape team data. T20, ODI and Test
# refers to the three different formats of cricket.

india_T20 <- getTeamData(teamName = "India",
                     dir = "raw_data",
                     file = "India_T20.csv",
                     matchType = "T20")

india_ODI <- getTeamData(teamName = "India",
                     dir = "raw_data",
                     file = "India_ODI.csv",
                     matchType = "ODI")

india_test <- getTeamData(teamName = "India",
                     dir = "raw_data",
                     file = "India_test.csv",
                     matchType = "Test")

# DATA ANALYSIS

# I will start by creating a plot with the number of games played by India in
# each format since 2004, which was when T20 cricket was sanctioned. The tibble
# I just scraped includes information about the day each game is played, so it
# should be easy to wrangle the nunber of games played per year.

# The as.Date() function ensures that the date column is, in fact, a date. The
# '%b' is R's way of referring to the abbreviations for month (such as Dec, Jan,
# etc). I want to extract the year for each game.

india_T20$`Start Date` <- as.Date(india_T20$`Start Date`, format = "%d %b %Y")

india_T20$year <- as.numeric(format(india_T20$`Start Date`, "%Y"))

# I can now calculate the number of games played per year.

T20s_played <- india_T20 %>% 
  select(year) %>%
  group_by(year) %>% 
  summarise(T20s = n())

# We can use the same technique for ODIs. The code is exactly the same.

india_ODI$`Start Date` <- as.Date(india_ODI$`Start Date`, format = "%d %b %Y")

india_ODI$year <- as.numeric(format(india_ODI$`Start Date`, "%Y"))

ODIs_played <- india_ODI %>% 
  select(year) %>%
  filter(year >= 2004) %>%
  group_by(year) %>% 
  summarise(ODIs = n())

# And now for Test matches

india_test$`Start Date` <- as.Date(india_test$`Start Date`, format = "%d %b %Y")

india_test$year <- as.numeric(format(india_test$`Start Date`, "%Y"))

# Counting test matches requires slightly different coding, because a test match
# has two innings

tests_played <- india_test %>% 
  select(year, Inns) %>%
  filter(year >= 2004 & (Inns == "1" | Inns == "2")) %>%
  group_by(year) %>% 
  summarise(Tests = n())

# Now, I can create a plot with these results.

games_played <- full_join(T20s_played, ODIs_played, by = "year") %>%
                full_join(tests_played, by = "year") %>%
  pivot_longer(names_to = "format",
               values_to = "number",
               cols = T20s:Tests)

# To have the bars show up next to each other for easy comparison, "dodge" is a
# useful command.

plot_games <- ggplot(data = games_played,
       aes(x = year, y = number, fill = format)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = seq(2004, 2021, by = 1)) +
  labs(title = "Games played by India since 2004, by format",
       subtitle = "ODIs, previously the most popular format, is on the wane, increasingly subsituted with T20s since the past 7 years",
       x = "Year",
       y = "Games played",
       caption = "source: ESPNCricinfo")
  

# Now for the next step of analysis! 
# Creating a vector of all the batsmen selected, so I can call upon it in my
# app.R file

batsman <- c("kohli", "sharma", "dhawan", "klrahul")

# My analysis of batsmen begins here. First I need to clean the data a bit. I 
# want a column with the number of each individual innings played.

kohli_pdata  <- kohli %>%
  mutate(innings = row_number()) %>%
  
  # Removing innings where the game was abandoned or Kohli did not bat
  
  subset(kohli$Runs != "TDNB" & kohli$Runs != "DNB") %>%
  
  # Removing the asterisk from the "Runs" column, the "Dismissal" column 
  # captures the not-outs already
  
  mutate(Runs = gsub("\\*", "", Runs))

# I need to convert the Runs, BF (Balls Faced) columns into numeric variables

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
# same method for Rohit Sharma, Shikhar Dhawan, and KL Rahul. The code is exactly
# the same, hence why I have not included many comments below.

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
# innings, first replacing any stray hyphens that exist in the tibble with 0.

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


# FITTING A MODEL

# library(rstanarm)

# Does it make sense to do a model of each batsman separately? Let me try it out
# with Virat Kohli's statistics

# fit_test <- stan_glm(Runs ~ BF + Pos,
#                     data = kohli_pdata,
#                     refresh = 0,
#                     seed = 71)

# print(fit_test, digits = 4)

# Probably not. The MAD_SD values are really large and there aren't really 
# enough data points to have a robust posterior. 
# Instead, let us try to construct a model for average runs scored by combinations
# of opening batsman together.

# First, we need to add a new column making sure we can differentiate 
# the batsmen from each other, before selecting for the relevant variables.

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

# Now we can bring the data together into one easy-to-read tibble.

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
  
  # The fct_reorder() here ensures that the plots are arranged in descending order.
  
  ggplot(aes(x = values, y = fct_reorder(batsman, values))) +
  
  # I like the stat_halfeye function because it portrays the posterior in an
  # easy-to-read form, with clear distinctions for the 95% confidence interval
  
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, .width = c(0.95, 1)))),
               show.legend = FALSE) +
  scale_fill_calc() +
  scale_x_continuous(labels = scales::number_format(accuracy = 1),
                     n.breaks = 10) +
  labs(title = "Expected Runs as Opener for India's Top Batsmen",
       subtitle = "KL Rahul has the highest projected runs, though all four are within a close range of values",
       x = "Average Runs Scored Per Innings (assuming 36 balls faced)",
       y = "Name of Batsman",
       caption = "Source: ESPNCricinfo",
       fill = "Interval")

# We can also construct a plot to visualize the best opening combinations

plot_2 <- model_data %>%
  
  # Total runs scored by each combination is just the sum of each individual's
  # predicted runs, taking the same assumptions as before.
  
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
       x = "Runs Scored per Innings (assuming 36 balls faced by each batsman)",
       y = "Name of Batsmen",
       caption = "Source: ESPNCricinfo")

