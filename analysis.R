library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)

data_scientist <- read.table("data scientist offers.csv", colClasses = c("character", "character"))
data_analyst <- read.table("data analyst offers.csv", colClasses = c("character", "character"))

all_data <- rbind(data_scientist, data_analyst)

all_data_stats <- all_data %>% 
  mutate(descr = str_replace(descr, "\\n", " ")) %>%
  mutate(R = str_detect(descr, "R[[:punct:][:space:]]")) %>%
  mutate(python = str_detect(descr, fixed("python", ignore_case = T))) %>%
  mutate(degree = str_detect(descr, 
        "[Dd]egree | [Ww]ykształcenie | [Uu]niversity | MSc | [Mm]aster"))

plot_data <- all_data_stats %>% select(R, python, degree) %>%
  summarise(R_y = sum(R), R_n = n() - sum(R),
            python_y = sum(python), python_n = n() - sum(python),
            degree_y = sum(degree), degree_n = n() - sum(degree)) %>%
  pivot_longer(everything(), names_to = "req")


# Charts

# Required R
plot_data %>% filter(str_detect(req, "R_")) %>% 
  mutate(ypos = cumsum(value)- 0.5*value) %>%
  ggplot(aes(x = "",y = value, fill = req)) + 
  geom_bar(stat = "identity", color = "white") + coord_polar("y") + theme_void() + 
  geom_text(aes(y = ypos, label = scales::percent(value/nrow(all_data_stats), accuracy = 0.1)),
          color = "white", size=6) +
  theme(legend.position="none")

# Required Python
plot_data %>% filter(str_detect(req, "py")) %>% 
  mutate(ypos = cumsum(value)- 0.5*value) %>% # positions of text labels
  ggplot(aes(x = "",y = value, fill = req)) + 
  geom_bar(stat = "identity", color = "white") + coord_polar("y") + theme_void() + 
  geom_text(aes(y = ypos, label = scales::percent(value/nrow(all_data_stats), accuracy = 0.1)), 
            color = "white", size=6) +
  theme(legend.position="none")

# Required degree
plot_data %>% filter(str_detect(req, "deg")) %>% 
  mutate(ypos = cumsum(value) - 0.5*value) %>% # positions of text labels
  ggplot(aes(x = "",y = value, fill = req)) + 
  geom_bar(stat = "identity", color = "white") + 
  coord_polar("y", direction = -1) + theme_void() + # anti-clockwise direction
  geom_text(aes(y = ypos, label = scales::percent(value/nrow(all_data_stats), accuracy = 0.1)), 
            color = "white", size=6) +
  theme(legend.position="none")
