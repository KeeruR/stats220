library(tidyverse)
library(lubridate)
library(stringr)
library(magick)


install.packages("remotes")
remotes::install_github("annafergusson/cuttingshapes")
library(cuttingshapes)




logged_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSnGZftiMapz_Zg6q_MLISNyoJ3PQ2kFAnSDlLvuXmJQuxxTAMDmfvt9W-rILHm2Ez1NyIkQMsqTQZy/pub?output=csv")
my_colours <- c("#f1eef6", "#d7b5d8", "#df65b0", "#dd1c77", "#980043")



# Plot 1 - Avg entertainment by mood (grouped + summarised)
avg_entertainment <- logged_data %>%
  mutate(timestamp = mdy_hm(Timestamp),
         weekday = wday(timestamp, label = TRUE),
         entertainment = as.numeric(`How many minutes did you spend on entertainment (e.g., streaming, games, social media) on your phone today?`),
         mood = str_to_title(`What was your mood before using the app?`)) %>%
  filter(!is.na(mood)) %>%
  group_by(mood, weekday) %>%
  summarise(avg_ent = mean(entertainment, na.rm = TRUE)) %>%
  arrange(desc(avg_ent))


ggplot(avg_entertainment) +
  geom_col(aes(x = reorder(mood, -avg_ent), y = avg_ent, fill = mood)) +
  scale_fill_manual(values = my_colours) +
  labs(title = "Average Entertainment Minutes by Mood",
       x = "Mood", y = "Average Minutes",
       caption = "Grouped by Mood") +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black", face = "bold"),
    plot.title = element_text(color = "black", face = "bold"),
    plot.caption = element_text(color = "black", face = "bold")
  )

ggsave("plot1.png", width = 8, height = 4)






# Plot 2 - Scatter plot of total vs entertainment use
logged_data <- logged_data %>%
  mutate(total_use = as.numeric(`How many minutes did you use your phone today?`),
         entertainment = as.numeric(`How many minutes did you spend on entertainment (e.g., streaming, games, social media) on your phone today?`))

ggplot(logged_data) +
  geom_point(aes(x = total_use, y = entertainment), color = my_colours[3], alpha = 0.7) +
  labs(title = "Entertainment vs Total Phone Use",
       x = "Total Minutes on Phone",
       y = "Entertainment Minutes",
       caption = "Source: Logged phone usage data") +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black", face = "bold"),
    plot.title = element_text(color = "black", face = "bold"),
    plot.caption = element_text(color = "black", face = "bold")
  )

ggsave("plot2.png", width = 6, height = 4)






# Plot 3 - Frequency of peak phone use time by time-of-day category
logged_data <- logged_data %>%
  mutate(active_time = `At what time of day did you use your phone the most?`,
         time_of_day = case_when(
           active_time == 1 ~ "Early Morning",
           active_time == 2 ~ "Morning",
           active_time == 3 ~ "Afternoon",
           active_time == 4 ~ "Evening",
           active_time == 5 ~ "Night"
         ),
         time_of_day = factor(time_of_day, 
                              levels = c("Early Morning", "Morning", "Afternoon", "Evening", "Night")))

ggplot(logged_data) +
  geom_bar(aes(x = time_of_day), fill = my_colours[5]) +
  labs(title = "Most Common Time of Day for Phone Use",
       x = "Time of Day",
       y = "Number of Entries",
       caption = "Source: Logged phone usage data") +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black", face = "bold"),
    plot.title = element_text(color = "black", face = "bold"),
    plot.caption = element_text(color = "black", face = "bold")
  )

ggsave("plot3.png", width = 6, height = 4)



image_url <- "https://images.pexels.com/photos/9430875/pexels-photo-9430875.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2" 

creative_plot <- cut_shape(
  image = image_url,
  shape = "plot3.png",       
  color = my_colours[5]
)

# Save the new creative image
image_write(creative_plot, "creative_plot3.png")

print(creative_plot)




# Plot 4
logged_data <- logged_data %>%
  mutate(
    mood = str_to_title(`What was your mood before using the app?`),
    phone_minutes = as.numeric(`How many minutes did you use your phone today?`)
  ) %>%
  filter(!is.na(mood))

ggplot(logged_data) +
  geom_boxplot(aes(x = mood, y = phone_minutes, fill = mood),
               colour = "blue",
               alpha = 1,  
               na.rm = TRUE) +
  scale_fill_manual(values = my_colours) +
  labs(
    title = "Phone Use Duration by Mood",
    x = "Mood Before Using App",
    y = "Phone Use Duration (minutes)",
    fill = "Mood",
    caption = "Source: Logged phone usage data | Background: Pexels.com"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black", face = "bold"),
    plot.title = element_text(color = "black", face = "bold"),
    plot.caption = element_text(color = "black", face = "bold")
  )

ggsave("plot4.png", width = 6, height = 4)


# with extra creativity
plot_img <- image_read("plot4.png")
background_img <- image_read("https://images.pexels.com/photos/8947145/pexels-photo-8947145.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2")


plot_img <- image_resize(plot_img, "800x600")
background_img <- image_resize(background_img, "800x600")
combined_img <- image_composite(background_img, plot_img, offset = "+0+0")


print(combined_img)
image_write(combined_img, "creative_plot4.png")