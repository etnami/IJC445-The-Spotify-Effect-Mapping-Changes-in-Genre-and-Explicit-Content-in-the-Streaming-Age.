library(tidyverse)

songs_cleaned <- read_csv("songs cleaned converted copy.csv", col_names = TRUE)
songs_cleaned

artists_cleaned <- read_csv("artists for vis cleaned.csv", col_names = TRUE)
artists_cleaned

songs_artists <- songs_cleaned %>%
  left_join(artists_cleaned, by = "artist_id")
songs_artists

musical_features <- read_csv("acoustic features cleaned converted copy.csv", col_names = TRUE)
musical_features

songs_artists_musicalfeatures <- songs_artists %>%
  left_join(musical_features, by = "song_id")
songs_artists_musicalfeatures

song_ispop <- read_csv("song ispop cleaned converted copy.csv", col_names = TRUE)
song_ispop

songs_artists_musicalfeatures_pop <- songs_artists_musicalfeatures %>%
  left_join(song_ispop, by = "song_id")
songs_artists_musicalfeatures_pop

combined_dataset <- songs_artists_musicalfeatures_pop

write.table(combined_dataset, "combined music dataset for vis.csv", sep= ",", row.names = FALSE)


colSums(is.na(combined_dataset))
sum(is.na(combined_dataset))



drop_na(combined_dataset)

combined_dataset <- combined_dataset %>% drop_na()


View(combined_dataset)

cleaned_dataset <- combined_dataset %>%
  select(-name)

View(cleaned_dataset)

final_dataset <- cleaned_dataset %>%
  filter(year >= 2000 & year <= 2018)

View(final_dataset)

library(ggplot2)
library(dplyr)


#CHOSEN VISUALISATIONS----------------------------------------------------------
#how did spotify's launch change how people interacted with music and what they made popular

#-------------------------------------------------------------------------------
#FIGURE 1: WAFFLE CHART COMPARISON OF POPULAR EXPLICIT SONGS PRE AND POST SPOTIFY LAUNCH

#fetching amount of popular songs that had explicit content from 2000-2018
waffle_stats <- final_dataset %>%
  filter(is_pop == 1, year >= 2000, year <= 2018) %>%
  mutate(period = factor(ifelse(year < 2008, "Pre-Spotify (2000-2007)", "Post-Spotify Launch (2008-2018)"),
                         levels = c("Pre-Spotify (2000-2007)", "Post-Spotify Launch (2008-2018)"))) %>%
  group_by(period, explicit) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(period) %>%
  mutate(
    percentage = round(n / sum(n) * 100),
    category = ifelse(explicit == TRUE, "Explicit", "Non-Explicit")
  )

#choosing to add labels to the plots to make it easier for viewer to see exact percentages of explicit popular music
labels_data <- waffle_stats %>%
  filter(category == "Explicit") %>%
  mutate(label_text = paste0(percentage, "% Explicit"))

#choosing the size and proportions of the waffle grids
periods <- levels(waffle_stats$period)
full_plot_data <- data.frame()

for (p in periods) {
  current_period_data <- waffle_stats %>% filter(period == p)
  exp_pct <- current_period_data$percentage[current_period_data$category == "Explicit"]
  cln_pct <- 100 - exp_pct
  cat_vec <- c(rep("Explicit", exp_pct), rep("Non-Explicit", cln_pct))
  
  grid_p <- expand.grid(y = 1:10, x = 1:10) %>%
    mutate(category = cat_vec, period = p)
  
  full_plot_data <- rbind(full_plot_data, grid_p)
}

full_plot_data$category <- factor(full_plot_data$category, levels = c("Explicit", "Non-Explicit"))
full_plot_data$period <- factor(full_plot_data$period, levels = periods)

#making the plot- putting two side-by-side for easier comparison
ggplot(full_plot_data, aes(x = x, y = y, fill = category)) +
  geom_tile(color = "white", linewidth = 0.8) +
  
  #using cividis for colour-blind accessibility and to keep the colour scale consistent across visualisations
  scale_fill_viridis_d(option = "cividis", name = "Content Type", direction = -1) +
  
  geom_label(data = labels_data, aes(x = 5.5, y = 5.5, label = label_text), 
             inherit.aes = FALSE, 
             fill = alpha("white", 0.85),
             color = "black",
             size = 6, 
             fontface = "bold",
             label.padding = unit(0.5, "lines")) +
  
  coord_fixed() +
  facet_wrap(~period) + 
  
  labs(
    title = "Comparing Total of Explicit Popular Songs Across Two Eras",
    subtitle = "Proportion of explicit content in popular songs pre-streaming and post-streaming",
    caption = "Each grid = 100% of popular tracks, 1 square = 1%"
  ) +
  theme_void() +
  theme(
    strip.text = element_text(size = 12, face = "bold", margin = margin(b = 10)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.margin = margin(20, 20, 20, 20)
  )


#-------------------------------------------------------------------------------------------------

#FIGURE 1.5: GROUPED BAR COMPARING AMOUNT OF TOTAL RELEASES, TOTAL POPULAR, EXPLICIT POPULAR, AND NON-EXPLICIT POPULAR

groupbar_data <- final_dataset %>%
  mutate(era = ifelse(year <= 2007, "Pre-Spotify (2000-2007)", "Post-Spotify (2008-2018)")) %>%
  mutate(era = factor(era, levels = c("Pre-Spotify (2000-2007)", "Post-Spotify (2008-2018)"))) %>%
  group_by(era) %>%
  summarise(
    "Total Released" = n(),
    "Popular Explicit" = sum(explicit == 1 & is_pop == 1, na.rm = TRUE),
    "Non-popular Explicit" = sum(explicit == 1 & is_pop == 0, na.rm = TRUE),
    "Total Popular" = sum(is_pop == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  pivot_longer(cols = -era, names_to = "metric", values_to = "count") %>%
  
  mutate(metric = factor(metric, levels = c("Total Released", "Total Popular", 
                                            "Popular Explicit", "Non-popular Explicit")))


ggplot(groupbar_data, aes(x = era, y = count, fill = metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, 
            fontface = "bold",
            size = 3.5) +
  scale_fill_manual(values = c("Total Released" = "grey70", 
                               "Total Popular" = "#e6cc7e", 
                               "Popular Explicit" = "#b59e3d", 
                               "Non-popular Explicit" = "#00204d"), 
                    name = "Information Type") +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(title = "Detailed Look at Volume, Explicitness, and Popularity by Era",
       subtitle = "Comparing total musical output against explicit and popular subsets",
       y = "Number of Songs", 
       x = "Era") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold", size = 10, color = "black")
  )


#-------------------------------------------------------------------------------

#FIGURE 2: HEAT MAP OF EXPLICIT SHIFT IN POPULAR SONGS PRE AND POST SPOTIFY LAUNCH

#getting top 5 genres in the dataset based on highest amount of popular songs (using is_pop)
top_5_genres <- final_dataset %>%
  filter(is_pop == 1) %>% 
  group_by(main_genre) %>%
  summarise(pop_song_count = n()) %>%
  slice_max(pop_song_count, n = 5) %>%
  pull(main_genre)

#setting my bins for the years i want to look at/ how i want to divide data on the plot
heatmap_custom <- final_dataset %>%
  filter(is_pop == 1, 
         main_genre %in% top_5_genres,
         year >= 2000, year <= 2018) %>%
  mutate(year_bin = case_when(
    year >= 2000 & year <= 2003 ~ "2000-2003",
    year >= 2004 & year <= 2007 ~ "2004-2007",
    year >= 2008 & year <= 2011 ~ "2008-2011",
    year >= 2012 & year <= 2015 ~ "2012-2015",
    year >= 2016 & year <= 2018 ~ "2016-2018"
  )) %>%
  mutate(year_bin = factor(year_bin, levels = c("2000-2003", "2004-2007", "2008-2011", "2012-2015", "2016-2018"))) %>%
  group_by(year_bin, main_genre) %>%
  summarise(
    pct_explicit = mean(explicit == TRUE, na.rm = TRUE) * 100,
    .groups = "drop"
  )

#plotting the heatmap showing explicit content in popular songs over time 
ggplot(heatmap_custom, aes(x = year_bin, y = reorder(main_genre, -pct_explicit), fill = pct_explicit)) +
  geom_tile(color = "white", linewidth = 0.5) +
  
  #using colourblind-friendly scale
  scale_fill_viridis_c(option = "cividis", 
                       name = "% Explicit", 
                       labels = function(x) paste0(x, "%")) +
  
  #adding a dashed line to show spotify's inception so it is easier to compare pre and post spotify dynamics
  #using the hexcode for spotify's logo for the line to make it relevant and stand out
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "#1db954", linewidth = 1.2) +
  
  #adding label to the line to tell the viewer what it means
  annotate("text", x = 2.6, y = 5.4, label = "Spotify Launch (2008)", 
           color = "white", angle = 0, fontface = "bold", hjust = 0, size = 3.5) +
  
  #adding percentage labels on the boxes to make it more readable and accessible- white for the dark colours and black for the light colours
  geom_text(aes(label = paste0(round(pct_explicit), "%"),
                color = ifelse(pct_explicit < 45, "white", "black")), 
            fontface = "bold", size = 4.5) +
  scale_color_identity() + 
  #adding labels to the plot
  labs(
    title = "How Has Interest in Explicit Content Changed Over Time",
    subtitle = "Showing how proportions of popular songs involved explicit content",
    x = "Time Period",
    y = "Most Popular Overall Genres",
    caption = "Dashed line indicates transition into the streaming age"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )


#-------------------------------------------------------------------------------

#FIGURE 2.5:WAFFLE CHART COMPARISON FOR ATL HIP HOP- how many songs were released in 2018 vs how many were popular

#filtering for 2018 and atl hiphop
explicit_atlhiphop_2018 <- final_dataset %>%
  filter(year == 2018, main_genre == "atl hip hop") %>%
  summarise(
    explicit_pct = round(mean(explicit == TRUE) * 100),
    non_explicit_pct = 100 - explicit_pct
  )

waffle_explicit_df <- data.frame(
  x = rep(1:10, each = 10),
  y = rep(1:10, times = 10),
  status = c(rep("Explicit", explicit_atlhiphop_2018$explicit_pct), 
             rep("Non-explicit", explicit_atlhiphop_2018$non_explicit_pct))
)

explicit_label <- paste0(explicit_atlhiphop_2018$explicit_pct, "% of songs were explicit")

ggplot(waffle_explicit_df, aes(x = x, y = y, fill = status)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis_d(option = "cividis", direction = -1) + 
  coord_fixed() +
  theme_void() + 
  labs(
    title = "2018 Atl Hip Hop: Explicitness by Volume Ratio",
    subtitle = "Each square represents 1% of total songs released",
    fill = "Content Type"
  ) +
  annotate("text", x = 5.5, y = 11, label = explicit_label, 
           size = 5, fontface = "bold", color = "#00204D") + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    #forcing the background to be white
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


#-------------------------------------------------------------------------------

#Figure 3.5: STACKED BAR COMPARING AMOUNT OF SONGS RELEASED WITH AMOUNT THAT BECAME POPULAR FOR THE TOP 5 MOST POPULAR GENRES

#getting top 5 genres based on popularity
top_5_vec <- final_dataset %>%
  filter(is_pop == 1) %>% 
  group_by(main_genre) %>%
  summarise(pop_song_count = n()) %>%
  slice_max(pop_song_count, n = 5) %>%
  pull(main_genre)

#counting both popular and non-popular songs so i can compare volume vs success
bar_data <- final_dataset %>%
  filter(main_genre %in% top_5_vec) %>%
  mutate(status = ifelse(is_pop == 1, "Popular (Hit)", "Non-Popular")) %>%
  group_by(main_genre, status) %>%
  summarise(count = n(), .groups = "drop")

#preparing data by making sure popular comes first in the legend and is in yellow
bar_data_labeled <- final_dataset %>%
  filter(main_genre %in% top_5_genres) %>%
  mutate(status = factor(ifelse(is_pop == 1, "Popular Song", "Non-Popular Song"),
                         levels = c("Popular Song", "Non-Popular Song"))) %>% 
  group_by(main_genre, status) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(main_genre) %>%
  mutate(pct = count / sum(count)) %>%
  ungroup()

#making the plot
ggplot(bar_data_labeled, aes(x = reorder(main_genre, -count, sum), y = count, fill = status)) +
  geom_bar(stat = "identity", position = "fill", color = "white", linewidth = 0.5) +
  
  #making sure the text labels for easier readability contrast the background of yellow vs navy 
  geom_text(aes(label = paste0(round(pct * 100), "%"),
                group = status, 
                color = ifelse(status == "Popular Song", "black", "white")), 
            position = position_fill(vjust = 0.5), 
            fontface = "bold", 
            size = 4.5) +
  scale_fill_viridis_d(option = "cividis", name = "Song Status", direction = -1) +
  scale_color_identity() + 
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Genre Efficiency: Volume of Songs Released vs Amount Of Popular Songs",
    subtitle = "Popular songs (Yellow) compared to non-popular releases (Navy) for highest performing genres",
    x = "Top 5 Genres",
    y = "Percentage of Total Genre Output"
  ) +
  theme_minimal() +
  #rotating the genre labels so they are more clear
  theme(
    axis.text.x = element_text(face = "bold", size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    legend.text = element_text(size = 11, face = "bold")
  )



#------------------------------------------------------------------------------------------------


#FIGURE 4: LINE GRAPH COMPARING NUMBER OF POPULAR EXPLICIT AND NON-POPULAR EXPLICIT WITH SPOTIFY LAUNCH IN 2008 MARKED

line_data_volume <- final_dataset %>%
  group_by(year, is_pop) %>%
  summarise(explicit_count = sum(explicit == 1, na.rm = TRUE), .groups = "drop")

ggplot(line_data_volume, aes(x = year, y = explicit_count, color = as.factor(is_pop), label = explicit_count)) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "#19a34a", alpha = 0.8) +
  
  annotate("text", x = 2008.5, y = max(line_data_volume$explicit_count) * 0.95, 
           label = "Spotify Launch (2008)", hjust = 0, size = 3.5, color = "#19a34a", fontface = "bold") +
  
  geom_line(linewidth = 1.2) + 
  geom_point(size = 1.8) +
  
  geom_text(aes(vjust = ifelse(year == 2002 & is_pop == 1, 4.5, 
                               ifelse(is_pop == 0, -1.5, 2.5))), 
            size = 3.5, 
            fontface = "bold", 
            show.legend = FALSE) +
  
  scale_x_continuous(breaks = seq(2000, 2018, by = 3)) + 
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.15))) +
  
  scale_color_viridis_d(option = "cividis", 
                        begin = 0, 
                        end = 0.8,
                        labels = c("Not Popular", "Popular"),
                        name = "Outcome Status") +
  
  labs(title = "Comparison of Total Explicit Songs By Year Over Time",
       subtitle = "Total number of explicit songs that were popular vs not popular (2000-2018)",
       y = "Number of Explicit Songs", 
       x = "Year") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )


#making more accessible
line_data_volume <- final_dataset %>%
  group_by(year, is_pop) %>%
  summarise(explicit_count = sum(explicit == 1, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(pct = (explicit_count / sum(explicit_count)) * 100,
         combined_label = paste0(explicit_count, " (", round(pct, 0), "%)")) %>%
  ungroup()

ggplot(line_data_volume, aes(x = year, y = explicit_count, color = as.factor(is_pop))) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "#19a34a", alpha = 0.8) +
  
  annotate("text", x = 2008.5, y = max(line_data_volume$explicit_count) * 0.95, 
           label = "Spotify Launch (2008)", hjust = 0, size = 3.5, color = "#19a34a", fontface = "bold") +
  
  geom_line(linewidth = 1.2) + 
  geom_point(size = 1.8) +
  
  geom_text(aes(label = ifelse(year %in% c(2000, 2002, 2008, 2015, 2018), combined_label, "")),
            vjust = ifelse(line_data_volume$is_pop == 1, 3.5, -2.0), 
            size = 3.2, 
            fontface = "bold", 
            show.legend = FALSE) +
  
  scale_x_continuous(breaks = seq(2000, 2018, by = 3)) + 
  
  scale_y_continuous(breaks = seq(0, 210, by = 30), 
                     expand = expansion(mult = c(0.15, 0.25))) + 
  
  scale_color_viridis_d(option = "cividis", 
                        begin = 0, 
                        end = 0.8,
                        labels = c("Not Popular", "Popular"),
                        name = "Outcome Status") +
  
  labs(title = "Key Shifts in Explicit Song Volume (2000-2018)",
       subtitle = "Yellow labels pushed down | Navy labels pushed up | Y-axis increments of 30",
       y = "Number of Explicit Songs", 
       x = "Year") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )
