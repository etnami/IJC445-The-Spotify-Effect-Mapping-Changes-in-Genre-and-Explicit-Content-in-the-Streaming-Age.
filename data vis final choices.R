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

#getting top 5 genres in the dataset based on highest amount of popular songs (using is_pop)
top_5_genres <- final_dataset %>%
  filter(is_pop == 1) %>% 
  group_by(main_genre) %>%
  summarise(pop_song_count = n()) %>%
  slice_max(pop_song_count, n = 5) %>%
  pull(main_genre)

#HEAT MAP OF EXPLICIT SHIFT IN POPULAR SONGS PRE AND POST SPOTIFY LAUNCH


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
    title = "How Has Interest in Explicit Content Changed\nOver Time",
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


#WAFFLE CHART COMPARISON OF POPULAR EXPLICIT SONGS PRE AND POST SPOTIFY LAUNCH

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


#SANKEY MAPPING TOP 5 GENRES BASED ON TOTAL AMOUNT OF POPULAR SONGS TO EXPLICITNESS AND POPULARITY 
#can be misleading example- based on story you want to tell how to organise nodes
install.packages("devtools")

#installing sankey
devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)


#preparing my data using my top_5_genres variable that fetches top 5 highest performing genres based on amount of popular songs
df_sankey <- final_dataset %>%
  filter(main_genre %in% top_5_genres) %>% 
  mutate(
    explicit = ifelse(explicit == TRUE, "Explicit", "Non-Explicit"),
    song_status = ifelse(is_pop == 1, "Popular Song", "Non-Popular Song")
  ) %>%
  # SWAP ORDER HERE: explicit first, then main_genre
  select(explicit, main_genre, song_status) %>%
  make_long(explicit, main_genre, song_status)

#making the plot, using cividis for consistency across my visualisations
ggplot(df_sankey, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node, 
                      fill = factor(node), 
                      label = node)) +
  geom_sankey(flow.alpha = 0.6, node.color = "black", node.size = 0.2) +
  geom_sankey_label(size = 3.5, color = "black", fill = "white", alpha = 0.9) +
  scale_fill_viridis_d(option = "cividis") +
  scale_x_discrete(labels = c("explicit" = "Content", 
                              "main_genre" = "Top Genre", 
                              "song_status" = "Outcome")) +
  
  theme_sankey(base_size = 14) +
  labs(
    title = "How Explicitness and Genre interact with Chart Popularity",
    subtitle = "Flowing from Content Type through Genre to Chart Success",
    x = NULL
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", color = "black", size = 12),
        plot.title = element_text(face = "bold"))


#STACKED BAR CHART SHOWING PROPORTION OF EXPLICIT VS CLEAN OF TOP 5 GENRES MEASURED BY TOTAL AMOUNT OF POPULAR SONGS

#fetching top 5 genres based on artist follower count (rather than amount of popular songs)
top_5_genres_artist <- final_dataset %>%
  group_by(main_genre) %>%
  # Sum the total followers for each genre
  summarise(total_followers = sum(follower_count, na.rm = TRUE)) %>%
  # Select the top 5 based on that sum
  slice_max(total_followers, n = 5) %>%
  # Extract just the names into a list (vector) for filtering later
  pull(main_genre)



explicit_5_data <- final_dataset %>%
  filter(main_genre %in% top_5_genres_artist) %>%
  mutate(explicit = ifelse(explicit == TRUE, "Explicit", "Non-Explicit"))


labeled_explicit5_data <- final_dataset %>%
  filter(main_genre %in% top_5_genres_artist) %>%
  mutate(explicit = ifelse(explicit == TRUE, "Explicit", "Non-Explicit")) %>%
  #grouping by genre and explicitness
  group_by(main_genre, explicit) %>%
  summarise(n = n(), .groups = "drop") %>%
  #calculating percentage of explicit popular songs in each genre
  group_by(main_genre) %>%
  mutate(percentage = n / sum(n))

#making the plot
ggplot(labeled_explicit5_data, aes(x = main_genre, y = percentage, fill = explicit)) +
  geom_col(position = "fill") +
  #adding labels inside the bars for easier readability
  geom_text(aes(label = percent(percentage, accuracy = 1)), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            fontface = "bold",
            size = 3.5) +
  scale_y_continuous(labels = percent) +
  scale_fill_viridis_d(option = "cividis", begin = 0.3, end = 0.7) +
  labs(title = "Proportion of Explicit vs. Non-Explicit Songs of Top Genres",
       subtitle = "top genre based on follower count of artist",
       x = "Main Genre",
       y = "Percentage of Songs",
       fill = "Content") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#BONUS GRAPH: COMPARING AMOUNT OF SONGS RELEASED WITH AMOUNT THAT BECAME POPULAR FOR THE TOP 5 MOST POPULAR GENRES

#getting top 5 genres based on popularity same as sankey
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
    title = "Genre Efficiency: Volume of Songs Released vs\nAmount Of Popular Songs",
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
