2022-02-08\_board-games\_HC
================
Hayley Caldwell
27/01/2022

The data this week comes from the ‘Board Games Geek’ website which
catalogs information about thousands of board games.

Firstly, I wanted to look at the player number ranges for board games,
but as there were thousands, I just decided to look at the top 20 most
owned board games.

``` r
bg_most_pop <- bgames %>% 
  slice_max(owned, n = 20)

light_die = 'light_die.png'
dark_die = 'dark_die.png'

bg_no.player_plot <- ggplot(bg_most_pop, aes(y = reorder(name, owned))) + 
  geom_dumbbell(aes(x = minplayers, xend = maxplayers, colour = name), size = 1.5, show.legend = FALSE) +
  geom_image(mapping = aes(x = minplayers, image = light_die), size = 0.04) +
  geom_image(mapping = aes(x = maxplayers, image = dark_die), size = 0.04) +
  scale_colour_manual(values = bg_colour) +
  labs(y = "Game",
       x = "No. of Players",
       title = "Player No. Ranges for the Most Owned Games",
         caption =  "White =  Min Players. Black = Max Players") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(), 
    plot.background = element_rect(fill = "#EEEEEE")) 
bg_no.player_plot
```

![](2022-02-08_board-games_HC_files/figure-gfm/min%20max%20players%20of%20most%20owned%20games-1.png)<!-- -->

Generally, the games that sell the best seem to cluster around having
2-6 players, which is a very typical amount for most board games. There
are some board games not represented here that allow for more people
than this, but they do not seem to be sold as much as ones with less
players.

Next I wanted to look at these same games’ play time ranges.

``` r
bg_time_plot <- ggplot(bg_most_pop, aes(y = reorder(name, owned))) + 
  geom_dumbbell(aes(x = minplaytime, xend = maxplaytime, colour = name), size = 1.5, show.legend = FALSE) +
  geom_image(mapping = aes(x = minplaytime, image = light_die), size = 0.04) +
  geom_image(mapping = aes(x = maxplaytime, image = dark_die), size = 0.04) +
  scale_colour_manual(values = bg_colour) +
  labs(y = "Game",
       x = "Playtime (min)",
       title = "Playtime Ranges for the Most Owned Games",
         caption =  "White =  Min Playtime. Black = Max Playtime") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(), 
    plot.background = element_rect(fill = "#EEEEEE")) 
bg_time_plot
```

![](2022-02-08_board-games_HC_files/figure-gfm/min%20max%20and%20average%20play%20time%20of%20most%20popular%20games-1.png)<!-- -->

Generally there seems to be a trend toward more popular games having
shorter play times, but it looks like the top games still have a variety
of play time averages and ranges. Some games have the same min and max
play time with no suggested range, and some games do not overlap in
their play times with each other.

Then I decided to look at the sales and ratings of the highest 20 and
lowest 20 selling publishers to see if there were differences in the
extremes. Each game had many publishers listed, so I decided to just
extract the first listed publisher.

``` r
bg_pop_pub <- bgames %>%
  mutate(publisher_all = {str_replace_all(boardgamepublisher, pattern = "\\[|\\]", replacement = "")}) %>%  
  separate(publisher_all, into = c("publisher"), sep = ",") %>% 
  mutate(publisher = gsub("[[:punct:]]", "", publisher)) %>% 
  group_by(publisher) %>% 
  mutate(pubtotalowned = sum(owned)) %>% 
  mutate(pubmeanrating = mean(average)) %>% 
  ungroup() %>% 
  select(publisher, pubmeanrating, pubtotalowned) %>% 
  distinct() %>% 
  rename(rating = pubmeanrating)
```

    ## Warning: Expected 1 pieces. Additional pieces discarded in 10780 rows [1, 2, 3,
    ## 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
# top

bg_pop_pub_top <- bg_pop_pub %>% 
  slice_max(pubtotalowned, n = 20)

pp_top_plot <- ggplot(bg_pop_pub_top, aes(x = reorder(publisher, pubtotalowned), y = pubtotalowned, fill = rating)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#FF5100", high = "#3f84e5", 
                      limits = c(1,10)) +
  labs(x = "Publisher",
       y = "Total Games Owned by Consumers",
       title = "Total Games Sold and Ratings of Publishers", 
       caption = "(Top 20)") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(), 
    plot.background = element_rect(fill = "#EEEEEE"), 
    legend.position = "bottom")

# bottom

bg_pop_pub_bot <- bg_pop_pub %>% 
  slice_min(pubtotalowned, n = 20)

pp_bot_plot <- ggplot(bg_pop_pub_bot, aes(x = reorder(publisher, pubtotalowned), y = pubtotalowned, fill = rating)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#FF5100", high = "#3f84e5", 
                      limits = c(1,10)) + 
  labs(x = "Publisher",
       y = "Total Games Owned by Consumers",
       title = "Total Games Sold and Ratings of Publishers", 
       caption = "(Bottom 20)") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(), 
    plot.background = element_rect(fill = "#EEEEEE"), 
    legend.position = "bottom")

ggarrange(pp_bot_plot, pp_top_plot)
```

![](2022-02-08_board-games_HC_files/figure-gfm/top%20and%20bottom%20publisher%20graph-1.png)<!-- -->

We can see here that amongst the lowest selling publishers, there is
more variety in the ratings that players give, likely because of the
fewer ratings given to less sold games. However, amongst the top sold
publishers, there are very similar, mid-high ratings.

Then I wanted to look at the sales and ratings of the highest and lowest
selling game mechanics.

``` r
bg_pop_mec <- bgames %>%
  mutate(mechanics_all = {str_replace_all(boardgamemechanic, pattern = "\\[|\\]", replacement = "")}) %>%  
  separate(mechanics_all, into = c("mechanic"), sep = ",") %>% 
  mutate(mechanic = gsub("[[:punct:]]", "", mechanic)) %>% 
  group_by(mechanic) %>% 
  mutate(mectotalowned = sum(owned)) %>% 
  mutate(mecmeanrating = mean(average)) %>% 
  ungroup() %>% 
  select(mechanic, mecmeanrating, mectotalowned) %>% 
  distinct() %>% 
  rename(rating = mecmeanrating)
```

    ## Warning: Expected 1 pieces. Additional pieces discarded in 15565 rows [1, 2, 3,
    ## 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
# top

bg_pop_mec_top <- bg_pop_mec %>% 
  slice_max(mectotalowned, n = 20)

mec_top_plot <- ggplot(bg_pop_mec_top, aes(x = reorder(mechanic, mectotalowned), y = mectotalowned, fill = rating)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#FF5100", high = "#3f84e5", 
                      limits = c(1,10)) + 
  labs(x = "Mechanic",
       y = "Total Games Owned per Mechanic",
       title = "Total Games Sold and Ratings of Mechanics", 
       caption = "(Top 20)") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(), 
    plot.background = element_rect(fill = "#EEEEEE"), 
    legend.position = "bottom")

# bottom

bg_pop_mec_bot <- bg_pop_mec %>% 
  slice_min(mectotalowned, n = 20)

mec_bot_plot <- ggplot(bg_pop_mec_bot, aes(x = reorder(mechanic, mectotalowned), y = mectotalowned, fill = rating)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#FF5100", high = "#3f84e5", 
                      limits = c(1,10)) + 
  labs(x = "Mechanic",
       y = "Total Games Owned per Mechanic",
       title = "Total Games Sold and Ratings of Mechanics", 
       caption = "(Bottom 20)") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(), 
    plot.background = element_rect(fill = "#EEEEEE"), 
    legend.position = "bottom")

ggarrange(mec_bot_plot, mec_top_plot)
```

![](2022-02-08_board-games_HC_files/figure-gfm/top%20and%20bottom%20mechanics%20graph-1.png)<!-- -->

You can see here that regardless of whether these mechanics are popular
or not, their ratings are very similarly clustered around the 6-8 range.
These consisten high ratings could be because the ratings are from those
who are purchasing/playing the games, who presumably already like games
with these mechanics.

Then, I decided to do the same this, but with game categories instead.

``` r
bg_pop_cat <- bgames %>%
  mutate(category_all = {str_replace_all(boardgamecategory, pattern = "\\[|\\]", replacement = "")}) %>%  
  separate(category_all, into = c("category"), sep = ",") %>% 
  mutate(category = gsub("[[:punct:]]", "", category)) %>% 
  group_by(category) %>% 
  mutate(cattotalowned = sum(owned)) %>% 
  mutate(catmeanrating = mean(average)) %>% 
  ungroup() %>% 
  select(category, catmeanrating, cattotalowned) %>% 
  distinct() %>% 
  rename(rating = catmeanrating)
```

    ## Warning: Expected 1 pieces. Additional pieces discarded in 17304 rows [2, 3, 4,
    ## 5, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 19, 20, 21, 22, 23, ...].

``` r
# top

bg_pop_cat_top <- bg_pop_cat %>% 
  slice_max(cattotalowned, n = 20)

cat_top_plot <- ggplot(bg_pop_cat_top, aes(x = reorder(category, cattotalowned), y = cattotalowned, fill = rating)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#FF5100", high = "#3f84e5", 
                      limits = c(1,10)) + 
  labs(x = "Category",
       y = "Total Games Owned per Category",
       title = "Total Games Sold and Ratings of Categories", 
       caption = "(Top 20)") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(), 
    plot.background = element_rect(fill = "#EEEEEE"), 
    legend.position = "bottom")

# bottom

bg_pop_cat_bot <- bg_pop_cat %>% 
  slice_min(cattotalowned, n = 20)

cat_bot_plot <- ggplot(bg_pop_cat_bot, aes(x = reorder(category, cattotalowned), y = cattotalowned, fill = rating)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#FF5100", high = "#3f84e5", 
                      limits = c(1,10)) + 
  labs(x = "Category",
       y = "Total Games Owned per Category",
       title = "Total Games Sold and Ratings of Categories", 
       caption = "(Bottom 20)") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(), 
    plot.background = element_rect(fill = "#EEEEEE"), 
    legend.position = "bottom")

ggarrange(cat_bot_plot, cat_top_plot)
```

![](2022-02-08_board-games_HC_files/figure-gfm/top%20and%20bottom%20category%20graph-1.png)<!-- -->

This is very similar to what we found with the top and bottom mechanics
in that there are consistent high ratings across both popularity
extremes.

After noticing the number of different attributes this dataset has for
each board game, I wanted to make something that would be able to let me
input what parameters I wanted out of a game, and give me the top rated
game that fits those. So, I decided to make a shiny app that does
exactly that. See my other file for this.
