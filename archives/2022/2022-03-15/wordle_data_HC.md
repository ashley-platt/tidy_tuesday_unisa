Wordle
================

Don’t run this script, unless you want to update the data.

``` r
knitr::opts_chunk$set(echo = TRUE)

pacman::p_load(install = FALSE, update = FALSE, tidyverse, tidytuesdayR, rvest)

# source: https://www.kaggle.com/benhamner/wordle-1-6
tweets <- read.csv("D:\\Tidy_Tuesday\\Wordle\\2022-03-15_wordle_HC\\tweets.csv")
```

``` r
tweets_clean <- tweets %>% 
  separate(tweet_text, into = c("attempts"), sep = '/') %>% 
  mutate(attempts = str_sub(attempts, -1))
```

    ## Warning: Expected 1 pieces. Additional pieces discarded in 329912 rows [1, 2, 3,
    ## 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
# read from website
answers1 <- read_html("https://progameguides.com/wordle/all-wordle-answers-in-2022-updated-daily/")

# extract dot points
answers <- answers1 %>% 
  html_nodes("ul") %>% 
  html_text()

# january answers 
  # select january list
answers_jan2 <- answers[5]
  # split list
answers_jan1 <- answers_jan2 %>% 
  strsplit(split = "01/")
  # dataframe
answers_jan <- as.data.frame(answers_jan1, col.names = c("all")) %>% 
  mutate(month = "01") 

# february
  #select february list 
answers_feb2 <- answers[4]
  # split list
answers_feb1 <- answers_feb2 %>% 
  strsplit(split = "02/")
  # dataframe
answers_feb <- as.data.frame(answers_feb1, col.names = c("all")) %>% 
  mutate(month = "02")

# march
  #select march list 
answers_mar2 <- answers[3]
  # split list
answers_mar1 <- answers_mar2 %>% 
  strsplit(split = "03/")
  # dataframe
answers_mar <- as.data.frame(answers_mar1, col.names = c("all")) %>% 
  mutate(month = "03")

# bind together
answers_all1 <- rbind(answers_jan, answers_feb)
answers_all <- rbind(answers_all1, answers_mar) %>% 
  separate(all, into = c("day", "rest"), sep = " - ") %>% 
  na.omit() %>% 
  separate(rest, into = c("wordle_id", "answer"), sep = ": ") %>% 
  separate(wordle_id, into = c("wordle_id"), sep = ":") 
```

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 3 rows [1, 33,
    ## 62].

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 1 rows [20].

    ## Warning: Expected 1 pieces. Additional pieces discarded in 1 rows [20].

``` r
  # remove hash
answers_all$wordle_id <- substring(answers_all$wordle_id, 2)
  # combine dates 
answers_all$date <- paste(answers_all$month, "-", answers_all$day)
  # remove space in date
answers_all <- answers_all %>% 
  mutate(date = gsub(" ", "", date)) %>% 
  select(wordle_id, date, answer)
  # manually fix weird week
answers_all[answers_all$date == "01-12", "answer"] <- "FAVOR"

# integer
answers_all$wordle_id <- as.integer(answers_all$wordle_id)
```

``` r
# combine with tweets 
wordle_tweets <- left_join(tweets_clean, answers_all)
```

    ## Joining, by = "wordle_id"

``` r
# manually add recent days not on website 
wordle_tweets[wordle_tweets$wordle_id == "265", "date"] <- "03-11"
wordle_tweets[wordle_tweets$wordle_id == "266", "date"] <- "03-12"
wordle_tweets[wordle_tweets$wordle_id == "265", "answer"] <- "WATCH"
wordle_tweets[wordle_tweets$wordle_id == "266", "answer"] <- "TODAY"

# numeric 
wordle_tweets$attempts <- as.numeric(wordle_tweets$attempts)
```

``` r
# split words into letters 

# function to assign letter frequency values 
  # data from http://pi.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html
letter_frequency <- function(base_column) {
  new_column <- case_when(
    base_column == "E" ~ 12.02, 
    base_column == "T" ~ 9.10, 
    base_column == "A" ~ 8.12,
    base_column == "O" ~ 7.68, 
    base_column == "I" ~ 7.31,
    base_column == "N" ~ 6.95, 
    base_column == "S" ~ 6.28, 
    base_column == "R" ~ 6.02, 
    base_column == "H" ~ 5.92,
    base_column == "D" ~ 4.32,
    base_column == "L" ~ 3.98,
    base_column == "U" ~ 2.88,
    base_column == "C" ~ 2.71, 
    base_column == "M" ~ 2.61, 
    base_column == "F" ~ 2.30, 
    base_column == "Y" ~ 2.11, 
    base_column == "W" ~ 2.09, 
    base_column == "G" ~ 2.03,
    base_column == "P" ~ 1.82, 
    base_column == "B" ~ 1.49, 
    base_column == "V" ~ 1.11,
    base_column == "K" ~ 0.69,
    base_column == "X" ~ 0.17, 
    base_column == "Q" ~ 0.11, 
    base_column == "J" ~ 0.10,
    base_column == "Z" ~ 0.07)
  return(new_column)
  }

wordle_all <- wordle_tweets %>% 
  mutate(letters = answer) %>% 
  separate(letters, into = c("L0", "L1", "L2", "L3", "L4", "L5"), sep = "") %>% 
  select(-L0) %>% 
  mutate(L1F = letter_frequency(L1), 
         L2F = letter_frequency(L2),
         L3F = letter_frequency(L3),
         L4F = letter_frequency(L4),
         L5F = letter_frequency(L5),
         letter_freq_total = L1F + L2F + L3F + L4F + L5F) %>% 
  group_by(wordle_id) %>% 
  mutate(mean_attempts = mean(attempts), # mean score per word
         total_attempts = n()) %>% # total attempts per word
  ungroup() %>% 
  group_by(wordle_id, attempts) %>% 
  mutate(attempt_count = n(), # no. of people who got each score
         perc_attempts = 
           round(attempt_count/total_attempts*100,2)) %>% # % of people w/ ea score
  ungroup()

# simplify to group data 
wordle_words <- wordle_all %>% 
  select(-tweet_id, -tweet_date, -tweet_username) %>% 
  distinct() %>% 
  arrange(wordle_id, attempts)
```

``` r
# big dataset with individual tweet data and group data 
write.csv(wordle_all, "D:\\Tidy_Tuesday\\Wordle\\2022-03-15_wordle_HC\\wordle_all.csv", row.names = FALSE)

# simplified dataset with only group data, not individual 
write.csv(wordle_words, "D:\\Tidy_Tuesday\\Wordle\\2022-03-15_wordle_HC\\wordle_words.csv", row.names = FALSE)

# dataset with only individual data, not group
write.csv(wordle_tweets, "D:\\Tidy_Tuesday\\Wordle\\2022-03-15_wordle_HC\\wordle_tweets.csv", row.names = FALSE)
```
