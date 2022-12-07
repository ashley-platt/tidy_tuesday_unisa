# Set-up

pacman::p_load(install = FALSE, update = FALSE, 
               tidytuesdayR, tidyverse, tidyEmoji, dplyr, ggimage, DT, scales, shiny, shinythemes)

tt_data_op <- tt_load("2022-08-16")

characters <- tt_data_op$characters
psych_stats <- tt_data_op$psych_stats
myers_briggs <- tt_data_op$myers_briggs

setwd("E:/Tidy_Tuesday")

psych_stats1 <- psych_stats %>% 
  filter(grepl("[A-Za-z]", question))

characters1 <- characters %>% 
  rename("char_name" = name) %>% 
  rename("char_id" = id) %>% 
  select(-uni_name, -char_id, -uni_id) %>% 
  group_by(char_name) %>% 
  slice_max(notability) %>%  # multiple entries per char for different shows 
  ungroup()

myers_briggs_top <- myers_briggs %>% 
  group_by(char_name) %>% 
  slice_max(avg_match_perc) %>%
  slice_max(number_users) %>% # multiple entries per char for different shows
  mutate(myers_briggs_total = myers_briggs) %>% 
  separate(myers_briggs, into = c("0", "Extrovert_Introvert", "Sensing_Intuiting", "Thinking_Feeling", "Judging_Perceiving"), sep = "") %>% 
  select(-"0") %>% 
  ungroup() %>% 
  distinct()

mb_shiny <- left_join(myers_briggs_top, characters1, by = "char_name") 




# Shiny App

ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel(h1("Which Characters Am I? - Myers-Briggs"),
                           h3("Enter you Myers Briggs Personality Code to Find Out!")),
                sidebarPanel(selectizeInput(inputId = "E_I",
                                          label = "Extrovert (E) or Introvert (I)?", 
                                          choices = mb_shiny$Extrovert_Introvert), 
                             selectizeInput(inputId = "S_N", 
                                          label = "Sensing (S) or Intuiting (N)?", 
                                          choices = mb_shiny$Sensing_Intuiting),
                             selectizeInput(inputId = "T_F", 
                                         label = "Thinking (T) or Feeling (F)?", 
                                         choices = mb_shiny$Thinking_Feeling), 
                             selectizeInput(inputId = "J_P", 
                                            label = "Judging (J) or Preceiving (P)?", 
                                            choices = mb_shiny$Judging_Perceiving)), 
                mainPanel(h4("You are like...", style = "color:#3F84E5"),
                             h2(strong(textOutput(outputId = "character1")), 
                                align="center", style = "color:#FF5100"),
                            h5("from", textOutput(outputId = "uni_name1"), align="center"),
                          h2(strong(textOutput(outputId = "character2")),
                             align="center", style = "color:#FF5100"),
                          h5("from", textOutput(outputId = "uni_name2"), align="center"),
                          h2(strong(textOutput(outputId = "character3")), 
                             align="center", style = "color:#FF5100"),
                          h5("from", textOutput(outputId = "uni_name3"), align="center"),
                          h2(strong(textOutput(outputId = "character4")), 
                             align="center", style = "color:#FF5100"),
                          h5("from", textOutput(outputId = "uni_name4"), align="center"),
                          h2(strong(textOutput(outputId = "character5")), 
                             align="center", style = "color:#FF5100"),
                          h5("from", textOutput(outputId = "uni_name5"), align="center")))

server <- function(input, output) {
  output$character1 <- renderText({
    mb_shiny %>% 
      filter(Extrovert_Introvert == input$E_I, 
             Sensing_Intuiting == input$S_N, 
             Thinking_Feeling == input$T_F, 
             Judging_Perceiving == input$J_P) %>% 
      arrange(-avg_match_perc) %>% 
      head(n = 1) %>% 
      pull(char_name)
  })
  output$uni_name1 <- renderText({
    mb_shiny %>% 
      filter(Extrovert_Introvert == input$E_I, 
             Sensing_Intuiting == input$S_N, 
             Thinking_Feeling == input$T_F, 
             Judging_Perceiving == input$J_P) %>% 
      arrange(-avg_match_perc) %>% 
      head(n = 1) %>% 
      pull(uni_name)
  })
  output$character2 <- renderText({
    mb_shiny %>% 
      filter(Extrovert_Introvert == input$E_I, 
             Sensing_Intuiting == input$S_N, 
             Thinking_Feeling == input$T_F, 
             Judging_Perceiving == input$J_P) %>% 
      arrange(-avg_match_perc) %>% 
      head(n = 2) %>% 
      tail(n = 1) %>% 
      pull(char_name)
  })
  output$uni_name2 <- renderText({
    mb_shiny %>% 
      filter(Extrovert_Introvert == input$E_I, 
             Sensing_Intuiting == input$S_N, 
             Thinking_Feeling == input$T_F, 
             Judging_Perceiving == input$J_P) %>% 
      arrange(-avg_match_perc) %>% 
      head(n = 2) %>% 
      tail(n = 1)%>%  
      pull(uni_name)
  })
  output$character3 <- renderText({
    mb_shiny %>% 
      filter(Extrovert_Introvert == input$E_I, 
             Sensing_Intuiting == input$S_N, 
             Thinking_Feeling == input$T_F, 
             Judging_Perceiving == input$J_P) %>% 
      arrange(-avg_match_perc) %>% 
      head(n = 3) %>% 
      tail(n = 1) %>% 
      pull(char_name)
  })
  output$uni_name3 <- renderText({
    mb_shiny %>% 
      filter(Extrovert_Introvert == input$E_I, 
             Sensing_Intuiting == input$S_N, 
             Thinking_Feeling == input$T_F, 
             Judging_Perceiving == input$J_P) %>% 
      arrange(-avg_match_perc) %>% 
      head(n = 3) %>% 
      tail(n = 1) %>% 
      pull(uni_name)
  })
  output$character4 <- renderText({
    mb_shiny %>% 
      filter(Extrovert_Introvert == input$E_I, 
             Sensing_Intuiting == input$S_N, 
             Thinking_Feeling == input$T_F, 
             Judging_Perceiving == input$J_P) %>% 
      arrange(-avg_match_perc) %>% 
      head(n = 4) %>% 
      tail(n = 1) %>% 
      pull(char_name)
  })
  output$uni_name4 <- renderText({
    mb_shiny %>% 
      filter(Extrovert_Introvert == input$E_I, 
             Sensing_Intuiting == input$S_N, 
             Thinking_Feeling == input$T_F, 
             Judging_Perceiving == input$J_P) %>% 
      arrange(-avg_match_perc) %>% 
      head(n = 4) %>% 
      tail(n = 1) %>%  
      pull(uni_name)
  })
  output$character5 <- renderText({
    mb_shiny %>% 
      filter(Extrovert_Introvert == input$E_I, 
             Sensing_Intuiting == input$S_N, 
             Thinking_Feeling == input$T_F, 
             Judging_Perceiving == input$J_P) %>% 
      arrange(-avg_match_perc) %>% 
      head(n = 5) %>% 
      tail(n = 1) %>% 
      pull(char_name)
  })
  output$uni_name5 <- renderText({
    mb_shiny %>% 
      filter(Extrovert_Introvert == input$E_I, 
             Sensing_Intuiting == input$S_N, 
             Thinking_Feeling == input$T_F, 
             Judging_Perceiving == input$J_P) %>% 
      arrange(-avg_match_perc) %>% 
      head(n = 5) %>% 
      tail(n = 1) %>% 
      pull(uni_name)
  })
}

shinyApp(ui = ui, server = server)
