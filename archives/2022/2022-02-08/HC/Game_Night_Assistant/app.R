
# Set-up

pacman::p_load(install = FALSE, update = FALSE, tidyverse, tidytuesdayR, 
               plotly, ggimage, ggalt, ggpubr, shiny, shinythemes)

tt_data_bg <- tt_load("2022-01-25")

ratings <- tt_data_bg$ratings
details <- tt_data_bg$details

bgames <- left_join(ratings, details, by = "id")

bgames$url <- paste("https://boardgamegeek.com", bgames$url, sep = "")

bgames_app <- bgames %>% 
    mutate(category_all = {str_replace_all(boardgamecategory, pattern = "\\[|\\]", replacement = "")}) %>%  
    separate(category_all, into = c("category"), sep = ",") %>% 
    mutate(category = gsub("[[:punct:]]", "", category)) %>% 
    group_by(category) %>% 
    mutate(cattotalowned = sum(owned)) %>% 
    ungroup() %>% #very important!!!!
    arrange(-cattotalowned)

# Shiny App

ui <- fluidPage(theme = shinytheme("simplex"),
                titlePanel(h1("Game Night Assistant")),
                sidebarPanel(numericInput(inputId = "player_no",
                                          label = "How many players?", 
                                          min = 1, max = 999, value = 1), 
                             numericInput(inputId = "playtime", 
                                          label = "How long do you want to play? (minutes)", 
                                          value = 30),
                             sliderInput(inputId = "youngest", 
                                         label = "How old is the youngest player?", 
                                         min = 0, max = 25, value = 18), 
                             selectizeInput(inputId = "game_category", 
                                            label = "What category do you want?", 
                                            choices = bgames_app$category)), 
                mainPanel(
                    tabsetPanel(
                        tabPanel("Game",
                                 h4("You should play...", style = "color:#3F84E5"),
                                 h2(strong(textOutput(outputId = "game_name")), 
                                    align="center", style = "color:#FF5100"),
                                 htmlOutput(outputId = "picture"), 
                                 align="center"),
                        tabPanel("Description", 
                                 textOutput(outputId = "summary")),
                        tabPanel("Learn More",
                                 "To learn more, visit",
                                 textOutput(outputId = "page_link")))))

server <- function(input, output) {
    output$game_name <- renderText({
        bgames_app %>% 
            filter(minplayers <= input$player_no, 
                   maxplayers >= input$player_no, 
                   minplaytime <= input$playtime, 
                   maxplaytime >= input$playtime,
                   minage <= input$youngest, 
                   str_detect(boardgamecategory, input$game_category)) %>%
            slice_max(average) %>% 
            pull(primary)
    })
    output$picture <- renderText({
        suggestion <- bgames_app %>% 
            filter(minplayers <= input$player_no, 
                   maxplayers >= input$player_no, 
                   minplaytime <= input$playtime, 
                   maxplaytime >= input$playtime,
                   minage <= input$youngest, 
                   str_detect(boardgamecategory, input$game_category)) %>%
            slice_max(average) %>% 
            pull(thumbnail)
        c('<img src="', suggestion,'">')
    })
    output$summary <- renderText({
        bgames_app %>% 
            filter(minplayers <= input$player_no, 
                   maxplayers >= input$player_no, 
                   minplaytime <= input$playtime, 
                   maxplaytime >= input$playtime,
                   minage <= input$youngest, 
                   str_detect(boardgamecategory, input$game_category)) %>%
            slice_max(average) %>% 
            pull(description)
    })
    output$page_link <- renderText({
        bgames_app %>% 
            filter(minplayers <= input$player_no, 
                   maxplayers >= input$player_no, 
                   minplaytime <= input$playtime, 
                   maxplaytime >= input$playtime,
                   minage <= input$youngest, 
                   str_detect(boardgamecategory, input$game_category)) %>% 
            slice_max(average)%>% 
            pull(url)
    })
}

shinyApp(ui = ui, server = server)

