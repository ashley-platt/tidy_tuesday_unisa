tidy tuesday 2-3-22
================

*This dataset is about chocolate bars and their ratings.*

``` r
choc <- read_csv("flavors_of_cacao.csv") %>% 
  clean_names()
```

    ## Rows: 1795 Columns: 9

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (6): Company 
    ## (Maker-if known), Specific Bean Origin
    ## or Bar Name, Cocoa
    ## ...
    ## dbl (3): REF, Review
    ## Date, Rating

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Looking at: Company with Ratings

``` r
choc %>% 
  group_by(company_maker_if_known) %>%
arrange(company_maker_if_known, -rating) %>% 
  select(company_maker_if_known, broad_bean_origin, rating) %>%
  distinct(company_maker_if_known, .keep_all = TRUE) %>%
  filter(company_maker_if_known == "A. Morin"| company_maker_if_known == "Acalli"| company_maker_if_known == "Adi"| company_maker_if_known == "Aequare (Gianduja)"| company_maker_if_known == "Ah Cacao"| company_maker_if_known == "Akesson's (Pralus)"| company_maker_if_known == "Alain Ducasse"| company_maker_if_known == "Alexandre"| company_maker_if_known == "Altus aka Cao Artisan"| company_maker_if_known == "Amano") %>% 
  ggplot(aes(x = rating, y = reorder(company_maker_if_known, rating), fill = company_maker_if_known)) + geom_col() + theme_light() + labs(x = "Rating (out of 5)", y = "Company", title = "Chocolate Company by Chocolate Ratings", subtitle = "Does certain companies make better chocolate than others?") + guides(fill=FALSE) + scale_fill_brewer(palette = "Set3")
```

    ## Warning: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> =
    ## "none")` instead.

![](2022-03-01_chocolate_CW_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# Looking at: Bean Type and Ratings

``` r
choc %>% 
  group_by(bean_type) %>% 
  summarise(mean_rating = mean(rating)) %>% 
  arrange(-mean_rating) %>% 
  na.omit() %>% 
  select(bean_type, mean_rating) %>% 
  ggplot(aes(x = mean_rating, y = reorder(bean_type, mean_rating), fill = bean_type)) + geom_col() + scale_fill_brewer(aesthetics = "colour") + guides(fill=FALSE) + labs(x = "Mean Rating", y = "Bean Type", title = "Bean Type Ratings", subtitle = "Is there a specific bean that makes better chocolate than others?")
```

    ## Warning: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> =
    ## "none")` instead.

![](2022-03-01_chocolate_CW_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
