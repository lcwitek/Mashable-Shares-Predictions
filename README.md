Project 2
================
Lauren Witek
07/03/2020

``` r
news <- read_csv("C:\\Users\\Lauren\\Documents\\ST558\\Project 2\\OnlineNewsPopularity.csv")


news$channel <- ifelse(news$data_channel_is_bus == 1, "Business", 
                ifelse(news$data_channel_is_entertainment == 1, "Entertainment", 
                ifelse(news$data_channel_is_lifestyle == 1, "Lifestlye", 
                ifelse(news$data_channel_is_socmed == 1, "Social Media", 
                ifelse(news$data_channel_is_tech == 1, "Technology", 
                ifelse(news$data_channel_is_world == 1, "World", "Other"))))))

newsLess <- news %>% filter(weekday_is_monday == 1) %>%  select(shares, channel, n_tokens_title, n_tokens_content, num_imgs, num_videos, rate_negative_words, rate_positive_words)
```

``` r
train <- sample(1:nrow(newsLess), size = nrow(newsLess)*0.7)
test <- dplyr::setdiff(1:nrow(newsLess), train)

newsTrain <- newsLess[train, ]
newsTest <- newsLess[test, ]
```

    ## 
    ## Call:
    ## lm(formula = shares ~ ., data = newsLess)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -8194  -2499  -1614   -577 685869 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           3.743e+03  1.511e+03   2.478 0.013243 *  
    ## channelEntertainment -1.259e+03  6.078e+02  -2.072 0.038286 *  
    ## channelLifestlye      4.249e+02  9.243e+02   0.460 0.645724    
    ## channelOther          2.682e+03  7.066e+02   3.796 0.000148 ***
    ## channelSocial Media   8.532e+01  9.094e+02   0.094 0.925259    
    ## channelTechnology    -1.147e+03  6.048e+02  -1.896 0.057989 .  
    ## channelWorld         -1.485e+03  5.977e+02  -2.484 0.013020 *  
    ## n_tokens_title        6.627e+01  8.739e+01   0.758 0.448268    
    ## n_tokens_content     -6.327e-02  4.405e-01  -0.144 0.885787    
    ## num_imgs              2.773e+01  2.471e+01   1.122 0.261834    
    ## num_videos            6.357e+01  4.084e+01   1.556 0.119669    
    ## rate_negative_words  -4.500e+02  1.397e+03  -0.322 0.747367    
    ## rate_positive_words  -6.547e+02  1.191e+03  -0.550 0.582422    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 14630 on 6648 degrees of freedom
    ## Multiple R-squared:  0.01011,    Adjusted R-squared:  0.008325 
    ## F-statistic: 5.659 on 12 and 6648 DF,  p-value: 8.93e-10
