Project 2
================
Lauren Witek
7/3/2020

``` r
weekday <- data.frame(days = c("weekday_is_monday", "weekday_is_tuesday", 
                                "weekday_is_wednesday", "weekday_is_thursday", 
                                "weekday_is_friday", "weekday_is_saturday", 
                                "weekday_is_sunday"))

days <- weekday$days

output_file <- paste0(days, ".md")

params <- lapply(days, FUN = function(x){list(days = x)})

reports <- tibble(output_file, params)

apply(reports, MARGIN = 1, FUN = function(x){
  render(input = "C:\\Users\\Lauren\\Documents\\ST558\\Project 2\\ST558_Project_2\\news.Rmd", output_file = x[[1]], params = x[[2]])
})
```


[Monday is Available Here](weekday_is_monday.md)

[Tuesday is Available Here](weekday_is_tuesday.md)

[Wednesday is Available Here](weekday_is_wednesday.md)

[Thursday is Available Here](weekday_is_thursday.md)

[Friday is Available Here](weekday_is_friday.md)

[Saturday is Available Here](weekday_is_Saturday.md)

[Sunday is Available Here](weekday_is_Sunday.md)
