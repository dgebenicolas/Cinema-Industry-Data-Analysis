HG_Index
================

hscore function, which takes a vector of HG Scores of a particular
director, returns a HG-H index of that director.

``` r
setwd('C:/Users/admin/Documents/R Data Analysis')
project_data = read_csv("Final_Project_FlixGem.csv")
```

    ## Rows: 9425 Columns: 29

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (19): Title, Genre, Tags, Languages, Series or Movie, Country Availabil...
    ## dbl   (8): Hidden Gem Score, IMDb Score, Rotten Tomatoes Score, Metacritic S...
    ## dttm  (2): Release Date, Netflix Release Date

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
q1_cleaned_data = project_data %>% drop_na() 
q1_cleaned_data = filter(q1_cleaned_data, q1_cleaned_data$`Series or Movie`=="Movie")
director = q1_cleaned_data
#director$Director = ifelse(grepl(',', director$Director, fixed = TRUE), substr(director$Director,1,regexpr(",",director$Director)-1), director$Director)

hscore = function(scores) {
  scores=sort(scores)
  result=0
  for (i in length(scores):1) {
    if (scores[i]>=length(scores)-i+1) {
      result=length(scores)-i+1
    }
    else {
      break;
    }
  }
  return(result)
}

director_hindex=director %>% group_by(Director) %>% dplyr::summarize(HG_H_index = hscore(`Hidden Gem Score`)) %>% arrange(desc(HG_H_index))

#Showing top 10 directors based on HG_index
director_hindex %>% head(., 10)
```

    ## # A tibble: 10 x 2
    ##    Director             HG_H_index
    ##    <chr>                     <dbl>
    ##  1 Alfonso Cuarón                4
    ##  2 Ang Lee                       4
    ##  3 Bong Joon Ho                  4
    ##  4 Christopher Nolan             4
    ##  5 David Fincher                 4
    ##  6 David Mackenzie               4
    ##  7 Edgar Wright                  4
    ##  8 Hayao Miyazaki                4
    ##  9 Martin Scorsese               4
    ## 10 Paul Thomas Anderson          4
