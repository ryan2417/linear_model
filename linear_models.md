Linear Models
================
Ruiqi Yan
11/16/2021

Load NYC Airbnb data

``` r
data("nyc_airbnb")
nyc_airbnb <-
  nyc_airbnb %>% 
  mutate(stars = review_scores_location / 2) %>% 
  rename(
    borough = neighbourhood_group
  ) %>% 
  filter(
    borough != "Staten Island"
  ) %>% 
  select(price, stars, borough, neighbourhood, room_type)
```

``` r
nyc_airbnb %>% 
  ggplot(aes(x = stars, y = price)) +
  geom_point()
```

![](linear_models_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Let’s look at this

``` r
fit <- lm(price ~ stars + borough, data = nyc_airbnb)

summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -169.8  -64.0  -29.0   20.2 9870.0 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -70.414     14.021  -5.022 5.14e-07 ***
    ## stars              31.990      2.527  12.657  < 2e-16 ***
    ## boroughBrooklyn    40.500      8.559   4.732 2.23e-06 ***
    ## boroughManhattan   90.254      8.567  10.534  < 2e-16 ***
    ## boroughQueens      13.206      9.065   1.457    0.145    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 181.5 on 30525 degrees of freedom
    ##   (9962 observations deleted due to missingness)
    ## Multiple R-squared:  0.03423,    Adjusted R-squared:  0.03411 
    ## F-statistic: 270.5 on 4 and 30525 DF,  p-value: < 2.2e-16

If you want to present output

``` r
fit %>% 
  broom::tidy() %>% 
  mutate(
    term = str_replace(term, "borough", "Borough: ")
  ) %>% 
  select(term, estimate, p.value) %>% 
  knitr::kable()
```

| term               |  estimate |   p.value |
|:-------------------|----------:|----------:|
| (Intercept)        | -70.41446 | 0.0000005 |
| stars              |  31.98989 | 0.0000000 |
| Borough: Brooklyn  |  40.50030 | 0.0000022 |
| Borough: Manhattan |  90.25393 | 0.0000000 |
| Borough: Queens    |  13.20617 | 0.1451682 |

Calculate residuals for observations

``` r
modelr::add_residuals(nyc_airbnb, fit) %>% 
  ggplot(aes(x = resid)) +
  geom_density() +
  xlim(-200, 200)
```

![](linear_models_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Let’s try a different model

``` r
fit_2 <- lm(price ~ stars*borough + room_type*borough, data = nyc_airbnb)
broom::tidy(fit_2)
```

    ## # A tibble: 16 × 5
    ##    term                                   estimate std.error statistic   p.value
    ##    <chr>                                     <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                               90.1       75.4    1.19   0.232    
    ##  2 stars                                      4.45      16.6    0.267  0.789    
    ##  3 boroughBrooklyn                          -20.4       77.1   -0.265  0.791    
    ##  4 boroughManhattan                           5.63      77.8    0.0723 0.942    
    ##  5 boroughQueens                              1.51      83.5    0.0181 0.986    
    ##  6 room_typePrivate room                    -52.9       17.8   -2.98   0.00288  
    ##  7 room_typeShared room                     -70.5       41.6   -1.70   0.0896   
    ##  8 stars:boroughBrooklyn                     16.5       17.0    0.973  0.331    
    ##  9 stars:boroughManhattan                    22.7       17.1    1.33   0.185    
    ## 10 stars:boroughQueens                        5.21      18.3    0.285  0.776    
    ## 11 boroughBrooklyn:room_typePrivate room    -39.3       18.0   -2.18   0.0292   
    ## 12 boroughManhattan:room_typePrivate room   -71.3       18.0   -3.96   0.0000754
    ## 13 boroughQueens:room_typePrivate room      -16.3       19.0   -0.859  0.390    
    ## 14 boroughBrooklyn:room_typeShared room     -35.3       42.9   -0.822  0.411    
    ## 15 boroughManhattan:room_typeShared room    -83.1       42.5   -1.96   0.0503   
    ## 16 boroughQueens:room_typeShared room       -24.4       44.4   -0.550  0.582

Compare performances of models

``` r
fit_null = lm(price ~ stars + borough, data = nyc_airbnb)
fit_alt = lm(price ~ stars + borough + room_type, data = nyc_airbnb)

anova(fit_null, fit_alt) %>% 
  broom::tidy()
```

    ## # A tibble: 2 × 6
    ##   res.df         rss    df     sumsq statistic p.value
    ##    <dbl>       <dbl> <dbl>     <dbl>     <dbl>   <dbl>
    ## 1  30525 1005601724.    NA       NA        NA       NA
    ## 2  30523  921447496.     2 84154228.     1394.       0

Let’s try nesting

``` r
nyc_airbnb %>% 
  relocate(borough) %>% 
  nest(data = price:room_type) %>% 
  mutate(
    lm_fits = map(.x = data, ~ lm(price ~ stars + room_type, data = .x)),
    lm_results = map(lm_fits, broom::tidy)
  ) %>% 
  select(borough, lm_results) %>% 
  unnest(lm_results) %>% 
  filter(term == "stars")
```

    ## # A tibble: 4 × 6
    ##   borough   term  estimate std.error statistic  p.value
    ##   <chr>     <chr>    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 Bronx     stars     4.45      3.35      1.33 1.85e- 1
    ## 2 Queens    stars     9.65      5.45      1.77 7.65e- 2
    ## 3 Brooklyn  stars    21.0       2.98      7.05 1.90e-12
    ## 4 Manhattan stars    27.1       4.59      5.91 3.45e- 9

Look at neighbourhood in Manhattan

``` r
manhattan_lm_results_df <- 
  nyc_airbnb %>% 
  filter(borough == "Manhattan") %>% 
  select(-borough) %>% 
  relocate(neighbourhood) %>% 
  nest(data = price:room_type) %>% 
  mutate(
    lm_fits = map(.x = data, ~lm(price ~ stars + room_type, data = .x)),
    lm_results = map(lm_fits, broom::tidy)
  ) %>% 
  select(neighbourhood, lm_results) %>% 
  unnest(lm_results)

manhattan_lm_results_df %>% 
  ggplot(aes(x = estimate)) +
  geom_density()
```

![](linear_models_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
manhattan_lm_results_df %>% 
  filter(str_detect(term, "room_type")) %>% 
  ggplot(aes(x = neighbourhood, y = estimate)) +
  geom_point() +
  facet_grid(~ term) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

![](linear_models_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
nyc_airbnb_glm <- 
  nyc_airbnb %>% 
  mutate(
    expensive_apt = as.numeric(price > 500)
  )
```

Let’s fit a logistic regression for the binary outcome

``` r
logistic_fit <- 
  glm(
    expensive_apt ~ stars + borough,
    data = nyc_airbnb_glm,
    family = binomial()
)

logistic_fit %>% 
  broom::tidy() %>% 
  mutate(
    term = str_replace(term, "borough", "Borough: "),
    estimate = exp(estimate)
  ) %>% 
  select(term, OR = estimate, p.value)
```

    ## # A tibble: 5 × 3
    ##   term                     OR    p.value
    ##   <chr>                 <dbl>      <dbl>
    ## 1 (Intercept)        7.52e-10 0.908     
    ## 2 stars              2.15e+ 0 0.00000292
    ## 3 Borough: Brooklyn  2.49e+ 5 0.945     
    ## 4 Borough: Manhattan 8.11e+ 5 0.940     
    ## 5 Borough: Queens    1.15e+ 5 0.949

``` r
nyc_airbnb %>% 
  modelr::add_predictions(logistic_fit) %>% 
  mutate(
    pred = boot::inv.logit(pred)
  )
```

    ## # A tibble: 40,492 × 6
    ##    price stars borough neighbourhood room_type                pred
    ##    <dbl> <dbl> <chr>   <chr>         <chr>                   <dbl>
    ##  1    99   5   Bronx   City Island   Private room     0.0000000343
    ##  2   200  NA   Bronx   City Island   Private room    NA           
    ##  3   300  NA   Bronx   City Island   Entire home/apt NA           
    ##  4   125   5   Bronx   City Island   Entire home/apt  0.0000000343
    ##  5    69   5   Bronx   City Island   Private room     0.0000000343
    ##  6   125   5   Bronx   City Island   Entire home/apt  0.0000000343
    ##  7    85   5   Bronx   City Island   Entire home/apt  0.0000000343
    ##  8    39   4.5 Bronx   Allerton      Private room     0.0000000234
    ##  9    95   5   Bronx   Allerton      Entire home/apt  0.0000000343
    ## 10   125   4.5 Bronx   Allerton      Entire home/apt  0.0000000234
    ## # … with 40,482 more rows