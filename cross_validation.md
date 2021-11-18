Cross validation
================
Ruiqi Yan
11/18/2021

Let’s simulate a dataset

``` r
set.seed(1)

nonlin_df = 
  tibble(
    id = 1:100,
    x = runif(100, 0, 1),
    y = 1 - 10 * (x - .3) ^ 2 + rnorm(100, 0, .3)
  )

nonlin_df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()
```

![](cross_validation_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Create splits by hand; plot some models,

``` r
train_df <- sample_n(nonlin_df, size = 80)
test_df <- anti_join(nonlin_df, train_df, by = "id")

ggplot(train_df, aes(x = x, y = y)) +
  geom_point() +
  geom_point(data = test_df, color = "red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

fit my models

``` r
linear_mod <- lm(y ~ x, data = train_df)
smooth_mod <- mgcv::gam(y ~ s(x), data = train_df)
wiggly_mod <- mgcv::gam(y ~ s(x, k = 30), sp = 10e-6, data = train_df)
```

plot the results

``` r
train_df %>% 
  add_predictions(linear_mod) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = pred)) 
```

![](cross_validation_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

quantify the results

``` r
rmse(linear_mod, test_df)
```

    ## [1] 0.7052956

``` r
rmse(smooth_mod, test_df)
```

    ## [1] 0.2221774

``` r
rmse(wiggly_mod, test_df)
```

    ## [1] 0.289051

## CV iteratively

Use `modelr::crossv_mc`

``` r
cv_df <-
  crossv_mc(nonlin_df, 100)
cv_df <-
  cv_df %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  )
```

Let’s fit some models

``` r
cv_df <-
  cv_df %>% 
  mutate(
    linear_mod = map(.x = train, ~lm(y ~ x, data = .x)),
    smooth_mod = map(.x = train, ~gam(y ~ s(x), data = .x, )),
    wiggly_mod = map(.x = train, ~gam(y ~ s(x, k = 30), sp = 10e-6, data = .x))
    ) %>% 
  mutate(
    rmse_linear = map2_dbl(.x = linear_mod, .y = test, ~rmse(model = .x, data = .y)),
    rmse_smooth = map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y)),
    rmse_wiggly = map2_dbl(.x = wiggly_mod, .y = test, ~rmse(model = .x, data = .y))
  )
```

Look at RMSE distributions.

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    rmse_linear:rmse_wiggly,
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_"
  ) %>% 
  ggplot(aes(x = model, y = rmse)) +
  geom_boxplot()
```

![](cross_validation_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Child growth data

import data

``` r
child_growth_df <- 
  read_csv("data/nepalese_children.csv")
```

``` r
child_growth_df %>% 
  ggplot(aes(x = weight, y = armc)) + 
  geom_point(alpha = .5)
```

![](cross_validation_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
child_growth_df <-
  child_growth_df %>% 
  mutate(weight_cp = (weight > 7) * (weight - 7))

linear_mod <- lm(armc ~ weight, data = child_growth_df)
pwl_mod    <- lm(armc ~ weight + weight_cp, data = child_growth_df)
smooth_mod <- gam(armc ~ s(weight), data = child_growth_df)

child_growth_df %>% 
  add_predictions(smooth_mod) %>% 
  ggplot(aes(x = weight, y = armc)) + 
  geom_point(alpha = .5) +
  geom_line(aes(y = pred), color = "red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
cv_df <-
  crossv_mc(child_growth_df, 100, test = .2) %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble))
```

Fit models and extract RMSE

``` r
cv_df <-
  cv_df %>% 
  mutate(
    linear_mod = map(.x = train, ~lm(armc ~ weight, data = .x)),
    pwl_mod = map(.x = train, ~lm(armc ~ weight + weight_cp, data = .x)),
    smooth_mod = map(.x = train, ~gam(armc ~ s(weight), data = .x))
    ) %>% 
  mutate(
    rmse_linear = map2_dbl(.x = linear_mod, .y = test, ~rmse(model = .x, data = .y)),
    rmse_smooth = map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y)),
    rmse_pwl = map2_dbl(.x = pwl_mod, .y = test, ~rmse(model = .x, data = .y))
  )
```

Look at RMSE distributions.

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    rmse_linear:rmse_pwl,
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_"
  ) %>% 
  ggplot(aes(x = model, y = rmse)) +
  geom_boxplot()
```

![](cross_validation_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
