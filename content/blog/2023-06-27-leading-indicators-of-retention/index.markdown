---
title: Leading Indicators of Retention
author: Julian
date: '2023-06-27'
slug: []
categories: []
tags: []
---

In this analysis we'll look at how two measures of activity relate to user retention:

 1. The number of key actions taken during the first week.
 2. The number of days active during the first week.


 

## Data Collection
To answer these questions, we'll collect data from over one million signups that occurred since January 1, 2021 with the SQL query below.


```r
# define sql query
sql <- "
  with users as (
    select
      u.user_id
      , u.signup_at_date
      , date_trunc(u.signup_at_date, week) as signup_week
      , date(min(a.timestamp)) as activated_at
      , count(distinct case when a.date < date_add(u.signup_at_date, interval 7 day) then a.id end) as actions
      , count(distinct case when a.date < date_add(u.signup_at_date, interval 7 day) then a.date end) as days_active
      , max(a.date) as last_active_date
    from dbt_buffer.buffer_users as u
    left join dbt_buffer.buffer_key_actions as a
      on u.user_id = a.user_id
      and a.timestamp >= u.signup_at
    where u.signup_at_date between date('2022-01-01') and date_sub(current_date(), interval 60 day)
    group by 1,2,3
  )
  
  select * from users
"

# collect data from bigquery
users <- bq_query(sql = sql)

# save data as rds object
saveRDS(users, "user_retention.rds")
```



We'll defined "retained" has having been active at least 60 days after signing up. Surprisingly, only around 12% of signups fit this description of retained.


```r
# determine if they activated and were retained
users <- users %>% 
  mutate(activated = !is.na(activated_at),
         retained = activated & as.numeric(last_active_date - signup_at_date) >= 60)

# calculate percentage active after 60 days
users %>% 
  group_by(retained) %>% 
  summarise(n = n_distinct(user_id)) %>% 
  mutate(prop = percent(n / sum(n)))
```

```
## # A tibble: 2 × 3
##   retained      n prop 
##   <lgl>     <int> <chr>
## 1 FALSE    927300 88%  
## 2 TRUE     120647 12%
```

## Key Actions and Retention
In this section we'll try to visualize the relationship between the number of key actions taken during the first week and retention.

We will only look at activated users here. The boxplots below show that users that are retained do tend to take more key actions in their first week.


```r
# filter for activated users
active <- users %>% 
  filter(activated & actions > 0 & days_active > 0)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

The median number of key actions taken in the first week for retained users is 8, compared to 3 for users that were not retained. Around 25% of retained users took 18 or more key actions in their first week, and around 25% of churned users took 8 or more key actions in their first week. 


```r
# define quantiles of interest
q = c(.25, .5, .75)

#calculate quantiles by grouping variable
active %>%
  group_by(retained) %>%
  summarize(quant25 = quantile(actions, probs = q[1]), 
            quant50 = quantile(actions, probs = q[2]),
            quant75 = quantile(actions, probs = q[3]))
```

```
## # A tibble: 2 × 4
##   retained quant25 quant50 quant75
##   <lgl>      <dbl>   <dbl>   <dbl>
## 1 FALSE          1       3       8
## 2 TRUE           3       8      18
```

## Days Active
Next we'll look at the relationship between days active and retention. The plot below shows us that there is a clear relationship between the number of days active and the likelihood of being retained.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

Even for activated users, being active in multiple days greatly increases the likelihood of still being active in 60 days. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

# Simple Logistic Regression
Next we'll fit a simple logistic regression to see how well the number of days active and the number of key actions can predict the likelihood of being retained.

We'll ignore potential collinearity for now and will only look at activated users. Both `days_active` and `actions` have significant effects in the model, but `days_active` has a positive relationship, and, suprisingly, `actions` has a negative one. 


```r
# fit logistic regression model
mod <- glm(retained ~ days_active + actions, data = active, family = "binomial")

# summarise model
summary(mod)
```

```
## 
## Call:
## glm(formula = retained ~ days_active + actions, family = "binomial", 
##     data = active)
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.806e+00  7.465e-03 -241.90   <2e-16 ***
## days_active  4.676e-01  3.425e-03  136.53   <2e-16 ***
## actions     -1.725e-03  8.263e-05  -20.88   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 363457  on 309026  degrees of freedom
## Residual deviance: 342513  on 309024  degrees of freedom
## AIC: 342519
## 
## Number of Fisher Scoring iterations: 5
```

After taking the log of actions, both variables have positive effects of a similar size.


```r
# fit logistic regression model
mod <- glm(retained ~ days_active + log(actions), data = active, family = "binomial")

# summarise model
summary(mod)
```

```
## 
## Call:
## glm(formula = retained ~ days_active + log(actions), family = "binomial", 
##     data = active)
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.930477   0.007925 -243.61   <2e-16 ***
## days_active   0.274177   0.004028   68.06   <2e-16 ***
## log(actions)  0.270867   0.004190   64.64   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 363457  on 309026  degrees of freedom
## Residual deviance: 339154  on 309024  degrees of freedom
## AIC: 339160
## 
## Number of Fisher Scoring iterations: 4
```
