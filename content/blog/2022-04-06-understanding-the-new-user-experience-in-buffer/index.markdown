---
title: Understanding the New User Experience in Buffer
author: Julian Winternheimer
date: '2022-04-06'
slug: []
categories: []
tags: []
---

We'd like to gain a better understand our new user journey within Buffer and document its current performance.In doing so, we'd like to be able to answer the following questions:

 1. What is the elasticity of our signup-to-customer conversion rate for cohorts starting in Trial when we increase volume?

 2. Does defaulting new signups to Trial increase their exposure and usage of our pay-walled features like Analyze?

 3. What improvements can be made to the 14-Day Trial period for less qualified users? 
 

 


## Data Collection
We'll collect the approximately 1M signups that occurred since January 1, 2021.


```r
# define sql query
sql <- "
  with users as (
    select
      u.user_id
      , u.stripe_customer_id
      , u.signup_at_date
      , date_trunc(u.signup_at_date, week) as signup_week
      , u.did_signup_from_mobile as mobile_signup
      , u.is_team_member
      , u.is_currently_trialing
      , u.did_signup_on_trial as trial_signup
      , date(min(a.timestamp)) as activated_at
      , date(min(s.first_paid_invoice_created_at)) as converted_date
      , count(distinct ss.session_id) as sessions_14_days
      , count(distinct a.id) as actions
      , count(distinct a.date) as days_active
      , count(distinct case when a.product = 'publish' then a.id end) as publish_actions
      , count(distinct case when a.product = 'analyze' then a.id end) as analyze_actions
      , count(distinct case when a.product = 'engage' then a.id end) as engage_actions
      , count(distinct case when a.product = 'start_pages' then a.id end) as sp_actions
    from dbt_buffer.buffer_users as u
    left join dbt_buffer.buffer_key_actions as a
      on u.user_id = a.user_id
      and a.timestamp >= u.signup_at
      and a.timestamp <= timestamp_add(u.signup_at, interval 14 day)
    left join dbt_buffer.stripe_paid_subscriptions as s
      on u.stripe_customer_id = s.customer_id
      and s.first_paid_invoice_created_at >= u.signup_at
    left join dbt_buffer.segment_sessions as ss
      on ss.dbt_visitor_id = u.user_id
      and ss.started_at >= u.signup_at
      and date(ss.started_at) <= date_add(u.signup_at_date, interval 14 day)
    where u.signup_at >= '2021-01-01'
    group by 1,2,3,4,5,6,7,8
  )
  
  select * from users
"

# collect data from bigquery
users <- bq_query(sql = sql)

# save data
saveRDS(users, "trial_analysis_users.rds")
```




## Baseline Conversion Rates
Let's start by calculating the proportion of free and trial signups that subscribed to a paid plan within 30 days of signing up.


```r
# calculate conversion rates
users %>% 
  filter(signup_week < "2022-03-01" & (is.na(mobile_signup) | !mobile_signup)) %>% 
  group_by(trial_signup, converted) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(percent = percent(users / sum(users), accuracy = 0.01)) %>% 
  filter(converted)
```

```
## # A tibble: 2 × 4
## # Groups:   trial_signup [2]
##   trial_signup converted users percent
##   <lgl>        <lgl>     <int> <chr>  
## 1 FALSE        TRUE       3828 0.81%  
## 2 TRUE         TRUE      14588 5.22%
```

Approximately 0.8% of free web signups convert within 30 days, compared to 5.2% of trial signups. This includes users that signed up that were automatically put onto trials. This trend is relatively stable over time.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />


## Distribution of Time to Conversion
Next we'll analyze the distributions of the number of days it takes new signups to convert to a paid plan. We can start by calculating quantiles for trial and free users.


```r
#define quantiles of interest
q = c(.25, .5, .75)

#calculate quantiles by grouping variable
users %>%
  filter(converted & (is.na(mobile_signup) | !mobile_signup)) %>% 
  mutate(days_to_convert = as.numeric(converted_date - signup_at_date)) %>% 
  group_by(trial_signup) %>%
  summarize(quant25 = quantile(days_to_convert, probs = q[1]), 
            quant50 = quantile(days_to_convert, probs = q[2]),
            quant75 = quantile(days_to_convert, probs = q[3]))
```

```
## # A tibble: 2 × 4
##   trial_signup quant25 quant50 quant75
##   <lgl>          <dbl>   <dbl>   <dbl>
## 1 FALSE              0       6      16
## 2 TRUE               0      13      15
```

These quantiles show us that the median number of days to convert is 13 days for trial signups and 6 days for free signups. It's interesting to note that more than a quarter of the people that convert do so on the day they sign up. We should remember that this only takes into account users that converted. 

We can plot the distribution of the number of days to convert below.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

A higher proportion of free signups convert on the day that they sign up, and more conversions are clustered around the 14-day mark (the length of the trial) for trial signups.


## Distribution of Number of Sessions
We can use a similar approach to calculate quantiles of the number of sessions in the first 14 days for free and trial users.


```r
#calculate quantiles by grouping variable
users %>%
  filter(is.na(mobile_signup) | !mobile_signup) %>% 
  mutate(days_to_convert = as.numeric(converted_date - signup_at_date)) %>% 
  group_by(converted, trial_signup) %>%
  summarize(quant25 = quantile(sessions_14_days, probs = q[1]), 
            quant50 = quantile(sessions_14_days, probs = q[2]),
            quant75 = quantile(sessions_14_days, probs = q[3]))
```

```
## `summarise()` has grouped output by 'converted'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 4 × 5
## # Groups:   converted [2]
##   converted trial_signup quant25 quant50 quant75
##   <lgl>     <lgl>          <dbl>   <dbl>   <dbl>
## 1 FALSE     FALSE              0       1       3
## 2 FALSE     TRUE               1       1       3
## 3 TRUE      FALSE              5      11      22
## 4 TRUE      TRUE               5      11      21
```

The distributions of the number of sessions in the first 14 days looks similar for free and trial signups.


```r
# define breakpoints 
breakpoints <- c(-Inf, 0, 1, 10, 25, 50, Inf)

# plot distributions
users %>%
  filter(is.na(mobile_signup) | !mobile_signup) %>% 
  mutate(session_bucket = cut(sessions_14_days, breaks = breakpoints),
         type = case_when(
          converted & trial_signup ~ "converted_trial",
          converted & !trial_signup ~ "converted_free",
          !converted & trial_signup ~ "no_conversion_trial",
          !converted & !trial_signup ~ "no_conversion_free",
          TRUE ~ "unknown"
  )) %>% 
  group_by(type, session_bucket) %>%
  summarise(users = n_distinct(user_id)) %>% 
  mutate(percent = users / sum(users)) %>% 
  buffplot(aes(x = session_bucket, y = percent, fill = type)) +
  geom_col(show.legend = F) +
  facet_wrap(~type) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  labs(x = "Sessions First 14 Days", y = "Percent of Users")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

We can see that users that convert to paid plans generally have more sessions in their first 14 days. The largest bucket of converted users had 10-25 sessions in their first 14 days.

## Web vs Mobile
It's important to note that mobile signups do not start on trials -- they all start on a free plan.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="672" />

30-day conversion rates are generally much lower for people that sign up on mobile. Let's now compare conversion rates for web users that signed up without a trial.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

We can see that the conversion rates are much closer, but they're still higher for web signups.

## Key Actions and Upgrades
Let's see which key actions are most correlated with conversion.




```r
# filter data
filtered <- users %>% select(converted, publish_actions:sp_actions)

# create correlation matrix
m <- cor(filtered)

# view matrix
head(round(m, 2))
```

```
##                 converted publish_actions analyze_actions engage_actions
## converted            1.00            0.05            0.13           0.02
## publish_actions      0.05            1.00            0.01           0.00
## analyze_actions      0.13            0.01            1.00           0.03
## engage_actions       0.02            0.00            0.03           1.00
## sp_actions           0.01            0.00            0.01           0.00
##                 sp_actions
## converted             0.01
## publish_actions       0.00
## analyze_actions       0.01
## engage_actions        0.00
## sp_actions            1.00
```

We can see that the correlations are quite low, but this could be because the variance of the number of actions is quite high. For example, let's plot the distribution of the number of publish actions taken in the first 14 days.


```r
# plot dist of publish actions
users %>% 
  buffplot(aes(x = publish_actions, color = converted)) +
  stat_ecdf() +
  coord_cartesian(xlim = c(0, 500))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />

We can see that users that converted tended to take more publish actions than those that didn't convert. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />


The plot above shows us the conversion rates for those that did and did not use certain features in their first 14 days. We can see that users that use Analyze or Engage tend to convert at high rates, but not using those features doesn't necessarily mean that there's a low chance of conversion.

We can also bucket the number of actions taken in each product to visualize the correlations.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-16-1.png" width="672" />

The plots above show that there are indeed correlations between the number of key actions taken and conversion rates, which is unsurprising. 

## Lasso Regression
Lasso regression is a method we can use to fit a regression model when multicollinearity is present in the data.

In a nutshell, lasso regression shrinks the coefficients of predictors to 0 if they don't contribute enough to the model.


```r
# set new column
users <- users %>% mutate(response = ifelse(converted, 1, 0))

# define response variable
y <- users$response

# define matrix of predictor variables
features <- users %>% 
  dplyr::select(mobile_signup, is_team_member, trial_signup,
                sessions_14_days, actions, days_active,used_publish, 
                used_engage, used_analyze, used_sp)

# turn strings into factors
features <- as.data.frame(unclass(features), stringsAsFactors = TRUE)

# create matrix
x <- as.matrix(features)
```


```r
# fit lasso regression model
mod <- cv.glmnet(x, y, alpha = 1)

# summarise model
coef(mod)
```

```
## 11 x 1 sparse Matrix of class "dgCMatrix"
##                            s1
## (Intercept)      -0.002163823
## mobile_signup     .          
## is_team_member    .          
## trial_signup      0.024786009
## sessions_14_days  0.001439186
## actions           .          
## days_active       0.005757164
## used_publish      0.006156045
## used_engage       0.108551558
## used_analyze      0.052469141
## used_sp           .
```

The lasso regression model suggests that using engage and analyze are predictive of conversions. After that, signing up with a trial, the number of days active, and the number of sessions in the first 14 days are the most predictive features.

We can also plot the relationship between the number of days active in the first 14 days and conversion rate.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-19-1.png" width="672" />

Users that are active at least 5 days have a much higher likelihood of converting to a paid plan, especially if they signed up with a trial.

## Frequency of Login and Activation
To answer this question we'll need to collect more data. The data could be muddled because some users activate quickly, and activated users are more likely to log in. Additionally [80% of users that activate in their first 7 days do so within 12 hours of signing up](https://mixpanel.com/s/2HgEX). 

Let's look at the number of events in the first session and average session duration.


```r
# define sql query
sql <- "
  with users as (
    select
      u.user_id
      , u.stripe_customer_id
      , u.signup_at_date
      , date_trunc(u.signup_at_date, week) as signup_week
      , u.did_signup_from_mobile as mobile_signup
      , u.is_team_member
      , u.is_currently_trialing
      , u.did_signup_on_trial as trial_signup
      , date(min(a.timestamp)) as activated_at
      , date(min(s.first_paid_invoice_created_at)) as converted_date
      , count(distinct ss.session_id) as signup_sessions
      , avg(ss.n_events) as avg_sesh_events
      , avg(ss.session_duration_minutes) as avg_sesh_duration
    from dbt_buffer.buffer_users as u
    left join dbt_buffer.buffer_key_actions as a
      on u.user_id = a.user_id
      and a.timestamp >= u.signup_at
      and a.timestamp <= timestamp_add(u.signup_at, interval 14 day)
    left join dbt_buffer.stripe_paid_subscriptions as s
      on u.stripe_customer_id = s.customer_id
      and s.first_paid_invoice_created_at >= u.signup_at
    left join dbt_buffer.segment_sessions as ss
      on ss.dbt_visitor_id = u.user_id
      and ss.signup_session
      and date(ss.started_at) = date(u.signup_at)
    where u.signup_at >= '2021-01-01'
    group by 1,2,3,4,5,6,7,8
  )
  
  select * from users
"

# collect data from bigquery
activations <- bq_query(sql = sql)

# save data
saveRDS(activations, "trial_analysis_activations.rds")
```



Let's show some summary stats for session length.


```r
# calculate quantiles by grouping variable
activations %>%
  filter(!is.na(avg_sesh_duration)) %>% 
  group_by(activated_day1) %>%
  summarize(quant25 = quantile(avg_sesh_duration, probs = q[1]), 
            quant50 = quantile(avg_sesh_duration, probs = q[2]),
            quant75 = quantile(avg_sesh_duration, probs = q[3]))
```

```
## # A tibble: 2 × 4
##   activated_day1 quant25 quant50 quant75
##   <lgl>            <dbl>   <dbl>   <dbl>
## 1 FALSE                1       5      17
## 2 TRUE                10      26      52
```

Those that activated on their first day tended to have much longer first sessions. The median session duration was 26 minutes, compared to 5 mintes for those that didn't activate. Let's now look at the number of events.


```r
# calculate quantiles by grouping variable
activations %>%
  filter(!is.na(avg_sesh_events)) %>% 
  group_by(activated_day1) %>%
  summarize(quant25 = quantile(avg_sesh_events, probs = q[1]), 
            quant50 = quantile(avg_sesh_events, probs = q[2]),
            quant75 = quantile(avg_sesh_events, probs = q[3]))
```

```
## # A tibble: 2 × 4
##   activated_day1 quant25 quant50 quant75
##   <lgl>            <dbl>   <dbl>   <dbl>
## 1 FALSE                8      17      31
## 2 TRUE                25      50      87
```

Again, users that activated in their first day tended to have more events in their first session, which makes sense if the session was longer.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-24-1.png" width="672" />

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-25-1.png" width="672" />

We can see that there is a strong correlation between first session duration and the likelihood of activating on the first day.
