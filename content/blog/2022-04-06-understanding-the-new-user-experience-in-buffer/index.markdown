---
title: Understanding the New User Experience in Buffer
author: Julian Winternheimer
date: '2022-04-06'
slug: []
categories: []
tags: []
---

In this analysis we'll attempt to gain a better understanding of how new users interact with Buffer, identify behavior patterns that are indicative of success and share a few recommendations for how to improve the experience.

Over the course of this analysis we'll look at paid conversion rates, activation rates, the relationships between feature usage and conversion rates, session duration, upgrade paths, the distribution of the time it takes to convert, and much more. 

I'll share a brief summary of the findings in the section below if you don't want to read the entire analysis document (it's long!), but I'd recommend looking through it all when you have the time.

___

## Summary
Overall, this exploratory analysis confirms what we would suspect -- usage of Buffer's features is correlated with a higher likelihood of activating and purchasing a paid subscription. 

Usage of each feature is indicative of success, even when we try to control for users being active in general. 

There are a couple upgrade paths that have historically driven a large quantity of upgrades that are no longer performing well. Specifically, I would recommend looking more closely at these two paths and designing experiments to learn more about why they aren't performing as well:

 - `publish-composer-profileQueueLimit-showPaidPlans-1`
 - `publish-profileSidebar-addChannelButton-upgrade-1`
 
In addition to these two upgrade paths, I would recommend that we create a new upgrade path that is targeted specifically to users that want to add a team member. This is a key action that one can take and something that is highly correlated with success. 

The duration of new users' first sessions is highly correlated with success. In general, the longer they spend in the product during their first session, the more likely they are to activate and pay for a subscription. 

First session duration could be a useful early indicator of success that we can optimize for in growth experiments. 

In general, if users do not purchase a paid plan in their first 60 days, they are significantly less likely to do so in the future. The experience that new users have when signing up is highly influential and has a significant effect on their (and our) success with the product. 

___
 

 

## Data Collection
To answer these questions, we'll collect data from over one million signups that occurred since January 1, 2021 with the SQL query below.


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

# save data as rds object
saveRDS(users, "trial_analysis_users.rds")
```



The resulting dataset includes approximately 1 million users, their signup dates, whether or not they signed up on a mobile device, whether or not they signed up from a team member invite, their activation date (the date on which they took their first key action), and several other fields related to their activity in the first 14 days after signing up for Buffer.

We can get a glimpse of the dataset using the `glimpse()` command below.


```r
# preview dataset
glimpse(users)
```

```
## Rows: 1,005,973
## Columns: 19
## $ user_id               <chr> "5ff01cd5a3db4b2b618a9a44", "5ff09a60757a915cfaa…
## $ stripe_customer_id    <chr> "cus_IgRA0CEEXYIthc", NA, "cus_IgOzw8pjSaWjOx", …
## $ signup_at_date        <date> 2021-01-02, 2021-01-02, 2021-01-02, 2021-01-02,…
## $ signup_week           <date> 2020-12-27, 2020-12-27, 2020-12-27, 2020-12-27,…
## $ mobile_signup         <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ is_team_member        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ is_currently_trialing <lgl> TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRU…
## $ trial_signup          <lgl> TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRU…
## $ activated_at          <date> NA, NA, 2021-01-02, NA, NA, NA, NA, NA, 2021-01…
## $ converted_date        <date> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ sessions_14_days      <int> 0, 1, 17, 0, 0, 4, 2, 0, 3, 0, 1, 5, 0, 3, 1, 0,…
## $ actions               <int> 0, 0, 339, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 32, 0, …
## $ days_active           <int> 0, 0, 3, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, …
## $ publish_actions       <int> 0, 0, 339, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 32, 0, …
## $ analyze_actions       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ engage_actions        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ sp_actions            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ converted_30day       <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
## $ converted             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
```

## 30-Day Conversion Rates
We'll begin with a bit of exploratory analysis. We'd like to have a better understanding of the effect that starting new users on a trial has, so we'll calculate the proportion of free and trial signups that subscribed to a paid plan within 30 days of signup. 

The proportions below only include people that signed up on the web app, since people that sign up on the mobile apps do not have the option of starting a trial when they sign up.


```r
# calculate 30-day conversion rates
users %>% 
  filter(signup_week < "2022-03-01" & (is.na(mobile_signup) | !mobile_signup)) %>% 
  group_by(trial_signup, converted_30day) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(percent = percent(users / sum(users), accuracy = 0.01)) %>% 
  filter(converted_30day)
```

```
## # A tibble: 2 × 4
## # Groups:   trial_signup [2]
##   trial_signup converted_30day users percent
##   <lgl>        <lgl>           <int> <chr>  
## 1 FALSE        TRUE             3828 0.81%  
## 2 TRUE         TRUE            14588 5.22%
```

Unsurprisingly people that have decided to sign up on a trial have converted to paid plans at a much higher rate than those that have signed up on a free plan. There is likely some selection bias at play here. If we put all signups on to a trial, it's likely that the 30-day conversion rate will fall somewhere in between these two rates.


Next we'll plot these conversion rates over time, grouping by the week in which people signed up. The plot below shows us that the conversion rates have remained relatively stable over time, with a slight positive trend. 

The 30-day conversion rate for people that sign up on a trial has been consistently higher than that of people that sign up on a Free plan.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />


## Distribution of Time to Conversion
The next thing we'll cover is the distribution of the number of days it takes new signups to convert to a paid plan. We'll start by calculating the quantiles of the number of days to convert for both free and trial signups.


```r
# define quantiles of interest
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
## 1 FALSE              2      18      57
## 2 TRUE               2      14      21
```

These quantiles show us that the median number of days to convert is 14 days for trial signups and 18 days for free signups, which is logical. As of April 2022, Buffer trials are 14 days in length, and many people that subscribe to paid plan do so on the day that their trials expire. 

It's interesting to note that over a quarter of users that convert do so by their second day.

The plot below visualizes distribution of the number of days to convert below.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

A higher proportion of free signups convert on the day that they sign up, and more conversions are clustered around the 14-day mark (the length of the trial) for those that sign up on a trial. 

Another way to visualize the distributions of the number of days it takes people to convert is to plot the cumulative distribution functions (CDFs) for these two populations.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

Each point on the line represents the proportion of conversions that occurred in X days _or fewer_. For example, at X = 14 we can see that approximately 64% of trial signups have converted (implying that approximately 36% of conversions take longer than 14 days) and 58% of free signups have converted. This principle can be applied to any point on the curve. We can see that by day 60 most people that will convert have converted.

This doesn't quite give us a complete picture though. The plots above only include users that have subscribed to a paid plan and the data is [right censored](https://reliability.readthedocs.io/en/latest/What%20is%20censored%20data.html#:~:text=Censored%20data%20is%20any%20data,referred%20to%20as%20complete%20data.), meaning there are some users that will convert that haven't yet done so.

One useful technique for dealing with censored data is [survival analysis](https://en.wikipedia.org/wiki/Survival_analysis).


## Survival Analysis
Because a greater proportion of trialists end up converting, it could be worth using survival analysis techniques to visualize the amount of time it takes to convert.

The survival probability at a certain time, S(t), is a conditional probability of surviving (i.e. not converting) beyond that time, given that an individual hasn't converted just prior to that time.

The Kaplan-Meier method is the most common way to estimate survival times and probabilities. It is a non-parametric approach that results in a step function, where there is a step down each time an event occurs. 

A Kaplan-Meier plot is included below for both free and trial signups.



<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

Each point on the graph represents the proportion of the population that _hasn't_ converted after X days. The inverse would be the proportion that had converted by day X.

The plot tells us a couple things. The first, which we already knew, is that people that sign up with trials convert at much higher rates that don't. The second, and perhaps more important, thing is that the first 60 days after signing up are crucial for those that convert.

There is a long tail of people that convert after the 60 day mark, but it is significantly less likely for any given user to convert if they haven't done so by their 60th day.


## Session Counts and Conversion
The next thing we'll look at is the relationship between session counts and paid conversion. We'll use a similar approach to calculate quantiles of the number of sessions in the first 14 days for free and trial users.


```r
# quantiles of interest
q = c(.25, .5, .75)

# calculate quantiles by grouping variable
users %>%
  filter(is.na(mobile_signup) | !mobile_signup) %>% 
  mutate(days_to_convert = as.numeric(converted_date - signup_at_date)) %>% 
  group_by(converted) %>%
  summarize(quant25 = quantile(sessions_14_days, probs = q[1]), 
            quant50 = quantile(sessions_14_days, probs = q[2]),
            quant75 = quantile(sessions_14_days, probs = q[3]))
```

```
## # A tibble: 2 × 4
##   converted quant25 quant50 quant75
##   <lgl>       <dbl>   <dbl>   <dbl>
## 1 FALSE           0       1       3
## 2 TRUE            4      10      19
```

These quantiles tell that there is a significant difference in the number of sessions that users who convert and don't convert have in their first 14 days. This is to be expected, as more engaged users are more likely to have more sessions and convert.

The plot below shows the distribution of the number of sessions users had in their first 14 days for four distinct populations:

 - Users that signed up on a free plan and converted
 - Users that signed up with a trial and converted
 - Users that signed up on a free plan and didn't convert
 - Users that signed up with a trial and didn't convert
 
We can see that for all signups, the distribution of the number of sessions is shifted to the right for those that ended up converting. 

Users that convert to paid plans generally have more sessions in their first 14 days. The largest bucket of converted users had 10-25 sessions in their first 2 weeks.



```r
# define breakpoints 
breakpoints <- c(-Inf, 0, 1, 5, 10, 25, 50, Inf)

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

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />

The plot below shows the conversion rates for users that had certain numbers of sessions in their first 14 days.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />

We can see that there is a clear correlation between the number of sessions in users' first 14 days and the likelihood of converting to a paid subscription.

Next we'll look at some of the differences between people that sign up on web and those that sign up on one of the mobile apps.


## Web and Mobile Signups
It's important to note that by "mobile" we only refer to those that signed up on one of the mobile apps. People that sign up on a mobile web browser are still considered "web" signups.

The first thing we'll look at is the 30-day conversion rate for web and mobile signups.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />

30-day conversion rates are generally much lower for people that sign up on mobile. Let's now compare conversion rates for web users that signed up without a trial.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-16-1.png" width="672" />

We can see that the conversion rates are much closer, but, overall, people that sign up on the mobile apps convert at lower rates than those that sign up on web.

Next we'll explore Buffer's key actions and their relationships to paid conversions.

## Key Actions and Upgrades
First we'll calculate the correlation coefficients for each of the key actions and whether or not a user converted.




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

We can see that the correlations are quite low, but this could be because the variance of the number of actions is quite high. 

For example, the plot below shows the cumulative distribution function (CDF) for the number of publishing actions taken in the first 14 days for people that converted and people that didn't. There's a large range for the `publish_actions` variable -- scaling the action counts might lead to more instructive correlation coefficients.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-19-1.png" width="672" />

The plot below shows the relative conversion rates for those that did and didn't use certain products in their first two weeks. We can see that users that use Analyze or Engage tend to convert at higher rates, but not using those features doesn't necessarily mean that there's a low chance of conversion.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-20-1.png" width="672" />

We can also group the number of actions taken in each product to visualize the correlations.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-21-1.png" width="672" />

The plots above show that there are indeed correlations between the number of key actions taken and conversion rates. This is all intuitive -- users that are engaged and more active in the product are more likely to convert. Additionally, users that convert within the first 14 days are more likely to take more actions.

In the next section we'll try to get a better understanding of the individual importance of each feature. One way to do that is to fit a model containing all p predictors and use a technique that constrains or regularizes the coefficient estimates, or equivalently, that shrinks the coefficient estimates towards zero. 

The two best-known techniques for shrinking the regression coefficients towards zero are ridge regression and the lasso. Each method utilizes a shrinkage penalty which has the effect of shrinking the estimates of βj towards zero when the coefficients themselves are close to 0. 

The tuning parameter λ serves to control the relative impact of these two terms on the regression coefficient estimates. When λ = 0, the penalty term has no effect, and ridge regression will produce the least squares estimates. However, as λ → ∞, the impact of the shrinkage penalty grows, and the ridge regression coefficient estimates will approach zero.

While ridge regression shrinks the coefficient estimates towards 0, Lasso regressions actually shrinks the coefficients to exactly 0, which is a form of variable selection. It's generally useful when multicollinearity is present and one wants to get a better understanding of which predictors are most important.


## Lasso Regression
Simply put, lasso regression shrinks the coefficients of predictors to 0 if they don't explain a sufficient amount of the variance in the response.

A lasso regression model is fit with the simple command below. 




```r
# fit lasso regression model
mod <- cv.glmnet(x, y, alpha = 1)

# plot model
plot(mod)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-23-1.png" width="672" />

```r
# get best lambda value
bestlam <- mod$lambda.min

# show coefficients
predict(mod, type = "coefficients", s = bestlam)
```

```
## 11 x 1 sparse Matrix of class "dgCMatrix"
##                             s1
## (Intercept)      -9.009296e-03
## mobile_signup     .           
## is_team_member    .           
## trial_signup      3.346454e-02
## sessions_14_days  1.806440e-03
## actions          -3.400643e-06
## days_active       5.332346e-03
## used_publish      1.422048e-02
## used_engage       1.557889e-01
## used_analyze      6.702460e-02
## used_sp           7.979015e-04
```

We can see that the coefficients for `mobile_signup` and `is_team_member` have been shrunk to 0. The rest of the features may have some predictive value, however more time should be spent on feature engineering and model tuning than we have to complete this analysis. 


## Number of Days Active and Conversion
One of the coefficients remaining in the lasso regression model was `days_active`, which refers to the number of days in which a key action was taken during the 14 days after signup. 

The plot below shows the number of days active on the x-axis and the conversion rate on the y-axis. We can see that there is a relationship, though it isn't completely linear. Users that are active around 7 days are more likely to convert, and users that are active every day are very likely to convert.

We have to remember that the actions could have been taken _after_ users convert, so there might be some reverse causality here.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-24-1.png" width="672" />

Next we'll shift gears and look at activation, which is defined as having taken one's first key action.

## Frequency of Login and Activation
The first thing we'll look at is how frequency of login relates to the likelihood of activated. The data could be muddled because some users activate quickly, and activated users are more likely to log in. Additionally [80% of users that activate in their first 7 days do so within 12 hours of signing up](https://mixpanel.com/s/2HgEX). 

We'll use the query below to collect activation-related data for over one million Buffer users.


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



The output below shows the quantiles for average session duration for users that did and didn't activate _in their first day_. The average session duration refers only to the session in which the users signed up.


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

Those that activated on their first day tended to have much longer first sessions. The median session duration was 26 minutes, compared to 5 minutes for those that didn't activate. 

Let's now look at the number of events that occur in the first session.


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

Again, users that activated in their first day tended to have more events in their first session, which is logical.

The plot below shows the activation rates for certain session duration buckets. We can see that there's a large increase in the activation rate for those whose first session was at least 10 minutes.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-29-1.png" width="672" />

The plot below shows the total activation rate by first session length for each user. It's clear that first session length is highly correlated with activation.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-30-1.png" width="672" />

Next we'll extend this to look at the relationship between first session length and the likelihood of converting to a paid subscription. Again we see that there's a strong correlation present.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-31-1.png" width="672" />

In the next session we'll shift gears again and look at specific upgrade paths within Buffer's products.

## Upgrade Paths
The data used in the analysis below comes from [this Mixpanel report](https://mixpanel.com/s/4DPzTa) and only includes upgrade paths that have been defined in the `Upgrade Path Viewed` tracking event.





We'll start by looking at the number of conversions each path drives each week. All of the conversions must occur within 7 days of viewing the upgrade path.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-34-1.png" width="672" />

The upgrade path that stands out most clearly is `publish-profile-nav-tabNavigation-upgrade-1`. At one point it was driving over 200 upgrades per week, but it has since been changed to a prompt to start a trial. Now it drives only 5-15 upgrades per week.  

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-35-1.png" width="672" />

Another one that stands out is `publish-profileSidebar-addChannelButton-upgrade-1`. Upgrades from this path have also decreased by over 50%. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-36-1.png" width="672" />

**_I would strongly recommend testing those upgrade paths. Specifically, I would run an A/B test to measure the effect of changing it from a trial prompt to a direct upgrade path_**.

Next let's look at the conversion rates of each upgrade path. The conversion rate is just the proportion of users that viewed an upgrade path and converted within 7 days of that event.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-37-1.png" width="672" />

Here are the top upgrade paths by conversion rate:

 - Queue limit
 - Campaigns empty state
 - Awaiting approval paywall
 - Top nav upgrade button
 - Add channel button
 - Hashtag manager path
 - Drafts paywall
 
We'll isolate a couple of these in the section below.
 
 
## Queue Limit and Channel Limit Upgrade Paths
These upgrade paths have the highest conversion rates. Both are based on activity -- the queue limit upgrade path is displayed when free users schedule too many posts, and the channel limit upgrade path is diplayed when people connect more channels than what's allowed.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-38-1.png" width="672" />

Many more users view the channel limit upgrade path, which is why it drives more conversions. The plot below shows the number of weekly conversions that each upgrade path drives.
 
<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-39-1.png" width="672" />

In the section below we'll attempt to answer a specific question about the role that the user limit plays in converting people to paid subscribers.

## User Limit Upgrade Path
As of April 2022 we're not tracking a distinct upgrade path for the 1-user limit. I'd recommend tracking this upgrade path if it exists, and if it doesn't I'd recommend creating one. 

Right now the closest upgrade path we have would probably be `publish-awaitingApproval-paywall-upgrade-1`. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-40-1.png" width="672" />


## Role of Analyze and Engage During Trial
Next we'll look at the role analyze and engage play during trials. The data used in this section of the analysis is gathered with the following SQL query. The data includes 180K trials started since September 2021 (after the New Buffer launch).


```r
# define sql query
sql <- "
  select
    t.id as trial_id
    , t.trial_started_at
    , t.subscription_id
    , t.customer_id
    , t.plan_id
    , t.metadata_user_id as trial_user_id
    , t.has_converted as converted
    , count(distinct a.id) as actions
    , count(distinct case when a.product = 'publish' then a.id end) as pub_actions
    , count(distinct case when a.product = 'engage' then a.id end) as engage_actions
    , count(distinct case when a.product = 'analyze' then a.id end) as analyze_actions
  from dbt_buffer.stripe_trials as t
  left join dbt_buffer.buffer_key_actions as a
    on t.metadata_user_id = a.user_id
    and a.timestamp > t.trial_started_at
    and a.timestamp < timestamp_add(t.trial_started_at, interval 14 day)
  where t.trial_started_at >= '2021-09-01'
  group by 1,2,3,4,5,6,7
"

# collect data from bigquery
trials <- bq_query(sql = sql)

# save data
saveRDS(trials, "trial_analysis_trials.rds")
```



One quick approach we could take is to fit a logistic regression model to see if using each of the features is correlated with trial conversion _given that the user was active during trial_. It's important to note that there's likely collinearity, which is why some regularization or feature selection cpuld be important. 

For this exploratory analysis we'll only look at the model coefficients without regularization or further feature engineering.


```r
# fit logistic regression model
mod <- glm(converted ~ active + publish_active + engage_active + analyze_active,
           data = trials, family = "binomial")

# summarize model
summary(mod)
```

```
## 
## Call:
## glm(formula = converted ~ active + publish_active + engage_active + 
##     analyze_active, family = "binomial", data = trials)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0557  -0.5001  -0.2233  -0.2233   2.7216  
## 
## Coefficients:
##                    Estimate Std. Error  z value Pr(>|z|)    
## (Intercept)        -3.67866    0.02018 -182.308  < 2e-16 ***
## activeTRUE          0.16817    0.04420    3.805 0.000142 ***
## publish_activeTRUE  1.49454    0.03748   39.873  < 2e-16 ***
## engage_activeTRUE   0.82993    0.04156   19.969  < 2e-16 ***
## analyze_activeTRUE  0.89281    0.02375   37.590  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 93982  on 180332  degrees of freedom
## Residual deviance: 82598  on 180328  degrees of freedom
## AIC: 82608
## 
## Number of Fisher Scoring iterations: 6
```

The model output suggests that use of each feature is correlated with trial conversion. We can try using lasso regression again to see if any of the features can be dropped from the model.




```r
# fit lasso regression model
mod <- cv.glmnet(x, y, alpha = 1)

# summarize model
coef(mod)
```

```
## 5 x 1 sparse Matrix of class "dgCMatrix"
##                        s1
## (Intercept)    0.03144349
## active         .         
## publish_active 0.08766096
## analyze_active 0.06294737
## engage_active  0.07372473
```

All features are correlated with conversion, which isn't surprising. However, using the publishing feature seems to be indicative of a much greater likelihood of converting.

The plot below shows the relative conversion rates for trialists that did and didn't use each feature.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-46-1.png" width="672" />


## Role of Analyze and Engage For Free Signups
Finally we'll look at the correlations between using Analyze and Engage and paid conversions for users that sign up on a free plan. 

We'll use the query below to gather the number of times free users used Analyze and Engage in their first 14 days after signing up (or before converting if they did convert). There are about 61 thousand users in total.


```r
# define sql query
sql <- "
  with first_sub as (
    select
      c.user_id
      , c.timestamp as signup_at
      , date(c.timestamp) as signup_date
      , count(distinct s.id) > 0 as converted
      , min(s.first_paid_invoice_created_at) as converted_at
    from dbt_buffer.segment_accounts_created as c
    left join dbt_buffer.stripe_paid_subscriptions as s
      on s.account_id = c.user_id
      and s.first_paid_invoice_created_at >= c.timestamp
    where not c.created_with_trial
    group by 1,2,3
  )
  select
    f.user_id
    , f.signup_at
    , f.signup_date
    , f.converted
    , f.converted_at
    , count(distinct a.id) as actions
    , count(distinct case when a.product = 'publish' then a.id end) as pub_actions
    , count(distinct case when a.product = 'engage' then a.id end) as engage_actions
    , count(distinct case when a.product = 'analyze' then a.id end) as analyze_actions
  from first_sub as f
  left join dbt_buffer.buffer_key_actions as a
    on f.user_id = a.user_id
    and a.timestamp > f.signup_at
    and a.timestamp < timestamp_add(f.signup_at, interval 14 day)
    and (a.timestamp < f.converted_at or f.converted_at is null)
  group by 1,2,3,4,5
"

# collect data from bigquery
free <- bq_query(sql = sql)

# save data
saveRDS(free, "trial_analysis_free.rds")
```



We'll fit another logistic regression model to see if using each of the features is correlated with paid conversion _given that the user was active_. It's important to note that there's likely collinearity, which is why some regularization or feature selection is important.


```r
# fit logistic regression model
mod <- glm(converted ~ active + publish_active + engage_active + analyze_active,
           data = free, family = "binomial")

# summarize model
summary(mod)
```

```
## 
## Call:
## glm(formula = converted ~ active + publish_active + engage_active + 
##     analyze_active, family = "binomial", data = free)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.6318  -0.1675  -0.0923  -0.0923   3.3151  
## 
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -5.45592    0.07368 -74.051  < 2e-16 ***
## activeTRUE         -0.03495    0.24049  -0.145    0.884    
## publish_activeTRUE  1.23111    0.22236   5.537 3.08e-08 ***
## engage_activeTRUE   1.32803    0.24875   5.339 9.36e-08 ***
## analyze_activeTRUE  1.42168    0.14312   9.933  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5685.8  on 61081  degrees of freedom
## Residual deviance: 5311.6  on 61077  degrees of freedom
## AIC: 5321.6
## 
## Number of Fisher Scoring iterations: 8
```

The model coefficients and p-values suggest that the use of each feature is correlated with success.

Below we'll plot conversion rates by whether or not users used each feature in their first 14 days.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-50-1.png" width="672" />

Again we can see that usage of Engage and Analyze are correlated with conversion.

___ 

## Summary
Overall, this exploratory analysis confirms what we would suspect -- usage of Buffer's features is correlated with a higher likelihood of activating and purchasing a paid subscription. 

Usage of each feature is indicative of success, even when we try to control for users being active in general. 

There are a couple upgrade paths that have historically driven a large quantity of upgrades that are no longer performing well. Specifically, I would recommend looking more closely at these two paths and designing experiments to learn more about why they aren't performing as well:

 - `publish-composer-profileQueueLimit-showPaidPlans-1`
 - `publish-profileSidebar-addChannelButton-upgrade-1`
 
In addition to these two upgrade paths, I would recommend that we create a new upgrade path that is targeted specifically to users that want to add a team member. This is a key action that one can take and something that is highly correlated with success. 

The duration of new users' first sessions is highly correlated with success. In general, the longer they spend in the product during their first session, the more likely they are to activate and pay for a subscription. 

First session duration could be a useful early indicator of success that we can optimize for in growth experiments. 

In general, if users do not purchase a paid plan in their first 60 days, they are significantly less likely to do so in the future. The experience that new users have when signing up is highly influential and has a significant effect on their (and our) success with the product. 
