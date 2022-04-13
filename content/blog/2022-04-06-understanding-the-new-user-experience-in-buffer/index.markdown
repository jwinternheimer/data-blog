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
We'll collect over one million signups that occurred since January 1, 2021 with the query below.


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




## 30-Day Conversion Rates
Let's start by calculating the proportion of free and trial signups that subscribed to a paid plan within 30 days of signing up.


```r
# calculate conversion rates
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
## 1 FALSE              2      18      57
## 2 TRUE               2      14      21
```

These quantiles show us that the median number of days to convert is 14 days for trial signups and 18 days for free signups. It's interesting to note that more than a quarter of the people that convert do so by their second day. We should remember that this only takes into account users that converted. 

We can plot the distribution of the number of days to convert below.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

A higher proportion of free signups convert on the day that they sign up, and more conversions are clustered around the 14-day mark (the length of the trial) for trial signups.


## Survival Analysis
Because a greater proportion of trialists end up converting, it could be worth using survival analysis techniques to visualize the amount of time it takes to convert.


```r
# set status column
users <- users %>% 
  mutate(status = ifelse(converted, 1, 0),
         time = ifelse(
           converted, as.numeric(converted_date - signup_at_date),
           as.numeric(Sys.Date() - signup_at_date)))

# fit survival curve
fit.surv <- survfit(Surv(time, status) ~ trial_signup, data = users)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

Each point on the graph represents the proportion of the population that _hasn't_ converted after X days. The inverse is the proportion that had converted by day X.

## Distribution of Number of Sessions
We can use a similar approach to calculate quantiles of the number of sessions in the first 14 days for free and trial users.


```r
# calculate quantiles by grouping variable
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
## 3 TRUE      FALSE              4       9      18
## 4 TRUE      TRUE               5      10      20
```


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

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

We can see that users that convert to paid plans generally have more sessions in their first 14 days. The largest bucket of converted users had 10-25 sessions in their first 14 days.


## Web vs Mobile
It's important to note that mobile signups do not start on trials -- they all start on a free plan.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

30-day conversion rates are generally much lower for people that sign up on mobile. Let's now compare conversion rates for web users that signed up without a trial.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />

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

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-16-1.png" width="672" />

We can see that users that converted tended to take more publish actions than those that didn't convert. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="672" />


The plot above shows us the conversion rates for those that did and did not use certain features in their first 14 days. We can see that users that use Analyze or Engage tend to convert at high rates, but not using those features doesn't necessarily mean that there's a low chance of conversion.

We can also bucket the number of actions taken in each product to visualize the correlations.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-18-1.png" width="672" />

The plots above show that there are indeed correlations between the number of key actions taken and conversion rates, which is unsurprising. 

## Lasso Regression
Lasso regression is a method we can use to fit a regression model when multicollinearity is present in the data.

In a nutshell, lasso regression shrinks the coefficients of predictors to 0 if they don't contribute enough to the model.


```r
# set new column
users <- users %>% mutate(response = ifelse(converted_30day, 1, 0))

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

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-21-1.png" width="672" />

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

Those that activated on their first day tended to have much longer first sessions. The median session duration was 26 minutes, compared to 5 minutes for those that didn't activate. Let's now look at the number of events.


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

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-26-1.png" width="672" />

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-27-1.png" width="672" />

We can see that there is a strong correlation between first session duration and the likelihood of activating on the first day. Let's look at the relationship between first session length and the likelihood of conversion.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-28-1.png" width="672" />

We can see that there is a clear correlation with conversion as well.

## Upgrade Paths
Next we'll look at upgrade paths.The data used in the analysis below comes from [this Mixpanel report](https://mixpanel.com/s/4DPzTa) and only includes upgrade paths that have been defined in the `Upgrade Path Viewed` tracking event.





We'll start by looking at the number of conversions each path drives each week. All of the conversions must occur within 7 days of viewing the upgrade path.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-31-1.png" width="672" />

The upgrade path that stands out most clearly is `publish-profile-nav-tabNavigation-upgrade-1`. At one point it was driving over 200 upgrades per week, but it has since been changed to a prompt to start a trial. Now it drives only 5-15 upgrades per week.  

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-32-1.png" width="672" />


Another one that stands out is `publish-profileSidebar-addChannelButton-upgrade-1`. Upgrades from this path have also decreased by over 50%. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-33-1.png" width="672" />

Next let's look at the conversion _rates_ of each upgrade path. The conversion rate is just the proportion of users that viewed an upgrade path and converted within 7 days of that event.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-34-1.png" width="672" />

Here are the top upgrade paths by conversion rate:

 - Queue limit
 - Campaigns empty state
 - Awaiting approval paywall
 - Top nav upgrade button
 - Add channel button
 - Hashtag manager path
 - Drafts paywall
 
Let's isolate a couple of these.
 
 
## Queue Limit and Channel Limit Upgrade Paths
This upgrade path has the highest conversion rate. Around 140-160 users run into it each week.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-35-1.png" width="672" />

Many more users view the channel limit upgrade path, which is why it drives more conversions.
 
<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-36-1.png" width="672" />


## User Limit Upgrade Path
As of April 2022 we're not tracking a distinct upgrade path for the 1-user limit. I'd recommend tracking this upgrade path if it exists, and if it doesn't I'd recommend creating one. 

Right now the closest upgrade path we have would probably be `publish-awaitingApproval-paywall-upgrade-1`. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-37-1.png" width="672" />


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



One quick approach we could take is to fit a logistic regression model to see if using each of the features is correlated with trial conversion _given that the user was active during trial_. It's important to note that there's likely collinearity, which is why some regularization or feature selection is important.


```r
# fit logistic regression model
mod <- glm(converted ~ active + publish_active + engage_active + analyze_active,
           data = trials, family = "binomial")

# summarise model
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

The use of each feature is correlated with success. We can try using lasso regression again.


```r
# set new column
trials <- trials %>% mutate(response = ifelse(converted, 1, 0))

# define response variable
y <- trials$response

# define matrix of predictor variables
features <- trials %>% 
  dplyr::select(active:engage_active)

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
## 5 x 1 sparse Matrix of class "dgCMatrix"
##                        s1
## (Intercept)    0.03144349
## active         .         
## publish_active 0.08766096
## analyze_active 0.06294737
## engage_active  0.07372473
```

All features are correlated with conversion, which isn't surprising. However, using Engage seems to be indicative of a much greater likelihood of converting.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-43-1.png" width="672" />


## Role of Analyze and Engage For Free Signups
Finally we'll look at the correlations between using Analyze and Engage and paid conversions for users that sign up on a free plan. We'll use the query below to gather the number of times free users used Analyze and Engage in their first 14 days after signing up (or before converting if they did convert). There are about 61 thousand users in total.


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

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-47-1.png" width="672" />

Again we can see that usage of Engage and Analyze are correlated with conversion.
