---
title: Distribution of Conversion Time
author: Julian Winternheimer
date: '2022-02-02'
slug: []
categories: []
tags: []
---

In this analysis we'll analyze the distribution of the amount of time it takes new Buffer users to subscribe to a paid plan. 

This data is censored in nature -- there are people that will convert to paid plans that have yet to do so -- however, for the purpose of this analysis, we'll look only at paying customers that have had at least six months to convert.




## Data Collection
The dataset we'll use in this analysis contains around 259 thousand users that subscribed to a paid plan at some point before August 2021 (six months ago). Everyone in the dataset had at least six months to convert, which means that all users that started their first subscription on or after August 2021 have been excluded. 

The SQL query below was used to collect data for this analysis.


```r
# define sql query
sql <- "
  with first_conversion as (
    select
      u.user_id
      , u.signup_at
      , min(s.first_paid_invoice_created_at) as first_conversion_at
    from dbt_buffer.stripe_paid_subscriptions s
    inner join dbt_buffer.buffer_users u
      on s.account_id = u.user_id
    group by 1,2
  
  )
  select
    user_id
    , date(signup_at) as signup_date
    , s.id as subscription_id
    , s.plan_id
    , date(first_conversion_at) as conversion_date
    , timestamp_diff(first_conversion_at, signup_at, day) as days_to_convert
  from first_conversion f
  left join dbt_buffer.stripe_paid_subscriptions s
    on s.account_id = f.user_id
    and s.first_paid_invoice_created_at = f.first_conversion_at
  where first_conversion_at <= '2021-08-01'
  and timestamp_diff(first_conversion_at, signup_at, day)  >= 0
"

# collect data from bigquery
users <- bq_query(sql = sql)

# save data
saveRDS(users, "subscription_time.rds")
```




## Exploratory Analysis
First let's plot the distribution of the number of days it took users to subscribe to their first paid plan.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

As expected, the distribution resembles a [power law distribution](https://en.wikipedia.org/wiki/Power_law), with many conversions occurring in a short time and a long tail of conversions that took a very long time.

Another way to visualize this distribution is to plot the cumulative distribution function, which shows us the proportion of users that converted in X days _or fewer_.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />

This plot tells us that around 55% of the customers in our dataset subscribed to a paid plan within 30 days of signing up. This implies that around 45% of customers took longer than 30 days to convert.

Around 63% of customers took 60 days or fewer to convert and around 75% took 180 days or less.Around 84% of customers converted within 365 days of signing up (16% took longer than a year).

We can take a closer look at those that convert within their first 30 days.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

This shows us that there is a large group of users that subscribed to paid plans on the same day that they signed up for Buffer. There are also spikes at the 7 and 14 day marks, which would correspond with various trial lengths Buffer has had over the years.

Next we'll look at the quantiles corresponding to given probabilities.


```r
# quantiles
quantile(users$days_to_convert, probs = seq(0, 1, 0.1))
```

```
##   0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
##    0    0    1    7   14   22   47  118  279  619 4404
```

These quantiles tell us that 20% of users in our dataset converted within 1 day of signing up for Buffer. Around 50% converted within 22 days and 80% converted within 279 days of signing up.

Overall the number of days to convert has a very long tail, and it might be best to focus on the first 28 or 30 days after signing up for Buffer.
