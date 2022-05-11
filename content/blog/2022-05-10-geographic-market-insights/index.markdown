---
title: Geographic Market Insights
author: Julian Winternheimer
date: '2022-05-10'
slug: []
categories: []
tags: []
---

Hello. The purpose of this analysis is to gain a better understanding of Buffer’s customer penetration in various countries and regions around the world. We would like to learn what markets we are doing well in and which have room for improvement. 

This analysis should help guide budget allocation decisions related to paid marketing by country and capture insights to guide our "market prioritization" framework for market development/expansion and penetration efforts.

**As of May 11, we're missing some data in the `fivetran_mixpanel.people` dataset. Because of this, I've mostly used proportions rather than counts in this analysis. 

**A key assumption made in this analysis is that the data is missing _at random_, and that the sample of data we do have is representative of the population.


## Summary
When we consider signups, usage, and paid conversions, Buffer's core markets are the US, UK, Canada, and Australia. 

Buffer gets a lot of signups and usage from India, but relatively few paid conversions. Other countries in which we see existing usage but relatively low conversion include France, Brazil, Bangladesh, and the Philippines.

Countries that have high conversion rates but relatively low signup numbers include South Africa, Belgium, Norway, New Zealand, Switzerland, Denmark, Finland, Ireland, and Germany.

Given that many citizens of European countries speak English, I'd recommend exploring translating site content into Spanish, Portuguese, and French. This could help us expand into markets with existing demand but relatively low conversion rates, like Brazil, the Philippines, and France. 

We could also consider German for Germany, Switzerland, and Austria, but people from these country may be more likely to already speak English.
___
 

 

## Data Collection
We'll collect the country codes from all signups, monthly active users, and new customers from the past 30 days.


```r
# define sql query
sql <- "
  select distinct
    a.user_id
    , a.created_with_trial
    , p.country_code as country
    , count(distinct k.id) as actions
    , count(distinct s.id) as subs
  from dbt_buffer.segment_accounts_created as a
  left join fivetran_mixpanel.people as p
    on a.user_id = p.distinct_id
  left join dbt_buffer.buffer_key_actions as k
    on k.user_id = a.user_id
    and k.timestamp <= timestamp_add(a.timestamp, interval 30 day)
  left join dbt_buffer.stripe_paid_subscriptions as s
    on s.account_id = a.user_id
  where a.timestamp >= '2022-01-01'
  group by 1,2,3
"

# collect data from bigquery
signups <- bq_query(sql = sql)

# save data as rds object
saveRDS(signups, "signups_by_country.rds")
```




```r
# define sql query
sql <- "
  select distinct
    k.user_id
    , p.country_code as country
  from dbt_buffer.buffer_key_actions as k
  left join fivetran_mixpanel.people as p
    on k.user_id = p.distinct_id
  where k.date >= date_sub(current_date(), interval 30 day)
"

# collect data from bigquery
mau <- bq_query(sql = sql)

# save data as rds object
saveRDS(mau, "mau_by_country.rds")
```




```r
# define sql query
sql <- "
  select distinct
    s.account_id as user_id
    , p.country_code as country
  from dbt_buffer.stripe_paid_subscriptions as s
  left join fivetran_mixpanel.people as p
    on s.account_id = p.distinct_id
  where date(s.first_paid_invoice_created_at) >= 
    date_sub(current_date(), interval 30 day)
"

# collect data from bigquery
conversions <- bq_query(sql = sql)

# save data as rds object
saveRDS(conversions, "conversions_by_country.rds")
```




## Signups By Country
First we'll look at which countries drive the most signups. We exclude users for which no geographical data is available.

The United States, India, Great Britain, France, and Canada are the largest contributors to signups.  

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

If we only look at signups for which the `createdWithTrial` property is present -- we started tracking this on March 1, 2022 -- then Bangladesh and the Philippines join the top 6.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

## Monthly Active Users By Country
The United States, Great Britain, Canada, France, and India are the largest contributors to Buffer's monthly active users.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="672" />

## Paid Conversions By Country
The United States, Great Britain, Canada, Australia, France, and Germany are the largest contributors to paid conversions over the past 30 days.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

## Paid Conversion Rates
Next we'll calculate the proportion of signups that started a paid Stripe subscription.

Norway, the United States, Canada, Australia, and New Zealand have the highest proportion of signups that convert to paying plans. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

## Activation Rates
We'll take the same approach to looking at activation rates. We only include countries that have contributed at least 100 signups. 

Here we can see a very different list of countries.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />

## Market Opportunities
The plot 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />

Another (possibly dubious) approach we could take is to create a custom metric that combines the proportion of signups (`prop_signups`) and the proportion of conversions (`prop_conversions`). 

The metric can be simple -- if we want to identify countries with relatively high conversion rates that don't make up a large proportion of signups, we can divide `prop_conversions` by `prop_signup`, which effectively penalizes countries that already make up a large proportion of signups.

These are the countries that have the highest values of the resulting metric. Australia leads the pack, followed by Great Britain, the US, Canada, Belgium, Germany, and South Africa.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />

