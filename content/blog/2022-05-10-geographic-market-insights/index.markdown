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
When we consider signups, usage, and paid conversions, Buffer's core markets are the US, UK, Canada, Australia, and possibly France. 

Buffer gets a lot of signups and usage from India, but relatively few paid conversions. It seems like there is an opportunity here given India's size and usage of Buffer. Brazil is another country with a low paid conversion rate that contributes a relatively large number of signups.

Countries that have high conversion rates but relatively low signup numbers include New Zealand, Ireland, Colombia, Australia, and Switzerland.

Given that many citizens of European countries speak English, I'd recommend exploring translating site content into Spanish and French. This could help us expand into markets with existing demand but relatively low conversion rates like the Philippines.

It might also be worth targeting countries with high conversion rates and lower signup numbers like Australia, New Zealand, and Ireland.

___
 

 

## Data Collection
We'll collect the country codes from all signups, monthly active users, and new customers from the past 30 days.
















## Signups By Country
First we'll look at which countries drive the most signups. We exclude users for which no geographical data is available.

The United States, India, the UK, France, and Canada are the largest contributors to signups.  

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

If we only look at signups for which the `createdWithTrial` property is present -- we started tracking this on March 1, 2022 -- then Bangladesh and the Philippines join the top 6.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="672" />

## Monthly Active Users By Country
The United States, Great Britain, Canada, India, and France are the largest contributors to Buffer's monthly active users.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

## Paid Conversions By Country
The United States, the UK, Canada, Australia, France, and Germany are the largest contributors to paid conversions since the beginning of 2022.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

## Paid Conversion Rates
Next we'll calculate the proportion of signups that started a paid Stripe subscription within 30 days.

Colombia, Canada, the US, Australia, and the UK have the highest proportion of signups that convert to paying plans within 30 days of signing up. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />

## Activation Rates
We'll take the same approach to looking at activation rates. We only include countries that have contributed at least 500 signups. 

The UK, Netherlands, Australia, Canada, and Japan have the highest activation rates.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />

## Market Opportunities
The plot 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />

Another (possibly dubious) approach we could take is to create a custom metric that combines the proportion of signups (`prop_signups`) and the proportion of conversions (`prop_conversions`). 

The metric can be simple -- if we want to identify countries with relatively high conversion rates that don't make up a large proportion of signups, we can divide `prop_conversions` by `prop_signup`, which effectively penalizes countries that already make up a large proportion of signups.

These are the countries that have the highest values of the resulting metric. The US, Australia, New Zealand, Ireland, Switzerland, and Canada lead the pack if we go by this metric.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-16-1.png" width="672" />


## MRR by Country
The MRR amounts used to generate the plot below are estimates based on plan values and exclude any and all discounts.

The US contributes the most to MRR by far, followed by the UK, Canada, Australia, and Germany. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="672" />


## How Country Data is Collected
Each section of this analysis _besides the MRR breakdown_ uses country codes that are set in Mixpanel. Mixpanel's client-side libraries collect user location data (city, region, country) as roperties by default. 

The way they do this is by pulling the user's IP address and running it through a third-party IP geolocator, MaxMind. MaxMind returns the city, region, and country, and Mixpanel sets those as user properties.

For the MRR breakdown, we utilize the countries associated with Stripe customers' credit cards. 
