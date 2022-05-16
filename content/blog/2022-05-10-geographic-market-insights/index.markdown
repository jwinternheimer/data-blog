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

**As of May 12, we're missing county data for around 8% of users. Because of this, I've mostly used proportions rather than counts in this analysis. 

**A key assumption made in this analysis is that the data is missing _at random_, and that the sample of data we do have is representative of the population.


## Summary
When we consider signups, activation, and paid conversions, Buffer's core market is the United States. It's singular and distinct from the rest of the countries in this analysis in its contribution to Buffer's business and financial success.

The UK, Canada, India, and Australia are secondary, but still important, markets for Buffer. Of these, the UK contributes the most -- the contribution of signups and revenue from the UK is more than that of Australia and Canada combined. Together with the US, Canada, Australia, and the UK contribute more conversions than the rest of the world.

The lowest performing market contains a group of countries including Bangladesh, Russia, Brazil, Algeria, and Venezuela. The rest of the world can be grouped together into a third market.

Buffer gets a lot of signups and usage from India, but relatively few paid conversions. It seems like there is an opportunity here given India's size and usage of Buffer. We should do what we can to make it easier for people in India to subscribe to Buffer's paid products.

Countries that have high conversion rates but relatively low signup numbers include New Zealand, Ireland, Denmark, Sweden, and Switzerland.

Given that many citizens of European countries speak English, I'd recommend exploring translating site content into Spanish and French. This could help us expand into markets with existing demand but relatively low conversion rates like the Philippines.

It might also be worth exploring paid advertising in countries with high conversion rates relative to their number of signups like Australia, New Zealand, Switzerland, and Ireland.

Finally, at the end of this analysis you'll find a section on how this location data has been collected.

___
 

 

## Data Collection
We'll collect the country codes from all signups, monthly active users, and new customers from the past 30 days.


















## Signups By Country
First we'll look at which countries drive the most signups. We exclude users for which no geographical data is available.

The United States, India, the UK, France, and Canada are the largest contributors to signups.  

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="672" />

If we only look at signups for which the `createdWithTrial` property is present -- we started tracking this on March 1, 2022 -- then Bangladesh and the Philippines join the top 6.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

## Monthly Active Users By Country
The United States, Great Britain, Canada, India, and France are the largest contributors to Buffer's monthly active users.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

## Paid Conversions By Country
The United States, the UK, Canada, Australia, France, and Germany are the largest contributors to paid conversions since the beginning of 2022.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />

## Paid Conversion Rates
Next we'll calculate the proportion of signups that started a paid Stripe subscription within 30 days.

Colombia, Canada, the US, Australia, and the UK have the highest proportion of signups that convert to paying plans within 30 days of signing up. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />

## Activation Rates
We'll take the same approach to looking at activation rates. We only include countries that have contributed at least 500 signups. 

The UK, Netherlands, Australia, Canada, and Japan have the highest activation rates.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />


## MRR by Country
The MRR amounts used to generate the plot below are estimates based on plan values and exclude any and all discounts.

The US contributes the most to MRR by far, followed by the UK, Canada, Australia, and Germany. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-16-1.png" width="672" />


## Active User Retention
The plot below shows the countries with the highest 3-month retention rates for the `Key Action Taken` event. For each country, this shows the percentage of people that take a key action 3 months after taking their first key action.

People from the UK, Australia, US, Japan, and Canada have the highest 3-month retention rates.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="672" />


## Market Opportunities
The plot below shows the proportion of signups for each country on the x-axis and the proportion of conversions for each country on the y-axis. 

Countries above the dotted line contribute a higher percentage of conversions relative to the number of signups than countries located below the dotted line. Countries like Switzerland, Canada, Australia, New Zealand, and Ireland are in this group.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-18-1.png" width="672" />

Another (possibly dubious) approach we could take is to create a custom metric that combines the proportion of signups (`prop_signups`) and the proportion of conversions (`prop_conversions`). 

The metric can be simple -- if we want to identify countries with relatively high conversion rates that don't make up a large proportion of signups, we can divide `prop_conversions` by `prop_signup`, which effectively penalizes countries that already make up a large proportion of signups.

These are the countries that have the highest values of the resulting metric. The US, Australia, New Zealand, Ireland, Switzerland, and Canada lead the pack if we go by this metric.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-19-1.png" width="672" />


## Clustering Countries
Clustering refers to a very broad set of techniques for finding subgroups, or clusters, in a dataset. When we cluster the countries, the goal is to partition them into distinct groups so that the countries within each group are quite similar to each other, while countries in different groups are quite different from each other.

In this analysis we'll take a simple approach to clustering and use a technique called [_K-means clustering_](https://en.wikipedia.org/wiki/K-means_clustering).

To perform K-means clustering, we must first specify the desired number of clusters K; then the K-means algorithm will assign each country to exactly one of those K clusters by minimizing the _within cluster variance_. 

In this simple exercise we'll only utilize two features, `prop_signups` and `prop_conversions`, for each country. This makes visualizing the clusters very simple.


```r
# set seed for reproducibility
set.seed(13)

# scale signups and conversions
countries_scaled <- countries %>% 
  filter(signups > 500) %>% 
  mutate(signups_scaled = scale(signups),
         conversions_scaled = scale(conversions))

# perform k-means clustering with k = 2,3,4
km2 <- kmeans(select(countries_scaled, signups_scaled:conversions_scaled), 
              centers = 2, nstart = 25)

km3 <- kmeans(select(countries_scaled, signups_scaled:conversions_scaled),  
              centers = 3, nstart = 25)

km4 <- kmeans(select(countries_scaled, signups_scaled:conversions_scaled), 
              centers = 4, nstart = 25)

# add to countries
countries_scaled$cluster2 <- km2$cluster
countries_scaled$cluster3 <- km3$cluster
countries_scaled$cluster4 <- km4$cluster
```

Now we can plot each of the clusters. If we only include two clusters, the United States is in a cluster of its own.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-21-1.png" width="672" />

If we cluster the countries into three groups, the US makes up one cluster, France, the UK, and India make up the second, and the rest of the world makes up the third.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-22-1.png" width="672" />

If we cluster the countries into four groups, this is how they turn out:

 - Cluster 1: United States
 - Cluster 2: United Kingdom and India
 - Cluster 3: Canada, France, Australia, Germany, Netherlands, Philippines, Spain, Mexico, Indonesia, Mexico, Brazil, Italy, Turkey, Nigeria, Pakistan.
 - Cluster 4: Rest of the world.


<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-23-1.png" width="672" />


## Hierarchical Clustering
One potential disadvantage of K-means clustering is that it requires us to specify the number of clusters K. Hierarchical clustering is a clustering technique that does not require a specified number of clusters.

In the dendrogram below, each leaf corresponds to one country As we move up the tree, countries that are similar to each other are combined into branches, which are themselves fused at a higher height.

The height of the fusion indicates the dissimilarity between two observations. The higher the height of the fusion, the less similar the countries are.

It's important to remember that we use the _height_ of the fusion to visualize the similarity of countries, not the proximity on the x-axis. For example, the US is not similar to Bangladesh. However, Bangladesh and Russia are similar.


```r
# set seed
set.seed(8)

# hierarchical cluster
m <- countries %>% 
  filter(signups >= 500) %>% 
  mutate(signups = scale(signups),
         conversions = scale(conversions),
         total_mrr = scale(total_mrr),
         activation_rate = scale(activation_rate)) %>% 
  select(country, signups, conversions, activation_rate, total_mrr) %>% 
  as.data.frame()

# set row names
rownames(m) <- m$country

# remove country column
m <- m %>% select(-country)

# cluster countries with complete linkeage
hc_complete <- hclust(dist(m), method = "complete")

# convert hclust into a dendrogram and plot
hcd <- as.dendrogram(hc_complete)

# plot dendrogram
plot(hcd, type = "rectangle", ylab = "Height",
     nodePar = list(lab.cex = 0.6, pch = NA, cex = NA, col = NA))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-24-1.png" width="672" />

We can "cut" the dendrogram to obtain clusters of countries. The height of the cut controls the number of clusters obtained -- it plays the same role as the k in k-means clustering. In order to identify sub-groups (i.e. clusters), we can cut the dendrogram with the `cutree` function.

If we cut the dendrogram into 4 distinct clusters, we get the following groups:

 - The United States
 - The UK and India
 - Bangladesh, Russia, Algeria, Brazil, and Venezuela.
 - The rest of the world.


```r
#cut tree into 4 distinct groups
sub_grp <- cutree(hc_complete, k = 4)

# number of countries in each cluster
table(sub_grp)
```

```
## sub_grp
##  1  2  3  4 
##  5 46  2  1
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-26-1.png" width="672" />

We can also use the `cutree` output to add the the cluster each observation belongs to to our original data.


```r
# add back to original data
m <- m %>%
  mutate(cluster = sub_grp)

# set country
m$country = rownames(m)

# preview data
head(m)
```

```
##                signups conversions activation_rate  total_mrr cluster
## Algeria    -0.44424774  -0.2769158      -1.7043142 -0.2287470       1
## Argentina  -0.26572985  -0.2422497      -0.7410994 -0.2182506       2
## Australia   0.03924935   0.3272641       1.0500353  0.3349677       2
## Austria    -0.42537891  -0.2187263       0.7706291 -0.1851314       2
## Bangladesh  0.28146627  -0.2657731      -2.8115030 -0.2286690       1
## Belgium    -0.35231237  -0.1791080       0.6532620 -0.1651913       2
##               country
## Algeria       Algeria
## Argentina   Argentina
## Australia   Australia
## Austria       Austria
## Bangladesh Bangladesh
## Belgium       Belgium
```

We can then join this back into the original `countries` data frame and compute summary statistics.


```r
# join clusters
countries <- countries %>% 
  inner_join(select(m, country, cluster))

# summarise clusters
countries %>% 
  group_by(cluster) %>% 
  summarise(countries = n_distinct(country, na.rm = T),
            avg_signups = mean(signups, na.rm = T),
            avg_conversions = mean(conversions, na.rm = T),
            avg_activation_rate = mean(activation_rate, na.rm = T),
            avg_mrr = mean(total_mrr, na.rm = T),
            total_signups = sum(signups),
            total_conversions = sum(conversions))
```

```
## # A tibble: 4 × 8
##   cluster countries avg_signups avg_conversions avg_activation_rate avg_mrr
##     <int>     <int>       <dbl>           <dbl>               <dbl>   <dbl>
## 1       1         5       2959             18.8               0.153    546.
## 2       2        46       2355.           103.                0.336   8409.
## 3       3         2      19590.           891                 0.361 103564.
## 4       4         1      49530           5809                 0.345 829816.
## # … with 2 more variables: total_signups <dbl>, total_conversions <dbl>
```

The US is in a cluster of its own. It contributes the most to signups and conversions by a long shot.

The UK and India also contribute a lot of signups and conversions. Excluding the US, the UK and India contribute over 8 times the number of signups and conversions than the rest of the world, on average. 

The five countries including Brazil, Venezuela, Russia, Bangladesh, and Algeria, contribute a relatively high number of signups, but their activation and paid conversion rates are extremely low compared to the rest of the world.

It might be worth mentioning that Australia and Canada have high activation rates and contribute a significant amount of signups and conversions, but combined they still contribute less conversions than the UK.


## How Country Data is Collected
Each section of this analysis _besides the MRR breakdown_ uses country codes that are set in Mixpanel. Mixpanel's client-side libraries collect user location data (city, region, country) as roperties by default. 

The way they do this is by pulling the user's IP address and running it through a third-party IP geolocator, MaxMind. MaxMind returns the city, region, and country, and Mixpanel sets those as user properties.

For the MRR breakdown, we utilize the countries associated with Stripe customers' credit cards. 
