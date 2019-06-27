require("reshape2")
require("dplyr")
require("stringr")
require("lubridate")
require("ggplot2")
require("knitr")
require("ggrepel")
require("gplots")

# Visualization of data to detect marginal ROI

#read file
socialTopline <- read.csv("$socialData")
socialTopline$WeekStart <- as.Date(socialTopline$weekStart, "%Y-%m-%d")

#summerization of dataset
summary(socialTopline)

# check NA
sum(is.na(socialTopline))

#get PPA and Virtual wallet
unique(socialTopline$HighLevelCampaign)

# campaign1 lower only
campaign1_lower <- filter(socialTopline, HighLevelCampaign == "campaign1_lower")

# impressions by sites
campaign1_lower %>%
  select(Partner_Platform, impressions) %>%
  ggplot() +
  geom_bar(aes(x = Partner_Platform)) +
  theme_linedraw()

# clicks by site
campaign1_lower %>%
  select(Partner_Platform, clicks) %>%
  ggplot() +
  geom_bar(aes(x = Partner_Platform)) +
  theme_linedraw()

# time series plot: impression
campaign1_lower %>%
  gather(key, value, impressions) %>%
  ggplot(aes(x = WeekStart, y = value, colour = key)) +
  geom_line()

# time series plot: clicks, visits, leads_2nd
campaign1_lower %>%
  gather(key, value, clicks, Visits, leads_2nd) %>%
  ggplot(aes(x = WeekStart, y = value, colour = key)) +
  geom_point()

# it is not linear, penalized by price
campaign1_lower %>%
  ggplot(aes(leads_2nd, conversions)) +
  geom_point()


campaign1_lower %>%
  ggplot(aes(clicks, leads_2nd)) +
  geom_point()

campaign1_lower %>%
  ggplot(aes(cost, leads_2nd)) +
  geom_point()

campaign1_lower %>%
  ggplot(aes(cost, conversions)) +
  geom_point()

# marginal ROIs
campaign1_lower <- mutate(campaign1_lower, leads_2nd_cumsum = cumsum(leads_2nd))
campaign1_lower <- mutate(campaign1_lower, conversions_cumsum = cumsum(conversions))
campaign1_lower <- mutate(campaign1_lower, clicks_cumsum = cumsum(clicks))
campaign1_lower <- mutate(campaign1_lower, cost_cumsum = cumsum(cost))
campaign1_lower <- mutate(campaign1_lower, visit_cumsum = cumsum(Visits))

# plots with cummulative values
campaign1_lower %>%
  ggplot(aes(cost_cumsum, clicks_cumsum)) +
  geom_line()

campaign1_lower %>%
  ggplot(aes(cost_cumsum, leads_2nd_cumsum)) +
  geom_line()

campaign1_lower %>%
  ggplot(aes(cost_cumsum, conversions_cumsum)) +
  geom_line()


campaign1_lower %>%
  ggplot(aes(cost_cumsum, visit_cumsum)) +
  geom_line()
