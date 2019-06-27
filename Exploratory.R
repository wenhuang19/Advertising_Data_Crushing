require("reshape2")
require("dplyr")
require("stringr")
require("lubridate")
require("ggplot2")
require("knitr")
require("ggrepel")
require("gplots")
require("forecast")
require("tseries")
require("tidyr")

#read file
social <- read.csv("$dailySocialData")
social$Date <- as.Date(social$Date, "%Y-%m-%d")

#summerization of dataset
summary(social)

# check NA
sum(is.na(social))

#get PPA and Virtual wallet
unique(social$HighLevelCampaign)


# campaign1 lower only
campaign1_lower <- filter(social, HighLevelCampaign == "campaign1_lower")
range(campaign1_lower$Date)

campaign1_lower %>%
  select(Partner_Platform, impressions) %>%
  ggplot() +
  geom_bar(aes(x = Partner_Platform)) +
  theme_linedraw() +
  ggtitle("XXX")

ggplot(data = campaign1_lower[, c('Date', 'cost', 'impressions')],
       aes(x=Date, y = value, colours = variable)) +
       geom_line()


campaign1_lower %>%
  gather(key, value, impressions) %>%
  ggplot(aes(x = Date, y = value, colour = key)) +
  geom_line()

campaign1_lower %>%
  gather(key, value, cost) %>%
  ggplot(aes(x = Date, y = value, colour = key)) +
  geom_line()

campaign1_lower %>%
  gather(key, value, clicks) %>%
  ggplot(aes(x = Date, y = value, colour = key)) +
  geom_line()

campaign1_lower %>%
  ggplot(aes(leads_2nd, conversions)) +
  geom_point()

campaign1_lower <- mutate(campaign1_lower, leads_2nd_cumsum = cumsum(leads_2nd))
campaign1_lower <- mutate(campaign1_lower, conversions_cumsum = cumsum(conversions))
campaign1_lower <- mutate(campaign1_lower, clicks_cumsum = cumsum(clicks))
campaign1_lower <- mutate(campaign1_lower, cost_cumsum = cumsum(cost))

campaign1_lower %>%
  ggplot(aes(cost_cumsum, clicks_cumsum)) +
  geom_line()

campaign1_lower %>%
  ggplot(aes(cost_cumsum, leads_2nd_cumsum)) +
  geom_line()

campaign1_lower %>%
  ggplot(aes(cost_cumsum, conversions_cumsum)) +
  geom_line()

# fatal_fe_lm_mod <- lm(fatal_rate ~ beertax + state - 1, data = Fatalities)
x <- lm(leads_2nd ~ cost + Creative.Group - 1, data = campaign1_lower)
xx <- lm(conversions ~ cost,data = campaign1_lower)

sort(colnames(campaign1_lower))
campaign1_lower %>%
  ggplot(aes(leads_2nd, clicks)) +
  geom_point()

campaign1_lower %>%
  ggplot(aes(conversions, clicks)) +
  geom_point()

# no app complete until clicks are somewhat large
# so, the curve model?
campaign1_lower %>%
  ggplot(aes(conversions, clicks)) +
  geom_point()


campaign1_lower %>%
  ggplot(aes(cost, clicks)) +
  geom_point()

# bao, appComplete up, cost up, then down as cost gets higher
campaign1_lower %>%
  ggplot(aes(cost, conversions)) +
  geom_point()

# obviously linear
campaign1_lower %>%
  ggplot(aes(cost, leads_2nd)) +
  geom_point()

# hard to visulized
social %>%
  filter(HighLevelCampaign == "campaign2_webinar") %>%
  ggplot() +
  geom_line(aes(x = Date, y = clicks, col = Creative.Group)) +
  theme_linedraw()
  
social %>%
  filter(HighLevelCampaign == "campaign2_webinar") %>%
  ggplot() +
  geom_line(aes(x = Date, y = clicks)) +
  theme_linedraw()

social %>%
  filter(HighLevelCampaign == "campaign2_webinar") %>%
  ggplot() +
  geom_line(aes(x = Date, y = leads)) +
  theme_linedraw()

social %>%
  filter(HighLevelCampaign == "campaign2_webinar") %>%
  dplyr::select(clicks, impressions, leads, cost, Date) %>%
  distinct() %>%
  arrange(Date)

# num rows
campaign2 <- filter(social, HighLevelCampaign == "campaign2_webinar")

campaign2_imp <- ts(data = campaign2$impressions,
                 start = 1,
                 end = nrow(campaign2),
                 frequency = 1)
plot(campaign2_imp)

# Augmented Dickey-Fuller Test
adf.test(x = campaign2_imp)


campaign2_clicks <- ts(data = campaign2$clicks,
                  start = 1,
                  end = nrow(campaign2),
                  frequency = 1)
plot(campaign2_clicks)

# Augmented Dickey-Fuller Test
adf.test(x = campaign2_clicks)


campaign2_cost <- ts(data = campaign2$cost,
                     start = 1,
                     end = nrow(campaign2),
                     frequency = 1)
plot(campaign2_cost)

# Augmented Dickey-Fuller Test
adf.test(x = campaign2_cost)


campaign2_leads <- ts(data = campaign2$leads,
                   start = 1,
                   end = nrow(campaign2),
                   frequency = 1)
plot(campaign2_leads)

# Augmented Dickey-Fuller Test
adf.test(x = campaign2_leads)

kpss.test(x = campaign2_cost)
kpss.test(x = campaign2_clicks)
kpss.test(x = campaign2_imp)
kpss.test(x = campaign2_leads)

