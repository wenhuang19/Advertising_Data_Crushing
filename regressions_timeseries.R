require("reshape2")
require("dplyr")
require("stringr")
require("lubridate")
require("forecast")
require("tseries")

#read file
social <- read.csv("dailySocialData")
social$Date <- as.Date(social$Date, "%Y-%m-%d")


#summerization of dataset
summary(social)

# check NA
sum(is.na(social))

#get PPA and Virtual wallet
unique(socialTopline$HighLevelCampaign)

# campaing1 lower only
campaing1_lower <- filter(social, HighLevelCampaign == "campaing1_lower")

# check performance on Creative
creative_lm <- lm(appStarts ~ cost + Creative.Group - 1, data = campaing1_lower)
summary(creative_lm)

# not significant, conversion is affected by multiple factors
cost_lm <- lm(conversions ~ cost,data = campaing1_lower)
summary(cost_lm)

# this is much clearer than conversion
cost_lm <- lm(appStarts ~ cost,data = campaing1_lower)
summary(cost_lm)


# campaign with monthly ads
campaign2 <- filter(social, HighLevelCampaign == "campaign2_webinar")

campaign2_imp <- ts(data = campaign2$impressions,
                  start = 1,
                  end = nrow(campaign2),
                  frequency = 1)
# seasonality?
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

