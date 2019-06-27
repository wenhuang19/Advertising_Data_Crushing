require("reshape2")
require("dplyr")
require("stringr")
require("lubridate")
require("rpart")
require("rpart.plot")
require("party")
require("randomForest")

# Decision Tree on Leads based on weekly data

#read file
socialTopline <- read.csv("$socialDataFile")
socialTopline$WeekStart <- as.Date(socialTopline$weekStart, "%Y-%m-%d")

#summerization of dataset
summary(socialTopline)

# check NA
sum(is.na(socialTopline))

#get PPA and Virtual wallet
unique(socialTopline$HighLevelCampaign)

# campaign1 lower only
campaign1_lower <- filter(socialTopline, HighLevelCampaign == "campaign1_lower")

# selecting only the userful columns
sort(colnames(campaign1_lower))
campaign1_lower_lite <- campaign1_lower[, c("Ad.Format", "conversions", "lead_2nd",
                              "cost", "Creative.Group", "clicks", "Target",
                              "impressions", "Partner_Platform")]

campaign1_lower_lite$ac_cato <- "ZERO"
campaign1_lower_lite$ac_cato[campaign1_lower_lite$conversions > 0 & campaign1_lower$conversions < 1] <- "< ONE"
campaign1_lower_lite$ac_cato[campaign1_lower_lite$conversions >= 1 ] <- "ONE or MORE"

# rf_campaign1_lower = randomForest(ac_cato ~ ., data = campaign1_lower_lite)
campaign1_lower_lite$conversions <- NULL
.data <- c("training", "test") %>%
  sample(nrow(campaign1_lower_lite), replace = T) %>%
  split(campaign1_lower_lite, .)

appCompleteTree_cato <- rpart(ac_cato ~ ., data = .data$training)
prp(appCompleteTree_cato)
summary(appCompleteTree)
