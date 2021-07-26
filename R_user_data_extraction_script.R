# Libraries and meta data---------------------------------------------------------------
library(lubridate)
library(googleAnalyticsR)
library(dplyr)

#ga_auth()

#collect list of fields and account IDs
metadata <- ga_meta()
account_list <- ga_account_list()
ga_id <- account_list$viewId[1]
#NICE.org.uk View ID for Cookie only view
viewId <- 229892739
#get goals details for the property
goallist <- ga_goal_list(accountId = 28620212,
                         webPropertyId = "UA-28620212-1",
                         viewId)

# API call ---------------------------------------------------------------
date_range <- c("2021-06-01", "2021-06-30")
#Get client ids, who visited a hst page, max 50
cids <- google_analytics(
    viewId,
    date_range = date_range,
    filter_clause_ga4(list(
        dim_filter("pagePath", "REGEXP", ".*\\/hst\\d+$")
    ),
    operator = "AND"),
    metrics = "sessions",
    dimensions = "clientId",
    anti_sample = TRUE,
    anti_sample_batches = 1
)
#Get user activity for first X clients
users <- ga_clientid_activity(cids$clientId,
                              viewId = viewId,
                              date_range = date_range)
#Extract from this pull the sessions and hits
hits <- users$hits
#add on goal info to the goal events
hits <-
    hits %>% left_join(
        hitsunnested %>% mutate(has_goal = TRUE),
        by = c("id", "activityTime", "has_goal", "sessionId"),
        keep = F
    )
#unlist the goal hits so we know what they are
hitsunnested <- ga_clientid_activity_unnest(hits,
                                            column = c("goals"))

# Recode data and hit sequence ---------------------------------------------------------------
#add event sequence so we can visualise what the group is doing
#Remove duplicate cookie banner event and goals 1 and 4
hits <-
    hits %>% filter(eventCategory != "Cookie banner" |
                        is.na(eventCategory)) %>%
    filter((goalIndex != 1 & goalIndex != 4) | is.na(goalIndex))
hits <-
    hits %>% arrange(activityTime, desc(activityType)) %>% group_by(sessionId) %>%
    mutate(eventseq = row_number())
#add session data into hits
hits <- left_join(hits, users$sessions, by='sessionId')

#Summary info --------------------------------------------------------------

#look at how many activities each user does on avg
hist(hits$eventseq)
summary(hits$eventseq)

#look at first activities - landing pages
hits %>% filter(eventseq == 1) %>% group_by(landingPagePath) %>% summarize(count = n())
#look at second activities - all the same goal
hits %>% filter(eventseq == 2) %>% group_by(activityType, eventCategory, goalName, pageTitle) %>% summarize(count = n())
#look at third activities - mix
hits %>% filter(eventseq == 3) %>% group_by(activityType, eventCategory, goalName, pageTitle) %>% summarize(count = n())
#look at fourth activities - mix
hits %>% filter(eventseq == 4) %>% group_by(activityType, eventCategory, goalName, pageTitle) %>% summarize(count = n())
#look at fifth activities - mix
hits %>% filter(eventseq == 5) %>% group_by(activityType, eventCategory, goalName, pageTitle) %>% summarize(count = n())

write.csv(hits[c("sessionId","activityTime","source","medium","channelGrouping","campaign","landingPagePath","activityType","pagePath","pageTitle","has_goal","eventCategory","eventAction","eventLabel","eventValue","eventCount","id.x","goalIndex","goalName","goalCompletionLocation","eventseq","deviceCategory","platform","dataSource","sessionDate")],"full_hst_hits_June-21.csv")