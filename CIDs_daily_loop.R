library(googleAnalyticsR)
ga_auth()
ga_id <- 1234
dates <- seq.Date(as.Date("2019-01-01"),as.Date("2019-12-31"),by=1)
strDates <- format(dates,format="%Y-%m-%d")
#define the segment using the ID (can be found in the query explorer)
seg <- segment_ga4("Accounts sign-in", "gaid::r1649o_gSbiojXEW6w6e5Q")
df_total = data.frame()
#manual loop through dates required as anti_sample doesn't seem to work with the query (due to using a segment?)
for (d in strDates){
    print(d)
    testData1 <- google_analytics(ga_id,
                                  date_range = c(d,d),
                                  metrics = "sessions",
                                  dimensions = "clientId",
                                  segment=seg)
    df_total <- rbind(df_total,testData1)
}
