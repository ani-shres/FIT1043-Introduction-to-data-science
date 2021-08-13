getwd()
setwd("D:/FIT1043tutorial/ass3")
getwd()
install.packages("dplyr")
library(dplyr) 

#Q7"the-wall-street-journal" has asked you to focus on the analysis of the post_types forwhich the number of comments is less than 4000. You need to read the "the-wall-street-journal.csv" file generated as mentioned above into R, filter the data based on therequirements of "the-wall-street-journal", and draw boxplots to show the distributionof comments made against each type of post (event, link, photo, status and video). Youneed to present one plot which contains different boxplots for different post types. Whatcan you infer from this plot? Can you detect which one is the most engaging post type?Make sure that your plot has proper labels and a title.

wallstreetJ=  read.csv("wall-street-journal.csv" )
head(wallstreetJ)

#creating a dataframe with post types having less than 4000 comments 
wallstreetJ <- wallstreetJ[(wallstreetJ$comments_count< 4000),]
head(wallstreetJ)

# ordering dataset in a asscending order by comments_counts
wallstreetJ = wallstreetJ[order(wallstreetJ$comments_count, decreasing = TRUE),]
head(wallstreetJ)

#creating a boxplot 
boxplot(wallstreetJ$comments_count
         ~ wallstreetJ$post_type,
        main="Boxplot of Comment Count of Post Types",
        xlab="Post Type",
        ylab="Comment Count")


#Q8 You may have noticed that the presence of outliers affects the readability andinterpretation of the data in the box plots. Redraw the boxplot by filtering out values(comments_count) greater than 1000.
filter_wsj = wallstreetJ[(wallstreetJ$comments_count<= 1000),]
head(filter_wsj)
#creating a boxplot 
boxplot(filter_wsj$comments_count
        ~ filter_wsj$post_type,
        main="Boxplot of Comment Count of Post Types",
        xlab="Post Type",
        ylab="Comment Count")

#b2

#Q10 Create a bar chart which shows the total number of reactions to the posts published by"abc-news", in which the posted message contains the term "Donald Trump"(ignorethe case) for each day of the week ('Monday', 'Tuesday', 'Wednesday', 'Thursday','Friday', 'Saturday', 'Sunday'). Make sure the bar chart is sorted based on the weekdaysas shown in the screenshot below. Understanding what should be considered as areaction is a part of the answer to this question which you can figure out by checkingdifferent columns of your dataset. You need to mention and justify the criterion whichyou choose to define a reaction to a post. (Please pay attention that the plot does notshow the real values and it is created with fake data just to show you how the outputshould be).
library(lubridate)

abc_news = read.csv("abc_news_d.csv")


head(abc_news)

filter_abc_news = abc_news[(grep("Donald Trump",abc_news$message,ignore.case = TRUE)),]
head(filter_abc_news)
filter_abc_news = filter_abc_news[c(2,4:11)]
head(filter_abc_news)
filter_abc_news$total_reactions = rowSums(filter_abc_news[,2:8])
head(filter_abc_news)


filter_abc_news$date=  as.Date(filter_abc_news$posted_at,format("%d/%m/%Y %H:%M"))
filter_abc_news$time=  format(as_datetime(filter_abc_news$posted_at,format("%d/%m/%Y %H:%M")),format="%H:%M")
head(filter_abc_news)
class(filter_abc_news$time)
filter_abc_news$weekdays = weekdays(filter_abc_news$date)
head(filter_abc_news)

#https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
weekdays_treaction = aggregate(filter_abc_news$total_reactions,
                               by=list(Weekdays=filter_abc_news$weekdays), FUN=sum)
data.frame(weekdays_treaction)
names(weekdays_treaction)[names(weekdays_treaction) == "x"]="total_reaction"
data.frame(weekdays_treaction)


#https://stackoverflow.com/questions/10309564/reorder-factor-levels-by-day-of-the-week-in-r
weekdays_treaction$Weekdays = ordered(weekdays_treaction$Weekdays,
                                       levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                         "Friday", "Saturday", "Sunday"))


weekdays_treaction=weekdays_treaction[order(weekdays_treaction$Weekdays), ]

barplot(weekdays_treaction$total_reaction~weekdays_treaction$Weekdays,  col="blue",
        main="Total daily Reactions",
        xlab="Days",
        ylab="Reaction count" )


#Q12 We need to take a closer look at the total reactions in the two days which users have shown the most reactions. Create two bar charts to show the hourly total reactions for each of two days. What time did the most reactions happen on each day? Is there any similarity between the number of hourly reactions in these two days? (Please pay attention that the sample plot given below does not show the real values and it is created with fake data just to show you how the output should be presented for this question.)

#creating a dataset where weekday is Tuesday 
tuesday_tr = filter_abc_news[(filter_abc_news$weekdays=="Tuesday"),]
head(tuesday_tr)

#creating a dataset where weekday is Sunday 
sun_tr = filter_abc_news[(filter_abc_news$weekdays=="Sunday"),]
head(sun_tr)

tuesday_tr$hours = format(as_datetime(tuesday_tr$posted_at,
                           "%d/%m/%Y %H:%M") ,
       format = "%H")
head(tuesday_tr)

sun_tr$hours = format(as_datetime(sun_tr$posted_at,
                                  "%d/%m/%Y %H:%M"),
                      format = "%H")
head(sun_tr)


class(tuesday_tr$hours)
tuesday_tr$hours= as.integer(tuesday_tr$hours)
class(tuesday_tr$hours)

sun_tr$hours= as.integer(sun_tr$hours)
class(sun_tr$hours)


tues_h_treaction = aggregate(tuesday_tr$total_reactions,
                               by=list(hours=tuesday_tr$hours), FUN=sum)
names(tues_h_treaction)[names(tues_h_treaction) == "x"]="total_reaction"
head(tues_h_treaction,10)

sun_h_treaction = aggregate(sun_tr$total_reactions,
                            by=list(hours=sun_tr$hours), FUN=sum)
names(sun_h_treaction)[names(sun_h_treaction) == "x"]="total_reaction"
head(sun_h_treaction,10)

df = data.frame(hours =0:24)
head(df)
class(df$hours)

final_tues_tr = full_join(df,tues_h_treaction)
head(final_tues_tr,10)

final_sun_tr = full_join(df,sun_h_treaction)
tail(final_sun_tr,10)


final_tues_tr[is.na(final_tues_tr)] = 0
head(final_tues_tr,10)

final_sun_tr[is.na(final_sun_tr)] = 0
tail(final_sun_tr,10)

barplot(final_tues_tr$total_reaction~final_tues_tr$hours,  col="blue",
        main="Total number of Reactions in Day Tuesday",
        xlab="Hours",
        ylab="Total Reactions" )





barplot(final_sun_tr$total_reaction~final_sun_tr$hours,  col="blue",
        main="Total number of Reactions in Day Sunday",
        xlab="Hours",
        ylab="Total Reactions" )
