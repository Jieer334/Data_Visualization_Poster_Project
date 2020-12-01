
# IST719
# Final Poster
# JIEER CHEN 
# Google Play Store App

# [library packages] 
library(tidyverse) 
library(dplyr)
library(xts)
library(readr)
library(plyr)

library(stringr)

library(RColorBrewer)
library(treemap)
library(ggplot2)
library(ggthemes) 

library(base)
library(lubridate) # date

# [read data in] 
go.df <- read.csv('/Users/jieerchen/Desktop/2019 semester2/IST719 Data Visualization /final project/google-play-store-apps/googleplaystore.csv'
                  ,stringsAsFactors = FALSE
                  ,header = TRUE)



# [look at data]
# 'data.frame':	10841 obs. of  13 variables
str(go.df)
head(go.df,3)

# [Data Cleaning]
# turn of scietific notation 
options(scipen = 999)
# [Data Type]
# Installs: chr  "10,000+"
# Eliminate some characters to transform Installs to numeric
go.df$Installs <- gsub("\\+", "", go.df$Installs)
go.df$Installs <- gsub(",", "", go.df$Installs)
go.df$Installs <- as.numeric(go.df$Installs)
head(go.df$Installs,5)
# Size(unit:M): chr  "19M" 
# Eliminate M to transform Size to numeric
go.df$Size = gsub("M", "", go.df$Size)
# Replace cells with k to 0 since it is < 1MB
go.df$Size = ifelse(grepl("k", go.df$Size), 0, as.numeric(go.df$Size))
head(go.df$Size,5)
# Reviews: chr  "159" 
# Transform reviews to numeric
go.df$Reviews = as.numeric(go.df$Reviews)
# Price: chr
# Remove currency symbol from Price, change it to numeric
go.df$Price = as.numeric(gsub("\\$", "", as.character(go.df$Price)))
# Last.Updated: chr  "January 7, 2018"
# Last Updated to date format
go.df$Last.Updated = dmy(go.df$Last.Updated)
which(is.na(go.df$Last.Updated))
# Replace "Varies with device" to NA since it is unknown
go.df$Min.Android.Ver <- gsub("Varies with device", NA, go.df$Android.Ver)
# Keep only version number to 1 decimal
go.df$Min.Android.Ver <- as.numeric(substr(go.df$Min.Android.Ver, start = 1, stop = 3))
# Drop old Android version column
go.df$Android.Ver = NULL
# Two apps had type as 0 or NA, they will be removed 
go.df$Type
go.df <- go.df %>% filter(go.df$Type %in% c("Free", "Paid"))
#[Duplicate Data] 
# check if there are any duplicate rows 
nrow(go.df %>% distinct())
# remove duplicate rows
go.df <- go.df %>% distinct()
# 'data.frame':	10356 obs. of  13 variables:
str(go.df)
head(go.df,3) 
# [NA values]
naN<-rep(0,13)
for (i in 1:ncol(go.df)){
  naN[i]<-sum(is.na(go.df[,i]))
}
naN
colnames(go.df[c(3,5,13)])

View(go.df)

# Graph1: pie chart
# [Percentage of paid vs free app]
per_type <- prop.table(table(go.df$Type))
per_type
df <- data.frame(table(go.df$Type))
colnames(df) <- c('Type','Freq')
df$Perc <- round(df$Freq / sum(df$Freq) * 100)
df
# simple pie chart 
per_label = c("Free:92.6%",'Paid:7.4%')
pie(table(go.df$Type),
    main = "Percentage of paid vs free app",
    col=c('skyblue1','red'),
    labels = per_label,
    border = NA)

# Graph2: density plot
# [Distribution of size]
# Non-NA values:8831
go.df %>% ggplot()+
  aes(x=go.df$Size)+ 
  geom_density(fill="skyblue1",col="skyblue4",alpha=0.6,na.rm = TRUE,size=0.6)+
  ggtitle("Distribution of Application Size(in MB)") +
  geom_vline(data=go.df,aes(xintercept=mean(go.df$Size)),size=1)

count(is.na(go.df$Size))
1525/(1525+8831)
str(go.df)
geom_vline(data=cdat, aes(xintercept=rating.mean),
           linetype="dashed", size=1)


# Graph3: Treemap
# [Popular categories by number of counts]
cc <- data.frame(table(go.df$Category))
cc<- cc[order(cc$Freq,decreasing = TRUE),]
cc$Var1 <- factor(cc$Var1)
cc
cc10 <- cc[1:10,]
num.cols <- 10
FUN <- colorRampPalette(c("#eef8fe","#6BC4ED"))
my.pal <- FUN(num.cols)
treemap(cc10, index = c("Var1",'Freq')
        ,vSize = "Freq"
        ,palette = my.pal
        ,border.col = "white"
        ,border.lwds = 0.5
        ,type = "index")

# Graph4: points 
# App ratings vs Number of reviews vs Type 
ggplot(go.df) + 
  aes(x=go.df$Reviews,y=go.df$Rating)+
  scale_x_continuous(trans='log10')+
  geom_point(aes(col=Type),alpha = 0.8) + 
  labs(title = "Android App Ratings vs Number of Reviews"
       , y = "Rating from 1 to 5 stars"
       , x = "number of reviews") +
  theme_linedraw() + facet_wrap(~go.df$Type)

# [Graph5] 
# check if the Android application Last Updated date might affect the number of reviews the app recieves
# the Last Updated has been changed into a date value 
# create a new column of month
head(go.df$Last.Updated)
go.df$Month <- format(go.df$Last.Updated,"%b")
go.df$Month
table(go.df$Month)
# creat a factor 'months'
# so that the x axis of the plot will be in chronological order, rather than the default alphabetical.
months <- factor(go.df$Month,level=c(
  'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
# create a faceted boxplot 
ggplot(go.df) +
  aes(x=months,y=Reviews)+
  scale_y_continuous(trans="log10")+
  geom_boxplot(fill="skyblue") +
  labs(title = "Number of Android app reviews based on month it was last updated",
       y = "Number of Reviews",
       x = "Month it was Updated") +
  theme_linedraw()+
  facet_wrap(~go.df$Type) + 
  scale_fill_brewer(palette="Blues") 

# Graph6:line chart 
# Andriod App Ratings vs Number of Reviews 
go.df$Reviews <- as.numeric(go.df$Reviews)
go.df %>% 
  filter(!is.na(log(Reviews))) %>% 
  ggplot(mapping = aes(x = log(Reviews), y = Rating)) + 
  geom_smooth(method = "lm") + 
  ggtitle("Relation between the log of the reviews received with the rating for the app") + 
  xlab("Log of the reviews received")

# Graph7: line and point chart 
# average number of app revewis and number of updated 

go.df %>% group_by(Month) %>% summarize(Reviews=mean(Reviews))

gd <- go.df %>% group_by(Month) %>% summarize(Reviews=mean(Reviews))
ggplot(go.df, aes(x=Month, y=Reviews, group=12)) +
  scale_y_continuous(trans='log10') +
  geom_point(data=gd, color="skyblue") +
  geom_line(data=gd, color="skyblue") +
  scale_x_discrete(limits = month.abb) +
  labs(title = "Average number of Android app reviews and month updated",
       y = "Number of Reviews",
       x = "Month it was Updated") + 
  theme_linedraw()


# Graph8: bpxlot 
# Rating vs. Type
# Paid apps are slightly better rated than free apps. 
#plot for rating vs. type
ggplot(aes(x = Type, y = Rating), data = go.df )+
  geom_boxplot(fill = 'violetred2' )+
  ggtitle('Rating vs. Type')
#summary for the paid
summary(subset(df, Type == 'Paid')$Rating)
#summary
summary(subset(df, Type == 'Free')$Rating)
