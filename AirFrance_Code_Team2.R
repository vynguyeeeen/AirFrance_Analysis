## PREPARE THE DATA 

#import Packages and Air France Dataset  

```{r, warning = FALSE}
#import packages 
library(readxl)             # data importing 
library(splitstackshape)    # data wrangling  
library(dplyr)              # data wrangling
library(ggplot2)            # data plotting 
library(plotly)             # data plotting
library(hrbrthemes)         # plot customizing
library(tidyr)              # tidy up code/ pivot tables 
library(gridExtra)          # ggplot on the same grid 
library(data.table)         # Data Cleaning 

#import dataset 
airfrance <- read_excel("Air France Case Spreadsheet Supplement.xlsx")
air_france <- as.data.frame(airfrance)
```

## DATA CLEAN UP 
air_france$match_factor <- as.numeric(as.factor(air_france$`Match Type`))
air_france$`Match Type`   <-as.factor(air_france$`Match Type`)
air_france$`Publisher Name` <- as.factor(air_france$`Publisher Name`)
sum(is.na(air_france$`Match Type`))

new_df <- air_france[ , c(2,5,12,13,14,15,16,17,18,19,20,21,22,23,24)]

sum(is.na(new_df))#to check that there are no missing values 



## MASSAGE THE DATA
#Create a new column to label Overture as Yahoo, and call it: 'New Publisher Name'
#Step 1: create a new column in dataset with an empty vector 
airfrance$`New Publisher Name` <- c()

#Step 2: loop through column 'Publisher Name', label Overture=Yahoo, place output in new column 'New Publisher Name'
for (i in 1:nrow(airfrance)){
  if (airfrance$`Publisher Name`[i] == 'Overture - US'){
    airfrance$`New Publisher Name`[i] <- 'Yahoo - US'
  }else if (airfrance$`Publisher Name`[i] == 'Overture - Global'){
    airfrance$`New Publisher Name`[i] <- 'Yahoo - Global'
  }else{
    airfrance$`New Publisher Name`[i] <- airfrance$`Publisher Name`[i]
  }
}
```

#Create a new column to group match types as 'Broad' or 'Exact':
#Google and MSN uses 'Broad' and 'Exact' to categorize keyword matching types. Yahoo uses 'Advanced' and 'Standard' for the same match type. To make things easier, group all labels into 2 types: Broad and Exact. 
#Step 1: create a new column in dataset with an empty vector 
airfrance$`New Match Type` <- c()

#Step 2: loop through column 'Match Type', label Advanced=Exact and Standard=Broad, and place output in new column 'New Match Type' 
for (i in 1:nrow(airfrance)){
  if (airfrance$`Match Type`[i] == 'Advanced'){
    airfrance$`New Match Type`[i] <- 'Exact'
  }else if (airfrance$`Match Type`[i] == 'Standard'){
    airfrance$`New Match Type`[i] <- 'Broad'
  }else{
    airfrance$`New Match Type`[i] <- airfrance$`Match Type`[i]
  }
}

#Create a new column in the dataset for Return on Asset (ROA):
#Step 1: find the Net Revenue 
airfrance$`Net Revenue` <- airfrance$Amount - airfrance$`Total Cost`

#Step 2: use Net Revenue to find ROA 
airfrance$ROA <- airfrance$`Net Revenue`/airfrance$`Total Cost` 


#Alternative, Creating ROA variable 
#Creating ROA variable 
new_df$ROA <- (new_df$Amount)/(new_df$`Total Cost`)

#Taking care of infinity values obtained by dividing by 0 
for (i in 1:nrow(new_df)){
  
  for (j in 1:ncol(new_df)){
    
    if(new_df[i,j] == Inf){
      
      new_df[i,j] <- 0 
      
    }#End of if statement 
  }#End of j loop 
}#End of i loop


#Business success 
ROA_total_average <-sum(new_df$Amount)/sum(new_df$`Total Cost`)

#1 = Above average ROA
#0-Below average ROA

for (i in 1:nrow(new_df)){
  
  
  if (new_df[i,"ROA"] > ROA_total_average){
    
    new_df$ROA_Success[i] <- 1
  }# End of if
  else if (new_df[i,"ROA"] < ROA_total_average){
    
    new_df$ROA_Success[i] <- 0
  }#End of Else 
}#End of i



## SAMPLING DATA

#Creating training data and testing data 

training_strat_testing <- stratified(as.data.frame(new_df),
                                     group=1 , size =0.8,
                                     bothSets = TRUE) #Randomly picking the data to run ana analyis on 

training_stratified_air <- training_strat_testing$SAMP1 
testing_stratified_air <- training_strat_testing$SAMP2


#Check columns 3 were added 
# print first 6 rows
head(airfrance) 



## KEYWROD MATCH TYPE PER PUBLISHER
#Pivot table to present summary 

#Between 4 Publishers, their 'new' Match Type and Average ROA: 
#Step 1: subset dataset to display positive ROA only (ROA greater than 0)
pivot_match <- subset(x=airfrance, subset = (airfrance$ROA > 0)) 

#Step 2: group table by 'Publisher Name' and 'New Match Type' 
pivot_match <- group_by(pivot_match, `New Publisher Name`, `New Match Type`)

#Step 3: summarize findings by 'Publisher Name', 'New Match Type' and ROA 
pivot_match <- summarise(pivot_match,
                         `Avg ROA` = mean(ROA))
print(pivot_match) #print pivot table 



### BOXPLOT 
For New Publisher, New Match Type and ROA: 

#remove N/A and subset for boxplot to show Broad & Exact Match Type per Publisher 
new_match_boxplotdata <- subset(x=airfrance, subset = ((airfrance$ROA > 0) & (airfrance$`New Match Type` != 'N/A')))

#make boxplot 
newmatch_boxplot <- ggplot(new_match_boxplotdata, aes(x=`New Match Type`, y=ROA, fill=`New Match Type`)) + 
  geom_boxplot(alpha=0.3) + facet_grid(.~`Publisher Name`) + 
  theme(legend.position = "none") +
  labs(title= "Keyword Match Type (Broad or Exact) Per Publisher", x= "Keyword Match Types",y="Return on Asset (ROA)") 

#make boxplot interactive 
ggplotly(newmatch_boxplot)



## LOGISTIC REGRESSION 
air_france_logit <- glm(ROA_Success ~ `Click Charges` + `Search Engine Bid`+ 
                          `Total Volume of Bookings`+match_factor,data= training_stratified_air)

summary(air_france_logit)



## COMPARISON OF PUBLISHERS 
#creating a pivot table 

#Totals Pivot
pivot_totals <- as_data_frame(training_stratified_air %>% 
                                select(`Publisher Name`, `Search Engine Bid`, Clicks, `Click Charges`,
                                       `Avg. Cost per Click`, Impressions,`Engine Click Thru %`,`Trans. Conv. %`,
                                       `Total Cost/ Trans.`,Amount,`Total Cost`,`Total Volume of Bookings`,`Match Type`)%>% 
                                group_by(`Publisher Name`,`Match Type`) %>% 
                                summarise(Total_Bids= sum(`Search Engine Bid`), Total_Clicks = sum(Clicks),
                                          Total_Click_Charges = sum(`Click Charges`), Total_Impressions = sum(Impressions),
                                          Total_Revenue = sum(Amount), Total_Cost = sum(`Total Cost`),
                                          Total_Bookings = sum(`Total Volume of Bookings`)))

pivot_totals$ROA <- pivot_totals$Total_Revenue/pivot_totals$Total_Cost

#Means Pivot 
pivot_means <- as.data.frame(training_stratified_air %>% 
                               select(`Publisher Name`, `Search Engine Bid`, Clicks, `Click Charges`,
                                      `Avg. Cost per Click`, Impressions,`Engine Click Thru %`,`Trans. Conv. %`,
                                      `Total Cost/ Trans.`,Amount,`Total Cost`,`Total Volume of Bookings`,`Trans. Conv. %`)%>% 
                               group_by(`Publisher Name`) %>% 
                               summarise(Average_Bids= mean(`Search Engine Bid`), Average_Clicks = mean(Clicks),
                                         Average_Click_Charges = mean(`Click Charges`), Average_Impressions = mean(Impressions),
                                         Average_Revenue = mean(Amount), Average_Cost = mean(`Total Cost`),
                                         Average_Bookings = mean(`Total Volume of Bookings`), Average_trans_conv = mean(`Trans. Conv. %`)))

pivot_means$ROA <- pivot_means$Average_Revenue/pivot_means$Average_Cost

mean(pivot_totals$ROA)


#Match type 
pivot_totals_match <- as_data_frame(training_stratified_air %>% 
                                      select( `Search Engine Bid`, Clicks, `Click Charges`,
                                              `Avg. Cost per Click`, Impressions,`Engine Click Thru %`,`Trans. Conv. %`,
                                              `Total Cost/ Trans.`,Amount,`Total Cost`,`Total Volume of Bookings`,`Match Type`)%>% 
                                      group_by(`Match Type`) %>% 
                                      summarise(Total_Bids= sum(`Search Engine Bid`), Total_Clicks = sum(Clicks),
                                                Total_Click_Charges = sum(`Click Charges`), Total_Impressions = sum(Impressions),
                                                Total_Revenue = sum(Amount), Total_Cost = sum(`Total Cost`),
                                                Total_Bookings = sum(`Total Volume of Bookings`)))

pivot_totals_match$ROA <- pivot_totals_match$Total_Revenue/pivot_totals_match$Total_Cost



## PLOTS FOR ANALYSIS OF PUBLISHER VS MATCH TYPE 
#Plots as visualizations of usage 

gg1 <-ggplot()+ 
  geom_histogram(data=as.data.frame(training_stratified_air[which(training_stratified_air[ , 1] == "Google - US")][ , 15]), aes(x=match_factor), bins = 3,fill="dark red")+#, color="red",alpha=0.4 )+
  labs(title= "Google US", x= "Match Type",y="Count")+
  annotate("text", x = 4, y = 200, label = "ROA = 4.1")+
  theme_classic()

gg2 <-ggplot()+ 
  geom_histogram(data=as.data.frame(training_stratified_air[which(training_stratified_air[ , 1] == "MSN - US")][ , 15]), aes(x=match_factor), bins = 5,fill="dark green")+#, color="green",alpha=0.4 )+
  labs(title= "MSN US", x= "Match Type",y="Count")+
  annotate("text", x = 1.5, y = 50, label = "ROA = 16.3")+
  theme_classic()


gg3 <-ggplot()+ 
  geom_histogram(data=as.data.frame(training_stratified_air[which(training_stratified_air[ , 1] == "Yahoo - US")][ , 15]), aes(x=match_factor), bins = 5,fill="dark green")+#, color="green")+#,alpha=0.4)+
  labs(title= "Yahoo US", x= "Match Type",y="Count")+
  annotate("text", x = 3, y = 200, label = "ROA = 12.0")+
  theme_classic()

grid.arrange(gg1,gg2,gg3)

ggplot()+
  geom_col(data=pivot_totals_match, aes(x=`Match Type`,y= ROA, fill = `Match Type`))



#Clicks, Click charges,Total Cost, Total Volume of bookings

air_france_amount <- lm(Amount ~ `Click Charges` + `Search Engine Bid`+ `Impressions`+
                          `Total Volume of Bookings`,data= training_stratified_air)
summary(air_france_amount)

air_france_amount_KPI <- lm(Amount ~ `Clicks` + `Impressions` + `Engine Click Thru %` + 
                              `Trans. Conv. %` + `Total Volume of Bookings`, data = training_stratified_air) 
summary(air_france_amount_KPI)

#the most significant from the marketing point og the KPI are Impressions, Total Volume of Bookings, 
#Clicks and the Trans. Conv. %

air_france_amount_KPI_Fin <- lm(`Total Cost` ~ `Avg. Cost per Click` + `Total Cost/ Trans.`, data = training_stratified_air) 
summary(air_france_amount_KPI_Fin)
#The most significant from financial prospective is Total cost/trans


## KAYAK COMPARISON
#load dataset
Air_France_Case_Spreadsheet_Supplement <- read_excel("Air France Case Spreadsheet Supplement.xls", 
                                                     sheet = "Kayak")
View(Air_France_Case_Spreadsheet_Supplement)

my_kayak_df <- as.data.frame(Air_France_Case_Spreadsheet_Supplement)

#Totals Pivot
pivot_totals_2 <- as_data_frame(training_stratified_air %>% 
                                  select(`Publisher Name`, `Search Engine Bid`, Clicks, `Click Charges`,
                                         `Avg. Cost per Click`, Impressions,`Engine Click Thru %`,`Trans. Conv. %`,
                                         `Total Cost/ Trans.`,Amount,`Total Cost`,`Total Volume of Bookings`,`Match Type`)%>% 
                                  group_by(`Publisher Name`) %>% 
                                  summarise(Total_Bids= sum(`Search Engine Bid`), Total_Clicks = sum(Clicks),
                                            Total_Click_Charges = sum(`Click Charges`), Total_Impressions = sum(Impressions),
                                            Total_Revenue = sum(Amount), Total_Cost = sum(`Total Cost`),
                                            Total_Bookings = sum(`Total Volume of Bookings`)))

pivot_totals$ROA <- pivot_totals$Total_Revenue/pivot_totals$Total_Cost


my_kayak_df<- na.omit(my_kayak_df)
names(my_kayak_df) <- my_kayak_df[1,]
my_kayak_df <- my_kayak_df[-1,]


my_kayak_df$Clicks <- as.numeric(my_kayak_df$Clicks)
my_kayak_df$`Total Bookings` <- as.numeric(my_kayak_df$`Total Bookings`)

my_kayak_df$conversionRate <- (my_kayak_df$`Total Bookings`/my_kayak_df$Clicks) * 100


rev_vector <- c(pivot_totals_2[,"Total_Revenue"],my_kayak_df$`Total Revenue`)
rev_vector <- unlist(rev_vector)
rev_vector <- as.data.frame(rev_vector)
colnames(rev_vector) <- c("Total Revenue")
rev_vector$Companies <- c("Google - Global","Google - US","MSN - Global","MSN - US","Overture - Global",
                          "Overture - US","Yahoo - US","Kayak")

rev_vector$`Total Revenue` <- as.numeric(rev_vector$`Total Revenue`)
rev_vector <- rev_vector[-c(5,6),]



b <-  ggplot(rev_vector, aes(x=Companies,y=`Total Revenue`))
b + geom_bar(stat = "identity",color="green") +labs(title = "Histogram of Total Revenue", x="Companies", y="Total Revenue ($)")


transactions <- c(pivot_means[,"Average_trans_conv"],my_kayak_df$conversionRate)
transactions <- as.data.frame(transactions)
colnames(transactions) <- c("rate")
transactions$Companies <- c("Google - Global","Google - US","MSN - Global","MSN - US","Overture - Global",
                            "Overture - US","Yahoo - US","Kayak")
transactions <- transactions[-c(5,6),]


b2 <-  ggplot(transactions, aes(x=Companies, y=rate, color="gold"))
b2 + geom_bar(stat="identity") + labs(title = "Histogram of Conversion Rate",x="Companies", y="Rate of Conversion (%)")


## KEYWORD COUNT 
#split all words in column 'Keyword'
split_words <- data.frame(table(unlist(strsplit(tolower(airfrance$`Keyword`), " "))))

#count frequency of each word 
count_keyword <- cSplit(airfrance, "Keyword", " ", "long")[, .N, tolower(`Keyword`)]

#sort keyword frequencies from largest to smallest 
sort_count_keyword <- count_keyword %>% 
  arrange(desc(N)) %>%          # desc orders from largest to smallest  
  select(tolower, N)            # select subsets the columns you want

#print top 10 keyword 
head(sort_count_keyword, 10)


