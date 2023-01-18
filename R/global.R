
#Packages and Setup
library(dplyr)
library(rsconnect)
library(dslabs)
library(ggplot2)
library(tidyverse)
library(micromapST)
library(ggpubr)

###############################################################################
########## MICROMAP TIME SERIES PLOT CODE

#Data and setup
cases <- read.csv(file = "cases.csv", check.names = FALSE)
cases <- cases %>%
  filter(countyFIPS != 0)
pop <- read.csv(file = "State_Pop_2019.csv")
load("fixDates.RData")
load("County_and_State_2019_Pop.RData")
nday <-12
daygap <- 29


#Fixing and selecting date in cases 
nams <- names(cases)
lastvar <- length(nams)
subs <- lastvar- daygap*((nday-1):0)
newnams <- c('FIPS','County','Postal',fixDates(nams[subs]))
subs <- c(1:3,subs)
cases <- cases[,subs]
names(cases) <- newnams


#Adding population data
workPop <-select(County_2019_Pop,FIPS,Pop)
names(workPop) <- c('FIPS','Pop')
cases <- left_join(cases,workPop,by='FIPS')


#Grouping county data by state
cases.state <- cases %>%
  group_by(Postal) %>%
  summarise("1" = sum(May_24_2020),
            "2" = sum(Jun_22_2020),
            "3" = sum(Jul_21_2020),
            "4" = sum(Aug_19_2020),
            "5" = sum(Sep_17_2020),
            "6" = sum(Oct_16_2020),
            "7" = sum(Nov_14_2020),
            "8" = sum(Dec_13_2020),
            "9" = sum(Jan_11_2021),
            "10"= sum(Feb_09_2021),
            "11" = sum(Mar_10_2021),
            "12" = sum(Apr_08_2021),
            "Pop" = sum(Pop)) 


#Creating population rate variable for each date grouped by state
#Renaming columns to P1:P12. Month 1 = May 24, 2020, ... Month 12 = April 8 2021
#Making postal the row names and removing date named columns for the P1:P12
cases.ts <- cases %>%
  group_by(Postal) %>%
  summarise(May_24_2020 = sum(May_24_2020),
            Jun_22_2020 = sum(Jun_22_2020),
            Jul_21_2020 = sum(Jul_21_2020),
            Aug_19_2020 = sum(Aug_19_2020),
            Sep_17_2020 = sum(Sep_17_2020),
            Oct_16_2020 = sum(Oct_16_2020),
            Nov_14_2020 = sum(Nov_14_2020),
            Dec_13_2020 = sum(Dec_13_2020),
            Jan_11_2021 = sum(Jan_11_2021),
            Feb_09_2021= sum(Feb_09_2021),
            Mar_10_2021 = sum(Mar_10_2021),
            Apr_08_2021 = sum(Apr_08_2021),
            "Pop" = sum(Pop)) 

cases.ts <- cases.ts %>%
  mutate(P1 = 100000*May_24_2020/Pop,
         P2 = 100000*Jun_22_2020/Pop,
         P3 = 100000*Jul_21_2020/Pop,
         P4 = 100000*Aug_19_2020/Pop,
         P5 = 100000*Sep_17_2020/Pop,
         P6 = 100000*Oct_16_2020/Pop,
         P7 = 100000*Nov_14_2020/Pop,
         P8 = 100000*Dec_13_2020/Pop,
         P9 = 100000*Jan_11_2021/Pop,
         P10 = 100000*Feb_09_2021/Pop,
         P11 = 100000*Mar_10_2021/Pop,
         P12 = 100000*Apr_08_2021/Pop) %>%
  mutate(P1 = round(P1),
         P2 = round(P2),
         P3 = round(P3),
         P4 = round(P4),
         P5 = round(P5),
         P6 = round(P6),
         P7 = round(P7),
         P8 = round(P8),
         P9 = round(P9),
         P10 = round(P10),
         P11 = round(P11),
         P12 = round(P12)) %>%
  select(!c(May_24_2020,Jun_22_2020,Jul_21_2020,
            Aug_19_2020,Sep_17_2020,Oct_16_2020,
            Nov_14_2020,Dec_13_2020,Jan_11_2021,
            Feb_09_2021,Mar_10_2021,Apr_08_2021,
            Pop))
postal <- cases.ts$Postal


#Making Dataframe with Row Names
cases.ts <- data.frame(cases.ts)
rownames(cases.ts) <-as.character(cases.ts$Postal)
cases.ts <- cases.ts %>%
  select(!Postal)


#Pivot_longer & rates for cases.state
nams <- names(cases.state)[2:(ncol(cases.state)-1)]
tmp.s <- pivot_longer(cases.state,all_of(nams),
                      names_to = "Days", values_to ="Counts")

state.cRates<- tmp.s %>% 
  mutate(Rates=100000*Counts/Pop) %>%
  select(!c(Pop,Counts)) %>%
  mutate(Rates = round(Rates))

state.cRates$Days <- as.integer(state.cRates$Days)
state.cRates <- data.frame(state.cRates)


#Array with state.cRates which from case.state 
daymat <- matrix(rep(1:12, times = 51),
                 nrow = 51,ncol = 12, byrow = T)
ratesmat <- as.matrix(cases.ts[,1:12])
workmat <- cbind(daymat,ratesmat)
tsdata <- NULL
tsdata <- array(workmat, dim=c(51,12,2))
rownames(tsdata) <-as.character(postal)
colnames(tsdata) <- as.character(c("P1","P2","P3","P4","P5",
                                   "P6","P7","P8","P9","P10",
                                   "P11","P12"))


#Linked Micromap paneldesc
paneldesc = data.frame(
  type = c('id','dot','ts','map'),
  lab1 = c('','Case Rate: April 8th 2021','12 Month Time Series',''),
  lab2 = c('','','Cases per 100,000',''),
  lab3 = c('','Rate per 100,000 People','Months (May 2020 - April 2021)',''),
  lab4 = c('','','Rate',''),
  col1 = c(NA, 12, NA, NA),
  panelData = c(NA,NA,'tsdata',NA))
#micromapst() found in sevrer of AppEC.R






##############################################################################
######### HT MAP CODE

#Data and setup
load('fixDates.RData')
load('County_and_State_2019_Pop.RData')
# Lines 66, 68, 172, 179 only change from original ht2.R
source("ht2.R")
case <- read.csv('cases.csv', header=TRUE, check.names = FALSE) %>%
  filter(countyFIPS != 0)


#Fixing and selecting date in case 
#Only selecting last observation (April 8 2021)
nday1 = 1
daygap1 = 28
nams <- names(case)
lastvar <- length(nams)
subs <- lastvar- daygap1*((nday1-1):0)
newnams <- c('FIPS','County','Postal',
             fixDates(nams[subs]))
subs <- c(1:3,subs)
case <- case[,subs]
names(case) <- newnams


#Adding population variable 
workPop <-select(County_2019_Pop,FIPS,Pop)
names(workPop) <- c('FIPS','Pop')
case <- left_join(case,workPop,by='FIPS')


#Adding state variable
workState <-select(County_2019_Pop,FIPS,State)
names(workState) <- c('FIPS','State')
case <- left_join(case,workState,by='FIPS')


#Grouping county data by state
case.state <- case %>%
  group_by(State) %>%
  summarise("Apr_08_2021" = sum(Apr_08_2021),
            Pop = sum(Pop))


# Turning case numbers into rates 
case.rate <- case.state %>%
  mutate(Apr2021 = 100000*Apr_08_2021/Pop)%>%
  select(!c(Apr_08_2021, Pop))


#Creating confidence intervals for the national rate for range of 'Similar'
#99% CI For National Rate (Normally Distribute): (8273.674, 10078.910)
t <- t.test(case.rate$Apr2021, conf.level = 0.99)

#The distribution is a bit skewed & mean and median are not close but n >30
#normal.check <- hist(case.rate$Apr2021)
#meancr<- mean(case.rate$Apr2021)
#mediancr <- median(case.rate$Apr2021)

#99% CI For National Rate (Non normally Distributed):(8536.699 10174.337)
wmw <- wilcox.test(case.rate$Apr2021,
            conf.int = TRUE,
            conf.level = 0.99)


# Creating final data frame with 2020 presidential election results.
# Also included is whether state case rate is below, similar, or above national avg.
case.rank <- case.rate %>% 
  mutate(Covid.Case.Rate = case_when(Apr2021 < 8273.674 ~ "Below National Average", 
                                     Apr2021 <= 10078.910 ~ "Similar to National Average",
                                     Apr2021 > 10078.910 ~ "Above National Average")) %>%
  select(State, Covid.Case.Rate)

Party <- c("r","r","f","r","d","d","d","d","d","r",
           "f","d","r","d","r","r","r","r","r","d",
           "d","d","d","d","r","r","r","r","d","d",
           "d","d","d","r","r","r","r","d","d","d",
           "r","r","r","r","r","d","d","d","r","f","r")

case.rank$Election.Results.2020 <- Party

case.rank <- case.rank %>%
  mutate(Election.Results.2020 = 
           case_when(endsWith(Election.Results.2020,"r") ~ "Republican",
                     endsWith(Election.Results.2020, "d") ~ "Democrat",
                     endsWith(Election.Results.2020, "f") ~ "Flipped Democrat")) 

case.rank$Election.Results.2020 <- as.factor(case.rank$Election.Results.2020)
case.rank$Covid.Case.Rate <- factor(case.rank$Covid.Case.Rate,
                                    levels = c("Below National Average",
                                               "Similar to National Average",
                                               "Above National Average"))
case.rank <- as.data.frame(case.rank)
title <- paste('COVID-19 Case Rate per 100,000 (January 2020 - April 2021)', 
               'Compared to National Average',
               sep = '\n' )
#Using the ht2 function with case.rank can be found in server of AppEC.R 





###############################################################################
########### ORIGINAL ASSIGNMENT 5B SUBMISSION

#Data and setup
deaths <- read.csv("deaths.csv", header=TRUE, check.names = FALSE)
population <- read.csv("State_Pop_2019.csv",header=TRUE, check.names = FALSE)


#Preparing data frame and adding 2020 presidential election results column
death <- select(deaths, -1,-2,-4)
death.state <- aggregate(.~State,data=death,FUN=sum)
names(population)[2] <- "State"
pop.death.state <-merge(population,death.state, by ="State")
pop.death.state <- pop.death.state[-c(8),]
Covid.Deaths <- select(pop.death.state, -3)
State <- Covid.Deaths$State
Political.Party <- c("r","r","r","d","d","d","d","d","r","d",
           "d","r","r","d","r","r","r","r","d","d",
           "d","d","d","r","r","r","r","d","d","d",
           "d","d","r","r","r","r","d","d","d","d",
           "r","r","r","r","d","d","d","r","d","r")
election <- data.frame(State,Political.Party)
Covid.Deaths.Election <- merge(election,Covid.Deaths, by ="State")


#Creating four visualizations for ggarange
One <- Covid.Deaths.Election %>%
  ggplot()+
  geom_histogram(mapping = aes(x=`4/1/2020`, y=..density..,
                               fill=Political.Party,
                               color=Political.Party),
                 alpha = .5)+
  labs(x="Covid Deaths by April 1st 2020")
Two <- Covid.Deaths.Election %>%
  ggplot()+
  geom_histogram(mapping = aes(x=`8/1/2020`, y=..density..,
                               fill=Political.Party,
                               color=Political.Party),
                 alpha = .5)+
  labs(x="Covid Deaths by August 1st 2020")
Three <- Covid.Deaths.Election %>%
  ggplot()+
  geom_histogram(mapping = aes(x=`12/1/2020`, y=..density..,
                               fill=Political.Party,
                               color=Political.Party),
                 alpha = .5)+
  labs(x="Covid Deaths by December 1st 2020")
Four <- Covid.Deaths.Election %>%
  ggplot()+
  geom_histogram(mapping = aes(x=`4/1/2021`, y=..density..,
                               fill=Political.Party,
                               color=Political.Party),
                 alpha = .5)+
  labs(x="Covid Deaths by April 1st 2021")

#Final step creating plot can be found server of AppEC.R 
tt <- "Visualizing COVID-19 Deaths by State Political Majority"
tgrob <- text_grob(tt, size=10)
Zero <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,0, "cm"))

##############################################################################

##### Data
dataState <-select(County_2019_Pop,FIPS,State)
names(dataState) <- c('FIPS','State')
data <- left_join(cases,dataState,by='FIPS')

data <- data %>%
  group_by(State) %>%
  summarise(May_24_2020 = sum(May_24_2020),
            Jun_22_2020 = sum(Jun_22_2020),
            Jul_21_2020 = sum(Jul_21_2020),
            Aug_19_2020 = sum(Aug_19_2020),
            Sep_17_2020 = sum(Sep_17_2020),
            Oct_16_2020 = sum(Oct_16_2020),
            Nov_14_2020 = sum(Nov_14_2020),
            Dec_13_2020 = sum(Dec_13_2020),
            Jan_11_2021 = sum(Jan_11_2021),
            Feb_09_2021= sum(Feb_09_2021),
            Mar_10_2021 = sum(Mar_10_2021),
            Apr_08_2021 = sum(Apr_08_2021),
            "Pop" = sum(Pop)) 

namsdata <- names(data)[2:(ncol(data)-1)]
tmp.d <- pivot_longer(data,all_of(namsdata),
                      names_to = "Date", values_to ="Case_Count")

c.rates <- tmp.d %>% 
  mutate(Rates=100000*Case_Count/Pop) %>%
  mutate(Case_Rate = round(Rates)) %>%
  mutate(Election_Majority = rep(c("Republican","Republican","Flipped","Republican","Democrat","Democrat","Democrat",
                                 "Democrat","Democrat","Republican","Flipped","Democrat","Republican","Democrat",
                                 "Republican","Republican","Republican","Republican","Republican","Democrat",
                                 "Democrat","Democrat","Flipped","Democrat","Republican","Republican","Republican",
                                 "Republican","Democrat","Democrat","Democrat","Democrat","Democrat","Republican",
                                 "Republican","Republican","Republican","Democrat","Flipped","Democrat","Republican",
                                 "Republican","Republican","Republican","Republican","Democrat","Democrat","Democrat",
                                 "Republican","Flipped","Republican"), each=12)) %>%
  select(! c(Pop,Rates))
c.rates <- c.rates[,c(2,1,3,4,5)]



