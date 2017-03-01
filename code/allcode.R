#setwd("/Users/frankiezeager/Documents/Graduate School/Spring 2017/Machine Learning/project 1/git/msds_policeshootings/data")
#setwd("/Users/jordanbaker/Documents/School/University of Virginia/Spring 2017/Machine Learning")

library(readr)
library(xlsx)
library(plyr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(rattle)

set.seed(12002)

#data sources
#washington post: https://github.com/washingtonpost/data-police-shootings
#lemas: https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/36164
#crimes: https://ucr.fbi.gov/crime-in-the-u.s/2015/crime-in-the-u.s.-2015/tables/table-69
#clearances: https://ucr.fbi.gov/crime-in-the-u.s/2015/crime-in-the-u.s.-2015/tables/table-26
#regions: https://ucr.fbi.gov/crime-in-the-u.s/2015/crime-in-the-u.s.-2015/tables/table-26/table-26/@@template-layout-view?override-view=data-declaration
#demo: https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk

#read in data
lemas<-da36164.0001
crime <- read.csv('fatal-police-shootings-data.csv',header=TRUE)
crimes <- read.xlsx("arrest-by-state.xls", 1)
clearances <- read.xlsx("clearance-rates.xls", 1)
regions <- read.xlsx("regions.xlsx", 1)
demo <- read_csv("demo-by-state.csv")

####### Data Cleaning ##########

### Washington Post Data ###
#aggregate WP data by state
#first convert categorical variables into dummy variable by level
for(level in unique(crime$manner_of_death)){
  crime[paste("manner_of_death", level, sep = "_")] <- ifelse(crime$manner_of_death == level, 1, 0)
}

crime$armed_bin<-ifelse(crime$armed=='unarmed',0,1)

for(level in unique(crime$gender)){
  crime[paste("gender", level, sep = "_")] <- ifelse(crime$gender == level, 1, 0)
}

for(level in unique(crime$race)){
  crime[paste("race", level, sep = "_")] <- ifelse(crime$race == level, 1, 0)
}

for(level in unique(crime$signs_of_mental_illness)){
  crime[paste("signs_of_mental_illness", level, sep = "_")] <- ifelse(crime$signs_of_mental_illness == level, 1, 0)
}

for(level in unique(crime$threat_level)){
  crime[paste("threat_level", level, sep = "_")] <- ifelse(crime$threat_level == level, 1, 0)
}

for(level in unique(crime$flee)){
  crime[paste("flee", level, sep = "_")] <- ifelse(crime$flee == level, 1, 0)
}

for(level in unique(crime$body_camera)){
  crime[paste("body_camera", level, sep = "_")] <- ifelse(crime$body_camera == level, 1, 0)
}

#group WP data by state
crime %>% group_by(state)%>% summarise(pct_shot=mean(manner_of_death_shot),pct_shot_and_tasered=mean(`manner_of_death_shot and Tasered`),
                                       pct_armed=mean(armed_bin),pct_male=mean(gender_M),pct_A=mean(race_A),pct_W=mean(race_W),
                                       pct_H=mean(race_H),pct_B=mean(race_B),pct_Other=(mean(race_O)+mean(race_N)+mean(race_)),
                                       pct_mental_illness=mean(signs_of_mental_illness_True), pct_threat_attack=mean(threat_level_attack),
                                       pct_flee=(1-mean(`flee_Not fleeing`)),pct_body_cam=mean(body_camera_True))->crime_by_state

#add number killed to WP data
crime %>% group_by(state) %>% summarise(num_killed=n())->num_killed
crime_by_state$num_killed<-num_killed$num_killed

### LEMAS Data ###
lemas %>% group_by(STATECODE)%>% summarise(population=sum(POP2012,na.rm=TRUE),total_number_police=sum(FTSWORN,na.rm=TRUE)+sum(PTSWORN,na.rm=TRUE),num_white_police=sum(PERS_FTS_WHT,na.rm=TRUE),num_black_police=sum(PERS_FTS_BLK, na.rm=TRUE),num_hsp_police=sum(PERS_FTS_HSP,na.rm=TRUE),num_native_police=sum(PERS_FTS_IND,na.rm=TRUE),num_asian_police=sum(PERS_FTS_ASN,na.rm=TRUE), num_hawaii_police=sum(PERS_FTS_HAW,na.rm=TRUE),num_biracial_police=sum(PERS_FTS_TWO,na.rm=TRUE),num_unknown_race_police=sum(PERS_FTS_UNK,na.rm=TRUE))->lemas_by_state

merged<-merge(crime_by_state,lemas_by_state,by.x='state',by.y='STATECODE')

#whites includes whites and hispanics (to be consistent with other data sources)
merged$pct_police_white<-(merged$num_white_police+merged$num_hsp_police)/merged$total_number_police
merged$pct_police_nonwhite<-(merged$num_asian_police+merged$num_black_police+merged$num_biracial_police+merged$num_hawaii_police+merged$num_native_police)/merged$total_number_police

### Crimes data ###

#keep the necessary columns for the crimes file
#change column names for the crimes file
keep <- c("State", "Violent.crime2", "Murder.and.nonnegligent.manslaughter", "X2015.estimated..population")
crimes <- crimes[keep]
names <- c("State", "ViolentCrime", "Murder", "Population")
colnames(crimes) <- names

#create the violence rate and murder rate features
#violence rate = # of violent crimes / population
#murder rate = # of murders / population
crimes$ViolenceRate <- (crimes$ViolentCrime/crimes$Population)
crimes$MurderRate <- (crimes$Murder/crimes$Population)

### Clearance Rate Data ###

#keep the necessary columns for the clearances file
#change column names for the clearances file
keep <- c("Geographic.region.division", "Violent.crime", "Murder.and.nonnegligent.manslaughter")
clearances <- clearances[keep]
names <- c("Region", "ViolentCrime", "Murder")
colnames(clearances) <- names

#assign each state a clearance rate based on region
stateclearances <- join(regions, clearances, by="Region")
colnames(stateclearances) <- c('State', 'Region', 'ViolentClearance', 'MurderClearance')

#join the crimes and stateclearances files
#pull only the columns we care about
crimes <- crimes[c('State', 'ViolenceRate', 'MurderRate')]
stateclearances <- stateclearances[c('State', 'ViolentClearance', 'MurderClearance')]
demo <- demo[c('State', 'White', 'Non-White', 'Population')]
aggstate <- join(crimes, stateclearances, by="State")
aggstate <- join(aggstate, demo, by="State")

#change column names to lowercase
colnames(aggstate) <- c("state", "violence_rate", "murder_rate", "violent_clearance", "murder_clearance", "white", "non_white", "statepop")

#join aggstate with all other data
police <- merge(aggstate, merged, by='state')

#drop dc bc the data came from the metro police, which for some reason included 0 murders
police <- subset(police, police$state != 'DC')


#define diversity (is the police dept representative of the population demographics?)
police$diversity <- abs(police$pct_police_white - police$white)

#define killrate
police$killrate <- police$num_killed/police$statepop
hist(police$killrate) #the killrate looks skewed to the right, but there appears to be two clusters

#k-means cluster police kill rates into two clusters (high and low)
clusters <- kmeans(police$killrate, centers = 2)
clusters

#assign high, medium, and low kill rate categories
police$rank <- clusters$cluster
police$rank <- ifelse(police$rank ==  1,'high', 'low')

####### Model Building #######

### Decision Tree ###
#cross validation (not enough data for validation set, so using CV accuracy)
formula=as.formula(rank~violence_rate+murder_rate+violent_clearance+murder_clearance+non_white+pct_armed+
                     pct_mental_illness+pct_threat_attack+
                     pct_flee+pct_body_cam+num_killed+diversity)
trained_model<-train(formula,police,method='rpart')

#look at tree
#in some instances you will have trouble generating the plot of the tree if the final tree is a root node
#in these cases, just reset the seed until the actual tree is generated
fancyRpartPlot(trained_model$finalModel)

#text output of tree
#1) root 50 9 low (0.1800000 0.8200000)  
#  2) murder_clearance>=67.05 8 3 high (0.6250000 0.3750000) *
#  3) murder_clearance< 67.05 42 4 low (0.0952381 0.9047619) *

#check out performance 
summary(trained_model$finalModel)
printcp(trained_model$finalModel)
#variable importance
#murder_clearance     pct_body_cam 
#89                      11 

#root node error: 9/50=.18, so accuracy =1-.18=.82


#### T-test ####

#identify states that may be under pressure due to low clearance rates
police$pressure <- ifelse(police$murder_clearance > mean(police$murder_clearance),'not pressured', 'pressured')

#create the pressured and not pressured samples
pressured <- police[police$pressure == 'pressured',]
notpressured <- police[police$pressure == 'not pressured', ]

#test the hypothesis that pressured offers have higher killrates than officers who arent pressured
t.test(pressured$killrate, notpressured$killrate, alternative="greater", paired=FALSE)

#result=pressured officers do not have statistically higher killrates than non-pressured officers
#t=-2.2717
#df=47.142
#p-value=0.9861
#mean of x=5.784390e-06
#mena of y=8.000415e-06

#export police file
write.xlsx(police, "police_agg.xlsx")
