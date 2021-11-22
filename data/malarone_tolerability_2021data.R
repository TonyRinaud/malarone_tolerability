# Routine clean-up upon start
rm(list=ls())

ring_list <- read.csv2('./Buzzard ringing list condensed 2021-11-18 MO_reduced.csv', h=T, sep=';', dec=".", colClasses = c("factor","character","factor","factor","factor","factor","factor","factor","factor","numeric","numeric","factor")) 

ring_list$Date=chron(as.character(ring_list$Date), format="d/m/y")

## Some filters

ring_list=ring_list[ring_list$Year %in% c("2016","2018", "2019", "2020"),]

ring_list=ring_list[ring_list$Age %in% 'juvenile',]

ring_list=ring_list[!is.na(ring_list$Weight),]

## ordering by Ring.no and date

ring_list=ring_list[order(ring_list$Ring.no, ring_list$Date),]

# adding sampling rank

ring_list$sampling_rank=NA
ring_list$sampling_rank[1]=1

for (i in 1:length(ring_list$Ring.no)){
  ifelse(ring_list$Ring.no[i+1]==ring_list$Ring.no[i], ring_list$sampling_rank[i+1] <- ring_list$sampling_rank[i]+1, ring_list$sampling_rank[i+1] <- 1)
}

# Addind Treatments

ring_list$Treatments=NA

for (i in 1:(length(ring_list$Treatm)-1)){
  
  if(ring_list$Ring.no[i] != ring_list$Ring.no[i+1]){next}
  if(ring_list$sampling_rank[i] %in% 1){
    
    if(is.na(ring_list$Treatm[i]==T) | ring_list$Treatm[i] %in% "none"){
      ring_list$Treatments[i]="Control"
      ring_list$Treatments[i+1]="Control"
    }
    if(ring_list$Treatm[i] %in% c('control','Control','green -', 'Green -', 'green +', 'Green +',  'Plastic +')){
    ring_list$Treatments[i]="Control"
    ring_list$Treatments[i+1]="Control"
    }
    
    if(ring_list$Treatm[i] %in% c('Malarone +', 'Green -/Malarone +','mal')){
    ring_list$Treatments[i]="Malarone"
    ring_list$Treatments[i+1]="Malarone"
    }
    if(ring_list$Treatm[i] %in% c('Green -/Water +', 'Green -/ Water +',"Green-/Water +", 'Green +/ Water + +', 'Water +','water')){
    ring_list$Treatments[i]="Water"
    ring_list$Treatments[i+1]="Water"}
  }else{
    next
  }
}

ring_list$Treatments=as.factor(ring_list$Treatments)

ring_list=ring_list[!is.na(ring_list$Treatments),]
    

# Adding growth rate

ring_list$growth_rate=NA
ring_list$days_resample=NA

for(i in 1:length(ring_list$Weight)){
  if(ring_list$sampling_rank[i] %in% 1){next}
  else{
    ring_list$growth_rate[i]=log(ring_list$Weight[i]/ring_list$Weight[i-1])/as.numeric(ring_list$Date[i]-ring_list$Date[i-1])
    ring_list$days_resample[i]=as.numeric(ring_list$Date[i]-ring_list$Date[i-1]) 
  }
}

# Refining sex 

ring_list$sex=NA


for(i in 1:length(ring_list$sex)){
  if(ring_list$sampling_rank[i] %in% 1){next}
  else{
    ifelse(!is.na(ring_list$Sex..1.male.0.fem.[i]), ring_list$sex[i] <- as.numeric(ring_list$Sex..1.male.0.fem.[i])-1, ring_list$sex[i] <- as.numeric(ring_list$Sex.guess[i])-1)
    ifelse(!is.na(ring_list$Sex..1.male.0.fem.[i]), ring_list$sex[i-1] <- as.numeric(ring_list$Sex..1.male.0.fem.[i])-1, ring_list$sex[i-1] <- as.numeric(ring_list$Sex.guess[i])-1)
  }
}

# Adding body condition (lm(Weight~Wing+Sex))

source(file = "DBChecks-master/R/buteo_condition.R")

ring_list$body_condition=buteo_condition(df=ring_list, wing='Wing', weight='Weight', sex = "sex", unit = "cm", .plot = F)

for(i in 1:length(ring_list$Weight)){
  if(ring_list$sampling_rank[i] %in% 1){next}
  else{ring_list$delta_body_condition[i] = ring_list$body_condition[i] - ring_list$body_condition[i-1]
  }
}  

### Age estimation from the growth data from Bijlsma, G. 1999: Sex determination of nestling Common Buzzards buteo buteo

source("DBChecks/R/buteo_age.R")

growth=read.csv2("growth_data.csv")

growth$age=as.numeric(growth$age)
growth$sex=as.factor(growth$sex)
growth$wing=as.numeric(growth$wing/10)
growth$weight=as.numeric(growth$weight)

model <- stats::lm(age ~ poly(wing, 4, raw = T) + sex, growth)
fit.val <- data.frame(age = stats::predict(model, growth),
                      wing = growth$wing, sex = growth$sex)
## format data
to_predict <- data.frame(wing = ring_list$Wing,
                         sex = as.factor(ring_list$sex))

out <- stats::predict.lm(model, to_predict, se.fit = T)

ring_list$Age_in_days=out$fit

# average age of nestlings

ring_list$avg_age=NA

for(i in 1:length(ring_list$Weight)){
  if(ring_list$sampling_rank[i] %in% 1){next}
  else{ring_list$avg_age[i] = mean(c(ring_list$Age_in_days[i],ring_list$Age_in_days[i-1]))
  }
}  

## relabelling sexes

ring_list$sex=factor(ring_list$sex, levels = c(0, 1), labels = c("Female", "Male"))

## removing time span < 3 and > 27 days between samplings

ring_list=filter(ring_list, ring_list$Ring.no %in% ring_list$Ring.no[ring_list$days_resample > 3 & ring_list$days_resample < 27])

## removing unused columns

ring_list %>% select(-c(Treatm, Sex.guess ,Sex..1.male.0.fem., Age)) -> ring_list

# saving full and second sample only tables


# Mon Nov 22 18:35:58 2021 ------------------------------
# Use: write.csv2(..., dec = ".") to save numbers with decimal points!

# write.csv2(ring_list, 'Phd Bielefeld/Manuscripts/Tolerance to malarone in buzzard/malarone_tolerability.csv')

# write.csv2(ring_list[ring_list$sampling_rank %in% 2,], 'Phd Bielefeld/Manuscripts/Tolerance to malarone in buzzard/malarone_tolerability_resamples_only.csv')


################################################################################
################################################################################
################################################################################

## saved as txt file with "," replaced by "."
## -----------------------------------------------------------------------------
malarone.data <- readr::read_delim(
  file = 'data/malarone_tolerability.txt', delim = ";")

## condense data to long-format
## -----------------------------------------------------------------------------
library(tidyverse)

## Resampling data
## -----------------------------------------------------------------------------
long.2 <- filter(malarone.data, sampling_rank == 2) %>% 
  mutate(Date.2 = Date) %>% 
  mutate(Lprev.2 = Lprev) %>% 
  mutate(Lbinom.2 = Lbinom) %>% 
  mutate(Weight.2 = Weight) %>% 
  mutate(Wing.2 = Wing) %>% 
  mutate(body_condition.2 = body_condition) %>% 
  mutate(Age.2 = Age_in_days) %>% 
  subset(., select = -c(Date, Lprev, Lbinom, Weight, body_condition, Age_in_days, Wing))

## First sampling 
## -----------------------------------------------------------------------------
long.1 <- filter(malarone.data, sampling_rank == 1) %>% 
  mutate(Date.1 = Date) %>% 
  mutate(Lprev.1 = Lprev) %>% 
  mutate(Lbinom.1 = Lbinom) %>% 
  mutate(Weight.1 = Weight) %>% 
  mutate(Wing.1 = Wing) %>% 
  mutate(body_condition.1 = body_condition) %>% 
  mutate(Age.1 = Age_in_days) %>% 
  subset(., select = c(Date.1, Lprev.1, Lbinom.1, Weight.1, body_condition.1, Age.1, Wing.1))

merged <- cbind(long.1, long.2)
merged <- merged[,c(
  "Ring.no", "Year", "sex", "Treatments", "Nest",
  "Date.1", "Date.2", "days_resample", "Age.1", "Age.2", "avg_age",
  "Weight.1", "Weight.2", "Wing.1", "Wing.2", "growth_rate",
  "body_condition.1", "body_condition.2", "delta_body_condition", 
  "Lprev.1", "Lprev.2", "Lbinom.1", "Lbinom.2"
)]

names(merged)[c(1,3,4)] <- c("Ring", "Sex", "Treatment")
readr::write_excel_csv(merged, file = "data/malarone_tolerability.csv")
