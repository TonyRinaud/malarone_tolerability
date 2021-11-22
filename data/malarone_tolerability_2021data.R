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

# write.csv2(ring_list, 'Phd Bielefeld/Manuscripts/Tolerance to malarone in buzzard/malarone_tolerability.csv')

# write.csv2(ring_list[ring_list$sampling_rank %in% 2,], 'Phd Bielefeld/Manuscripts/Tolerance to malarone in buzzard/malarone_tolerability_resamples_only.csv')
