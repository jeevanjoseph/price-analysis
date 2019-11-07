install.packages("tidyverse")
library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(MASS)
source("ford.R", local = TRUE)
source("bmw.R", local = TRUE)
source("toyota.R", local = TRUE)
source("chevy.R", local = TRUE)

data <- read_csv("craigslistVehicles_semi.csv");
cities <- read_csv("cities.csv");
cities <- distinct(cities,city_url,.keep_all = TRUE)
modelTypes <- read_csv("model-types.csv")

#plot(as.numeric(data$year),as.numeric(data$odometer))

#data %>%group_by(manufacturer)%>%summarise(count = n()) %>% View

data$manufacturer <- factor(data$manufacturer)
data$condition <- factor(data$condition)
data$title_status <- factor(data$title_status)
data$type <- factor(data$type)
data$size <- factor(data$size)
data$fuel <- factor(data$fuel)
data$cylinders <- factor(data$cylinders)
data$transmission <- factor(data$transmission)
data$paint_color <- factor(data$paint_color)
data$drive <- factor(data$drive)

summary(data);

#complete <- data[complete.cases(data),]
complete <- data %>%  
    drop_na(odometer) %>%
    drop_na(year) %>%
    drop_na(cylinders) %>%
    drop_na(manufacturer) %>%
    drop_na(title_status) %>% 
    drop_na(condition) %>%
    drop_na(fuel) %>%
    drop_na(transmission) %>%
    drop_na(paint_color) %>%
    drop_na(drive) %>%
    drop_na(make)

# clean manufacturers
manu_dist <- complete %>% group_by(manufacturer)%>%summarise(count = n()) %>% arrange(count)
manu_dist$manufacturer <- factor(manu_dist$manufacturer, levels = manu_dist$manufacturer[order(manu_dist$count)])
#ggplot(manu_dist, aes(x=manufacturer, y=count, fill=manufacturer)) + geom_bar(stat="identity", width=1)

sig_manu <- complete %>% group_by(manufacturer)%>%summarise(count = n()) %>% filter(count>(.02*nrow(complete)))

complete <- complete %>% filter(manufacturer %in% sig_manu$manufacturer)

manu_dist <- complete %>% group_by(manufacturer)%>%summarise(count = n()) %>% arrange(count)
manu_dist$manufacturer <- factor(manu_dist$manufacturer, levels = manu_dist$manufacturer[order(manu_dist$count)])
#ggplot(manu_dist, aes(x=manufacturer, y=count, fill=manufacturer)) + geom_bar(stat="identity", width=1)


# Clean Year
year_dist <- complete %>% group_by(year) %>% summarise(count = n()) %>% arrange(count)
#ggplot(data=year_dist,aes(x=year,y=count)) + geom_point()

complete$age <- with(complete, 2020-year)
complete <- complete %>% filter(age<=20 & age>=0)

# clean odometer

# we seem to have a isngle influential outlier for the odometer reading

# The third quantile and the Max are several orders of magnitude different
summary(complete$odometer)
# Discretizing the range of the odometer readings in to 10 equal intervals,
# we see that a single observarion is in the 10th interval and every other
# observation being in the 1st interval clearly indicating that the observation
# is anomalous. The value itself is 102,102,785 or 102 million miles.
# the valu eis so extreme that we can assume that its erroneous and a data entry 
# mistake - likely the user meant 102,785 and repeated the character sequence 102
#
table(cut(complete$odometer, breaks = 10))

# From teh data released by the Fedral Highway administration's offoce of 
# Highway policy Information, vehicles travel an average of 11,789 miles /year.
# Limiting this to our 20 year time frame, we can exlude vehicles that have 
# more than 350k miles on it, and also observations where there are 0 miles on it.
# The extreme observation is removed.

complete <- complete %>% filter(odometer<=quantile(odometer,0.99))

# We can also exclude observations that are reporting a 0 odometer reading unless they 
# are a brand new vehicle. Since we do not have a field for indicating if the 
# automobile being sold is new or used, we can estimate that these are the ones 
# in excellent condition that are also in the model years 2019/2020.
complete <- complete %>% 
    filter(odometer >= quantile(odometer,0.01)  )

quantile(complete$odometer,probs = seq(0,1,.01))

odo_dist<-complete %>%group_by(odometer) %>% summarise(count = n()) 
#ggplot(data=odo_dist,aes(x=odometer,y=count)) + geom_point()


# the distribution indiacts that while some of the observations report very specific mileage,
# other approximate or round off the mileage to a nearest number.
# this is a case where we can discretize the valriables in to constant sized bins.
# this will likely need to be binned.
## remove 0 readings ?

### !!!!!!!!!!    Most important   !!!!!!!!!!!
### the bins here determne the adj-R-squared and the performance
#complete <- complete %>% mutate(mileage = cut(odometer, breaks = c(0,5000,15000,25000,35000,45000,60000,75000,90000,105000,125000,150000,200000,250000,275000,300000,350000))) 
complete <- complete %>% mutate(mileage = cut(odometer, breaks = quantile(complete$odometer,probs = seq(0,1,.05)))) 
mileage_dist <- complete %>% group_by(mileage) %>% summarise(count=n(),pr=median(price))
ggplot(data=mileage_dist,aes(x=mileage,y=count,fill=pr)) + geom_bar(stat="identity", width=1)

# clean price

#complete$price <-with(complete,as.numeric(as.character(price)))
quantile(complete$price,probs = seq(0,1,.01))
quantile(complete$price,.05)
summary(complete$price)
price_dist<-complete %>% 
    filter(price > 0) %>%
    filter(price<= quantile(complete$price,.99) & price>= quantile(complete$price,.01) ) %>% 
    mutate(price_bin = cut(price, breaks = c(0,1000,2500,3500,5000,6500,9000,12000,16000,20000,25000,30000,35000,40000,45000,50000))) %>%
    group_by(price_bin) %>%
    summarise(count = n())
ggplot(data=price_dist,aes(x=price_bin,y=count)) + geom_bar(stat="identity", width=1)

complete <- complete %>% filter(price > 0)
complete <- complete %>% filter(price<= quantile(complete$price,.98) & price>= quantile(complete$price,.02) )

summary(complete$price)
hist(complete$price)

#binned <- complete %>% mutate(price_bin = cut(price, breaks = quantile(price, probs = seq(0, 1, .1)))) %>% group_by(price_bin)%>%summarise(count = n())
#ggplot(data=binned,aes(x=price_bin,y=count,fill=price_bin)) + geom_bar(stat="identity", width=1)


#Clean state, city


# list all cities
levels(factor(complete$city))

complete<-left_join(complete,cities,by="city_url")

#complete <- extract(complete,city,c("City","State"),"([a-z/-]*)[, ]?([A-Z]{2})?")
complete$State<- factor(complete$State)

#complete <- extract(complete,city,c("city"),"([a-zA-Z]*)?")
complete$City<- factor(complete$City)
levels(complete$State)



# Clean transmission
#complete <- complete[which(complete$transmission!=""),]


# Clean drive
#complete <- complete[which(complete$drive!=""),]

# Clean size
#complete <- complete[which(complete$size!=""),]

# Clean type
#complete <- complete[which(complete$type!=""),]

# Clean paint_color
#complete <- complete[which(complete$paint_color!=""),]

# Clean cylinders
#complete <- complete[which(complete$cylinders!=""),]

# Clean fuel
#complete <- complete[which(complete$fuel!=""),]


## reevaluate the factors
complete[] <- lapply(complete, function(x) if(is.factor(x)) factor(x) else x)

## Setup for model & trim
complete$model <- c("")
complete$trim <- c("")


##############################################
## BMW

bmw<-extract(complete[which(complete$manufacturer=="bmw"),],make,c("model","trim"),"(.*?series)(.*)",remove = FALSE)

#bmw %>% View

for (row in 1:nrow(bmw)) {
    make <- bmw[row, "make"]
    model <- bmw_models(make)
    trim <- bmw_trims(make,model)
    
    if(is.na(model) || model == ""){
        bmw[row, "model"] = NA
    }else{
        model = gsub("-","",model)
        model = gsub(" ","",model)
        bmw[row, "model"] = model
    }
    if(is.na(trim)||trim == ""){
        bmw[row, "trim"] = NA
    }else{
        bmw[row, "trim"] = trim
    }
    
    
    ## Collapse 3-Series and 3 Series in to a single level 
    bmw[row, "model"] = gsub("-"," ",bmw[row, "model"])
}

bmw$model<-factor(bmw$model)
bmw$trim<-factor(bmw$trim)
levels(bmw$model)

sig_bmw <- bmw %>% group_by(model,trim) %>% summarise(count = n())  %>% filter(count > 10)
bmw <- bmw %>% filter(model %in% sig_bmw$model) %>% filter(trim %in% sig_bmw$trim)


bmw$type <- as.character(bmw$type)
bmw$size <- as.character(bmw$size)
for (row in 1:nrow(modelTypes)){
    name <- str_trim(modelTypes[row,"model_name"])
    body_type <-  modelTypes[row,"body_type"]
    body_size <-  modelTypes[row,"size"]
    
    bmw$type[bmw$model == name ] <- body_type
    bmw$size[bmw$model == name ] <- body_size
}

bmw$type <- factor(unlist(bmw$type))
bmw$size <- factor(unlist(bmw$size))



### ford

## extract ford make in to model and trim based on the spacing. @todo cleanup the spacing 
ford <- complete[which(complete$manufacturer=="ford"),]
nrow(ford)
for (row in 1:nrow(ford)) {
    make <- ford[row, "make"]
    model <- ford_models(make)
    trim  <- ford_trims(make,model)    
    if(is.na(model) || model == ""){
        ford[row, "model"] = NA
    }else{
        ford[row, "model"] = model
    }
    if(is.na(trim)||trim == ""){
        ford[row, "trim"] = NA
    }else{
        ford[row, "trim"] = trim
    }
}
nrow(ford)
ford <- drop_na(ford,model)
droplevels.factor(ford$model)
levels(ford$model)
sig_ford <- ford %>% group_by(model,trim) %>% summarise(count = n()) %>% filter(count > 10)

nrow(ford %>% filter(is.na(model)))


ford <- ford %>% filter(model %in% sig_ford$model) %>% filter(trim %in% sig_ford$trim)
ford$model<-factor(ford$model)
ford$trim<-factor(ford$trim)
ford[] <- lapply(ford, function(x) if(is.factor(x)) factor(x) else x)

## Ford Consumer Auto
ford_c <- ford %>% filter(!model %in% c("e-150","e-250","e-350","e-450","e-series","econoline","t-150","t-250","t-350","transit","lcf"))
ford_c$type <- as.character(ford_c$type)
ford_c$size <- as.character(ford_c$size)
for (row in 1:nrow(modelTypes)){
    name <- str_trim(modelTypes[row,"model_name"])
    body_type <-  modelTypes[row,"body_type"]
    body_size <-  modelTypes[row,"size"]
    
    ford_c$type[ford_c$model == name ] <- body_type
    ford_c$size[ford_c$model == name ] <- body_size
}

ford_c$type <- factor(unlist(ford_c$type))
ford_c$size <- factor(unlist(ford_c$size))


### Toyota

## extract toyota make in to model and trim based on the spacing. @todo cleanup the spacing 
toyota <- extract(complete[which(complete$manufacturer=="toyota"),],make,c("model","trim"),"([a-zA-Z0-9]*)[ ]?(.*)?",remove = FALSE)

for (row in 1:nrow(toyota)) {
    make <- toyota[row, "make"]
    model <- toyota_models(make)
    trim  <- toyota_trims(make,model)    
    if(is.na(model) || model == ""){
        toyota[row, "model"] = NA
    }else{
        toyota[row, "model"] = model
    }
    if(trim == ""){
        toyota[row, "trim"] = NA
    }else{
        toyota[row, "trim"] = trim
    }
}

nrow(toyota)
toyota <- drop_na(toyota,model)
droplevels.factor(toyota$model)
levels(toyota$model)
sig_toyota <- toyota %>% group_by(model,trim) %>% summarise(count = n()) %>% filter(count > 10)

nrow(toyota %>% filter(is.na(model)))

#toyota %>% filter(model %in% valid_models$model) %>% filter(trim %in% valid_trims$trim) %>% group_by(trim) %>% summarise(count = n()) %>% View

toyota <- toyota %>% filter(model %in% sig_toyota$model) %>% filter(trim %in% sig_toyota$trim)
toyota$model<-factor(toyota$model)
toyota$trim<-factor(toyota$trim)
toyota[] <- lapply(toyota, function(x) if(is.factor(x)) factor(x) else x)

toyota$type <- as.character(toyota$type)
toyota$size <- as.character(toyota$size)
for (row in 1:nrow(modelTypes)){
    name <- str_trim(modelTypes[row,"model_name"])
    body_type <-  modelTypes[row,"body_type"]
    body_size <-  modelTypes[row,"size"]
    
    toyota$type[toyota$model == name ] <- body_type
    toyota$size[toyota$model == name ] <- body_size
}

toyota$type <- factor(unlist(toyota$type))
toyota$size <- factor(unlist(toyota$size))


### chevrolet

## extract chevrolet make in to model and trim based on the spacing. @todo cleanup the spacing 
chevrolet <- complete[which(complete$manufacturer=="chevrolet"),]


for (row in 1:nrow(chevrolet)) {
    make <- chevrolet[row, "make"]
    model <- chevy_models(make)
    trim  <- chevy_trims(make,model)
    

    if(is.na(model) || model == ""){
        chevrolet[row, "model"] = NA
    }else{
        chevrolet[row, "model"] = model
    }
    
    if(trim == ""){
        chevrolet[row, "trim"] = NA
    }else{
        chevrolet[row, "trim"] = trim
    }
    # Collapse 3-Series and 3 Series in to a single level 
    #chevrolet[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    #chevrolet[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}


#chevrolet <- drop_na(chevrolet,model)
#droplevels.factor(chevrolet$model)




levels(chevrolet$model)
sig_chevy<-chevrolet %>% group_by(model,trim) %>% summarise(count=n()) %>% filter(count>=10)
chevrolet <- chevrolet %>% filter(model %in% sig_models$model) %>% filter(trim %in% sig_models$trim)
chevrolet$model<-factor(chevrolet$model)
chevrolet$trim<-factor(chevrolet$trim)
droplevels.factor(chevrolet$model)
chevrolet[] <- lapply(chevrolet, function(x) if(is.factor(x)) factor(x) else x)


## Chevy Consumer Auto
chevy_c <- chevrolet %>% filter(!model %in% c("Astrovan","express")) %>% filter(!type %in% c("van"))
chevy_c$type <- as.character(chevy_c$type)
chevy_c$size <- as.character(chevy_c$size)
for (row in 1:nrow(modelTypes)){
    name <- str_trim(modelTypes[row,"model_name"])
    body_type <-  modelTypes[row,"body_type"]
    body_size <-  modelTypes[row,"size"]
    
    chevy_c$type[chevy_c$model == name ] <- body_type
    chevy_c$size[chevy_c$model == name ] <- body_size
}

chevy_c$type <- factor(unlist(chevy_c$type))
chevy_c$size <- factor(unlist(chevy_c$size))



######################################################
##############   VALIDATON
######################################################


trainingset <- rbind(toyota, bmw, ford, chevrolet)
comp_set <- trainingset %>% dplyr::select(price,manufacturer,model,trim,condition,cylinders,fuel,odometer,title_status,transmission,drive,size,paint_color,age,mileage,type)

comp_set$manufacturer <- factor(comp_set$manufacturer)
comp_set$condition <- factor(comp_set$condition)
comp_set$title_status <- factor(comp_set$title_status)
comp_set$type <- factor(comp_set$type)
comp_set$size <- factor(comp_set$size)
comp_set$fuel <- factor(comp_set$fuel)
comp_set$cylinders <- factor(comp_set$cylinders)
summary(comp_set)


comp_set %>% filter(age<2) %>% View

ggplot(data=comp_set,aes(x=size,y=price,fill=size)) + geom_bar(stat="identity", width=1)

sample <- comp_set %>% sample_n(10000)

sample %>% group_by(manufacturer) %>% summarise(count = n()) %>% View

linearMod <- lm(price ~age*model*mileage+condition+trim+title_status+cylinders+drive+size, data=sample) 
summary(linearMod)
#plot(linearMod)

## VAriable selection by AIC
step <- stepAIC(linearMod, scope=list(upper= ~., lower= ~1),direction = "both",trace=10)
#bigmod <- linearMod
summary(step)
step$anova
plot(step)

##Model 1
linearMod <- lm(price ~age + model + mileage + condition + trim + age:model + 
                    age:mileage + model:mileage + age:model:mileage, data=sample) 
summary(linearMod)

## Model 2
linearMod <- lm(price ~age + model + mileage + condition + trim + age:model + 
                    age:mileage +model:mileage, data=sample) 
summary(linearMod)

## Model 3
bm_mod <- lm(price ~age*mileage*type*size+condition+title_status, data=chevy_c) 
summary(bm_mod)
plot(bm_mod)



######################################################
##############  END  VALIDATON
######################################################




### buick

## extract buick make in to model and trim based on the spacing. @todo cleanup the spacing 
buick <- others[which(others$manufacturer=="buick"),]
others <- others[which(others$manufacturer!="buick"),]

for (row in 1:nrow(buick)) {
    make <- buick[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    buick[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    buick[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

buick$model<-factor(buick$model)
buick$trim<-factor(buick$trim)
levels(buick$model)


### cadillac

## extract cadillac make in to model and trim based on the spacing. @todo cleanup the spacing 
cadillac <- others[which(others$manufacturer=="cadillac"),]
others <- others[which(others$manufacturer!="cadillac"),]

for (row in 1:nrow(cadillac)) {
    make <- cadillac[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    cadillac[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    cadillac[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

cadillac$model<-factor(cadillac$model)
cadillac$trim<-factor(cadillac$trim)
levels(cadillac$model)

### audi

## extract audi make in to model and trim based on the spacing. @todo cleanup the spacing 
audi <- others[which(others$manufacturer=="audi"),]
others <- others[which(others$manufacturer!="audi"),]

for (row in 1:nrow(audi)) {
    make <- audi[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    audi[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    audi[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

audi$model<-factor(audi$model)
audi$trim<-factor(audi$trim)
levels(audi$model)


### chrysler

## extract chrysler make in to model and trim based on the spacing. @todo cleanup the spacing 
chrysler <- others[which(others$manufacturer=="chrysler"),]
others <- others[which(others$manufacturer!="chrysler"),]

for (row in 1:nrow(chrysler)) {
    make <- chrysler[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    chrysler[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    chrysler[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

chrysler$model<-factor(chrysler$model)
chrysler$trim<-factor(chrysler$trim)
levels(chrysler$model)

hist(chevrolet$price)

### dodge

## extract dodge make in to model and trim based on the spacing. @todo cleanup the spacing 
dodge <- others[which(others$manufacturer=="dodge"),]
others <- others[which(others$manufacturer!="dodge"),]

for (row in 1:nrow(dodge)) {
    make <- dodge[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    dodge[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    dodge[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

dodge$model<-factor(dodge$model)
dodge$trim<-factor(dodge$trim)
levels(dodge$model)


### gmc

## extract gmc make in to model and trim based on the spacing. @todo cleanup the spacing 
gmc <- others[which(others$manufacturer=="gmc"),]
others <- others[which(others$manufacturer!="gmc"),]

for (row in 1:nrow(gmc)) {
    make <- gmc[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    gmc[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    gmc[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

gmc$model<-factor(gmc$model)
gmc$trim<-factor(gmc$trim)
levels(gmc$model)


### honda

## extract honda make in to model and trim based on the spacing. @todo cleanup the spacing 
honda <- others[which(others$manufacturer=="honda"),]
others <- others[which(others$manufacturer!="honda"),]

for (row in 1:nrow(honda)) {
    make <- honda[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    honda[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    honda[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

honda$model<-factor(honda$model)
honda$trim<-factor(honda$trim)
levels(honda$model)


### hyundai

## extract hyundai make in to model and trim based on the spacing. @todo cleanup the spacing 
hyundai <- others[which(others$manufacturer=="hyundai"),]
others <- others[which(others$manufacturer!="hyundai"),]

for (row in 1:nrow(hyundai)) {
    make <- hyundai[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    hyundai[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    hyundai[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

hyundai$model<-factor(hyundai$model)
hyundai$trim<-factor(hyundai$trim)
levels(hyundai$model)



### infiniti

## extract infiniti make in to model and trim based on the spacing. @todo cleanup the spacing 
infiniti <- others[which(others$manufacturer=="infiniti"),]
others <- others[which(others$manufacturer!="infiniti"),]

for (row in 1:nrow(infiniti)) {
    make <- infiniti[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    infiniti[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    infiniti[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

infiniti$model<-factor(infiniti$model)
infiniti$trim<-factor(infiniti$trim)
levels(infiniti$model)


### jeep

## extract jeep make in to model and trim based on the spacing. @todo cleanup the spacing 
jeep <- others[which(others$manufacturer=="jeep"),]
others <- others[which(others$manufacturer!="jeep"),]

for (row in 1:nrow(jeep)) {
    make <- jeep[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    jeep[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    jeep[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

jeep$model<-factor(jeep$model)
jeep$trim<-factor(jeep$trim)
levels(jeep$model)


### kia

## extract kia make in to model and trim based on the spacing. @todo cleanup the spacing 
kia <- others[which(others$manufacturer=="kia"),]
others <- others[which(others$manufacturer!="kia"),]

for (row in 1:nrow(kia)) {
    make <- kia[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    kia[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    kia[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

kia$model<-factor(kia$model)
kia$trim<-factor(kia$trim)
levels(kia$model)


### lexus

## extract lexus make in to model and trim based on the spacing. @todo cleanup the spacing 
lexus <- others[which(others$manufacturer=="lexus"),]
others <- others[which(others$manufacturer!="lexus"),]

for (row in 1:nrow(lexus)) {
    make <- lexus[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    lexus[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    lexus[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

lexus$model<-factor(lexus$model)
lexus$trim<-factor(lexus$trim)
levels(lexus$model)

### lincoln

## extract lincoln make in to model and trim based on the spacing. @todo cleanup the spacing 
lincoln <- others[which(others$manufacturer=="lincoln"),]
others <- others[which(others$manufacturer!="lincoln"),]

for (row in 1:nrow(lincoln)) {
    make <- lincoln[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    lincoln[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    lincoln[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

lincoln$model<-factor(lincoln$model)
lincoln$trim<-factor(lincoln$trim)
levels(lincoln$model)


### mazda

## extract mazda make in to model and trim based on the spacing. @todo cleanup the spacing 
mazda <- others[which(others$manufacturer=="mazda"),]
others <- others[which(others$manufacturer!="mazda"),]

for (row in 1:nrow(mazda)) {
    make <- mazda[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    mazda[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    mazda[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

mazda$model<-factor(mazda$model)
mazda$trim<-factor(mazda$trim)
levels(mazda$model)


### mercedes-benz

## extract mercedes-benz make in to model and trim based on the spacing. @todo cleanup the spacing 
mercedes_benz <- others[which(others$manufacturer=="mercedes-benz"),]
others <- others[which(others$manufacturer!="mercedes-benz"),]

for (row in 1:nrow(mercedes_benz)) {
    make <- mercedes_benz[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    mercedes_benz[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    mercedes_benz[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

mercedes_benz$model<-factor(mercedes_benz$model)
mercedes_benz$trim<-factor(mercedes_benz$trim)
levels(mercedes_benz$model)



### mercury

## extract mercury make in to model and trim based on the spacing. @todo cleanup the spacing 
mercury <- others[which(others$manufacturer=="mercury"),]
others <- others[which(others$manufacturer!="mercury"),]

for (row in 1:nrow(mercury)) {
    make <- mercury[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    mercury[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    mercury[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

mercury$model<-factor(mercury$model)
mercury$trim<-factor(mercury$trim)
levels(mercury$model)



### mini

## extract mini make in to model and trim based on the spacing. @todo cleanup the spacing 
mini <- others[which(others$manufacturer=="mini"),]
others <- others[which(others$manufacturer!="mini"),]

for (row in 1:nrow(mini)) {
    make <- mini[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    mini[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    mini[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

mini$model<-factor(mini$model)
mini$trim<-factor(mini$trim)
levels(mini$model)




### mitsubishi

## extract mitsubishi make in to model and trim based on the spacing. @todo cleanup the spacing 
mitsubishi <- others[which(others$manufacturer=="mitsubishi"),]
others <- others[which(others$manufacturer!="mitsubishi"),]

for (row in 1:nrow(mitsubishi)) {
    make <- mitsubishi[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    mitsubishi[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    mitsubishi[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

mitsubishi$model<-factor(mitsubishi$model)
mitsubishi$trim<-factor(mitsubishi$trim)
levels(mitsubishi$model)




### nissan

## extract nissan make in to model and trim based on the spacing. @todo cleanup the spacing 
nissan <- others[which(others$manufacturer=="nissan"),]
others <- others[which(others$manufacturer!="nissan"),]

for (row in 1:nrow(nissan)) {
    make <- nissan[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    nissan[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    nissan[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

nissan$model<-factor(nissan$model)
nissan$trim<-factor(nissan$trim)
levels(nissan$model)




### pontiac

## extract pontiac make in to model and trim based on the spacing. @todo cleanup the spacing 
pontiac <- others[which(others$manufacturer=="pontiac"),]
others <- others[which(others$manufacturer!="pontiac"),]

for (row in 1:nrow(pontiac)) {
    make <- pontiac[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    pontiac[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    pontiac[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

pontiac$model<-factor(pontiac$model)
pontiac$trim<-factor(pontiac$trim)
levels(pontiac$model)


### ram

## extract ram make in to model and trim based on the spacing. @todo cleanup the spacing 
ram <- others[which(others$manufacturer=="ram"),]
others <- others[which(others$manufacturer!="ram"),]

for (row in 1:nrow(ram)) {
    make <- ram[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    ram[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    ram[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

ram$model<-factor(ram$model)
ram$trim<-factor(ram$trim)
levels(ram$model)


### rover

## extract rover make in to model and trim based on the spacing. @todo cleanup the spacing 
rover <- others[which(others$manufacturer=="rover"),]
others <- others[which(others$manufacturer!="rover"),]

for (row in 1:nrow(rover)) {
    make <- rover[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    rover[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    rover[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

rover$model<-factor(rover$model)
rover$trim<-factor(rover$trim)
levels(rover$model)


### saturn

## extract saturn make in to model and trim based on the spacing. @todo cleanup the spacing 
saturn <- others[which(others$manufacturer=="saturn"),]
others <- others[which(others$manufacturer!="saturn"),]

for (row in 1:nrow(saturn)) {
    make <- saturn[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    saturn[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    saturn[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

saturn$model<-factor(saturn$model)
saturn$trim<-factor(saturn$trim)
levels(saturn$model)


### subaru

## extract subaru make in to model and trim based on the spacing. @todo cleanup the spacing 
subaru <- others[which(others$manufacturer=="subaru"),]
others <- others[which(others$manufacturer!="subaru"),]

for (row in 1:nrow(subaru)) {
    make <- subaru[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    subaru[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    subaru[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

subaru$model<-factor(subaru$model)
subaru$trim<-factor(subaru$trim)
levels(subaru$model)


### volkswagen

## extract volkswagen make in to model and trim based on the spacing. @todo cleanup the spacing 
volkswagen <- others[which(others$manufacturer=="volkswagen"),]
others <- others[which(others$manufacturer!="volkswagen"),]

for (row in 1:nrow(volkswagen)) {
    make <- volkswagen[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    volkswagen[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    volkswagen[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

volkswagen$model<-factor(volkswagen$model)
volkswagen$trim<-factor(volkswagen$trim)
levels(volkswagen$model)


### volvo

## extract volvo make in to model and trim based on the spacing. @todo cleanup the spacing 
volvo <- others[which(others$manufacturer=="volvo"),]
others <- others[which(others$manufacturer!="volvo"),]

for (row in 1:nrow(volvo)) {
    make <- volvo[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    volvo[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    volvo[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

volvo$model<-factor(volvo$model)
volvo$trim<-factor(volvo$trim)
levels(volvo$model)



### fiat

## extract fiat make in to model and trim based on the spacing. @todo cleanup the spacing 
fiat <- others[which(others$manufacturer=="fiat"),]
others <- others[which(others$manufacturer!="fiat"),]

for (row in 1:nrow(fiat)) {
    make <- fiat[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    fiat[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    fiat[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

fiat$model<-factor(fiat$model)
fiat$trim<-factor(fiat$trim)
levels(fiat$model)



### jaguar

## extract jaguar make in to model and trim based on the spacing. @todo cleanup the spacing 
jaguar <- others[which(others$manufacturer=="jaguar"),]
others <- others[which(others$manufacturer!="jaguar"),]

for (row in 1:nrow(jaguar)) {
    make <- jaguar[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    jaguar[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    jaguar[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

jaguar$model<-factor(jaguar$model)
jaguar$trim<-factor(jaguar$trim)
levels(jaguar$model)

ford %>% 
    group_by(age) %>% 
    summarise(median_price = log(median(price[age>=0 && age<=20]))) %>% 
    ggplot( aes(x=age, y=median_price)) + geom_point(aes(col=age)) + geom_smooth(method="lm")



rbind(toyota, bmw,ford) %>% group_by(manufacturer,age) %>% summarise(median_price = log(median(price[age>=0 && age<=18])), cnt = n()) %>% ggplot( aes(x=age, y=cnt)) + geom_point(aes(col=manufacturer)) + geom_smooth(method="lm")

ggplot(data=ford, aes(x=age, y=price)) + geom_point(aes(col=model)) + geom_smooth(method="lm")
ggplot(data=ford, aes(y=price, x=state)) + 
    geom_bar(position="dodge", stat="identity")

## reevaluate the factors
complete[] <- lapply(complete, function(x) if(is.factor(x)) factor(x) else x)

summary(complete);
levels(complete$state)
hist(complete$city)

scatter.smooth(x=complete$age, y=complete$price,main="Dist ~ Speed")

cor(complete$price,complete$paint_color)

trainingset <- rbind(toyota, bmw, ford, chevrolet)
comp_set <- trainingset[complete.cases(trainingset),]

comp_set$manufacturer <- factor(comp_set$manufacturer)
comp_set$condition <- factor(comp_set$condition)
comp_set$title_status <- factor(comp_set$title_status)
comp_set$type <- factor(comp_set$type)
comp_set$size <- factor(comp_set$size)
comp_set$fuel <- factor(comp_set$fuel)
comp_set$cylinders <- factor(comp_set$cylinders)
summary(comp_set)

linearMod <- lm(price ~ manufacturer+age+mileage+model+trim+title_status+condition, data=comp_set) 
summary(linearMod)
#plot(linearMod)

## VAriable selection by AIC
step <- stepAIC(linearMod, scope=list(upper= ~manufacturer*age*mileage*model*trim*title_status*condition, lower= ~manufacturer+age+mileage+model+trim))
#bigmod <- linearMod

residual <- resid(linearMod)

plot(linearMod)
ggplot(data=ford,aes(x=odometer,y=price)) + geom_point()


