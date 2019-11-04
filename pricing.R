install.packages("tidyverse")
library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
source("ford.R", local = TRUE)

data <- read_csv("craigslistVehicles_semi.csv");
cities <- read_csv("cities.csv");
cities <- distinct(cities,city_url,.keep_all = TRUE)
summary(data);
#plot(as.numeric(data$year),as.numeric(data$odometer))
levels(data$type)
summary(data$type)
data %>%group_by(type)%>%summarise(count = n()) %>% View


source("ford.R", local = TRUE)
#complete <- data[complete.cases(data),]
complete <- data %>%  
    drop_na(odometer) %>%
    drop_na(year) %>%
    drop_na(make) %>%
    drop_na(manufacturer) %>%
    drop_na(title_status) %>% 
    drop_na(condition)

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
ggplot(data=year_dist,aes(x=year,y=count)) + geom_point()

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
    filter(odometer >= quantile(odometer,0.02)  )

quantile(complete$odometer,probs = seq(0,1,.01))

odo_dist<-complete %>%group_by(odometer) %>% summarise(count = n()) 
ggplot(data=odo_dist,aes(x=odometer,y=count)) + geom_point()
# the distribution indiacts that while some of the observations report very specific mileage,
# other approximate or round off the mileage to a nearest number.
# this is a case where we can discretize the valriables in to constant sized bins.
# this will likely need to be binned.
## remove 0 readings ?

complete <- complete %>% mutate(mileage = cut(odometer, breaks = c(0,5000,15000,25000,35000,45000,60000,75000,90000,105000,125000,150000,200000,250000,275000,300000,350000))) 
mileage_dist <- complete %>% group_by(mileage) %>% summarise(count=n(),pr=median(price))
ggplot(data=mileage_dist,aes(x=mileage,y=count,fill=pr)) + geom_bar(stat="identity", width=1)

# clean price

complete%>%filter(price==NA)
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

complete <- complete %>% 
    filter(price > 0) %>%
    filter(price<= quantile(complete$price,.99) & price>= quantile(complete$price,.01) )

summary(price_clean$price)
hist(price_clean$price)

#binned <- complete %>% mutate(price_bin = cut(price, breaks = quantile(price, probs = seq(0, 1, .1)))) %>% group_by(price_bin)%>%summarise(count = n())
#ggplot(data=binned,aes(x=price_bin,y=count,fill=price_bin)) + geom_bar(stat="identity", width=1)


#Clean state, city


# list all cities
levels(factor(complete$city))

ambiguous <- complete %>% filter(!str_detect(city,"[ ][:upper:]{2}")) %>% select(city)
unambiguous <- complete %>% filter(str_detect(city,"[ ][:upper:]{2}")) %>% select(city)
levels(factor(ambiguous$city))

complete<-left_join(complete,cities,by="city_url")

#complete <- extract(complete,city,c("City","State"),"([a-z/-]*)[, ]?([A-Z]{2})?")
complete$State<- factor(complete$State)

#complete <- extract(complete,city,c("city"),"([a-zA-Z]*)?")
complete$city<- factor(complete$city)
levels(complete$State)

# Clean tilte status
complete <- complete[which(complete$title_status!=""),]

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

# Clean condition
complete <- complete[which(complete$condition!=""),]

# Clean cylinders
#complete <- complete[which(complete$cylinders!=""),]

# Clean fuel
#complete <- complete[which(complete$fuel!=""),]


## reevaluate the factors
complete[] <- lapply(complete, function(x) if(is.factor(x)) factor(x) else x)

## Setup for model & trim
complete$model <- c("")
complete$trim <- c("")

## BMW

bmw<-extract(complete[which(complete$manufacturer=="bmw"),],make,c("model","trim"),"(.*?series)(.*)",remove = FALSE)
others <- complete[which(complete$manufacturer!="bmw"),]

for (row in 1:nrow(bmw)) {
    make <- bmw[row, "make"]
    model_start = substr(make,1,1)
    if(model_start == "1" || model_start == "3" ||model_start == "5" ||model_start == "7" ) {
        bmw[row, "model"]<- paste(model_start,"series")
    }
    else if(model_start == "m" || model_start == "x") {
        ## for X and M models, model will be X{num}.
        ## whatever follows is used as a trim - will need more work to reduce the levels here.
        bmw[row, "model"] = substr(gsub("(x|m)[ \\-]?([0-9]+)","\\1\\2",make),1,2)
        bmw[row, "trim"] = substr(gsub("(x|m)[ \\-]?([0-9]+)","\\1\\2",make),4,nchar(gsub("(x|m)[ \\-]?([0-9]+)","\\1\\2",make)))
    }
    
    ## Collapse 3-Series and 3 Series in to a single level 
    bmw[row, "model"] = gsub("-"," ",bmw[row, "model"])
}

bmw$model<-factor(bmw$model)
bmw$trim<-factor(bmw$trim)
levels(bmw$model)

### ford

## extract ford make in to model and trim based on the spacing. @todo cleanup the spacing 
ford <- others[which(others$manufacturer=="ford"),]
others <- others[which(others$manufacturer!="ford"),]
nrow(ford)
for (row in 1:nrow(ford)) {
    make <- ford[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    model <- ford_models(make)
    trim  <- ford_trims(make,model)    
    
    
    if(model == ""){
        ford[row, "model"] = NA
    }else{
        ford[row, "model"] = model
    }
    
    if(trim == ""){
        ford[row, "trim"] = NA
    }else{
        ford[row, "trim"] = trim
    }
    
}
nrow(ford)
ford$model<-factor(ford$model)
ford$trim<-factor(ford$trim)
ford[] <- lapply(ford, function(x) if(is.factor(x)) factor(x) else x)

ford <- drop_na(ford,model)
droplevels.factor(ford$model)
levels(ford$model)
valid_models <- ford %>% group_by(model) %>% summarise(count = n()) %>% filter(count > 10)
valid_trims <- ford %>% group_by(trim) %>% summarise(count = n()) %>% filter(count>10)

nrow(ford %>% filter(is.na(model)))

ford %>% filter(model %in% valid_models$model) %>% filter(trim %in% valid_trims$trim) %>% group_by(trim) %>% summarise(count = n()) %>% View

ford <- ford %>% filter(model %in% valid_models$model) %>% filter(trim %in% valid_trims$trim)

### Toyota

## extract toyota make in to model and trim based on the spacing. @todo cleanup the spacing 
toyota <- extract(others[which(others$manufacturer=="toyota"),],make,c("model","trim"),"([a-zA-Z0-9]*)[ ]?(.*)?",remove = FALSE)
others <- others[which(others$manufacturer!="toyota"),]
toyota$model<-factor(toyota$model)
toyota$trim<-factor(toyota$trim)
levels(toyota$model)


### acura

## extract acura make in to model and trim based on the spacing. @todo cleanup the spacing 
acura <- others[which(others$manufacturer=="acura"),]
others <- others[which(others$manufacturer!="acura"),]

for (row in 1:nrow(acura)) {
    make <- acura[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    acura[row, "model"] = gsub("(.*)?(tsx|mdx|rdx|ilx|mds|pilot|rlx|rsx|tls|tsx|tlx|zdx|cl|rl|tl)(.*)?","\\2",make,ignore.case = TRUE)
    acura[row, "trim"] = gsub(acura[row, "model"],"",make,ignore.case = TRUE)
}

acura$model<-factor(acura$model)
acura$trim<-factor(acura$trim)
levels(acura$model)


### chevrolet

## extract chevrolet make in to model and trim based on the spacing. @todo cleanup the spacing 
chevrolet <- others[which(others$manufacturer=="chevrolet"),]
others <- others[which(others$manufacturer!="chevrolet"),]

for (row in 1:nrow(chevrolet)) {
    make <- chevrolet[row, "make"]
    # Collapse 3-Series and 3 Series in to a single level 
    chevrolet[row, "model"] = gsub("(.*)?[ ](.*)?","\\1",make,ignore.case = TRUE)
    chevrolet[row, "trim"] = gsub("(.*)?[ ](.*)?","\\2",make,ignore.case = TRUE)
}

chevrolet$model<-factor(chevrolet$model)
chevrolet$trim<-chevrolet(acura$trim)
levels(chevrolet$model)

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



rbind(toyota, honda, hyundai,ford) %>% group_by(manufacturer,age) %>% summarise(median_price = log(median(price[age>=0 && age<=18])), cnt = n()) %>% ggplot( aes(x=age, y=cnt)) + geom_point(aes(col=manufacturer)) + geom_smooth(method="lm")

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

summary(ford)
linearMod <- lm(price ~ age*odometer*model+type+title_status+trim, data=ford) 
summary(linearMod)

#bigmod <- linearMod

residual <- resid(linearMod)

plot(linearMod)
ggplot(data=ford,aes(x=odometer,y=price)) + geom_point()


