#Author-Drashti Shah
#Date- December 9th,2020
#APAN 5200  Kaggle Assignment

#Pre-work 

#Set Working Directory

setwd('C:/Users/drats/Desktop/Columbia/APAN 5200 APAN Frameworks and Methods 1/Assignments/Kaggle Assignment')


# Read the 2 data sets

data = read.csv('analysisData.csv', 
                na.strings = c("NA","N/A",""),
                stringsAsFactors = T)

scoringData = read.csv('scoringData.csv', 
                       na.strings = c("NA","N/A",""),
                       stringsAsFactors = T)

## STEP 1- Drop irrelevant variables from both data sets

data = subset(data, 
              select = -c(id,host_name, host_location, host_acceptance_rate, host_neighbourhood, host_listings_count, street,
                                neighbourhood, city, state, market, country_code, country, square_feet, weekly_price, monthly_price,
                                has_availability, requires_license, license, jurisdiction_names, is_business_travel_ready,minimum_minimum_nights,
                                maximum_minimum_nights,minimum_maximum_nights,
                                maximum_maximum_nights, minimum_nights_avg_ntm,
                                maximum_nights_avg_ntm))

scoringData = subset(scoringData, select = -c(host_name, host_location, host_acceptance_rate, host_neighbourhood, host_listings_count, street,
                                              neighbourhood, city, state, market, country_code, country, square_feet, weekly_price, monthly_price,
                                              has_availability, requires_license, license, jurisdiction_names, is_business_travel_ready,minimum_minimum_nights,
                                              maximum_minimum_nights,minimum_maximum_nights,
                                              maximum_maximum_nights, minimum_nights_avg_ntm,
                                              maximum_nights_avg_ntm))

## STEP 2- Check distribution of Price, Remove where prices are zero
data = data[!data$price==0,]


##STEP 3- Removing outliers from Maximum nights column in both data sets

Outliermax_nights=(data$maximum_nights>1200)
data[Outliermax_nights,"maximum_nights"]=median(data$maximum_nights<1200,na.rm=T)
summary(data$maximum_nights)

Outliermax_nights1=(scoringData$maximum_nights>1200)
scoringData[Outliermax_nights1,"maximum_nights"]=median(scoringData$maximum_nights<1200,na.rm=T)
summary(scoringData$maximum_nights)

## STEP 4- Imputation with median for missing numeric data in various numeric columns
set.seed(1031)

#Host_response_rate
host_response_ratemissing=is.na(data$host_response_rate)
data[host_response_ratemissing,'host_response_rate']=median(data$host_response_rate,na.rm=T)
data$host_response_rate

scoringData$host_response_rate = as.numeric(sub("%","",scoringData$host_response_rate))/100
host_response_ratemissing1=is.na(scoringData$host_response_rate)
scoringData[host_response_ratemissing1,'host_response_rate']=median(scoringData$host_response_rate,na.rm=T)
scoringData$host_response_rate

#Cleaning_fee

cleaning_feemissing=is.na(data$cleaning_fee)
data[cleaning_feemissing,'cleaning_fee']=median(data$cleaning_fee,na.rm=T)
data$cleaning_fee

cleaning_feemissing1=is.na(scoringData$cleaning_fee)
scoringData[cleaning_feemissing1,'cleaning_fee']=median(scoringData$cleaning_fee,na.rm=T)
scoringData$cleaning_fee

#Security_deposit

security_depositmissing=is.na(data$security_deposit)
data[security_depositmissing,'security_deposit']=median(data$security_deposit,na.rm=T)
data$security_deposit

security_depositmissing1=is.na(scoringData$security_deposit)
scoringData[security_depositmissing1,'security_deposit']=median(scoringData$security_deposit,na.rm=T)
scoringData$security_deposit

#Host_total_listings_count
host_listingsmissing=is.na(data$host_total_listings_count)
data[host_listingsmissing,'host_total_listings_count']=median(data$host_total_listings_count,na.rm=T)
data$host_total_listings_count

host_listingsmissing1=is.na(scoringData$host_total_listings_count)
scoringData[host_listingsmissing1,'host_total_listings_count']=median(scoringData$host_total_listings_count,na.rm=T)
scoringData$host_total_listings_count

#Beds
bedsmissing=is.na(data$beds)
data[bedsmissing,'beds']="1"

bedsmissing1=is.na(scoringData$beds)
scoringData[bedsmissing1,'beds']="1"

#reviews_per_month
reviews_per_monthmissing=is.na(data$reviews_per_month)
data[reviews_per_monthmissing,'reviews_per_month']=median(data$reviews_per_month,na.rm=T)
data$reviews_per_month

reviews_per_monthmissing1=is.na(scoringData$reviews_per_month)
scoringData[reviews_per_monthmissing1,'reviews_per_month']=median(scoringData$reviews_per_month,na.rm=T)
scoringData$reviews_per_month


## STEP 5- Imputation for missing values in string columns
library(data.table)

#Host_response_time
table(data$host_response_time)
host_response_timemissing=is.na(data$host_response_time)
data[host_response_timemissing,'host_response_time']="a few days or more"
data$host_response_time

host_response_timemissing1=is.na(scoringData$host_response_time)
scoringData[host_response_timemissing1,'host_response_time']="a few days or more"
scoringData$host_response_time

## STEP 6- Property Type-Combining levels
table(data$property_type)
levels(data$property_type)
levels(data$property_type)=c("Other","Apartment","Bed and breakfast",
                             "Other","Hotel","Other",
                             "Other","Other","Other",
                             "Other","Condominium","Other",
                             "Other","Other","Guesthouse",
                             "Guesthouse","Hostel","Hotel",
                             "House","Other","Other",
                             "Loft","Other","Other",
                             "Resort","Apartment","Other",
                             "Other","Other","House",
                             "Other")

table(scoringData$property_type)
levels(scoringData$property_type)
levels(scoringData$property_type)=c("Other","Apartment","Bed and breakfast",
                                    "Other","Hotel","Other",
                                    "Other","Other","Other",
                                    "Other","Condominium","Other",
                                    "Other","Other","Guesthouse",
                                    "Guesthouse","Hostel","Hotel",
                                    "House","Other","Loft",
                                    "Other","Resort","Apartment",
                                    "Other","Other","House",
                                    "Other")

## STEP 7- Clean Zipcodes
table(data$zipcode)
max(table(data$zipcode))
zipcodemissing=is.na(data$zipcode)
data[zipcodemissing,'zipcode']='11211'

#Changing bad formats of Zipcodes in analysis data
data$zipcode[data$zipcode == "11249\n11249"] = "11249"
data$zipcode[data$zipcode == "11385-2308"] = "11385"
data$zipcode[data$zipcode == "11413-3220"] = "11413"
data$zipcode[data$zipcode == "11103-3233"] = "11103"
data$zipcode[data$zipcode == "1m"] = "11221"

table(scoringData$zipcode)
max(table(scoringData$zipcode))
zipcodemissing1=is.na(scoringData$zipcode)
scoringData[zipcodemissing1,'zipcode']='11211'

#Changing bad formats of Zipcodes in Scoring Data
scoringData$zipcode[scoringData$zipcode == "11103-3233"] = "11103"
scoringData$zipcode[scoringData$zipcode == "111211"] = "11221"

##STEP 8- Amenities and host verifications

library(stringr)

'Host_verifications-counting total number of host verifications-
specific host_verifications do not seem to be important'

data$total_host_verifications=str_count(data$host_verifications,
                                        pattern=',')+1
scoringData$total_host_verifications=str_count(scoringData$host_verifications,
                                               pattern=',')+1
#Amenities-counting total number of amenities
data$totalamenities=str_count(data$amenities,
                        pattern=',')+1
scoringData$totalamenities=str_count(scoringData$amenities,
                              pattern=',')+1

#Amenities-converting to logicals-specific amenities could be relevant
#Only considered a few popular Airbnb amenities
#Changing them to numeric to use them in correlation matrix later
#TRUE-1,FALSE-0

#1. TV
data$TV=as.numeric(str_detect(data$amenities, "TV"))
scoringData$TV=as.numeric(str_detect(scoringData$amenities, "TV"))

#2. Wifi
data$Wifi=as.numeric(str_detect(data$amenities, "Wifi"))
scoringData$Wifi=as.numeric(str_detect(scoringData$amenities, "Wifi"))

#3. Air conditioning
data$AC=as.numeric(str_detect(data$amenities, "Air conditioning"))
scoringData$AC=as.numeric(str_detect(scoringData$amenities, "Air conditioning"))

#4. Heating
data$Heating=as.numeric(str_detect(data$amenities, "Heating"))
scoringData$Heating=as.numeric(str_detect(scoringData$amenities, "Heating"))

#5. Washer
data$Washer=as.numeric(str_detect(data$amenities, "Washer"))
scoringData$Washer=as.numeric(str_detect(scoringData$amenities, "Washer"))

#6. Dryer
data$Dryer=as.numeric(str_detect(data$amenities, "Dryer"))
scoringData$Dryer=as.numeric(str_detect(scoringData$amenities, "Dryer"))

#7. Essentials
data$Essentials=as.numeric(str_detect(data$amenities, "Essentials"))
scoringData$Essentials=as.numeric(str_detect(scoringData$amenities, "Essentials"))

#8. Elevator
data$Elevator=as.numeric(str_detect(data$amenities, "Elevator"))
scoringData$Elevator=as.numeric(str_detect(scoringData$amenities, "Elevator"))

#9. Kitchen
data$Kitchen=as.numeric(str_detect(data$amenities, "Kitchen"))
scoringData$Kitchen=as.numeric(str_detect(scoringData$amenities, "Kitchen"))

#10. Free parking on premises
data$PremisesP=as.numeric(str_detect(data$amenities, "Free parking on premises"))
scoringData$PremisesP=as.numeric(str_detect(scoringData$amenities, "Free parking on premises"))

#11. Free Street Parking
data$StreetP=as.numeric(str_detect(data$amenities, "Free street parking"))
scoringData$StreetP=as.numeric(str_detect(scoringData$amenities, "Free street parking"))

#12. Gym
data$Gym=as.numeric(str_detect(data$amenities, "Gym"))
scoringData$Gym=as.numeric(str_detect(scoringData$amenities, "Gym"))

#13. Internet
data$Internet=as.numeric(str_detect(data$amenities, "Internet"))
scoringData$Internet=as.numeric(str_detect(scoringData$amenities, "Internet"))

##STEP 9- Other logicals need to match the Step 8 logicals

#Instant_bookable
library(plyr)
data$instant_bookable=revalue(data$instant_bookable,c("t"="TRUE","f"="FALSE"))
scoringData$instant_bookable=revalue(scoringData$instant_bookable,c("t"="TRUE","f"="FALSE"))


#Host_is_superhost
data$host_is_superhost=revalue(data$host_is_superhost,c("t"="TRUE","f"="FALSE"))
scoringData$host_is_superhost=revalue(scoringData$host_is_superhost,c("t"="TRUE","f"="FALSE"))

#Host_has_profile_pic
data$host_has_profile_pic=revalue(data$host_has_profile_pic,c("t"="TRUE","f"="FALSE"))
scoringData$host_has_profile_pic=revalue(scoringData$host_has_profile_pic,c("t"="TRUE","f"="FALSE"))

#Host_identity_verified
data$host_identity_verified=revalue(data$host_identity_verified,c("t"="TRUE","f"="FALSE"))
scoringData$host_identity_verified=revalue(scoringData$host_identity_verified,c("t"="TRUE","f"="FALSE"))

#Is_location_exact
data$is_location_exact=revalue(data$is_location_exact,c("t"="TRUE","f"="FALSE"))
scoringData$is_location_exact=revalue(scoringData$is_location_exact,c("t"="TRUE","f"="FALSE"))

#Require_guest_profile_picture
data$require_guest_profile_picture=revalue(data$require_guest_profile_picture,c("t"="TRUE","f"="FALSE"))
scoringData$require_guest_profile_picture=revalue(scoringData$require_guest_profile_picture,c("t"="TRUE","f"="FALSE"))

#Require_guest_phone_verification
data$require_guest_phone_verification=revalue(data$require_guest_phone_verification,c("t"="TRUE","f"="FALSE"))
scoringData$require_guest_phone_verification=revalue(scoringData$require_guest_phone_verification,c("t"="TRUE","f"="FALSE"))

##STEP 10- Bed Type Variable-Changing it based on Real Bed

data$real_bed=str_detect(data$bed_type,"Real Bed")
scoringData$real_bed=str_detect(scoringData$bed_type,"Real Bed")

##STEP 11- Removing NA's from logicals

#Didn't realize logicals had NA as well before


#host_is_superhost
Superhostmissing=is.na(data$host_is_superhost)
data[Superhostmissing, 'host_is_superhost'] = "FALSE"
Superhostmissing1=is.na(scoringData$host_is_superhost)
scoringData[Superhostmissing1, 'host_is_superhost'] = "FALSE"

#host_has_profile_pic
Profilepicmissing=is.na(data$host_has_profile_pic)
data[Profilepicmissing, 'host_has_profile_pic'] = "FALSE"
Profilepicmissing1=is.na(scoringData$host_has_profile_pic)
scoringData[Profilepicmissing1, 'host_has_profile_pic'] = "FALSE"


#host_identity_verified
Identityverifiedmissing=is.na(data$host_identity_verified)
data[Identityverifiedmissing, 'host_identity_verified'] = "FALSE"
Identityverifiedmissing1=is.na(scoringData$host_identity_verified)
scoringData[Identityverifiedmissing1, 'host_identity_verified'] = "FALSE"

## STEP 12- Name variable

#Uppercase letters sometimes might have an effect on price

data$name_upper=as.numeric(str_count(data$name,"[A-Z]"))
scoringData$name_upper=as.numeric(str_count(scoringData$name,"[A-Z]"))

## STEP 13- Count of words in the string variables and impute NAs with median

#Name
data$name=str_count(data$name,pattern=" ")+1
scoringData$name = str_count(scoringData$name,pattern = " ")+1

#Summary
data$summary=str_count(data$summary,pattern=" ")+1
scoringData$summary= str_count(scoringData$summary,pattern = " ")+1
summarymissing=is.na(data$summary)
data[summarymissing, 'summary'] = median(data$summary,na.rm = T)
summarymissing1=is.na(scoringData$summary)
scoringData[summarymissing1, 'summary'] = median(scoringData$summary,na.rm = T)

#Space
data$space=str_count(data$space,pattern=" ")+1
scoringData$space= str_count(scoringData$space,pattern = " ")+1
spacemissing=is.na(data$space)
data[spacemissing, 'space'] = median(data$space,na.rm = T)
spacemissing1=is.na(scoringData$space)
scoringData[spacemissing1, 'space'] = median(scoringData$space,na.rm = T)

#Description
data$description=str_count(data$description,pattern=" ")+1
scoringData$description= str_count(scoringData$description,pattern = " ")+1
descriptionmissing=is.na(data$description)
data[descriptionmissing, 'description'] = median(data$description,na.rm = T)
descriptionmissing1=is.na(scoringData$description)
scoringData[descriptionmissing1, 'description'] = median(scoringData$description,na.rm = T)

#Neighbourhood_overview
data$neighborhood_overview=str_count(data$neighborhood_overview,pattern=" ")+1
scoringData$neighborhood_overview= str_count(scoringData$neighborhood_overview,pattern = " ")+1
N_Omissing=is.na(data$neighborhood_overview)
data[N_Omissing, 'neighborhood_overview'] = median(data$neighborhood_overview,na.rm = T)
N_Omissing1=is.na(scoringData$neighborhood_overview)
scoringData[N_Omissing1, 'neighborhood_overview'] = median(scoringData$neighborhood_overview,na.rm = T)

#Notes
data$notes = as.numeric(str_count(data$notes, pattern = " "))
scoringData$notes = as.numeric(str_count(scoringData$notes,pattern = " "))
Notesmissing = is.na(data$notes)
data[Notesmissing, 'notes'] = median(data$notes, na.rm = T)
Notesmissing1 = is.na(scoringData$notes)
scoringData[Notesmissing1, 'notes'] = median(scoringData$notes, na.rm = T)


#Transit
data$transit = as.numeric(str_count(data$transit, pattern = " "))
scoringData$transit = as.numeric(str_count(scoringData$transit,pattern = " "))
transitmissing = is.na(data$transit)
data[transitmissing, 'transit'] = median(data$transit, na.rm = T)
transitmissing1 = is.na(scoringData$transit)
scoringData[transitmissing1, 'transit'] = median(scoringData$transit, na.rm = T)

#Access
data$access = str_count(data$access, pattern = " ")
scoringData$access = str_count(scoringData$access,pattern = " ")
Accessmissing = is.na(data$access)
data[Accessmissing, 'access'] = median(data$access, na.rm = T)
Accessmissing1 = is.na(scoringData$access)
scoringData[Accessmissing1, 'access'] = median(scoringData$access, na.rm = T)

#Interaction
data$interaction = as.numeric(str_count(data$interaction, pattern = " "))
scoringData$interaction = as.numeric(str_count(scoringData$interaction,pattern = " "))
Interactionmissing = is.na(data$interaction)
data[Interactionmissing, 'interaction'] = median(data$interaction, na.rm = T)
Interactionmissing1 = is.na(scoringData$interaction)
scoringData[Interactionmissing1, 'interaction'] = median(scoringData$interaction, na.rm = T)

# Host_about
data$host_about = str_count(data$host_about, pattern = " ")
scoringData$host_about = str_count(scoringData$host_about,pattern = " ")
Hostaboutmissing = is.na(data$host_about)
data[Hostaboutmissing, 'host_about'] = median(data$host_about, na.rm = T)
Hostaboutmissing1 = is.na(scoringData$host_about)
scoringData[Hostaboutmissing1, 'host_about'] = median(scoringData$host_about, na.rm = T)

# HOUSE_RULES
data$house_rules = as.numeric(str_count(data$house_rules, pattern = " "))
scoringData$house_rules = as.numeric(str_count(scoringData$house_rules,pattern = " "))
HouseRulesmissing = is.na(data$house_rules)
data[HouseRulesmissing, 'house_rules'] = median(data$house_rules, na.rm = T)
HouseRulesmissing1 = is.na(scoringData$house_rules)
scoringData[HouseRulesmissing1, 'house_rules'] = median(scoringData$house_rules, na.rm = T)


## STEP 14-Dropping variables

# host_since,first_review and last_review didn't come up as important variables in initial analysis
# smart_location and neighborhood_cleansed are captured in neighborhood_group_cleansed variable
# Calender_updated has too many levels>10

datanew=subset(data,select=-c(host_since,host_verifications,amenities,
                              first_review,last_review,bed_type,neighbourhood_cleansed,
                              smart_location,calendar_updated))
names(datanew)
scoringDatanew=subset(scoringData,select=-c(host_since,host_verifications,amenities,
                              first_review,last_review,bed_type,neighbourhood_cleansed,
                              smart_location,calendar_updated))

## STEP 15-Check correlations in numeric variables

library(tidyr);library(corrplot);library(ggplot2);library(ggcorrplot)


#Changing some integer values to numeric

#Accomodates
datanew$accommodates=as.numeric(datanew$accommodates)
scoringDatanew$accommodates=as.numeric(scoringDatanew$accommodates)

#Bedrooms
datanew$bedrooms=as.numeric(datanew$bedrooms)
scoringDatanew$bedrooms=as.numeric(scoringDatanew$bedrooms)

#Price
datanew$price=as.numeric(datanew$price)

#Security Deposit
datanew$security_deposit=as.numeric(datanew$security_deposit)
scoringDatanew$security_deposit=as.numeric(scoringDatanew$security_deposit)

#Cleaning fee
datanew$cleaning_fee=as.numeric(datanew$cleaning_fee)
scoringDatanew$cleaning_fee=as.numeric(scoringDatanew$cleaning_fee)

#Guests included
datanew$guests_included=as.numeric(datanew$guests_included)
scoringDatanew$guests_included=as.numeric(scoringDatanew$guests_included)

#Extra People
datanew$extra_people=as.numeric(datanew$extra_people)
scoringDatanew$extra_people=as.numeric(scoringDatanew$extra_people)

#Minumum_nights
datanew$minimum_nights=as.numeric(datanew$minimum_nights)
scoringDatanew$minimum_nights=as.numeric(scoringDatanew$minimum_nights)

#availability_30
datanew$availability_30=as.numeric(datanew$availability_30)
scoringDatanew$availability_30=as.numeric(scoringDatanew$availability_30)

#availability_60
datanew$availability_60=as.numeric(datanew$availability_60)
scoringDatanew$availability_60=as.numeric(scoringDatanew$availability_60)

#availability_90
datanew$availability_90=as.numeric(datanew$availability_90)
scoringDatanew$availability_90=as.numeric(scoringDatanew$availability_90)

#availability_365
datanew$availability_365=as.numeric(datanew$availability_365)
scoringDatanew$availability_365=as.numeric(scoringDatanew$availability_365)

#number_of_reviews
datanew$number_of_reviews=as.numeric(datanew$number_of_reviews)
scoringDatanew$number_of_reviews=as.numeric(scoringDatanew$number_of_reviews)

#number_of_reviews_ltm
datanew$number_of_reviews_ltm=as.numeric(datanew$number_of_reviews_ltm)
scoringDatanew$number_of_reviews_ltm=as.numeric(scoringDatanew$number_of_reviews_ltm)

#Beds
datanew$beds=as.numeric(datanew$beds)
scoringDatanew$beds=as.numeric(scoringDatanew$beds)

#Review_scores_rating
datanew$review_scores_rating=as.numeric(datanew$review_scores_rating)
scoringDatanew$review_scores_rating=as.numeric(scoringDatanew$review_scores_rating)

#Review scores accuracy
datanew$review_scores_accuracy=as.numeric(datanew$review_scores_accuracy)
scoringDatanew$review_scores_accuracy=as.numeric(scoringDatanew$review_scores_accuracy)

#Review scores cleanliness
datanew$review_scores_cleanliness=as.numeric(datanew$review_scores_cleanliness)
scoringDatanew$review_scores_cleanliness=as.numeric(scoringDatanew$review_scores_cleanliness)

#Review scores checkin
datanew$review_scores_checkin=as.numeric(datanew$review_scores_checkin)
scoringDatanew$review_scores_checkin=as.numeric(scoringDatanew$review_scores_checkin)

#Review scores communication
datanew$review_scores_communication=as.numeric(datanew$review_scores_communication)
scoringDatanew$review_scores_communication=as.numeric(scoringDatanew$review_scores_communication)

#Review scores location
datanew$review_scores_location=as.numeric(datanew$review_scores_location)
scoringDatanew$review_scores_location=as.numeric(scoringDatanew$review_scores_location)

#Review scores value
datanew$review_scores_value=as.numeric(datanew$review_scores_value)
scoringDatanew$review_scores_value=as.numeric(scoringDatanew$review_scores_value)

#Calculated host listings
datanew$calculated_host_listings_count=as.numeric(datanew$calculated_host_listings_count)
scoringDatanew$calculated_host_listings_count=as.numeric(scoringDatanew$calculated_host_listings_count)

#Calculated host listings entire homes
datanew$calculated_host_listings_count_entire_homes=as.numeric(datanew$calculated_host_listings_count_entire_homes)
scoringDatanew$calculated_host_listings_count_entire_homes=as.numeric(scoringDatanew$calculated_host_listings_count_entire_homes)

#Calculated host listings private rooms
datanew$calculated_host_listings_count_private_rooms=as.numeric(datanew$calculated_host_listings_count_private_rooms)
scoringDatanew$calculated_host_listings_count_private_rooms=as.numeric(scoringDatanew$calculated_host_listings_count_private_rooms)

#Calculated host listings shared rooms
datanew$calculated_host_listings_count_shared_rooms=as.numeric(datanew$calculated_host_listings_count_shared_rooms)
scoringDatanew$calculated_host_listings_count_shared_rooms=as.numeric(scoringDatanew$calculated_host_listings_count_shared_rooms)

#Checking classes of variables
str(datanew)



##STEP 16- Checking correlations between numeric variables-It didn't help at all

numericVars = which(sapply(datanew,is.numeric))#Isolating numeric variables
data_numVar = datanew[, numericVars]
ggcorrplot(cor(data_numVar),
           type = 'lower',
           show.diag = F,
           colors = c('red','white','darkgreen'))#This looks very messy but will decide my feature selection process


## STEP 17- Writing Clean CSVs

write.csv(datanew, "CleanAnalysis.csv",row.names = F)
write.csv(scoringDatanew, "CleanScoring.csv",row.names = F)

## STEP 18-Splitting Analysis data in train and test
data = read.csv('CleanAnalysis.csv',stringsAsFactors = T)
scoringData = read.csv('CleanScoring.csv',stringsAsFactors = T)

library(caret)
set.seed(1031)
split = createDataPartition(y=data$price,p = 0.7,list = F,groups = 100)
train = data[split,]
test = data[-split,]

## STEP 19-Using lasso for feature selection

library(caret);library(glmnet)
x=model.matrix(price~.,data=train)
dim(x)

y=train$price
str(y)

cv.lasso=cv.glmnet(x,y,alpha=1)
coef(cv.lasso)

## STEP 20  DATA MODELING
#Modeling using features selected by lasso after observing correlations with price

#Model 1 xgboost,rmse=59.54490

library(vtreat)
names(data)
data1=subset(data,select=-c(name,summary,description,neighborhood_overview,notes,
                            transit,access,interaction,host_about,
                            host_response_time,host_response_rate,
                            host_is_superhost,host_total_listings_count,
                            host_has_profile_pic,host_identity_verified,
                            zipcode,maximum_nights,availability_60,
                            availability_90,availability_365,
                            review_scores_rating,review_scores_accuracy,
                            review_scores_communication,review_scores_value,
                            instant_bookable,cancellation_policy,
                            require_guest_profile_picture,
                            require_guest_phone_verification,
                            calculated_host_listings_count_entire_homes,
                            total_host_verifications,
                            totalamenities,Wifi,Heating,Washer,Essentials,
                            Internet,real_bed))
scoringData1=subset(scoringData,select=-c(name,summary,description,neighborhood_overview,notes,
                            transit,access,interaction,host_about,
                            host_response_time,host_response_rate,
                            host_is_superhost,host_total_listings_count,
                            host_has_profile_pic,host_identity_verified,
                            zipcode,maximum_nights,availability_60,
                            availability_90,availability_365,
                            review_scores_rating,review_scores_accuracy,
                            review_scores_communication,review_scores_value,
                            instant_bookable,cancellation_policy,
                            require_guest_profile_picture,
                            require_guest_phone_verification,
                            calculated_host_listings_count_entire_homes,
                            total_host_verifications,
                            totalamenities,Wifi,Heating,Washer,Essentials,
                            Internet,real_bed))

library(caret)
set.seed(1031)
splitt = createDataPartition(y=data1$price,p = 0.7,list = F,groups = 100)
traint = data1[splitt,]
traint1=subset(traint,select=-price)
testt = data1[-splitt,]
testt1=subset(testt,select=-price)

library(xgboost)
trt = designTreatmentsZ(dframe = traint1,
                        varlist = names(traint1))
newvars = trt$scoreFrame[trt$scoreFrame$code%in% c('clean','lev'),'varName']
train_input = prepare(treatmentplan = trt,
                      dframe = traint1,
                      varRestriction = newvars)
test_input = prepare(treatmentplan = trt,
                     dframe = testt1,
                     varRestriction = newvars)
head(train_input)
set.seed(617)
tune_nrounds = xgb.cv(data=as.matrix(train_input),
                      label = traint$price,
                      nrounds=25,
                      nfold = 5,
                      verbose = 0)
which.min(tune_nrounds$evaluation_log$test_rmse_mean)
xgboost2= xgboost(data=as.matrix(train_input),
                  label = traint$price,
                  nrounds=25,
                  verbose = 0)
pred20 = predict(xgboost2,
               newdata=as.matrix(test_input))
rmse_xgboost = sqrt(mean((pred20 - testt$price)^2)); rmse_xgboost

#Predicting on scoring using entire dataset

data2=subset(data1,select=-price)
scoringData2=subset(scoringData1,select=-id)
trt = designTreatmentsZ(dframe = data2,
                        varlist = names(data2))
newvars = trt$scoreFrame[trt$scoreFrame$code%in% c('clean','lev'),'varName']
data_input = prepare(treatmentplan = trt,
                      dframe = data2,
                      varRestriction = newvars)
tune_nrounds = xgb.cv(data=as.matrix(data_input),
                      label = data1$price,
                      nrounds=25,
                      nfold = 5,
                      verbose = 0)
which.min(tune_nrounds$evaluation_log$test_rmse_mean)
xgboost2= xgboost(data=as.matrix(data_input),
                  label = data1$price,
                  nrounds=25,
                  verbose = 0)


trt = designTreatmentsZ(dframe = scoringData1,
                        varlist = names(scoringData1))
newvars1 = trt1$scoreFrame[trt1$scoreFrame$code%in% c('clean','lev'),'varName']

scoring_input = prepare(treatmentplan = trt,
                     dframe = scoringData2,
                     varRestriction = newvars)
pred21 = predict(xgboost2, newdata=as.matrix(scoring_input))
submissionFile = data.frame(id = scoringData1$id, price = pred21)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)

#Model 2 Best model, Forest Ranger without tuning,rmse=58.94117

forest_ranger = ranger(price~neighbourhood_group_cleansed+
                          is_location_exact+property_type+room_type+
                          accommodates+bathrooms+bedrooms+security_deposit+
                          cleaning_fee+guests_included+extra_people+
                          availability_30+minimum_nights+
                          review_scores_cleanliness+review_scores_location+
                          calculated_host_listings_count_private_rooms+
                          calculated_host_listings_count_shared_rooms+
                          reviews_per_month+TV+AC+Dryer+
                          Elevator+PremisesP+StreetP+Gym,data=train,
                        num.trees = 1000)
pred = predict(forest_ranger, data =test,num.trees = 1000)
rmse_forest_ranger = sqrt(mean((pred$predictions-test$price)^2)) 
rmse_forest_ranger

#Predicting on scoring using entire data set
forest_ranger1 = ranger(price~neighbourhood_group_cleansed+
                          is_location_exact+property_type+room_type+
                          accommodates+bathrooms+bedrooms+security_deposit+
                          cleaning_fee+guests_included+extra_people+
                          availability_30+minimum_nights+
                          review_scores_cleanliness+review_scores_location+
                          calculated_host_listings_count_private_rooms+
                          calculated_host_listings_count_shared_rooms+
                          reviews_per_month+TV+AC+Dryer+
                          Elevator+PremisesP+StreetP+Gym,data=data,
                        num.trees = 1000)
pred1 = predict(forest_ranger1, data =scoringData,num.trees = 1000)
submissionFile = data.frame(id = scoringData$id, price = pred1$predictions)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)


################################  THE END   ###############################
