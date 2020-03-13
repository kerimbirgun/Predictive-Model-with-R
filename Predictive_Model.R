library(dplyr); library(readr);library(caret);library(stringr);
library(ggplot2);library(qdap);library(dplyr);library(tm);library(wordcloud);library(plotrix)
library(dendextend);library(ggplot2);library(ggthemes);library(RWeka)
library(reshape2);library(quanteda)


setwd('C:/Users/USER01/Desktop/R/Kaggle')
data_main = read.csv('analysisData.csv')
dataq=data_main


dataq$last_review = NULL
dataq$first_review =NULL
dataq$host_name = NULL
dataq$license = NULL
dataq$calendar_updated = NULL
dataq$host_verifications = NULL
dataq$square_feet = NULL
dataq$square_feet = NULL
dataq$monthly_price = NULL
dataq$country = NULL
dataq$country_code = NULL
dataq$id = NULL
dataq$host_about = NULL
dataq$jurisdiction_names = NULL
dataq$has_availability = NULL
dataq$requires_license = NULL
dataq$is_business_travel_ready = NULL
dataq$host_acceptance_rate = NULL
dataq$host_response_time = NULL
dataq$state = NULL
dataq$market = NULL
# low correlation
dataq$maximum_minimum_nights = NULL
dataq$minimum_maximum_nights = NULL
dataq$maximum_maximum_nights = NULL
dataq$number_of_reviews = NULL
dataq$availability_30 = NULL
dataq$availability_60 = NULL
dataq$availability_90 = NULL

#location related
dataq$zipcode = NULL
dataq$smart_location = NULL
dataq$city = NULL
dataq$street = NULL
dataq$host_neighbourhood = NULL
dataq$host_location = NULL
dataq$neighbourhood = NULL

# keep null until feature engineering
dataq$name = NULL
dataq$space = NULL
dataq$notes = NULL
dataq$transit = NULL
dataq$summary = NULL
dataq$interaction = NULL
dataq$house_rules = NULL
dataq$host_since = NULL
dataq$neighborhood_overview = NULL

#accomodate binning
dataq$accommo_binned = NA
dataq$accommo_binned = as.numeric(dataq$accommodates)
dataq$accommo_binned = ifelse(dataq$accommodates == 1, 1, ifelse(dataq$accommodates == 2, 2,
                                                                 ifelse(dataq$accommodates == 3, 3, ifelse(dataq$accommodates == 4, 4,
                                                                                                           ifelse(dataq$accommodates > 4 & dataq$accommodates < 9, 5, 6 )))))


# correlation matrix for numerics
correlation_matrix = data.frame(cor(dataq[,unlist(lapply(dataq, is.numeric))]))
write.csv(correlation_matrix, 'airbnbcorrelation2.csv',row.names = F)

#Quantifying Factor Variables
dataq$host_has_profile_pic = as.character(dataq$host_has_profile_pic)
dataq$host_has_profile_pic = ifelse(dataq$host_has_profile_pic == 't','1','0')
dataq$host_has_profile_pic = as.factor(dataq$host_has_profile_pic)
dataq$host_has_profile_pic = as.factor(dataq$host_has_profile_pic)

dataq$host_is_superhost = as.character(dataq$host_is_superhost)
dataq$host_is_superhost = ifelse(dataq$host_is_superhost == 't','1','0')
dataq$host_is_superhost = as.factor(dataq$host_is_superhost)
dataq$host_is_superhost = as.numeric(dataq$host_is_superhost)

dataq$instant_bookable = as.character(dataq$instant_bookable)
dataq$instant_bookable = ifelse(dataq$instant_bookable == 't','1','0')
dataq$instant_bookable = as.factor(dataq$instant_bookable)
dataq$instant_bookable = as.factor(dataq$instant_bookable)

dataq$require_guest_profile_picture = as.character(dataq$require_guest_profile_picture)
dataq$require_guest_profile_picture = ifelse(dataq$require_guest_profile_picture == 't','1','0')
dataq$require_guest_profile_picture = as.factor(dataq$require_guest_profile_picture)
dataq$require_guest_profile_picture = as.factor(dataq$require_guest_profile_picture)

dataq$require_guest_phone_verification = as.character(dataq$require_guest_phone_verification)
dataq$require_guest_phone_verification = ifelse(dataq$require_guest_phone_verification == 't','1','0')
dataq$require_guest_phone_verification = as.factor(dataq$require_guest_phone_verification)
dataq$require_guest_phone_verification = as.factor(dataq$require_guest_phone_verification)

dataq$is_location_exact = as.character(dataq$is_location_exact)
dataq$is_location_exact = ifelse(dataq$is_location_exact == 't','1','0')
dataq$is_location_exact = as.factor(dataq$is_location_exact)
dataq$is_location_exact = as.factor(dataq$is_location_exact)

dataq$host_identity_verified = as.character(dataq$host_identity_verified)
dataq$host_identity_verified = ifelse(dataq$host_identity_verified == 't','1','0')
dataq$host_identity_verified = as.factor(dataq$host_identity_verified)
dataq$host_identity_verified = as.factor(dataq$host_identity_verified)

dataq$host_response_rate = as.character(dataq$host_response_rate)
dataq$host_response_rate = str_replace_all(dataq$host_response_rate,'N/A','0%')
dataq$host_response_rate = str_replace_all(dataq$host_response_rate,'%','')
dataq$host_response_rate = as.integer(dataq$host_response_rate)

dataq$review_scores_accuracy = NULL
dataq$host_is_superhost = NULL
dataq$is_location_exact = NULL
dataq$review_scores_communication = NULL
dataq$minimum_nights = NULL
dataq$require_guest_phone_verification = NULL
dataq$minimum_nights_avg_ntm = NULL
dataq$review_scores_checkin = NULL
dataq$maximum_nights = NULL
dataq$maximum_nights_avg_ntm = NULL
dataq$host_identity_verified = NULL
dataq$require_guest_profile_picture = NULL
dataq$host_has_profile_pic = NULL
dataq$number_of_reviews_ltm = NULL
dataq$review_scores_value = NULL
dataq$reviews_per_month = NULL
dataq$instant_bookable = NULL

# host_total_listings_count
dataq$host_list_final = NA
dataq$host_list_final = dataq$host_total_listings_count
dataq$host_list_final = as.character(dataq$host_list_final)
dataq$host_list_final = ifelse(dataq$host_total_listings_count < 6,1,
                               ifelse(dataq$host_total_listings_count  >5 & dataq$host_total_listings_count  < 250 , 2,
                                      ifelse(dataq$host_total_listings_count  >249 & dataq$host_total_listings_count <500, 3, 
                                             ifelse(dataq$host_total_listings_count  >499 & dataq$host_total_listings_count <1000,4,5))))
dataq$host_list_final = as.factor(dataq$host_list_final)


# minimum minimum nights
str(dataq$minimum_minimum_nights)
dataq %>% select(price, minimum_minimum_nights) %>% group_by(minimum_minimum_nights) %>% summary(minimum_minimum_nights)

dataq$min_night = NA
dataq$min_night = ifelse(dataq$minimum_minimum_nights < 70,1,
                         ifelse(dataq$minimum_minimum_nights  >69 & dataq$minimum_minimum_nights  < 105 , 2,
                                ifelse(dataq$minimum_minimum_nights  >104 & dataq$minimum_minimum_nights  <173, 3, 4)))

str(dataq$min)
cor(dataq$price, dataq$min)
dataq$minimum_minimum_nights = NULL

#Amenities

dataq$amenities = as.character(dataq$amenities)
dataq$amenity_wcount = NA
dataq$amenity_wcount = as.integer(nchar(dataq$amenities))

dataq %>% select(price, amenity_wcount) %>% group_by(amenity_wcount) %>% summary(amenity_wcount)

dataq$amenity_wcount4 = ifelse(dataq$amenity_wcount < 70,1,
                               ifelse(dataq$amenity_wcount >69 & dataq$amenity_wcount < 137 , 2,
                                      ifelse(dataq$amenity_wcount >136 & dataq$amenity_wcount < 173 , 3, 4)))

dataq$amenity_wcount4 = as.integer(dataq$amenity_wcount4)

cor(dataq$price,dataq$amenity_wcount4)
cor(dataq$price,dataq$amenity_wcount)
dataq$amenity_wcount4 = NULL

#description
dataq$description = as.character(dataq$description)
dataq$descr_count = NA
dataq$descr_count = as.integer(nchar(dataq$description))

dataq %>% select(price, descr_count) %>% group_by(descr_count) %>% summary(descr_count)

dataq$descr_final = NA
dataq$descr_final = ifelse(dataq$descr_count < 1050 ,0,1)

cor(dataq$price,dataq$descr_final)
dataq$descr_final = NULL
dataq$description = NULL

# Convert NA fees to 0
dataq$cleaning_fee[is.na(dataq$cleaning_fee)] <- 0
c= data.frame(dataq %>% select(cleaning_fee,price) %>% 
                group_by(cleaning_fee) %>% summarise(med = mean(price) , n = length(cleaning_fee)))
glimpse(c)

dataq$cleanfee_new = ifelse(dataq$cleaning_fee == 0, '1', ifelse(dataq$cleaning_fee > 0 & dataq$cleaning_fee <51,'2',
                                                                 ifelse(dataq$cleaning_fee > 50 & dataq$cleaning_fee <151,'3', ifelse(dataq$cleaning_fee > 150 & dataq$cleaning_fee <301,
                                                                                                                                      '4','5'))))
dataq$cleanfee_new = as.factor(dataq$cleanfee_new)
dataq$cleanfee_new = as.numeric(dataq$cleanfee_new)


dataq$security_deposit[is.na(dataq$security_deposit)] <- 0

dataq$security_deposit[is.na(dataq$security_deposit)] <- 0
dataq$security_deposit = as.character(dataq$security_deposit)
table(dataq$neighbourhood_group_cleansed)

dataq$nb = NA
dataq$nb = dataq$neighbourhood_group_cleansed
str(dataq$security_deposit)
dataq$nb = as.character(dataq$nb)
dataq$security_deposit = ifelse(dataq$security_deposit == '0' & dataq$nb == 'Bronx','60',
                                ifelse(dataq$security_deposit == '0' & dataq$nb == 'Brooklyn','91',
                                       ifelse(dataq$security_deposit == '0' & dataq$nb == 'Manhattan','145',
                                              ifelse(dataq$security_deposit == '0' & dataq$nb == 'Queens','72','70'))))
dataq$security_deposit = as.numeric(dataq$security_deposit)
dataq$nb = NULL

cor(dataq$price, dataq$security_deposit)
cor(dataq$price, dataq$cleaning_fee)

# Binning - "#cancellation policy"
dataq$cancellation_policy = as.character(dataq$cancellation_policy)
dataq$cancellation_policy = ifelse(dataq$cancellation_policy == 'flexible','1',ifelse(dataq$cancellation_policy == 'moderate','2','3'))
dataq$cancellation_policy = as.factor(dataq$cancellation_policy)

# Binning - "#property type"
a= dataq %>% select(property_type,price) %>% group_by(property_type) %>% summarise(med = median(price))
a = data.frame(a)
write.csv(a, 'medianoroperty.csv',row.names = F)

dataq$property_typeold = dataq$property_type
dataq$property_type = as.character(dataq$property_type)
dataq$property_type = ifelse(dataq$property_type == 'Lighthouse'| dataq$property_type == 'Houseboat'
                             | dataq$property_type == 'Timeshare' | dataq$property_type == 'Resort','1', 
                             ifelse(dataq$property_type == 'Cottage'| dataq$property_type == 'Tent'
                                    | dataq$property_type == 'Hotel' | dataq$property_type == 'Serviced apartment'
                                    | dataq$property_type == 'Condominium' | dataq$property_type == 'Loft'
                                    | dataq$property_type == 'Boutique hotel' | dataq$property_type == 'Boat','2',
                                    ifelse(dataq$property_type == 'Cave'| dataq$property_type == 'Other'
                                           | dataq$property_type == 'Apartment' | dataq$property_type == 'Nature lodge'
                                           | dataq$property_type == 'Bungalow' | dataq$property_type == 'Guest suite'
                                           | dataq$property_type == 'Townhouse' | dataq$property_type == 'Cabin' | dataq$property_type == 'Camper/RV','3',
                                           ifelse(dataq$property_type == 'Bed and breakfast'| dataq$property_type == 'Aparthotel'
                                                  | dataq$property_type == 'Guesthouse' | dataq$property_type == 'House'
                                                  | dataq$property_type == 'Hostel' | dataq$property_type == 'Loft'
                                                  | dataq$property_type == 'Boutique hotel' | dataq$property_type == 'Earth house','4',
                                                  ifelse(dataq$property_type == 'Tiny house'|dataq$property_type == 'Dome house'
                                                         | dataq$property_type == 'Villa' |dataq$property_type == 'Pension (South Korea)','5','6')))))  

dataq$property_type = as.factor(dataq$property_type)


# Binning - "Location"
b= dataq %>% select(neighbourhood_cleansed,price) %>% group_by(neighbourhood_cleansed) %>% summarise(med = median(price) , n = length(neighbourhood_cleansed))
write.csv(b, 'neighbour.csv',row.names = F)

dataq$neighbourhood_before = NA
dataq$neighbourhood_before = dataq$neighbourhood_group_cleansed
dataq$neighbourhood_cleansed = as.character(dataq$neighbourhood_cleansed)

dataq$neighbourhood_cleansed = ifelse(dataq$neighbourhood_cleansed == 'Neponsit'|dataq$neighbourhood_cleansed 
                                      == 'Tribeca'| dataq$neighbourhood_cleansed == 'Willowbrook','1',ifelse(dataq$neighbourhood_cleansed == 'NoHo','2',
                                                                                                             ifelse(dataq$neighbourhood_cleansed == 'Flatiron District','3',ifelse(dataq$neighbourhood_cleansed == 'Chelsea'|
                                                                                                                                                                                     dataq$neighbourhood_cleansed == 'West Village'| dataq$neighbourhood_cleansed == 'SoHo','4',
                                                                                                                                                                                   ifelse(dataq$neighbourhood_cleansed == 'Theater District'|dataq$neighbourhood_cleansed == 'Breezy Point'| 
                                                                                                                                                                                            dataq$neighbourhood_cleansed == 'DUMBO','5',ifelse(dataq$neighbourhood_cleansed == 'Midtown','6',
                                                                                                                                                                                                                                               ifelse(dataq$neighbourhood_cleansed == 'Greenwich Village'|dataq$neighbourhood_cleansed == 'Tottenville'| 
                                                                                                                                                                                                                                                        dataq$neighbourhood_cleansed == 'Financial District' | dataq$neighbourhood_cleansed == 'Nolita', '7',
                                                                                                                                                                                                                                                      ifelse(dataq$neighbourhood_cleansed == 'Murray Hill'|dataq$neighbourhood_cleansed == 'Battery Park City'| 
                                                                                                                                                                                                                                                               dataq$neighbourhood_cleansed == 'Belle Harbor','8',ifelse(dataq$neighbourhood_cleansed == 'Holliswood'| 
                                                                                                                                                                                                                                                                                                                           dataq$neighbourhood_cleansed == 'Gramercy'| dataq$neighbourhood_cleansed == 'Brooklyn Heights' |
                                                                                                                                                                                                                                                                                                                           dataq$neighbourhood_cleansed == 'Sea Gate' | dataq$neighbourhood_cleansed == 'Kips Bay' | 
                                                                                                                                                                                                                                                                                                                           dataq$neighbourhood_cleansed == 'Lighthouse Hill','9',
                                                                                                                                                                                                                                                                                                                         ifelse(dataq$neighbourhood_cleansed == "Hell's Kitchen"| dataq$neighbourhood_cleansed == 'East Village'| 
                                                                                                                                                                                                                                                                                                                                  dataq$neighbourhood_cleansed == 'Upper West Side' | dataq$neighbourhood_cleansed == 'Carroll Gardens' | 
                                                                                                                                                                                                                                                                                                                                  dataq$neighbourhood_cleansed == 'Stuyvesant Town' | dataq$neighbourhood_cleansed == 'Navy Yard' | 
                                                                                                                                                                                                                                                                                                                                  dataq$neighbourhood_cleansed == 'Riverdale' | dataq$neighbourhood_cleansed == 'Lower East Side' , '10' ,
                                                                                                                                                                                                                                                                                                                                ifelse(dataq$neighbourhood_cleansed == 'Civic Center' , '11' ,ifelse(dataq$neighbourhood_cleansed == 
                                                                                                                                                                                                                                                                                                                                                                                                       'Vinegar Hill'| dataq$neighbourhood_cleansed == 'Upper East Side'| dataq$neighbourhood_cleansed == 
                                                                                                                                                                                                                                                                                                                                                                                                       'Park Slope' | dataq$neighbourhood_cleansed == 'Boerum Hill' | dataq$neighbourhood_cleansed == 
                                                                                                                                                                                                                                                                                                                                                                                                       'Little Italy' | dataq$neighbourhood_cleansed == 'Cobble Hill' | dataq$neighbourhood_cleansed ==
                                                                                                                                                                                                                                                                                                                                                                                                       'Bay Terrace' , '12' ,ifelse(dataq$neighbourhood_cleansed == 'South Slope'| dataq$neighbourhood_cleansed == 
                                                                                                                                                                                                                                                                                                                                                                                                                                      'Downtown Brooklyn'| dataq$neighbourhood_cleansed == 'Chinatown' | dataq$neighbourhood_cleansed == 
                                                                                                                                                                                                                                                                                                                                                                                                                                      'Columbia St' | dataq$neighbourhood_cleansed == 'Fort Greene' | dataq$neighbourhood_cleansed == 'Gowanus' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                      dataq$neighbourhood_cleansed == 'Huguenot' | dataq$neighbourhood_cleansed == 'Manhattan Beach' , '13' ,
                                                                                                                                                                                                                                                                                                                                                                                                                                    ifelse(dataq$neighbourhood_cleansed == 'Clinton Hill' | dataq$neighbourhood_cleansed == 'Windsor Terrace' ,'14', 
                                                                                                                                                                                                                                                                                                                                                                                                                                           ifelse(dataq$neighbourhood_cleansed == 'Prospect Heights'| dataq$neighbourhood_cleansed == 'Gerritsen Beach'| 
                                                                                                                                                                                                                                                                                                                                                                                                                                                    dataq$neighbourhood_cleansed == 'Greenpoint' | dataq$neighbourhood_cleansed == "Prince's Bay" | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                    dataq$neighbourhood_cleansed == 'Grymes Hill' , '15' ,ifelse(dataq$neighbourhood_cleansed == 'Williamsburg'| 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   dataq$neighbourhood_cleansed == 'Two Bridges' | dataq$neighbourhood_cleansed == 'Rosebank' |
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   dataq$neighbourhood_cleansed == 'Grymes Hill' , '16' ,ifelse(dataq$neighbourhood_cleansed == 'East Harlem'| 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  dataq$neighbourhood_cleansed == 'Red Hook'| dataq$neighbourhood_cleansed == 'Arverne' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  dataq$neighbourhood_cleansed == 'Unionport' | dataq$neighbourhood_cleansed == 'Spuyten Duyvil' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  dataq$neighbourhood_cleansed == 'Morningside Heights' | dataq$neighbourhood_cleansed == 'Rockaway Beach' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  dataq$neighbourhood_cleansed == 'Middle Village' | dataq$neighbourhood_cleansed == 'Great Kills' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  dataq$neighbourhood_cleansed == 'Todt Hill' | dataq$neighbourhood_cleansed == 'Glen Oaks' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  dataq$neighbourhood_cleansed == 'Bergen Beach' | dataq$neighbourhood_cleansed == 'City Island' , '17',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ifelse(dataq$neighbourhood_cleansed == 'Howard Beach' | dataq$neighbourhood_cleansed == 'Harlem' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         dataq$neighbourhood_cleansed == 'Long Island City' | dataq$neighbourhood_cleansed == 'Mariners Harbor' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         dataq$neighbourhood_cleansed == 'Whitestone' | dataq$neighbourhood_cleansed == 'Ditmars Steinway' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         dataq$neighbourhood_cleansed == 'Forest Hills' , '18' ,ifelse(dataq$neighbourhood_cleansed == 'Hollis'| 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         dataq$neighbourhood_cleansed == 'Kew Gardens Hills'| dataq$neighbourhood_cleansed == 'Crown Heights' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         dataq$neighbourhood_cleansed == 'Astoria' | dataq$neighbourhood_cleansed == 'Prospect-Lefferts Gardens' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         dataq$neighbourhood_cleansed == 'Roosevelt Island' | dataq$neighbourhood_cleansed == 'Ozone Park' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         dataq$neighbourhood_cleansed == 'Pelham Bay' | dataq$neighbourhood_cleansed == 'Shore Acres' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         dataq$neighbourhood_cleansed == 'Midland Beach' | dataq$neighbourhood_cleansed == 'Mill Basin' , '19',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ifelse(dataq$neighbourhood_cleansed == 'Throgs Neck'| dataq$neighbourhood_cleansed == 'Van Nest'|
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                dataq$neighbourhood_cleansed == 'Bedford-Stuyvesant' | dataq$neighbourhood_cleansed == 'Bay Terrace, Staten Island' 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | dataq$neighbourhood_cleansed == 'East New York' | dataq$neighbourhood_cleansed == 'Fort Hamilton' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                dataq$neighbourhood_cleansed == 'Richmondtown' | dataq$neighbourhood_cleansed == 'Dongan Hills' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                dataq$neighbourhood_cleansed == 'Sunset Park' | dataq$neighbourhood_cleansed == 'Bay Ridge' , '20' ,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ifelse(dataq$neighbourhood_cleansed == 'Brighton Beach'| dataq$neighbourhood_cleansed == 'Marble Hill'| 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       dataq$neighbourhood_cleansed == 'Canarsie' | dataq$neighbourhood_cleansed == 'Bayside' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       dataq$neighbourhood_cleansed == 'Bayswater' | dataq$neighbourhood_cleansed == 'East Flatbush' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       dataq$neighbourhood_cleansed == 'Springfield Gardens' | dataq$neighbourhood_cleansed == 'Glendale' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       dataq$neighbourhood_cleansed == 'Arrochar' | dataq$neighbourhood_cleansed == 'West Brighton' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       dataq$neighbourhood_cleansed == 'Edenwald' | dataq$neighbourhood_cleansed == 'Baychester' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       dataq$neighbourhood_cleansed == 'Co-op City' | dataq$neighbourhood_cleansed == 'Rossville' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       dataq$neighbourhood_cleansed == 'Mott Haven' | dataq$neighbourhood_cleansed == 'Kingsbridge' , '21' ,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ifelse(dataq$neighbourhood_cleansed == 'Queens Village' | dataq$neighbourhood_cleansed == 'Sunnyside' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dataq$neighbourhood_cleansed == 'South Ozone Park' | dataq$neighbourhood_cleansed == 'Grant City' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dataq$neighbourhood_cleansed == 'Concourse' | dataq$neighbourhood_cleansed == 'St. George' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dataq$neighbourhood_cleansed == 'Graniteville' , '22' ,ifelse(dataq$neighbourhood_cleansed == 'Gravesend'| 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dataq$neighbourhood_cleansed == 'Washington Heights'| dataq$neighbourhood_cleansed == 'Flatbush' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dataq$neighbourhood_cleansed == 'Kensington' | dataq$neighbourhood_cleansed == 'Maspeth' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dataq$neighbourhood_cleansed == 'Bensonhurst' | dataq$neighbourhood_cleansed == 'Pelham Gardens' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dataq$neighbourhood_cleansed == 'Bath Beach' | dataq$neighbourhood_cleansed == 'Clifton' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dataq$neighbourhood_cleansed == 'Jackson Heights' | dataq$neighbourhood_cleansed == 'Fresh Meadows' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dataq$neighbourhood_cleansed == 'Dyker Heights' | dataq$neighbourhood_cleansed == 'Randall Manor' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dataq$neighbourhood_cleansed == 'Midwood' | dataq$neighbourhood_cleansed == 'Westchester Square' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dataq$neighbourhood_cleansed == 'Coney Island' | dataq$neighbourhood_cleansed == 'Cypress Hills' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dataq$neighbourhood_cleansed == 'South Beach' | dataq$neighbourhood_cleansed == 'Eltingville' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dataq$neighbourhood_cleansed == 'Flatlands' | dataq$neighbourhood_cleansed == 'Tompkinsville' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dataq$neighbourhood_cleansed == 'West Farms ', '23' ,ifelse(dataq$neighbourhood_cleansed == 'Bushwick'| 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            dataq$neighbourhood_cleansed == 'Castle Hill'| dataq$neighbourhood_cleansed == 'Ridgewood' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            dataq$neighbourhood_cleansed == 'Jamaica' | dataq$neighbourhood_cleansed == 'Woodside' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            dataq$neighbourhood_cleansed == 'Jamaica Estates' | dataq$neighbourhood_cleansed == 'Laurelton' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            dataq$neighbourhood_cleansed == 'Morris Park' | dataq$neighbourhood_cleansed == 'East Morrisania' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            dataq$neighbourhood_cleansed == 'Douglaston' | dataq$neighbourhood_cleansed == 'Bellerose' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            dataq$neighbourhood_cleansed == 'Jamaica Hills' | dataq$neighbourhood_cleansed == 'Oakwood', '24' ,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ifelse(dataq$neighbourhood_cleansed == 'Rosedale'| dataq$neighbourhood_cleansed == 'Rego Park'| 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   dataq$neighbourhood_cleansed == 'Flushing' | dataq$neighbourhood_cleansed == 'Sheepshead Bay' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   dataq$neighbourhood_cleansed == 'Richmond Hill' | dataq$neighbourhood_cleansed == 'Brownsville' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   dataq$neighbourhood_cleansed == 'Longwood' | dataq$neighbourhood_cleansed == 'Fordham' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   dataq$neighbourhood_cleansed == 'Williamsbridge' | dataq$neighbourhood_cleansed == 'Kew Gardens' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   dataq$neighbourhood_cleansed == 'Wakefield' | dataq$neighbourhood_cleansed == 'Concourse Village' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   dataq$neighbourhood_cleansed == 'College Point' | dataq$neighbourhood_cleansed == 'Eastchester' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   dataq$neighbourhood_cleansed == 'Fieldston' | dataq$neighbourhood_cleansed == 'Morrisania' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   dataq$neighbourhood_cleansed == 'New Springville' , '25' ,ifelse(dataq$neighbourhood_cleansed == 'Elmhurst'|
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      dataq$neighbourhood_cleansed == 'St. Albans'| dataq$neighbourhood_cleansed == 'Briarwood' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      dataq$neighbourhood_cleansed == 'Cambria Heights' | dataq$neighbourhood_cleansed == 'Woodlawn' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      dataq$neighbourhood_cleansed == 'Bronxdale' | dataq$neighbourhood_cleansed == 'Claremont Village' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      dataq$neighbourhood_cleansed == 'Morris Heights' | dataq$neighbourhood_cleansed == 'Edgemere' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      dataq$neighbourhood_cleansed == 'North Riverdale' | dataq$neighbourhood_cleansed == 'Castleton Corners' , '26' ,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ifelse(dataq$neighbourhood_cleansed == 'Norwood'| dataq$neighbourhood_cleansed == 'Silver Lake'|
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             dataq$neighbourhood_cleansed == 'East Elmhurst' | dataq$neighbourhood_cleansed == 'Stapleton' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             dataq$neighbourhood_cleansed == 'Arden Heights' | dataq$neighbourhood_cleansed == 'Port Morris' |
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             dataq$neighbourhood_cleansed == 'Allerton' | dataq$neighbourhood_cleansed == 'University Heights' |
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             dataq$neighbourhood_cleansed == 'Mount Hope' | dataq$neighbourhood_cleansed == 'New Brighton' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             dataq$neighbourhood_cleansed == 'Olinville' , '27' ,ifelse(dataq$neighbourhood_cleansed == 'Borough Park'| 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          dataq$neighbourhood_cleansed == 'Parkchester'| dataq$neighbourhood_cleansed == 'Highbridge' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          dataq$neighbourhood_cleansed == 'Woodhaven' | dataq$neighbourhood_cleansed == 'Far Rockaway' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          dataq$neighbourhood_cleansed == 'Melrose' | dataq$neighbourhood_cleansed == 'Emerson Hill' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          dataq$neighbourhood_cleansed == 'Clason Point' | dataq$neighbourhood_cleansed == 'Belmont' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          dataq$neighbourhood_cleansed == 'Little Neck' | dataq$neighbourhood_cleansed == 'Soundview' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          dataq$neighbourhood_cleansed == 'Pleasant Plains', '28' ,ifelse(dataq$neighbourhood_cleansed == 'Corona'|
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            dataq$neighbourhood_cleansed == 'Port Richmond'| dataq$neighbourhood_cleansed == 'Mount Eden' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            dataq$neighbourhood_cleansed == 'Hunts Point' | dataq$neighbourhood_cleansed == 'Tremont' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            dataq$neighbourhood_cleansed == 'Westerleigh' | dataq$neighbourhood_cleansed == 'Schuylerville' | 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            dataq$neighbourhood_cleansed == 'Concord' , '29' ,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ifelse(dataq$neighbourhood_cleansed == "Bull's Head" ,'30', '31' ))))))))))))))))))))))))))))))

dataq$neighbourhood_cleansed = as.factor(dataq$neighbourhood_cleansed)
dataq$neighbourhood_group_cleansed = as.numeric(dataq$neighbourhood_group_cleansed)


#############################  Text Mining  (Amenities)  ########################3

dataq$amenities = as.character(dataq$amenities)

corpus_review=Corpus(VectorSource(dataq$amenities))
corpus_review=tm_map(corpus_review, tolower)
corpus_review=tm_map(corpus_review, removePunctuation)
corpus_review=tm_map(corpus_review, removeWords, stopwords("english"))
corpus_review=tm_map(corpus_review, stemDocument)
term_count <- freq_terms(corpus_review, 20)

# dtm & tdm

review_dtm <- DocumentTermMatrix(corpus_review)
review_tdm <- TermDocumentMatrix(corpus_review)

review_m <- as.matrix(review_tdm)

#glimpse(review_m); dim(review_m)
review_term_freq <- rowSums(review_m)
review_term_freq <- sort(review_term_freq, decreasing = T)
review_term_freq[1:10]

##Create bi-grams
review_bigram <- tokens(dataq$amenities) %>%
  tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
  tokens_remove(stopwords("english"), padding  = TRUE) %>%
  tokens_ngrams(n = 2) %>%
  dfm()
topfeatures(review_bigram)

###Tokenization (Amenity Variable)

# Tokenize descriptions
reviewtokens=tokens(dataq$amenities,what="word",
                    remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE, remove_hyphens=TRUE)
# Lowercase the tokens
reviewtokens=tokens_tolower(reviewtokens)
# remove stop words and unnecessary words
rmwords <- c("dress", "etc", "also", "xxs", "xs", "s")
reviewtokens=tokens_select(reviewtokens, stopwords(),selection = "remove")
reviewtokens=tokens_remove(reviewtokens,rmwords)
# Stemming tokens
reviewtokens=tokens_wordstem(reviewtokens,language = "english")
reviewtokens=tokens_ngrams(reviewtokens,n=1:6)

# Creating a bag of words
reviewtokensdfm=dfm(reviewtokens,tolower = FALSE)
# Remove sparsity
reviewSparse <- convert(reviewtokensdfm, "tm")
tm::removeSparseTerms(reviewSparse, 0.7)
# Create the dfm
dfm_trim(reviewtokensdfm, min_docfreq = 0.3)
x=dfm_trim(reviewtokensdfm, sparsity = 0.98)

## Setup a dataframe with features
df=convert(x,to="data.frame")

positives = c('iron_laptop_friend', 'elev', 'tv_wifi_air',  'gym', 'gym_elev','play_travel','tv','friend_washer_dryer','dryer','friend_washer',
              'kid_friend_washer','friend','heat_famili','heat_famili_kid',
              'washer_dryer','famili','kid','famili_kid','kid_friend',
              'famili_kid_friend','washer_dryer_smoke','dryer_smoke','dryer_smoke_detector','washer','maker_refriger_dishwash','dryer_iron_laptop','tv_cabl','tv_cabl_tv','dishwash','cabl','cabl_tv','refriger_dishwash','refriger_dishwash_dish','dishwash_dish','dishwash_dish_silverwar')

positivedf <- df[,colnames(df) %in% positives] 

positivedf$total = rowSums(positivedf, na.rm=T)


negatives = c('heat_smoke','heat_smoke_detector','bedroom','lock_bedroom','bedroom_door','calculated_host_listings_count_private_rooms','wifi_kitchen','lock','door','essenti_lock','essenti_lock_bedroom','miss_translat','translat_miss_translat',
              'miss_translat_miss','translat','miss','translat_miss','park_heat_smoke','door_hanger','bedroom_door_hanger',
              'weekly_price','kitchen_free','calculated_host_listings_count_shared_rooms','park_heat','street_park_heat')

negativedf <- df[,colnames(df) %in% negatives] 

negativedf[negativedf==1]=-1
negativedf[negativedf==2]=-1
negativedf[negativedf==3]=-1
negativedf$totalneg = rowSums(negativedf, na.rm=T)


consolidated_df = cbind(positivedf,negativedf)
consolidated_df$finalsum = positivedf$total + negativedf$totalneg

consolidated_df$finalsum_bin = NA 
consolidated_df$finalsum_bin = as.character(consolidated_df$finalsum)
consolidated_df$finalsum_bin  = ifelse(consolidated_df$finalsum_bin < -10,1,
                                       ifelse(consolidated_df$finalsum_bin >-11 & consolidated_df$finalsum_bin < 0 , 2,
                                              ifelse(consolidated_df$finalsum_bin >-1 & consolidated_df$finalsum_bin <20,3,4))) 

consolidated_df$finalsum_bin = as.factor(consolidated_df$finalsum_bin)

finalcolumns = c('finalsum_bin','finalsum','elev','dryer_iron_laptop',
                 'washer','dryer','dryer_smoke','gym_elev','tv','kitchen_free','park_heat_smoke','gym'
                 ,'play_travel','dryer_iron_laptop','dishwash_dish','refriger_dishwash')

consolidated_df <- consolidated_df[,colnames(consolidated_df) %in% finalcolumns] 
xyz = cbind(consolidated_df,dataq)
datap = xyz


# Create correlation matrix and eliminate additional variables before prediction

correlation_matrix = data.frame(cor(datap[,unlist(lapply(datap, is.numeric))]))
write.csv(correlation_matrix, 'airbnbcorrelation3.csv',row.names = F)


datap$review_multiplied = NA
datap$review_multiplied = datap$review_scores_cleanliness * datap$review_scores_location * datap$review_scores_rating

datap$highpredictors = NA
datap$highpredictors = datap$accommo_binned * datap$bedrooms * datap$bathrooms * datap$cleanfee_new 

datap$review_scores_cleanliness = NULL
datap$calculated_host_listings_count_private_rooms = NULL
datap$descr_count = NULL
datap$host_response_rate = NULL
datap$amenity_wcount4 = NULL
datap$calculated_host_listings_count_entire_homes = NULL
datap$host_listings_count = NULL
datap$amenities = NULL
datap$access = NULL
datap$document = NULL
datap$heat_essenti_shampoo = NULL
datap$dryer_iron_self = NULL
datap$hair_dryer_iron_self = NULL
datap$dryer_iron_self_check = NULL
datap$blanket_microwav_refriger = NULL
datap$stay_allow_clean = NULL
datap$kitchen_elev_free = NULL
datap$property_typeold = NULL
datap$washer = NULL
datap$dryer = NULL
datap$gym_elev = NULL
datap$weekly_price = NULL

datap = na.omit(datap)
Airbnb_Final = write.csv(datap,"Airbnb_Final")

#########################    #PREDICTION   ##################

library(caret):library(rpart):library(rpart.plot)
set.seed(1031)
split = createDataPartition(datap$price,p = 0.7, list = F)
train = datap[split,]
test = datap[-split,]

## Build the CART model
tree=rpart(price ~ ., data = train, method="anova",
           control = rpart.control(minsplit = 200,  
                                   minbucket = 30, cp = 0.0001))
printcp(tree)
plotcp(tree)
##Prune down the tree
bestcp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
ptree=prune(tree,cp=bestcp)

rpart.plot(ptree,cex = 0.6)
prp(ptree, faclen = 0, cex = 0.5, extra = 2)

pred = predict(ptree)
rmse = sqrt(mean((pred-train$price)^2)); rmse

pred2 = predict(ptree,newdata = test)
rmse2 = sqrt(mean((pred2-test$price)^2)); rmse2

#Final Prediction
tree=rpart(price ~ ., data = datap, method="anova",
           control = rpart.control(minsplit = 200,  
                                   minbucket = 30, cp = 0.0001))
bestcp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
ptree=prune(tree,cp=bestcp)


#linear model

lm = lm(price~.,data=train) 

pred = predict(lm)
rmse = sqrt(mean((pred-train$price)^2)); rmse

pred2 = predict(lm,newdata = test)
rmse2 = sqrt(mean((pred2-test$price)^2)); rmse2

summary(lm)

#Random Forest model
library(randomForest)
set.seed(617)
forest = randomForest(price~.,data=train,ntree = 500)

pred = predict(forest)
rmse = sqrt(mean((pred-train$price)^2)); rmse

pred2 = predict(forest,newdata = test)
rmse2 = sqrt(mean((pred2-test$price)^2)); rmse2

importance(forest) # relative importance of predictors (highest <-> most important)
varImpPlot(forest) # plot results

# Boosting Model
library(gbm)
set.seed(617)
boosted_model = gbm(price~.,data=train,verbose = TRUE,shrinkage = 0.01,  
                    interaction.depth = 3,n.minobsinnode = 5, n.trees = 10000,cv.folds = 20)

pred = predict(boosted_model)
rmse = sqrt(mean((pred-train$price)^2)); rmse

pred2 = predict(boosted_model,newdata = test)
rmse2 = sqrt(mean((pred2-test$price)^2)); rmse2

forest_top30 = c('price','highpredictors','neighbourhood_cleansed','room_type','bedrooms','accommodates','cleaning_fee','accommo_binned','bathrooms','amenity_wcount','neighbourhood_before','availability_365',
                 'finalsum','cleanfee_new','review_multiplied','extra_people','property_type','review_scores_rating','guests_included','beds','host_total_listings_count','calculated_host_listings_count','cancellation_policy',
                 'tv','security_deposit','finalsum_bin','review_scores_location','neighbourhood_group_cleansed','elev','gym','dryer_smoke')
datap <- datap[,colnames(datap) %in% forest_top30] 

