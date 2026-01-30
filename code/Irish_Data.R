#Jakub Pawelczak 2024-'Flipping Houses in a Decentralized Market' using Irish data 2010-2024

#Whenever I use Irish_data is raw data set of transactions in Ireland between 2010-2024 
#it has 6491569 observations
#Whenever I use Irish_data_sorted is  data set of relevant  transactions in Ireland between 2010-2024 
#it has 638779 observations
#Whenever I use Irish_data_flipper is  data set of repeated transactions in Ireland between 2010-2024
#it has 59768 observations
#To lad environment use
#load("~/Desktop/Praca/Minnesota 5y/Flippers/irish data/plots/Untitled.RData")
rm(list = ls())
#rm(list = setdiff(ls(), "Irish_data"))
# Set seed for reproducibility
set.seed(123)
#Load Irish transaction data on houses between 2010 and 2024
Irish_data <- read.csv("/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/PPR-ALL2.csv")
CPI2012 <- read.csv("/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/CPI2012.csv")
Rental <- read.csv("/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/RENTAL.csv")
QRental <- read.csv("/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/QRENTAL.csv")

setwd("/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/plots")
#Basic data management: change names of columns, extract year, edit addresses into separate columns
names(Irish_data)[1] <- "Date"
names(Irish_data)[5] <- "Price"
names(Irish_data)[8] <- "NewHouse"
names(Irish_data)[9] <- "Size"

# When was fisr Eircode assigned? 1/1/21
#earliest_date <- min(Irish_data[Irish_data$Eircode != "", ]$Date, na.rm = TRUE)
#print(earliest_date)

####Date
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("stringr")) install.packages("stringr")
library(dplyr)
library(lubridate)
library(stringr)

Irish_data <- Irish_data %>%
  mutate(
    # Extract the day as the first number before the first "/"
    Day = as.integer(str_extract(Date, "^[0-9]{1,2}")),
    
    # Extract the month as the number between the first and second "/"
    Month = as.integer(str_extract(Date, "(?<=/)[0-9]{1,2}(?=/)")),
    
    # Extract the year as the number after the last "/"
    # and add 2000 if it's a two-digit year to interpret it as 2000s
    Year = as.integer(str_extract(Date, "(?<=/)[0-9]{2,4}$")),
    Year = ifelse(Year < 100, Year + 2000, Year)
  )

Irish_data <- Irish_data %>%
  mutate(
    # Create a Date column from Day, Month, Year
    Full_Date = make_date(Year, Month, Day),
    
    # Calculate the difference in days from December 31, 2009
    Date_as_integer = as.integer(Full_Date - make_date(2009, 12, 31))
  )

Irish_data$Price <- as.numeric(gsub(",", "",Irish_data$Price))
Irish_data$Not.Full.Market.Price  <- ifelse(Irish_data$Not.Full.Market.Price == "Yes", TRUE, FALSE)
Irish_data$VAT.Exclusive  <- ifelse(Irish_data$VAT.Exclusive == "Yes", TRUE, FALSE)
Irish_data$NewHouse  <- ifelse(Irish_data$NewHouse == "New Dwelling house /Apartment", TRUE, FALSE)

Irish_data <- Irish_data %>%
  mutate(Size = case_when(
    Size %in% c("greater than or equal to 125 sq metres", "greater than 125 sq metres") ~ ">125 sq m",
    Size %in% c("n\xedos m\xf3 n\xe1 n\xf3 cothrom le 38 m\xe9adar cearnach agus n\xedos l\xfa n\xe1 125 m\xe9adar cearnach", "greater than or equal to 38 sq metres and less than 125 sq metres") ~ ">38 <125 sq m",
    Size %in% c("less than 38 sq metres", "n?os l? n? 38 m?adar cearnach") ~ "<38 sq m",
    TRUE ~ Size
  ))
############Addresses- separate number, street, district, city, dublin
if (!require("stringi")) install.packages("stringi")

library(stringi)

Irish_data$Address <- stri_trans_general(Irish_data$Address, "Latin-ASCII")
Irish_data$Address <- tolower(Irish_data$Address)

if (!require("tidyr")) install.packages("tidyr")
library(tidyr)


split_address <- function(address) {
  # Initialize parts of the address
  apartment <- NA
  street <- NA
  district <- NA
  city <- NA
  dublin <- NA
  
  # Identify if the address is in Dublin and extract postal code
  if(str_detect(address, "(?i)dublin")) {
    city <- "Dublin"
    dublin <- str_extract(address, "\\d+")
    if(is.na(dublin) | nchar(dublin) == 0) dublin <- NA
  }
  
  # Clean the address by removing common prefixes and extracting apartment number
  prefix_pattern <- "(?i)^\\s*(no\\.\\s*|apt\\s*|unit\\s*|no\\s*|number\\s*|house\\s*|apartment\\s*|site\\s*)?(\\d+[a-z]?\\s*)"
  if(str_detect(address, prefix_pattern)) {
    apartment <- str_extract(address, "\\d+[a-z]?")
    address <- str_replace(address, prefix_pattern, "")
  }
  
  # Split the address into components
  address_parts <- str_split(address, ",\\s*")[[1]]
  
  # Assign the address components based on their count
  len <- length(address_parts)
  if(len == 3) {
    street <- address_parts[1]
    district <- address_parts[2]
    city <- ifelse(is.na(city), address_parts[3], city)
  } else if(len == 2) {
    street <- address_parts[1]
    city <- ifelse(is.na(city), address_parts[2], city)
  } else if(len == 1) {
    # Decide whether it's likely a street or city based on presence of apartment number
    if(!is.na(apartment)) {
      street <- address_parts[1]
    } else {
      city <- ifelse(is.na(city), address_parts[1], city)
    }
  }
  
  list(apartment = apartment, street = street, district = district, city = city, dublin = dublin)
}

# Apply the split function using lapply
address_parts <- lapply(Irish_data$Address, split_address)


Irish_data$apartment <- sapply(address_parts, `[[`, "apartment")
Irish_data$street <- sapply(address_parts, `[[`, "street")
Irish_data$district <- sapply(address_parts, `[[`, "district")
Irish_data$city <- sapply(address_parts, `[[`, "city")
Irish_data$dublin <- sapply(address_parts, `[[`, "dublin")

rm(address_parts)






# Count the number of commas in each address
#comma_counts <- str_count(Irish_data$Address, ",")

# Count how many addresses have 1, 2, and 3 commas
#one_comma <- sum(comma_counts == 1)
#two_commas <- sum(comma_counts == 2)
#three_commas <- sum(comma_counts == 0)

# Print the counts
#cat("Number of addresses with 1 comma:", one_comma, "\n")
#cat("Number of addresses with 2 commas:", two_commas, "\n")
#cat("Number of addresses with 0 commas:", three_commas, "\n")
#print(sample(Irish_data$Address[str_count(Irish_data$Address, ",")==2], size = 100, replace = FALSE))

# Updated split_address function to handle new specifications
#split_address <- function(address) {
# Initial split by commas and trim
#  parts <- strsplit(address, ",")[[1]]
#  parts <- trimws(parts)
#  len <- length(parts)

# Initialize variables
#  street <- NA
#  district <- NA
#  city <- NA
#  dublin <- NA

# Remove specified prefixes globally
#  address_cleaned <- str_remove_all(address, "(?i)\\b(no\\. |apt |n\\. |n |apartment|number)")

# Re-split and trim after cleaning
#  parts_cleaned <- strsplit(address_cleaned, ",")[[1]]
#  parts_cleaned <- trimws(parts_cleaned)

#  if (len == 1) {
#    street <- parts[1]
#  } else if (len == 2) {
#    street <- parts[1]
#    city <- parts[2]
#  } else if (len == 3) {
# Find parts starting with a number
#    number_starts <- sapply(parts_cleaned, function(part) str_detect(part, "^[0-9]"))

#    if (sum(number_starts) >= 1) {
#      street_parts <- parts_cleaned[number_starts]
#      street <- paste(street_parts, collapse = " ")
#      non_street_parts <- parts_cleaned[!number_starts]
#      if (length(non_street_parts) >= 2) {
#        district <- non_street_parts[1]
#        city <- non_street_parts[2]
#      } else {
#        city <- non_street_parts[1]
#      }
#    } else {
#      street <- parts_cleaned[1]
#      district <- parts_cleaned[2]
#      city <- parts_cleaned[3]
#    }

# Special handling for Dublin with a number
#    if (!is.na(city) &&str_detect(city, "(?i)dublin") && str_detect(city, "\\d")) {
#      dublin <- str_extract(city, "\\d+")
#      city <- str_remove(city, "\\d+")
#    }
#  }

#  if (len > 3) {
#    return(NA) # Returning NA for addresses with more than two commas
#  }

#  return(list(street = street, district = district, city = city, dublin = dublin))
#}

#address_parts <- lapply(seq_along(Irish_data$Address), function(i) {
#  if (i %% 50000 == 0) {
#    cat("Processing row:", i, "\n")
#  }
#  split_address(Irish_data$Address[i])
#})
# WHen I more patient I use - Apply the updated function
#address_parts <- lapply(Irish_data$Address, split_address)

# Create new columns based on the processed address parts
#Irish_data$street <- sapply(address_parts, `[[`, "street")
#Irish_data$district <- sapply(address_parts, `[[`, "district")
#Irish_data$city <- sapply(address_parts, `[[`, "city")
#Irish_data$dublin <- sapply(address_parts, `[[`, "dublin")

# Sample 100 rows randomly (if the dataset has fewer than 100 rows, adjust the number accordingly)
#random_rows <- sample(nrow(Irish_data), min(100, nrow(Irish_data)))
# Select specific columns and print the sampled data
#print(Irish_data[random_rows, c( "street", "district", "city", "Date_as_integer")])

########end of editing addresses############
table(Irish_data$city)
##########Sort addresses and Date_as_integer to identify Flippers
#create data frame with Time_diff time difference from last trade and 0 if there was no trades
Irish_data_sorted <- Irish_data %>%
  arrange(apartment, street, district, city, Date_as_integer) %>% # maybe arrange(street, district, city, Date_as_integer)
  group_by(apartment, street, district, city) %>% # group_by(street, district, city, Date_as_integer)
  mutate(
    Time_diff = if_else(
      row_number() == 1, 
      0.0, # Assign 0.0 (double) to the first row in each group
      as.double(Date_as_integer - lag(Date_as_integer, default = NA)) # Calculate diff for subsequent rows and convert to double
    )
  ) %>%
  ungroup()

#check if Time_diff went through
#sum(Irish_data_sorted$Time_diff)
#Irish_data_sorted[306:307, c("street", "district", "city", "Date_as_integer", "Time_diff")]
# Filter out rows where Time_diff is NA using complete.cases()
#Irish_data_sorted <- Irish_data_sorted[complete.cases(Irish_data_sorted$Time_diff), ]

# Eliminate apartment buildings sold in bulk with identical address on the same day
Irish_data_sorted <- Irish_data_sorted %>%
  group_by(apartment,street,district, city) %>%
  mutate(is_first_row = row_number() == 1) %>%
  # Filter out rows where Time_diff < 30, except for the first row in each group
  filter(Time_diff >= 30 | is_first_row) %>%
  # Optionally, remove the helper column if no longer needed
  select(-is_first_row) %>%
  ungroup()

# Joining CPI2012 data with Irish_data_sorted on 'Year' and 'Month'
Irish_data_sorted <- merge(Irish_data_sorted, CPI2012, by = c("Year", "Month"))

# Calculating Price2012
Irish_data_sorted$Price2012 <- (Irish_data_sorted$Price / Irish_data_sorted$CPI) * 100

# Before renaming, ensure there's no column named "Price_nom" to avoid overwriting
if("Price_nom" %in% names(Irish_data_sorted)) {
  names(Irish_data_sorted)[names(Irish_data_sorted) == "Price_nom"] <- "Price_nom_old"
}

# 3. Rename the 'Price' column to 'Price_nom'
names(Irish_data_sorted)[names(Irish_data_sorted) == "Price"] <- "Price_nom"

# Ensure there's no column named "Price" before renaming "Price2012" to avoid overwriting
if("Price" %in% names(Irish_data_sorted)) {
  names(Irish_data_sorted)[names(Irish_data_sorted) == "Price"] <- "Price_old"
}

# 4. Rename 'Price2012' column to 'Price'
names(Irish_data_sorted)[names(Irish_data_sorted) == "Price2012"] <- "Price"


#calculate Price_Ratio whenever u find two leg transaction
Irish_data_sorted <- Irish_data_sorted %>%
  group_by(apartment,street, district, city) %>%
  mutate(Price_Ratio = if_else(Time_diff - lag(Time_diff) != 0,
                               (Price / lag(Price))^(365 / (Time_diff - lag(Time_diff))),
                               0)) %>%
  ungroup()

#if above does not work try:
#Irish_data_sorted <- Irish_data_sorted %>%
#  group_by(Address) %>%
#  mutate(
#    Prev_Time_diff = lag(Time_diff),
#    Diff = Time_diff - lag(Time_diff),
#    Condition = !is.na(Diff) & Diff != 0 & Diff>=90,
#    Price_Ratio = if_else(Condition,
#                          (Price / lag(Price))^(365 / Diff),
#                          1) # Use 1 instead of 0 to differentiate from false conditions
#  ) %>%
#  ungroup()



#Create Flipper variable-if house was sold faster (not bought faster) than 730 but after 90 days assign 1 if later 2,0 otherwise
Irish_data_sorted <- Irish_data_sorted %>%
  mutate(Flipper = case_when(
    Time_diff >= 90 & Time_diff <= 730 ~ 1,
    Time_diff > 730 ~ 2,
    Time_diff < 90 ~ 0
  ))
#alternative 1 year def of flipping
Irish_data_sorted <- Irish_data_sorted %>%
  mutate(Flipper1 = case_when(
    Time_diff >= 90 & Time_diff <= 365 ~ 1,
    Time_diff > 365 ~ 2,
    Time_diff < 90 ~ 0
  ))
#alternative 4 year def of flipping
Irish_data_sorted <- Irish_data_sorted %>%
  mutate(Flipper4 = case_when(
    Time_diff >= 90 & Time_diff <= 1460 ~ 1,
    Time_diff > 1460 ~ 2,
    Time_diff < 90 ~ 0
  ))


#for price index calculation find expenditure share in that year
Irish_data_sorted <- Irish_data_sorted %>%
  # Ensure you group by Year to perform operations within each Year
  group_by(Year) %>%
  # Calculate Expenditure_Share with conditions
  mutate(Expenditure_Share = case_when(
    Time_diff == 0  ~ 0, # Conditions for assigning 0
    TRUE ~ Price / sum(Price[Time_diff > 0 ], na.rm = TRUE) # Default case
  )) %>%
  # Ungroup to remove the grouping structure
  ungroup()

#########Identify Flipped houses as parts in two leg transactions
#####


#assign to column B 1 if it was buyer of flipped transaction ,2 of second leg normal transaction, 0 if wasnt buyer in two leg transction
#assign to column S 1 if it was buyer of flipped transaction ,2 of second leg normal transaction, 0 if wasnt buyer in two leg transction
#Fi is  function of BS its 0 if this house has never been resold
# flipped if  1  and 2 if resold reboutgh but later than 2 y

Irish_data_sorted <- Irish_data_sorted %>%
  arrange(apartment,street,district, city, Date_as_integer) %>%
  group_by(apartment,street,district, city) %>%
  mutate(
    B = case_when(
      lead(Date_as_integer) - Date_as_integer >= 30 & lead(Date_as_integer) - Date_as_integer <= 730 ~ 1,
      lead(Date_as_integer) - Date_as_integer > 730 ~ 2,
      TRUE ~ 0
    )
  ) %>%
  ungroup()
Irish_data_sorted <- Irish_data_sorted %>%
  arrange(apartment, street,district, city, Date_as_integer) %>%
  group_by(apartment,street,district, city) %>%
  mutate(
    S = case_when(
      Date_as_integer - lag(Date_as_integer) >= 30 & Date_as_integer - lag(Date_as_integer) <= 730 ~ 1,
      Date_as_integer - lag(Date_as_integer) > 730 ~ 2,
      TRUE ~ 0
    )
  ) %>%
  ungroup()

Irish_data_sorted <- Irish_data_sorted %>%
  mutate(
    Fi = case_when(
      B == 0 & S == 0 ~ 0,
      B == 0 & S == 1 ~ 1,
      B == 0 & S == 2 ~ 2,
      B == 1 & S == 0 ~ 1,
      B == 1 & S == 1 ~ 1,
      B == 1 & S == 2 ~ 1,
      B == 2 & S == 0 ~ 2,
      B == 2 & S == 1 ~ 1,
      B == 2 & S == 2 ~ 2
    )
  )
#alternative 1 year
Irish_data_sorted <- Irish_data_sorted %>%
  arrange(apartment,street,district, city, Date_as_integer) %>%
  group_by(apartment,street,district, city) %>%
  mutate(
    B1 = case_when(
      lead(Date_as_integer) - Date_as_integer >= 30 & lead(Date_as_integer) - Date_as_integer <= 365 ~ 1,
      lead(Date_as_integer) - Date_as_integer > 365 ~ 2,
      TRUE ~ 0
    )
  ) %>%
  ungroup()
Irish_data_sorted <- Irish_data_sorted %>%
  arrange(apartment, street,district, city, Date_as_integer) %>%
  group_by(apartment,street,district, city) %>%
  mutate(
    S1 = case_when(
      Date_as_integer - lag(Date_as_integer) >= 30 & Date_as_integer - lag(Date_as_integer) <= 365 ~ 1,
      Date_as_integer - lag(Date_as_integer) > 365 ~ 2,
      TRUE ~ 0
    )
  ) %>%
  ungroup()

Irish_data_sorted <- Irish_data_sorted %>%
  mutate(
    Fi1 = case_when(
      B1 == 0 & S1 == 0 ~ 0,
      B1 == 0 & S1 == 1 ~ 1,
      B1 == 0 & S1 == 2 ~ 2,
      B1 == 1 & S1 == 0 ~ 1,
      B1 == 1 & S1 == 1 ~ 1,
      B1 == 1 & S1 == 2 ~ 1,
      B1 == 2 & S1 == 0 ~ 2,
      B1 == 2 & S1 == 1 ~ 1,
      B1 == 2 & S1 == 2 ~ 2
    )
  )
#alternative 4 years

Irish_data_sorted <- Irish_data_sorted %>%
  arrange(apartment,street,district, city, Date_as_integer) %>%
  group_by(apartment,street,district, city) %>%
  mutate(
    B4 = case_when(
      lead(Date_as_integer) - Date_as_integer >= 30 & lead(Date_as_integer) - Date_as_integer <= 1460 ~ 1,
      lead(Date_as_integer) - Date_as_integer > 1460 ~ 2,
      TRUE ~ 0
    )
  ) %>%
  ungroup()
Irish_data_sorted <- Irish_data_sorted %>%
  arrange(apartment, street,district, city, Date_as_integer) %>%
  group_by(apartment,street,district, city) %>%
  mutate(
    S4 = case_when(
      Date_as_integer - lag(Date_as_integer) >= 30 & Date_as_integer - lag(Date_as_integer) <= 1460 ~ 1,
      Date_as_integer - lag(Date_as_integer) > 1460 ~ 2,
      TRUE ~ 0
    )
  ) %>%
  ungroup()

Irish_data_sorted <- Irish_data_sorted %>%
  mutate(
    Fi4 = case_when(
      B4 == 0 & S4 == 0 ~ 0,
      B4 == 0 & S4 == 1 ~ 1,
      B4 == 0 & S4 == 2 ~ 2,
      B4 == 1 & S4 == 0 ~ 1,
      B4 == 1 & S4 == 1 ~ 1,
      B4 == 1 & S4 == 2 ~ 1,
      B4 == 2 & S4 == 0 ~ 2,
      B4 == 2 & S4 == 1 ~ 1,
      B4 == 2 & S4 == 2 ~ 2
    )
  )



# Generate Quarter variable as a numerical value
Irish_data_sorted <- Irish_data_sorted %>%
  mutate(Quarter = ceiling(Month / 3))



############################geocoding 
if (!require("png")) install.packages("png", type = "source")
if (!require("ggmap")) install.packages("ggmap", type = "source")
if (!require("data.table")) install.packages("data.table")

library(png)
library(ggmap)
library(data.table)
# Merge the map data with your average prices data
ireland_map <- map_data("world", "Ireland")
#map_data <- merge(ireland_map, average_prices_county, by = "County", all.x = TRUE)

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("rnaturalearth")) install.packages("rnaturalearth")
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata")
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Load Ireland counties map
ireland_counties <- ne_states(country = "ireland", returnclass = "sf")

#change names of three counties: north tipperary, south tipperary and laoighis to tipperary , tipperary and laois
row_to_change <- which(ireland_counties$name == "North Tipperary")
# Change the county name to 'Tipperary'
if (length(row_to_change) == 1) {
  ireland_counties$name[row_to_change] <- "Tipperary"
} else {
  message("Either no row or multiple rows found, please check the condition.")
}
# Check the change
print(ireland_counties$name[row_to_change])
row_to_change <- which(ireland_counties$name == "South Tipperary")
# Change the county name to 'Tipperary'
if (length(row_to_change) == 1) {
  ireland_counties$name[row_to_change] <- "Tipperary"
} else {
  message("Either no row or multiple rows found, please check the condition.")
}
# Check the change
print(ireland_counties$name[row_to_change])
row_to_change <- which(ireland_counties$name == "Laoighis")
# Change the county name to 'Laois'
if (length(row_to_change) == 1) {
  ireland_counties$name[row_to_change] <- "Laois"
} else {
  message("Either no row or multiple rows found, please check the condition.")
}
# Check the change
print(ireland_counties$name[row_to_change])
# Check the data
row_to_change <- which(ireland_counties$name == "South Dublin")
# Change the county name to 'Dublin'
if (length(row_to_change) == 1) {
  ireland_counties$name[row_to_change] <- "Dublin"
} else {
  message("Either no row or multiple rows found, please check the condition.")
}
# Check the change
#print(ireland_counties$name[row_to_change])
#print(head(ireland_counties))



#Find all non empty rows Eircode with identical addresses which have different Eircodes
# Step 1: Filter out rows with empty Eircode
filtered_data <- Irish_data_sorted %>%
  filter(Eircode != "")
# Step 2 & 3: Group by Address and filter groups with more than one unique Eircode
result <- filtered_data %>%
  group_by(Address) %>%
  filter(n_distinct(Eircode) > 1)
# Step 4: View the result
#print(result)
#what i did previously is geting rid of 
############without getting rid of those with too high and too low returns


# Delete Irish_data_sorted  rows with addresses that occur less than once and more than 5

#Irish_data_flipper <- Irish_data_flipper %>%
#  group_by(Address) %>%
#  filter(5>n() ,n()>1) %>%
#  ungroup()
#Irish_data_flipper2 <- Irish_data_flipper2 %>%
#  group_by(Address) %>%
#  filter(5>n() & n() > 1) %>%
#  ungroup()

######################
########Now we are done with data science part or editing data
#############
# For each year calculate unique number of houses traded in Irealand
unique_addresses_year <- Irish_data_sorted %>%
  group_by(Year) %>%
  summarise(Unique_Addresses = n_distinct(paste(apartment,street,district, city)))
unique_addresses_year
# In total
total_unique_addresses <- n_distinct(paste( Irish_data_sorted$apartment,Irish_data_sorted$street, Irish_data_sorted$district,Irish_data_sorted$city))
total_unique_addresses

# Calculate the frequency of each address
address_frequencies <- Irish_data_sorted%>%
  group_by(apartment,street,district, city) %>%
  summarize(Frequency = n()) %>%
  ungroup()
# Step 2: Calculate the frequency of each frequency value
frequency_of_frequencies <- address_frequencies %>%
  group_by(Frequency) %>%
  summarize(Count = n()) %>%
  ungroup()
print(frequency_of_frequencies)
graphics.off()

# Plotting the distribution of address frequencies
plot<-ggplot(address_frequencies, aes(x = Frequency)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +  # Adjust binwidth as needed
  labs(title = "Distribution of Address Frequencies ",
       x = "Frequency of Addresses",
       y = "Count of Addresses") +
  theme(plot.title = element_text(size = 20), # Increase plot title text size
        axis.title = element_text(size = 14), # Increase axis title text size
        legend.title = element_text(size = 12), # Increase legend title text size
        legend.text = element_text(size = 10)) # Increase legend text size
plot
png("pic0.png", width = 10, height = 8, res = 300, units = "in")
print(plot)
dev.off()


#Look at frequencies
table(Irish_data_sorted$County  )
table(Irish_data_sorted$Not.Full.Market.Price  )
table(Irish_data_sorted$VAT.Exclusive  )
table(Irish_data_sorted$NewHouse)
table(Irish_data_sorted$Size  )
#head( sort(table(Irish_data$Eircode), decreasing = TRUE), 10)

#nr of houses never retraded, flipped and sold bought later
sum(Irish_data_sorted$Fi == 0)
sum(Irish_data_sorted$Fi ==1)
sum(Irish_data_sorted$Fi ==2)
#fraction of houses never retraded
sum(Irish_data_sorted$Fi == 0)/nrow(Irish_data_sorted)
#fraction of houses flipped 
sum(Irish_data_sorted$Fi ==1) / nrow(Irish_data_sorted)
#fraction of houses  retraded but not flipped
sum(Irish_data_sorted$Fi ==2)/nrow(Irish_data_sorted)

# Count where B=1 and NewHouse=FALSE
count_b1_newhouse_false <- Irish_data_sorted %>%
  filter(B == 1, NewHouse == FALSE, Fi == 1) %>%
  nrow()

# Count where S=1 and NewHouse=TRUE
count_s1_newhouse_true <- Irish_data_sorted %>%
  filter(S == 1, NewHouse == TRUE, Fi == 1) %>%
  nrow()

# Total count where Fi=1
total_fi_1 <- Irish_data_sorted %>%
  filter(Fi== 1) %>%
  nrow()

# Calculate the proportions
proportion_b1_newhouse_false <- count_b1_newhouse_false / total_fi_1
proportion_s1_newhouse_true <- count_s1_newhouse_true / total_fi_1


#####
##plot nr of flipped transactions
#####

# Filter data for relevant years and calculate the fraction of flipped trades
flipped_fi <- Irish_data_sorted %>%
  filter(Year >= 2012 & Year <= 2021) %>%
  group_by(Year) %>%
  summarise(Fraction_Fi = sum(Fi == 1) / n())

flipped_fi1 <- Irish_data_sorted %>%
  filter(Year >= 2011 & Year <= 2022) %>%
  group_by(Year) %>%
  summarise(Fraction_Fi1 = sum(Fi1 == 1) / n())

flipped_fi4 <- Irish_data_sorted %>%
  filter(Year >= 2014 & Year <= 2019) %>%
  group_by(Year) %>%
  summarise(Fraction_Fi4 = sum(Fi4 == 1) / n())

# Combine the data for plotting
flipped_data <- full_join(flipped_fi, flipped_fi1, by = "Year") %>%
  full_join(flipped_fi4, by = "Year")
graphics.off()
# Create the plot
flipped_plot <- ggplot(flipped_data, aes(x = Year)) +
  geom_line(aes(y = Fraction_Fi), color = "red", size = 3) +
  scale_x_continuous(breaks = seq(2012, 2021, by = 1)) +  # Ensure each year is marked
  ylab("Fraction of total trade") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  # Increase font size of x-axis label
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  )
flipped_plot
# Save the plot as an SVG
ggsave("p1.png", plot = flipped_plot, width = 10, height = 6, dpi = 300)


library(ggplot2)

# Replace this vector with the actual extracted data for home flipping rates from the image
years <- seq(2000, 2023)
flipping_rates <- c(6.2, 6.0, 5.8, 6.0, 6.3, 7.0, 8.5, 10.0, 9.5, 8.0, 7.0, 6.5, 6.0, 
                    5.8, 5.7, 5.6, 5.8, 6.0, 6.3, 6.5, 7.0, 8.0, 8.5, 8.0)  # Example data

# Create a data frame
flipping_data <- data.frame(
  Year = years,
  Flipping_Rate = flipping_rates
)

# Create the plot
flipped_plot <- ggplot(flipping_data, aes(x = Year)) +
  geom_line(aes(y = Flipping_Rate), color = "blue", size = 3) +
  scale_x_continuous(breaks = seq(2000, 2023, by = 3)) +  # Adjust x-axis breaks for the data range
  scale_y_continuous(breaks = seq(0, 12, by = 1)) +       # Adjust y-axis breaks for flipping rates
  ylab("Fraction of total trade") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(flipped_plot)

# Save the plot
ggsave("p1USA.png", plot = flipped_plot, width = 10, height = 6, dpi = 300)

# Create the plot
flipped_plot2 <- ggplot(flipped_data, aes(x = Year)) +
  geom_line(aes(y = Fraction_Fi), color = "red", size = 3) +
  geom_line(aes(y = Fraction_Fi1), color = "blue", size = 3, linetype = "dotted") +  # Second line dotted
  geom_line(aes(y = Fraction_Fi4), color = "black", size = 3, linetype = "dashed") +  # Third line dashed
  scale_x_continuous(breaks = seq(2011, 2023, by = 1)) +  # Ensure each year is marked
  ylab("Fraction of flipped trades") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  # Increase font size of x-axis label
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  )
flipped_plot2
# Save the plot as an SVG
ggsave("p2.png", plot = flipped_plot2, width = 10, height = 6, dpi = 300)


####
###plot nr of trades each year
###

# Calculate the number of trades per year from 2010 to 2023
trades_per_year <- Irish_data_sorted %>%
  filter(Year >= 2012 & Year <= 2021) %>%
  group_by(Year) %>%
  summarise(Num_Trades = n())
graphics.off()
# Create the plot
trades_plot <- ggplot(trades_per_year, aes(x = Year, y = Num_Trades)) +
  scale_x_continuous(breaks = seq(2012, 2021, by = 1)) +  # Ensure each year is marked
  geom_line(color = "black", size = 3) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  # Increase font size of x-axis label
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  )+
  ylab("Number of trades")
trades_plot
# Save the plot as an SVG
ggsave("p3.png", plot = trades_plot, width = 10, height = 6, dpi = 300)


##################
################# PREPARE DATA FOR STATA - to do hedonic regression
#################

# Install and load haven package if not already installed
if (!require("haven")) install.packages("haven")
library(haven)
# Step 1: Count observations for each city
city_counts <- Irish_data_sorted %>%
  group_by(city) %>%
  summarise(n = n(), .groups = "drop")

# Step 2: Identify cities with at least 10 observations
valid_cities <- city_counts %>%
  filter(n >= 10) %>%
  pull(city)

# Step 3: Filter the data for these valid cities
filtered_data <- Irish_data_sorted %>%
  filter(city %in% valid_cities)

# Select the relevant variables from Irish_data_sorted
final_data <- Irish_data_sorted %>%
  select(apartment,street, district, city, dublin,County, Time_diff, Price, Price_Ratio, Fi,B,S,B1,S1,B4,S4, Fi1, Fi4, Year, Quarter)

#########
#########
# I did one change added apartment above in final_data and rerun the stata file flipper_hedonic.do file
########
# Save the data to the specified path
write_dta(final_data,  "/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/individual_price_data.dta")
#################
#################
##WORK IN STATA - file flipper0_R2_specification.do###
#################
#################

# Load necessary libraries
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
library(readxl)

# Define the path to the Excel file
file_path <- "/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/plots/locations_R2.xlsx"

# Read the R-squared values from the Excel file
r2_data <- read_excel(file_path)

# Start the LaTeX table string
latex_table <- "\\begin{table}[h]
    \\centering
    \\caption{Variation Explained by Observable}
    \\begin{tabular}{lc}
        \\toprule
        \\textbf{Fixed Effects} & \\textbf{$R^2$} \\\\
        \\midrule
"

# Loop through each row in the data frame and add it to the LaTeX table string
# Loop through each row in the data frame using column indices
for (i in 1:nrow(r2_data)) {
  latex_table <- paste0(latex_table, sprintf("        %s & %.6f \\\\\n", r2_data[i, 1], r2_data[i, 2]))
}

# Finish the LaTeX table string
latex_table <- paste0(latex_table, "        \\bottomrule\n    \\end{tabular}\n\\end{table}")

# Define the path to save the LaTeX file
output_path <- "/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/plots/R2_FE.tex"

# Save the LaTeX table to a .tex file
writeLines(latex_table, output_path)



#################
#################
##WORK IN STATA - file flipper1_load_hedonic.do###
#################
#################

# Install and load necessary packages
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

library(openxlsx)
library(dplyr)


# Load the Excel file
results <- read.xlsx("/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/HFCS ECB data/HFCS_UDB_2_5_STATA/results2013.xlsx")
results <- results %>%
  mutate(Value = ifelse(as.numeric(Value) > 1000, round(as.numeric(Value), -3), round(as.numeric(Value), 2)))

# Generate LaTeX table content
latex_table <- "\\begin{table}[h!]
\\centering
\\begin{tabular}{lll}
  \\hline
Variable & Moment & Value \\\\ 
  \\hline
    \\hline
"

# Append the data rows
for (i in 1:nrow(results)) {
  latex_table <- paste0(latex_table, results$Variable[i], " & ", results$Moment[i], " & ", results$Value[i], " \\\\ \n")
}

# Close the table
latex_table <- paste0(latex_table, "  \\hline
\\end{tabular}
\\end{table}
")

# Define the path to save the .tex file
tex_file_path <- "/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/plots/table1.tex"

# Write the LaTeX table to the .tex file
write(latex_table, file = tex_file_path)
#################
#################
##WORK IN STATA - file flipper1_load_hedonic.do###
#################
#################
# Load the individual_price_data2 dataset directly from the specified path
individual_price_data2 <- read_dta("/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/individual_price_data2.dta")

Irish_data_sorted_temp <- as.data.frame(individual_price_data2)
Irish_data_sorted2<-Irish_data_sorted
Irish_data_sorted<-Irish_data_sorted_temp



# Create a new dataframe with 'Lag_Average_Price'
Irish_data_test <- Irish_data_sorted %>%
  group_by(apartment, street, district, city) %>%
  mutate(Lag_Average_Price = lag(Average_Price)) %>%
  ungroup()

Irish_data_test <- Irish_data_sorted %>%
  group_by(apartment, street, district, city) %>%
  mutate(Lag_Average_Price = lag(log_Price)) %>%
  ungroup()

# Run regression for observations with B == 1
regression_B <- lm(Lag_Average_Price ~ Time_diff, data = subset(Irish_data_test, B == 1))
beta_B <- coef(regression_B)["Time_diff"]
cat("Beta for B == 1:", beta_B, "\n")

# Handle missing values for correlation calculation
subset_B <- subset(Irish_data_test, B == 1)
subset_B <- subset_B[complete.cases(subset_B$Lag_Average_Price, subset_B$Time_diff), ]

# Recalculate correlation
cor_B <- cor(subset_B$Lag_Average_Price, subset_B$Time_diff)
cat("Correlation for B == 1:", cor_B, "\n")


# Run regression for observations with S == 1
regression_S <- lm(Lag_Average_Price ~ Time_diff, data = subset(Irish_data_test, S == 1))
beta_S <- coef(regression_S)["Time_diff"]

# Handle missing values for correlation calculation
subset_S <- subset(Irish_data_test, S == 1)
subset_S <- subset_S[complete.cases(subset_S$Lag_Average_Price, subset_S$Time_diff), ]
cor_S <- cor(subset_S$Lag_Average_Price, subset_S$Time_diff)

cat("Beta for S == 1:", beta_S, "\n")
cat("Correlation for S == 1:", cor_S, "\n\n")


# Run regression for observations with Fi == 2
regression_F <- lm(Lag_Average_Price ~ Time_diff, data = subset(Irish_data_test, Fi == 2))
beta_F <- coef(regression_F)["Time_diff"]

# Handle missing values for correlation calculation
subset_F <- subset(Irish_data_test, Fi == 2)
subset_F <- subset_F[complete.cases(subset_F$Lag_Average_Price, subset_F$Time_diff), ]
cor_F <- cor(subset_F$Lag_Average_Price, subset_F$Time_diff)

cat("Beta for Fi == 2:", beta_F, "\n")
cat("Correlation for Fi == 2:", cor_F, "\n")




graphics.off()

# 1. Observations with B == 1
pnew1 <- ggplot(filter(Irish_data_test, B == 1), aes(x = Time_diff, y = Lag_Average_Price)) +
  geom_point(size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", color = "black", size = 1.5) +  # Regression line
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  ) +
  ylab("Lag Average Price")
pnew1
ggsave("pnew1.png", plot = pnew1, width = 10, height = 6, dpi = 300)

# 2. Observations with S == 1
pnew2 <- ggplot(filter(Irish_data_test, S == 1), aes(x = Time_diff, y = Lag_Average_Price)) +
  geom_point(size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", color = "black", size = 1.5) +  # Regression line
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  ) +
  ylab("Lag Average Price")
pnew2
ggsave("pnew2.png", plot = pnew2, width = 10, height = 6, dpi = 300)

# 3. Observations with F == 2
pnew3 <- ggplot(filter(Irish_data_test, Fi == 2), aes(x = Time_diff, y = Lag_Average_Price)) +
  geom_point(size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", color = "black", size = 1.5) +  # Regression line
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  ) +
  ylab("Lag Average Price")

ggsave("pnew3.png", plot = pnew3, width = 10, height = 6, dpi = 300)
pnew3

# Filter the data for the year 2021
data_2021 <- Irish_data_sorted %>% filter(Year == 2021)
data_2012 <- Irish_data_sorted %>% filter(Year == 2012)
if (!require("readxl")) install.packages("readxl", type = "source")
library(readxl)
# Load the Excel file
calibration_table2012 <- read_excel("/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/HFCS ECB data/HFCS_UDB_2_5_STATA/results2013.xlsx")

# Display the first few rows of the data to check it
print(calibration_table2012)
# Divide the Average_Price by calibration_table2012[2, 3][[1]]
adjusted_price_2012 <- data_2012$Average_Price / calibration_table2012[2, 3][[1]]

# Run the regression of adjusted_price_2012 on Fi
regression_result <- lm(adjusted_price_2012 ~ Fi, data = data_2012)

# Report the results
summary(regression_result)


# Calculate percentiles to filter out the 2nd to 99th percentiles
price_percentiles <- quantile(data_2021$Price, probs = c(0.02, 0.99), na.rm = TRUE)
price_fe_percentiles <- quantile(data_2021$Price_FE, probs = c(0.02, 0.99), na.rm = TRUE)
price_AveragePrice_percentiles <- quantile(data_2021$Average_Price, probs = c(0.02, 0.99), na.rm = TRUE)

# Filter data to include only 2nd to 99th percentiles for Price and Price_FE
filtered_data_2021 <- data_2021 %>%
  filter(Price >= price_percentiles[1] & Price <= price_percentiles[2])
graphics.off()
# Plot the PDF for Price within the 2nd to 99th percentiles
price_pdf_plot <- ggplot(filtered_data_2021, aes(x = Price)) +
  geom_density(color = "blue", fill = "blue", alpha = 0.4) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(x = "Price", y = "Density") +
  scale_x_log10()  # Apply log scale to x-axis

# Display the PDF plot for Price
price_pdf_plot
ggsave("p4.png", plot = price_pdf_plot, width = 10, height = 6, dpi = 300)

graphics.off()
# Plot the PDF for Price_FE within the 2nd to 99th percentiles
price_fe_pdf_plot <- ggplot(filtered_data_2021, aes(x = Average_Price)) +
  geom_density(color = "red", fill = "red", alpha = 0.4) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(x = "Residual Price", y = "Density") +
  scale_x_log10()  # Apply log scale to x-axis

# Display the PDF plot for Price_FE
price_fe_pdf_plot
ggsave("p5.png", plot = price_fe_pdf_plot, width = 10, height = 6, dpi = 300)




#### calculate average return on flipping


#Irish_data_flipper<- Irish_data_sorted %>%
#  filter( Price_Ratio > 0.75, Price_Ratio < 1.5, Fi>0) 
Irish_data_flipper <- Irish_data_sorted %>%
  filter(Fi == 1 & !is.na(Price_Ratio))

Irish_data_flipper<- Irish_data_sorted %>%
  filter( Price_Ratio > 0.01, Price_Ratio < 10.0, Fi==1) 
Irish_data_flipper1<- Irish_data_sorted %>%
  filter( Price_Ratio > 0.01, Price_Ratio < 10.0, Fi1>0) 
Irish_data_flipper4<- Irish_data_sorted %>%
  filter( Price_Ratio > 0.01, Price_Ratio < 10.0, Fi4>0) 

Irish_data_flipper2 <- Irish_data_sorted %>%
  filter( Price_Ratio > 0.5, Price_Ratio < 3.0) 
#below check



if (!require("readxl")) install.packages("readxl", type = "source")
library(readxl)
# Load the Excel file
calibration_table2012 <- read_excel("/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/HFCS ECB data/HFCS_UDB_2_5_STATA/results2013.xlsx")

# Display the first few rows of the data to check it
print(calibration_table2012)

avg_price_ratio_2011_fi1 <- Irish_data_sorted %>%
  filter(Year == 2011 ,Time_diff < 730,  Time_diff > 30, Price_Ratio > 0.01, Price_Ratio < 10.0) %>%
  summarise(avg_price_ratio = mean(Price_Ratio, na.rm = TRUE))
avg_price_ratio_2011_fi1
price_fe_stats_2011 <- Irish_data_sorted %>%
  filter(Year == 2011) %>%
  summarise(
    avg_price_fe = mean(Price, na.rm = TRUE),
    var_price_fe = var(Price, na.rm = TRUE),
    avg_price=mean(Price/calibration_table2012[2, 3][[1]], na.rm = TRUE),
    var_price_ = var(Price/calibration_table2012[2, 3][[1]], na.rm = TRUE),
  )
price_fe_stats_2011
P1P02011 <- avg_price_ratio_2011_fi1$avg_price_ratio
price2011 <- price_fe_stats_2011$avg_price_fe / calibration_table2012[2, 3][[1]] # Using proper indexing for data frames
P1P02011
price2011

avg_price_ratio_2014_fi1 <- Irish_data_sorted %>%
  filter(Year == 2014 ,Time_diff < 730,  Time_diff > 30, Price_Ratio > 0.01, Price_Ratio < 10.0) %>%
  summarise(avg_price_ratio = mean(Price_Ratio, na.rm = TRUE))
avg_price_ratio_2014_fi1
price_fe_stats_2014 <- Irish_data_sorted %>%
  filter(Year == 2014) %>%
  summarise(
    avg_price_fe = mean(Price, na.rm = TRUE),
    var_price_fe = var(Price, na.rm = TRUE),
    avg_price=mean(Price/calibration_table2012[2, 3][[1]], na.rm = TRUE),
    var_price_ = var(Price/calibration_table2012[2, 3][[1]], na.rm = TRUE),
  )
price_fe_stats_2014
P1P02014 <- avg_price_ratio_2014_fi1$avg_price_ratio
price2014 <- price_fe_stats_2014$avg_price_fe / calibration_table2012[2, 3][[1]] # Using proper indexing for data frames
P1P02014
price2014

# Calculate the average of Price_ratio for Year = 2012 and Fi = 1
avg_price_ratio_2012_fi1 <- Irish_data_sorted %>%
  filter(Year == 2012 ,Time_diff < 730,  Time_diff > 30, Price_Ratio > 0.01, Price_Ratio < 10.0) %>%
  summarise(avg_price_ratio = mean(Price_Ratio, na.rm = TRUE))
avg_price_ratio_2012_fi1
# Calculate the average and variance of Price_FE for all Year = 2012 observations
price_fe_stats_2012 <- Irish_data_sorted %>%
  filter(Year == 2012) %>%
  summarise(
    avg_price_fe = mean(Price, na.rm = TRUE),
    var_price_fe = var(Price, na.rm = TRUE),
    avg_price=mean(Price/calibration_table2012[2, 3][[1]], na.rm = TRUE),
    var_price_ = var(Price/calibration_table2012[2, 3][[1]], na.rm = TRUE),
  )
price_fe_stats_2012

# Ensure necessary library for writing Excel files is installed and loaded
if (!require("writexl")) install.packages("writexl", type = "source")
library(writexl)

# Assuming the necessary data and libraries are already loaded and available in your environment

# Calculate P1P02013 and other variables
P1P02012 <- avg_price_ratio_2012_fi1$avg_price_ratio
price2012 <- price_fe_stats_2012$avg_price_fe / calibration_table2012[2, 3][[1]] # Using proper indexing for data frames
price_var_2012 <- price_fe_stats_2012$var_price_fe / (calibration_table2012[2, 3][[1]]^2)

s2012 <- calibration_table2012[12, 3][[1]]
r2012 <- calibration_table2012[13, 3][[1]]
gamma2012 <- calibration_table2012[7, 3][[1]]
share2012 <- flipped_data$Fraction_Fi[flipped_data$Year == 2012]

# Create a data frame with variable names in the first column and their values in the second column
calibration_data2012 <- data.frame(
  Variable = c("r", "s", "share", "P1P0", "price", "gamma"),
  Value = c(r2012, s2012, mean(share2012, na.rm = TRUE), P1P02012, price2012, gamma2012)
)

# Print the data frame to check it
print(calibration_data2012)

# Save the data frame to an Excel file
write_xlsx(calibration_data2012, "/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/plots/calibration_data2012.xlsx")



# Load necessary libraries
if (!require("readxl")) install.packages("readxl", type = "source")
library(readxl)
if (!require("dplyr")) install.packages("dplyr", type = "source")
library(dplyr)
if (!require("writexl")) install.packages("writexl", type = "source")
library(writexl)

# Calculate the deciles of price2012 (Price / calibration_table2012[2, 3][[1]])
price_deciles <- Irish_data_sorted %>%
  filter(Year == 2012) %>%
  mutate(price2012 = Average_Price / calibration_table2012[2, 3][[1]]) %>%
  summarise(
    decile_10 = quantile(price2012, 0.1, na.rm = TRUE),
    decile_20 = quantile(price2012, 0.2, na.rm = TRUE),
    decile_30 = quantile(price2012, 0.3, na.rm = TRUE),
    decile_40 = quantile(price2012, 0.4, na.rm = TRUE),
    decile_50 = quantile(price2012, 0.5, na.rm = TRUE),
    decile_60 = quantile(price2012, 0.6, na.rm = TRUE),
    decile_70 = quantile(price2012, 0.7, na.rm = TRUE),
    decile_80 = quantile(price2012, 0.8, na.rm = TRUE),
    decile_90 = quantile(price2012, 0.9, na.rm = TRUE)
  )


# Print the deciles to check the results
print(price_deciles)

# Save the deciles to an Excel file
write_xlsx(price_deciles, "/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/plots/deciles_prices.xlsx")

###2021

# Calculate the deciles of price2012 (Price / calibration_table2012[2, 3][[1]])
price_deciles2021 <- Irish_data_sorted %>%
  filter(Year == 2021) %>%
  mutate(price2012 = Average_Price / calibration_table2021[2, 3][[1]]) %>%
  summarise(
    decile_10 = quantile(price2012, 0.1, na.rm = TRUE),
    decile_20 = quantile(price2012, 0.2, na.rm = TRUE),
    decile_30 = quantile(price2012, 0.3, na.rm = TRUE),
    decile_40 = quantile(price2012, 0.4, na.rm = TRUE),
    decile_50 = quantile(price2012, 0.5, na.rm = TRUE),
    decile_60 = quantile(price2012, 0.6, na.rm = TRUE),
    decile_70 = quantile(price2012, 0.7, na.rm = TRUE),
    decile_80 = quantile(price2012, 0.8, na.rm = TRUE),
    decile_90 = quantile(price2012, 0.9, na.rm = TRUE)
  )


# Print the deciles to check the results
print(price_deciles2021)

# Save the deciles to an Excel file
write_xlsx(price_decile2021s, "/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/plots/deciles_prices2021.xlsx")


# Assuming `calibration_table2012[2, 3][[1]]` contains the necessary scaling factor
calibration_value <- calibration_table2012[2, 3][[1]]

# Calculate the percentiles of price2012
price_percentiles <- Irish_data_sorted %>%
  filter(Year == 2012) %>%
  mutate(price2012 = Average_Price / calibration_value) %>%
  summarise(
    percentile_02 = quantile(price2012, 0.02, na.rm = TRUE),
    percentile_04 = quantile(price2012, 0.04, na.rm = TRUE),
    percentile_06 = quantile(price2012, 0.06, na.rm = TRUE),
    percentile_08 = quantile(price2012, 0.08, na.rm = TRUE),
    percentile_10 = quantile(price2012, 0.10, na.rm = TRUE),
    percentile_12 = quantile(price2012, 0.12, na.rm = TRUE),
    percentile_14 = quantile(price2012, 0.14, na.rm = TRUE),
    percentile_16 = quantile(price2012, 0.16, na.rm = TRUE),
    percentile_18 = quantile(price2012, 0.18, na.rm = TRUE),
    percentile_20 = quantile(price2012, 0.20, na.rm = TRUE),
    percentile_22 = quantile(price2012, 0.22, na.rm = TRUE),
    percentile_24 = quantile(price2012, 0.24, na.rm = TRUE),
    percentile_26 = quantile(price2012, 0.26, na.rm = TRUE),
    percentile_28 = quantile(price2012, 0.28, na.rm = TRUE),
    percentile_30 = quantile(price2012, 0.30, na.rm = TRUE),
    percentile_32 = quantile(price2012, 0.32, na.rm = TRUE),
    percentile_34 = quantile(price2012, 0.34, na.rm = TRUE),
    percentile_36 = quantile(price2012, 0.36, na.rm = TRUE),
    percentile_38 = quantile(price2012, 0.38, na.rm = TRUE),
    percentile_40 = quantile(price2012, 0.40, na.rm = TRUE),
    percentile_42 = quantile(price2012, 0.42, na.rm = TRUE),
    percentile_44 = quantile(price2012, 0.44, na.rm = TRUE),
    percentile_46 = quantile(price2012, 0.46, na.rm = TRUE),
    percentile_48 = quantile(price2012, 0.48, na.rm = TRUE),
    percentile_50 = quantile(price2012, 0.50, na.rm = TRUE),
    percentile_52 = quantile(price2012, 0.52, na.rm = TRUE),
    percentile_54 = quantile(price2012, 0.54, na.rm = TRUE),
    percentile_56 = quantile(price2012, 0.56, na.rm = TRUE),
    percentile_58 = quantile(price2012, 0.58, na.rm = TRUE),
    percentile_60 = quantile(price2012, 0.60, na.rm = TRUE),
    percentile_62 = quantile(price2012, 0.62, na.rm = TRUE),
    percentile_64 = quantile(price2012, 0.64, na.rm = TRUE),
    percentile_66 = quantile(price2012, 0.66, na.rm = TRUE),
    percentile_68 = quantile(price2012, 0.68, na.rm = TRUE),
    percentile_70 = quantile(price2012, 0.70, na.rm = TRUE),
    percentile_72 = quantile(price2012, 0.72, na.rm = TRUE),
    percentile_74 = quantile(price2012, 0.74, na.rm = TRUE),
    percentile_76 = quantile(price2012, 0.76, na.rm = TRUE),
    percentile_78 = quantile(price2012, 0.78, na.rm = TRUE),
    percentile_80 = quantile(price2012, 0.80, na.rm = TRUE),
    percentile_82 = quantile(price2012, 0.82, na.rm = TRUE),
    percentile_84 = quantile(price2012, 0.84, na.rm = TRUE),
    percentile_86 = quantile(price2012, 0.86, na.rm = TRUE),
    percentile_88 = quantile(price2012, 0.88, na.rm = TRUE),
    percentile_90 = quantile(price2012, 0.90, na.rm = TRUE),
    percentile_92 = quantile(price2012, 0.92, na.rm = TRUE),
    percentile_94 = quantile(price2012, 0.94, na.rm = TRUE),
    percentile_96 = quantile(price2012, 0.96, na.rm = TRUE),
    percentile_98 = quantile(price2012, 0.98, na.rm = TRUE)
  )

# Print the results to check
print(price_percentiles)



write_xlsx(price_percentiles, "/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/plots/percentiles_prices.xlsx")









# Filter the data for Year 2012
data_2012_2 <- Irish_data_sorted[Irish_data_sorted$Year == 2012, ]

# Calculate the variance of Average_Price for the filtered data
variance_average_price_2012 <- var(data_2012_2$Average_Price / calibration_table2012[2, 3][[1]])

# Display the variance
print(variance_average_price_2012)

mean(data_2012_2$Average_Price/calibration_table2012[2, 3][[1]])



# Load the Excel file
calibration_table2021 <- read_excel("/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/HFCS ECB data/HFCS_UDB_4_0_STATA/results2021.xlsx")

# Display the first few rows of the data to check it
print(calibration_table2021)


# Calculate the average of Price_ratio for Year = 2012 and Fi = 1
avg_price_ratio_2021_fi1 <- Irish_data_sorted %>%
  filter(Year == 2021, Time_diff < 730,  Time_diff > 30, Price_Ratio > 0.01, Price_Ratio < 10.0) %>%
  summarise(avg_price_ratio = mean(Price_Ratio, na.rm = TRUE))
avg_price_ratio_2021_fi1
# Calculate the average and variance of Price_FE for all Year = 2012 observations
price_fe_stats_2021 <- Irish_data_sorted %>%
  filter(Year == 2021) %>%
  summarise(
    avg_price_fe = mean(Price, na.rm = TRUE),
    var_price_fe = var(Price, na.rm = TRUE),
    avg_price=mean(Price/calibration_table2021[2, 3][[1]], na.rm = TRUE),
    var_price_ = var(Price/calibration_table2021[2, 3][[1]], na.rm = TRUE),
  )
price_fe_stats_2021
# Ensure necessary library for writing Excel files is installed and loaded
if (!require("writexl")) install.packages("writexl", type = "source")
library(writexl)

# Assuming the necessary data and libraries are already loaded and available in your environment

# Calculate P1P02013 and other variables
P1P02021 <- avg_price_ratio_2021_fi1$avg_price_ratio
price2021 <- price_fe_stats_2021$avg_price_fe / calibration_table2021[2, 3][[1]] # Using proper indexing for data frames
s2021 <- calibration_table2021[12, 3][[1]]
r2021 <- calibration_table2021[13, 3][[1]]
gamma2021 <- calibration_table2021[7, 3][[1]]
share2021 <- flipped_data$Fraction_Fi[flipped_data$Year == 2021]

# Create a data frame with variable names in the first column and their values in the second column
calibration_data2021 <- data.frame(
  Variable = c("r", "s", "share", "P1P0", "price", "gamma"),
  Value = c(r2021, s2021, mean(share2021, na.rm = TRUE), P1P02021, price2021, gamma2021)
)

# Print the data frame to check it
print(calibration_data2021)

# Save the data frame to an Excel file
write_xlsx(calibration_data2021, "/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/plots/calibration_data2021.xlsx")
print((price_fe_stats_2021$avg_price_fe -price_fe_stats_2012$avg_price_fe )/price_fe_stats_2012$avg_price_fe)
print((price2021-price2012)/price2012)
########stata again
#####
######data for stata



# Filter to include only years between 2012 and 2021
filtered_data <- Irish_data_sorted2 %>%
  filter(Year >= 2012 & Year <= 2021)

# Count observations for each city and year
city_year_counts <- filtered_data %>%
  group_by(city, Year) %>%
  summarise(n = n(), .groups = "drop")

# Identify cities with at least 10 observations in each year within the range 2012 to 2021
valid_cities <- city_year_counts %>%
  group_by(city) %>%
  filter(all(n >= 10) & n() == 10) %>%  # Ensures the city has at least 10 observations for all years and exactly 10 years
  pull(city)

# Filter data for these cities
final_filtered_data <- filtered_data %>%
  filter(city %in% valid_cities)

# Save the necessary variables to a .dta file
final_data <- final_filtered_data %>%
  select(Price, city, Year, Quarter, Fi, Fi1, Fi4) %>%
  mutate(
    Fi = ifelse(Fi == 1, 1, 0),   # Set Fi to 1 if Fi is 1, otherwise 0
    Fi1 = ifelse(Fi1 == 1, 1, 0), # Set Fi1 to 1 if Fi1 is 1, otherwise 0
    Fi4 = ifelse(Fi4 == 1, 1, 0)  # Set Fi4 to 1 if Fi4 is 1, otherwise 0
  )


write_dta(final_data, "/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/city_price_data.dta")

###do in STATA flipper2_city_fig_reg,flipper2_city_fig_reg1,flipper2_city_fig_reg4
#### validation 
# Define file paths
file_path <- "/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/plots/city_results.xlsx"
file_path1 <- "/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/plots/city_results1.xlsx"
file_path4 <- "/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/plots/city_results4.xlsx"

# Read data from each Excel file
results_normal <- read_excel(file_path, sheet = "Fi 2012-2021")
results_fi1 <- read_excel(file_path1, sheet = "Fi1 2011-2022")
results_fi4 <- read_excel(file_path4, sheet = "Fi4 2014-2019")

# Add a 'Flipping_Definition' column to each dataset
results_normal <- results_normal %>% mutate(Flipping_Definition = "Normal Fi")
results_fi1 <- results_fi1 %>% mutate(Flipping_Definition = "Fi1")
results_fi4 <- results_fi4 %>% mutate(Flipping_Definition = "Fi4")
# Combine all results into a single dataframe
combined_results <- bind_rows(results_normal, results_fi1, results_fi4)
# Display the combined table
print(combined_results)



# Add a 'Flipping_Definition' column to each dataset
results_normal <- results_normal %>% mutate(Flipping_Definition = "Normal Fi")
results_fi1 <- results_fi1 %>% mutate(Flipping_Definition = "Fi1")
results_fi4 <- results_fi4 %>% mutate(Flipping_Definition = "Fi4")

# Combine all results into a single dataframe
combined_results <- bind_rows(results_normal, results_fi1, results_fi4)

# Filter for Individual_Level_Fixed_Effects rows only
individual_results <- combined_results %>%
  filter(...1 == "Individual_Level_Fixed_Effects")

# Extract beta and R-squared for each flipping definition
beta_normal <- individual_results %>% filter(Flipping_Definition == "Normal Fi") %>% pull(Beta)
r2_normal <- individual_results %>% filter(Flipping_Definition == "Normal Fi") %>% pull(`R-squared`)

beta_fi1 <- individual_results %>% filter(Flipping_Definition == "Fi1") %>% pull(Beta)
r2_fi1 <- individual_results %>% filter(Flipping_Definition == "Fi1") %>% pull(`R-squared`)

beta_fi4 <- individual_results %>% filter(Flipping_Definition == "Fi4") %>% pull(Beta)
r2_fi4 <- individual_results %>% filter(Flipping_Definition == "Fi4") %>% pull(`R-squared`)

# Create a data frame for the table
robustness_table <- data.frame(
  `1 Year` = c(beta_normal, r2_normal),
  `2 Years` = c(beta_fi1, r2_fi1),
  `4 Years` = c(beta_fi4, r2_fi4)
)

# Add a 'Flipping_Definition' column to each dataset
results_normal <- results_normal %>% mutate(Flipping_Definition = "Normal Fi")
results_fi1 <- results_fi1 %>% mutate(Flipping_Definition = "Fi1")
results_fi4 <- results_fi4 %>% mutate(Flipping_Definition = "Fi4")

# Combine all results into a single dataframe
combined_results <- bind_rows(results_normal, results_fi1, results_fi4)

# Filter for Individual_Level_Fixed_Effects rows only
individual_results <- combined_results %>%
  filter(...1 == "Individual_Level_Fixed_Effects")

# Extract beta and R-squared for each flipping definition
beta_normal <- individual_results %>% filter(Flipping_Definition == "Normal Fi") %>% pull(Beta)
r2_normal <- individual_results %>% filter(Flipping_Definition == "Normal Fi") %>% pull(`R-squared`)

beta_fi1 <- individual_results %>% filter(Flipping_Definition == "Fi1") %>% pull(Beta)
r2_fi1 <- individual_results %>% filter(Flipping_Definition == "Fi1") %>% pull(`R-squared`)

beta_fi4 <- individual_results %>% filter(Flipping_Definition == "Fi4") %>% pull(Beta)
r2_fi4 <- individual_results %>% filter(Flipping_Definition == "Fi4") %>% pull(`R-squared`)

# Create a data frame for the table
robustness_table <- data.frame(
  `1 Year` = c(beta_normal, r2_normal),
  `2 Years` = c(beta_fi1, r2_fi1),
  `4 Years` = c(beta_fi4, r2_fi4)
)
robustness_table
# Set row names for the LaTeX table with LaTeX math mode expressions
rownames(robustness_table) <- c("$\\beta$", "$R^2$")

colnames(robustness_table) <- c("2 Years", "1 Year", "4 Years")
# Convert the data frame to a LaTeX table using xtable
latex_table <- xtable(robustness_table, align = c("l", "c", "c", "c"))

# Define the file path to save the LaTeX table
output_path <- "/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/irish data/plots/robustness_table.tex"

# Open a connection to write the table with additional LaTeX formatting
sink(output_path)

# Add LaTeX table environment and centering
cat("\\begin{table}[h!]\n")
cat("\\centering\n")

# Output the table from xtable
print(latex_table, include.rownames = TRUE, include.colnames = TRUE, floating = FALSE, 
      table.placement = "h", tabular.environment = "tabular", comment = FALSE, 
      hline.after = c(-1, 0, nrow(robustness_table)), sanitize.text.function = identity)

# End the table environment
cat("\\end{table}\n")

# Close the connection to the file
sink()

# Manually add centering to the table (if not included by print settings)
cat("\\begin{center}\n", file = output_path, append = TRUE)
cat(readLines(output_path), file = output_path, sep = "\n", append = TRUE)
cat("\\end{center}\n", file = output_path, append = TRUE)




# Calculate the average 'Price' for each 'Year' and 'Flipper' condition
average_prices_flipper <- Irish_data_sorted %>%
  group_by(Year, Fi) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = 'drop')

# Correct the variable name in the creation of the 'Label' column
average_prices_flipper$Fi <- factor(
  case_when(
    average_prices_flipper$Fi == 0 ~ "normal",
    average_prices_flipper$Fi == 1 ~ "flipper",
    average_prices_flipper$Fi == 2 ~ "multiple"
  ),
  levels = c("normal", "flipper", "multiple")
)



# Filter data for the years 2012 to 2022
average_prices_flipper_filtered <- average_prices_flipper %>%
  filter(Year >= 2012, Year < 2023)

# Calculate the log of average prices
average_prices_flipper_filtered <- average_prices_flipper_filtered %>%
  mutate(LogAvgPrice = log(AvgPrice))
graphics.off()

# Now plot the filtered data with log of average prices
plot <- ggplot(average_prices_flipper_filtered, aes(x = Year, y = LogAvgPrice, group = Fi)) +
  geom_line(aes(linetype = Fi, color = Fi), linewidth = 3) +  # Add linetype aesthetic
  scale_color_manual(values = c("normal" = "blue", "flipper" = "red", "multiple" = "green"),
                     guide = "none") +  # Remove the legend for color
  scale_linetype_manual(values = c("normal" = "dotted", "flipper" = "solid", "multiple" = "dashed"),
                        guide = "none") +  # Remove the legend for linetype
  labs(x = "Year", y = "Log Average Price") +  # Set axis labels
  theme_minimal() +
  scale_x_continuous(breaks = seq(2012, 2021, by = 1)) +  # Ensure each year is marked
  theme(
    axis.title.x = element_blank(),  # Increase font size of x-axis label
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  )

# Display the plot
plot

# Save the plot as PNG
ggsave("p6.png", plot = plot, width = 10, height = 6, dpi = 300, units = "in")



library(readxl)
# Load validation_price data from Excel
validation_price <- read_excel("/Users/user/Desktop/Praca/Minnesota season 6/JOB MARKET/data/HFCS ECB data/HFCS_UDB_4_0_STATA/validation_price.xlsx")
validation_price <- validation_price %>%
  mutate(Year = as.integer(Year))

# Now merge validation_price with CPI_January
CPI_January <- CPI2012 %>%
  filter(Month == 1, Year >= 2010 & Year <= 2020)

# Merge validation_price data with CPI_January based on Year
merged_data2 <- validation_price %>%
  inner_join(CPI_January, by = "Year") %>%
  mutate(AdjustedMean = 100*Mean / CPI,
         LogAvgPrice_validation = log(AdjustedMean))


# Calculate the average 'Price' for each 'Year'
average_prices_all <- Irish_data_sorted %>%
  filter(Year >= 2012, Year <= 2021) %>%   # Filter data for the years 2012 to 2021
  group_by(Year) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = 'drop')

# Calculate the log of average prices
average_prices_all <- average_prices_all %>%
  mutate(LogAvgPrice = log(AvgPrice))
# Merge merged_data2 with average_prices_all
final_data <- average_prices_all %>%
  left_join(merged_data2, by = "Year")

graphics.off()  # Close any open graphics

# Create the plot with both lines (blue dashed for LogAvgPrice and black X's for LogAvgPrice_validation)
plot_p11 <- ggplot(final_data, aes(x = Year)) +
  geom_line(aes(y = LogAvgPrice), color = "blue", size = 3, linetype = "dashed") +  # Blue dashed line for LogAvgPrice
  geom_point(aes(y = LogAvgPrice_validation), color = "black", shape = 4, stroke = 1.5, size = 4)+
# Black X's for LogAvgPrice_validation
  labs(x = "Year", y = "Log Average Price") +  # Axis labels
  theme_minimal() +
  scale_x_continuous(breaks = seq(2012, 2021, by = 1)) +  # Ensure each year is marked
  theme(
    axis.title.x = element_blank(),  # Increase font size of x-axis label
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  )


# Display the plot
plot_p11

# Save the plot as PNG
ggsave("p12.png", plot = plot_p11, width = 10, height = 6, dpi = 300, units = "in")

# Create the plot with both lines (blue dashed for LogAvgPrice and black X's for LogAvgPrice_validation)
plot_p11 <- ggplot(final_data, aes(x = Year)) +
  geom_line(aes(y = LogAvgPrice), color = "blue", size = 3, linetype = "dashed") +  # Blue dashed line for LogAvgPrice
  # Black X's for LogAvgPrice_validation
  labs(x = "Year", y = "Log Average Price") +  # Axis labels
  theme_minimal() +
  scale_x_continuous(breaks = seq(2012, 2021, by = 1)) +  # Ensure each year is marked
  theme(
    axis.title.x = element_blank(),  # Increase font size of x-axis label
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  )


# Display the plot
plot_p11

# Save the plot as PNG
ggsave("p11.png", plot = plot_p11, width = 10, height = 6, dpi = 300, units = "in")





##########rates of return
# Calculate overall average price growth between 2012 and 2022 using Irish_data_sorted
overall_avg_price_2012 <- Irish_data_sorted %>%
  filter(Year == 2012) %>%
  summarise(AvgPrice2012 = mean(Price, na.rm = TRUE)) %>%
  pull(AvgPrice2012)

overall_avg_price_2022 <- Irish_data_sorted %>%
  filter(Year == 2022) %>%
  summarise(AvgPrice2022 = mean(Price, na.rm = TRUE)) %>%
  pull(AvgPrice2022)

overall_growth_rate <- (overall_avg_price_2022 - overall_avg_price_2012) / overall_avg_price_2012 * 100

# Calculate average price growth for each category using average_prices_flipper_filtered
growth_rates <- average_prices_flipper_filtered %>%
  filter(Year == 2012 | Year == 2022) %>%
  group_by(Fi) %>%
  summarise(
    AvgPrice2012 = mean(AvgPrice[Year == 2012], na.rm = TRUE),
    AvgPrice2022 = mean(AvgPrice[Year == 2022], na.rm = TRUE)
  ) %>%
  mutate(GrowthRate = (AvgPrice2022 - AvgPrice2012) / AvgPrice2012 * 100)

# Combine overall growth rate and growth rates by category into a single table
growth_table <- bind_rows(
  data.frame(Label = "Overall", GrowthRate = overall_growth_rate),
  growth_rates %>% select(Fi, GrowthRate)
)

# Display the table
print(growth_table)



# Calculate average prices per county in your data
average_prices_county2012 <- Irish_data_sorted %>%
  filter(Year==2012)%>%
  group_by(County) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE))

# Merge the map data with your average prices data
map_data <- merge(ireland_counties, average_prices_county2012, by.x = "name", by.y = "County", all.x = TRUE)

graphics.off() # Close any open graphics devices
options(device = "RStudioGD")

# Plot the map with average prices
plot <- ggplot(data = map_data) +
  geom_sf(aes(fill = AveragePrice), color = "white") +
  scale_fill_viridis_c(name = "Avg Price", na.value = "grey50", breaks = range(map_data$AveragePrice, na.rm = TRUE)) + # Show only min and max on the scale
  labs() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),  # Center and increase plot title text size
    axis.title.x = element_blank(),                    # Remove x-axis title
    axis.title.y = element_blank(),                    # Remove y-axis title
    legend.title = element_text(size = 14),             # Increase legend title text size
    legend.text = element_text(size = 8),              # Increase legend text size
    legend.position = "bottom",                         # Move legend to bottom
    legend.direction = "horizontal",                    # Make legend horizontal
    panel.grid.major = element_blank(),                 # Remove major grid lines
    panel.grid.minor = element_blank()                  # Remove minor grid lines
  )

# Display the plot
print(plot)

# Save the plot as PNG with adjusted file name
ggsave("p7.png", plot = plot, width = 10, height = 8, dpi = 300, units = "in")





# Calculate yearly counts and ratios
yearly_counts <- Irish_data_sorted %>%
  filter(Year > 2011, Year < 2022) %>%
  group_by(Year) %>%
  summarise(
    F_Greater_Than_0 = sum(Fi == 1),
    B_Greater_Than_0 = sum(Bi == 1),
    S_Greater_Than_0 = sum(Si== 1),
    BS = sum(B * S == 1),
    Total_Rows = n(),
    RatioB = B_Greater_Than_0 / Total_Rows,
    RatioS = S_Greater_Than_0 / Total_Rows,
    Ratio = F_Greater_Than_0 / Total_Rows,
    RatioBS = BS / Total_Rows
  )

graphics.off()

# Create the plot
plot <- ggplot(yearly_counts, aes(x = Year)) +
  geom_line(aes(y = Ratio, color = "Fi = 1"), linewidth = 3) +
  geom_line(aes(y = RatioB, color = "B"), linewidth = 3) +
  geom_line(aes(y = RatioS, color = "S"), linewidth = 3) +
  labs(x = "Year", y = "Fraction") +
  scale_color_manual(
    name = "Legend",
    values = c("Fi = 1" = "red", "B" = "blue", "S" = "green"),
    labels = c("Fi = 1", "B", "S"),
    guide = "none"  # Remove the legend
  ) +
  theme_minimal() +  # Use a minimal theme for a cleaner look
  theme(
    axis.title.x = element_blank(),  # Increase font size of x-axis label
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  )

# Display the plot
plot


# Save the plot as PNG
ggsave("pic12.png", plot = plot, width = 10, height = 8, dpi = 300, units = "in")


price_index4 <- Irish_data_flipper %>%
  group_by(Year) %>%
  summarize(Price_index4 = mean(Price_Ratio, na.rm = TRUE)) %>%
  ungroup()

average_prices_flipper <- Irish_data_sorted%>%
  group_by(Year, Fi) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = 'drop') # Using .groups = 'drop' to ungroup automatically

normal <- average_prices_flipper %>%
  filter(Fi == 2)
normal <- normal %>%
  mutate(Growth = c(1, tail(AvgPrice, -1) / head(AvgPrice, -1)))

# Correctly adjust the default value in the lag function
normal <- normal %>%
  mutate(Growth = c(1, tail(AvgPrice, -1) / head(AvgPrice, -1)))

normal
Irish_data_flipper <- Irish_data_flipper %>%
  # Ensure you group by Year to perform operations within each Year
  group_by(Year) %>%
  # Calculate Expenditure_Share with conditions
  mutate(Expenditure_Share = case_when(
    Time_diff == 0  ~ 0, # Conditions for assigning 0
    TRUE ~ Price / sum(Price[Time_diff > 0 ], na.rm = TRUE) # Default case
  )) %>%
  # Ungroup to remove the grouping structure
  ungroup()
Annual_HPI <- Irish_data_flipper %>%
  group_by(Year) %>%
  summarize(Annual_HPI = sum(Price_Ratio * Expenditure_Share, na.rm = TRUE)) %>%
  ungroup()


merged_data <- merge(normal, price_index4, by = "Year", all = TRUE)
merged_data <- merge(merged_data, Annual_HPI, by = "Year", all = TRUE)

Fred_HPI<- read.csv("/Users/user/Desktop/Praca/Minnesota season 5/Flippers/irish data/QIEN628BIS.csv")
Fred_HPI$Year <- as.integer(format(as.Date(Fred_HPI$DATE, "%d/%m/%y"), "%Y"))

# Rename QIEN629BIS_PCH to Fred_HPI
names(Fred_HPI)[names(Fred_HPI) == "QIEN628BIS_PCH"] <- "Fred_HPI"

# Convert Fred_HPI to numeric if it's not already



# Merge Fred_HPI data with merged_data
merged_data <- merge(merged_data, Fred_HPI[, c("Year", "Fred_HPI")], by = "Year", all = TRUE)
graphics.off()
merged_data<-merged_data%>%
  filter(Year>2012,Year<2021)
graphics.off()

# Plot "Growth" and "Fred HPI"
plot1 <- ggplot(merged_data, aes(x = Year)) +
  geom_hline(yintercept = 0, linetype = "solid", size = 0.25) +  # Add horizontal line at 0
  geom_line(aes(y = Growth - 1, color = "Growth"), size = 3, show.legend = FALSE) +
  geom_point(aes(y = Growth - 1, color = "Growth"), size = 5, show.legend = FALSE) +
  geom_line(aes(y = Fred_HPI / 100, color = "Fred HPI"), size = 3, show.legend = FALSE) +
  geom_point(aes(y = Fred_HPI / 100, color = "Fred HPI"), size = 5, show.legend = FALSE) +
  scale_color_manual(
    name = "Legend",
    values = c("Growth" = "green", "Fred HPI" = "purple"),
    labels = c("Growth" = "Non-flipper expenditure", "Fred HPI" = "Fred HPI")
  ) +
  scale_y_continuous(name = "Net change", limits = c(-0.01, 0.17)) +
  scale_x_continuous(limits = c(2013, 2020), breaks = 2012:2021) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  # Increase font size of x-axis label
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  )

# Display the first plot
print(plot1)

# Save the first plot as p8.png
ggsave("p8.png", plot = plot1, width = 10, height = 8, dpi = 300, units = "in")

merged_data
plot2 <- ggplot(merged_data, aes(x = Year)) +
  geom_hline(yintercept = 0, linetype = "solid", size = 0.25) +  # Add horizontal line at 0
  geom_line(aes(y = Price_index4 - 1, color = "Price Index 4"), size = 3, show.legend = FALSE) +
  geom_point(aes(y = Price_index4 - 1, color = "Price Index 4"), size = 5, show.legend = FALSE) +
  geom_line(aes(y = Annual_HPI - 1, color = "Annual HPI"), size = 3, show.legend = FALSE) +
  geom_point(aes(y = Annual_HPI - 1, color = "Annual HPI"), size = 5, show.legend = FALSE) +
  scale_color_manual(
    values = c("Price Index 4" = "red", "Annual HPI" = "blue")
  ) +
  scale_y_continuous(name = "Net change") +  # Remove the limits to allow all data to be visible
  scale_x_continuous(limits = c(2013, 2020), breaks = 2013:2020) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  # Increase font size of x-axis label
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  )

# Display the second plot
print(plot2)

# Save the second plot as p9.png
ggsave("p9.png", plot = plot2, width = 10, height = 8, dpi = 300, units = "in")

###################################
### tables prices, returns
###################################
###table mean prices, residuals and Price_ratios
###################################


# Load necessary libraries
library(dplyr)
library(xtable)
if (!require("e1071")) install.packages("e1071", dependencies = TRUE)
library(e1071)

# Function to calculate statistics including skewness and apply rounding
calculate_stats <- function(data) {
  mean_value <- round(mean(data$Price, na.rm = TRUE) / 100) * 100
  std_dev <- round(sd(log(data$Price), na.rm = TRUE), 2)
  skewness_value <- round(skewness(data$Price, na.rm = TRUE), 2) 
  return(c(mean = mean_value, standard_deviation = std_dev, skewness = skewness_value))
}

# Filtering data based on Fi and calculating stats for each Year
stats_fi_0 <- Irish_data_sorted %>%
  filter(Fi == 0, Year < 2022, Year > 2011) %>%
  group_by(Year) %>%
  do(data.frame(t(calculate_stats(.))))

stats_fi_1 <- Irish_data_sorted %>%
  filter(Fi == 1, Year < 2022, Year > 2011) %>%
  group_by(Year) %>%
  do(data.frame(t(calculate_stats(.))))

stats_fi_2 <- Irish_data_sorted %>%
  filter(Fi == 2, Year < 2022, Year > 2011) %>%
  group_by(Year) %>%
  do(data.frame(t(calculate_stats(.))))

# Calculating overall statistics without filtering on Fi
stats_overall <- Irish_data_sorted %>%
  filter(Year < 2022, Year > 2011) %>%
  group_by(Year) %>%
  do(data.frame(t(calculate_stats(.))))

# Combining all statistics into a single table
combined_stats <- bind_rows(list(
  "Fi=0" = stats_fi_0,
  "Fi=1" = stats_fi_1,
  "Fi=2" = stats_fi_2,
  "Overall" = stats_overall
), .id = "Fi_Category")

# Convert mean values to thousands for readability
combined_stats <- combined_stats %>%
  mutate(mean = format(mean, big.mark = ",", scientific = FALSE))

# Reshape for mean
mean_table <- combined_stats %>%
  select(Year, Fi_Category, mean) %>%
  spread(key = Fi_Category, value = mean)

# Reshape for standard deviation
sd_table <- combined_stats %>%
  select(Year, Fi_Category, standard_deviation) %>%
  spread(key = Fi_Category, value = standard_deviation)

# Reshape for skewness
skewness_table <- combined_stats %>%
  select(Year, Fi_Category, skewness) %>%
  spread(key = Fi_Category, value = skewness)

# Generate LaTeX tables
print(xtable(mean_table), type = "latex", include.rownames = FALSE, file = "mean_table_price.tex")
print(xtable(sd_table), type = "latex", include.rownames = FALSE, file = "sd_table_price.tex")
print(xtable(skewness_table), type = "latex", include.rownames = FALSE, file = "skewness_table_price.tex")




# Function to calculate statistics including skewness and apply rounding for Average_Price
calculate_stats <- function(data) {
  mean_value <- round(mean(data$Average_Price, na.rm = TRUE) / 100) * 100
  std_dev <- round(sd(log(data$Average_Price), na.rm = TRUE), 2)
  skewness_value <- round(skewness(data$Average_Price, na.rm = TRUE), 2)
  return(c(mean = mean_value, standard_deviation = std_dev, skewness = skewness_value))
}

# Filtering data based on Fi and calculating stats for each Year for Average_Price
stats_fi_0 <- Irish_data_sorted %>%
  filter(Fi == 0, Year < 2022, Year > 2011) %>%
  group_by(Year) %>%
  do(data.frame(t(calculate_stats(.))))

stats_fi_1 <- Irish_data_sorted %>%
  filter(Fi == 1, Year < 2022, Year > 2011) %>%
  group_by(Year) %>%
  do(data.frame(t(calculate_stats(.))))

stats_fi_2 <- Irish_data_sorted %>%
  filter(Fi == 2, Year < 2022, Year > 2011) %>%
  group_by(Year) %>%
  do(data.frame(t(calculate_stats(.))))

# Calculating overall statistics for Average_Price without filtering on Fi
stats_overall <- Irish_data_sorted %>%
  filter(Year < 2022, Year > 2011) %>%
  group_by(Year) %>%
  do(data.frame(t(calculate_stats(.))))

# Combining all statistics into a single table for Average_Price
combined_stats <- bind_rows(list(
  "Fi=0" = stats_fi_0,
  "Fi=1" = stats_fi_1,
  "Fi=2" = stats_fi_2,
  "Overall" = stats_overall
), .id = "Fi_Category")

# Convert mean values to thousands for readability
combined_stats <- combined_stats %>%
  mutate(mean = format(mean, big.mark = ",", scientific = FALSE))

# Reshape for mean
mean_table <- combined_stats %>%
  select(Year, Fi_Category, mean) %>%
  spread(key = Fi_Category, value = mean)

# Reshape for standard deviation
sd_table <- combined_stats %>%
  select(Year, Fi_Category, standard_deviation) %>%
  spread(key = Fi_Category, value = standard_deviation)

# Reshape for skewness
skewness_table <- combined_stats %>%
  select(Year, Fi_Category, skewness) %>%
  spread(key = Fi_Category, value = skewness)

# Generate LaTeX tables for Average_Price with _res suffix
print(xtable(mean_table), type = "latex", include.rownames = FALSE, file = "mean_table_price_res.tex")
print(xtable(sd_table), type = "latex", include.rownames = FALSE, file = "sd_table_price_res.tex")
print(xtable(skewness_table), type = "latex", include.rownames = FALSE, file = "skewness_table_price_res.tex")




# Load necessary libraries
library(dplyr)
library(xtable)

# Function to calculate statistics including Sharpe ratio and apply rounding
calculate_stats <- function(data) {
  mean_value <- mean(data$Price_Ratio, na.rm = TRUE)
  std_dev <- sd(data$Price_Ratio, na.rm = TRUE)
  sharpe_ratio <- (mean_value - 1.02) / std_dev
  return(c(mean = round(mean_value, 2), standard_deviation = round(std_dev, 2), sharpe_ratio = round(sharpe_ratio, 2)))
}

# Filtering data based on Fi and calculating stats for each Year
stats_fi_0 <- Irish_data_sorted %>%
  filter(Fi == 0, Year < 2022, Year > 2011, Time_diff > 30, Price_Ratio > 0.01, Price_Ratio < 10.0) %>%
  group_by(Year) %>%
  do(data.frame(t(calculate_stats(.))))

stats_fi_1 <- Irish_data_sorted %>%
  filter(Fi == 1, Year < 2022, Year > 2011, Time_diff > 30, Price_Ratio > 0.01, Price_Ratio < 10.0) %>%
  group_by(Year) %>%
  do(data.frame(t(calculate_stats(.))))

stats_fi_2 <- Irish_data_sorted %>%
  filter(Fi == 2, Year < 2022, Year > 2011, Time_diff > 30, Price_Ratio > 0.01, Price_Ratio < 10.0) %>%
  group_by(Year) %>%
  do(data.frame(t(calculate_stats(.))))

# Calculating overall statistics without filtering on Fi
stats_overall <- Irish_data_sorted %>%
  filter(Year < 2022, Year > 2011, Time_diff > 30, Price_Ratio > 0.01, Price_Ratio < 10.0) %>%
  group_by(Year) %>%
  do(data.frame(t(calculate_stats(.))))

# Combining all statistics into a single table
combined_stats <- bind_rows(list(
  "Fi=0" = stats_fi_0,
  "Fi=1" = stats_fi_1,
  "Fi=2" = stats_fi_2,
  "Overall" = stats_overall
), .id = "Fi_Category")

# Reshape for mean
mean_table <- combined_stats %>%
  select(Year, Fi_Category, mean) %>%
  spread(key = Fi_Category, value = mean)

# Reshape for standard deviation
sd_table <- combined_stats %>%
  select(Year, Fi_Category, standard_deviation) %>%
  spread(key = Fi_Category, value = standard_deviation)

# Reshape for Sharpe ratio
sharpe_table <- combined_stats %>%
  select(Year, Fi_Category, sharpe_ratio) %>%
  spread(key = Fi_Category, value = sharpe_ratio)

# Generate LaTeX tables
print(xtable(mean_table), type = "latex", include.rownames = FALSE, file = "mean_return.tex")
print(xtable(sd_table), type = "latex", include.rownames = FALSE, file = "sd_return.tex")
print(xtable(sharpe_table), type = "latex", include.rownames = FALSE, file = "sharpe_return.tex")



# Calculate yearly counts and ratios
yearly_counts <- Irish_data_sorted %>%
  filter(Year > 2011, Year < 2022) %>%
  group_by(Year) %>%
  summarise(
    F_Greater_Than_0 = sum(Fi == 1),
    B_Greater_Than_0 = sum(B == 1),
    S_Greater_Than_0 = sum(S == 1),
    BS = sum(B * S == 1),
    Total_Rows = n(),
    RatioB = B_Greater_Than_0 / Total_Rows,
    RatioS = S_Greater_Than_0 / Total_Rows,
    Ratio = F_Greater_Than_0 / Total_Rows,
    RatioBS = BS / Total_Rows
  )

# Remove graphics devices
graphics.off()

# Create the plot
# Create the plot
plot4 <- ggplot(yearly_counts, aes(x = Year)) +
  geom_line(aes(y = Ratio), color = "red", linetype = "solid", linewidth = 3) +  # Solid red line for Ratio
  geom_line(aes(y = RatioB), color = "red", linetype = "dotted", linewidth = 3) +  # Dotted red line for RatioB
  geom_line(aes(y = RatioS), color = "red", linetype = "dashed", linewidth = 3) +  # Dashed red line for RatioS
  labs(x = "Year", y = "Fraction of total trade") +  # Removed the plot title
  theme_minimal() +  # Use a minimal theme for clean presentation
  theme(
    axis.title.x = element_blank(),  # Increase font size of x-axis label
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  )


# Display the plot
plot4

# Save the plot as PNG
ggsave("p10.png", plot = plot4, width = 10, height = 8, dpi = 300, units = "in")



#untargeted
# Add the Fi_reg variable: Fi_reg = 1 when Fi = 1, otherwise Fi_reg = 0
Irish_data_sorted_untargeted <- Irish_data_sorted %>%
  mutate(Fi_reg = ifelse(Fi == 1, 1, 0))

# Filter for observations from the year 2012
Irish_data_2012 <- Irish_data_sorted_untargeted %>%
  filter(Year==2012)

# Run the regression: Average_Price ~ Fi_reg, using only data from 2012
regression_model <- lm(Average_Price / calibration_table2012[2, 3][[1]] ~ Fi_reg, data = Irish_data_2012)

# Summarize and print the results of the regression
summary(regression_model)




# Calculate the 80th and 20th percentiles of Average_Price
percentile_80 <- quantile(Irish_data_2012$Average_Price/calibration_table2012[2, 3][[1]], 0.50, na.rm = TRUE)
percentile_20 <- quantile(Irish_data_2012$Average_Price/calibration_table2012[2, 3][[1]], 0.10, na.rm = TRUE)

# Calculate the 80/20 ratio
percentile_ratio <- percentile_80 / percentile_20

# Output the percentiles and ratio
cat("80th Percentile of Average_Price:", percentile_80, "\n")
cat("20th Percentile of Average_Price:", percentile_20, "\n")
cat("80/20 Percentile Ratio:", percentile_ratio, "\n")
# Calculate the mean of the normalized Average_Price
mean_Average_Price <- mean(Irish_data_2012$Average_Price / calibration_table2012[2, 3][[1]], na.rm = TRUE)

# Output the mean
cat("Mean of Normalized Average_Price:", mean_Average_Price, "\n")

# Calculate the variance of the normalized Average_Price
variance_Average_Price <- var(Irish_data_2012$Average_Price/ calibration_table2012[2, 3][[1]] , na.rm = TRUE)

# Output the variance
cat("Variance of Normalized Average_Price:", variance_Average_Price, "\n")

Irish_data_2012$Normalized_Average_Price <- Irish_data_2012$Average_Price / calibration_table2012[2, 3][[1]]

# Generate a CDF plot
cdf_plot <- ecdf(Irish_data_2012$Normalized_Average_Price)

# Calculate the 2nd and 98th percentiles
p2 <- quantile(Irish_data_2012$Normalized_Average_Price, 0.02)
p98 <- quantile(Irish_data_2012$Normalized_Average_Price, 0.98)

# Subset the data within the percentile range
subset_data <- Irish_data_2012$Normalized_Average_Price[Irish_data_2012$Normalized_Average_Price >= p2 & 
                                                          Irish_data_2012$Normalized_Average_Price <= p98]

# Generate the CDF for the subsetted data
cdf_subset <- ecdf(subset_data)

# Plot the CDF for the subset
plot(cdf_subset, main = "CDF of Normalized Average Price (2012): 2nd to 98th Percentile", 
     xlab = "Normalized Average Price", ylab = "Cumulative Probability", col="blue", lwd=2,
     xlim = c(p2, p98))  # Set x-axis limits to 2nd and 98th percentiles

# Optionally, add percentile lines for reference
abline(v = p2, col = "red", lty = 2)  # 2nd percentile line
abline(v = p98, col = "red", lty = 2) # 98th percentile line

###untargeted
# Assuming Irish_data_sorted is your dataframe and Fi is a column in the dataframe

# Filter for the year 2012 and 2021
data_2012 <- subset(Irish_data_sorted, Year == 2012)
data_2021 <- subset(Irish_data_sorted, Year == 2021)

# Total observations and observations with Fi = 1 for 2012
total_obs_2012 <- nrow(data_2012)
fi_1_obs_2012 <- nrow(subset(data_2012, Fi == 1))

# Total observations and observations with Fi = 1 for 2021
total_obs_2021 <- nrow(data_2021)
fi_1_obs_2021 <- nrow(subset(data_2021, Fi == 1))

# Print the results
cat("For the year 2012:\n")
cat("Total observations:", 100*total_obs_2012/1980000, "\n")
cat("Observations with Fi = 1:", 100*fi_1_obs_2012/1980000, "\n\n")

cat("For the year 2021:\n")
cat("Total observations:", 100*total_obs_2021/2002000, "\n")
cat("Observations with Fi = 1:", 100*fi_1_obs_2021/2002000, "\n")



data_filtered_90 <- Irish_data_sorted %>% 
  filter(Time_diff > 90)
# Calculate PDF using density function for Irish_data_sorted
pdf_irish_data <- density(data_filtered_90$Time_diff)
pdf_data_frame_irish <- data.frame(Time_diff = pdf_irish_data$x, PDF = pdf_irish_data$y)
# Remove graphics devices
graphics.off()
# Plot with vertical lines at x = 365, 730, and 1460
plot <- ggplot(pdf_data_frame_irish, aes(x = Time_diff, y =100* PDF)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 365, linetype = "dashed", color = "blue", linewidth = 1) +
  geom_vline(xintercept = 730, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = 1460, linetype = "dashed", color = "black", linewidth = 1) +
  annotate("text", x = 365, y = 100*max(pdf_data_frame_irish$PDF) * 0.8, label = "1 year", color = "blue", fontface = "bold") +
  annotate("text", x = 730, y = 100*max(pdf_data_frame_irish$PDF) * 0.99, label = "2 years", color = "red", fontface = "bold") +
  annotate("text", x = 1460, y = 100*max(pdf_data_frame_irish$PDF) * 0.6, label = "4 years", color = "black", fontface = "bold") +
  #ggtitle("Multiple trades and Time since previous trade 2012-2021") +
  labs(x = "Days since last trade", y = "Density ") +
  theme_minimal() +  # Use a minimal theme for clean presentation
  theme(
    axis.title.x = element_text(size = 30),  # Increase font size of x-axis label
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  )
plot


# Save plot as PNG
ggsave("p15.png", plot = plot, width = 10, height = 8, dpi = 300, units = "in")


dev.off()

# Filter the data for Time_diff between 30 and 730
filtered_data <- Irish_data_sorted[Irish_data_sorted$Time_diff > 30 & Irish_data_sorted$Time_diff < 730 & Irish_data_sorted$Year==2012, ]

# Calculate the mean of Time_diff
mean_time_diff <- mean(filtered_data$Time_diff)

# Print the result
mean_time_diff


# Filter the data for Time_diff between 30 and 730
filtered_data <- Irish_data_sorted[Irish_data_sorted$Year==2021 &Irish_data_sorted$Time_diff > 30 & Irish_data_sorted$Time_diff < 730, ]

# Count the number of rows (observations) in the filtered data
num_observations <- nrow(filtered_data)

# Print the result
num_observations

# Filter the data for Time_diff between 30 and 730
filtered_data <- Irish_data_sorted[Irish_data_sorted$Year==2012 & Irish_data_sorted$Time_diff >730, ]

# Count the number of rows (observations) in the filtered data
num_observations <- nrow(filtered_data)

# Print the result
num_observations


# Assuming Irish_data_sorted and data_filtered_90 are already loaded and defined

data_filtered_90 <- Irish_data_sorted %>% 
  filter(Time_diff > 30 )
# Calculate PDF using density function for Irish_data_sorted
pdf_irish_data <- density(data_filtered_90$Time_diff)
pdf_data_frame_irish <- data.frame(Time_diff = pdf_irish_data$x, PDF = pdf_irish_data$y)

# Plot with shaded areas and vertical lines
plot <- ggplot(pdf_data_frame_irish, aes(x = Time_diff, y = 100 * PDF)) +
  # Shaded area below x = 730 (red)
  geom_area(data = subset(pdf_data_frame_irish, Time_diff <= 730), aes(x = Time_diff, y = 100 * PDF), fill = "red", alpha = 0.3) +
  
  # Shaded area above x = 730 (blue)
  geom_area(data = subset(pdf_data_frame_irish, Time_diff > 730), aes(x = Time_diff, y = 100 * PDF), fill = "blue", alpha = 0.3) +
  
  # Plot the density line
  geom_line(linewidth = 1) +
  
  # Vertical line at x = 730
  geom_vline(xintercept = 730, linetype = "dashed", color = "red", linewidth = 1) +
  
  # Annotate for 2 years mark
  annotate("text", x = 730, y = 100 * max(pdf_data_frame_irish$PDF) * 0.99, label = "2 years", color = "red", fontface = "bold") +
  
  # Axis labels and minimal theme
  labs(x = "Days since last trade", y = "Density") +
  theme_minimal() +  # Use a minimal theme for clean presentation
  theme(
    axis.title.x = element_text(size = 30),  # Increase font size of x-axis label
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  )

# Show plot
print(plot)

# Save plot as PNG
ggsave("p16.png", plot = plot, width = 10, height = 8, dpi = 300, units = "in")

# Clean up any remaining graphics devices
dev.off()


mean_time_diff <- mean(data_filtered_90$Time_diff, na.rm = TRUE)
print(mean_time_diff/365)
print(365/mean_time_diff)


########estimating 
## Load necessary packages
library(ggplot2)

# Step 1: Filter the data to focus on retraded houses with Time_diff > 30
data_filtered_90 <- Irish_data_sorted %>% filter(Time_diff > 30)

# Step 2: Calculate the PDF using the density function
pdf_irish_data <- density(data_filtered_90$Time_diff)
pdf_data_frame_irish <- data.frame(Time_diff = pdf_irish_data$x, PDF = pdf_irish_data$y)

# Step 3: Estimate lambda from the exponential distribution (1 / mean of Time_diff)
lambda_hat <- 1 / mean(data_filtered_90$Time_diff)

# Step 4: Add the exponential distribution curve for the estimated lambda
# Create a data frame for the exponential distribution based on lambda_hat
exp_data_frame <- data.frame(
  Time_diff = seq(0, max(pdf_data_frame_irish$Time_diff), length.out = 1000)
)
exp_data_frame$PDF <- lambda_hat * exp(-lambda_hat * exp_data_frame$Time_diff)
# Remove graphics devices
graphics.off()
# Step 5: Plot the data with shaded areas and overlay the exponential curve
plot <- ggplot(pdf_data_frame_irish, aes(x = Time_diff, y = 100 * PDF)) +
  # Shaded area below x = 730 (red)
  geom_area(data = subset(pdf_data_frame_irish, Time_diff <= 730), aes(x = Time_diff, y = 100 * PDF), fill = "red", alpha = 0.3) +
  
  # Shaded area above x = 730 (blue)
  geom_area(data = subset(pdf_data_frame_irish, Time_diff > 730), aes(x = Time_diff, y = 100 * PDF), fill = "blue", alpha = 0.3) +
  
  # Plot the density line
  geom_line(linewidth = 1) +
  
  # Overlay the estimated exponential distribution curve (black line)
  geom_line(data = exp_data_frame, aes(x = Time_diff, y = 100 * PDF), color = "black", linewidth = 1.5) +
  
  # Vertical line at x = 730
  geom_vline(xintercept = 730, linetype = "dashed", color = "red", linewidth = 1) +
  
  # Annotate for 2 years mark
  annotate("text", x = 730, y = 100 * max(pdf_data_frame_irish$PDF) * 0.99, label = "2 years", color = "red", fontface = "bold") +
  
  # Axis labels and minimal theme
  labs(x = "Days since last trade", y = "Density") +
  theme_minimal() +  # Use a minimal theme for clean presentation
  theme(
    axis.title.x = element_text(size = 30),  # Increase font size of x-axis label
    axis.title.y = element_text(size = 30),  # Increase font size of y-axis label
    axis.text.x = element_text(size = 20),   # Increase font size of x-axis numbers
    axis.text.y = element_text(size = 20),   # Increase font size of y-axis numbers
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank()       # Remove minor grid lines
  )

# Show plot
print(plot)

# Save plot as PNG
ggsave("p16_with_lambda.png", plot = plot, width = 10, height = 8, dpi = 300, units = "in")

# Clean up any remaining graphics devices
dev.off()
# Remove graphics devices
graphics.off()

### 02052025

mean_time<-mean(data_filtered_90$Time_diff)
if (!require("rootSolve")) install.packages("rootSolve")
library(rootSolve)

# Function to compute expected waiting time under right censoring
expected_waiting_time <- function(kappa, T) {
  (1 / kappa) - (T * exp(-kappa * T)) / (1 - exp(-kappa * T))
}

# Function to solve for kappa given T and observed mean waiting time
solve_for_kappa <- function(T, observed_mean_time) {
  equation <- function(kappa) {
    expected_waiting_time(kappa, T) - observed_mean_time
  }
  
  # Initial guess for kappa (using naive 1/mean_time approximation)
  initial_guess <- 1 / observed_mean_time
  
  # Solve for kappa
  kappa_solution <- uniroot(equation, lower = 1e-6, upper = 10)$root
  
  return(kappa_solution)
}

# Example usage:
T <- 12 * 365  # Total observation period (e.g., 12 years)
observed_mean_time <- mean_time  # Mean waiting time in days from data

kappa_estimated2012 <- solve_for_kappa(T, observed_mean_time)*365
cat(sprintf("Estimated kappa: %.6f\n", kappa_estimated2012 ))

# Calculate P1P02013 and other variables
P1P02012 <- avg_price_ratio_2012_fi1$avg_price_ratio
price2012 <- price_fe_stats_2012$avg_price_fe / calibration_table2012[2, 3][[1]] # Using proper indexing for data frames
price_var_2012 <- price_fe_stats_2012$var_price_fe / (calibration_table2012[2, 3][[1]]^2)

s2012 <- calibration_table2012[12, 3][[1]]
r2012 <- calibration_table2012[13, 3][[1]]
gamma2012 <- calibration_table2012[7, 3][[1]]
share2012 <- flipped_data$Fraction_Fi[flipped_data$Year == 2012]

# Create a data frame with variable names in the first column and their values in the second column
calibration_data2012new <- data.frame(
  Variable = c("r", "s",  "P1P0", "mean price", "var price","trade","gamma"),
  Value = c(r2012, s2012,  P1P02012, price2012,price_var_2012,kappa_estimated2012, gamma2012)
)
