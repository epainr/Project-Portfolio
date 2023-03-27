library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(stringr)
library(tibble)
library(forcats)

# DATA CLEANING

# 2021 Dataset

pharm_21 <- read.csv("main_data.csv")

# 2016 Dataset

pharm_16 <- read.csv("secondary_data.csv")

# Rename columns.
names(pharm_21) <- c("contract_number", "vendor", "contract_start",
                     "contract_end", "NDC", "sub_item_identifier", "package", 
                     "generic_drug", "trade_name_drug", "class", "covered", 
                     "prime_vendor", "price", "price_start", "price_end", 
                     "price_type", "compliant")

names(pharm_16) <- c("contract_number", "vendor", "contract_start",
                     "contract_end", "NDC", "sub_item_identifier", "package", 
                     "generic_drug", "trade_name_drug", "class", "covered", 
                     "prime_vendor", "price", "price_start", "price_end", 
                     "price_type")

# Remove the sub item identifier column
pharm_21 <- select(pharm_21, -sub_item_identifier)
pharm_16 <- select(pharm_16, -sub_item_identifier)

# Convert variables to logical variables.
pharm_21$covered <- as.logical(pharm_21$covered)
pharm_16$covered <- as.logical(pharm_16$covered)
pharm_21$prime_vendor <- as.logical(pharm_21$prime_vendor)
pharm_16$prime_vendor <- as.logical(pharm_16$prime_vendor)
pharm_21$compliant <- as.logical(factor(pharm_21$compliant))

# Simplify class variable -- the first two letters of a drug's class represent 
# the family of drugs they belong to, which is more than enough information for 
# our analysis. There is a loss of specific information as a result, but our 
# drug classes are now generalized down to groupings like "Immunological Agents"
# rather than "Immunoglobulins".
pharm_21$class <- substring(pharm_21$class, 1, 2)
pharm_16$class <- substring(pharm_16$class, 1, 2)

# Clean up fringe errors in the class data. If the fringe case is clearly 
# identifiable with what class it should fall into (Using a 0 instead of a O, 
# lowercase instead of uppercase, the same drugs in the file are all classified 
# under a valid code) and comparison and outside verification affirms that the
# nearest class is correct for the particular drug, the drug is reclassified
# appropriately. Else, the drug's class is reassigned to NA.
pharm_21$class <- toupper(pharm_21$class)
pharm_16$class <- toupper(pharm_16$class)

pharm_21$class[pharm_21$class == "0T"] <- "OT"
pharm_16$class[pharm_16$class == "0T"] <- "OT"

pharm_21$class[pharm_21$class == "1M"] <- "IM"
pharm_16$class[pharm_16$class == "1M" | pharm_16$class == "I,"] <- "IM"

pharm_21$class[pharm_21$class == "CA" | pharm_21$class == "CH"] <- "CN"
pharm_16$class[pharm_16$class == "CA"] <- "CN"

pharm_16$class[pharm_16$class == " C"] <- "CV"

pharm_21$class[pharm_21$class == "HF"] <- "HS"

pharm_21$class[pharm_21$class == "10" | 
               pharm_21$class == "44" |
               pharm_21$class == "C0" |
               pharm_21$class == "C1" |
               pharm_21$class == "CA" |
               pharm_21$class == "CH" |
               pharm_21$class == "GO" |
               pharm_21$class == "IO" |
               pharm_21$class == "OY" |
               pharm_21$class == "XS"] <- NA

pharm_16$class[pharm_16$class == "CT" |
               pharm_16$class == "GO" |
               pharm_16$class == "IO" |
               pharm_16$class == "NO" |
               pharm_16$class == "TX"] <- NA


# Store class abbreviations and names in their respective character vectors
class_abbreviations <- c("AD", "AH", "AM", "AN", "AP", "AS", "AU", "BL", "CN", 
                         "CV", "DE", "DX", "GA", "GU", "HA", "HS", "IM", "IN", 
                         "IR", "MS", "NT", "OP", "OR", "OT", "PH", "RE", "RS", 
                         "TN", "VT", "XA", "XX")

class_names <- c("Antidotes and Deterrents", "Antihistamines", "Antimicrobials",
                 "Antineoplastics", "Antiparasitics", 
                 "Antiseptics and Disinfectants", "Autonomic", "Blood", 
                 "Central Nervous System", "Cardiovascular", "Dermatological", 
                 "Diagnostic", "Gastrointestinal", "Genitourinary",
                 "Herbal and Alternative", "Hormones and Synthetics", 
                 "Immunological", "Investigational", "Irrigation and Dialysis", 
                 "Musculoskeletal", "Nasal and Throat", "Ophthalmic", 
                 "Dental and Oral", "Otic", "Pharmaceutical Aids",
                 "Respiratory Tract", "Rectal", "Nutrients", "Vitamins", 
                 "Prosthetics and Supplies", "Miscellaneous")

# Factor the class variable and rename each factor to correspond with each class
# code's meaning.
pharm_21$class <- factor(pharm_21$class,
                         levels = class_abbreviations,
                         labels = class_names)

pharm_16$class <- factor(pharm_16$class,
                         levels = class_abbreviations,
                         labels = class_names)

# Factor the price_type variable
pharm_21$price_type <- factor(pharm_21$price_type, 
                              labels = c("Big 4", 
                                         "FSS", 
                                         "VA National Contract"))
pharm_16$price_type <- factor(pharm_16$price_type, 
                              labels = c("Big 4", 
                                         "FSS", 
                                         "VA National Contract"))

# Convert the price variable from character to numeric
pharm_21$price <- as.numeric(pharm_21$price)
pharm_16$price <- as.numeric(pharm_16$price)

# Create a factored_price variable that categorizes drugs into log(10) 
# price ranges.
price_groups <- c("< $1",  "$1 - $9", "$10 - $99", "$100 - $999", 
                  "$1,000 - $9,999", "$10,000 - $99,999", "$100,000 - $999,999",
                  "> $1,000,000")

pharm_21$factored_price <- cut(log10(pharm_21$price), 
                               breaks = -1:7, 
                               labels = price_groups)
pharm_16$factored_price <- cut(log10(pharm_16$price), 
                               breaks = -1:7, 
                               labels = price_groups)

pharm_21$factored_price <- factor(pharm_21$factored_price, ordered = TRUE)
pharm_16$factored_price <- factor(pharm_16$factored_price, ordered = TRUE)

# Coerce date variables into Date objects

# 2021 price start
pharm_21$price_start <- paste(substring(pharm_21$price_start, 1, 6), 
                              substring(pharm_21$price_start, 9, 10), 
                              sep = "")
pharm_21$price_start <- as.Date(pharm_21$price_start, "%m/%d/%y")

# 2021 price end
pharm_21$price_end <- paste(substring(pharm_21$price_end, 1, 6), 
                            substring(pharm_21$price_end, 9, 10), 
                            sep = "")
pharm_21$price_end <- as.Date(pharm_21$price_end, "%m/%d/%y")

# 2016 price start
pharm_16$price_start <- paste(substring(pharm_16$price_start, 1, 6), 
                              substring(pharm_16$price_start, 9, 10), 
                              sep = "")
pharm_16$price_start <- as.Date(pharm_16$price_start, "%m/%d/%y")

# 2016 price end
pharm_16$price_end <- paste(substring(pharm_16$price_end, 1, 6), 
                            substring(pharm_16$price_end, 9, 10), 
                            sep = "")
pharm_16$price_end <- as.Date(pharm_16$price_end, "%m/%d/%y")

# 2021 contract start
pharm_21$contract_start <- paste(substring(pharm_21$contract_start, 1, 6), 
                                 substring(pharm_21$contract_start, 9, 10), 
                                 sep = "")
pharm_21$contract_start <- as.Date(pharm_21$contract_start, "%m/%d/%y")

# 2021 contract end
pharm_21$contract_end <- paste(substring(pharm_21$contract_end, 1, 6), 
                               substring(pharm_21$contract_end, 9, 10), 
                               sep = "")
pharm_21$contract_end <- as.Date(pharm_21$contract_end, "%m/%d/%y")

# 2016 contract start
pharm_16$contract_start <- paste(substring(pharm_16$contract_start, 1, 6), 
                                 substring(pharm_16$contract_start, 9, 10), 
                                 sep = "")
pharm_16$contract_start <- as.Date(pharm_16$contract_start, "%m/%d/%y")

# 2016 contract end
pharm_16$contract_end <- paste(substring(pharm_16$contract_end, 1, 6), 
                               substring(pharm_16$contract_end, 9, 10), 
                               sep = "")
pharm_16$contract_end <- as.Date(pharm_16$contract_end, "%m/%d/%y")

# Clean up fringe errors in the date data.

pharm_16$price_end[substring(pharm_16$price_end, 1, 4) == "2007"] <- NA

pharm_21$price_start[substring(pharm_21$price_start, 1, 4) == "2002" |
                     substring(pharm_21$price_start, 1, 4) == "2000"] <- NA

pharm_21$price_end[substring(pharm_21$price_end, 1, 4) == "2029"] <- NA

# Store price start dates as characters to be used later
price_starts <- levels(factor(pharm_21$price_start))

# Create contract_length variable
pharm_21$contract_length <- pharm_21$contract_end - pharm_21$contract_start
pharm_16$contract_length <- pharm_16$contract_end - pharm_16$contract_start

# Create vector of common vendors and subset x and x16 into only those drugs 
# with common vendors
common_vendors <- unique((inner_join(pharm_21, pharm_16, by = "vendor"))$vendor)

# Store subsetted versions of the data containing only the common vendors.
common_21 <- filter(pharm_21, vendor %in% common_vendors)
common_16 <- filter(pharm_16, vendor %in% common_vendors)

# Store all vendors as characters to be used later
vendor_names <- levels(factor(pharm_21$vendor))

# Store main color palette in character vector to more easily access specific 
# colors
spectral_palette <- brewer.pal(11, "Spectral")


# GRAPHS

# Figure 3.1.1

# Store the contract length means by price type
mean_contract_length_by_price_type <- by(pharm_21$contract_length / 365.0, 
                                         pharm_21$price_type, 
                                         mean)

# Mean Contract Length by Price Type
ggplot(mapping = aes(x = mean_contract_length_by_price_type, 
                     y = levels(pharm_21$price_type),
                     fill = levels(pharm_21$price_type))) + 
  geom_col(show.legend = FALSE) +
  theme_minimal() +
  labs(x = "Average Contract Length (Year)", y = "Price Type",
       title = "Average Contract Length in Years by Price Type") +
  scale_fill_manual(values = c(spectral_palette[2], 
                               spectral_palette[9], 
                               spectral_palette[10]))

# Figure 3.1.2

# Store the summary data of price by price_start dates by price types
summary_price_FSS <- by(pharm_21$price[pharm_21$price_type == "FSS"], 
                        pharm_21$price_start[pharm_21$price_type == "FSS"], 
                        summary)
summary_price_Big4 <- by(pharm_21$price[pharm_21$price_type == "Big 4"], 
                         pharm_21$price_start[pharm_21$price_type == "Big 4"], 
                         summary)
summary_price_VNC <- by(pharm_21$price[pharm_21$price_type == 
                                         "VA National Contract"], 
                        pharm_21$price_start[pharm_21$price_type == 
                                               "VA National Contract"], 
                        summary)

# Create vectors of the appropriate size.
median_price_FSS <- 1:length(price_starts)
median_price_Big4 <- 1:length(price_starts)
median_price_VNC <- 1:length(price_starts)

# Iterate through each date
for (i in 1:length(price_starts)) {
  
  if (as.Date(price_starts[i]) %in% 
      pharm_21$price_start[pharm_21$price_type == "FSS"])
    median_price_FSS[i] <- summary_price_FSS[[price_starts[i]]][["Median"]]
  else
    median_price_FSS[i] <- NA
  
  if (as.Date(price_starts[i]) %in% 
      pharm_21$price_start[pharm_21$price_type == "Big 4"])
    median_price_Big4[i] <- summary_price_Big4[[price_starts[i]]][["Median"]]
  else
    median_price_Big4[i] <- NA
  
  if (as.Date(price_starts[i]) %in% 
      pharm_21$price_start[pharm_21$price_type == "VA National Contract"])
    median_price_VNC[i] <- summary_price_VNC[[price_starts[i]]][["Median"]]
  else
    median_price_VNC[i] <- NA
}

# Create a data frame to hold the medians and rename the columns.
med_price_date <- data.frame(cbind(price_starts, 
                                   median_price_FSS, 
                                   median_price_Big4,
                                   median_price_VNC))
names(med_price_date) <- c("price_start", "median_price_FSS", 
                           "median_price_Big4", "median_price_VNC")

# Coerce price_start back to Date type and medians into numeric type
med_price_date$price_start <- as.Date(med_price_date$price_start)
med_price_date$median_price_FSS <- as.numeric(med_price_date$median_price_FSS)
med_price_date$median_price_Big4 <- as.numeric(med_price_date$median_price_Big4)
med_price_date$median_price_VNC <- as.numeric(med_price_date$median_price_VNC)

# Plot the data
ggplot(mapping = aes(x = med_price_date$price_start)) +
  scale_color_manual(name = "Price Type", 
                     values = c(FSS_color = spectral_palette[9], 
                                Big4_color = spectral_palette[2], 
                                VNC_color = spectral_palette[10]),
                     labels = c("FSS", "Big 4", "VA National Contract")) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  geom_point(mapping = aes(y = med_price_date$median_price_FSS, 
                           color = "FSS_color")) +
  geom_point(mapping = aes(y = med_price_date$median_price_Big4, 
                           color = "Big4_color")) +
  geom_point(mapping = aes(y = med_price_date$median_price_VNC, 
                           color = "VNC_color")) +
  geom_rug(data = subset(pharm_21, price < 25000), 
           mapping = aes(x = price_start, y = price), 
           sides = "b", 
           inherit.aes = FALSE) +
  labs(title = "Median Price Per Day by Price Type",
       x = "Price Start Date",
       y = "Price ($)")

# Figure 3.1.3

# Store the total number of contracts in each price type by vendor 
# (of the vendors that are common between 2016 and 2021 data).
price_types_21 <- by(common_21$price_type, common_21$vendor, summary)
price_types_16 <- by(common_16$price_type, common_16$vendor, summary)

# Create vectors of the appropriate length for storage of information about 
# vendor contracts for future usage.
VNC <- 1:length(common_vendors)
FSS <- 1:length(common_vendors)
Big4 <- 1:length(common_vendors)

VNC16 <- 1:length(common_vendors)
FSS16 <- 1:length(common_vendors)
Big416 <- 1:length(common_vendors)

# Iterate through each common vendor and store the total number of contracts
# in each price type in the respective vectors.
for (i in 1:length(common_vendors)) {
  VNC[i] <- price_types_21[[common_vendors[i]]][["VA National Contract"]]
  FSS[i] <- price_types_21[[common_vendors[i]]][["FSS"]]
  Big4[i] <- price_types_21[[common_vendors[i]]][["Big 4"]]
}

for (i in 1:length(common_vendors)) {
  VNC16[i] <- price_types_16[[common_vendors[i]]][["VA National Contract"]]
  FSS16[i] <- price_types_16[[common_vendors[i]]][["FSS"]]
  Big416[i] <- price_types_16[[common_vendors[i]]][["Big 4"]]
}

# Create a data frame of the common vendors and the respective totals 
# of each price type.
total_price_types_21 <- data.frame(cbind(common_vendors, 
                                           VNC, 
                                           FSS, 
                                           Big4))
total_price_types_16 <- data.frame(cbind(common_vendors, 
                                           VNC16, 
                                           FSS16, 
                                           Big416))

# Coerce into numeric types.
total_price_types_21$VNC <- as.numeric(total_price_types_21$VNC)
total_price_types_21$FSS <- as.numeric(total_price_types_21$FSS)
total_price_types_21$Big4 <- as.numeric(total_price_types_21$Big4)

total_price_types_16$VNC16 <- as.numeric(total_price_types_16$VNC16)
total_price_types_16$FSS16 <- as.numeric(total_price_types_16$FSS16)
total_price_types_16$Big416 <- as.numeric(total_price_types_16$Big416)

# Create a variable to keep track of the total number of contracts 
# by a given vendor.
total_price_types_21$total_contracts <- total_price_types_21$VNC + 
                                        total_price_types_21$FSS + 
                                        total_price_types_21$Big4
total_price_types_16$total_contracts16 <- total_price_types_16$VNC16 + 
                                          total_price_types_16$FSS16 + 
                                          total_price_types_16$Big416

# Reassign the price type variables to the proportion of the total number 
# of contracts they represent, rather than the raw count.
total_price_types_21$VNC <- total_price_types_21$VNC / 
                            total_price_types_21$total_contracts
total_price_types_21$FSS <- total_price_types_21$FSS / 
                            total_price_types_21$total_contracts
total_price_types_21$Big4 <- total_price_types_21$Big4 / 
                             total_price_types_21$total_contracts

total_price_types_16$VNC16 <- total_price_types_16$VNC16 / 
                              total_price_types_16$total_contracts16
total_price_types_16$FSS16 <- total_price_types_16$FSS16 / 
                              total_price_types_16$total_contracts16
total_price_types_16$Big416 <- total_price_types_16$Big416 / 
                               total_price_types_16$total_contracts16

# Combine the two data frames.
total_price_types <- inner_join(total_price_types_16, 
                                total_price_types_21, 
                                by = "common_vendors")

# Create a vector of the appropriate size and initialize counter
# variables to zero.
total_price_types$variations <- 1:length(total_price_types$FSS)
price_type_0range <- 0
price_type_5range <- 0
price_type_10range <- 0
price_type_15range <- 0
price_type_over15range <- 0

# Iterate through each vendor
for (i in 1:length(total_price_types$FSS)) {
  
  if (((total_price_types$FSS16[i] == total_price_types$FSS[i]) &
       (total_price_types$Big416[i] == total_price_types$Big4[i]) &
       (total_price_types$VNC16[i] == total_price_types$VNC[i]))) {
    
    total_price_types$variations[i] <- "0% change"
    price_type_0range <- price_type_0range + 1
    
  } else if (between(total_price_types$FSS16[i], 
                     (total_price_types$FSS[i] - 0.05), 
                     (total_price_types$FSS[i] + 0.05)) &
             between(total_price_types$Big416[i], 
                     (total_price_types$Big4[i] - 0.05), 
                     (total_price_types$Big4[i] + 0.05)) &
             between(total_price_types$VNC16[i], 
                     (total_price_types$VNC[i] - 0.05), 
                     (total_price_types$VNC[i] + 0.05))) {
    
    total_price_types$variations[i] <- "0% - 5% change"
    price_type_5range <- price_type_5range + 1
    
  } else if (between(total_price_types$FSS16[i], 
                     (total_price_types$FSS[i] - 0.10), 
                     (total_price_types$FSS[i] + 0.10)) &
             between(total_price_types$Big416[i], 
                     (total_price_types$Big4[i] - 0.10), 
                     (total_price_types$Big4[i] + 0.10)) &
             between(total_price_types$VNC16[i], 
                     (total_price_types$VNC[i] - 0.10), 
                     (total_price_types$VNC[i] + 0.10))) {
    
    total_price_types$variations[i] <- "5% - 10% change"
    price_type_10range <- price_type_10range + 1
    
  } else if (between(total_price_types$FSS16[i], 
                     (total_price_types$FSS[i] - 0.15), 
                     (total_price_types$FSS[i] + 0.15)) &
             between(total_price_types$Big416[i], 
                     (total_price_types$Big4[i] - 0.15), 
                     (total_price_types$Big4[i] + 0.15)) &
             between(total_price_types$VNC16[i], 
                     (total_price_types$VNC[i] - 0.15), 
                     (total_price_types$VNC[i] + 0.15))) {
    
    total_price_types$variations[i] <- "10% - 15% change"
    price_type_15range <- price_type_15range + 1
    
  } else {
    
    total_price_types$variations[i] <- "> 15% change"
    price_type_over15range <- price_type_over15range + 1
    
  }
}

total_price_types$variations <- factor(total_price_types$variations,
                                       ordered = TRUE,
                                       levels = c(1, 2, 3, 4, 5))

variation_distribution <- c(price_type_0range / 170, 
                            price_type_5range / 170, 
                            price_type_10range / 170,
                            price_type_15range / 170, 
                            price_type_over15range / 170)

# Plot the pie chart.
ggplot(mapping = aes(x = "", y = variation_distribution)) +
  geom_bar(stat = "identity", 
           width = 1, 
           mapping = aes(fill = levels(total_price_types$variations))) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Spectral", 
                    direction = -1,
                    labels = c("0% change", "0% - 5% change", 
                               "5% - 10% change", "10% - 15% change",
                                "> 15% change")) +
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Change in Contract Price Types
         with a Common Vendor")

# Figure 3.2.1

# Store the summary data of price by class
summary_price_21 <- by(pharm_21$price, pharm_21$class, summary)
summary_price_16 <- by(pharm_16$price, pharm_16$class, summary)

# Create a vector of the appropriate size
median_price_21 <- 1:length(class_names)
median_price_16 <- 1:length(class_names)

# Iterate through each class and store the median prices in the 
# previously created vector.
for (i in 1:length(class_names)) {
  
  # Store the median prices only in the vector
  median_price_21[i] <- summary_price_21[[class_names[i]]][["Median"]]
  median_price_16[i] <- summary_price_16[[class_names[i]]][["Median"]]
  
}

# Create a data frame out of the useful information and rename the columns
median_price <- data.frame(cbind(class_names, 
                                 median_price_21, 
                                 median_price_16))
names(median_price) <- c("class", "median21", "median16")

# Coerce median variables into numeric types
median_price$median16 <- as.numeric(median_price$median16)
median_price$median21 <- as.numeric(median_price$median21)

# Create a variable to hold the difference in median prices from 2016 to 2021
median_price$price_change <- median_price$median21 - median_price$median16

# Create a variable to hold the percent price change from 2016 to 2021
median_price$percent_change <- median_price$price_change / median_price$median16

# Plot the percent change in median price for each class
ggplot(data = subset(median_price, class != "Antiseptics and Disinfectants" &
                                   class != "Otic" &
                                   class != "Investigational" &
                                   class != "Miscellaneous" &
                                   class != "Dental and Oral" &
                                   class != "Irrigation and Dialysis")) +
  geom_col(mapping = aes(x = percent_change,
                         y = fct_reorder(class, 
                                         percent_change, 
                                         sum, 
                                         .desc = TRUE), 
                         fill = percent_change > 0), 
           show.legend = FALSE) +
  scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1, 1.5),
                     labels = c("-100%", "-50%", "0%", 
                                "+50%", "+100%", "+150%")) +
  theme_minimal() +
  labs(title = "Percent Median Price Change by Classification",
       x = "Percent Change",
       subtitle = "Top 25 Classifications",
       y = "Classification") +
  scale_fill_manual(values = c(spectral_palette[2], spectral_palette[10]))

# Figure 3.2.2

t <- pharm_21[, c(2, 9)]

vendor_class_map <- t %>% 
  group_by(vendor, class) %>% 
  summarise(Count = n())

vendor_class_map <- vendor_class_map[order(vendor_class_map$Count, 
                                           decreasing = T), ]

top_25 <- vendor_class_map[c(1:25), ]

ggplot(top_25, aes(x = class, y = vendor)) +
  geom_tile(aes(fill = Count))+
  geom_text(aes(label = Count), col = "black") +
  labs(title = "Top Vendor + Classification Pairings",
       y = "Vendor",
       x = "Classification") +
  theme_minimal() +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  scale_x_discrete(labels = function(pharm_21) str_wrap(pharm_21, width = 20)) +
  theme(axis.text.x.bottom = element_text(angle = 330, hjust = 0, size = 7))

# Figure 3.2.3

# Find summary data about the number of classifications each vendor 
# typically sells
num_vendor_appearances <- data.frame(table(vendor_class_map$vendor))
names(num_vendor_appearances) <- c("vendors", "num_classes")

# Plot a boxplot of the number of classifications per vendor
ggplot(data = num_vendor_appearances, mapping = aes(y = num_classes, x = 1)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.3) +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  theme(axis.text.x.bottom = element_blank(), 
        axis.title.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank()) +
  labs(title = "Number of Different Classifications 
                  Sold Per Vendor",
       y = "Number of Classifications")

# Figure 3.2.4

# Plot a histogram of the number of classifications per vendor
ggplot(data = num_vendor_appearances, mapping = aes(x = num_classes)) +
  geom_histogram(bins = ceiling(sqrt(length(num_vendor_appearances$vendors)))) +
  theme_minimal() +
  labs(title = "Number of Different Classifications Sold Per Vendor",
       y = "Count",
       x = "Number of Classifications")

# Figure 3.3.1

ggplot(data = pharm_21, mapping = aes(y = price_type, 
                                      x = ..count.., 
                                      fill = price_type, 
                                      alpha = covered)) +
  scale_alpha_manual(values = c(0.5, 1), labels = c("Not Covered", "Covered")) +
  scale_fill_manual(values = c("FSS" = spectral_palette[9], 
                               "Big 4" = spectral_palette[2], 
                               "VA National Contract" = spectral_palette[10])) +
  theme_void() +
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  geom_bar(position = "fill") + 
  coord_polar() +
  labs(title = "Proportion of Covered vs Not Covered Drugs Per Price Type")

# Figure 3.3.2

ggplot(data = subset(pharm_21, !is.na(pharm_21$class))) +
  geom_bar(mapping = aes(y = fct_infreq(factor(class)), 
                         fill = covered)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c(spectral_palette[2], spectral_palette[10]), 
                    labels = c("Not Covered", "Covered")) +
  labs(title = "Distribution of Covered vs Not Covered Drugs by Classification",
       x = "Count", 
       y = "Classification")

# Figure 3.3.3

ggplot(data = subset(pharm_21, !is.na(class) &
                       class != "Antiseptics and Disinfectants" &
                       class != "Otic" &
                       class != "Investigational" &
                       class != "Miscellaneous" &
                       class != "Dental and Oral" &
                       class != "Irrigation and Dialysis"), 
       mapping = aes(y = fct_infreq(factor(class)), fill = price_type)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_fill_manual(values = c("FSS" = spectral_palette[9], 
                               "Big 4" = spectral_palette[2], 
                               "VA National Contract" = spectral_palette[10])) +
  theme(legend.title = element_blank()) +
  labs(title = "Distribution of Different Price Types Per Classification",
       x = "Proportion",
       subtitle = "Top 25 Classifications",
       y = "Classification")

