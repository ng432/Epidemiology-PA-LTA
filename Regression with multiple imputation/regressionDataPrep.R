

library(haven)
library(dplyr)
library(nnet)


# Regression analysis to see effect of exposure (adulthood SEP) and outcome ()

# All variables considering:
# Gender, ethnicity
# Region at 16, School type at 16, parental social class at 16
# Malaise at 16, Health at 16, Health behaviors at 16
# children at 26


# loading data
data_10 <- read_dta("../BCS70 Data/Age 10/stata11_se/sn3723.dta")
derived_10 <- read_dta("../BCS70 Data/Age 10/stata11_se/bcs3derived.dta")
data_30 <- read_dta("../BCS70 Data/Age 29/stata11_se/bcs2000.dta")
derived_16 <- read_dta("../BCS70 Data/Age 16/stata11_se/bcs4derived.dta")
derived_26 <- read_dta("../BCS70 Data/age 26/stata11/bcs5derived.dta")
data_26 <- read_dta("../BCS70 Data/age 26/stata11/bcs96x.dta")


# loading and filtering trajectory class data (outcome) and SEP at 30 (exposure)

load("../Data/four_group_ids.Rdata")

four_group_ids %>%
  filter(age == 30) %>%
  select(all_of(c('BCSID', 'class'))) ->
  selected_four_group_data

selected_four_group_data$class = factor(selected_four_group_data$class)


SEP_data <- read_dta("../Data/BCS70 SEP trajectory class.dta")
colnames(SEP_data)[colnames(SEP_data) == "Class"] <- "SEP"

SEP_data$SEP = factor(SEP_data$SEP) 

selected_four_group_data %>%
  left_join(SEP_data, by = c('BCSID')) ->
  selected_four_group_data



######## extracting age 10 data  #################
# taking gender and parental social class


# extracting gender from non-derived variables

data_10 %>%
  rename("BCSID" = "bcsid") %>%
  select("a4a_2", "BCSID") %>%
  rename("gender" = "a4a_2")  ->
  selected_data_10

# defining NA values for missing data
selected_data_10$gender[selected_data_10$gender == 3] = NA

selected_data_10$gender = 
  factor(selected_data_10$gender, 
         levels = c(1, 2),
         labels = c('Male', 'Female'))

selected_four_group_data %>%
  left_join(selected_data_10, by = c('BCSID')) ->
  data_for_regression





# extracting parental class from derived variables

derived_10 %>%
  select('BCSID', 'BD3PSOC', 'BD3REGN') %>%
  rename(parental_class_10 = BD3PSOC) %>%
  rename(region_10 = BD3REGN) ->
  selected_derived_10

selected_derived_10$parental_class_10[
  selected_derived_10$parental_class_10 %in% c(-2:-1)
] = NA

parental_class_10_labels <- c(
  "V unskilled",
  "IV partly-skilled",
  "III manual",
  "III non-manual",
  "II managerial and Technical",
  "I professional"
)

selected_derived_10$parental_class_10 = 
  factor(selected_derived_10$parental_class_10,
         levels = c(1:6),
         labels = parental_class_10_labels)


selected_derived_10$region_10[
  selected_derived_10$region_10 %in% c(-2:-1)
] = NA



region_levels = c(1:10)
region_labels = c("North", "Yorks and Humberside", "East Midlands","East Anglia", 
                  "South East", "South West", "West Midlands", "North West",
                  "Wales", "Scotland")

# Note: "Northern Ireland","Overseas"have no entries, so are not included in the factor

selected_derived_10$region_10 = factor(selected_derived_10$region_10, 
                                       levels = region_levels,
                                       labels = region_labels)

data_for_regression %>%
  left_join(selected_derived_10, by = c('BCSID')) ->
  data_for_regression
  





######## extracting age 30 data  #################
# ethnicity 

data_30 %>%
  select('bcsid', 'ethnic') %>%
  rename(BCSID = bcsid) ->
  selected_data_30

# creating new ethnicity column, where 1 represents white, and 2 other 
selected_data_30$ethnicity_collapsed = 2

# selecting for 'British', 'Irish', 'White other'
selected_data_30$ethnicity_collapsed[
  selected_data_30$ethnic %in% c(1:3)] = 1

# selecting for 'Dont know', 'Not answered'
selected_data_30$ethnicity_collapsed[
  selected_data_30$ethnic %in% c(98:99)] = NA


selected_data_30$ethnicity_collapsed = factor(
  selected_data_30$ethnicity_collapsed,
  levels = c(1,2),
  labels = c('White', 'Other')
)

selected_data_30 %>%
  select('ethnicity_collapsed', 'BCSID') ->
  selected_data_30

data_for_regression %>%
  left_join(selected_data_30, by = c('BCSID')) ->
  data_for_regression



######## extracting 16 data ###############

# Problem: lots of missing data at 16?

# parent's social class, malaise, school type, region
derived_16_variables = c('BCSID', 'BD4PSOC', 'BD4MAL', 'BD4STYPE', 'BD4REGN')


derived_16 %>%
  select(all_of(derived_16_variables)) ->
  selected_derived_16

selected_derived_16 %>%
  rename('parental_sc_16' = 'BD4PSOC') %>%
  rename('malaise_16' = 'BD4MAL') %>%
  rename('school_type_16' = 'BD4STYPE') %>%
  rename('region_16' = 'BD4REGN') ->
  selected_derived_16


# setting 'parent dead', 'not stated', 'no questionnaire' to NA 
selected_derived_16$parental_sc_16[
  selected_derived_16$parental_sc_16 %in% c(-3:-1)] = NA

parental_sc_16_labels <- c("student","unskilled","partly skilled","skilled manual",
  "skilled non-manual","managerial/technical","professional")

selected_derived_16$parental_sc_16 = factor(
  selected_derived_16$parental_sc_16,
  levels = c(0:6), labels = parental_sc_16_labels)



# setting 'not stated any questions' or 'incomplete info' to NA
selected_derived_16$malaise_16[
  selected_derived_16$malaise_16 %in% c(-2:-1)] = NA

selected_derived_16$malaise_16 = as.numeric(selected_derived_16$malaise_16)

selected_derived_16$school_type_16[
  selected_derived_16$school_type_16 == -1 ] = NA

school_type_16_labels <- c("Comprehensive", "Grammar", "Secondary Modern/Technical",
  "Independent Private","LEA Special","Independent Special","Other","Scottish LEA")

selected_derived_16$school_type_16 = 
  factor(selected_derived_16$school_type_16,
         levels = c(1:8),
         labels = school_type_16_labels)

selected_derived_16$region_16[
  selected_derived_16$region_16 %in% c(-2:-1)] = NA

selected_derived_16$region_16 = 
  factor(selected_derived_16$region_16,
         levels = region_levels,
         labels = region_labels) 


# need health, health behaviors

#exercise:
# F2-0AI: F2-B43

# cigarettes
# F43A:F44

# alcohol
# F56:F58H

# Weight:rd4.1 
# Height:rd2.1
# Other options to ask about:
# RB1: checks for emotional or behavioural problems
# RB2: checks for significant illness, developmental problem, defect or handicap

data_for_regression %>%
  left_join(selected_derived_16 , by = c('BCSID')) ->
  data_for_regression



######## extracting 26 data ###############

# Missing data ~ 2000 data points 


#Number of children: B960333
#'Do not have children': B960334


# collating 2 columns together, and setting invalid entries to 'na' 
data_26$b960333[data_26$b960334 == 1] = 0

data_26$b960333[data_26$b960333 == -8 |
                  data_26$b960333 == -2 |
                  data_26$b960333 == -1] = NA

data_26$b960333 = as.numeric(data_26$b960333)

data_26_variables = c('bcsid', 'b960333')

data_26 %>%
  select(all_of(data_26_variables)) %>%
  rename("BCSID" = 'bcsid') %>%
  rename('children_26' = 'b960333') ->
  selected_data_26
  
derived_26 %>% 
  select('BCSID', 'BD5REGN', 'BD5MAL') %>%
  rename(region_26 = BD5REGN) %>%
  rename(malaise_26 = BD5MAL) ->
  selected_derived_26

selected_derived_26$region_26[
  selected_derived_26$region_26 %in% c(-2:-1)] = NA

selected_derived_26$region_26 = 
  factor(selected_derived_26$region_26,
         levels = region_levels,
         labels = region_labels)


selected_derived_26$malaise_26[
  selected_derived_26$malaise_26 %in% c(-2:-1)] = NA

selected_derived_26$malaise_26 = as.numeric(selected_derived_26$malaise_26)


data_for_regression %>%
  left_join(selected_derived_26, by = c('BCSID')) ->
  data_for_regression
  
data_for_regression %>%
  left_join(selected_data_26, by = c('BCSID')) ->
  data_for_regression


######## CHECKING MISSING DATA COUNT FOR EACH COVARIATE ##################

na_counts <- data.frame(NACount = colSums(is.na(data_for_regression)))


save(data_for_regression, file = "../Data/data_for_regression.Rdata")














