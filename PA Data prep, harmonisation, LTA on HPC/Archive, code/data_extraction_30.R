# initially trying to extract data

# relevant rows are F20A1-F20A43, paired with F20B1-F20B43

library(foreign)

all_data_30 <- read.dta("K:/Behavioural/Nicholas Gregory/Downloaded data/Age 29/stata11_se/bcs2000.dta")

#activity_data <- select(all_data, contains('F20'))

# exercise: do any reg exer?
# breathls: how freq?
# sweat: how often CM gets out of breath / sweaty?
# 

activity_data_30 <- all_data_30[3642:3644]

y_pa30 <- activity_data_30[activity_data_30$exercise=='Yes',]

n_pa30 <- activity_data_30[activity_data_30$exercise=='No',]

# not sure what to do with these 2 groups....
dk_pa30 <- activity_data_30[activity_data_30$exercise=='Dont know',]
da_na30 <- activity_data_30[activity_data_30$exercise=='Not answered',]
