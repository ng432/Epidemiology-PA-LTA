# initially trying to extract data

# relevant rows are F20A1-F20A43, paired with F20B1-F20B43

library(foreign)

all_data <- read.dta("K:/Behavioural/Nicholas Gregory/Downloaded data/Age16/stata11_se/bcs7016x.dta")

#activity_data <- select(all_data, contains('F20'))

# 'as.factor' how to change data type

s_i <- which(colnames(all_data)=='f20a1')
e_i <- which(colnames(all_data)=='f20b43')

activity_data = all_data[s_i:e_i]

sports_list <- c(
  'baseball','basketball', 'cricket', 'football', 'hockey', 'netball', 'rounders', 
  'rugby', 'volleyball', 'aerobics', 'track / field', 'badminton', 'canoeing',
  'cross-country', 'cycling', 'dancing', 'gymnastics', 'horse-riding', 'jogging',
  'fitness exercises', 'motor cycling', 'roller or ice skating', 'rowing', 
  'sailing', 'scrambling', 'skiing', 'squash', 'swimming', 'table tennis', 'tennis',
  'walking', 'water-skiing', 'weight training', 'wind surfing', 'billiards', 'darts',
  'fishing', 'pool', 'shooting', 'snooker', 'other'
)




