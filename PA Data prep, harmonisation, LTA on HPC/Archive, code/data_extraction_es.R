
.libPaths(new = "K:/Behavioural/Nicholas Gregory/R modules")

library(haven)
library(dplyr)

extract_PA_data <- function(data, variable_names, extra_variables=NULL)
  {
s_i <- which(colnames(data)==variable_names[1])
e_i <- which(colnames(data)==variable_names[2])

if (length(extra_variables) == 0) 
{ PA_data <- data[c(1, s_i:e_i)] }
else
{
  ev_v <- c()
  for (x in extra_variables) {
    ev_i <- which(colnames(data)==x)
    ev_v <- c(ev_v, ev_i)
  }
  PA_data <- data[c(1, s_i:e_i, ev_v)]
  }
}

# extracting 30 data
alld30 <- read_dta("K:/Behavioural/Nicholas Gregory/Downloaded data/Age 29/stata11_se/bcs2000.dta")
avd30 <- extract_PA_data(alld30, c('exercise', 'sweat'))

# extracting 34 data
alld34 <- read_dta("K:/Behavioural/Nicholas Gregory/Downloaded data/age 34/stata/stata13_se/bcs_2004_followup.dta")

# extracting 42 data
alld42 <- read_dta('K:/Behavioural/Nicholas Gregory/Downloaded data/Age 42/stata11/bcs70_2012_flatfile.dta')
avd42 <- extract_PA_data(alld42, c('B9SCQ2A', 'B9SCQ2O'), extra_variables = c('B9EXERSE', 'B9EXERDR') )

# extracting 46 data
alld46 <- read_dta('K:/Behavioural/Nicholas Gregory/Downloaded data/Age 46/stata/stata11/bcs_age46_main.dta')
avd46 <- extract_PA_data(alld46, c('B10Q25A', 'B10Q25IIM'), extra_variables = c('B10EXERSE'))

# trying to now organise the data
# need to add variable of 'age'
# need to consolidate data columns


# NOT WORKING NEEDS SORTING

avd30$cons <- 'Placeholder'
avd30$age <- 30 

do_i = is.na(avd30$exercise)
avd30$cons[do_i] = 34 # marking no data for weird 'na' cases of exercise 

avd30$exercise[do_i] = -100 # marking where there is na for exercise, avoiding na error
avd30$sweat[do_i] = -100 # marking na in exercise, for sweat

for (x in attributes(avd30$exercise)$labels)
{
  
  ex_i = (avd30$exercise == x)
  if (x == 1) # Yes
  {
    avd30$cons[ex_i] = avd30$breathls[ex_i]
    
    dk_i = (avd30$cons == 8)
    avd30$cons[dk_i] <- 34#30 # exercise but don't know how much
    
    
    dk_i = (avd30$cons == 9)
    avd30$cons[dk_i] <- 34#31 # exercise but didn't answer how much
    
    
  }
  else if (x == 2) # No 
  {avd30$cons[ex_i] = 7 # meaning they do no exercise
  avd30$sweat[ex_i] = -70
  }
  else if (x == 8) # 
  {avd30$cons[ex_i] = 34 #32 # meaning they don't know if they exercise
  avd30$sweat[ex_i] = -80 # just
  } 
  else if (x == 9) # 
  { avd30$cons[ex_i] = 34 #33 # meaning un answered to original exercise question
  avd30$sweat[ex_i] = -90 # just
  } 
  
}



avd30$cons = strtoi(avd30$cons)

 
for (x in attributes(avd30$sweat)$labels) {
  
  sw_i = (avd30$sweat == x)
  
  if (x == 1) { # always gets sweaty 
    # leave it 
  }
  else if (x == 2) { # sometimes gets sweaty
    
    avd30$cons[sw_i] <- avd30$cons[sw_i] + 1
    
  }
  
  else if (x == 3) { # rarely gets sweaty
    
    avd30$cons[sw_i] = avd30$cons[sw_i] + 2
    
  }
  
  else if (x == 4) { # never gets sweaty
    avd30$cons[sw_i] = 7
  }
  
  else if (x == 9) { # not answered
    avd30$cons[sw_i] = 34
  }

  
}

avd30$cons[(avd30$cons > 7 ) & (avd30$cons < 34)] = 7 # capping limit

lab = c('Every day', '4-5 days a week', '2-3 days a week', 'Once a week', 
        '2-3 times a month', 'Less often', 'No exercise', 'No data')


avd30$cons = factor(avd30$cons, levels = c(1:7, 34), # 30:35)
       labels=lab, ordered = TRUE) 



# 34 data extraction


avd34 <- extract_PA_data(alld34, c('b7exerse', 'b7sweat'))
avd34$cons = 'Placeholder'

for (x in attributes(avd34$b7exerse)$labels)
  {
  
  e_i = (avd34$b7exerse == x)
  
  if (x == - 9) # refusal
  {
    avd34$cons[e_i] = 34#35
  }
  else if (x == -8)  # don't know
  {
    avd34$cons[e_i] = 34 # 32 
  }
  else if (x == -7) # missing 
  {
    avd34$cons[e_i] = 34 # 34
  }
  else if (x == 2) # no exercise
  {
    avd34$cons[e_i] = 7
  }
  else
  {
    avd34$cons[e_i] = avd34$b7breals[e_i]
  }
}


avd34$cons = strtoi(avd34$cons)

avd34$cons[avd34$b7breals == -8] = 34


for (x in attributes(avd34$b7sweat)$labels) {
  
  sw_i = (avd34$b7sweat == x)
  
  if (x == 1) {
    
  }
  else if (x == 2) {
    
    avd34$cons[sw_i] = avd34$cons[sw_i] + 1
    
  }
  
  else if (x == 3) {
    
    avd34$cons[sw_i] = avd34$cons[sw_i] + 2
  }
  
  else if (x == 4) {
    avd34$cons[sw_i] = 7
  }
  
  else if ((x == -9) | (x == -8) | (x == -7)) {
    avd34$cons[sw_i] = 34
  }
  
  
}

avd34$cons[(avd34$cons > 7 ) & (avd34$cons < 34)] = 7 # capping limit

avd34$cons[avd34$b7breals==-8] = 34

avd34$age = 34
avd34$cons = factor(avd34$cons, levels = c(1:7, 34),
                    labels=lab, ordered = TRUE)


# function necessary for segementing 42 / 46

segment_activity <- function(summed, conso)
{
  
  conso[summed >= 5.5 ] = 1 # every day
  conso[3.5<= summed & summed < 5.5] = 2 # 4 -5 days
  conso[1.5<= summed & summed < 3.5] = 3 # 2 - 3days
  conso[0.77<= summed & summed < 1.5] = 4 # once a week
  conso[0.33<= summed & summed < 0.77] = 5 # 2 -3 times a month
  conso[0 < summed & summed < 0.33] = 6 # less often
  conso[summed == 0] = 7
  return(conso)
  
}

## 42 segmentation


avd42$age = 42
avd42$cons = 'Placeholder'

avd42$cons <- segment_activity(avd42$B9EXERSE, avd42$cons)
avd42$cons[((avd42$B9EXERSE == -9) |  (avd42$B9EXERSE == -8) | (avd42$B9EXERSE == -1))] = 34

avd42$cons = factor(avd42$cons, levels = c(1:7, 34),
                    labels=lab, ordered = TRUE) 



# doing it for 46
# index for relevant exercises (little arbitrary check spreadsheet)


avd46$age = 46
avd46$cons = 'Placeholder'

avd46$cons = segment_activity(avd46$B10EXERSE, avd46$cons)

# No data
avd46$cons[(avd46$B10EXERSE == -9) | (avd46$B10EXERSE == -8) | (avd46$B10EXERSE == -1)] = 34

avd46$cons = factor(avd46$cons, levels = c(1:7, 34), # 30:35)
                    labels=lab, ordered = TRUE) 




#now need to merge
# wanting to merge avd, 30, 34, 42, 46

avd30 %>% 
  full_join(avd34, by = c('cons', 'age', 'bcsid' )) %>% 
  full_join(avd42, by = c('cons', 'age', 'bcsid' = 'BCSID')) %>% 
  full_join(avd46, by = c('cons', 'age', 'bcsid' = 'BCSID')) %>% 
  select(c('cons', 'age', 'bcsid')) ->
  avd_all


levels(avd_all$cons)[(levels(avd_all$cons) == '2-3 times a month') | 
                       (levels(avd_all$cons) == 'Less often')]<- 'No exercise'

save(avd_all, file = 'K:/Behavioural/Nicholas Gregory/Early data analysis/PAdata_es.Rdata')


# sum(table(avd_all$bcsid) == 4) check for number of valid counts for 





















