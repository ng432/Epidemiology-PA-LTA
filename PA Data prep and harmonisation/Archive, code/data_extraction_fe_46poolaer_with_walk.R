
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
avd34 <- extract_PA_data(alld34, c('b7exerse', 'b7sweat'))

# extracting 42 data
alld42 <- read_dta('K:/Behavioural/Nicholas Gregory/Downloaded data/Age 42/stata11/bcs70_2012_flatfile.dta')
avd42 <- extract_PA_data(alld42, c('B9SCQ2A', 'B9SCQ2O'), extra_variables = c('B9EXERSE', 'B9EXERDR') )

# extracting 46 data
alld46 <- read_dta('K:/Behavioural/Nicholas Gregory/Downloaded data/Age 46/stata/stata11/bcs_age46_main.dta')

# trying to now organise the data
# need to add variable of 'age'
# need to consolidate data columns

avd30$cons <- 'Placeholder'
avd30$age <- 30 

do_i = is.na(avd30$exercise)
avd30$cons[do_i] = 34 # marking no data for weird 'na' cases of exercise 

avd30$exercise[do_i] = -100 # marking where there is na for exercise, avoiding na error

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
  }
  else if (x == 8) # 
  {avd30$cons[ex_i] = 34 #32 # meaning they don't know if they exercise
  } 
   else if (x == 9) # 
   { avd30$cons[ex_i] = 34 #33 # meaning un answered to original exercise question
   } 
 
}

# 
# avd30$cons = labelled(avd30$cons,
#          c('Every day' = 1, '4-5 days a week' = 2, '2-3 days a week' = 3, 'Once a week' = 4, 
#            '2-3 times a month' = 5, 'Less often' = 6, 'No exercise' = 7, 
#            'Ex but dont know much' = 30 , 'Ex but didnt ans how much' = 31, 'Dont know if they ex' = 32, 
#            'Unans. to exercise' = 33)
#          )

# lab = c('Every day', '4-5 days a week', '2-3 days a week', 'Once a week', 
#   '2-3 times a month', 'Less often', 'No exercise', 
#   'Ex but dont know much', 'Ex but didnt ans how much', 'Dont know if they ex', 
#   'Unans. to exercise', 'No data', 'refusal')

lab = c('Every day', '4-5 days a week', '2-3 days a week', 'Once a week', 
        '2-3 times a month', 'Less often', 'No exercise', 'No data')


avd30$cons = factor(avd30$cons, levels = c(1:7, 34), # 30:35)
       labels=lab, ordered = TRUE) 


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

avd34$age = 34
avd34$cons = factor(avd34$cons, levels = c(1:7, 34),
                    labels=lab, ordered = TRUE)

## 42
avd42$age = 42
avd42$sum = 0
avd42$cons = 'Placeholder'

for (x in avd42[c(2:11, 13, 16)])
{
  add = numeric(length(avd42$sum))
  add[x>0] <- x[x>0]
  add[add==7] = 0
  add[add==1] = 7
  add[add==2] = 4.5
  add[add==3] = 2.5
  add[add==4] = 1
  add[add==5] = 0.55
  add[add==6] = 0.03 # not sure on this value, just leaving it for now for ease
  avd42$sum <- avd42$sum + add
}

segment_activity <- function(summed, conso)
{
  
  conso[summed >= 5.5 ] = 1
  conso[3.5<= summed & summed < 5.5] = 2 
  conso[1.5<= summed & summed < 3.5] = 3 
  conso[0.77<= summed & summed < 1.5] = 4
  conso[0.33<= summed & summed < 0.77] = 5
  conso[0 < summed & summed < 0.33] = 6
  conso[summed == 0] = 7
  return(conso)
  
}

avd42$cons <- segment_activity(avd42$sum, avd42$cons)


avd42$cons[avd42$B9SCQ2A == -1] = 34

avd42$cons = factor(avd42$cons, levels = c(1:7, 34),
                    labels=lab, ordered = TRUE) 


# doing it for 46
# index for relevant exercises (little arbitrary check spreadsheet)


avd46 <- extract_PA_data(alld46, c('B10Q25A', 'B10Q25IIM'), extra_variables = c('B10EXERSE'))

coln46 = colnames(avd46)

aer_i = seq(which(coln46 == 'B10Q25A'), which(coln46 == 'B10Q25O'), by = 3)

avd46$max_aer = apply(avd46[,aer_i], 1, max) # taking max frequency of aerobic group

rs_i = c(
  seq(which(coln46 == 'B10Q25A'), which(coln46 == 'B10Q25F'), by = 3),
  seq(which(coln46 == 'B10Q25P'), which(coln46 == 'B10Q25S'), by = 3),
  seq(which(coln46 == 'B10Q25U'), which(coln46 == 'B10Q25V'), by = 3),
  seq(which(coln46 == 'B10Q25Y'), which(coln46 == 'B10Q25BB'), by = 3),
  seq(which(coln46 == 'B10Q25HH'), which(coln46 == 'B10Q25II'), by = 3),
  which(coln46 == 'max_aer')
  )
  


avd46$sum = 0
avd46$age = 46
avd46$cons = 'Placeholder'

# Pooling values of 25A-0
# i.e. 35:44




for (x in avd46[rs_i])
{
  add = numeric(length(avd46$sum))

  add[x>0] <- x[x>0]
  add[add==1] = 0
  add[add==7] = 4.5
  add[add==8] = 7
  add[add==6] = 2.5
  add[add==5] = 1
  add[add==4] = 0.55
  add[add==3] = 0.22
  add[add=2] = 0.03 # not sure on this value, just leaving it for now for ease, tracked up from above

  avd46$sum <- avd46$sum + add
}

avd46$cons = segment_activity(avd46$sum, avd46$cons)

# No data
avd46$cons[avd46$B10Q25A == -1] = 34


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

avd_all$cons[avd_all$cons == 'Less often'] = 'No exercise'

save(avd_all, file = 'K:/Behavioural/Nicholas Gregory/Early data analysis/PAdata_fe_46pool_ww.Rdata')


# sum(table(avd_all$bcsid) == 4) check for number of valid counts for 

## data check on avd 46 
# seeing if problem due to combination of aerobic exercise stuff
add = numeric(length(avd42$sum))


avd46$aer_s = 0

check_GEex <- function(a_data, vec_ex) # function to check the total counts of similar exercises
{
  
  sum_c = numeric(length(a_data$cons))
  
  for (x in which(a_data$sum >= 5.5)) # looping through people who do ~7 days of exercise a week
  {
    
    for (j in a_data[x,vec_ex]) # looping round their aerobic exercise
    {
      if (j > 5) # selecting exercises which people do more than once a week
      {
        sum_c[x] = sum_c[x] + 1
      }
    }
  }
  
  return(sum_c)
  
}

avd46$aer_s = check_GEex(avd46, c(seq(which(coln46 == 'B10Q25L'), which(coln46 == 'B10Q25P'), by = 3)))

frac_ma = sum((avd46$aer_s>=3)) / sum((avd46$sum>=5.5)) 

# result 31% of multiple day activities account for 'every day'  (2 or more aerobic exercises)
# 14% for 3 or more 

avd46$team_s = check_GEex(avd46, c(seq(which(coln46 == 'B10Q25Y'),which(coln46 == 'B10Q25BB'), by = 3)))

frac_mt = sum((avd46$team_s>=2)) / sum((avd46$sum>=5.5)) 
# 0.04 % from 2 or more team activities.... 












