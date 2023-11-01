
.libPaths(new = "K:/P7_Behavioural/Nicholas Gregory/R modules")

library(lcmm)
library(dplyr)

conv_ord_cont <- function(data) # converting ordinal PA frequency data to continuous 
{
  cont_d = numeric(length(data$cons))
  
  conv_t = c(7, 4.5, 2.5, 1, 0.56, 0, 0, NA)
  i = 1
  
  for (x in attributes(data$cons)$levels)
  {
    con_i = (data$cons == x)
    cont_d[con_i] = conv_t[i]
    i <- i + 1 
    
    
  }
  return(cont_d)
  
}

fill_gaps <- function(data) # ensuring any bcsid missing for year is represented as 'na'
{
  
  fl_BCID = table(data$bcsid)
  fl_BCID = attributes(fl_BCID)$dimnames[[1]] # full list of BCID
  
  ages = c(30, 34, 42, 46)
  
  c_avd_nd = data.frame(
    bcsid = rep(fl_BCID, each = 4),
    age = rep(ages, times = length(fl_BCID))
  )
  
  c_avd_nd$cont_ex = NA
  
  c_avd_all = right_join(data, c_avd_nd, by = c('age', 'bcsid' ))
  
  c_avd_all$cont_ex = c_avd_all$cont_ex.x
  
  keeps <- c('cont_ex', 'age', 'bcsid', 'cons')
  c_avd_all = c_avd_all[keeps]
  
  return(c_avd_all)
  
}

load('K:/P7_Behavioural/Nicholas Gregory/Early data analysis/PAdata_fe_46pool.Rdata') 
avd_all$cont_ex = conv_ord_cont(avd_all) # needs to go first as creates NA
avd_all_c = fill_gaps(avd_all) # needs to go second as relies on NA

# creating numeric ID's from BCSID
avd_all_c$id = as.factor(avd_all_c$bcsid)
levels(avd_all_c$id) = 1:length(levels(avd_all_c$id))
avd_all_c$id = as.numeric(avd_all_c$id)

avd_all_c$cons_num <- as.numeric(avd_all_c$cons)
avd_all_c$cons_num[avd_all_c$cons_num==8] = NA

save(avd_all_c, file = "ordinal_PA_data.Rdata")


# avd_all_c is data organised for latent analysis. Note as 4 data points, for
# each ID there is 4 copies for each recorded age 
# contains following columns:
# BCSID:  ID for individual from the 1973 cohort
# ID: ID starting from 1, necessary for LCMM package
# age: ages at recorded PA - 30, 34, 42, 46
# cont_ex: exercise in continuous format
# cons: exercise organised into factors
# cons_num: exercise organised into numeric levels


# Attempted analysis using ordinal (fixed i.e. quicker)  -----------------------------------------------------
# Remembering data is defined using ordinal, this code aims to carry out
# latent analysis on ordinal data of physical activity, using lcmm module

# latent analysis into 1 group, using continuous data
# fixed (no random effects, need to double check), linear model, 1 group
# note, no mixed model as only defined with more than 2 groups
avd_slp_1_fix = hlme(cont_ex ~ age, subject = 'id', 
                     ng = 1, data = avd_all_c)

# removing random effects to reduce estimation time

# converting ordinal PA frequency data to continuous 
# takes non random ordinal classes, spline classes, and number of groups 
create_bin <- function(ordinal_nr, spline, ng) 
{
  n_ev = (ng-1)*3# number of extra values necessary in bigger calls
  print(n_ev)
  # vector of initial values 
  binit <- vector('numeric', length = 9)
  ## need to spline guesses for ages and co-variances
  binit[1:(4+n_ev)] <- spline$best[1:(4+n_ev)] 
  # need to threshold guesses from non random threshold link 
  binit[(5+n_ev):(9+n_ev)] <- ordinal_nr$best[(2+n_ev):(6+n_ev)] 
  
  return(binit)
}


# Thresholds maps to ordinal
# Random = ~ -1
# Page 33, shows removal of random effects, and necessity of finding initial 
# values like so
avd_ord_nr = lcmm(cons_num ~ age, subject = 'id', random = ~ -1, data = avd_all_c, link = 'thresholds')

# spline interpolation (not threshold), necessary for in
avd_ord_sp = lcmm(cons_num ~ age, subject = 'id', random = ~ age, data = avd_all_c, link = 'splines')

# calling function to create initial bin values for 1 group latent analysis
binit = create_bin(avd_ord_nr, avd_ord_sp)

# Linear, and ng = 1 (which is default of function)
avd_ord_1 = lcmm(cons_num ~ age, subject = 'id', random = ~ age, data = avd_all_c, 
                 link = 'thresholds',
                 B = binit)

save(avd_ord_1, 
     file = 'K:/P7_Behavioural/Nicholas Gregory/Early data analysis/Ordinal_linear_ng1_rand.Rdata')

# Loads avd_ord_1 necessary for initial Bin values for 2 latent groups
load('K:/P7_Behavioural/Nicholas Gregory/Early data analysis/Ordinal_linear_ng1_rand.Rdata')

# Creating bin for good starting point for ordinal at 2 classes 
avd_ord_nr_2 = lcmm(cons_num ~ age, subject = 'id', random = ~ -1, data = avd_all_c, 
                 link = 'thresholds', ng = 2,  mixture = ~ age,
                 B = avd_ord_nr)
avd_ord_sp_2 = lcmm(cons_num ~ age, subject = 'id', random = ~ age, data = avd_all_c, 
                    link = 'splines', ng = 2, mixture = ~age,
                    B = avd_ord_sp)


# creating vector for initial values using spline 
# uses values from spline and non-random threshold models
binit_2 = create_bin(avd_ord_nr_2, avd_ord_sp_2, 2)

# Next step: get this function working on HPC
avd_ord_2 = lcmm(cons_num ~ age, subject = 'id', random = ~ age, data = avd_all_c, 
                 link = 'thresholds', ng = 2,  mixture = ~ age,
                 B = binit_2)


# plotting results
f_avd = data.frame(age = seq(30, 46, length = 100))

ord_y_1 = predictY(avd_ord_1, f_avd, var.time = 'age')


# this shows exercise reducing (i think)... so somewhat working 
plot(ord_y_1)


