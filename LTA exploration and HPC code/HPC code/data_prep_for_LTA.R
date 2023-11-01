

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





