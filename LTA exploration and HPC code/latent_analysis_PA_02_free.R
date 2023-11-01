

.libPaths(new = "K:/P7_Behavioural/Nicholas Gregory/R modules")

library(lcmm)
library(dplyr)
library(ggplot2)

conv_ord_cont <- function(data) # converting ordinal PA frequency data to continous 
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
  
  keeps <- c('cont_ex', 'age', 'bcsid')
  c_avd_all = c_avd_all[keeps]
  
  return(c_avd_all)
  
}

load('K:/P7_Behavioural/Nicholas Gregory/Early data analysis/PAdata_fe_46pool.Rdata') 
avd_all$cont_ex = conv_ord_cont(avd_all) # needs to go first as creates NA
avd_all_c = fill_gaps(avd_all) # needs to go second as relies on NA


# creating numeric ID's
avd_all_c$id = as.factor(avd_all_c$bcsid)
levels(avd_all_c$id) = 1:length(levels(avd_all_c$id))
avd_all_c$id = as.numeric(avd_all_c$id)


# Linear Stuff  -----------------------------------------------------


avd_slp_1 = hlme(cont_ex ~ age, random = ~age, subject = 'id', 
                 ng = 1, data = avd_all_c)


avd_slp_2 = hlme(cont_ex ~ age, subject = 'id', random = ~age,
               ng = 2, data = avd_all_c, mixture = ~ age, B = avd_slp_1)

avd_slp_3 = hlme(cont_ex ~ age, subject = 'id', random = ~age, 
                 ng = 3, data = avd_all_c, mixture = ~ age, B = avd_slp_1)


avd_slp_4 = hlme(cont_ex ~ age, subject = 'id', random = ~age, 
                 ng = 4, data = avd_all_c, mixture = ~ age, B = avd_slp_1)

avd_slp_5 = hlme(cont_ex ~ age, subject = 'id', random = ~age, 
                 ng = 5, data = avd_all_c, mixture = ~ age, B = avd_slp_1)


avd_slp_6 = hlme(cont_ex ~ age, subject = 'id', random = ~age, 
                 ng = 6, data = avd_all_c, mixture = ~ age, B = avd_slp_1)



lin_free_sf = c(avd_slp_1, avd_slp_2, avd_slp_3, avd_slp_4, avd_slp_5)

save(lin_free_sf, file = 'K:/Behavioural/Nicholas Gregory/Early data analysis/1to5lin_free.Rdata')

load('K:/Behavioural/Nicholas Gregory/Early data analysis/1to5lin_free.Rdata')
load('K:/Behavioural/Nicholas Gregory/Early data analysis/linear_6_free.Rdata')
avd_slp_1 = lin_free_sf[1:30]
class(avd_slp_1) = 'hlme'
avd_slp_2 = lin_free_sf[31:60]
class(avd_slp_2) = 'hlme'
avd_slp_3 = lin_free_sf[61:90]
class(avd_slp_3) = 'hlme'
avd_slp_4 = lin_free_sf[91:120]
class(avd_slp_4) = 'hlme'
avd_slp_5 = lin_free_sf[121:150]
class(avd_slp_5) = 'hlme'

summarytable(avd_slp_1, avd_slp_2, avd_slp_3,
             avd_slp_4, avd_slp_5, avd_slp_6)


## haven't tried ones below... iteration cap reached with 6 classes


avd_slp_7 = hlme(cont_ex ~ age, subject = 'id', random = ~age, 
                 ng = 7, data = avd_all_c, mixture = ~ age, B = avd_slp_1)

avd_slp_8 = hlme(cont_ex ~ age, subject = 'id', random = ~age, 
                 ng = 8, data = avd_all_c, mixture = ~ age, B = avd_slp_1)

# got classes working, now need to decide what model to use...


# do we want mixture and random effect? need to work it out

f_avd = data.frame(age = seq(30, 46, length = 100))


alm_1c = predictY(avd_slp_1, f_avd, var.time = 'age')
alm_2c = predictY(avd_slp_2, f_avd, var.time = 'age')
alm_3c = predictY(avd_slp_3, f_avd, var.time = 'age')
alm_4c = predictY(avd_slp_4, f_avd, var.time = 'age')

plot(alm_2c)



#lty = 1, lwd = 2, type = "l", col = 1:2, 
   #  bty = "l", xlab = "age", ylab = "exercise",
    # legend = NULL)




# Looking at quadratic relationship ---------------------------------------



avd_slp_q1 = hlme(cont_ex ~ poly(age, degree = 2, raw = TRUE), subject = 'id', 
                  random = ~poly(age, degree = 2, raw = TRUE),
                 ng = 1, data = avd_all_c)


avd_slp_q2 = hlme(cont_ex ~ poly(age, degree = 2, raw = TRUE), subject = 'id', 
                 ng = 2, data = avd_all_c, 
                 random = ~poly(age, degree = 2, raw = TRUE),
                 mixture = ~ poly(age, degree = 2, raw = TRUE), B = avd_slp_q1)


avd_slp_q3 = hlme(cont_ex ~ poly(age, degree = 2, raw = TRUE), subject = 'id', 
                  ng = 3, data = avd_all_c, 
                  random = ~poly(age, degree = 2, raw = TRUE),
                  mixture = ~ poly(age, degree = 2, raw = TRUE), B = avd_slp_q1)


avd_slp_q5 = hlme(cont_ex ~ poly(age, degree = 2, raw = TRUE), subject = 'id', 
                  ng = 5, data = avd_all_c, 
                  random = ~poly(age, degree = 2, raw = TRUE),
                  mixture = ~ poly(age, degree = 2, raw = TRUE), B = avd_slp_q1)

alm_q5c = predictY(avd_slp_q5, f_avd, var.time = 'age')



#save(sum_tab, file = 'K:/Behavioural/Nicholas Gregory/Early data analysis/table_fixed.Rdata') # actually free woops 
#save(avd_slp_6, file = 'K:/Behavioural/Nicholas Gregory/Early data analysis/linear_6_free.Rdata')


load('K:/Behavioural/Nicholas Gregory/Early data analysis/quad_free_1to4.Rdata')
load('K:/Behavioural/Nicholas Gregory/Early data analysis/quad_free_5unc.Rdata')



avd_slp_q1 = quad_free_1to4[1:30]
class(avd_slp_q1) = 'hlme'
avd_slp_q2 = quad_free_1to4[31:60]
class(avd_slp_q2) = 'hlme'
avd_slp_q3 = quad_free_1to4[61:90]
class(avd_slp_q3) = 'hlme'
avd_slp_q4 = quad_free_1to4[91:120]
class(avd_slp_q4) = 'hlme'


alm_q1c = predictY(avd_slp_q1, f_avd, var.time = 'age')
alm_q2c = predictY(avd_slp_q2, f_avd, var.time = 'age')

# these have not converged...
alm_q3c = predictY(avd_slp_q3, f_avd, var.time = 'age')
alm_q4c = predictY(avd_slp_q4, f_avd, var.time = 'age')


plot(alm_q2c)






# trying to ensure convergence.... 

# need to modify starting point B... 

## trying grid search method first

# centering age around mid point of 38

# don't think it actually helps...
avd_all_c$age38 = (avd_all_c$age - 38) / 10 

avd_slp_q1 = hlme(cont_ex ~ poly(age, degree = 2, raw = TRUE), subject = 'id', 
                  random = ~poly(age, degree = 2, raw = TRUE),
                  ng = 1, data = avd_all_c)


avd_slp_q2_gs_ar = gridsearch(hlme(cont_ex ~ poly(age38, degree = 2, raw = TRUE), subject = 'id', 
                                random = ~poly(age38, degree = 2, raw = TRUE),
                  ng = 2, data = avd_all_c, mixture = ~ poly(age38, degree = 2, raw = TRUE),), 
                  rep = 30 , maxiter = 15, minit = avd_slp_q1)


avd_slp_q3_gs = gridsearch(hlme(cont_ex ~ poly(age, degree = 2, raw = TRUE), subject = 'id', 
                                random = ~poly(age, degree = 2, raw = TRUE),
                                ng = 3, data = avd_all_c, mixture = ~ poly(age, degree = 2, raw = TRUE),), 
                           rep = 20 , maxiter = 20, minit = avd_slp_q1)


summarytable(avd_slp_q1, avd_slp_q2, avd_slp_q3_gs)

alm_q3c_gs = predictY(avd_slp_q3_gs, f_avd, var.time = 'age')

# 
plot(alm_q3c_gs)


# converged ! 
save(avd_slp_q3_gs, file = 'K:/Behavioural/Nicholas Gregory/Early data analysis/q3_free_gs.Rdata')

# if not succesful, try link function thing

# if doesn't show convincing convergence, consider normalising options (or ordinal options)




# trying ordinal options 
# have to use lcmm

#  link = "thresholds" believe need to use this, more info on page 33 of R module thing

avd_slp_1 = lcmm(cont_ex ~ age, random = ~age, subject = 'id', 
                 ng = 1, data = avd_all_c, link = 'thresholds')











