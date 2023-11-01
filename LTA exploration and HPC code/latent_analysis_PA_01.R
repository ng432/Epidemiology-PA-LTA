library(lcmm)




# initially , centre age 


# checking distribution of mmse of paquid

ggplot(paquid) +
  geom_density(kernel = 'gaussian', mapping = aes(x = MMSE)) # histo of data at age 30

# not sure if need to do for my data set... probably worth checking 



load('K:/Behavioural/Nicholas Gregory/Early data analysis/PAdata_fe_46pool.Rdata') 
avd_all$cont_ex = conv_ord_cont(avd_all) # needs to go first as creates NA
avd_all_c = fill_gaps(avd_all) # needs to go second as relies on NA


# creating numeric ID's
avd_all_c$id = as.factor(avd_all_c$bcsid)
levels(avd_all_c$id) = 1:length(levels(avd_all_c$id))
avd_all_c$id = as.numeric(avd_all_c$id)


# Initial Mess Around -----------------------------------------------------

# (consider 'joint' as in accounting for missing data?)
# leave for now 

# latent class linear mixed model


# Looking at linear relationship ------------------------------------------


avd_slp_1_fix = hlme(cont_ex ~ age, subject = 'id', 
                 ng = 1, data = avd_all_c)


# AIC reduces with 2 groups, so good sign
avd_slp_2_fix = hlme(cont_ex ~ age, subject = 'id', 
               ng = 2, data = avd_all_c, mixture = ~ age, B = avd_slp_1_fix)

avd_slp_3_fix = hlme(cont_ex ~ age, subject = 'id', 
                 ng = 3, data = avd_all_c, mixture = ~ age, B = avd_slp_1_fix)


avd_slp_4_fix = hlme(cont_ex ~ age, subject = 'id', 
                 ng = 4, data = avd_all_c, mixture = ~ age, B = avd_slp_1_fix)

# got classes working, now need to decide what model to use...


# do we want mixture and random effect? need to work it out

f_avd = data.frame(age = seq(30, 46, length = 100))

alm_1c_fix = predictY(avd_slp_1_fix, f_avd, var.time = 'age')
alm_2c_fix = predictY(avd_slp_2_fix, f_avd, var.time = 'age')
alm_3c_fix = predictY(avd_slp_3_fix, f_avd, var.time = 'age')
alm_4c_fix = predictY(avd_slp_4_fix, f_avd, var.time = 'age')

plot(alm_2c_fix)

summarytable(avd_slp_1_fix, avd_slp_2_fix)


#lty = 1, lwd = 2, type = "l", col = 1:2, 
   #  bty = "l", xlab = "age", ylab = "exercise",
    # legend = NULL)




# Looking at quadratic relationship ---------------------------------------



avd_slp_q1_fix = hlme(cont_ex ~ poly(age, degree = 2, raw = TRUE), subject = 'id', 
                 ng = 1, data = avd_all_c)

alm_q1c_fix = predictY(avd_slp_q1_fix, f_avd, var.time = 'age')

avd_slp_q2_fix = hlme(cont_ex ~ poly(age, degree = 2, raw = TRUE), subject = 'id', 
                 ng = 2, data = avd_all_c, 
                 mixture = ~ poly(age, degree = 2, raw = TRUE), B = avd_slp_q1_fix)

alm_q2c_fix = predictY(avd_slp_q2_fix, f_avd, var.time = 'age')


avd_slp_q3_fix = hlme(cont_ex ~ poly(age, degree = 2, raw = TRUE), subject = 'id', 
                  ng = 3, data = avd_all_c, 
                  mixture = ~ poly(age, degree = 2, raw = TRUE), B = avd_slp_q1_fix)


alm_q3c_fix = predictY(avd_slp_q3_fix, f_avd, var.time = 'age')




avd_slp_q5_fix = hlme(cont_ex ~ poly(age, degree = 2, raw = TRUE), subject = 'id', 
                  ng = 5, data = avd_all_c, 
                  mixture = ~ poly(age, degree = 2, raw = TRUE), B = avd_slp_q1_fix)

alm_q5c_fix = predictY(avd_slp_q5_fix, f_avd, var.time = 'age')


plot(alm_q2c_fix)



save(sum_tab, file = 'K:/Behavioural/Nicholas Gregory/Early data analysis/table_fixed.Rdata')


# can use summarytable(m1, m2, m2b, m2c, m2d, m3, m3b)
# to summarise and compare different classes found 

