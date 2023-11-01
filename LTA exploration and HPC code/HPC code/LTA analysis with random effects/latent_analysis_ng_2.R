.libPaths(new = "K:/P7_Behavioural/Nicholas Gregory/R modules")

library(lcmm)

# Analysis using ordinal GROUPS (fixed i.e. quicker)  -----------------------------------------------------
# Remembering data is defined using ordinal, this code aims to carry out
# latent analysis with random effects on ordinal data of physical activity, using lcmm module

load(file = "ordinal_PA_data.Rdata")
# avd_all_c is data organised for latent analysis. Note as 4 data points, for
# each ID there is 4 copies for each recorded age 
# contains following columns:
# BCSID:  ID for individual from the 1973 cohort
# ID: ID starting from 1, necessary for LCMM package
# age: ages at recorded PA - 30, 34, 42, 46
# cont_ex: exercise in continuous format
# cons: exercise organised into factors
# cons_num: exercise organised into numeric levels


# Creating correct data structure to initialize latent analysis
# takes non-random ordinal classes, spline classes, and number of groups 
create_bin <- function(ordinal_nr, spline, ng) 
{
  n_ev = (ng-1)*3# number of extra values necessary in bigger calls

  # vector of initial values 
  binit <- vector('numeric', length = 9)

  # need spline guesses for ages and co-variances
  binit[1:(4+n_ev)] <- spline$best[1:(4+n_ev)] 

  # need threshold guesses from non random threshold link 
  binit[(5+n_ev):(9+n_ev)] <- ordinal_nr$best[(2+n_ev):(6+n_ev)] 
  
  return(binit)
}


avd_wrapper <- function(prev_no_rand, prev_spline, gs)
{
  
  avd_ord_nr = lcmm(cons_num ~ age, subject = 'id', random = ~ -1, data = avd_all_c, 
                    link = 'thresholds', ng = gs,  mixture = ~ age,
                    B = prev_no_rand)
  
  avd_ord_sp = lcmm(cons_num ~ age, subject = 'id', random = ~ age, data = avd_all_c, 
                      link = 'splines', ng = gs, mixture = ~age,
                      B = prev_spline)
  
  binit = create_bin(avd_ord_nr, avd_ord_sp, gs)
  
  avd_ord = lcmm(cons_num ~ age, subject = 'id', random = ~ age, data = avd_all_c, 
                   link = 'thresholds', ng = gs,  mixture = ~ age,
                   B = binit)
  
  out_list <- list("no_rand" = avd_ord_nr, "spline" = avd_ord_sp,  
                   "binit" = binit, "rand" = avd_ord)
  
  return(out_list)
  
}


# latent analysis into 1 group, using continuous data
# fixed, linear model, 1 group
# note, no mixed model as only defined with more than 2 groups
avd_slp_1_fix = hlme(cont_ex ~ age, subject = 'id', 
                     ng = 1, data = avd_all_c)


# Thresholds maps to ordinal
# Random = ~ -1
# Page 33, shows removal of random effects, and necessity of finding initial 
# values like so
avd_ord_nr = lcmm(cons_num ~ age, subject = 'id', random = ~ -1, data = avd_all_c, link = 'thresholds')

# spline interpolation (not threshold), necessary for in
avd_ord_sp = lcmm(cons_num ~ age, subject = 'id', random = ~ age, data = avd_all_c, link = 'splines')

# calling function to create initial bin values for 1 group latent analysis
binit = create_bin(avd_ord_nr, avd_ord_sp, ng = 1)

# Linear, and ng = 1 (which is default of function)
avd_ord_1 = lcmm(cons_num ~ age, subject = 'id', random = ~ age, data = avd_all_c, 
                 link = 'thresholds',
                 B = binit)

save(avd_ord_1, 
     file = 'ordinal_linear_ng1_rand.Rdata')

# group2 = avd_wrapper(avd_ord_nr, avd_ord_sp, 2)

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


avd_ord_2 = lcmm(cons_num ~ age, subject = 'id', random = ~ age, data = avd_all_c, 
                 link = 'thresholds', ng = 2,  mixture = ~ age,
                 B = binit_2)


save(avd_ord_2, file = "ordinal_linear_ng2_rand.Rdata")














