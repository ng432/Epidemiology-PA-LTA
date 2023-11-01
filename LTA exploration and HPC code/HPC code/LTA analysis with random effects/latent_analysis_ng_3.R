

library(lcmm)

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


# need to consdier using previous ordinal thresholds to initalise next one 
# need to iinitialise for 3 groups
# first will do normal method however

load(file = "group_2.Rdata")
load(file = "ordinal_PA_data.Rdata")

avd_ord_sp = lcmm(cons_num ~ age, subject = 'id', random = ~ age, data = avd_all_c, 
                  link = 'splines', ng = 3, mixture = ~age,
                  B = group_2$spline)


group_3 = avd_wrapper(group_2$no_rand, group_2$spline, 3)

save(group_3, file ="group_3.Rdata")




