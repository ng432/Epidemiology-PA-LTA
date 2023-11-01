

library(lcmm)


load(file = "ordinal_PA_data.Rdata")

no_rand_list = vector(mode = "list", length = 7)
minit_list = vector(mode = "list", length = 7)

no_rand_list[[1]] = lcmm(cons_num ~ age, subject = 'id', 
                         random = ~ -1, data = avd_all_c, link = 'thresholds')

for (i in 1:5)
{
  
  minit_list[[i]] <- gridsearch(rep = 50, maxiter = 15, minit = no_rand_list[[1]],
                              lcmm(cons_num ~ age, subject = 'id', random = ~ -1, 
                                   data = avd_all_c, link = 'thresholds', 
                                   ng = i,  mixture = ~ age))
  
  no_rand_list[[i]] = lcmm(cons_num ~ age, subject = 'id', random = ~ -1, 
                           data = avd_all_c, link = 'thresholds', 
                           ng = i,  mixture = ~ age, maxiter = 200,
                           B = minit_list[[i]])
}


save(no_rand_list, file ="no_rand_list.Rdata")






