

library(lcmm)

load("~/Documents/MRC epidemiology bits/no_rand_1_to_5.Rdata")

n_groups = 4
ng1 = no_rand_1_to_5[[1]]
ng2 = no_rand_1_to_5[[2]]
ng3 = no_rand_1_to_5[[3]]
ng4 = no_rand_1_to_5[[4]]
ng5 = no_rand_1_to_5[[5]]

summaryplot(ng1, ng2, ng3, ng4, ng5, which=c("entropy", "BIC"))