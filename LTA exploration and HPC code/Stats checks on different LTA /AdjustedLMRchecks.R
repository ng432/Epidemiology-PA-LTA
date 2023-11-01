



library(haven)
library(dplyr)
library(ggplot2)

load(file = "no_rand_1_to_5.Rdata")
load(file = "ordinal_PA_data.Rdata")

# which groups to compare for LMR 
# lg: low group vs high group


lg = no_rand_1_to_5[1] # number of samples
hg = no_rand_1_to_5[2]
adlr1_2 = calc_lrt(lg[[1]]$ns, lg[[1]]$loglik, length(lg[[1]]$best), lg[[1]]$ng, hg[[1]]$loglik, length(hg[[1]]$best), hg[[1]]$ng)

lg = no_rand_1_to_5[2]
hg = no_rand_1_to_5[3]
adlr2_3 = calc_lrt(lg[[1]]$ns, lg[[1]]$loglik, length(lg[[1]]$best), lg[[1]]$ng, hg[[1]]$loglik, length(hg[[1]]$best), hg[[1]]$ng)

lg = no_rand_1_to_5[3]
hg = no_rand_1_to_5[4]
adlr3_4 = calc_lrt(lg[[1]]$ns, lg[[1]]$loglik, length(lg[[1]]$best), lg[[1]]$ng, hg[[1]]$loglik, length(hg[[1]]$best), hg[[1]]$ng)

lg = no_rand_1_to_5[4]
hg = no_rand_1_to_5[5]
adlr4_5 = calc_lrt(lg[[1]]$ns, lg[[1]]$loglik, length(lg[[1]]$best), lg[[1]]$ng, hg[[1]]$loglik, length(hg[[1]]$best), hg[[1]]$ng)


x = c(1, 2, 3, 4)
y = c(adlr1_2[4], adlr2_3[4], adlr3_4[4], adlr4_5[4])
  
plot(y, type = "l", lty = 1, xlab = "Group comparison", ylab = 'Probality', xaxt = "n")
x_labels <- c("1 vs 2 ", "2 vs 3", "3 vs 4 ", "4 vs 5")
axis(1, at = x, labels = x_labels)






