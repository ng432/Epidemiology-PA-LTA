


library(haven)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

load("~/Documents/MRC epidemiology bits/no_rand_1_to_5.Rdata")
load("~/Documents/MRC epidemiology bits/ordinal_PA_data.Rdata")

source("plot_activity_group.R")

n_groups = 1

group_data = no_rand_1_to_5[[n_groups]]

jgd = group_data$pprob

# first joining
# need to filter out those with "NA"

avd_all_c %>% 
  full_join(jgd, by = c("id")) -> 
  avd_with_groups

# need to plot for single group REMOVING no data entries


p1_all = plot_activity_group(1, TRUE)
p1_rem = plot_activity_group(1, FALSE)


plots = grid.arrange(p1_all, p1_rem,
                     ncol = 2)

