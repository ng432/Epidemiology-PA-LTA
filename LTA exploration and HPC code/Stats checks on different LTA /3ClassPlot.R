


library(haven)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

load("~/Documents/MRC epidemiology bits/no_rand_1_to_5.Rdata")
load("~/Documents/MRC epidemiology bits/ordinal_PA_data.Rdata")

source("plot_activity_group.R")

n_groups = 3

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

p2_all = plot_activity_group(2, TRUE)
p2_rem = plot_activity_group(2, FALSE)

p3_all = plot_activity_group(3, TRUE)
p3_rem = plot_activity_group(3, FALSE)

plots = grid.arrange(p1_all, p1_rem,
                     p2_all, p2_rem,
                     p3_all, p3_rem,
                     ncol = 2)

