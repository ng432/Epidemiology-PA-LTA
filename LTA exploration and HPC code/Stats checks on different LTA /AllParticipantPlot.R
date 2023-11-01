

library(haven)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(viridisLite)

load("~/Documents/MRC epidemiology bits/no_rand_1_to_5.Rdata")
load("~/Documents/MRC epidemiology bits/ordinal_PA_data.Rdata")

n_groups = 4
group_data = no_rand_1_to_5[[n_groups]]
jgd = group_data$pprob
avd_all_c %>% 
  full_join(jgd, by = c("id")) -> 
  avd_with_groups

plot_all_groups <- function(remove_no_data)
{
  if (remove_no_data == TRUE)
  {
    avd_with_groups %>%
      filter(cons=="No data" | is.na(class) | is.na(cons))  %>%
      select(id) %>%
      distinct()->
      bad_ids
  } else
  {
    avd_with_groups %>%
      filter(is.na(class))  %>%
      select(id) %>%
      distinct()->
      bad_ids
  }
  
  avd_with_groups %>%
    filter(!id %in% bad_ids$id) ->
    plot_data
  
  pa_colours = viridis(8)
  plot_colours = c(
    'Every day' = pa_colours[7], 
    '4-5 days a week' = pa_colours[6],
    '2-3 days a week' = pa_colours[5],
    'Once a week' = pa_colours[4],
    '2-3 times a month' = pa_colours[3],
    'Less often'=  pa_colours[2],
    'No exercise' = pa_colours[1],
    'No data'= 'grey')
  
  # only does something if  has been leeft in 
  plot_data$cons[is.na(plot_data$cons)] =  "No data"
  
  value = c(1)
  
  # need to first join 
  plot <- ggplot(plot_data, aes(fill=cons, x= age, y = value)) + 
    geom_bar(position = "fill", stat="identity")  +
    scale_fill_manual(values = plot_colours)
  
  
  return(plot)
  
}

p1all = plot_all_groups(TRUE)
p2all = plot_all_groups(FALSE)

grid.arrange(p1all, p2all, ncol = 2)
