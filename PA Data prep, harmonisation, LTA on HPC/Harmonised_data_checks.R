

# initial data analysis...

.libPaths(new = "K:/P7_Behavioural/Nicholas Gregory/R modules")
library(dplyr)
library(ggplot2)

rmv_inc_dat <- function(data) # don't think I need as package meant to account for missing data
{
  avd_all_comp = data[(data$cons != 'No data'),] # works
  # removing people that gave weird answer (e.g. 'don't know')
  
  l_BCID = table(avd_all_comp$bcsid)[table(avd_all_comp$bcsid) == 4] # getting combination of number of appearances and BCID
  l_BCID = attributes(l_BCID)$dimnames[[1]] # extractingg just BCID with 4 appearances 
  # list of BCID with 4 entries
  
  # not quite working sort out 
  
  # just BCID with 4 entries and all data 
  avd_all_comp = avd_all_comp[(avd_all_comp$bcsid %in% l_BCID),]
  return(avd_all_comp)
}

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

# Ordinal data analysis ---------------------------------------------------

avd_all_r = rmv_inc_dat(avd_all)

ggplot(avd_all_r, aes(x = age, fill = cons)) +
  geom_bar(position = 'dodge')




# Continous data analysis -------------------------------------------------

avd_all$cont_ex = conv_ord_cont(avd_all) # needs to go first as creates NA
avd_all_c = fill_gaps(avd_all) # needs to go second as relies on NA

# calculating means and SDs

mean_sd_age <- function(data, age)
{
  mean_a = mean(data$cont_ex[data$age==age], na.rm = TRUE)
  sd_a = sd(data$cont_ex[data$age==age], na.rm = TRUE)
  
  return(c(mean_a, sd_a))
}

stat_30 = mean_sd_age(avd_all_c, 30)
stat_34 = mean_sd_age(avd_all_c, 34)
stat_42 = mean_sd_age(avd_all_c, 42)
stat_46 = mean_sd_age(avd_all_c, 46)

## gaussian plot for avd46 to check distribution 
ggplot(avd46) +
  geom_density(kernel = 'gaussian', mapping = aes(x = sum))

ggplot(avd42) +
  geom_density(kernel = 'gaussian', mapping = aes(x = sum))

ggplot(avd_all_c[avd_all_c$age==30,]) +
  geom_area(stat = 'bin', mapping = aes(x = cont_ex)) # histo of data at age 30

# gaussian plot using data from ordinal to cont
ggplot(avd_all_c[avd_all_c$age==46,]) +
  geom_density(kernel = 'gaussian', mapping = aes(x = cont_ex)) # histo of data at age 30
# nothing implies it is gaussian


# checking plotting by using points
ggplot(avd_all_c) +
  geom_point(aes(x=age, y=cont_ex)) # this works





# plotting line for one individual
ggplot(avd_all_c, aes(x=age, y = cont_ex, colour=bcsid)) +
  geom_line() +
  guides(colour = 'none')# this works
#somewhat un satisfcatory looks v dodge ...
# due to limited number of points, just get lines joining all points


# trying to plot straight line through all points (i.e. linear regression)
ggplot(avd_all_c, aes(x=age, y = cont_ex)) +
  geom_smooth(method = 'lm', formula = y ~ x)

# i.e overall trend positive and upwards 

  


# Messing around / exploring ----------------------------------------------



ggplot(avd_all_c, aes(x=age, y = cont_ex)) +
  geom_bin_2d(binwidth = c(0.25, 0.25))
# gonna try loop 

ggplot(avd_all_c, aes(x=age, y = cont_ex)) +
  geom_density_2d()
# gonna try loop 










