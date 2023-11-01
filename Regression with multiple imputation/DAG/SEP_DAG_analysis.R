


library(ggdag)
library(dagitty)


dag <- dagitty(
  gender <- health_16,
  gender <- malaise_16,
  gender <- health_behaviours_16,
  gender <- schooltype_16,
  ethnicity <- children_26,
  ethnicity <- health_16,
  ethnicity <- malaise_16,
  ethnicity <- health_behaviours_16,
  ethnicity <- schooltype_16,
  ethnicity <- region_16,
  health_16 <- SEP_30,
  malaise_16 <- SEP_30,
  health_behaviours_16 <- SEP_30,
  schooltype_16 <- SEP_30,
  region_16 <- SEP_30,
  parental_sc_16 <- SEP_30,
  gender <- class,
  ethnicity <- class, 
  children_26 <- class,
  health_16 <- class,
  malaise_16 <- class,
  health_behaviours_16 <- class,
  schooltype_16 <- class,
  region_16 <- class,
  parental_sc_16 <- class,
  SEP_30 <- class
)


