## Cleaning acs file for subsequent use in poststrat

library(foreign)
library(dplyr)

acs_pop <- read.dta("weighting_code/data/acs_nyc_2011_wpov1.dta", convert.factors = FALSE)

acs_ad <- 
  acs_pop %>%
  filter(age >= 18) %>%
  rename(
    edu = educat,
    eth = racex
  ) %>% 
  mutate(
    age = 
      cut(x = age,
          breaks = c(0,34,44,54,64,Inf),
          right = TRUE,
          label = FALSE),
    inc = cut(x = poverty,
              breaks = c(0,50,100,200,300,Inf),
              right = TRUE,
              label = FALSE),
    eld = if_else(eldx > 1, 3, eldx + 1),
    cld = if_else(childx > 2, 4, childx + 1),
    ps = if_else(personx > 4,4,personx),
    age = as.factor(age),
    eth = as.factor(eth),
    edu = as.factor(edu),
    sex = as.factor(sex),
    inc = as.factor(inc),
    eld = as.factor(eld),
    cld = as.factor(cld),
    ps = as.factor(ps)
  ) %>%
  mutate_at(
    .vars = vars(age, eth, edu, sex,
                 inc, eld, cld, ps),
    .funs = funs(int = as.integer(.))
  ) %>% saveRDS('weighting_code/data/acs_ad.RDS')