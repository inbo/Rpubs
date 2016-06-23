library(readr)
library(dplyr)
library(INLA)
dataset <- read_csv("correlated-random-effects.csv")
dataset$region <- factor(dataset$region)
dataset$country <- factor(dataset$country)
dataset <- dataset %>% 
  group_by(region, country, wave) %>% 
  summarise(
    Present = sum(y == 1), 
    N = n(), 
    x1 = mean(x1, na.rm = TRUE), 
    x2 = mean(x2)
  )
dataset$countrywave <- interaction(dataset$country, dataset$wave)
dataset$regionid <- as.integer(dataset$region)
dataset$regionid1 <- dataset$regionid + max(dataset$regionid)
dataset$regionid2 <- dataset$regionid1 + max(dataset$regionid)
dataset$cwave <- dataset$wave - min(dataset$wave)
n.region <- n_distinct(dataset$region)
m1 <- inla(
  Present ~ x1 + x2 + f(regionid, model = "iid3d", n = 3 * n.region) + f(regionid1, x1, copy = "regionid") + f(regionid2, x2, copy = "regionid") + f(country, model = "iid") + f(countrywave, model = "iid"),
  Ntrials = N,
  data = dataset,
  family = "binomial",
  control.compute = list(dic = TRUE)
)
m2 <- inla(
  Present ~ x1 + x2 + f(country, model = "iid") + f(cwave, replicate = as.integer(country), model = "rw1"),
  Ntrials = N,
  data = dataset,
  family = "binomial",
  control.compute = list(dic = TRUE)
)
m3 <- inla(
  Present ~ x1 + x2 + f(country, model = "iid") + f(countrywave, model = "iid"),
  Ntrials = N,
  data = dataset,
  family = "binomial",
  control.compute = list(dic = TRUE)
)
m4 <- inla(
  Present ~ x1 + x2 + f(country, model = "iid"),
  Ntrials = N,
  data = dataset,
  family = "binomial",
  control.compute = list(dic = TRUE)
)
m1$dic$dic
m2$dic$dic
m3$dic$dic
m4$dic$dic
