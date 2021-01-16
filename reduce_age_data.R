## Simulate age distribution to preserve privacy
library(tidyverse)
library(fitdistrplus)

## Read data
a <- read_csv("data/age_sex_distribution_covid19_deaths_care_homes_private.csv")
a_lng <- a %>% 
  gather("sex", "n", female, male)
a_lng2 <- a_lng[rep(1:nrow(a_lng), a_lng$n),] %>% 
  dplyr::select(-n)

a_nst <- a_lng2 %>%
  dplyr::select(-carehome) %>% 
  mutate(indx = paste(sex, hosptlsd, in_ch_chi, in_ch_uprn, sep = "Z"))
a_lst <- tapply(a_nst$age_year, a_nst$indx, identity, simplify = FALSE)  

## Fit distribution to all data
res <-map(a_lst, ~ fitdistrplus::fitdist(data = .x, distr = "weibull"))
## Graphically the Weibull distribution fits reasonably
pdf("plot_weib_distributions_private.pdf")
map(res, plot)
dev.off()

res2 <- res[map_int(a_lst, length) >= 100]
pdf("plot_weib_distributions_public.pdf")
map(res2, denscomp)
dev.off()

## So sample from weibull distribution to get ages
prms <- map(res, ~ .x$estimate)
prms <- do.call(rbind, prms)
prms <- as_tibble(prms, rownames = "indx")

## add back onto data to simulate ages
a_sim <- a_nst %>% 
  count(hosptlsd, in_ch_chi, in_ch_uprn, sex, indx) %>% 
  left_join(prms)
a_sim$data <- pmap(list(a_sim$n, shape = a_sim$shape, scale = a_sim$scale), function(n, shape, scale){
  rweibull(n, shape, scale)
}
)

## reshape to original format
a_sim2 <- a_sim %>% 
  dplyr::select(-indx, -n, -shape, -scale) %>% 
  unnest(data) %>% 
  rename(age_year = data) %>% 
  mutate(carehome = if_else(in_ch_chi == 0 & in_ch_uprn ==0, "not care home", "care home"))

a_sim3 <- a_sim2 %>% 
  mutate(age_year = round(age_year)) %>% 
  count(age_year, hosptlsd, carehome, in_ch_chi, in_ch_uprn, sex) %>% 
  spread(sex, n, fill = 0L)

write_csv(a_sim3, "data/age_sex_distribution_covid19_deaths_care_homes_public.csv")
