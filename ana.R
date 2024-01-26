# check volatility and count number of records per station

library(dplyr)
library(lme4)
library(mixedup)
library(tibble)

dat <- dat |> dplyr::group_by(STATION) |>
  dplyr::mutate(
    insolation=insolation(J2000_cent(as.POSIXct(DATE)+43200), pi*LATITUDE/180),
    delta_sol= ifelse(DATE==lag(DATE)+1, insolation-lag(insolation), NA),
    maxsun = declination(J2000_cent(as.POSIXct(DATE)+43200))+pi*(90-LATITUDE)/180) |> 
  ungroup()
dat0 <- dplyr::filter(dat, !is.na(delta_sol))
stat800 <- sample(subset(stations, n>1400)$STATION, 800)
dat800 <- dplyr::filter(dat0, STATION %in% stat800)


# (yy <- lme4::lmer(TEMP ~ ELEVATION + insolation + maxsun + delta_sol + (delta_sol|STATION),
#                  control = lme4::lmerControl(optimizer = 'Nelder_Mead'),
#                  data=dat800))
# mixedup::summarise_model(yy)

# best model so far
(yy1 <- lme4::lmer(TEMP ~ ELEVATION + insolation + (insolation|STATION) + maxsun + delta_sol + (delta_sol|STATION),
                  control = lme4::lmerControl(optimizer = 'Nelder_Mead'),
                  data=dat800))
mixedup::summarise_model(yy1)
