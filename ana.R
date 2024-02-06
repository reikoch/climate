# check volatility and count number of records per station

library(dplyr)
library(ggplot2)
library(lme4)
library(mixedup)
library(tibble)

# instead of delta_X, try moving exp average of maxsun

dat <- dat |> dplyr::group_by(STATION) |>
  dplyr::mutate(
    insolation=insolation(J2000_cent(as.POSIXct(DATE)+43200), pi*LATITUDE/180),
    delta_sol= ifelse(DATE==lag(DATE)+1, 100*(insolation-lag(insolation)), NA),
    maxsun = cos(pi*LATITUDE/180-declination(J2000_cent(as.POSIXct(DATE)+43200))),
    delta_maxsun=ifelse(DATE==lag(DATE)+1, 100*(maxsun-lag(maxsun)), NA)) |> 
  ungroup()
dat0 <- dplyr::filter(dat, !is.na(delta_sol), !is.na(delta_maxsun))
stat800 <- sample(subset(stations, n>1400)$STATION, 800)
dat800 <- dplyr::filter(dat0, STATION %in% stat800)

# hysteresis loop at dead sea
ggplot(subset(dat0, STATION=='40192199999'), aes(x=insolation, y=TEMP)) + geom_point() +
  ggtitle('sun and temperature @ Sedom (Dead Sea)')

(yy <- lme4::lmer(TEMP ~ ELEVATION + insolation * delta_maxsun + ((insolation + delta_maxsun)|STATION),
                 control = lme4::lmerControl(optimizer = 'Nelder_Mead'),
                 data=dat800))
mixedup::summarise_model(yy)
dat800$res <- residuals(yy)

stations_800 <- dplyr::right_join(stations, tibble::rownames_to_column(lme4::ranef(yy)$STATION, var = 'STATION'))

# best model so far
(yy1 <- lme4::lmer(TEMP ~ ELEVATION + insolation + (insolation|STATION) + maxsun + delta_sol + (delta_sol|STATION),
                  control = lme4::lmerControl(optimizer = 'Nelder_Mead'),
                  data=dat800))
mixedup::summarise_model(yy1)

# remove ELEVATION from data
dat800$temp <- dat800$TEMP - 4.41737*dat800$ELEVATION
(yy2 <- lme4::lmer(temp ~ insolation + (insolation|STATION) + maxsun + delta_sol + (delta_sol|STATION),
                   control = lme4::lmerControl(optimizer = 'Nelder_Mead'),
                   data=dat800))
mixedup::summarise_model(yy2)
dat800$res2 <- residuals(yy2)

ggplot(subset(dat800, STATION=='42379099999'), aes(x=DATE, y=res, color=temp)) + geom_point() + geom_smooth(se=FALSE)

dat800$tempr <- dat800$TEMP + 59.68750*dat800$delta_sol - 2.91161*dat800$maxsun - 1.78965*dat800$insolation 
(yy3 <- lme4::lmer(tempr ~  (insolation|STATION) + (delta_sol|STATION),
                   control = lme4::lmerControl(optimizer = 'Nelder_Mead'),
                   data=dat800))
dat800$resr <- residuals(yy3)
