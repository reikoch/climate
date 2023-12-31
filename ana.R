# check volatility and count number of records per station

library(dplyr)
library(lme4)
library(mixedup)
library(tibble)

dat <- dat |> dplyr::group_by(STATION) |>
  dplyr::mutate(delta= ifelse(DATE==lag(DATE)+1, TEMP-lag(TEMP), NA))
volatility <- dat |> dplyr::group_by(STATION) |>
  dplyr::summarise(volatil=mean(abs(delta), na.rm = TRUE))
nrecords <- dat |> dplyr::group_by(STATION) |> tally()
stations <- stations |> 
  dplyr::left_join(volatility) |> 
  dplyr::left_join(nrecords)

yy <- lme4::lmer(TEMP ~ ELEVATION + maxsun + (maxsun|STATION),
                 control = lme4::lmerControl(optimizer = 'Nelder_Mead'),
                 data=subset(dat, STATION %in% subset(stations, n>300)$STATION))
mixedup::summarise_model(yy)

stations <- stations |> 
  dplyr::left_join(structure(tibble::rownames_to_column(lme4::ranef(yy)$STATION, 'STATION'),
                             names=c('STATION', 'rndi', 'rnd_maxsun')))
ggplot(subset(stations, !is.na(rnd_maxsun)), aes(x=LONGITUDE, y=LATITUDE, color=rndi)) + geom_point()

# problem: seasonal residuals
ggplot(data = subset(dat2, STATION=='71796099999'), aes(x=DATE, y=res, color=TEMP)) + geom_point() + geom_smooth()
