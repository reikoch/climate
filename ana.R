# check volatility and count number of records per station

dat <- dat |> group_by(STATION) |> mutate(delta= ifelse(DATE==lag(DATE)+1, TEMP-lag(TEMP), NA))
volatility <- dat |> group_by(STATION) |> summarise(volatil=mean(abs(delta), na.rm = TRUE))
nrecords <- dat |> group_by(STATION) |> tally()
stations <- stations |> 
  left_join(volatility) |> 
  left_join(nrecords)

yy <- lme4::lmer(TEMP ~ maxsun * ELEVATION + (maxsun|STATION),
                 control = lme4::lmerControl(optimizer = 'Nelder_Mead'),
                 data=subset(dat, STATION %in% subset(stations, n>300)$STATION))
mixedup::summarise_model(yy)

stations <- stations |> 
  dplyr::left_join(structure(tibble::rownames_to_column(lme4::ranef(yy)$STATION, 'STATION'),
                             names=c('STATION', 'rndi', 'rnd_maxsun')))
