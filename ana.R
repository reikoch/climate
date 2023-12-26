# check volatility and count number of records per station

dat <- dat |> group_by(STATION) |> mutate(delta= ifelse(DATE==lag(DATE)+1, TEMP-lag(TEMP), NA))
volatility <- dat |> group_by(STATION) |> summarise(volatil=mean(abs(delta), na.rm = TRUE))
nrecords <- dat |> group_by(STATION) |> tally()
stations <- stations |> 
  left_join(volatility) |> 
  left_join(nrecords)
