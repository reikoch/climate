# read whole year of global meteo data
# from  https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_year

library(readr)
library(dplyr)
library(tidyr)

d_2024 <- readr::read_csv('2024.csv.gz',
                      col_types = list(station=col_character(),
                                       date=col_date(format = '%Y%m%d'),
                                       parm=col_character(),
                                       value=col_double(),
                                       flag_m=col_character(),
                                       flag_q=col_character(),
                                       flag_s=col_character(),
                                       time=col_skip()
                                       ),
                      col_names = c('station', 'date', 'parm', 'value', 'flag_m', 'flag_q', 'flag_s', 'time')
)

stations <- readr::read_fwf('ghcnd-stations.txt', 
                            col_positions = readr::fwf_widths(c(11, 9, 10, 7, 29, 4, 5, NA),
                              col_names = c('station', 'lat', 'long', 'elevation', 'station_name', 'state',
                                            'gsn', 'wmo_id')))

# keep only core weather parameters
dat2024 <- dplyr::right_join(stations[,1:4], dplyr::select(d_2024, !starts_with('flag'))) |> 
  dplyr::filter(parm %in% c('PRCP', 'SNOW', 'SNWD', 'TMAX', 'TMIN'), elevation != -999.9) |> 
  tidyr::pivot_wider(names_from = 'parm', values_from = 'value') |> 
  dplyr::mutate(across(c('TMAX', 'TMIN', 'PRCP'), \(X) X/10))

# subset to stations with >nmin measurements
nmin <- 60
good_stations <- unique(dplyr::group_by(dat2024, station)
                        |> dplyr::tally()
                        |> dplyr::filter(n>nmin)
                        |> dplyr::select(station))

good2024 <- dplyr::left_join(good_stations, dat2024) |> 
  dplyr::mutate(J2000T = J2000T(date), .after=date)

# some visualization
ggplot(good2024, aes(x=TMIN, y=TMAX)) + geom_point() + geom_smooth()

