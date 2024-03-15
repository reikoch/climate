# read whole year of global meteo data
# from  https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_year

library(readr)

dat2024 <- readr::read_csv('2024.csv.gz',
                      col_types = list(station=col_character(),
                                       date=col_date(format = '%Y%m%d'),
                                       parm=col_character(),
                                       value=col_double(),
                                       flag_m=col_character(),
                                       flag_q=col_character(),
                                       flag_s=col_character(),
                                       time=col_time(format = '%H %M')
                                       ),
                      col_names = c('station', 'date', 'parm', 'value', 'flag_m', 'flag_q', 'flag_s', 'time')
)

stations <- readr::read_fwf('ghcnd-stations.txt', 
                            col_positions = readr::fwf_widths(c(11, 9, 10, 7, 29, 4, 5, NA),
                              col_names = c('station', 'lat', 'long', 'elevation', 'station_name', 'state',
                                            'gsn', 'wmo_id')))
