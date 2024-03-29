# read temp data of selected years
# maybe alternate source in https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_year

library(dplyr)
library(readr)

years <- as.character(2020:2023)

for (year in years[!file.exists(years)]) {
  tf <- tempfile('climate', fileext = '.tar.gz')
  download.file(url = paste0('https://www.ncei.noaa.gov/data/global-summary-of-the-day/archive/',
                             year, '.tar.gz'),
                destfile = tf)
  untar(tf, exdir = year)
}

# col_date(format='%F')
coltypes <- readr::cols_only(STATION='c',
                        DATE=readr::col_date(format = '%F'),
                        LATITUDE='d',
                        LONGITUDE='d',
                        ELEVATION='d',
                        NAME='c',
                        TEMP='d')

dat <- dplyr::bind_rows(lapply(list.files(path=years, full.names = TRUE),
                               readr::read_csv, col_types=coltypes))

# fix missing ELEVATION
dat$ELEVATION <- ifelse(dat$ELEVATION == -999.9, NA, dat$ELEVATION)
if (file.exists('repair_elevation.rds')) {
  repair_elevation <- readr::read_rds('repair_elevation.rds')
  dat <- dplyr::left_join(dat, repair_elevation, by = 'STATION', suffix = c('', '.y')) |>
    dplyr::mutate(ELEVATION=dplyr::coalesce(ELEVATION, ELEVATION.y)) |> 
    dplyr::select(-ELEVATION.y)
}
# use km for elevation for numerical reasons
dat$ELEVATION <- dat$ELEVATION/1000

# remove stations with unknown location or unknown elevation
# remove IKERMIUARSUK April 2022 data, they are completely off
dat <- dat |> 
  dplyr::filter(!is.na(LATITUDE) & !is.na(ELEVATION) &
                (STATION!='04382099999' | lubridate::month(DATE)!=4 | lubridate::year(DATE)!=2022))

# convert temperature to celsius
dat$TEMP <- (dat$TEMP-32)/9*5

## prepare stations and its characteristics
stations <- dat[,c('STATION', 'LATITUDE', 'LONGITUDE', 'ELEVATION', 'NAME')] |> 
  dplyr::group_by(STATION) |> 
  dplyr::slice_tail(n=1)
nrecords <- dat |>
  dplyr::group_by(STATION) |> tally()
stations <- stations |> 
  dplyr::left_join(nrecords)
