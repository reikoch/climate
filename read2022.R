# retrieve global weather data 2022

library(dplyr)
library(readr)

tf <- tempfile('climate', fileext = '.tar.gz')
download.file(url = 'https://www.ncei.noaa.gov/data/global-summary-of-the-day/archive/2022.tar.gz',
              destfile = tf)
untar(tf, exdir = '2022')

coltypes <- readr::cols(STATION='c',
                        DATE='D',
                        LATITUDE='d',
                        LONGITUDE='d',
                        ELEVATION='d',
                        NAME='c',
                        TEMP='d',
                        TEMP_ATTRIBUTES='i',
                        DEWP='d',
                        DEWP_ATTRIBUTES='i',
                        SLP='d',
                        SLP_ATTRIBUTES='i',
                        STP='d',
                        STP_ATTRIBUTES='i',
                        VISIB='d',
                        VISIB_ATTRIBUTES='i',
                        WDSP='d',
                        WDSP_ATTRIBUTES='i',
                        MXSPD='d',
                        GUST='d',
                        MAX='d',
                        MAX_ATTRIBUTES='c',
                        MIN='d',
                        MIN_ATTRIBUTES='c',
                        PRCP='d',
                        PRCP_ATTRIBUTES='c',
                        SNDP='d',
                        FRSHTT='c'
                        )
dat <- dplyr::bind_rows(lapply(list.files(path='2022', full.names = TRUE),
                              readr::read_csv, col_types=coltypes))

# fix missing values
dat$ELEVATION <- ifelse(dat$ELEVATION == -999.9, NA, dat$ELEVATION)
dat$DEWP <- ifelse(dat$DEWP == 9999.9, NA, dat$DEWP)
dat$SLP <- ifelse(dat$SLP == 9999.9, NA, dat$SLP)
dat$STP <- ifelse(dat$STP == 999.9, NA, dat$STP)
dat$VISIB <- ifelse(dat$VISIB == 999.9, NA, dat$VISIB)
dat$WDSP <- ifelse(dat$WDSP == 999.9, NA, dat$WDSP)
dat$MXSPD <- ifelse(dat$MXSPD == 999.9, NA, dat$MXSPD)
dat$GUST <- ifelse(dat$GUST == 999.9, NA, dat$GUST)
dat$MAX <- ifelse(dat$MAX == 9999.9, NA, dat$MAX)
dat$MIN <- ifelse(dat$MIN == 9999.9, NA, dat$MIN)
dat$PRCP <- ifelse(dat$PRCP == 99.99, 0, dat$PRCP)
dat$SNDP <- ifelse(dat$SNDP == 999.9, 0, dat$SNDP)

# fix missing elevations if repair_elevations is available
if (file.exists('repair_elevation.rds')) {
  repair_elevation <- readr::read_rds('repair_elevation.rds')
  dat <- dplyr::left_join(dat, repair_elevation, by = 'STATION', suffix = c('', '.y')) |>
    dplyr::mutate(ELEVATION=dplyr::coalesce(ELEVATION, ELEVATION.y)) |> 
    dplyr::select(-ELEVATION.y)
}

# remove stations with unknown location
dat <- dat |> 
  dplyr::filter(!is.na(LATITUDE))

# metricize
dat <- dat |> 
  dplyr::mutate(dplyr::across(c('TEMP', 'DEWP', 'MAX', 'MIN'), ~ (.x -32)/9*5)) |> 
  dplyr::mutate(VISIB=VISIB*1.60934, PRCP=PRCP*25.4, SNDP=SNDP*25.4,
                WDSP=WDSP*0.5144444, MXSPD=MXSPD*0.5144444, GUST=GUST*0.5144444)

# stations
stations <- unique(dat[,c('STATION', 'LATITUDE', 'LONGITUDE', 'ELEVATION', 'NAME')])
