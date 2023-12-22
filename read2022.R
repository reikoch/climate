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
