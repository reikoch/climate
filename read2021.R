# retrieve global weather dat2021a 2021

library(dplyr)
library(readr)

if (!file.exists('2021')) {
  tf <- tempfile('climate', fileext = '.tar.gz')
  download.file(url = 'https://www.ncei.noaa.gov/dat2021a/global-summary-of-the-day/archive/2021.tar.gz',
                destfile = tf)
  untar(tf, exdir = '2021')
}

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
dat2021 <- dplyr::bind_rows(lapply(list.files(path='2021', full.names = TRUE),
                              readr::read_csv, col_types=coltypes))

# fix missing values
dat2021$ELEVATION <- ifelse(dat2021$ELEVATION == -999.9, NA, dat2021$ELEVATION)
dat2021$DEWP <- ifelse(dat2021$DEWP == 9999.9, NA, dat2021$DEWP)
dat2021$SLP <- ifelse(dat2021$SLP == 9999.9, NA, dat2021$SLP)
dat2021$STP <- ifelse(dat2021$STP == 999.9, NA, dat2021$STP)
dat2021$VISIB <- ifelse(dat2021$VISIB == 999.9, NA, dat2021$VISIB)
dat2021$WDSP <- ifelse(dat2021$WDSP == 999.9, NA, dat2021$WDSP)
dat2021$MXSPD <- ifelse(dat2021$MXSPD == 999.9, NA, dat2021$MXSPD)
dat2021$GUST <- ifelse(dat2021$GUST == 999.9, NA, dat2021$GUST)
dat2021$MAX <- ifelse(dat2021$MAX == 9999.9, NA, dat2021$MAX)
dat2021$MIN <- ifelse(dat2021$MIN == 9999.9, NA, dat2021$MIN)
dat2021$PRCP <- ifelse(dat2021$PRCP == 99.99, 0, dat2021$PRCP)
dat2021$SNDP <- ifelse(dat2021$SNDP == 999.9, 0, dat2021$SNDP)

# fix missing elevations if repair_elevations is available
if (file.exists('repair_elevation.rds')) {
  repair_elevation <- readr::read_rds('repair_elevation.rds')
  dat2021 <- dplyr::left_join(dat2021, repair_elevation, by = 'STATION', suffix = c('', '.y')) |>
    dplyr::mutate(ELEVATION=dplyr::coalesce(ELEVATION, ELEVATION.y)) |> 
    dplyr::select(-ELEVATION.y)
}
# use km for elevation for numerical reasons
dat2021$ELEVATION <- dat2021$ELEVATION/1000

# remove stations with unknown location
dat2021 <- dat2021 |> 
  dplyr::filter(!is.na(LATITUDE))
  
# metricize
dat2021 <- dat2021 |> 
  dplyr::mutate(dplyr::across(c('TEMP', 'DEWP', 'MAX', 'MIN'), ~ (.x -32)/9*5)) |> 
  dplyr::mutate(VISIB=VISIB*1.60934, PRCP=PRCP*25.4, SNDP=SNDP*25.4,
                WDSP=WDSP*0.5144444, MXSPD=MXSPD*0.5144444, GUST=GUST*0.5144444)

# stations
stations2021 <- unique(dat2021[,c('STATION', 'LATITUDE', 'LONGITUDE', 'ELEVATION', 'NAME')])
