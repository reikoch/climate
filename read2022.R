# retrieve global weather data 2022

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
