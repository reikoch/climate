# read temp data of selected years

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

# astronomical parameters, 
# source: https://media4.obspm.fr/public/ressources_lu/pages_mouvement-terre-lune/stlp-elements-orbitaux.html
ecc <- 0.0167086342
tropyear <-  365.2421904 
invdist <- function(N) 1+ecc*cos(2*pi*N/tropyear)
distsun <- function(N) 1/(1+ecc*cos(2*pi*N/tropyear))
sundeclin <- function(N) -0.3977885*cos(0.01720289*(N+10) + 0.0334*sin(0.01720289*(N-2)))

## prepare sun angle parameters
sunrising <- function(N) 0.3977885*sin(0.01720289*(N+10) + 0.0334*sin(0.01720289*(N-2))) * 
  0.01720289*(1+0.0334*cos(0.01720289*(N-2)))
solation <- function(N, latitude) {
  delta <- sundeclin(N)
  H0 <- acos(tan(latitude)*tan(delta))
  invdist(N)*invdist(N) * (H0*sin(latitude)*sin(delta) + cos(latitude)*cos(delta)*sin(H0)) 
}

dat1 <- dat |> 
  dplyr::mutate(maxsun=sin(LATITUDE/180*pi)*sundeclin(lubridate::yday(DATE)) +
                  cos(LATITUDE/180*pi)*sqrt(1-sundeclin(lubridate::yday(DATE))^2),
                sunspeed=ifelse(DATE==lag(DATE)+1, maxsun-lag(maxsun), NA))

## prepare stations and its characteristics
stations <- dat1[,c('STATION', 'LATITUDE', 'LONGITUDE', 'ELEVATION', 'NAME')] |> 
  dplyr::group_by(STATION) |> 
  dplyr::slice_tail(n=1)
dat1 <- dat1 |>
  dplyr::group_by(STATION) |>
  dplyr::mutate(delta=ifelse(DATE==lag(DATE)+1, TEMP-lag(TEMP), NA))
volatility <- dat1 |>
  dplyr::group_by(STATION) |>
  dplyr::summarise(volatil=mean(abs(delta), na.rm = TRUE))
nrecords <- dat1 |>
  dplyr::group_by(STATION) |> tally()
stations <- stations |> 
  dplyr::left_join(volatility) |> 
  dplyr::left_join(nrecords)

# model
stat1000 <- subset(stations, n>1400)
dat1000 <- subset(dat1, STATION %in% stat1000$STATION & !is.na(sunspeed))

(yy2 <- lme4::lmer(TEMP ~ ELEVATION + maxsun + sunspeed + (maxsun + sunspeed | STATION), data=dat1000 ))
dat1000$res2 <- residuals(yy2)
summary(yy2)
mixedup::summarise_model(yy2)
stat1000 <- stat1000 |> 
  dplyr::bind_cols(lme4::ranef(yy2)$STATION) |> 
  dplyr::rename(intercept=`(Intercept)`)

ggplot(data=subset(dat1000, STATION %in% sample(stat1000$STATION, 5)),
       aes(x=DATE, y=res2, group=STATION, color=STATION)) + geom_smooth()

(yy3 <- lme4::lmer(TEMP ~ ELEVATION + maxsun + sunspeed + (sunspeed | STATION), data=dat1000 ))
dat1000$res3 <- residuals(yy3)
summary(yy3)
stat1000 <- stat1000 |> 
  dplyr::bind_cols(structure(lme4::ranef(yy3)$STATION, names=c('intercept3', 'sunspeed3')))

(yy4 <- lme4::lmer(TEMP ~ ELEVATION + maxsun + sunspeed + (0+sunspeed | STATION), data=dat1000 ))
dat1000$res4 <- residuals(yy4)
summary(yy4)
stat1000 <- stat1000 |> 
  dplyr::bind_cols(structure(lme4::ranef(yy4)$STATION, names=c('sunspeed4')))

(yy5 <- lme4::lmer(TEMP ~ ELEVATION + maxsun + lagT + (0+lagT | STATION), data=dat1000 ))
dat1000$res5 <- residuals(yy5)
summary(yy5)
stat1000 <- stat1000 |> 
  dplyr::bind_cols(structure(lme4::ranef(yy5)$STATION, names=c('lagT5')))
