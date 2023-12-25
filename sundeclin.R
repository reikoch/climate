# compute sun declination according to https://en.wikipedia.org/wiki/Position_of_the_Sun#Calculations

sundeclin <- function(N) -asin(0.3977885*cos(0.01720289*(N+10) + 2*0.0167*sin(0.01720289*(N-2))))

dat <- dat |> 
  dplyr::mutate(maxsun=sin(LATITUDE/180*pi)*sin(sundeclin(lubridate::yday(DATE))) +
                              cos(LATITUDE/180*pi)*cos(sundeclin(lubridate::yday(DATE))))
