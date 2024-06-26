# helpful functions according to Meeus

# for dates noon is assumed
# generic function to convert to Julian century in J2000 frame
J2000T <- function(t) UseMethod("J2000T")
J2000T.Date <- function(date) (as.numeric(date)-10957.5)/36525
J2000T.POSIXct <- function(dt) (as.numeric(dt)-946728000.0)/3155760000.0
J2000T.default <- function(x) stop('Only objects of class "Date" or "POSIXct" can be converted. ',
                                   'Here it was a ', class(x))

# generic function to convert to Julian day
JD <- function(t) UseMethod("JD")
JD.POSIXct <- function(dt) as.numeric(dt)/86400 + 2440587.5
JD.Date <- function(date) as.numeric(date)+2440588
JD.default <- function(x) stop('Only objects of class "Date" or "POSIXct" can be converted. ',
                                   'Here it was a ', class(x))

# mean obliquity of the ecliptic
epsilon <- function(J2000T) {
  (pi/180)*(((5.03611111111e-07*J2000T - 1.63888888889e-07)*J2000T - 0.0130041666667)*J2000T + 23.4392911111)
}

# mean anomaly of the sun
M <- function(J2000T) (pi/180) * (J2000T*(J2000T*(J2000T*-0.00000048 - 0.0001559) + 35999.05030) + 357.52910)

# eccentricity of the Earth's orbit
ecc <- function(J2000T) J2000T*(J2000T* -0.0000001236 - 0.000042037) + 0.016708617

# sun's equation of center
C <- function(J2000T) {
  M <- M(J2000T)
  (pi/180) * ((J2000T*(J2000T* -0.000014 - 0.004817) + 1.914600) * sin(M) +
  (0.019993 - 0.000101*J2000T)*sin(2*M) + 0.000290*sin(3*M))
}
# sun - earth distance
rho <- function(J2000T) {
  ecc <- ecc(J2000T)
  1.000001018*(1-ecc*ecc) / (1 + ecc*cos(C(J2000T) + M(J2000T)))
}

# mean longitude of sun
L0 <- function(J2000T) (pi/180) * (280.46645 + J2000T*(36000.76983 + J2000T*0.0003032))

# longitude of ascending node of moon
omega <- function(J2000T) (pi/180) * (125.04452 + J2000T*(-1934.136261 + J2000T*(0.0020708 + J2000T/450000)))

# apparent longitude of sun
lambda <- function(J2000T) L0(J2000T) + C(J2000T) - 0.00569 - 0.00478*sin(omega(J2000T))

# sun declination
declination <- function(J2000T) asin(sin(epsilon(J2000T)) * sin(lambda(J2000T)))


# sunrise hour angle
h0 <- function(J2000T, latitude) {
  x <- tan(declination(J2000T))*tan(latitude)
  ifelse (x< -1, 0, ifelse(x>1, pi, acos(-x)))
}

# irradiation in kWh per m^2 per day
insolation <- function(J2000T, latitude) {
  rho <- rho(J2000T)
  h0 <- h0(J2000T, latitude)
  x <- sin(epsilon(J2000T)) * sin(lambda(J2000T))
  10.4033856721 * rho*rho * (h0*sin(latitude)*x + cos(latitude)*cos(asin(x))*sin(h0))
}
