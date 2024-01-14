# helpful functions according to Meeus
J2000_cent <- function(dt) {
  (as.numeric(dt)-946728000.0)/3155760000.0
}

# mean obliquity of the ecliptic
epsilon <- function(J2000T) {
  ((5.03611111111e-07*J2000T - 1.63888888889e-07)*J2000T - 0.0130041666667)*J2000T + 23.4392911111
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
