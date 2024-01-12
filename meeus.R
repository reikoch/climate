# helpful functions according to Meeus
J2000_cent <- function(dt) {
  (as.numeric(dt)-946728000.0)/3155760000.0
}

epsilon <- function(J2000T) {
  (((0.001813/3600.0)*J2000T - (0.00059/3600.0))*J2000T - (46.8150/3600.0))*J2000T +
  (23+26/60.0+21.448/3600.0)
}
