# compute sun declination according to https://en.wikipedia.org/wiki/Position_of_the_Sun#Calculations

# sundecl <- -asin(0.39779*cos(0.98565*(N+10) + 1.914*sin(0.98565*(N-2))))
# sundecl <- function(N) -asin(0.39779*cos(0.01720284*(N+10) + 1.914*sin(0.01720284*(N-2))))
# s2 <- function(N) -0.4091052*cos(2*pi/365.25 * (N+10))

sundeclin <- function(N) -asin(0.3977885*cos(0.01720289*(N+10) + 2*0.0167*sin(0.01720289*(N-2))))
