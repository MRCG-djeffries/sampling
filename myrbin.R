myrbin=function (prop = 0.5, prvar = 0, noc, csize, csvar = 0, rho) 
{
  cluster <- c()
  x <- c()
  for (i in 1:noc) {
    min_csize <- ifelse((csize - round(csize * csvar)) >= 
                          2, csize - round(csize * csvar), 2)
    csizen <- abs(round(csize + (csize * csvar) * rnorm(1)))
    while (csizen < min_csize) {
      csizen <- abs(round(csize + (csize * csvar) * rnorm(1)))
    }
    min_prop <- ifelse((prop - prop * prvar) >= 0, prop - 
                         prop * prvar, 0)
    max_prop <- ifelse((prop + prop * prvar) <= 1, prop + 
                         prop * prvar, 1)
    propn <- abs(prop + (prop * prvar) * rnorm(1))
    while (propn < min_prop | propn > max_prop) {
      propn <- abs(prop + (prop * prvar) * rnorm(1))
    }
    ri <- sqrt(rho)
    zi <- rbinom(n = 1, size = 1, prob = propn)
    for (j in 1:csizen) {
      yij <- rbinom(n = 1, size = 1, prob = propn)
      uij <- rbinom(n = 1, size = 1, prob = ri)
      xij <- (1 - uij) * yij + uij * zi
      cluster <- c(cluster, i)
      x <- c(x, xij)
    }
  }
  cbcdata <- data.frame(cid = as.factor(cluster), y = x)
  return(cbcdata)
}