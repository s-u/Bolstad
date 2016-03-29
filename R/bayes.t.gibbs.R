bayes.t.gibbs = function(x, y, nIter = 10000, nBurn = 1000, sigmaPrior = c("chisq", "gamma")){
  
  sigmaPrior = match.arg(sigmaPrior)
  data = list(x, y)

  if(sigmaPrior == "chisq"){
    ybar = sapply(data, mean)
    Sy = sapply(data, sum)
    SSy = sapply(data, function(x)sum((x - mean(x))^2))
    n = sapply(data, length)
    
    ## prior means and sds
    m = m1 = ybar
    s = s1 = rep(1, 2)
    
    post.prec = prior.prec = 1 / s^2
    kappa = 1
    
    kappa1 = kappa + n
    S1 = S = s^2*qchisq(0.5, 1)
    
    sigma.sq = S/rchisq(2, 1)
    mu = rnorm(2, m, s)
    
    res = matrix(0, nr = nIter, nc = 6)
    
    for(i in 1:(nIter + nBurn)){
      for(j in 1:2){
        S1[j] = S[j] + sum((data[[j]] - mu[j])^2)
        sigma.sq[j] = S1[j] / rchisq(1, kappa1[j])
        post.prec[j] = prior.prec[j] + n[j] / sigma.sq[j]
        s1[j] = 1 / post.prec[j]
        m1[j] = m[j] * prior.prec[j] / post.prec[j] + Sy[j] * s1[j] / sigma.sq[j]
        mu[j] = rnorm(1, m1[j], sqrt(s1[j]))
      }
      if(i > nBurn){
        d = mu[1] - mu[2]
        se.diff = sqrt(sum(sigma.sq / n))
        tstat = d / se.diff
        res[i - nBurn,] = c(mu, d, sigma.sq, tstat)
      }
    }
  }else{
    ybar = sapply(data, mean)
    Sy = sapply(data, sum)
    SSy = sapply(data, function(x)sum((x - mean(x))^2))
    n = sapply(data, length)
    
    ## prior means and sds
    m = ybar
    s = rep(sqrt(2), 2)
    
    prior.prec = 1 / s^2
    alpha = c(0.001, 0.001)
    beta = c(0.001, 0.001)
    alpha1 = n / 2 + alpha
    beta1 = beta + SSy / 2
    
    sigma.sq = 1/rgamma(2, alpha1, beta1)
    mu = rnorm(2, m, s)
    
    res = matrix(0, nr = nIter, nc = 5)
    
    for(i in 1:(nIter + nBurn)){
      for(j in 1:2){
        beta1[j] = beta[j] + sum((data[,j] - mu[j])^2) / 2
        sigma.sq[j] = 1 / rgamma(1, alpha1[j], beta1[j])
        post.prec[j] = prior.prec[j] + n[j] / sigma.sq[j]
        s1[j] = 1 / post.prec[j]
        m1[j] = m[j] * prior.prec[j] / post.prec[j] + Sy[j] * s1[j] / sigma.sq[j]
        mu[j] = rnorm(1, m1[j], sqrt(s1[j]))
      }
      if(i > nBurn){
        d = mu[1] - mu[2]
        se.diff = sqrt(sum(sigma.sq / n))
        tstat = d / se.diff
        res[i - nBurn,] = c(mu, d, sigma.sq, tstat)
      }
    }
  }
  
  colnames(res) = c("mu.x","mu.y", "mu.diff", "sigma.sq.1", "sigma.sq.2", "t.stat")
  return(as.data.frame(res))
}