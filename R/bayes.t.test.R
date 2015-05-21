#' @export bayes.t.test
bayes.t.test = function(x, ...){
  UseMethod("bayes.t.test")
}

#' @export bayes.t.test.default
bayes.t.test.default = function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = 0.95, prior = c("jeffreys", "joint.conj"), 
       m = NULL, n0 = NULL, sig.med = NULL, kappa = 1, ...){
  
  prior = match.arg(prior)
  
  if(prior == "joint.conj" & (is.null(m) | is.null(n0) | is.null(sig.med) | kappa < 1)){
    m1 = "If you are using the joint conjugate prior, you need so specify:"
    m2 = "the prior mean(s), the prior precision(s), the prior median standard deviation," 
    m3 = "and the degrees of freedom associated with the prior for the standard deviation"
    stop(paste(m1, m2, m3, sep = "\n"))
  }
    
  ## Shamelessly copied from t.test.default
  
  alternative = match.arg(alternative)
  
  if (!missing(mu) && (length(mu) != 1 || is.na(mu))) 
    stop("'mu' must be a single number")
  
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) || 
                                 conf.level < 0 || conf.level > 1)) 
    stop("'conf.level' must be a single number between 0 and 1")
  
  if (!is.null(y)) {
    dname = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    if (paired) 
      xok = yok = complete.cases(x, y)
    else {
      yok = !is.na(y)
      xok = !is.na(x)
    }
    y = y[yok]
  } else {
    dname = deparse(substitute(x))
    if (paired) 
      stop("'y' is missing for paired test")
    xok = !is.na(x)
    yok = NULL
  }
  
  x = x[xok]
  
  if (paired) {
    x = x - y
    y = NULL
  }
  
  nx = length(x)
  mx = mean(x)
  SSx = sum((x-mx)^2)
  vx = var(x)
  estimate = 0
  
  if (is.null(y)) { ## one sample or paired
    if (nx < 2) 
      stop("not enough 'x' observations")
    
    stderr = sqrt(vx/nx)
    if (stderr < 10 * .Machine$double.eps * abs(mx)) 
      stop("data are essentially constant")
    
    name = if(!paired) 'mu' else 'mu[d]'
    
    if(prior == "jeffreys"){
      S1 = SSx
      kappa1 = nx - 1
      npost = nx
      
      mpost = mx
      se.post = sqrt(S1 / kappa1 / nx)
      df = kappa1
      
      param.x = seq(mx - 4 * sqrt(vx), mx + 4 * sqrt(vx), length = 200)
      prior = 1 / diff(range(x))
      likelihood = dnorm(mx, param.x, se.post)
      std.x = (param.x - mpost) / se.post
      posterior = dt(std.x, df = df)
    }else{
      S0 = qchisq(0.5, kappa) * sig.med^2
      S1 = SSx + S0
      kappa1 = nx + kappa
      npost = n0 + nx
      sigma.sq.B = (S1 + (n0 * nx / kappa1) * (mx - m)^2)/npost
      
      mpost = (nx * mx + n0 * m) / npost
      se.post = sqrt(sigma.sq.B / kappa1)
      df = kappa1
      
      lb = min(mpost - 4 * se.post, m - 4 * sqrt(1 / n0))
      ub = max(mpost + 4 * se.post, m + 4 * sqrt(1 / n0))
      param.x = seq(lb, ub, length = 200)
      prior = dnorm(param.x, m, sqrt(1 / n0))
      likelihood = dnorm(mx, param.x, se.post)
      std.x = (param.x - mpost) / se.post
      posterior = dt(std.x, df = df)
    }
    
    method = if (paired) 
      "Paired t-test"
    else 
      "One Sample t-test"
    
  } else { ## two sample
    ny = length(y)
    if (nx < 1 || (!var.equal && nx < 2)) 
      stop("not enough 'x' observations")
    
    if (ny < 1 || (!var.equal && ny < 2)) 
      stop("not enough 'y' observations")
    
    if (var.equal && nx + ny < 3) 
      stop("not enough observations")
    
    my = mean(y)
    vy = var(y)
    stderr = sqrt((sum((x - mx)^2) + sum((y - my)^2) / (nx + ny - 2)) * (1 / nx + 1 / ny))
    
    if (stderr < 10 * .Machine$double.eps * max(abs(mx), abs(my))) 
      stop("data are essentially constant")
    
    SSp = sum((x - mx)^2) + sum((y - my)^2)
    
    method = paste(if (!var.equal) 
      "Welch", "Two Sample t-test")
    
    estimate = c(mx, my)
    names(estimate) = c("mean of x", "mean of y")
    
    lb = mx - my - 4 * sqrt(vx/nx + vy/ny)
    ub = mx - my + 4 * sqrt(vx/nx + vy/ny)
    param.x = seq(lb, ub, length = 1000)
    name = 'mu[1]-mu[2]'
    
    if (var.equal) {
      if(prior == "jeffreys"){
        kappa1 = nx + ny - 2
        sigma.sq.Pooled = SSp / kappa1 
        
        mpost = mx - my
        se.post = sqrt(sigma.sq.Pooled * (1/nx + 1/ny))
        df = kappa1
        
        prior = 1 / diff(range(param.x))
        likelihood = dnorm(mx - my, param.x, se.post)
        posterior = dt((param.x - mpost)/se.post, df)
      }else{
        kappa1 = kappa + nx + ny
        n1post = nx + n0[1]
        n2post = ny + n0[2]
        S = qchisq(0.5, 1) * sig.med^2
        S1 = S + SSp
        m1post = (nx * mx + n0[1] * m[1]) / n1post
        m2post = (ny * my + n0[2] * m[2]) / n2post
        sigma.sq.B = S1 / kappa1
        mpost = m1post - m2post
        se.post = sqrt(sigma.sq.B * (1/n1post + 1/n2post))
        df = kappa1
        
        prior = dnorm(param.x, m[1] - m[2], sqrt(sum(1/n0)))
        likelihood = dnorm(mx - my, param.x, se.post)
        posterior = dt((param.x - mpost)/se.post, df)
      }
    }else {
     
    }
  }
  
  result = list(name = name, param.x = param.x, 
                prior = prior, likelihood = likelihood, posterior = posterior,
                mean = mpost,
                var = se.post^2,
                cdf = function(x)dt((x - mpost) / se.post, df = df))
  class(result) = 'Bolstad'
  
  tstat = (mpost - mu) / se.post
  estimate = mpost
  
  if (alternative == "less") {
    pval = pt(tstat, df)
    cint = c(-Inf, tstat + qt(conf.level, df))
  }
  else if (alternative == "greater") {
    pval = pt(tstat, df, lower.tail = FALSE)
    cint = c(tstat - qt(conf.level, df), Inf)
  }
  else {
    pval = 2 * pt(-abs(tstat), df)
    alpha = 1 - conf.level
    cint = qt(1 - alpha/2, df)
    cint = tstat + c(-cint, cint)
  }
  cint = mu + cint * se.post
  names(tstat) = "t"
  names(df) = "df"
  #names(mu) = if (paired || !is.null(y)) 
  #  "difference in means"
  #else "mean"
  #attr(cint, "conf.level") = conf.level
  rval = list(statistic = tstat, parameter = df, p.value = pval, 
               conf.int = cint, estimate = estimate, null.value = mu, 
               alternative = alternative, method = method, data.name = dname,
              result = result)
  class(rval) = "htest"
  return(rval)
}

## S3 method for class 'formula'
#' @export bayes.t.test.formula
bayes.t.test.formula = function(formula, data, subset, na.action, ...){
  ## shamelessly hacked from t.test.formula
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]), 
                                                                  "term.labels")) != 1L)) 
    stop("'formula' missing or incorrect")
  m = match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data = as.data.frame(data)
  m[[1L]] = quote(stats::model.frame)
  m$... = NULL
  mf = eval(m, parent.frame())
  DNAME = paste(names(mf), collapse = " by ")
  names(mf) = NULL
  response = attr(attr(mf, "terms"), "response")
  g = factor(mf[[-response]])
  if (nlevels(g) != 2L) 
    stop("grouping factor must have exactly 2 levels")
  DATA = setNames(split(mf[[response]], g), c("x", "y"))
  y = do.call("bayes.t.test", c(DATA, list(...)))
  y$data.name = DNAME
  if (length(y$estimate) == 2L) 
    names(y$estimate) = paste("mean in group", levels(g))
  y
}