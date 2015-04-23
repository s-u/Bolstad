print.sscsamp = function(x, ...){
  cat("Sample   Mean    Stratum 1  Stratum 2  Stratum 3\n", ...)
  cat("------  -------  ---------  ---------  ---------\n", ...)

  n.samples = length(x$means)
  fmt = '%6d  %7.4f  %9d  %9d  %9d\n'
  
  for (r in 1:n.samples) {
    s = sprintf(fmt, r, round(x$means[r], 4), x$s.strata[r,1],
                x$s.strata[r, 2], x$s.strata[r, 3])
    cat(s, ...)
  }
}