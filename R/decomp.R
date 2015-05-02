decomp = function(x, ...){
  if(class(x) != "Bolstad")
    stop("This function only works for objects of class Bolstad")
  
  oPar = par(mfrow = c(3, 1), mar = c(1, 1, 1, 1))
  with(x, {
    yLims = c(0, 1.1 * max(results$posterior, results$prior));
  
    plot(prior ~ param.x, ylim = yLims, type = "l", xlim = range(param.x),
         xlab = "", ylab = "", main = "",
         axes = FALSE, ...);
    polygon(param.x, prior, col = "red");
    box();
    r = legend("topleft", legend = "Prior",lty = 1, bty = "n", plot = FALSE)$text;
    text(r$x, r$y, "Prior", adj = 0);
    
    plot(likelihood ~ param.x, type="l",
         xlab = "", ylab = "", main = "",
         axes = FALSE, ...);
    polygon(param.x, likelihood, col = "green");
    box();
    r = legend("topleft", legend = "Prior",lty = 1, bty = "n", plot = FALSE)$text;
    text(r$x, r$y, "Likelihood", adj = 0);
    
    plot(posterior ~ param.x, ylim = yLims, type = "l",
         xlab = "", ylab = "", main = "",
         axes = F, ...);
    polygon(param.x, posterior, col = "blue");
    box();
    r = legend("topleft", legend = "Prior",lty = 1, bty = "n", plot = FALSE)$text;
    text(r$x, r$y, "Posterior", adj = 0);
  })
  
  par(oPar)
}