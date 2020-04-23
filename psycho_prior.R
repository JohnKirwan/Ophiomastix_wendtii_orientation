
prior_predict <- function(chance_rate,robustness,nsamples){
  
  
  base       <- rnorm(nsamples,chance_rate,0.05)  # Base is close to random probability 
  
  
  # robustness of the response - good (~ 99%), fair (~ 90%), poor (~ 75%), v. poor (~ 50%)
  
  
  lapse      <- rbeta(nsamples,3,5)       # Lapse is wide range - should not overlap base rate
  lapse      <- runif(nsamples,0,.65)
  
  
  # Prior for the threshold based on the max plausible value (I_max) relative to stimulus maximum 
  # Could use rgamma(2,I_max)
  
  threshold  <- rnorm(nsamples,-.5,0.5)    # Threshold must be > 0 and can be > 1.
  width      <- rnorm(nsamples,-.5,0.5)    # Width must be > 0 and can be > 1.
  

  plot( NULL, xlim = c(-.5,2), ylim = c(0,1) , 
        xlab = "Scaled stimulus strength", ylab = "Proportion correct response" )
  abline (h=1/6, lty=2)
  for ( i  in 1:N ) curve (
    base[i] + 
      (1 - base[i] - lapse[i]) * 
      plogis(0++4.39*(	x - exp(threshold[i])	)	/ ( exp (width[i]) ) ),
    from=-.5, to = 2, add = TRUE, col = col.alpha("black",0.1))
}

