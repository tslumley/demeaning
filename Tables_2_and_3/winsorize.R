# winsorized mean of a vector
winsorize <- function(x, alpha = 0.05, na.rm = FALSE) {
  if (length(alpha) != 1) stop("Alpha must be specified.")
  if (alpha < 0 || alpha > 0.5) stop("Alpha must be between 0 and 0.5.")
  
  # ## get the limits
  # k <- floor((length(x) + 1)*alpha)
  # 
  # ## order of x
  # x.ord <- order(x)
  # limits <- c(x[x.ord][k], x[x.ord][length(x) - k])
  xq <- quantile(x, probs = c(alpha, 1 - alpha), na.rm = na.rm)
  limits <- c(xq[1], xq[2])
  
  ## set equal to the limit if outside
  x[x > limits[2]] <- limits[2]
  x[x < limits[1]] <- limits[1]
  
  ## return the winsorized mean
  return(mean(x, na.rm = na.rm))
}
