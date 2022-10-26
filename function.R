###############################
skew = function(x){
  n=length(x)
  meanx = mean(x)
  sdx = sd(x)
  u = sum((x-meanx)^3)/n
  g1 = (n^2*u)/((n-1)*(n-2)*sdx^3)
  return(g1)
}
####################################
kurt = function(x){
  sdx = sd(x)
  n = length(x)
  uk4 = sum((x-mean(x))^4)/n
  g2 = (n^2*(n+1)*uk4)/((n-1)*(n-2)*(n-3)*sdx^4)-((3*(n-1)^2)/((n-2)*(n-3)))
  return(g2)
}
#######################################
se = function(x){
  error = sd(x)/(length(x)-1)
  return(error)
}
#####################################
