pois.prob <- function(x, lambda, type= "=="){
  if(type == "=="){
    return(dpois(x, lambda))
  }else if(type == "!="){
    return(1-dpois(x, lambda))
  }else if(type == "<"){
    return(ppois(x-1, lambda))
  }else if(type == "<="){
    return(ppois(x, lambda))
  }else if(type == ">"){
    return(1-ppois(x, lambda))
  }else if(type == ">="){
    return(1-ppois(x-1, lambda))
  }
}


beta.prob <- function(x, alpha, beta, type= "=="){
  if(type == "=="){
    return(0)
  }else if(type == "!="){
    return(1)
  }else if(type == "<"){
    return(pbeta(x, alpha, beta))
  }else if(type == "<="){
    return(pbeta(x, alpha, beta))
  }else if(type == ">"){
    return(1-pbeta(x, alpha, beta))
  }else if(type == ">="){
    return(1-pbeta(x, alpha, beta))
  }
}
