# Simulation
# Based on Robert & Casella (2010) Introducing Monte Carlo Methods with R  

# Accept-Reject -method
#   - f is a target distribution, g is a candicate distribution
#   - f and g have compatible support areas, g(x) > 0 when f(x) > 0
#   - there is a constant M with f(x)/g(x) <= M for all x

library(unikn)

accept.reject <- function(c.distribution, Nsim, a, b, M){
  
  if(c.distribution == "unif"){
    all.sim <- c()
    accept <- c()
    all.u <- c()
    
    for(i in 1:Nsim){
      u <- runif(1)
      g <- runif(1,a,b)
      all.sim <- c(all.sim, g)
      all.u <- c(all.u, u)
      
      if(u <= dnorm(g)/(dunif(g,a,b)*M))
        accept <- c(accept, g)  
    }
    
  return(list(a=a, b=b, M=M, all.sim = all.sim, accept = accept, all.u=all.u))
  }
  else if(c.distribution == "norm"){
    all.sim <- c()
    accept <- c()
    all.u <- c()
    
    for(i in 1:Nsim){
      u <- runif(1)
      g <- rnorm(1,a,b)
      all.sim <- c(all.sim, g)
      all.u <- c(all.u, u)
      
      if(u <= dnorm(g)/(dnorm(g,a,b)*M))
        accept <- c(accept, g)  
    }
    
    return(list(a=a, b=b, M=M, all.sim = all.sim, accept = accept, all.u=all.u))
  }
  
}
plot_c.unif <- function(sim){
  cols = c(usecol(pal_peach)[1], usecol(pal_bordeaux)[3])
  
  plot(sim[["all.u"]]*dunif(sim[["all.sim"]], sim[["a"]], sim[["b"]]) ~ sim[["all.sim"]],
       col=ifelse(sim[["all.sim"]] %in% sim[["accept"]], cols[2], cols[1]),
       xlab="x", ylab="ug(x)", main="", pch=1)
  
  legend("topright", legend=c(paste0("Nsim = ", Nsim),
                              paste0("Accepted (",round(length(sim[["accept"]])/length(sim[["all.u"]]),3)*100,"%)"), 
                              "Rejected", "f ~ N(0,1)",
                              paste0("g ~ U(", sim[["a"]],",", sim[["b"]],")"),
                              paste0("M = ", sim[["M"]])
                             ),
         col=c("white", cols[2], cols[1], c(rep("white",3))), pch=16, horiz = F, bty="n",
         inset=c(-0.55, 0))
}
plot_c.norm <- function(sim){
  cols = usecol(pal_petrol)[c(1,5)]
  
  plot(sim[["all.u"]]*dnorm(sim[["all.sim"]], sim[["a"]], sim[["b"]]) ~ sim[["all.sim"]],
       col=ifelse(sim[["all.sim"]] %in% sim[["accept"]], cols[2], cols[1]),
       xlab="x", ylab="ug(x)", main="", pch=1)
  
  legend("topright", legend=c(paste0("Nsim = ", Nsim),
                              paste0("Accepted (",round(length(sim[["accept"]])/length(sim[["all.u"]]),3)*100,"%)"), 
                              "Rejected", "f ~ N(0,1)",
                              paste0("g ~ N(", sim[["a"]],",", sim[["b"]]^2,")"),
                              paste0("M = ", sim[["M"]])
                              ),
        col=c("white", cols[2], cols[1], c(rep("white",3))), pch=16, horiz = F, bty="n",
        inset=c(-0.55, 0))
}

Nsim = 10000

# Candidate uniform distribution
sim1.u <- accept.reject("unif", Nsim, -4, 4, 6)   # f ~ N(0,1), g ~ uni(-4,4), M = 6
sim2.u <- accept.reject("unif", Nsim, -4, 4, 3.2) # f ~ N(0,1), g ~ uni(-4,4), M = 3.2
sim3.u <- accept.reject("unif", Nsim, -8, 8, 6.4) # f ~ N(0,1), g ~ uni(-8,8), M = 6.4
sim4.u <- accept.reject("unif", Nsim, -8, 8, 20)  # f ~ N(0,1), g ~ uni(-8,8), M = 20

# Plots  
par(mfrow=c(2,2))
par(mar=c(5.1, 4.1, 4.1, 8), xpd=T)
plot_c.unif(sim1.u)
plot_c.unif(sim2.u)
plot_c.unif(sim3.u)
plot_c.unif(sim4.u)

# Candidate normal distribution
sim1.n <- accept.reject("norm", Nsim, 0, 3, 7)    # f ~ N(0,1), g ~ N(0, 9), M = 7
sim2.n <- accept.reject("norm", Nsim, 0, 3, 3)    # f ~ N(0,1), g ~ N(0, 9), M = 3
sim3.n <- accept.reject("norm", Nsim, -2, 2, 15)  # f ~ N(0,1), g ~ N(-2, 4), M = 7
sim4.n <- accept.reject("norm", Nsim, -2, 2, 4.5)   # f ~ N(0,1), g ~ N(-2, 4), M = 3


par(mfrow=c(2,2))
plot_c.norm(sim1.n)
plot_c.norm(sim2.n)
plot_c.norm(sim3.n)
plot_c.norm(sim4.n)
