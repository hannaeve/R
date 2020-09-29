library(MASS)
library(ggplot2)
library(unikn)
library(Hmisc)

#seecol("all")
my_pal = usecol(pal_unikn_pref)

data_iris = iris
str(iris)

attach(data_iris)

iris.species <- unique(Species)
iris.var <- colnames(data_iris)[1:4]; iris.var

# Lajikohtaiset muuttujien keskiarvot
for(specie in iris.species){
  print(specie)
  print(apply(data_iris[Species == specie,1:4], 2, mean))
}

#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# General things:
cols = usecol(my_pal)[c(2,5,7)]

#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------

par(mfrow=c(2,2))
for(var in iris.var){
  boxplot(data_iris[, var]~Species, horizontal = T, col=cols,
          main=gsub("\\.", " ", var), xlab="")
}


#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------

par(mfrow=c(2,1))

plot(Petal.Width~Petal.Length, col=cols[Species], pch=19, ylab=gsub("\\.", " ", iris.var[4]),
     xlab=gsub("\\.", " ", iris.var[3]))
legend(x="topleft", legend=capitalize(as.character(unique(Species))), col=cols, border = F, pch=19, bty="n",
       ncol=1)

plot(Sepal.Width~Sepal.Length, col=cols[Species], pch=19, ylab=gsub("\\.", " ", iris.var[2]),
     xlab=gsub("\\.", " ", iris.var[1]))
legend(x="topleft", legend=capitalize(as.character(unique(Species))), col=cols, border = F, pch=19, bty="n",
       ncol=1)

#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------


#detach(data_iris)
