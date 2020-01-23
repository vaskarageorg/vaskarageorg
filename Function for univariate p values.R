#Generate some data----
x1 = 1:2000
x2 = matrix(rnorm(length(x1) * 1000), nrow = length(x1), ncol = 1000)
x3 = seq(0,2,length.out = length(x1))

#One Significant
x1001 = x3*0.0002 + rnorm(length(x3),0,0.01)
x1and1 = cbind(x2, x1001)

#Extract p-values from univariate approach function----
linearmod = function(x.indep){
  mod1 = lm(x3 ~ x.indep)
  summa = summary(mod1)
  return(summa$coefficients[2,4])
}

#Bonferroni correction----
pvalues = apply(x1and1,2,linearmod)

pval.log = -log(pvalues)/log(10)
plot(pval.log,ylim = c(0,15))

bonferroni.cutoff = 0.05/ncol(x2)
plot.bonf = -log(bonferroni.cutoff)

abline(h=plot.bonf, col = 'red')

#Hochberg----
#bonferoni etcx.




