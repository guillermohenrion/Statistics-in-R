n<-20
pval<-seq(0.1,0.9,by=0.05)
pval
nosim<-1000
coverage <- sapply(pval, function(p) {
  phats<-(rbinom(nosim, prob=p, size=n))/(n)
  ll<-phats-qnorm(0.975)*sqrt(phats*(1-phats)/n)
  ul<-phats+qnorm(0.975)*sqrt(phats*(1-phats)/n)
  mean(ll<p&ul>p)
})


coverage2 <- sapply(pval, function(p) {
  phats<-(rbinom(nosim, prob=p, size=n)+2)/(n+4)
  ll<-phats-qnorm(0.975)*sqrt(phats*(1-phats)/n)
  ul<-phats+qnorm(0.975)*sqrt(phats*(1-phats)/n)
  mean(ll<p&ul>p)
})

plot(coverage, type='o', col='blue')
plot(coverage2, type='o', col='green')




  
