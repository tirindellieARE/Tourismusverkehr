suppressPackageStartupMessages(library(data.table))
ll0 <- -5703.782; N <- 1000
models <- list(baseline=list(k=12,ll=-5089.800), minimal=list(k=27,ll=-4826.972), full=list(k=48,ll=-4509.233))
for (nm in names(models)) {
  m <- models[[nm]]
  cat(nm, "k=",m$k,"LL=",round(m$ll,3),"rho2=",round(1-m$ll/ll0,4),
      "AIC=",round(-2*m$ll+2*m$k,1),"BIC=",round(-2*m$ll+m$k*log(N),1),"\n")
}
cat("\nLRT baseline->minimal: chi2=",round(2*(5089.800-4826.972),2),"df=15\n")
cat("LRT minimal->full:     chi2=",round(2*(4826.972-4509.233),2),"df=21\n")
cat("LRT baseline->full:    chi2=",round(2*(5089.800-4509.233),2),"df=36\n")
