#' Measurement invariance test
#'
#' This packages compares the groups based on the measurement model
#'
#'
#'
#'
#'
#'
#'
#' @examples
#'
#' @export
mint <- function(meas.model, group, data, fitmeas = FALSE){

 library(lavaan)

 #equal form
 eqform <- sem(meas.model, bfi, group = "gender")
 eqform.fit <- fitMeasures(eqform, c("chisq", "df", "pvalue", "aic", "bic"))
 eqform.fit2 <- fitMeasures(eqform, c("chisq", "df", "pvalue", "rmsea", "cfi", "tli"))

 #equal loadings
 eqload <- sem(meas.model, bfi, group = "gender", group.equal = c("loadings"))
 eqload.fit <- fitMeasures(eqload, c("chisq", "df", "pvalue", "rmsea", "cfi", "tli"))
 #equal intercepts
 eqint <- sem(meas.model, bfi, group = "gender", group.equal = c("loadings", "intercepts"))
 eqint.fit <- fitMeasures(eqint, c("chisq", "df", "pvalue", "rmsea", "cfi", "tli"))
 #equal error variance
 eqerrvar <- sem(meas.model, bfi, group = "gender", group.equal = c("loadings", "intercepts", "residuals"))
 eqerrvar.fit <- fitMeasures(eqerrvar, c("chisq", "df", "pvalue", "rmsea", "cfi", "tli"))
 #equal factor means
 eqfmeans <- sem(meas.model, bfi, group = "gender", group.equal = c("loadings", "intercepts", "residuals", "means"))
 eqfmeans.fit <- fitMeasures(eqfmeans, c("chisq", "df", "pvalue", "rmsea", "cfi", "tli"))

 #comparisons
 # 1
 eqform.fit
 # 2 vs 1
 eqloadVSeqform <- anova(eqload, eqform)
 eqloadVSeqform
 # 3 vs 2
 eqintVSeqload <- anova(eqint, eqload)
 eqintVSeqload
 # 4 vs 3
 eqerrvarVSeqint <- anova(eqerrvar, eqint)
 eqerrvarVSeqint
 # 5 vs 4
 eqfmeansVSeqerrvar <- anova(eqfmeans, eqerrvar)
 eqfmeansVSeqerrvar
 out <- list(paste("Tests of measurement invariance and factor mean difference"),
             "Equal.Form?" = eqform.fit,
             "Equal.Loadings?" = eqloadVSeqform,
             "Equal.Intercepts?" = eqintVSeqload,
             "Equal.Error.Variances?" = eqerrvarVSeqint,
             "Equal.Factor.Means?" = eqfmeansVSeqerrvar)

 if(fitmeas == TRUE){
  return(list(out,
              paste("Fit Indices for all the models estimated:"),
              Equal.form = eqform.fit2,
              Equal.loadings = eqload.fit,
              Equal.Intercepts = eqint.fit,
              Equal.Error.Variances = eqerrvar.fit,
              Equal.Factor.Means = eqfmeans.fit))
 }
 else {
  return(out)
 }
}




