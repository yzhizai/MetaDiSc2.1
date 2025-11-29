library(plyr)
library(lmtest)
library(readxl)
library(tools)
library(DT)
library(PropCIs)
library(lme4)
library(dplyr)
library(msm)
library(pracma)
library(plotly)
library(forcats)
library(ggplot2)
library(ggsci)
library(parallel)
library(nlme)
library(arm)
library(egg)
library(mada)
library(optimx)
library(dfoptim)
library(meta) # Forest plot
library(grid)
library(gridtext)


# ----------------------------------------------------------------------------------------------- #

# Dataset column names transformation (to lower case):
dataset_colnames <- function(data){
  colnames(data) = tolower(colnames(data)) 
  if ("id" %in% data){
    data$id = make.names(data$id, unique = TRUE, allow_ = FALSE)
  }
  return(data)
}

# ----------------------------------------------------------------------------------------------- #

dataset_XYZ <- function(data) {

  X = data %>% mutate(
    n1 = data$tp + data$fn,
    n0 = data$fp + data$tn,
    true1 = data$tp,
    true0 = data$tn,
    study = 1:nrow(data),
    sens = data$tp/n1,
    spec = data$tn/n0
  )   
  
  if (0 %in% X$n1 | 0 %in% X$n0){
    X_biv <- subset(X, X$n1!= 0 & X$n0 !=0)
  } else{
    X_biv <- X}

  X_biv <-  reshape(as.data.frame(X), direction = "long", varying = list(c("n1", "n0"), c("true1", "true0")), timevar = "sens", times = c(1,0),
                    v.names = c("n", "true"))

  row.names(X_biv) <- NULL
 
  X_biv <- X_biv[order(X_biv$id),]
  X_biv$spec <-  1-X_biv$sens
  
  
  Y = X[order(X$id), ]
  Y = X %>% mutate(
    sens = X$tp/n1,
    spec = X$tn/n0
  )
  
  for (i in 1:nrow(Y)){
    tmp = exactci(Y$tp[i], Y$n1[i], 0.95)
    Y$CIinfsen[i] = round(tmp$conf.int[1],3)
    Y$CIsupsen[i] = round(tmp$conf.int[2],3)
    tmp2 = exactci(Y$tn[i], Y$n0[i], 0.95)
    Y$CIinfspe[i] = round(tmp2$conf.int[1],3)
    Y$CIsupspe[i] = round(tmp2$conf.int[2],3)}
  
  
  Z <- data %>% mutate(
    n1 = tp + fn,
    n0 = fp + tn,
    pos = tp + fp,
    neg = fn + tn,
    sens = round(tp/n1,3),
    spec = round(tn/n0,3),
    TPR = tp/n1,
    FPR = fp/n0
  )
  
  if (0 %in% Z$n1 | 0 %in% Z$n0 ){
    Z <- subset(Z, Z$n1!= 0 & Z$n0 !=0)
  } else{
    Z <- Z}
  
  return(list(X=X_biv, Y=Y, Z=Z, X_uni = X))
}

# ----------------------------------------------------------------------------------------------- #

# Dataset summary function (used to give the user a summary of the uploaded dataset):
dataset_summary <- function(data){
  
  n_studies = nrow(data) 
  n_positives = sum(data$tp)+sum(data$fn)
  n_negatives = sum(data$tn)+sum(data$fp)
  n_total = n_positives + n_negatives
  prevalence = n_positives/n_total
  
  if (prevalence <= 1/100000){
    prevalence = round(prevalence, 8)
  }
  else if (prevalence > 1/100000 & prevalence <= 1/10000){
    prevalence = round(prevalence, 5)
  }
  else if (prevalence > 1/10000 & prevalence <= 1/1000){
    prevalence = round(prevalence, 4)
  }
  else if (prevalence > 1/1000 & prevalence <= 1/100){
    prevalence = round(prevalence, 3)
  }
  else{
    prevalence = round(prevalence, 2)
  }
  
  summary = data.frame(Coefficient = c("Studies", "Number of diseased", "Number of non-diseased", "Total", "Prevalence"),
                       Estimate = c(n_studies, n_positives, n_negatives, n_total, prevalence))
  return(summary)
}

# ----------------------------------------------------------------------------------------------- #

#### FOREST PLOTS: ####


# Sensitivity:

forest_plot_sen <- function(data, covariate=NULL) {
  
  Y_forest = dataset_XYZ(data)$Y
  
  if(0 %in% Y_forest$n1){
    Y_forest <- subset(Y_forest, Y_forest$n1!= 0)
  } else {
    Y_forest <- Y_forest
  }
  
  if (!is.null(covariate)){
    if (covariate != "-") {
      sen <- metaprop(Y_forest$tp, Y_forest$n1, studlab = id, data = Y_forest,
                      byvar = Y_forest[,covariate], print.byvar = TRUE, bylab = covariate,
                      overall = FALSE,
                      overall.hetstat= FALSE, 
                      comb.fixed = FALSE, 
                      comb.random = FALSE)
    }
    else if (covariate == "-") {
      sen <- metaprop(Y_forest$tp, Y_forest$n1, studlab = id, data = Y_forest,
                      overall = FALSE,
                      overall.hetstat= FALSE, 
                      comb.fixed = FALSE, 
                      comb.random = FALSE)
    }
  }  
  else {
    sen <- metaprop(Y_forest$tp, Y_forest$n1, studlab = id, data = Y_forest,
                    overall = TRUE,
                    overall.hetstat= FALSE,
                    comb.fixed = FALSE)
  }
  
  f_sen <- forest(sen, 
                       xlim = c(0, 1), 
                       leftcols = c("studlab","tp", "n1"), 
                       leftlabs = c("Study", "TP", "Total \n (TP+FN)"), 
                       rightlabs= c("Sensitivity", "95% CI"),
                       xlab = "Sensitivity", 
                       col.square = "grey", 
                       col.square.lines = "grey", 
                       colgap = "5mm", # Espacio entre columnas.
                       just.studlab = "left", # Justificación de la columna de estudios
                       allstudies = TRUE,
                       col.by = "black")

  return(f_sen)
}


# Specificity:

forest_plot_spe <- function(data, covariate=NULL) {
  
  Y_forest = dataset_XYZ(data)$Y
  
  if(0 %in% Y_forest$n0){
    Y_forest <- subset(Y_forest, Y_forest$n0!= 0)
  } else {
    Y_forest <- Y_forest
  }
  
  if (!is.null(covariate)){
    if (covariate != "-") {
      spe <- metaprop(Y_forest$tn, Y_forest$n0, studlab = id, data = Y_forest,
                      byvar = Y_forest[,covariate], print.byvar = TRUE, bylab = covariate,
                      overall = FALSE,
                      overall.hetstat= FALSE, 
                      comb.fixed = FALSE, 
                      comb.random = FALSE)
    }
    else if (covariate == "-") {
      spe <- metaprop(Y_forest$tn, Y_forest$n0, studlab = id, data = Y_forest,
                      overall = FALSE,
                      overall.hetstat= FALSE, 
                      comb.fixed = FALSE, 
                      comb.random = FALSE)
    }
  }  
  else {
    spe <- metaprop(Y_forest$tn, Y_forest$n0, studlab = id, data = Y_forest,
                    overall = TRUE,
                    overall.hetstat= FALSE,
                    comb.fixed = FALSE)
  }
  
  f_spe <- forest(spe, 
                       xlim = c(0, 1), 
                       leftcols = c("studlab", "tn", "n0"), 
                       leftlabs = c("Study", "TN", "Total \n (TN+FP)"), 
                       rightlabs= c("Specificity", "95% CI"),
                       xlab = "Specificity",
                       col.square = "grey", 
                       col.square.lines = "grey",
                       colgap = "5mm", # Espacio entre columnas.
                       just.studlab = "left", # Justificación de la columna de estudios
                       allstudies = TRUE,
                       col.by = "black")
  
  
  return(f_spe)
}
  
  
# ----------------------------------------------------------------------------------------------- #

#### ROC PLANE: ####

roc_plane_func <- function(data, covariate, ci_bars) {
  
  Y = dataset_XYZ(data)$Y
  
  if (0 %in% Y$n1 | 0 %in% Y$n0){
    Y <- subset(Y, Y$n1!= 0 & Y$n0 !=0)
  } else{
    Y<- Y}
  
  # If a covariate is selected:
  if (covariate != "-") {
    roc <- ggplot(data = Y, 
                  aes(x = 1-spec, 
                      y = sens, 
                      color = factor(Y[,covariate]), # color depends on the covariate
                      shape = factor(Y[,covariate]), # shape depends on the covariate
                      size = n1+n0, # size depends on number of observations of the study
                      text = paste('ID:', id, '\nSize:', n1+n0, '\n1-Specificity:', round(1-spec,3), '\nSensitivity:' , round(sens,3)))) + # text displayed when hovering over a point 
      scale_shape_manual(values=c(0, 1, 2, 5, 6)) + # posible shapes
      scale_color_grey(start=0.2, end=0.6) + # grayscale parameters
      guides(size = FALSE) + # turn off size legend
      xlim(0,1) + ylim(0,1) + # axes limits
      theme_light() + # to set the background as white
      labs(x = "1-Specificity", y = "Sensitivity", title = "ROC plane") + # axes and graph title
      theme(plot.title = element_text(hjust = 0.5), # to adjust the title position
            legend.title = element_blank(), # to hide the legend title
            legend.background = element_rect(fill="lightgray"), # background color of legend
            legend.position = c(0.87, 0.1)) # lengend position
    
    # If confidence intervals are to be displayed:
    if (ci_bars == TRUE){
      roc <- roc + geom_errorbar(aes(ymin = CIinfsen, ymax = CIsupsen), 
                                 width = 0.02, size = 0.4) + # width of bar ends
        geom_errorbarh(aes(xmin = 1-CIsupspe, xmax = 1-CIinfspe), 
                       height = 0.05, size = 0.4) # width of bar ends
    }
    
    roc <- roc + geom_point(aes(shape = factor(Y[,covariate]))) # to make the shape of the points depend on the covariate
    
  }
  
  # Else (if no covariate has been selected):
  else {
    roc <- ggplot(data = Y, 
                  aes(x = 1-spec, 
                      y = sens, 
                      size = n1+n0, # size depends on number of observations of the study
                      color = "black", # color does not depend on the covariate
                      shape = 1, # shape does not depend on the covariate
                      text = paste('ID:', id, '\nSize:', n1+n0, '\n1-Specificity:', round(1-spec,3), '\nSensitivity:' , round(sens,3)))) + # text displayed when hovering over a point 
      scale_shape_identity() +
      scale_color_grey(start=0.2, end=0.6) + # grayscale parameters
      xlim(0,1) + ylim(0,1) + # axes limits
      theme_light() + # to set the background as white
      labs(x = "1-Specificity", y = "Sensitivity", title = "ROC plane") + # axes and graph title
      theme(plot.title = element_text(hjust = 0.5), # to adjust the title position
            legend.position = "none") # to hide the legend 
    
    # If confidence intervals are to be displayed:
    if (ci_bars == TRUE){
      roc <- roc + geom_errorbar(aes(ymin = CIinfsen, ymax = CIsupsen), 
                                 width = 0.02, size = 0.4) + # width of bar ends
        geom_errorbarh(aes(xmin = 1-CIsupspe, xmax = 1-CIinfspe), 
                       height = 0.05, size = 0.4) # width of bar ends
    }
    
    roc <- roc + geom_point() 
     
  }
  
  return(roc)
  
}
  

# ----------------------------------------------------------------------------------------------- #

#### BRMA - MODEL SELECTION ####
  
BRMA_model_select <-function(data){
  
  X = dataset_XYZ(data)$X
  Z = dataset_XYZ(data)$Z
  
  MA_biv <-  glmer(formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study), data = X, family = binomial, 
                   nAGQ = 1, verbose = 0)
  error<-MA_biv@optinfo$conv$lme4$code
  convergencia<- is.null(error)
  
  if (nrow(Z) < 4){
    model<-"univariate"
    return(list(model = model, MA_biv = NULL))
  } else if(convergencia == TRUE){
    model<- "bivariate"
    MA_biv <- MA_biv
    return(list(model = model, MA_biv = MA_biv))
  } else{
    MA_biv <-update(MA_biv,control=glmerControl(optimizer="bobyqa"))
    error<-MA_biv@optinfo$conv$lme4$code
    convergencia<- is.null(error)
    if(convergencia == TRUE){
      model <- "bivariate"
      MA_biv <- MA_biv
      return(list(model= model, MA_biv = MA_biv))
    } else {
      MA_biv <-update(MA_biv,control=glmerControl(optimizer="optimx",optCtrl=list(method="nlminb")))
      error<-MA_biv@optinfo$conv$lme4$code
      convergencia<- is.null(error)
      if (convergencia == TRUE){
        model <- "bivariate"
        MA_biv <- MA_biv
        return(list(model = model, MA_biv = MA_biv))
      } else {
        MA_biv<-update(MA_biv,control=glmerControl(optimizer="optimx",optCtrl=list(method="L-BFGS-B")))
        error<-MA_biv@optinfo$conv$lme4$code
        convergencia<- is.null(error)
        if (convergencia == TRUE){
          model <-"bivariate"
          MA_biv<-MA_biv
          return(list(model = model, MA_biv = MA_biv))
        } else { 
          MA_biv <- update(MA_biv,control=glmerControl(optimizer="nloptwrap",optCtrl=list(method="NLOPT_LN_BOBYQA")))
          error<-MA_biv@optinfo$conv$lme4$code
          convergencia<- is.null(error)
          if (convergencia == TRUE){
            model<-"bivariate"
            MA_biv<-MA_biv
            return(list(model = model, MA_biv = MA_biv))
          } else {
            MA_biv <- update(MA_biv,control=glmerControl(optimizer="nloptwrap",optCtrl=list(method="NLOPT_LN_NELDERMEAD")))
            error<-MA_biv@optinfo$conv$lme4$code
            convergencia<- is.null(error)
            if (convergencia == TRUE){
              model<-"bivariate"
              MA_biv <- MA_biv
              return(list(model = model, MA_biv = MA_biv))
            } else {
              model<- "univariate"
              return(list(model = model, MA_biv = NULL))
            }
          }
          
        }
        
      }
    }
  }
}

# ----------------------------------------------------------------------------------------------- #

#### BRMA - STATISTICS ####

BRMA_statistics <- function(data) {

  X = dataset_XYZ(data)$X
  Z = dataset_XYZ(data)$Z
  
  MA_biv = BRMA_model_select(data)$MA_biv
    
  ma_biv = summary(MA_biv)
  N <- ma_biv$ngrps
  
  # logit sens and logit spec
  lsensbiv = ma_biv$coeff[1,1]
  lspecbiv = ma_biv$coeff[2,1]
  se.lsensbiv = ma_biv$coeff[1,2]
  se.lspecbiv = ma_biv$coeff[2,2]
  
  # 95% confidence intervals for logit sens and logit spec
  Sensbiv = c(lsensbiv, lsensbiv-qnorm(0.975)*se.lsensbiv, lsensbiv+qnorm(0.975)*se.lsensbiv)
  Specbiv = c(lspecbiv, lspecbiv-qnorm(0.975)*se.lspecbiv, lspecbiv+qnorm(0.975)*se.lspecbiv)
  
  # sens and spec estimates in the raw scale
  sbiv <-plogis( Sensbiv ) 
  ebiv <-plogis( Specbiv ) 
  
  # DOR and likelihood ratios
  DORbiv = exp(lsensbiv+lspecbiv )
  LRpbiv = plogis(lsensbiv)/(1-plogis(lspecbiv))
  LRmbiv = ((1-plogis(lsensbiv))/plogis(lspecbiv))
  
  # Confidence intervals with the delta method
  se.DORbiv = deltamethod (~ (x1+x2) , mean = c(lsensbiv,lspecbiv) , cov = ma_biv$vcov )
  
  se.LRpbiv = deltamethod (~ log((exp(x1)/(1+exp(x1)))/(1-(exp(x2)/(1+exp(x2))))),
                           mean = c(lsensbiv,lspecbiv) , cov = ma_biv$vcov )
  
  se.LRmbiv = deltamethod (~ log((1-(exp(x1)/(1+exp(x1))))/(exp(x2)/(1+exp(x2)))) , 
                           mean = c(lsensbiv,lspecbiv) , cov = ma_biv$vcov ) 
  
  # random effects correlation (correlation between logits)
  corr <- attr(summary(MA_biv)$varcor$study,"correlation")[1,2]
  
  ##Covariance betwen E(logit(sen)) and E(logit(spe))
  cov <- ma_biv$vcov[1, 2]  
  
  ##Var(logit(sen)) and Var(logit(spe))
  VarLogitSenbiv <- ma_biv$varcor$study[1, 1]  
  VarLogitSpebiv <- ma_biv$varcor$study[2, 2]  
  
  ##Median Odds Ratio of sensitivity and specificity
  MORsenbiv <- exp(qnorm(0.75)*sqrt(2*(ma_biv$varcor$study[1,1])))
  MORspebiv <- exp(qnorm(0.75)*sqrt(2*(ma_biv$varcor$study[2,2])))
  
  
  ##Bivariate I2
  sum_sen = 0
  sum_spe = 0
  for(i in 1:nrow(Z)){
    sum_sen = sum_sen + (1/Z$n1[i])
    sum_spe = sum_spe + (1/Z$n0[i])
  }
  with_stud_var_sen = (exp((VarLogitSenbiv/2) + lsensbiv) + exp((VarLogitSenbiv/2) - lsensbiv) + 2) * sum_sen * (1/N)
  with_stud_var_spe = (exp((VarLogitSpebiv/2) + lspecbiv) + exp((VarLogitSpebiv/2) - lspecbiv) + 2) * sum_spe * (1/N)
  
  matT <- matrix(c(VarLogitSenbiv, corr*sqrt(VarLogitSenbiv)*sqrt(VarLogitSpebiv), corr*sqrt(VarLogitSenbiv)*sqrt(VarLogitSpebiv), VarLogitSpebiv), ncol=2, byrow=T)
  detT <- det(matT)
  sigma <- matrix(c(with_stud_var_sen, 0, 0, with_stud_var_spe), ncol=2, byrow=T)
  detsigma <- det(sigma)
  
  I2biv = sqrt(detT)/(sqrt(detsigma)+sqrt(detT))
  
  #Elipse de predicción y su área.
  
  phi <- seq(0, 2 * pi, len = 1000)
  
  SEPA <- sqrt(VarLogitSenbiv + se.lsensbiv ^ 2)
  SEPB <- sqrt(VarLogitSpebiv + se.lspecbiv ^ 2)
  sAB <- corr * sqrt(VarLogitSenbiv) * sqrt(VarLogitSpebiv)
  rho <- (sAB + cov) / (SEPA * SEPB)
  
  # boundary constant of the ellipse
  croot <- sqrt(2 * qf(0.95, 2, N - 2))
  
  a <- SEPA * croot
  b <- SEPB * croot
  sen <- exp(lsensbiv) / (1 + exp(lsensbiv))
  esp <- exp(lspecbiv) / (1 + exp(lspecbiv))
  
  y <-lsensbiv + a * cos(phi + acos(pmin(pmax(rho,-1.0),1.0)))     ## Sensitivity (logit scale)
  x <-lspecbiv + b * cos(phi)                 ## Specificity (logit scale)
  specificity <- 1 - (exp(x) / (1 + exp(x)))  ## ROC scale
  sensitivity <- exp(y) / (1 + exp(y))      ## ROC scale
  
  area <- polyarea(specificity, sensitivity)
  area <- round(area, 4)
  
  # SROC
  lambda_sroc<-(((VarLogitSpebiv)/(VarLogitSenbiv))^(1/4))*lsensbiv + (((VarLogitSenbiv)/(VarLogitSpebiv))^(1/4))*lspecbiv
  beta_sroc<-1/2 * log(VarLogitSpebiv/VarLogitSenbiv)
  theta_sroc<-1/2*(((sqrt(VarLogitSpebiv)/sqrt(VarLogitSenbiv))^(1/2))*lsensbiv-((sqrt(VarLogitSenbiv)/sqrt(VarLogitSpebiv))^(1/2))*lspecbiv)
  var_alpha_sroc<- 2*(sqrt(VarLogitSenbiv)*sqrt(VarLogitSpebiv)+sAB)
  var_theta_sroc<- (1/2)*(sqrt(VarLogitSenbiv)*sqrt(VarLogitSpebiv)-sAB)
  
  #Resultados
  summary_statistics = matrix(round(c(sbiv[1], ebiv[1], DORbiv , LRpbiv , LRmbiv,  1-ebiv[1], 
                                      sbiv[2], ebiv[2], DORbiv *exp(- qnorm(0.975) * se.DORbiv), LRpbiv * exp(- qnorm(0.975) * se.LRpbiv) , LRmbiv * exp(- qnorm(0.975) * se.LRmbiv), 1-ebiv[3],
                                      sbiv[3], ebiv[3], DORbiv * exp(qnorm(0.975) * se.DORbiv), LRpbiv * exp(qnorm(0.975) * se.LRpbiv), LRmbiv * exp(qnorm(0.975) * se.LRmbiv), 1-ebiv[2]), 3), 
                                      byrow = F, nrow=6, 
                                      dimnames=list(c("Sensitivity", "Specificity", "DOR", "LR+", "LR-", "FPR"), c("Estimate", "95% LCI", "95% UCI")))
  
  revman = matrix(round(c(lsensbiv, lspecbiv, VarLogitSenbiv, VarLogitSpebiv, se.lsensbiv, se.lspecbiv, cov, corr), 3), byrow=T, nrow=8,
                  dimnames = list(c("Logit sensitivity", "Logit specificity", "Var(logit(sen))", "Var(logit(spec))", "SE(logit(sens))", "SE(logit(spec))", "Covariance(ES)", "Correlation"), c("Estimate")))
  
  heterogeneity = matrix(round(c(VarLogitSenbiv, VarLogitSpebiv, MORsenbiv, MORspebiv, I2biv, area), 3), byrow=T, nrow=6, 
                               dimnames=list(c("Var logit(sen)", "Var logit(spe)", "MOR sensitivity", "MOR specificity", "Bivariate I2", "Area 95% Prediction Ellipse"), c("Estimate")))
  
  return(list(summary_statistics, revman, heterogeneity))
  
}


# ----------------------------------------------------------------------------------------------- #

#### BRMA - METAREGRESSION ####

metaregresion <- function(data, covariate) {
  
  X = dataset_XYZ(data)$X
  X.split = split(X, X[,covariate])
  
  X.1 = X.split[[1]]
  X.2 = X.split[[2]]
  
  ## Modelos
  ma.global <- BRMA_model_select(data)$MA_biv
  ma.1 <- glmer(cbind(true, n-true) ~ 0 + sens + spec + (0 + sens + spec | study), data = X.1, family = binomial)
  ma.2 <- glmer(cbind(true, n-true) ~ 0 + sens + spec + (0 + sens + spec | study), data = X.2, family = binomial)
  
  ###### Creamos dummies ####
  
  X$cov1 <- 2 - as.numeric(factor(X[,covariate]))
  X$cov2 <- 1 - X$cov1
  
  X$secov1 <- (X$cov1)*(X$sens)
  X$secov2 <- (X$cov2)*(X$sens)
  
  X$spcov1 <- (X$cov1)*(X$spec)
  X$spcov2 <- (X$cov2)*(X$spec)
  
  ######### comparación de modelos + p-valores ###########
  ma.global2 <- glmer(cbind(true, n-true) ~ 0 + secov1 + secov2 + spcov1 + spcov2 + (0 + sens + spec | study), data = X, family = binomial)
  comp.global<-lrtest(ma.global, ma.global2) # pvalor de comparacion modelos globales (sin covariate)
  p.global <- comp.global$`Pr(>Chisq)`[2]
  
  ma.sens <- glmer(cbind(true, n-true) ~ 0 + sens + spcov1 + spcov2 + (0 + sens + spec | study), data = X, family = binomial)
  comp.sens <- lrtest(ma.global2, ma.sens) # difference in sensitivity between categories of covariate?
  p.sens <- comp.sens$`Pr(>Chisq)`[2]
  
  ma.spe <- glmer(cbind(true, n-true) ~ 0 + secov1 +  secov2 + spec + (0 + sens + spec | study), data = X, family = binomial)
  comp.spec <- lrtest(ma.global2, ma.spe) #difference in specificity between categories of covariate?
  p.spec <- comp.spec$`Pr(>Chisq)`[2]
  
  ## Sensibilidad y especificidad para cada categoria de la covariable + ratios
  
  lsenscov1 = (summary(ma.global2)$coeff[1,1]) ## Este es logit
  lsenscov2 = (summary(ma.global2)$coeff[2,1])
  
  lsenscov1logit = plogis(lsenscov1) ## OJO no es logit!! aunque se llame asi
  lsenscov2logit = plogis(lsenscov2)
  
  ratiosen <- lsenscov2logit/lsenscov1logit # sensibilidad cov1/sensibilidad cov2 (no está en logit)
  
  lspecov1 = (summary(ma.global2)$coeff[3,1])
  lspecov2 = (summary(ma.global2)$coeff[4,1])
  
  lspecov1logit = plogis(lspecov1)
  lspecov2logit = plogis(lspecov2)
  
  ratiospe <- lspecov2logit/lspecov1logit
  
  #matriz de var-cov
  
  matriz.vcovsen = summary(ma.global2)$vcov[1:2, 1:2]
  matriz.vcovspe = summary(ma.global2)$vcov[3:4, 3:4] 
  
  ###### IC 95% #####

  se.relsen <- deltamethod (~ log((exp(x1)/(1+exp(x1)))/(exp(x2)/(1+exp(x2)))), mean = c(lsenscov1,lsenscov2), cov = matriz.vcovsen)
  se.relspe <- deltamethod (~ log((exp(x1)/(1+exp(x1)))/(exp(x2)/(1+exp(x2)))), mean = c(lspecov1,lspecov2), cov = matriz.vcovspe)
  
  tablameta = data.frame(Parameter = c(paste("Relative sensitivity level", unique(X.2[,covariate]), "vs", unique(X.1[,covariate]), sep=" "), 
                                                 paste("Relative specificity level", unique(X.2[,covariate]), "vs", unique(X.1[,covariate]), sep=" "), 
                                                 "Global test comparison"),
                                   Estimate = round(c(ratiosen, ratiospe, NA), 3),
                                   LCI = c(
                                     round(ratiosen * exp(- qnorm(0.975) * se.relsen),3),
                                     round(ratiospe * exp(- qnorm(0.975) * se.relspe),3),
                                     NA),
                                   UCI = c(
                                     round(ratiosen * exp(qnorm(0.975) * se.relsen),3),
                                     round(ratiospe * exp(qnorm(0.975) * se.relspe),3),
                                     NA),
                                   "p-value" = round(c(p.sens, p.spec, p.global),3),
                                   check.names = FALSE)
  
  return(tablameta)
}

# ----------------------------------------------------------------------------------------------- #

#### URMA - STATISTICS ####

URMA_statistics <- function(data){
  
  X_uni = dataset_XYZ(data)$X_uni
  Y_forest = dataset_XYZ(data)$Y
  
  if (0 %in% X_uni$n1){
    X_uni_sen <- subset(X_uni, X_uni$n1!= 0)
  } else {
    X_uni_sen <- X_uni
  }
  
  if(0 %in% X_uni$n0 ){
    X_uni_spe <- subset(X_uni, X_uni$n0!=0)
  } else {
    X_uni_spe <- X_uni
  }
  
  X_uni_sen<- reshape(as.data.frame(X_uni_sen), direction = "long", varying = list(c("n1", "n0"), c("true1", "true0")), timevar = "sens", times = c(1,0),
                      v.names = c("n", "true"))
  X_uni_spe<- reshape(as.data.frame(X_uni_spe), direction = "long", varying = list(c("n1", "n0"), c("true1", "true0")), timevar = "spec", times = c(0,1),
                      v.names = c("n", "true"))
  
  if(0 %in% Y_forest$n1 ){
    Y_forest_sen <- subset(Y_forest, Y_forest$n1!= 0)
  } else {
    Y_forest_sen <- Y_forest
  }
  
  if (0 %in% Y_forest$n0){
    Y_forest_spe <- subset(Y_forest, Y_forest$n0!= 0)
  } else {
    Y_forest_spe <- Y_forest
  }
  
  MA.sen <- glmer(formula = cbind(  true , n - true ) ~ 0 + sens + (0 + sens|study), data = X_uni_sen, family = binomial, 
                  nAGQ = 1, verbose = 0)
  ma_sen=summary(MA.sen)
  MA.spe <- glmer(formula = cbind(  true , n - true ) ~ 0 + spec + (0 + spec|study), data = X_uni_spe, family = binomial, 
                  nAGQ = 1, verbose = 0)
  ma_spe=summary(MA.spe)
  
  # logit sens and logit spec
  lsens = ma_sen$coeff[1,1]
  lspec = ma_spe$coeff[1,1]
  
  
  se.lsens = ma_sen$coeff[1,2]
  se.lspec = ma_spe$coeff[1,2]
  
  # 95% confidence intervals for logit sens and logit spec
  Sens = c(lsens, lsens-qnorm(0.975)*se.lsens, lsens+qnorm(0.975)*se.lsens)
  Spec = c(lspec, lspec-qnorm(0.975)*se.lspec, lspec+qnorm(0.975)*se.lspec)
  
  # sens and spec estimates in the raw scale
  s <-plogis( Sens ) 
  e <-plogis( Spec ) 
  
  # DOR and likelihood ratios
  DOR = exp(lsens+lspec )
  LRp = plogis(lsens)/(1-plogis(lspec))
  LRm = ((1-plogis(lsens))/plogis(lspec))
  
  
  #definimos matrix de var-cov
  #hay que coger el valor de vcov?o la varianza?
  matriz.cov<-matrix(c(ma_sen$vcov@x, 0, 0,ma_spe$vcov@x ), byrow=T, ncol=2)
  
  # Confidence intervals with the delta method
  se.DOR = deltamethod (~ (x1+x2) , mean = c(lsens,lspec), cov=matriz.cov) 
  
  se.LRp = deltamethod (~ log((exp(x1)/(1+exp(x1)))/(1-(exp(x2)/(1+exp(x2))))) , mean = c(lsens,lspec), cov=matriz.cov)
  
  se.LRm = deltamethod (~ log((1-(exp(x1)/(1+exp(x1))))/(exp(x2)/(1+exp(x2)))) , mean = c(lsens,lspec), cov=matriz.cov) 
  
  
  #Heterogeneidad
  VarLogitSen <- ma_sen$varcor$study[1]  ##Var(logit(sen))
  VarLogitSpe <- ma_spe$varcor$study[1]  ##Var(logit(spe))
  MORsen<-exp(qnorm(0.75)*sqrt(2*(ma_sen$varcor$study[1])))
  MORspe<-exp(qnorm(0.75)*sqrt(2*(ma_spe$varcor$study[1])))
  
  #I2 sensitivity and specificity
  i2sen <- metaprop(Y_forest_sen$tp, Y_forest_sen$n1, studlab = id, data = Y_forest_sen,
                  overall = TRUE,
                  overall.hetstat= FALSE,
                  comb.fixed = FALSE)
  I2sen<-i2sen$I2
  
  i2spe <- metaprop(Y_forest_spe$tn, Y_forest_spe$n0, studlab = id, data = Y_forest_spe,
                  overall = FALSE,
                  overall.hetstat= FALSE, 
                  comb.fixed = FALSE)
  I2spe<-i2spe$I2
  
  
  summary_statistics = matrix(round(c(s[1], e[1], DOR, LRp, LRm, 1-e[1], s[2], e[2], DOR *exp(- qnorm(0.975) * se.DOR),
                                   LRp * exp(-qnorm(0.975) * se.LRp), LRm * exp(- qnorm(0.975) * se.LRm), 1-e[3], s[3], e[3],
                                   DOR * exp(qnorm(0.975) * se.DOR), LRp * exp(qnorm(0.975) * se.LRp), LRm * exp(qnorm(0.975) * se.LRm), 1-e[2]) , 3), 
                              byrow = F, nrow=6, dimnames=list(c("Sensitivity", "Specificity", "DOR", "LR+", "LR-", "FPR" ), c("Estimate", "95% LCI", "95% UCI")))
 
  heterogeneity = matrix(round(c(VarLogitSen, VarLogitSpe, MORsen, MORspe, I2sen, I2spe), 3) , byrow=T, nrow=6, 
                        dimnames=list(c("Var logit(sen)", "Var logit(spe)", "MOR sensitivity", "MOR specificity", "I2 sensitivity", "I2 specificity"),c("Estimate")))
  
  
  
  
  return(list(summary_statistics, heterogeneity))
}

# ----------------------------------------------------------------------------------------------- #

#### BRMA - SUBGROUP MODEL SELECT ####

subgroup_model_select <-function(data, covariate){
  
  X = dataset_XYZ(data)$X
  Z = dataset_XYZ(data)$Z
  
  X$cov1 <- 2 - as.numeric(factor(X[,covariate]))
  X$cov2 <- 1 - X$cov1
  
  X$secov1 <- (X$cov1)*(X$sens)
  X$secov2 <- (X$cov2)*(X$sens)
  
  X$spcov1 <- (X$cov1)*(X$spec)
  X$spcov2 <- (X$cov2)*(X$spec)
  
  
  MA_sub <-  glmer(cbind(true, n-true) ~ 0 + secov1 + secov2 + spcov1 + spcov2 + (0 + sens + spec | study), data = X, family = binomial)
  error<-MA_sub@optinfo$conv$lme4$code
  convergencia<- is.null(error)
  
  if(convergencia == TRUE){
    model<- "bivariate"
    MA_sub <- MA_sub
    return(list(model = model, MA_sub = MA_sub))
  } else{
    MA_sub <-update(MA_sub,control=glmerControl(optimizer="bobyqa"))
    error<-MA_sub@optinfo$conv$lme4$code
    convergencia<- is.null(error)
    if(convergencia == TRUE){
      model <- "bivariate"
      MA_sub <- MA_sub
      return(list(model= model, MA_sub = MA_sub))
    } else {
      MA_sub <-update(MA_sub,control=glmerControl(optimizer="optimx",optCtrl=list(method="nlminb")))
      error<-MA_sub@optinfo$conv$lme4$code
      convergencia<- is.null(error)
      if (convergencia == TRUE){
        model <- "bivariate"
        MA_sub <- MA_sub
        return(list(model = model, MA_sub = MA_sub))
      } else {
        MA_sub<-update(MA_sub,control=glmerControl(optimizer="optimx",optCtrl=list(method="L-BFGS-B")))
        error<-MA_sub@optinfo$conv$lme4$code
        convergencia<- is.null(error)
        if (convergencia == TRUE){
          model <-"bivariate"
          MA_sub<-MA_sub
          return(list(model = model, MA_sub = MA_sub))
        } else { 
          MA_sub <- update(MA_sub,control=glmerControl(optimizer="nloptwrap",optCtrl=list(method="NLOPT_LN_BOBYQA")))
          error<-MA_sub@optinfo$conv$lme4$code
          convergencia<- is.null(error)
          if (convergencia == TRUE){
            model<-"bivariate"
            MA_sub<-MA_sub
            return(list(model = model, MA_sub = MA_sub))
          } else {
            MA_sub <- update(MA_sub,control=glmerControl(optimizer="nloptwrap",optCtrl=list(method="NLOPT_LN_NELDERMEAD")))
            error<-MA_sub@optinfo$conv$lme4$code
            convergencia<- is.null(error)
            if (convergencia == TRUE){
              model<-"bivariate"
              MA_sub <- MA_sub
              return(list(model = model, MA_sub = MA_sub))
            } else {
              model<- "univariate"
              return(list(model = model, MA_sub = NULL))
            }
          }
          
        }
        
      }
    }
  }
}


# ----------------------------------------------------------------------------------------------- #

#### BRMA - SUBGROUP ####

subgroup <- function(data, covariate){
  
  X = dataset_XYZ(data)$X
  X.split = split(X, X[,covariate])
  
  X.1 = X.split[[1]]
  X.2 = X.split[[2]]
  
  X$cov1 <- 2 - as.numeric(factor(X[,covariate]))
  X$cov2 <- 1 - X$cov1
  
  X$secov1 <- (X$cov1)*(X$sens)
  X$secov2 <- (X$cov2)*(X$sens)
  
  X$spcov1 <- (X$cov1)*(X$spec)
  X$spcov2 <- (X$cov2)*(X$spec)
  
  ######### comparación de modelos + p-valores ###########
  ma.global2 <- subgroup_model_select(data, covariate)$MA_sub
    
  #logits sens y spec
  lsenscov1 = (summary(ma.global2)$coeff[1,1])
  lsenscov2 = (summary(ma.global2)$coeff[2,1])
  
  lspeccov1 = (summary(ma.global2)$coeff[3,1])
  lspeccov2 = (summary(ma.global2)$coeff[4,1])
  
  #errores estandar
  se.lsenscov1 = summary(ma.global2)$coeff[1,2]
  se.lspeccov1 = summary(ma.global2)$coeff[3,2]
  
  se.lsenscov2 = summary(ma.global2)$coeff[2,2]
  se.lspeccov2 = summary(ma.global2)$coeff[4,2]
  
  
  # 95% confidence intervals for logit sens and logit spec
  Senscov1 = c(lsenscov1, lsenscov1-qnorm(0.975)*se.lsenscov1, lsenscov1+qnorm(0.975)*se.lsenscov1)
  Senscov2 = c(lsenscov2, lsenscov2-qnorm(0.975)*se.lsenscov2, lsenscov2+qnorm(0.975)*se.lsenscov2)
  
  Speccov1 = c(lspeccov1, lspeccov1-qnorm(0.975)*se.lspeccov1, lspeccov1+qnorm(0.975)*se.lspeccov1)
  Speccov2 = c(lspeccov2, lspeccov2-qnorm(0.975)*se.lspeccov2, lspeccov2+qnorm(0.975)*se.lspeccov2)
  
  #sen y spec tabla summary statistics
  # sens and spec estimates in the raw scale
  scov1 <-plogis( Senscov1 ) 
  scov2 <-plogis( Senscov2 ) 
  ecov1 <-plogis( Speccov1 ) 
  ecov2 <-plogis( Speccov2 ) 
  
  
  # DOR and likelihood ratios
  DORcov1 = exp(lsenscov1+lspeccov1 )
  DORcov2 = exp(lsenscov2+lspeccov2 )
  LRpcov1 = plogis(lsenscov1)/(1-plogis(lspeccov1))
  LRpcov2 = plogis(lsenscov2)/(1-plogis(lspeccov2))
  LRmcov1 = ((1-plogis(lsenscov1))/plogis(lspeccov1))
  LRmcov2 = ((1-plogis(lsenscov2))/plogis(lspeccov2))
  
  
  # Confidence intervals with the delta method 
  matriz.cov1 = matrix(c(summary(ma.global2)$vcov[1,1], summary(ma.global2)$vcov[1,3], summary(ma.global2)$vcov[3,1], summary(ma.global2)$vcov[3,3]), byrow = T, nrow = 2)
  matriz.cov2 = matrix(c(summary(ma.global2)$vcov[2,2], summary(ma.global2)$vcov[2,4], summary(ma.global2)$vcov[4,2], summary(ma.global2)$vcov[4,4]), byrow = T, nrow = 2)
  
  
  se.DORcov1 = deltamethod (~ (x1+x2) , mean = c(lsenscov1,lspeccov1), cov=matriz.cov1) 
  se.DORcov2 = deltamethod (~ (x1+x2) , mean = c(lsenscov2,lspeccov2), cov=matriz.cov2) 
  se.LRpcov1 = deltamethod (~ log((exp(x1)/(1+exp(x1)))/(1-(exp(x2)/(1+exp(x2))))) , mean = c(lsenscov1,lspeccov1), cov=matriz.cov1)
  se.LRpcov2 = deltamethod (~ log((exp(x1)/(1+exp(x1))/(1-(exp(x2)/(1+exp(x2)))))) , mean = c(lsenscov2,lspeccov2), cov=matriz.cov2)
  se.LRmcov1 = deltamethod (~ log((1-(exp(x1)/(1+exp(x1))))/(exp(x2)/(1+exp(x2)))) , mean = c(lsenscov1,lspeccov1), cov=matriz.cov1) 
  se.LRmcov2 = deltamethod (~ log((1-(exp(x1)/(1+exp(x1))))/(exp(x2)/(1+exp(x2)))) , mean = c(lsenscov2,lspeccov2), cov=matriz.cov2) 
  
  #covarianzas
  covar1 = summary(ma.global2)$vcov[3,1]
  covar2 = summary(ma.global2)$vcov[4,2]
  
  #correlation
  cor <- attr(summary(ma.global2)$varcor$study,"correlation")[1,2]
  
  #varianzas para ambas categorias de covariable
  varlogitsen = summary(ma.global2)$varcor$study[1, 1]
  varlogitspe = summary(ma.global2)$varcor$study[2, 2]
  
  
  #resultados
  summary_statistics_subgroup = data.frame(Parameter = c("Sensitivity", "Specificity", "DOR", "LR+", "LR-", "FPR"), 
                                           Estimate_cov1 = round(c(scov1[1], ecov1[1], DORcov1 , LRpcov1 , LRmcov1,  1-ecov1[1]), 3),
                                           LCI_cov1 = round(c(scov1[2], ecov1[2], DORcov1 *exp(- qnorm(0.975) * se.DORcov1), LRpcov1 * exp(- qnorm(0.975) * se.LRpcov1), LRmcov1 * exp(- qnorm(0.975) * se.LRmcov1), 1-ecov1[3]),3),
                                           UCI_cov1 = round(c(scov1[3], ecov1[3], DORcov1 *exp(qnorm(0.975) * se.DORcov1), LRpcov1 * exp(qnorm(0.975) * se.LRpcov1), LRmcov1 * exp(qnorm(0.975) * se.LRmcov1), 1-ecov1[2]),3),
                                           Estimate_cov2 = round(c(scov2[1], ecov2[1], DORcov2 , LRpcov2 , LRmcov2,  1-ecov2[1]),3),
                                           LCI_cov2 = round(c(scov2[2], ecov2[2], DORcov2 *exp(- qnorm(0.975) * se.DORcov2), LRpcov2 * exp(- qnorm(0.975) * se.LRpcov2), LRmcov2 * exp(- qnorm(0.975) * se.LRmcov2), 1-ecov2[3]),3),
                                           UCI_cov2 = round(c(scov2[3], ecov2[3], DORcov2 *exp(qnorm(0.975) * se.DORcov2), LRpcov2 * exp(qnorm(0.975) * se.LRpcov2), LRmcov2 * exp(qnorm(0.975) * se.LRmcov2), 1-ecov2[2]),3))
  
  colnames(summary_statistics_subgroup) <- c("Parameter", 
                                             paste("Estimate", unique(X.1[, covariate])), 
                                             paste("95% LCI", unique(X.1[, covariate])), 
                                             paste("95% UCI", unique(X.1[, covariate])), 
                                             paste("Estimate", unique(X.2[, covariate])), 
                                             paste("95% LCI", unique(X.2[, covariate])), 
                                             paste("95% UCI", unique(X.2[, covariate])))
  
  revman_subgroup = data.frame(Parameter = c("Logit sensitivity", "Logit specificity", "Var(logit(sen))*", "Var(logit(spec))*", "SE(logit(sens))", "SE(logit(spec))", "Covariance(ES)", "Correlation", "Studies"),
                               Estimate_cov1 = round(c(lsenscov1, lspeccov1, varlogitsen, varlogitspe, se.lsenscov1, se.lspeccov1, covar1, cor, nrow(X.1)/2), 3),
                               Estimate_cov2 = round(c(lsenscov2, lspeccov2, varlogitsen, varlogitspe, se.lsenscov2, se.lspeccov2, covar2, cor, nrow(X.2)/2), 3))
  
  revman_subgroup[9,2:3] <- round(as.numeric(revman_subgroup[9,2:3]), digits = 0)
  
  colnames(revman_subgroup) <- c("Parameter", 
                                 paste("Estimate", unique(X.1[, covariate])), 
                                 paste("Estimate", unique(X.2[, covariate])))
                                 
  return(list(summary_statistics_subgroup = summary_statistics_subgroup, revman_subgroup = revman_subgroup))
  
}


# ----------------------------------------------------------------------------------------------- #

#### BRMA - SROC CURVE ####

BRMA_sroc_data <- function(data) {
  
  X = dataset_XYZ(data)$X
  Z = dataset_XYZ(data)$Z
  
  MA_biv <-  BRMA_model_select(data)$MA_biv
  
  ma_biv = summary(MA_biv)
  N <- ma_biv$ngrps
  
  # logit sens and logit spec
  lsensbiv = ma_biv$coeff[1,1]
  lspecbiv = ma_biv$coeff[2,1]
  se.lsensbiv = ma_biv$coeff[1,2]
  se.lspecbiv = ma_biv$coeff[2,2]
  
  # random effects correlation
  corr <- attr(summary(MA_biv)$varcor$study,"correlation")[1,2]
  
  ##Covariance betwen E(logit(sen)) and E(logit(spe))
  cov <- ma_biv$vcov[1, 2]
  
  ##Var(logit(sen)) and Var(logit(spe))
  VarLogitSenbiv <- ma_biv$varcor$study[1, 1]
  VarLogitSpebiv <- ma_biv$varcor$study[2, 2]
  
  #Elipse de predicción y su área.
  phi <- seq(0, 2 * pi, len = 1000)
  N <- ma_biv$ngrps #número de estudios
  
  SEPA <- sqrt(VarLogitSenbiv + se.lsensbiv ^ 2)
  SEPB <- sqrt(VarLogitSpebiv + se.lspecbiv ^ 2)
  sAB <- corr * sqrt(VarLogitSenbiv) * sqrt(VarLogitSpebiv)
  rho <- (sAB + cov) / (SEPA * SEPB)
  
  # boundary constant of the ellipse
  croot <- sqrt(2 * qf(0.95, 2, N - 2))
  
  a <- SEPA * croot
  b <- SEPB * croot
  sen <- exp(lsensbiv) / (1 + exp(lsensbiv))
  esp <- exp(lspecbiv) / (1 + exp(lspecbiv))
  
  y <-lsensbiv + a * cos(phi + acos(pmin(pmax(rho,-1.0),1.0)))     ## Sensitivity (logit scale)
  x <-lspecbiv + b * cos(phi)                 ## Specificity (logit scale)
  specificity <- 1 - (exp(x) / (1 + exp(x)))  ## ROC scale
  sensitivity <- exp(y) / (1 + exp(y))      ## ROC scale
  
  elipse = data.frame(sensitivity, specificity)
  
  ##Elipse de confianza:
  mu.a <- lsensbiv + (se.lsensbiv * croot * cos(phi))
  mu.b <- lspecbiv + (se.lspecbiv * croot * cos(phi + acos(pmin(pmax(rho,-1.0),1.0))))
  
  sens <- exp(mu.a)/(1 + exp(mu.a))
  spec <- 1-(exp(mu.b)/(1 + exp(mu.b)))
  
  elipse_conf = data.frame(sens, spec)
  
  ##summary point
  sensit<- plogis(lsensbiv)
  specif<- plogis(lspecbiv)
  
  point<- data.frame(1-specif, sensit)
  
  ## ROC curve
  #metandiplot.ado
  b = ((VarLogitSpebiv)/(VarLogitSenbiv))^(1/4)
  lamb = (lsensbiv*b) + (lspecbiv/b)
  z = seq(0,1, len=1000)
  curva = invlogit((-logit(z)/b + lamb)/b)
  minse = min(Z$sens)
  minspe = min(Z$spec)
  for (i in 1:length(curva)){
    if (z[i] < minspe | curva[i] < minse){
      z[i] <- NA
    }
    
  }
  curv = data.frame(z, curva)
  curv = na.omit(curv)
  
  return(list(X = X, sum_point = point, ellipse_pred = elipse, ellipse_conf = elipse_conf, curv = curv))
  
}

subgroup_sroc_data <- function(data, covariate) {
  
  X = dataset_XYZ(data)$X
  Z = dataset_XYZ(data)$Z
  
  Z.split = split(Z, Z[,covariate])
  
  Z.1 = Z.split[[1]]
  Z.2 = Z.split[[2]]
  
  X$cov1 <- 2 - as.numeric(factor(X[,covariate]))
  X$cov2 <- 1 - X$cov1
  
  X$secov1 <- (X$cov1)*(X$sens)
  X$secov2 <- (X$cov2)*(X$sens)
  
  X$spcov1 <- (X$cov1)*(X$spec)
  X$spcov2 <- (X$cov2)*(X$spec)
  
  
  MA_sub <-  subgroup_model_select(data, covariate)$MA_sub
  
  ma_sub = summary(MA_sub)
  N_cov1 <- nrow(Z.1)
  N_cov2 <- nrow(Z.2)
  
  #logits sens y spec
  lsenscov1 = (ma_sub$coeff[1,1])
  lsenscov2 = (ma_sub$coeff[2,1])
  
  lspeccov1 = (ma_sub$coeff[3,1])
  lspeccov2 = (ma_sub$coeff[4,1])
  
  #errores estandar
  se.lsenscov1 = ma_sub$coeff[1,2]
  se.lspeccov1 = ma_sub$coeff[3,2]
  
  se.lsenscov2 = ma_sub$coeff[2,2]
  se.lspeccov2 = ma_sub$coeff[4,2]
  
  # random effects correlation
  corr <- attr(ma_sub$varcor$study,"correlation")[1,2]
  
  #varianzas para ambas categorias de covariable
  varlogitsen = ma_sub$varcor$study[1, 1]
  varlogitspe = ma_sub$varcor$study[2, 2]
  
  #covarianzas
  covar1 = ma_sub$vcov[3,1]
  covar2 = ma_sub$vcov[4,2]
  
  
  #Elipse de predicción y su área.
  phi <- seq(0, 2 * pi, len = 1000)
  
  SEPA_cov1 <- sqrt(varlogitsen + se.lsenscov1 ^ 2)
  SEPA_cov2 <- sqrt(varlogitsen + se.lsenscov2 ^ 2)
  SEPB_cov1 <- sqrt(varlogitspe + se.lspeccov1 ^ 2)
  SEPB_cov2 <- sqrt(varlogitspe + se.lspeccov2 ^ 2)
  sAB <- corr * sqrt(varlogitsen) * sqrt(varlogitspe)
  rho1 <- (sAB + covar1) / (SEPA_cov1 * SEPB_cov1)
  rho2 <- (sAB + covar2) / (SEPA_cov2 * SEPB_cov2)
  
  # boundary constant of the ellipse
  croot_cov1 <- sqrt(2 * qf(0.95, 2, N_cov1 - 2))
  croot_cov2 <- sqrt(2 * qf(0.95, 2, N_cov2 - 2))
  
  a1 <- SEPA_cov1 * croot_cov1
  a2 <- SEPA_cov2 * croot_cov2
  b1 <- SEPB_cov1 * croot_cov1
  b2 <- SEPB_cov2 * croot_cov2
  sen_cov1 <- exp(lsenscov1) / (1 + exp(lsenscov1))
  sen_cov2 <- exp(lsenscov2) / (1 + exp(lsenscov2))
  esp_cov1 <- exp(lspeccov1) / (1 + exp(lspeccov1))
  esp_cov2 <- exp(lspeccov2) / (1 + exp(lspeccov2))
  
  y_cov1 <-lsenscov1 + a1 * cos(phi + acos(pmin(pmax(rho1,-1.0),1.0)))     ## Sensitivity (logit scale)
  y_cov2 <-lsenscov2 + a2 * cos(phi + acos(pmin(pmax(rho2,-1.0),1.0)))
  x_cov1 <-lspeccov1 + b1 * cos(phi)                 ## Specificity (logit scale)
  x_cov2 <-lspeccov2 + b2 * cos(phi) 
  
  specificity_cov1 <- 1 - (exp(x_cov1) / (1 + exp(x_cov1)))  ## ROC scale
  specificity_cov2 <- 1 - (exp(x_cov2) / (1 + exp(x_cov2)))  ## ROC scale
  sensitivity_cov1 <- exp(y_cov1) / (1 + exp(y_cov1))      ## ROC scale
  sensitivity_cov2 <- exp(y_cov2) / (1 + exp(y_cov2))      ## ROC scale
  
  
  elipse_cov1 = data.frame(sensitivity_cov1, specificity_cov1)
  elipse_cov1$covar <- rep(Z.1[1,covariate], nrow(elipse_cov1)) 
  colnames(elipse_cov1) = c("sensitivity", "specificity", "covar")
  elipse_cov2 = data.frame(sensitivity_cov2, specificity_cov2)
  elipse_cov2$covar <-rep(Z.2[1,covariate], nrow(elipse_cov2))  
  colnames(elipse_cov2) = c("sensitivity", "specificity", "covar")
  
  ##Elipse de confianza:
  mu.a.cov1 <- lsenscov1 + (se.lsenscov1 * croot_cov1 * cos(phi))
  mu.a.cov2 <- lsenscov2 + (se.lsenscov2 * croot_cov2 * cos(phi))
  mu.b.cov1 <- lspeccov1 + (se.lspeccov1 * croot_cov1 * cos(phi + acos(pmin(pmax(rho1,-1.0),1.0))))
  mu.b.cov2 <- lspeccov2 + (se.lspeccov2 * croot_cov2 * cos(phi + acos(pmin(pmax(rho2,-1.0),1.0))))
  
  sens_cov1 <- exp(mu.a.cov1)/(1 + exp(mu.a.cov1))
  sens_cov2 <- exp(mu.a.cov2)/(1 + exp(mu.a.cov2))
  spec_cov1 <- 1-(exp(mu.b.cov1)/(1 + exp(mu.b.cov1)))
  spec_cov2 <- 1-(exp(mu.b.cov2)/(1 + exp(mu.b.cov2)))
  
  elipse_conf_cov1 = data.frame(sens_cov1, spec_cov1)
  elipse_conf_cov1$covar <-rep(Z.1[1,covariate], nrow(elipse_conf_cov1)) 
  colnames(elipse_conf_cov1) = c("sens", "spec", "covar")
  elipse_conf_cov2 = data.frame(sens_cov2, spec_cov2)
  elipse_conf_cov2$covar <-rep(Z.2[1,covariate], nrow(elipse_conf_cov2)) 
  colnames(elipse_conf_cov2) = c("sens", "spec", "covar")
  
  ##summary point
  sensit_cov1<- plogis(lsenscov1)
  sensit_cov2<- plogis(lsenscov2)
  specif_cov1<- plogis(lspeccov1)
  specif_cov2<- plogis(lspeccov2)
  
  point_cov1<- data.frame(1-specif_cov1, sensit_cov1)
  point_cov1$covar <- rep(Z.1[1,covariate], 1)
  colnames(point_cov1) = c("specif", "sensit", "covar")
  point_cov2<- data.frame(1-specif_cov2, sensit_cov2)
  point_cov2$covar <- rep(Z.2[1,covariate],1)
  colnames(point_cov2) = c("specif", "sensit", "covar")
  
  ## ROC curve
  #metandiplot.ado
  b = ((varlogitspe)/(varlogitsen))^(1/4)
  lamb_cov1 = (lsenscov1*b1) + (lspeccov1/b1)
  lamb_cov2 = (lsenscov2*b2) + (lspeccov2/b2)
  z1 = seq(0,1, len=1000)
  z2 = seq(0,1, len=1000)
  curva_cov1 = invlogit((-logit(z1)/b + lamb_cov1)/b)
  curva_cov2 = invlogit((-logit(z2)/b + lamb_cov2)/b)
  
  minse_cov1 = min(Z.1$sens)
  minse_cov2 = min(Z.2$sens)
  minspe_cov1 = min(Z.1$spec)
  minspe_cov2 = min(Z.2$spec)
  
  for (i in 1:length(curva_cov1)){
    if (z1[i] < minspe_cov1 | curva_cov1[i] < minse_cov1){
      z1[i] <- NA
    }
    
  }
  for (i in 1:length(curva_cov2)){
    if (z2[i] < minspe_cov2 | curva_cov2[i] < minse_cov2){
      z2[i] <- NA
    }
    
  }
  curv_cov1 = data.frame(z1, curva_cov1)
  curv_cov1 = na.omit(curv_cov1)
  curv_cov1$covar <- rep(Z.1[1,covariate], nrow(curv_cov1))
  colnames(curv_cov1) = c("z", "curva", "covar")
  curv_cov2 = data.frame(z2, curva_cov2)
  curv_cov2 = na.omit(curv_cov2)
  curv_cov2$covar <- rep(Z.2[1,covariate], nrow(curv_cov2))
  colnames(curv_cov2) = c("z", "curva", "covar")
  
  sum_point = rbind(point_cov1, point_cov2)
  sum_point <- as.data.frame(lapply(sum_point, unlist))
  elipse_pred = as.data.frame(rbind(elipse_cov1, elipse_cov2))
  elipse_pred <- as.data.frame(lapply(elipse_pred, unlist))
  elipse_conf = as.data.frame(rbind(elipse_conf_cov1, elipse_conf_cov2))
  elipse_conf <-as.data.frame(lapply(elipse_conf, unlist))
  curv = as.data.frame(rbind(curv_cov1, curv_cov2))
  curv <- as.data.frame(lapply(curv, unlist))
  
  return(list(sum_point = sum_point,  ellipse_pred = elipse_pred,  ellipse_conf = elipse_conf,  curv = curv, Z = Z))
  
}



BRMA_sroc <- function(data, covariate = NULL, subgroup = FALSE, 
                      points, summary_point, ellipse_pred, ellipse_conf, curve) {
  
  Z=dataset_XYZ(data)$Z
  Z$covar <- Z[,covariate]
  
  if (subgroup == TRUE) {

    sroc <- ggplot()
    
    if (points == TRUE) {
      sroc <- sroc + geom_point(data = Z,
                                aes(x = 1-spec,
                                    y = sens,
                                    text = paste('ID:', id, '\nSize:', tp+fp+tn+fn, '\n1-Specificity:', round(1-spec,3), '\nSensitivity:' , round(sens,3)),
                                    shape = factor(covar),
                                    size = tp+fp+tn+fn),
                                color = "black")
      
    }
    if (summary_point == TRUE) {
      sroc <- sroc + geom_point(data = subgroup_sroc_data(data, covariate)$sum_point,
                                aes(x = specif,
                                    y = sensit,
                                    text = paste(covar, 'summary point:', '\n1-Specificity:', round((specif),3), '\nSensitivity:' , round(sensit,3)),
                                    color = factor(covar)), 
                                shape = 15, # shape 15 is a square.
                                size = 2.5) + guides(color = guide_legend(title = "Summary points", override.aes = list(linetype = NA), order = 1))
    }
    if (ellipse_pred == TRUE) {
      sroc <- sroc + geom_path(data = subgroup_sroc_data(data, covariate)$ellipse_pred,
                               aes(x = specificity,
                                   y = sensitivity,
                                   text = paste(covar, 'prediction ellipse'),
                                   color = factor(covar), 
                                   linetype = "Prediction ellipse"),
                               size = 0.5)
    }
    if (ellipse_conf == TRUE) {
      sroc <- sroc + geom_path(data = subgroup_sroc_data(data, covariate)$ellipse_conf,
                               aes(x = spec,
                                   y = sens,
                                   text = paste(covar, 'confidence ellipse'),
                                   color = factor(covar), 
                                   linetype = "Confidence ellipse"),
                               size = 0.5)
    }
    if (curve == TRUE) {
      sroc <- sroc + geom_path(data = subgroup_sroc_data(data, covariate)$curv,
                               aes(x = 1-z,
                                   y = curva,
                                   text = paste(covar, 'ROC curve'),
                                   color = factor(covar), 
                                   linetype = "ROC curve"),
                               size = 0.5)
    }
    
    if (summary_point == FALSE) {
      sroc <- sroc + guides(color = guide_legend(title = "Categories", override.aes = list(linetype = "solid"), order = 1))
    }
    
    if (ellipse_pred == TRUE & ellipse_conf == FALSE & curve == FALSE) {
      sroc <- sroc + scale_linetype_manual("Curves", values = c("Prediction ellipse" = "dashed"))
    }
    if (ellipse_pred == FALSE & ellipse_conf == TRUE & curve == FALSE) {
      sroc <- sroc + scale_linetype_manual("Curves", values = c("Confidence ellipse" = "dotted"))
    }
    if (ellipse_pred == FALSE & ellipse_conf == FALSE & curve == TRUE) {
      sroc <- sroc + scale_linetype_manual("Curves", values = c("ROC curve" = "solid"))
    }
    if (ellipse_pred == TRUE & ellipse_conf == TRUE & curve == FALSE) {
      sroc <- sroc + scale_linetype_manual("Curves", values = c("Prediction ellipse" = "dashed", "Confidence ellipse" = "dotted"))
    }
    if (ellipse_pred == TRUE & ellipse_conf == FALSE & curve == TRUE) {
      sroc <- sroc + scale_linetype_manual("Curves", values = c("Prediction ellipse" = "dashed", "ROC curve" = "solid"))
    }
    if (ellipse_pred == FALSE & ellipse_conf == TRUE & curve == TRUE) {
      sroc <- sroc + scale_linetype_manual("Curves", values = c("Confidence ellipse" = "dotted", "ROC curve" = "solid"))
    }
    if (ellipse_pred == TRUE & ellipse_conf == TRUE & curve == TRUE) {
      sroc <- sroc + scale_linetype_manual("Curves", values = c("Prediction ellipse" = "dashed", "Confidence ellipse" = "dotted", "ROC curve" = "solid"))
    }
    
    
    sroc <- sroc + xlim(0,1) + ylim(0,1) + # axes limits
      theme_light() + # to set the background as white
      labs(x = "1-Specificity", y = "Sensitivity", title = "SROC curve") + # axes and graph title
      theme(plot.title = element_text(hjust = 0.5), # to adjust the title position 
            legend.background = element_rect(fill="white"), # background color of legend
            legend.spacing.y = unit(1, 'mm'), # spacing between legend items
            legend.margin= margin(2, unit="mm")) + # spacing between legends
      guides(shape = guide_legend(title = "Studies"), order = 3) +
      guides(size = FALSE) + 
      scale_shape_manual(values=c(1, 2, 5, 6))
  }
  
  else if (subgroup == FALSE) {
    
    X_data <- BRMA_sroc_data(data)$X
    sum_point_data <- BRMA_sroc_data(data)$sum_point
    ellipse_pred_data <- BRMA_sroc_data(data)$ellipse_pred
    ellipse_conf_data <- BRMA_sroc_data(data)$ellipse_conf
    curv_data <- BRMA_sroc_data(data)$curv
    
    sroc <- ggplot()
    
    if (points == TRUE) {
      sroc <- sroc + geom_point(data = X_data %>% mutate(x = 1 - (tn / (tn + fp)), y = tp / (tp + fn)),
                                aes(x = x,
                                    y = y,
                                    text = paste('ID:', id, '\nSize:', tp+fp+tn+fn, '\n1-Specificity:', round(x,3), '\nSensitivity:' , round(y,3)),
                                    shape = "Studies",
                                    size = tp+fp+tn+fn), # size depends on number of observations of the study),
                                color = "darkgrey") 
      
    }
    if (summary_point == TRUE) {
      sroc <- sroc + geom_point(data = sum_point_data,
                                aes(x = X1...specif,
                                    y = sensit,
                                    text = paste('Summary point', '\n1-Specificity:', round((X1...specif),3), '\nSensitivity:' , round(sensit,3)),
                                    shape = "Summary point"),
                                color = "black", 
                                size = 2.5) 
    }
    if (ellipse_pred == TRUE) {
      sroc <- sroc + geom_path(data = ellipse_pred_data,
                               aes(x = specificity,
                                   y = sensitivity,
                                   text = 'Prediction ellipse',
                                   linetype = "Prediction ellipse"),
                               color = "black",
                               size = 0.5)
    }
    if (ellipse_conf == TRUE) {
      sroc <- sroc + geom_path(data = ellipse_conf_data,
                               aes(x = spec,
                                   y = sens,
                                   text = 'Confidence ellipse',
                                   linetype = "Confidence ellipse"),
                               color = "black",
                               size = 0.5)
    }
    if (curve == TRUE) {
      sroc <- sroc + geom_path(data = curv_data,
                               aes(x = 1- (curv_data$z),
                                   y = curv_data$curva,
                                   text = 'ROC curve',
                                   linetype = "ROC curve"),
                               color = "black",
                               size = 0.5) 
    }
    
    if (points == TRUE & summary_point == FALSE) {
      sroc <- sroc + scale_shape_manual(values = c("Studies"=1))
    }
    if (summary_point == TRUE & points == FALSE) {
      sroc <- sroc + scale_shape_manual(values = c("Summary point"=15))
    }
    if (points == TRUE & summary_point == TRUE) {
      sroc <- sroc + scale_shape_manual(values = c("Studies"=1, "Summary point"=15))
    }
    
    if (ellipse_pred == TRUE & ellipse_conf == FALSE & curve == FALSE) {
      sroc <- sroc + scale_linetype_manual(values = c("Prediction ellipse" = "dashed"))
    }
    if (ellipse_pred == FALSE & ellipse_conf == TRUE & curve == FALSE) {
      sroc <- sroc + scale_linetype_manual(values = c("Confidence ellipse" = "dotted"))
    }
    if (ellipse_pred == FALSE & ellipse_conf == FALSE & curve == TRUE) {
      sroc <- sroc + scale_linetype_manual(values = c("ROC curve" = "solid"))
    }
    if (ellipse_pred == TRUE & ellipse_conf == TRUE & curve == FALSE) {
      sroc <- sroc + scale_linetype_manual(values = c("Prediction ellipse" = "dashed", "Confidence ellipse" = "dotted"))
    }
    if (ellipse_pred == TRUE & ellipse_conf == FALSE & curve == TRUE) {
      sroc <- sroc + scale_linetype_manual(values = c("Prediction ellipse" = "dashed", "ROC curve" = "solid"))
    }
    if (ellipse_pred == FALSE & ellipse_conf == TRUE & curve == TRUE) {
      sroc <- sroc + scale_linetype_manual(values = c("Confidence ellipse" = "dotted", "ROC curve" = "solid"))
    }
    if (ellipse_pred == TRUE & ellipse_conf == TRUE & curve == TRUE) {
      sroc <- sroc + scale_linetype_manual(values = c("Prediction ellipse" = "dashed", "Confidence ellipse" = "dotted", "ROC curve" = "solid"))
    }
    
    sroc <- sroc + xlim(0,1) + ylim(0,1) + # axes limits
      theme_light() + # to set the background as white
      labs(x = "1-Specificity", y = "Sensitivity", title = "SROC curve") + # axes and graph title
      theme(plot.title = element_text(hjust = 0.5), # to adjust the title position 
            legend.title = element_blank(),
            legend.background = element_rect(fill="white"), # background color of legend
            legend.spacing.y = unit(1, 'mm'), # spacing between legend items
            legend.margin= margin(-2, unit="mm")) + # spacing between legends
      guides(shape = guide_legend(order = 1), 
             size = FALSE) # turn off size legend
  }
  
  return(sroc)
  
}

## Summary of findings:

prevalencia <- function(data, model, prevslide, patients){
  
  X = dataset_XYZ(data)$X
  X_uni = dataset_XYZ(data)$X_uni
  
  if (0 %in% X_uni$n1){
    X_uni_sen <- subset(X_uni, X_uni$n1!= 0)
  } else {
    X_uni_sen <- X_uni
  }
  
  if(0 %in% X_uni$n0 ){
    X_uni_spe <- subset(X_uni, X_uni$n0!=0)
  } else {
    X_uni_spe <- X_uni
  }
  
  X_uni_sen<- reshape(as.data.frame(X_uni_sen), direction = "long", varying = list(c("n1", "n0"), c("true1", "true0")), timevar = "sens", times = c(1,0),
                      v.names = c("n", "true"))
  X_uni_spe<- reshape(as.data.frame(X_uni_spe), direction = "long", varying = list(c("n1", "n0"), c("true1", "true0")), timevar = "spec", times = c(0,1),
                      v.names = c("n", "true"))
  
  N <- length(X$tp)
  
  X$n1 <- X$tp+X$fn
  X$n0 <- X$fp+X$tn
  X$true1 <- X$tp
  X$true0 <- X$tn 
  # X$study <- 1:N
  
  ## Perform meta-analysis ## 
  
  if (model == "bivariate"){
    
    MA_Y = BRMA_model_select(data)$MA_biv
    
    ma_Y = summary(MA_Y)
    
    lsens = ma_Y$coeff[1,1]
    lspec = ma_Y$coeff[2,1]
    
    se.lsens = ma_Y$coeff[1,2]
    se.lspec = ma_Y$coeff[2,2] 
  }
  
  else if (model == "univariate"){
    MA.sen <- glmer(formula = cbind(  true , n - true ) ~ 0 + sens + (0 + sens|study), data = X_uni_sen, family = binomial, 
                    nAGQ = 1, verbose = 0)
    ma_sen=summary(MA.sen)
    MA.spe <- glmer(formula = cbind(  true , n - true ) ~ 0 + spec + (0 + spec|study), data = X_uni_spe, family = binomial, 
                    nAGQ = 1, verbose = 0)
    ma_spe=summary(MA.spe)
    
    # logit sens and logit spec
    lsens = ma_sen$coeff[1,1]
    lspec = ma_spe$coeff[1,1]
    
    
    se.lsens = ma_sen$coeff[1,2]
    se.lspec = ma_spe$coeff[1,2]
  }
  
  # Sensitivity and specificity with 95% CI's 
  Sens <- plogis(lsens)
  Sens_lci <- plogis(lsens-qnorm(0.975)*se.lsens)
  Sens_uci <- plogis(lsens+qnorm(0.975)*se.lsens)
  Spec <- plogis(lspec)
  Spec_lci <- plogis(lspec-qnorm(0.975)*se.lspec)
  Spec_uci <- plogis(lspec+qnorm(0.975)*se.lspec)
  
  # Calculate TP, FP, FN and TN based on sensitivity, specificity and inputted prevalence 
  Diseased <-(prevslide / 100) * patients # assuming population of 1000 
  NonDiseased <- patients - Diseased 
  
  TP <- round(Sens*Diseased)
  TN <- round(Spec*NonDiseased)
  FN <- round((1-Sens)*Diseased)
  FP <- round((1-Spec)*NonDiseased)
  Pos <- TP + FP # number of positive results
  Neg <- TN + FN # number of negative results
  
  # Calculate for lci and uci 
  TP_lci <- round(Sens_lci*Diseased)
  TP_uci <- round(Sens_uci*Diseased)
  
  TN_lci <- round(Spec_lci*NonDiseased)
  TN_uci <- round(Spec_uci*NonDiseased)
  
  # Calculations of lci and uci specifically for FN and FP differ slightly
  # the lci is used to calculate the uci and vice versa due to the mapping of sens/spec and 1-sens/1-spec
  FN_uci <- round((1-Sens_lci)*Diseased)
  FP_uci <- round((1-Spec_lci)*NonDiseased) 
  
  FN_lci <- round((1-Sens_uci)*Diseased)
  FP_lci <- round((1-Spec_uci)*NonDiseased) 
  
  Pos_lci <- TP_lci  + FP_lci  # number of positive results
  Neg_lci <- TN_lci  + FN_lci  # number of negative results
  
  Pos_uci <- TP_uci  + FP_uci  # number of positive results
  Neg_uci <- TN_uci  + FN_uci  # number of negative results
  
  Diseased_lci <- TP_lci + FN_lci
  NonDiseased_lci <- TN_lci + FP_lci
  
  Diseased_uci <- TP_uci + FN_uci
  NonDiseased_uci <- TN_uci + FP_uci
  
  
  Diseased <- grobTree(textGrob("Diseased", x=0.36,  y=0.83, hjust=0.5,
                                gp=gpar(col="white", fontsize=16, fontface="bold")))
  NonDiseased <- grobTree(textGrob("Non-Diseased", x=0.7,  y=0.83, hjust=0.5,
                                   gp=gpar(col="white", fontsize=16, fontface="bold")))
  PosResult <- grobTree(textGrob("Positive result", x=0.16,  y=0.63, vjust=0.5, rot=90,
                                 gp=gpar(col="white", fontsize=16, fontface="bold")))
  NegResult <- grobTree(textGrob("Negative result", x=0.16,  y=0.3, vjust=0.5, rot=90,
                                 gp=gpar(col="white", fontsize=16, fontface="bold")))
  
  
  TruePos <- grobTree(textGrob("True positive", x=0.23,  y=0.74, hjust=0,
                               gp=gpar(col="black", fontsize=15)))
  TruePosCom <- grobTree(textGrob("(correctly classified \n as diseased)", x=0.23,  y=0.685, hjust=0,
                                  gp=gpar(col="gray34", fontsize=12, fontface="italic")))
  TruePosBox <- roundrectGrob(x=0.36, y=0.63, width=0.3, height=0.3,
                              r=unit(2, "mm"), gp = gpar(fill="white", col="white"))
  TPval <- textbox_grob(paste0("<span style='color:#358e1c;font-size:16pt;'>**",TP,"**</span>", "<br><span style='color:#358e1c;'>(", TP_lci, " - ", TP_uci, ")</span>"),  
                        x = 0.36, y = 0.57, halign = 0.5, valign = 0.5,
                        width = 0.14+0.01*nchar(TP), height = 0.12,
                        box_gp = gpar(fill = "#b4eda4", col = "#b4eda4"),
                        padding = unit(c(3, 3, 3, 3), "pt"),
                        margin = unit(c(5, 5, 5, 5), "pt"), 
                        r=unit(6, "mm"))
  
  
  FalsePos <- grobTree(textGrob("False Positive", x=0.57,  y=0.74, hjust=0,
                                gp=gpar(col="black", fontsize=15)))
  FalsePosCom <- grobTree(textGrob("(incorrectly classified \n as diseased)", x=0.57,  y=0.685, hjust=0,
                                   gp=gpar(col="gray34", fontsize=12, fontface="italic")))
  FalsePosBox <- roundrectGrob(x=0.7, y=0.63, width=0.3, height=0.3,
                               r=unit(2, "mm"), gp = gpar(fill="white", col="white"))
  FPval <- textbox_grob(paste0("<span style='color:#8e1c35;font-size:16pt;'>**",FP,"**</span>", "<br><span style='color:#8e1c35;'>(", FP_lci, " - ", FP_uci, ")</span>"), 
                        x = 0.7, y = 0.57, halign = 0.5, valign = 0.5,
                        width = 0.14+0.01*nchar(FP), height = 0.12,
                        box_gp = gpar(fill = "#eda4b4", col = "#eda4b4"),
                        padding = unit(c(3, 3, 3, 3), "pt"),
                        margin = unit(c(5, 5, 5, 5), "pt"), 
                        r=unit(6, "mm"))
  
  
  FalseNeg <- grobTree(textGrob("False Negative", x=0.23,  y=0.41, hjust=0,
                                gp=gpar(col="black", fontsize=15)))
  FalseNegCom <- grobTree(textGrob("(incorrectly classified \n as non-diseased)", x=0.23,  y=0.355, hjust=0,
                                   gp=gpar(col="gray34", fontsize=12, fontface="italic")))
  FalseNegBox <- roundrectGrob(x=0.36, y=0.3, width=0.3, height=0.3,
                               r=unit(2, "mm"), gp = gpar(fill="white", col="white"))
  FNval <- textbox_grob(paste0("<span style='color:#8e1c35;font-size:16pt;'>**",FN,"**</span>", "<br><span style='color:#8e1c35;'>(", FN_lci, " - ", FN_uci, ")</span>"), 
                        x = 0.36, y = 0.24, halign = 0.5, valign = 0.5,
                        width = 0.14+0.01*nchar(FN), height = 0.12,
                        box_gp = gpar(fill = "#eda4b4", col = "#eda4b4"),
                        padding = unit(c(3, 3, 3, 3), "pt"),
                        margin = unit(c(5, 5, 5, 5), "pt"), 
                        r=unit(6, "mm"))
  
  
  TrueNeg <- grobTree(textGrob("True Negative", x=0.57,  y=0.41, hjust=0,
                               gp=gpar(col="black", fontsize=15)))
  TrueNegCom <- grobTree(textGrob("(correctly classified \n as non-diseased)", x=0.57,  y=0.355, hjust=0,
                                  gp=gpar(col="gray34", fontsize=12, fontface="italic")))
  TrueNegBox <- roundrectGrob(x=0.7, y=0.3, width=0.3, height=0.3,
                              r=unit(2, "mm"), gp = gpar(fill="white", col="white"))
  TNval <- textbox_grob(paste0("<span style='color:#358e1c;font-size:16pt;'>**",TN,"**</span>", "<br><span style='color:#358e1c;'>(", TN_lci, " - ", TN_uci, ")</span>"),  
                        x = 0.7, y = 0.24, halign = 0.5, valign = 0.5,
                        width = 0.14+0.01*nchar(TN), height = 0.12,
                        box_gp = gpar(fill = "#b4eda4", col = "#b4eda4"),
                        padding = unit(c(3, 3, 3, 3), "pt"),
                        margin = unit(c(5, 5, 5, 5), "pt"), 
                        r=unit(6, "mm"))
  
  
  Back <- roundrectGrob(x=0.5, y=0.5, width=0.77, height=0.77,
                        r=unit(5, "mm"), gp = gpar(fill="#235779", col="#235779", alpha=0.5))
  Info1 <- grobTree(textGrob(paste0("Prevalence: ", prevslide, "%\nCohort: ", patients), 
                             x=0.13,  y=0.935, hjust=0,
                             gp=gpar(col="black", fontsize=13, font_face="bold")))
  Info2 <- grobTree(textGrob(paste0("Sensitivity: ", round(Sens,3), "\nSpecificity: ", round(Spec,3)), 
                             x=0.715,  y=0.935, hjust=0,
                             gp=gpar(col="black", fontsize=13, font_face="bold")))
  
  
  prevalplot <- ggplot() +xlim(0, 0.8) +ylim(0, 0.85) + geom_blank() + theme_void() + theme(plot.margin = margin(.8,.8,.8,.8, "cm")) +
                annotation_custom(Back)+
                annotation_custom(Info1) +
                annotation_custom(Info2) +
                annotation_custom(TruePosBox) +
                annotation_custom(FalsePosBox) +
                annotation_custom(FalseNegBox) +
                annotation_custom(TrueNegBox) +
                annotation_custom(Diseased) +
                annotation_custom(NonDiseased) +
                annotation_custom(PosResult) +
                annotation_custom(NegResult) +
                annotation_custom(TruePos) +
                annotation_custom(TruePosCom) +
                annotation_custom(TPval) +
                annotation_custom(FalsePos) +
                annotation_custom(FalsePosCom) +
                annotation_custom(FPval) +
                annotation_custom(FalseNeg) +
                annotation_custom(FalseNegCom) +
                annotation_custom(FNval) +
                annotation_custom(TrueNeg) +
                annotation_custom(TrueNegCom) +
                annotation_custom(TNval) 
  
  return(prevalplot)
  
}

