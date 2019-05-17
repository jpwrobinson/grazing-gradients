
## simple version of mmi_tvalue which does not make model predictions

mmi_tvalue_simp<-function(M_FULL, dataset, indicator, t.subset=FALSE){

    library(MuMIn)
    library(piecewiseSEM)
    library(car)

    # Arguments:
    # dataset = dataset containing y and all x covariates. should be scaled and centered (mean = 0, sd = 1)
    # indicator = y variable of interest (character)


  #--------------------------------------------------------------------------------#
  	# Build global model and check for collinearity, and print model diagnostic plots#
  #--------------------------------------------------------------------------------#

  ## check for collinearity
  print('Checking collinearity. VIF values are:')
  print(vif(M_FULL))

  par(mfrow=c(2,2))
  hist(dataset[,indicator], main=paste('Hist of', indicator, sep=' '))
  plot(fitted(M_FULL), dataset[,indicator], main=paste('Fitted vs. obs', indicator, sep=' '))
  plot(resid(M_FULL), main=paste('Residuals of', indicator, sep=' '))
  hist(resid(M_FULL), main=paste('Hist of residuals of', indicator, sep=' '))
  # dev.off()
  #--------------------------------------------------------------------------------#
  			# Dredge for delta < 7 top models #
  #--------------------------------------------------------------------------------#

  M_FULL_SET<-dredge(M_FULL, rank="AICc")
  print(head(M_FULL_SET))

  ## extract models with AIC < 7
  if(t.subset == TRUE){
    M_FULL_SET<-subset(M_FULL_SET, delta<7)}

  #--------------------------------------------------------------------------------#
  # Model average absolute value of t-statistics for measure of variable importance ACROSS ALL MODELS #
  #--------------------------------------------------------------------------------#
  var.imp <- as.data.frame(matrix(NA, nrow = (ncol(M_FULL_SET) - 6), ncol = 4))
  colnames(var.imp) <- c("Var", "RI.t.abs", "RI.t.ratio",  "var.t")

  # Loop through all the variables in the model
  for (i in 1:(ncol(M_FULL_SET) - 6)){
    var.temp <- colnames(M_FULL_SET)[i+1]
    var.imp[i,"Var"] <- var.temp
    
    #Subset for only models for a given predictor, don't recalulate the model weights or deltaAICcs
    M_FULL_SET_x <- M_FULL_SET[i = !is.na(M_FULL_SET[,paste(var.temp)]), j = 1:ncol(M_FULL_SET), recalc.weights =FALSE, recalc.delta = FALSE]
    
    #Pull out the t-statistics for the given variables for each model in which it appears
    #Put all values in a data frame
    t.values <- as.data.frame(matrix(NA, nrow = nrow(M_FULL_SET_x), ncol = 4))
    colnames(t.values) <- c('varx',  "model.wt", 'imp.t.abs', "imp.t.ratio")
    for (l in 1:nrow(M_FULL_SET_x)){
      #put in variable name
      t.values[l,"varx"] <- var.temp
      
      #Pull out one model for the set that includes the variable of interest
      temp_mod <- get.models(M_FULL_SET, which(!is.na(M_FULL_SET[,paste(var.temp)]))[l])[[1]]
      
      #Pull out the model weight for this model
      wt.temp <- M_FULL_SET$weight[which(!is.na(M_FULL_SET[,paste(var.temp)]))][l]
      t.values[l,"model.wt"] <- wt.temp

      #abs value of t-statistics for variable of interest
      RI.x.temp <- abs(coef(summary(temp_mod))[paste(var.temp),3])
      t.values[l, "imp.t.abs"] <-  RI.x.temp 
      
      #ratios of t-statistics for variable of interest
      RI.x.temp <- abs(coef(summary(temp_mod))[paste(var.temp),3])/max(abs(coef(summary(temp_mod))[-1,3]))
      t.values[l, "imp.t.ratio"] <-  RI.x.temp 
    }
    #AICc weighted average of t-statistics for variable of interest
    avgRI.x.abs <- sum(apply(as.matrix(t.values[,2:4]), 1, function(x) x[2]*x[1]))
    var.imp[i,"RI.t.abs"] <- avgRI.x.abs
    
     #AICc weighted average ratio of t-statistics for variable of interest
    avgRI.x <- sum(apply(as.matrix(t.values[,2:4]), 1, function(x) x[3]*x[1]))
    var.imp[i,"RI.t.ratio"] <- avgRI.x

    ### variance
    varT.x<-sum(t.values[,2]*((t.values[,3]-avgRI.x.abs)^2))
    var.imp[i, "var.t"] <- varT.x
  }

  return(var.imp)
}