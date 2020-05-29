##########################################################################
## This file provides the function to run the actual experiment (for a given BBOB function, instance and dimension)
##########################################################################
## Careful note: to run this succesfully, make sure the right python version is used.
## Here, we made use of a BBOB version still depending on python 2, on a windows system
## so the "use_python" call below carefully selects Anaconda2
## Of course, this requires that the cocoex module (bbob) is installed in Anaconda2.

#devtools::install("bbobr") ## provided as a tar.gz. requires that bbob python version is installed and accessible to R
library(bbobr) ## see line above
## select the correct python version
use_python("C:\\Users\\Martin\\Anaconda2") #
#use_python("/usr/bin/python",required = T) #NOTE. this is system dependent! 

library(SPOT) ## please install release v20200429 from github: https://github.com/bartzbeielstein/SPOT/tree/v20200429
library(nloptr) ## install from CRAN, version 1.2.2.1

expFun <- function(
  funID = 1, # 1-24
  nDim = 2, # 2,3,5,10,20
  instID = 1, # 1-15
  nt = 50
){
  #devtools::install("bbobr")
  print(funID)
  print(nDim)
  print(instID)
  print(nt)
  
  nopt <- 1000*(nDim) # number of optimization evaluations (budget for solver)
  ntrain <- nt*(nDim) # number of training samples for the model
  
  solverseed <- instID
  simuseed <- instID
  trainseed <- instID
  datarunseed <- instID*10 #seed for data generation run
    
  Ncos <- 100*(nDim)
  psizedatarun <- 20*(nDim) #popsize DE solver, for data generation only
  psize <- 10*(nDim) #popsize DE solver for benchmark
  
  lowth <- 1e-6
  upth <- 1e12
  
  #
  #
  #
  #
  #
  #lambda!
  la <- F
  laup <- 0
  lalo <- -16
  #
  #
  #
  #
  #
  dirname <- paste("bbob-F",funID,"D",nDim,"I",instID,"N",nt,sep="-")
  res <- getProblem(dime = nDim,inst = instID,func = funID)
  lower <- res$lower
  upper <- res$upper
  problem <- res$problem
  
  ###
  ### 1) Define a solver that you want to use.
  ### The solver has to accept a function, lower and upper bounds as well as a parameter list.
  ### However, internally you can also ignore these parameters if you dont need them for your solver...
  ###
  solver <- function(fun,lower,upper,solverParameterList){
    set.seed(datarunseed)
    fn <- function(x){
      apply(x,1,fun)
    }
    res <- optimDE(# x = runif(length(lower),lower,upper),
      fun = fn,lower = lower, upper = upper,
      control = list(funEvals=nopt,populationSize=psizedatarun))
  }
  ###
  ### 2) To run your solver on some of the BBOB functions you use the 'runCOCO' command.
  ### If you are not using batches it is easiest to always leave current_batch and number_of_batches = 1
  ### Fill in dimensionality, instances and functions as well as your solver name.
  ### Dimensionality: 2,3,5,10 or 20
  ### Instances: 1-15
  ### Functions 1-24
  ###
  
  unlink(dirname,recursive = T)
  # via wrapper, for my own logging
  wrappedFun <- wrapToBBOBFunction(problem, ## Your function
                                   functionID = funID, ## Any ID that you want to assign >24 makes sense to not mix with BBOB
                                   nDim = nDim, ## Dimensionality of your function
                                   instanceID = instID, ## Any ID that you want
                                   knownFunctionOptimum = 0, ## If you know your optimum specify it here, otherwise 0
                                   algoID = "DE-groundtruth", ## Name of your solver
                                   experimentPath = dirname) ## Path you want to save the results to
  res <- solver(wrappedFun, lower, upper, list())
  df <- readBBOB(dirname) #load results
  df$model <- "groundtruth"
  df$algorithm <- "DE"		
  ### 
  ### 4a) extract data for model
  ###
  y <- df$measuredFitness
  x <- df[,which(str_detect(names(df),"x"))[1:nDim]]
  dups <- duplicated(x)
  y <- matrix(y[!dups],,1)
  x <- x[!dups,]
  # limit: first ntrain points only. due to training costs.
  y <- y[1:min(ntrain,nrow(y)),,drop=F]
  x <- x[1:min(ntrain,nrow(x)),,drop=F]
  
  ### 
  ### 4) train model
  ###
  set.seed(trainseed)
  fit <- buildKriging(x,y,control=list(
    useLambda=la,
    lambdaUpper = laup,
    lambdaLower = lalo,
    reinterpolate = T,
    thetaUpper=upth,
    thetaLower=lowth
  ))
  #print(fit2)
  #fit2$like
  
  #######################################################################
  #######################################################################  
  #######################################################################  
  #######################################################################  
  ###
  ### Repeat run, with different seed (same seed as competitors)
  ###
  solver <- function(fun,lower,upper,solverParameterList){
    set.seed(solverseed)
    fn <- function(x){
      apply(x,1,fun)
    }
    res <- optimDE(
      fun = fn,lower = lower, upper = upper,
      control = list(funEvals=nopt,populationSize=psize))

  }
  solverRS <- function(fun,lower,upper,solverParameterList){
    set.seed(solverseed)
    k <- length(lower)
    x <- matrix(runif(nopt*k,lower,upper),nopt,k,byrow=T)
    y <- apply(x,1,fun)
    return(min(y))
  }
  solverNM <- function(fun,lower,upper,solverParameterList){
    set.seed(solverseed)
    evals <- 0
    while(evals < nopt){
      budget <- nopt-evals
      k <- length(lower)
      x0 <- runif(k,lower,upper)
      cntrl <- list(maxeval=budget)
      res <- neldermead(x0=x0,fn=fun,lower=lower,upper=upper,control=cntrl)
      evals <- evals + res$iter
    }
    return(1)
  }
  unlink(dirname,recursive = T)
  # via wrapper, for my own logging
  wrappedFun <- wrapToBBOBFunction(problem, ## Your function
                                   functionID = funID, ## Any ID that you want to assign >24 makes sense to not mix with BBOB
                                   nDim = nDim, ## Dimensionality of your function
                                   instanceID = instID, ## Any ID that you want
                                   knownFunctionOptimum = 0, ## If you know your optimum specify it here, otherwise 0
                                   algoID = "DE-groundtruth", ## Name of your solver
                                   experimentPath = dirname) ## Path you want to save the results to
  res <- solver(wrappedFun, lower, upper, list())
  df1 <- readBBOB(dirname) #load results
  df1$model <- "groundtruth"
  df1$algorithm <- "DE"		
  
  ### run RS
  unlink(dirname,recursive = T)
  wrappedFun <- wrapToBBOBFunction(problem, ## Your function
                                   functionID = funID, ## Any ID that you want to assign >24 makes sense to not mix with BBOB
                                   nDim = nDim, ## Dimensionality of your function
                                   instanceID = instID, ## Any ID that you want
                                   knownFunctionOptimum = 0, ## If you know your optimum specify it here, otherwise 0
                                   algoID = "RS-groundtruth", ## Name of your solver
                                   experimentPath = dirname) ## Path you want to save the results to
  res <- solverRS(wrappedFun, lower, upper, list())
  df1RS <- readBBOB(dirname)
  df1RS$model <- "groundtruth"
  df1RS$algorithm <- "RS"		
  
  ### run NM
  unlink(dirname,recursive = T)
  wrappedFun <- wrapToBBOBFunction(problem, ## Your function
                                   functionID = funID, ## Any ID that you want to assign >24 makes sense to not mix with BBOB
                                   nDim = nDim, ## Dimensionality of your function
                                   instanceID = instID, ## Any ID that you want
                                   knownFunctionOptimum = 0, ## If you know your optimum specify it here, otherwise 0
                                   algoID = "NM-groundtruth", ## Name of your solver
                                   experimentPath = dirname) ## Path you want to save the results to
  res <- solverNM(wrappedFun, lower, upper, list())
  df1NM <- readBBOB(dirname)
  df1NM$model <- "groundtruth"
  df1NM$algorithm <- "NM"		 
  #######################################################################   
  ###
  ### 5) benchmnark with simulation
  ###
  funs <- simulateFunction(object=fit, method="spectral", nsim=1, Ncos=Ncos,
                            conditionalSimulation=T,
                            seed=simuseed)
  sfun23 <- function(x){
    xx <- matrix(x,1)
    funs[[1]](xx)
  }
  force(sfun23)
  
  ## clean old folder, because otherwise the current read function will fail.
  unlink(dirname,recursive = T)
  wrappedFun <- wrapToBBOBFunction(sfun23, ## Your function
                                   functionID = funID, ## Any ID that you want to assign >24 makes sense to not mix with BBOB
                                   nDim = nDim, ## Dimensionality of your function
                                   instanceID = instID, ## Any ID that you want
                                   knownFunctionOptimum = 0, ## If you know your optimum specify it here, otherwise 0
                                   algoID = "DE-simulation", ## Name of your solver
                                   experimentPath = dirname) ## Path you want to save the results to
  res <- solver(wrappedFun, lower, upper, list())
  df4 <- readBBOB(dirname)
  df4$model <- "simulation"
  df4$algorithm <- "DE"		
  
  ### run RS
  unlink(dirname,recursive = T)
  wrappedFun <- wrapToBBOBFunction(sfun23, ## Your function
                                   functionID = funID, ## Any ID that you want to assign >24 makes sense to not mix with BBOB
                                   nDim = nDim, ## Dimensionality of your function
                                   instanceID = instID, ## Any ID that you want
                                   knownFunctionOptimum = 0, ## If you know your optimum specify it here, otherwise 0
                                   algoID = "RS-simulation", ## Name of your solver
                                   experimentPath = dirname) ## Path you want to save the results to
  res <- solverRS(wrappedFun, lower, upper, list())
  df4RS <- readBBOB(dirname)
  df4RS$model <- "simulation"
  df4RS$algorithm <- "RS"		
  
  ### run NM
  unlink(dirname,recursive = T)
  wrappedFun <- wrapToBBOBFunction(sfun23, ## Your function
                                   functionID = funID, ## Any ID that you want to assign >24 makes sense to not mix with BBOB
                                   nDim = nDim, ## Dimensionality of your function
                                   instanceID = instID, ## Any ID that you want
                                   knownFunctionOptimum = 0, ## If you know your optimum specify it here, otherwise 0
                                   algoID = "NM-simulation", ## Name of your solver
                                   experimentPath = dirname) ## Path you want to save the results to
  res <- solverNM(wrappedFun, lower, upper, list())
  df4NM <- readBBOB(dirname)
  df4NM$model <- "simulation"
  df4NM$algorithm <- "NM"		
  
  
  
  ###
  ### 6) benchmnark with prediction
  ###
  
  sfun23p <- function(x){
    xx <- matrix(x,1)
    predict(fit,xx)$y 
  }
  
  force(sfun23p)
  ## clean old folder, because otherwise the current read function will fail.
  unlink(dirname,recursive = T)
  wrappedFun <- wrapToBBOBFunction(sfun23p, ## Your function
                                   functionID = funID, ## Any ID that you want to assign >24 makes sense to not mix with BBOB
                                   nDim = nDim, ## Dimensionality of your function
                                   instanceID = instID, ## Any ID that you want
                                   knownFunctionOptimum = 0, ## If you know your optimum specify it here, otherwise 0
                                   algoID = "DE-estimation", ## Name of your solver
                                   experimentPath = dirname) ## Path you want to save the results to
  res <- solver(wrappedFun, lower, upper, list())
  df5 <- readBBOB(dirname)
  df5$model <- "estimation"
  df5$algorithm <- "DE"
  
  ### run RS
  unlink(dirname,recursive = T)
  wrappedFun <- wrapToBBOBFunction(sfun23p, ## Your function
                                   functionID = funID, ## Any ID that you want to assign >24 makes sense to not mix with BBOB
                                   nDim = nDim, ## Dimensionality of your function
                                   instanceID = instID, ## Any ID that you want
                                   knownFunctionOptimum = 0, ## If you know your optimum specify it here, otherwise 0
                                   algoID = "RS-estimation", ## Name of your solver
                                   experimentPath = dirname) ## Path you want to save the results to
  res <- solverRS(wrappedFun, lower, upper, list())
  df5RS <- readBBOB(dirname)
  df5RS$model <- "estimation"
  df5RS$algorithm <- "RS"
  
  ### run NM
  unlink(dirname,recursive = T)
  wrappedFun <- wrapToBBOBFunction(sfun23p, ## Your function
                                   functionID = funID, ## Any ID that you want to assign >24 makes sense to not mix with BBOB
                                   nDim = nDim, ## Dimensionality of your function
                                   instanceID = instID, ## Any ID that you want
                                   knownFunctionOptimum = 0, ## If you know your optimum specify it here, otherwise 0
                                   algoID = "NM-estimation", ## Name of your solver
                                   experimentPath = dirname) ## Path you want to save the results to
  res <- solverNM(wrappedFun, lower, upper, list())
  df5NM <- readBBOB(dirname)
  df5NM$model <- "estimation"
  df5NM$algorithm <- "NM"
  
  
  ###
  ### concatenate results
  ###
    
  res <- rbind(df1,df4,df5,df1RS,df4RS,df5RS,df1NM,df4NM,df5NM)
	
  #remove na column
  res <- res[,apply(res,2,function(x)!all(is.na(x)))]
  
  unlink(dirname,recursive = T) # cleanup
  return(res)
}


