##########################################################################
## This file summarizes (and subsamples) the results created for each individual experiment run
##########################################################################

library(dplyr)
fids <- 1:24
iids <- 1:15
ndims <- c(2,3,5,10,20)
sets <- list(fids,iids,ndims)
nsets <- length(sets)  
setlengths <- sapply(sets,length)
jobIDs <- 1:prod(setlengths)

source("expFun.R")

df2r <- df3r <- df5r <- df10r <- df20r <- NULL

for(jobID in jobIDs){
	print(jobID)
	## get parameters from id
	index <- clusterJobID(jobID,nsets,setlengths)
	fid <- fids[index[,1]] #problem ID
	iid <- iids[index[,2]] #instance ID
	ndim <- ndims[index[,3]] #dimension

	## filename for result
	fname <- paste("results/rslts_F_",fid,"_I_",iid,"_D_",ndim,".RData",sep="")		
	
	## load
  load(fname)
	## summarize relevant values and scale
  res1 <- res %>% group_by(funID,instance,nDim,model) %>% mutate(bestRanked = rank(bestMeasuredFitness))
  res1 <- res1 %>% group_by(funID,instance,nDim,model) %>% mutate(ranked = rank(measuredFitness))
  res1 <- res1 %>% group_by(funID,instance,nDim,model) %>% mutate(bestScaled = bestMeasuredFitness-min(bestMeasuredFitness)+1)
  res1 <- res1 %>% group_by(funID,instance,nDim,model) %>% mutate(bestScaled = bestScaled / max(bestScaled))
	## subsample to reduce size
  indices <- unique(round(10^seq(0,log10(as.numeric(res1$nDim[1])*1000),length.out=100)))
  sel <- res1$fEvaluations %in% indices
  res1 <- res1[sel,]
  
  ## store  
	if(ndim == 2){    
		df2r <- rbind(df2r,res1)
	}else if(ndim == 3){
		df3r <- rbind(df3r,res1)
	}else if(ndim == 5){
		df5r <- rbind(df5r,res1)
  }else if(ndim == 10){
		df10r <- rbind(df10r,res1)
	}else if(ndim == 20){
		df20r <- rbind(df20r,res1)
	}	
}

## save result 
save(df2r,df3r,df5r,df10r,df20r,file="resultsred.RData")

