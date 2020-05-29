##########################################################################
## This file is intended for running the experiments distributed, on a pbs scheduled cluster.
## This file expects to be run by the scheduler, where each job receives a different
## PBS_ARRAYID (see line 48)
##########################################################################

## Some utility function for job assignment:
## returns for each value in jobID a row in a matrix with one column for each set in the sets lists
## the values in the matrix indicate the index to choose from each set.
clusterJobID <- function(jobID, nsets, setlengths){
  if(nsets ==1){
    index <- matrix(jobID,length(jobID),1)
  }else{  
    setsize <-  prod(unlist(setlengths[-1]))
    id <- jobID
    index <- matrix(0,length(jobID),nsets)
    for(i in 1:(nsets-1)){
      index[,i] <- ceiling(id/setsize) 
      id <- ((id-1)%%setsize)+1
      setsize <- setsize / setlengths[i+1] 
    }
    index[,i+1] <- id
  }
  return(index)
}


## making sure that lib paths are correct (may need to be adapted depending on setup of cluster)
#print(system("which R"), intern = T)
#.libPaths(c("/home/0/mzaeffer-12523/R/x86_64-pc-linux-gnu-library/3.6", "/opt/software/R/R-3.6.1/lib/R/library"))
#print(.libPaths())

## source function that runs the experiment
source("expFun.R")

## tested bbob function ids
fids <- 1:24
## tested bbob instance ids
iids <- 1:15
## tested dimensions
ndims <- c(2,3,5,10,20)
## combine into setup
sets <- list(fids,iids,ndims)
nsets <- length(sets)  
setlengths <- sapply(sets,length)

## get current job id (from PBS scheduler)
jobID <- as.numeric(Sys.getenv("PBS_ARRAYID"))  

## get parameters from id
index <- clusterJobID(jobID,nsets,setlengths)
fid <- fids[index[,1]] #problem ID
iid <- iids[index[,2]] #instance ID
ndim <- ndims[index[,3]] #dimension

## filename for result
fname <- paste("results/rslts_F_",fid,"_I_",iid,"_D_",ndim,".RData",sep="")
 
## only run if file does not exist yet 
if(!file.exists(fname)){
  res <- expFun(funID = fid,
       instID = iid,
       nDim = ndim,
       nt = 50)
  save(res,file=fname) #save result
}
gc()
