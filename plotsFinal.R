##########################################################################
## This file uses the summarized results to produce the plots for the paper
##########################################################################

library(dplyr)
library(ggplot2)
require(tidyr)
require(reshape2)
require(grid)
require(gtable) 
load("resultsred.RData")


ddf <- rbind(
  df2r[,c("funID","instance","nDim","fEvaluations","model","algorithm","bestScaled","ranked","bestRanked")],
  df3r[,c("funID","instance","nDim","fEvaluations","model","algorithm","bestScaled","ranked","bestRanked")],
  df5r[,c("funID","instance","nDim","fEvaluations","model","algorithm","bestScaled","ranked","bestRanked")],
  df10r[,c("funID","instance","nDim","fEvaluations","model","algorithm","bestScaled","ranked","bestRanked")],
  df20r[,c("funID","instance","nDim","fEvaluations","model","algorithm","bestScaled","ranked","bestRanked")]
)
ddf2 <- dcast(ddf,nDim+funID+algorithm+fEvaluations+instance~model,value.var="bestScaled")
ddf2$simulation <- abs(ddf2$simulation-ddf2$groundtruth)
ddf2$estimation <- abs(ddf2$estimation-ddf2$groundtruth)
ddf2$Evaluations <- ddf2$fEvaluations / as.numeric(ddf2$nDim)

df <- melt(ddf2,id.vars = c("nDim","funID","algorithm","Evaluations","instance"),measure.vars = c("simulation","estimation"))
df$Evaluations <- factor(df$Evaluations,levels=sort(unique(df$Evaluations)))
df$F <- factor(df$funID,levels=sort(unique(as.numeric(df$funID))))
df$n <- factor(df$nDim,levels=sort(unique(as.numeric(df$nDim))))

#log scale?
#df$value <- log10(df$value+0.0001) 
#no. problems with scale differences, unreadable.

## compute quartiles for ribbon plot
df <- df %>% group_by(n,F,variable,algorithm,Evaluations) %>%  
  summarise(upper = quantile(value,probs=0.75),
            lower = quantile(value,probs=0.25),
            med = median(value)) 

df$method <- df$variable

#######################
#######################
combineGridAndWrap <- function(wrapPlot, gridPlot, ncols = 5){
  
  ## Create tables out of graphix entities
  gt1 = ggplot_gtable(ggplot_build(wrapPlot))
  gt2 = ggplot_gtable(ggplot_build(gridPlot))
  
  ## Add a column for the right hand side boundary
  gt1 <- gtable_add_cols(gt1, widths=gt1$widths[6], pos = -1)
  
  ## Search for the right hand side strips in the plot and overwrite them with
  ## the ones from the grid plot
  gt1$grobs[grep(paste0('strip-r.+',ncols,'$'), gt1$layout$name)] <- gt2$grobs[grep('strip-r', gt2$layout$name)]

  ## Set all other stips to NULL
  for(i in 1:(ncols-1)){
    ind <- grep(paste0('strip-r.+',i,"$"), gt1$layout$name)
    gt1$grobs[ind] <- NULL
    gt1$layout <- gt1$layout[-ind,]
  }
  gt1$layout[grep('strip-r', gt1$layout$name),]$l <- ncol(gt1)
  
  ## Add Row for the top strips
  gt1 = gtable_add_rows(gt1, heights=gt1$widths[1], pos = -1)
  
  ## Add all top strips from the grid plot into the wrap plot
  panel_id <- gt1$layout[grep('panel-', gt1$layout$name),]
  gt1 = gtable_add_grob(gt1, zeroGrob(), t = 1, l = ncol(gt1), b=nrow(gt1))
  for(i in 1:ncols){
    gt.side = gtable_filter(gt2, paste0('strip-t-',i))
    gt1 = gtable_add_grob(gt1, gt.side, t = 6, l = unique(panel_id$l)[i])
  }
  
  ## Remove whitespace between plots
  gt1$widths[seq(6,length(gt1$widths)-1,5)] <- gt1$widths[2]
  return(gt1)
}

df$Evaluations <- as.numeric(as.character(df$Evaluations))
range(df$Evaluations)

## re-sort labels
df$method <- factor(df$method,levels=c("estimation","simulation"))


## plots
p1 <- ggplot(
    subset(df,algorithm=="DE"),  
    aes(x=Evaluations,y=med,group=method))  + 
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=method), alpha=0.5) +
  geom_line(aes(color=method,linetype=method))+ theme_classic() +
  facet_wrap(F~n,scales = "free_y",ncol=5,strip.position = "right",
             labeller = labeller(n=label_both,F=label_both, .multi_line = F)
  )+ ylab("error")+xlab("evaluations / n")+
  theme(
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    strip.background = element_blank()
  )+ theme(legend.position="top")
p1.grid <- ggplot(
  subset(df,algorithm=="DE"),  
  aes(x=Evaluations,y=med,group=method))  + 
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=method), alpha=0.5) +
  geom_line(aes(color=method,linetype=method))+ theme_classic() +
  facet_grid(F~n,scales = "free_y",
             labeller = labeller(n=label_both,F=label_both, .multi_line = F)
  )+ ylab("error")+xlab("evaluations / n")+
  theme(
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    strip.background = element_blank()
  )+ theme(legend.position="top")

p2 <- ggplot(
  subset(df,algorithm=="RS"),  
  aes(x=Evaluations,y=med,group=method))  + 
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=method), alpha=0.5) +
  geom_line(aes(color=method,linetype=method))+ theme_classic() +
  facet_wrap(F~n,scales = "free_y",ncol=5,strip.position = "right",
             labeller = labeller(n=label_both,F=label_both, .multi_line = F)
  )+ ylab("error")+xlab("evaluations / n")+
  theme(
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    strip.background = element_blank()
  )+ theme(legend.position="top")
p2.grid <- ggplot(
  subset(df,algorithm=="RS"),  
  aes(x=Evaluations,y=med,group=method))  + 
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=method), alpha=0.5) +
  geom_line(aes(color=method,linetype=method))+ theme_classic() +
  facet_grid(F~n,scales = "free_y",
             labeller = labeller(n=label_both,F=label_both, .multi_line = F)
  )+ ylab("error")+xlab("evaluations / n")+
  theme(
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    strip.background = element_blank()
  )+ theme(legend.position="top")
p3 <- ggplot(
  subset(df,algorithm=="NM"), 
  aes(x=Evaluations,y=med,group=method))  + 
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=method), alpha=0.5) +
  geom_line(aes(color=method,linetype=method)) + theme_classic() +
  facet_wrap(F~n,scales = "free_y",ncol=5,strip.position = "right",
             labeller = labeller(n=label_both,F=label_both, .multi_line = F)
             )+ ylab("error")+xlab("evaluations / n")+
  theme(
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    strip.background = element_blank()
  )+ theme(legend.position="top")
p3.grid <- ggplot(
  subset(df,algorithm=="NM"), 
  aes(x=Evaluations,y=med,group=method))  + 
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=method), alpha=0.5) +
  geom_line(aes(color=method,linetype=method)) + theme_classic() +
  facet_grid(F~n,scales = "free_y",
             labeller = labeller(n=label_both,F=label_both, .multi_line = F)
  )+ ylab("error")+xlab("evaluations / n")+
  theme(
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    strip.background = element_blank()
  )+ theme(legend.position="top")

pdf("plots09ribbon.pdf",width=14,height=20)
grid.draw(combineGridAndWrap(p1,p1.grid))
grid.newpage()
grid.draw(combineGridAndWrap(p2,p2.grid))
grid.newpage()
grid.draw(combineGridAndWrap(p3,p3.grid))
#grid.newpage()
#grid.draw(combineGridAndWrap(p1,p1.grid) + 
#          theme(strip.background = element_blank()))
dev.off()


p1 <- ggplot(
  subset(df,algorithm=="DE"&n!="3"&F%in%c(1:3,7,8,15:16,20,21,23,24)),  
  aes(x=Evaluations,y=med,group=method))  + 
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=method), alpha=0.5) +
  geom_line(aes(color=method,linetype=method))+ theme_classic() +
  facet_wrap(F~n,scales = "free_y",ncol=4,strip.position = "right",
             labeller = labeller(n=label_both,F=label_both, .multi_line = F)
  )+ ylab("error")+xlab("evaluations / n")+
  theme(
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    strip.background = element_blank()
  )+ theme(legend.position="top")
p1.grid <- ggplot(
  subset(df,algorithm=="DE"&n!="3"&F%in%c(1:3,7,8,15:16,20,21,23,24)),  
  aes(x=Evaluations,y=med,group=method))  + 
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=method), alpha=0.5) +
  geom_line(aes(color=method,linetype=method))+ theme_classic() +
  facet_grid(F~n,scales = "free_y",
             labeller = labeller(n=label_both,F=label_both, .multi_line = F)
  )+ ylab("error")+xlab("evaluations / n")+
  theme(
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    strip.background = element_blank()
  )+ theme(legend.position="top")

p2 <- ggplot(
  subset(df,algorithm=="RS"&n!="3"&F%in%c(1:3,7,8,15:16,20,21,23,24)),  
  aes(x=Evaluations,y=med,group=method))  + 
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=method), alpha=0.5) +
  geom_line(aes(color=method,linetype=method))+ theme_classic() +
  facet_wrap(F~n,scales = "free_y",ncol=4,strip.position = "right",
             labeller = labeller(n=label_both,F=label_both, .multi_line = F)
  )+ ylab("error")+xlab("evaluations / n")+
  theme(
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    strip.background = element_blank()
  )+ theme(legend.position="top")
p2.grid <- ggplot(
  subset(df,algorithm=="RS"&n!="3"&F%in%c(1:3,7,8,15:16,20,21,23,24)),  
  aes(x=Evaluations,y=med,group=method))  + 
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=method), alpha=0.5) +
  geom_line(aes(color=method,linetype=method))+ theme_classic() +
  facet_grid(F~n,scales = "free_y",
             labeller = labeller(n=label_both,F=label_both, .multi_line = F)
  )+ ylab("error")+xlab("evaluations / n")+
  theme(
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    strip.background = element_blank()
  )+ theme(legend.position="top")
p3 <- ggplot(
  subset(df,algorithm=="NM"&n!="3"&F%in%c(1:3,7,8,15:16,20,21,23,24)), 
  aes(x=Evaluations,y=med,group=method))  + 
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=method), alpha=0.5) +
  geom_line(aes(color=method,linetype=method)) + theme_classic() +
  facet_wrap(F~n,scales = "free_y",ncol=4,strip.position = "right",
             labeller = labeller(n=label_both,F=label_both, .multi_line = F)
  )+ ylab("error")+xlab("evaluations / n")+
  theme(
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    strip.background = element_blank()
  )+ theme(legend.position="top")
p3.grid <- ggplot(
  subset(df,algorithm=="NM"&n!="3"&F%in%c(1:3,7,8,15:16,20,21,23,24)), 
  aes(x=Evaluations,y=med,group=method))  + 
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=method), alpha=0.5) +
  geom_line(aes(color=method,linetype=method)) + theme_classic() +
  facet_grid(F~n,scales = "free_y",
             labeller = labeller(n=label_both,F=label_both, .multi_line = F)
  )+ ylab("error")+xlab("evaluations / n")+
  theme(
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    strip.background = element_blank()
  )+ theme(legend.position="top")

df$Evaluations <- (as.numeric(df$Evaluations))
p4 <- ggplot(
  subset(df,n=="20"&F==21), 
  aes(x=Evaluations,y=med,group=method))  + 
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=method), alpha=0.5) +
  geom_line(aes(color=method,linetype=method)) + theme_classic() +
  facet_wrap(~algorithm,scales = "free_y",ncol=4,strip.position = "top",
             labeller = labeller(n=label_both,F=label_both, .multi_line = F)
  )+ ylab("error")+xlab("evaluations / n")+
  theme(
    #axis.text.x = element_text(angle=30),
    strip.background = element_blank()
  )+ theme(legend.position="top") + 

pdf("plots09ribbonSelected.pdf",width=7,height=8)
grid.draw(combineGridAndWrap(p1,p1.grid, ncols = 4))
grid.newpage()
grid.draw(combineGridAndWrap(p2,p2.grid, ncols = 4))
grid.newpage()
grid.draw(combineGridAndWrap(p3,p3.grid, ncols = 4))
dev.off()

pdf("plots09f21only.pdf",width=7,height=3)
grid.draw(p4)
dev.off()
