#install.packages('survival')
#source("https://bioconductor.org/biocLite.R")
#biocLite("qvalue")

#install.packages(c("survival","survminer","ggplot2"))
library(survival)
library(survminer)
library(ggplot2)
setwd("D:\\Î´ÃüÃûÎÄ¼ş¼Ğ\\FigureYa35 batch_bestSeparation")
svdata <- read.table("tumor.time.txt",header = T,row.names = 1)
head(svdata)

for (ii in 3:ncol(svdata)) {
  sortsv<-svdata[order(svdata[,ii]),]
  pvals<-c()
  hrs<-c()
  for(n in 1:(nrow(sortsv)-1)){
    ssdf<-cbind(sortsv[,1:2],sortsv[,ii],data.frame(gp=rep(c("low","high"),c(n,nrow(sortsv)-n))))
    diff<-survdiff(Surv(futime,fustat)~gp,data=ssdf,rho = 0)
    pv<-pchisq(diff$chisq,length(diff$n)-1,lower.tail=FALSE)
    pvals<-c(pvals,pv)
    hr<-diff$obs[1]*diff$exp[2]/(diff$obs[2]*diff$exp[1])
    hrs<-c(hrs,hr)
  }
  
  fd<-data.frame(Tag=1:(nrow(sortsv)-1),HR=hrs,Pvalue=pvals)
  
  ssdf<-cbind(sortsv[,1:2],sortsv[,ii],
              data.frame(gp=rep(c("low","high"),
                                c(which.min(pvals),nrow(sortsv)-which.min(pvals)))))
  fit<-survfit(Surv(futime,fustat)~gp,data=ssdf)
  
  ggsurvplot(fit, linetype = "strata", 
             conf.int = F, #ä¸ç”»ç½®ä¿¡åŒºé—´
             pval = FALSE,
             palette = c("#D95F02","#1B9E77"),
             legend.title = paste0(colnames(svdata)[ii],", HR = ",round(hrs[which.min(pvals)],3),", P = ",round(min(pvals),3)),
             legend=c(0.35,0.15),
            
             legend.labs=c(paste0(">",round(sortsv[fit$n[2],ii],2),
                                  "(n = ",fit$n[1],")"),
                           paste0("<",round(sortsv[fit$n[2],ii],2),
                                  "(n = ",fit$n[2],")")))
  

  if (min(pvals) <= 0.05) {
    ggsave(paste0("good_",colnames(svdata)[ii],".pdf"),width = 4,height = 4)
  } else {
    ggsave(paste0("bad_",colnames(svdata)[ii],".pdf"),width = 4,height = 4)
  }
}
```

library(ggplotify)
library(magick)
library(cowplot)

#p value <= 0.05
fnames<-Sys.glob("good*.pdf")
length(fnames)
p<-lapply(fnames,function(i){
  pn<-as.ggplot(image_read_pdf(i))
})

plot_grid(plotlist = p, ncol=3)
ggsave("bestSep_good.pdf")
```

```{r,fig.height=12,fig.width=12}
#p value > 0.05
fnames<-Sys.glob("bad*.pdf")

p<-lapply(fnames,function(i){
  pn<-as.ggplot(image_read_pdf(i))
})

plot_grid(plotlist = p, ncol=3)
ggsave("bestSep_bad.pdf")

sessionInfo()
```
