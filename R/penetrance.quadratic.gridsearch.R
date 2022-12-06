rm(list=ls(all=TRUE))   
library("dplyr")
library("tidyr")
library(data.table)

out.dir <- "penetrance_data/"

 a<-seq(-51.66, -46.66, 0.01)
 b<-seq(19.46, 23.46, 0.01)
 c<-seq(-3.577, -1.577, 0.005)
 int.coef<-expand.grid(a,b,c)
 print(dim(int.coef))
 
   for (i in 1:70) {
     logdur<-log(i-0.5)
     logdur2<-logdur^2
     assign(paste0("pen.",i),as.vector(as.numeric(exp(int.coef[,1]+(int.coef[,2]*logdur)+(int.coef[,3]*logdur2)))))
   }
 
 pen<-(as.data.table((do.call(cbind, lapply( paste0("pen.", 1:70) , get) )    )))
 pen$max<-apply(pen,1,max)
 rownum<-as.numeric(rownames(pen))
 pen<-cbind(pen, rownum)
 pen.abcd<-cbind(pen,int.coef)


pen<-as.data.table(filter(pen.abcd, is.na(pen.abcd[,1])==FALSE & pen.abcd[,70]>=0.001 & pen.abcd[,70]<=0.1 & pen.abcd[,70]>=(0.80*pen.abcd$max) & pen[,70]<(1*pen.abcd$max)))
print(dim(pen))

keeprows<-pen$rownum
pen.abcd.small<-pen.abcd[pen.abcd$rownum %in% keeprows,]
fwrite(pen.abcd.small,"penetrance_data/penetrance_quadratic_valid_halfyears_extendedage74.csv", append=FALSE)
