function(x,y,nboot=200,plotit=TRUE,plotop=FALSE,SEED=TRUE){
        #
        #   Compute confidence intervals for the difference between deciles
        #   of two independent groups. The simultaneous probability coverage is .95.
        #   The Harrell-Davis estimate of the qth quantile is used.
        #   The default number of bootstrap samples is nboot=200
        #
        #   The results are stored and returned in a 9 by 3 matrix,
        #   the ith row corresponding to the i/10 quantile.
        #   The first column is the lower end of the confidence interval.
        #   The second column is the upper end.
        #   The third column is the estimated difference between the deciles
        #   (second group minus first).
        #
        plotit<-as.logical(plotit)
        x<-x[!is.na(x)]
        y<-y[!is.na(y)]
        if(SEED)set.seed(2) # set seed of random number generator so that
        #   results can be duplicated.
        crit<-80.1/(min(length(x),length(y)))^2+2.73
        m<-matrix(0,9,3)
        for (i in 1:9){
                q<-i/10
                print("Working on quantile")
                print(q)
                data<-matrix(sample(x,size=length(x)*nboot,replace=TRUE),nrow=nboot)
                bvec<-apply(data,1,hd,q)
                sex<-var(bvec)
                data<-matrix(sample(y,size=length(y)*nboot,replace=TRUE),nrow=nboot)
                bvec<-apply(data,1,hd,q)
                sey<-var(bvec)
                dif<-hd(y,q)-hd(x,q)
                m[i,3]<-dif
                m[i,1]<-dif-crit*sqrt(sex+sey)
                m[i,2]<-dif+crit*sqrt(sex+sey)
        }
        dimnames(m)<-list(NULL,c("ci.lower","ci.upper","Delta.hat"))
        if(plotit){
                if(plotop){
                        xaxis<-c(1:9)/10
                        xaxis<-c(xaxis,xaxis)
                }
                if(!plotop)xaxis<-c(deciles(x),deciles(x))
                par(pch="+")
                yaxis<-c(m[,1],m[,2])
                if(!plotop)plot(xaxis,yaxis,ylab="delta",xlab="x (first group)")
                if(plotop)plot(xaxis,yaxis,ylab="delta",xlab="Deciles")
                par(pch="*")
                if(!plotop)points(deciles(x),m[,3])
                if(plotop)points(c(1:9)/10,m[,3])
        }
        m
}