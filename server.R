suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(googleVis))
library(RCmodels)
library(ggplot2)
library(xlsx)
library(Cairo)
options(shiny.usecairo=T)




shinyServer(function(input, output) {
    
    data <- reactive({
        inFile <- input$file1
        
        if (is.null(inFile)){
            return(NULL)
        }
        
        else if (inFile$type =="text/plain"){
            qvdata=read.table(inFile$datapath,skip=3,sep="|",dec=",")
            qvdata=qvdata[,c(2:4,7)]
            qvdata[,3:4]=qvdata[,4:3]
            names(qvdata)=c("Date","Time","W","Q")
            qvdata$Time=as.character(qvdata$Time)
            qvdata$Date=as.Date(gsub("\\.","-",qvdata$Date),"%d-%m-%Y")
            if(input$checkboxA==TRUE){
                years=as.numeric(format(qvdata$Date, "%Y"))
                qvdata=qvdata[which(years<=input$slider[2] & years >= input$slider[1]),]
                
            }
            qvdata=qvdata[with(qvdata,order(W)),]
            wq=as.matrix(qvdata[,3:4])
        }
        return(list("wq"=wq,"qvdata"=qvdata))
    })
    
    ## MODEL1 ##  Begin
    
    model1 <-eventReactive(input$go,{
        if("mdl1" %in% input$checkbox2){
            if(!is.null(data())){
                wq=data()$wq
                qvdata=data()$qvdata
                withProgress(message = 'Making plot', value = 0, {
                    RC=priors(input$select)
                    
                    RC$y=as.matrix(log(wq[,2]));
                    RC$w=0.01*wq[,1]; #to meters 
                    RC$w_tild=RC$w-min(RC$w);
                    RC$n=length(RC$y);
                    
                    
                    Dens <- function(th){ Densevalm11(th,RC)$pmin}
                    Densmin=optim(par=c(0,0),Dens,hessian=TRUE)
                    t_m=as.matrix(Densmin$par)
                    H=Densmin$hessian
                    
                    
                    l_m=as.matrix(log(RC$w_tild+exp(t_m[1,]))) 
                    
                    X_m=cbind(matrix(1,nrow(l_m),ncol(l_m)),l_m) 
                    
                    L=t(chol(RC$Sig_xinv+t(X_m)%*%X_m/exp(t_m[2,]))) 
                    
                    mu=solve(t(L),(solve(L,(RC$Sinvmu+t(X_m)%*%RC$y/exp(t_m[2,])))))
                    
                    v_temp=X_m%*%solve(RC$Sig_xinv+t(X_m)%*%X_m/exp(t_m[2,]))%*%t(X_m) 
                    
                    varappr=mean(as.matrix(diag(v_temp)+exp(t_m[2,])))
                    
                    RC$fit=X_m%*%mu
                    
                    RC$confinterval= cbind(X_m%*%mu+qnorm(0.025,0,sqrt(varappr)),X_m%*%mu+qnorm(0.975,0,sqrt(varappr))) 
                    
                    realdata=data.frame(W=RC$w,Q=RC$y)
                    realdata$l_m=l_m
                    realdata$fit=RC$fit
                    realdata$upper=RC$confinterval[,2]
                    realdata$lower=RC$confinterval[,1]
                    c_hat=min(realdata$W)-exp(t_m[1,])
                    Wmax=as.numeric(input$Wmax)
                    if(is.na(Wmax)){
                        Wmax=max(RC$w)
                    }
                    simdata=data.frame(W=seq(ceiling(c_hat*10)/10,ceiling(Wmax*10)/10,length.out=1000))
                    simdata$l_m = log(simdata$W-c_hat)
                    simdata$fit=mu[1,]+mu[2,]*simdata$l_m
                    simdata$upper=simdata$fit+qnorm(0.975,0,sqrt(varappr))
                    simdata$lower=simdata$fit+qnorm(0.025,0,sqrt(varappr))
                    realdata$residraun=(exp(realdata$Q)-exp(realdata$fit))
                    realdata$residupper=exp(realdata$upper)-exp(realdata$fit)
                    realdata$residlower=exp(realdata$lower)-exp(realdata$fit)
                    realdata$residlog=(realdata$Q-realdata$fit)/sqrt(exp(t_m[2,]))
                    
                    tafla=qvdata
                    tafla$W=0.01*tafla$W
                    tafla$Q=round(tafla$Q,1)
                    tafla$Qfit=as.numeric(round(exp(realdata$fit),3))
                    tafla$lower=round(exp(realdata$lower),3)
                    tafla$upper=round(exp(realdata$upper),3)
                    tafla$diffQ=tafla$Q-tafla$Qfit
                    names(tafla)=c("Date","Time","W","Q", "Q fit","Lower", "Upper","Q diff")
                    tafla=tafla[with(tafla,order(Date)),]
                    
                    xout=seq(ceiling(c_hat*10)/10,-0.01+ceiling(Wmax*10)/10,by=0.01)
                    interpol=approx(simdata$W,simdata$fit,xout=xout)
                    rctafla=t(as.data.frame(split(x=interpol$y, f=ceiling(seq_along(interpol$y)/10))))
                    rownames(rctafla)=seq(min(interpol$x),floor(max(interpol$x)*10)/10,by=0.1)*100
                    colnames(rctafla)=0:9
                    rctafla=round(exp(rctafla),3)
                    
                    
                    
                    
                    
                    return(list("varappr"=varappr,"qvdata"=qvdata,"simdata"=simdata,"realdata"=realdata,
                                "tafla"=tafla,"mu"=mu,"c_hat"=c_hat,"rctafla"=rctafla))
                    
                })
            }
        }
    })
    ranges1 <- reactiveValues(x = NULL, y = NULL)
    ranges2 <- reactiveValues(x = NULL, y = NULL)
    
    plotratingcurve1 <- reactive({
        plotlist=model1()
        rclog=NULL
        rcraun=NULL
        rcleiflog=NULL
        rcleifraun=NULL
        tafla=NULL
        rctafla=NULL
        outputlist=list()
        if(!is.null(plotlist$qvdata)) {
            realdata=plotlist$realdata
            simdata=plotlist$simdata
            
            if ("raun" %in% input$checkbox){
                rcraun=ggplot(simdata)+theme_bw()+geom_point(data=realdata,aes(exp(Q),W))+geom_line(aes(exp(fit),W))+
                    geom_line(aes(exp(lower),W),linetype="dashed")+geom_line(aes(exp(upper),W),linetype="dashed")+
                    ggtitle(paste("Rating curve for",input$name))+ylab("W  [m]")+xlab(expression(paste("Q  [",m^3,'/s]',sep='')))+
                    theme(plot.title = element_text(vjust=2))+coord_cartesian(xlim = ranges1$x, ylim = ranges1$y)
                outputlist$rcraun=rcraun
            }
            if("log" %in% input$checkbox){
                rclog=ggplot(realdata)+geom_line(mapping=aes(fit,l_m))+theme_bw()+geom_point(mapping=aes(Q,l_m))+geom_line(mapping=aes(lower,l_m),linetype="dashed")+
                    geom_line(mapping=aes(upper,l_m),linetype="dashed")+ggtitle(paste("Rating curve for",input$name,"(log scale)"))+
                    ylab(expression(log(W-hat(c))))+xlab("log(Q)")+theme(plot.title = element_text(vjust=2))
                
                outputlist$rclog=rclog
            }
            
            
            if ("leifraun" %in% input$checkbox){
                
                rcleifraun=ggplot(realdata)+geom_point(aes(W,residraun),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_line(aes(W,residupper),linetype="dashed")+geom_line(aes(W,residlower),linetype="dashed")+ylab(expression(paste("Q - ",hat(Q) ,"  [",m^3,'/s]',sep='')))+
                    ggtitle("Residual plot")+xlab("W  [m]")+theme(plot.title = element_text(vjust=2))
                
                outputlist$rcleifraun=rcleifraun
            } 
            if("leiflog" %in% input$checkbox){
                rcleiflog=ggplot(realdata)+geom_point(aes(l_m,residlog),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_abline(intercept = 2, slope = 0,linetype="dashed")+geom_abline(intercept = -2, slope = 0,linetype="dashed")+ylim(-4,4)+
                    ylab(expression(epsilon[i]))+ggtitle("Residual plot (log scale)")+xlab(expression(log(W-hat(c))))+
                    theme(plot.title = element_text(vjust=2))
                
                
                outputlist$rcleiflog=rcleiflog
            }
#             outputlist$tafla=tafla
#             outputlist$rctafla=plotlist$rctafla
            
            
        }
        
        return(outputlist)
    })
    model2 <- eventReactive(input$go,{
        if("mdl2" %in% input$checkbox2){
            if(!is.null(data())){
                wq=data()$wq
                qvdata=data()$qvdata
                withProgress(message = 'Making plot', value = 0, {
                    Nit=20000
                    
                    RC=priors("Iceland")
                    
                    RC$nugget=10^-8
                    RC$mu_sb=0.5
                    RC$mu_pb=0.5
                    RC$tau_pb2=0.25^2
                    RC$s=3
                    RC$v=5
                    
                    RC$y=rbind(as.matrix(log(wq[,2])),0)
                    RC$w=as.matrix(0.01*wq[,1])
                    RC$w_tild=RC$w-min(RC$w)
                    
                    Adist1 <- Adist(RC$w)
                    RC$A=Adist1$A
                    RC$dist=Adist1$dist
                    RC$n=Adist1$n
                    RC$N=Adist1$N
                    RC$O=Adist1$O
                    
                    RC$P=diag(nrow=5,ncol=5,6)-matrix(nrow=5,ncol=5,1)
                    RC$Sig_ab= rbind(c(RC$sig_a^2, RC$p_ab*RC$sig_a*RC$sig_b), c(RC$p_ab*RC$sig_a*RC$sig_b, RC$sig_b^2))
                    RC$mu_x=as.matrix(c(RC$mu_a,RC$mu_b, rep(0,RC$n))) #Setja i RC
                    
                    RC$B=B_splines(t(RC$w_tild)/RC$w_tild[length(RC$w_tild)])
                    RC$Z=cbind(t(rep(0,2)),t(rep(1,RC$n)))
                    
                    RC$m1=matrix(0,nrow=2,ncol=RC$n)
                    RC$m2=matrix(0,nrow=RC$n,ncol=2)
                    theta.init=rep(0,9)
                    
                    Dens = function(th) {-Densevalm22(th,RC)$p}
                    Densmin=optim(par=theta.init,Dens,method="L-BFGS-B",hessian=TRUE)
                    t_m =Densmin$par
                    H=Densmin$hessian
                    phi_b=t_m[3]
                    sig_b2=t_m[2]
                    zeta=t_m[1]
                    lambda=t_m[4:9]
                    l=log(RC$w_tild+exp(t_m[1]))
                    varr_m=exp(RC$B%*%lambda)
                    Sig_eps=diag(as.numeric(rbind(varr_m,0)))
                    R_Beta=(1+sqrt(5)*RC$dist/exp(phi_b)+5*RC$dist^2/(3*exp(phi_b)^2))*exp(-sqrt(5)*RC$dist/exp(phi_b))+diag(1,RC$n,RC$n)*RC$nugget
                    Sig_x=rbind(cbind(RC$Sig_ab,matrix(0,nrow=2,ncol=RC$n)),cbind(matrix(0,nrow=RC$n,ncol=2),exp(sig_b2)*R_Beta))
                    
                    X=rbind(cbind(matrix(1,dim(l)),l,diag(as.numeric(l))%*%RC$A),RC$Z)
                    
                    
                    L=t(chol(as.matrix(X%*%Sig_x%*%t(X)+Sig_eps)))
                    
                    w=solve(L,(-RC$y+X%*%RC$mu_x))
                    mu=RC$mu_x-Sig_x%*%(t(X)%*%(solve(t(L),w)))
                    LH=t(chol(H))/0.8
                    
                    cl <- makeCluster(4)
                    registerDoParallel(cl)
                    Wmax=as.numeric(input$Wmax)
                    if(is.na(Wmax)){
                        Wmax=ceiling(max(RC$O)*10)/10
                    }
                    WFill=W_unobserved(RC$O,min=ceiling((min(RC$O)-exp(t_m[1]))*10)/10,max=Wmax)
                    RC$W_u=WFill$W_u
                    RC$W_u_tild=WFill$W_u_tild
                    RC$Bsim=B_splines(t(RC$W_u_tild)/RC$W_u_tild[length(RC$W_u_tild)])
                    MCMC <- foreach(i=1:4,.combine=cbind,.export=c("Densevalm22","Densevalm22_u")) %dopar% {
                        ypo_obs=matrix(0,nrow=RC$N,ncol=Nit)
                        param=matrix(0,nrow=9+RC$n+2,ncol=Nit)
                        t_old=as.matrix(t_m)
                        Dens<-Densevalm22(t_old,RC)
                        p_old=Dens$p
                        ypo_old=Dens$ypo
                        x_old=Dens$x
                        
                        for(j in 1:Nit){
                            t_new=t_old+solve(t(LH),rnorm(9,0,1))
                            Densnew<-Densevalm22(t_new,RC)
                            x_new=Densnew$x
                            ypo_new=Densnew$ypo
                            p_new=Densnew$p
                            logR=p_new-p_old
                            
                            if (logR>log(runif(1))){
                                t_old=t_new
                                p_old=p_new
                                ypo_old=ypo_new
                                x_old=x_new
                                
                                
                            }
                            ypo_obs[,j]=ypo_old
                            param[,j]=rbind(t_old,x_old)    
                        }
                        seq=seq(2000,Nit,5)
                        ypo_obs=ypo_obs[,seq]
                        param=param[,seq]
                        unobserved=apply(param,2,FUN=function(x) Densevalm22_u(x,RC))
                        #x_obs=param[10:nrow(param),]
                        output=rbind(ypo_obs,unobserved)
                        
                        return(output)
                    }
                    stopCluster(cl)
                    betasamples=apply(MCMC[(RC$N+length(RC$W_u)+1):nrow(MCMC),],2,FUN=function(x){x[2]+x[3:length(x)]})
                    yposamples=MCMC[1:(RC$N+length(RC$W_u)),]
                    ypodata=as.data.frame(t(apply(yposamples,1,quantile, probs = c(0.025,0.5, 0.975),na.rm=T)))
                    names(ypodata)=c("lower","fit","upper")
                    ypodata$W=c(RC$w,RC$W_u)
                    ypodata$l_m=c(l,log(RC$W_u-min(RC$O)+exp(t_m[1])))
                    realdata=ypodata[1:RC$N,]
                    ypodata=ypodata[with(ypodata,order(W)),]
                    betadata=as.data.frame(t(apply(betasamples,1,quantile, probs = c(0.025,0.5, 0.975),na.rm=T)))
                    names(betadata)=c("lower","fit","upper")
                    betadata$W=c(RC$O,RC$W_u)
                    betadata=betadata[with(betadata,order(W)),]
                    realdata$Q=RC$y[1:RC$N,]
                    realdata$residraun=(exp(realdata$Q)-exp(realdata$fit))
                    realdata$residupper=exp(realdata$upper)-exp(realdata$fit)
                    realdata$residlower=exp(realdata$lower)-exp(realdata$fit)
                    realdata$residlog=(realdata$Q-realdata$fit)/sqrt(varr_m)
                    
                    tafla=qvdata
                    tafla$W=0.01*tafla$W
                    tafla$Q=round(tafla$Q,1)
                    tafla$Qfit=round(exp(realdata$fit),3)
                    tafla$lower=round(exp(realdata$lower),3)
                    tafla$upper=round(exp(realdata$upper),3)
                    tafla$diffQ=tafla$Q-tafla$Qfit
                    names(tafla)=c("Date","Time","W","Q", "Q fit","Lower", "Upper","Q diff")
                    tafla=tafla[with(tafla,order(Date)),]
                    xout=seq(ceiling((min(RC$w)-exp(t_m[1]))*10)/10,-0.01+ceiling(max(RC$O)*10)/10,by=0.01)
                    interpol=approx(ypodata$W,ypodata$fit,xout=xout)
                        rctafla=t(as.data.frame(split(x=interpol$y, f=ceiling(seq_along(interpol$y)/10))))
                        rownames(rctafla)=seq(min(interpol$x),floor(max(interpol$x)*10)/10,by=0.1)*100
                        colnames(rctafla)=0:9
                        rctafla=round(exp(rctafla),3)
                    return(list("qvdata"=qvdata,"betadata"=betadata,"ypodata"=ypodata,"realdata"=realdata,"tafla"=tafla,"rctafla"=rctafla))
                })
            }
        }
    })
    
    plotratingcurve2 <- reactive({
        plotlist=model2()
        rclog=NULL
        rcraun=NULL
        rcleiflog=NULL
        rcleifraun=NULL
        tafla=NULL
        outputlist=list()
        if(!is.null(plotlist$qvdata)) {
            ypodata=plotlist$ypodata
            betadata=plotlist$betadata
            realdata=plotlist$realdata
            
            if ("raun" %in% input$checkbox){
                rcraun=ggplot(ypodata)+theme_bw()+geom_point(data=realdata,aes(exp(Q),W))+geom_line(aes(exp(fit),W))+
                    geom_line(aes(exp(lower),W),linetype="dashed")+geom_line(aes(exp(upper),W),linetype="dashed")+
                    ggtitle(paste("Rating curve for",input$name))+ylab("W [m]")+xlab(expression(paste("Q  [",m^3,'/s]',sep='')))+
                    theme(plot.title = element_text(vjust=2))+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
                outputlist$rcraun=rcraun
            }
            if("log" %in% input$checkbox){
                rclog=ggplot(realdata)+geom_line(mapping=aes(fit,l_m))+theme_bw()+geom_point(mapping=aes(Q,l_m))+geom_line(mapping=aes(lower,l_m),linetype="dashed")+
                    geom_line(mapping=aes(upper,l_m),linetype="dashed")+ggtitle(paste("Rating curve for",input$name,"(log scale)"))+
                    ylab(expression(log(W-hat(c))))+xlab("log(Q)")+theme(plot.title = element_text(vjust=2))
                
                outputlist$rclog=rclog
            }
            
            
            if ("leifraun" %in% input$checkbox){
                rcleifraun=ggplot(realdata)+geom_point(aes(W,residraun),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_line(aes(W,residupper),linetype="dashed")+geom_line(aes(W,residlower),linetype="dashed")+ylab(expression(paste("Q - ",hat(Q) ,"  [",m^3,'/s]',sep='')))+
                    ggtitle("Residual plot")+xlab("W  [cm]")+theme(plot.title = element_text(vjust=2))
                
                outputlist$rcleifraun=rcleifraun
            } 
            if("leiflog" %in% input$checkbox){
                rcleiflog=ggplot(realdata)+geom_point(aes(l_m,residlog),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_abline(intercept = 2, slope = 0,linetype="dashed")+geom_abline(intercept = -2, slope = 0,linetype="dashed")+ylim(-4,4)+
                    ylab(expression(epsilon[i]))+ggtitle("Residual plot (log scale)")+xlab(expression(log(W-hat(c))))+
                    theme(plot.title = element_text(vjust=2))
                
                
                outputlist$rcleiflog=rcleiflog
            }
            smoothbeta=ggplot(data=betadata)+geom_line(aes(W,fit))+
                geom_line(aes(W,lower),linetype="dashed")+geom_line(aes(W,upper),linetype="dashed")+
                ylab(expression(b+beta(W)))+ggtitle("b parameter as a function of stage W")+xlab("W [m]")+
                theme(plot.title = element_text(vjust=2))+theme_bw()
                outputlist$smoothbeta=smoothbeta
            
            
            return(outputlist)
        } 
        
        
        
        
    })
    output$plots1 <- renderUI({
        if(length(plotratingcurve1())!=0){
            plot_output_list <- lapply(1:(length(plotratingcurve1())-1), function(i) {
                plotname=paste("plot", i, sep="")
                clickname=paste("plot",i,"_click",sep="")
                dblclickname=paste("plot",i,"_dblclick",sep="")
                brushname=paste("plot",i,"_brush",sep="")
                plotOutput(plotname,click =clickname,dblclick = dblclickOpts(
                    id = dblclickname),brush = brushOpts(id = brushname,resetOnNew = TRUE))
            })
            
            do.call(tagList, plot_output_list)
        }
    })
    
    
    output$plot1<-renderPlot({
        if(length(plotratingcurve1())!=0)
            plotratingcurve1()[[1]]
    },height=400,width=600)
    output$plot2<-renderPlot({
        if(length(plotratingcurve1()) >= 2)
            plotratingcurve1()[[2]]   
    },height=400,width=600)
    output$plot3<-renderPlot({
        if(length(plotratingcurve1())>=3)
            plotratingcurve1()[[3]]    
    },height=400,width=600)
    output$plot4<-renderPlot({
        if(length(plotratingcurve1())>=4)
            plotratingcurve1()[[4]]     
    },height=400,width=600)
    output$tafla1 <- renderGvis({
        if(!is.null(model1())){
            table=model1()$tafla
            gvisTable(table,options=list(
                                page='enable',
                                pageSize=30,
                                width=550
                            ))
        }
        
    })
    output$hakk <- renderPrint({
        lapply(model2()$tafla,class)
    })
    output$rctafla1 <- renderGvis({
        if(!is.null(model1())){
            rctafla1=as.data.frame(model1()$rctafla)
            gvisTable(rctafla1,options=list(
                page='enable',
                pageSize=30,
                width=550
            ))
        }
        
    })
    output$plots2 <- renderUI({
        if(length(plotratingcurve2())!=0){
            plot_output_list <- lapply(1:(length(plotratingcurve2())-2), function(i) {
                plotname=paste("plot", 4+i, sep="")
                clickname=paste("plot",4+i,"_click",sep="")
                dblclickname=paste("plot",4+i,"_dblclick",sep="")
                brushname=paste("plot",4+i,"_brush",sep="")
                plotOutput(plotname,click =clickname,dblclick = dblclickOpts(
                    id = dblclickname),brush = brushOpts(id = brushname,resetOnNew = TRUE))
            })
            plot_output_list$Beta=plotOutput('Beta',click ='Beta_click',dblclick = dblclickOpts(
                id = 'Beta_dblclick'),brush = brushOpts(id = 'Beta_brush',resetOnNew = TRUE))
            
            do.call(tagList, plot_output_list)
        }
    })
    

    
    output$plot5<-renderPlot({
        if(length(plotratingcurve2())!=0)
            plotratingcurve2()[[1]]
    },height=400,width=600)
    output$plot6<-renderPlot({
        if(length(plotratingcurve2()) >= 2)
            plotratingcurve2()[[2]]   
    },height=400,width=600)
    
    output$plot7<-renderPlot({
        if(length(plotratingcurve2())>=3)
            plotratingcurve2()[[3]]    
    },height=400,width=600)
    
    output$plot8<-renderPlot({
        if(length(plotratingcurve2())>=4)
            plotratingcurve2()[[4]]     
    },height=400,width=600)
    
    output$Beta <- renderPlot({
        if(!is.null(plotratingcurve2()))
            plotratingcurve2()$smoothbeta
    },height=400,width=600)
    
    output$tafla2 <- renderGvis({
        if(!is.null(model2())){
            table=as.data.frame(model2()$tafla)
            gvisTable(table,options=list(
                page='enable',
                pageSize=30,
                width=550
            ))
        }
        
        
    })
    output$rctafla2 <- renderGvis({
        if(!is.null(model2())){
            rctafla2=as.data.frame(model2()$rctafla)
            gvisTable(rctafla2,options=list(
                page='enable',
                pageSize=30,
                width=550
            ))
        
        } 
    })
    
    #######Interactivity#######
    
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plot1_dblclick, {
        brush <- input$plot1_brush
        if (!is.null(brush)) {
            ranges1$x <- c(brush$xmin, brush$xmax)
            ranges1$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges1$x <- NULL
            ranges1$y <- NULL
        }
    })
    
    observeEvent(input$plot5_dblclick, {
        brush <- input$plot5_brush
        if (!is.null(brush)) {
            ranges2$x <- c(brush$xmin, brush$xmax)
            ranges2$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges2$x <- NULL
            ranges2$y <- NULL
        }
    })
    
    observeEvent(input$data1, {
        data1=model1()$tafla
        write.xlsx(data1,paste(input$name,"xlsx",sep="."),sheetName="data1",append=TRUE,overwrite=TRUE)
    })
    observeEvent(input$data2, {
        data2=model2()$tafla
        write.xlsx(data2,paste(input$name,"xlsx",sep="."),sheetName="data2",append=TRUE,overwrite=TRUE)
    })
     observeEvent(input$fullrc1, {
         fullrc1=model1()$rctafla
         write.xlsx(fullrc1,paste(input$name,"xlsx",sep="."),sheetName="fullrc1",append=TRUE,overwrite=TRUE)
     })
    observeEvent(input$fullrc2, {
       fullrc2=model2()$rctafla
       write.xlsx(fullrc2,paste(input$name,"xlsx",sep="."),sheetName="fullrc2",append=TRUE,overwrite=TRUE)
    })
    
    output$downloadReport <- downloadHandler(
        filename = function() {
            filename=gsub("\\.[^.]*$","",input$file1$name)
            paste(filename,'pdf', sep=".")
        },
        content <- function(file) {
            src <- normalizePath('myreport.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write
            #permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'myreport.Rmd')
            
            library(rmarkdown)
            out <- render('myreport.Rmd',pdf_document())
            file.rename(out, file)
        }
        
        
    )
    
    
})

