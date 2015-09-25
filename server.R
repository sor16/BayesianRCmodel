suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(googleVis))
library(RCmodels)
library(ggplot2)
library(xlsx)
library(Cairo)
library(rmarkdown)

options(shiny.usecairo=T)



vals<-reactiveValues(keeprows=NULL)
daterange=reactiveValues(keeprows=NULL)
ranges1 <- reactiveValues(x = NULL, y = NULL)
ranges2 <- reactiveValues(x = NULL, y = NULL)
dummy <- reactiveValues(Q=NULL,W=NULL)
force <- reactiveValues(Q=NULL,W=NULL)
shinyServer(function(input, output) {
    
    data <- eventReactive(input$go,{
        if(is.null(input$file)){
            return(NULL)
            }
        dummy=reactiveValuesToList(dummy)
        force=reactiveValuesToList(force)
        cleandata=clean(input$file,dummy=dummy,force=force,keeprows=vals$keeprows,shiny=TRUE,advanced=input$advanced,exclude=input$exclude,excludedates=input$excludeDates, includedates=input$includeDates)
        
        if(length(vals$keeprows)==0 ){
        vals$keeprows= rep(TRUE,nrow(cleandata$observedData_before))
        }
                years=as.numeric(format(cleandata$observedData_before$Date, "%Y"))
                includeindex=years<=input$includeDates[2] & years >= input$includeDates[1]
                excludeindex=cleandata$observedData_before$Date<=input$excludeDates[1] | cleandata$ observedData_before$Date >= input$excludeDates[2]
                daterange$keeprows=excludeindex & includeindex
        return(cleandata)
         
    })
    
    ## MODEL1 ##  Begin
    model1 <-eventReactive(input$go,{
        if("mdl1" %in% input$checkbox2){
            if(!is.null(data())){
                withProgress(message = 'Making plot', value = 0, {
                    output=model1BH(data(),country=input$select,Wmin="",Wmax=input$Wmax)
                    return(output)
                    
                })
            }
        }
    })

    
    plotratingcurve1 <- reactive({
        plotlist=model1()
        cleandata=data()
        dummy=as.data.frame(reactiveValuesToList(dummy))
        force=as.data.frame(reactiveValuesToList(force))
        rclog=NULL
        rcraun=NULL
        rcleiflog=NULL
        rcleifraun=NULL
        outputlist=list()
        if(!is.null(plotlist$observedData)) {
            observedPrediction=plotlist$observedPrediction
            completePrediction=plotlist$completePrediction
            keeprows=vals$keeprows & daterange$keeprows
            keep=cleandata$observedData_before[keeprows, ,drop=FALSE]
            excludeManual=cleandata$observedData_before[!vals$keeprows, ,drop=FALSE]
            excludeYears=cleandata$observedData_before[!daterange$keeprows, ,drop=FALSE]
            name=input$name
            if(nchar(name)==0){
                name=gsub("\\.[^.]*$","",input$file$name)
            }
            
            if ("raun" %in% input$checkbox){
                rcraun=ggplot(completePrediction)+theme_bw()+geom_point(data=keep,aes(Q,W))+geom_path(aes(exp(fit),W))+
                    geom_path(aes(exp(lower),W),linetype="dashed")+geom_path(aes(exp(upper),W),linetype="dashed")+
                    ggtitle(paste("Rating curve for ", name," Model 1"))+ylab("W  [m]")+xlab(expression(paste("Q  [",m^3,'/s]',sep='')))+
                    theme(plot.title = element_text(vjust=2))+coord_cartesian(xlim = ranges1$x, ylim = ranges1$y)+
                    geom_point(data=excludeManual,aes(Q,W),fill=NA,col="black",alpha=0.75,shape=21)+
                    geom_point(data=excludeYears,aes(Q,W),fill=NA,col="purple",alpha=1,shape=21)
                if(any(dim(dummy))){
                    rcraun=rcraun+geom_point(data=dummy,aes(Q,W),fill="red",col="red")
                }
                if(any(dim(force))){
                    rcraun=rcraun+geom_point(data=force,aes(Q,W),fill="blue",col="blue")
                }
                outputlist$rcraun=rcraun
            }
            if("log" %in% input$checkbox){
                rclog=ggplot(observedPrediction)+geom_path(mapping=aes(fit,l_m))+theme_bw()+geom_point(mapping=aes(Q,l_m))+geom_path(mapping=aes(lower,l_m),linetype="dashed")+
                    geom_path(mapping=aes(upper,l_m),linetype="dashed")+ggtitle(paste("Rating curve for ", name," Model 1 (logscale)"))+
                    ylab(expression(log(W-hat(c))))+xlab("log(Q)")+ theme(plot.title = element_text(vjust=2))
                
                outputlist$rclog=rclog
            }
            
            
            if ("leifraun" %in% input$checkbox){
                
                rcleifraun=ggplot(observedPrediction)+geom_point(aes(W,residuals),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_path(aes(W,residupper),linetype="dashed")+geom_path(aes(W,residlower),linetype="dashed")+ylab(expression(paste("Q - ",hat(Q) ,"  [",m^3,'/s]',sep='')))+
                    ggtitle("Residuals")+xlab("W  [m]")+theme(plot.title = element_text(vjust=2))
                
                outputlist$rcleifraun=rcleifraun
            } 
            if("leiflog" %in% input$checkbox){
                max=max(abs(observedPrediction$standardResiduals))
                if(max>4){
                    ylim=c(-(max+0.2),max+0.2)
                }else{
                    ylim=c(-4,4)
                }
                rcleiflog=ggplot(observedPrediction)+geom_point(aes(l_m,standardResiduals),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_abline(intercept = 2, slope = 0,linetype="dashed")+geom_abline(intercept = -2, slope = 0,linetype="dashed")+ylim(ylim)+
                    ylab(expression(epsilon[i]))+ggtitle("Standardized residuals")+xlab(expression(log(W-hat(c))))+
                    theme(plot.title = element_text(vjust=2))
                
                
                outputlist$rcleiflog=rcleiflog
            }
            
        }
        
        return(outputlist)
    })
    model2 <- eventReactive(input$go,{
        if("mdl2" %in% input$checkbox2){
            if(!is.null(data())){
                withProgress(message = 'Making plot', value = 0, {
                    output=model2BH(data(),country=input$select,Wmin="",Wmax=input$Wmax)
                    return(output)
                    
                })
            }
        }
    })
    
    plotratingcurve2 <- reactive({
        plotlist=model2()
        cleandata=data()
        dummy=as.data.frame(reactiveValuesToList(dummy))
        force=as.data.frame(reactiveValuesToList(force))
        rclog=NULL
        rcraun=NULL
        rcleiflog=NULL
        rcleifraun=NULL
        TableOfData=NULL
        outputlist=list()
        if(!is.null(plotlist$observedData)) {
            completePrediction=plotlist$completePrediction
            betaData=plotlist$betaData
            observedPrediction=plotlist$observedPrediction
            keeprows=vals$keeprows & daterange$keeprows
            keep=cleandata$observedData_before[keeprows, ,drop=FALSE]
            excludeManual=cleandata$observedData_before[!vals$keeprows, ,drop=FALSE]
            excludeYears=cleandata$observedData_before[!daterange$keeprows, ,drop=FALSE]
            filename=input$name
            name=input$name
            if(nchar(name)==0){
                name=gsub("\\.[^.]*$","",input$file$name)
            }
            
            
            if ("raun" %in% input$checkbox){
                rcraun=ggplot(completePrediction)+theme_bw()+geom_point(data=keep,aes(Q,W))+geom_path(aes(exp(fit),W))+
                    geom_path(aes(exp(lower),W),linetype="dashed")+geom_path(aes(exp(upper),W),linetype="dashed")+
                    ggtitle(paste("Rating curve for ", name," Model 2"))+ylab("W [m]")+xlab(expression(paste("Q  [",m^3,'/s]',sep='')))+
                    theme(plot.title = element_text(vjust=2))+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)+
                    geom_point(data=excludeManual,aes(Q,W),fill=NA,col="black",alpha=0.75,shape=21)+
                    geom_point(data=excludeYears,aes(Q,W),fill=NA,col="purple",alpha=1,shape=21)
                if(any(dim(dummy))){
                    rcraun=rcraun+geom_point(data=dummy,aes(Q,W),fill="red",col="red")
                }
                if(any(dim(force))){
                    rcraun=rcraun+geom_point(data=force,aes(Q,W),fill="blue",col="blue")
                }
                outputlist$rcraun=rcraun
            }
            if("log" %in% input$checkbox){
                rclog=ggplot(observedPrediction)+geom_path(mapping=aes(fit,l_m))+theme_bw()+geom_point(mapping=aes(Q,l_m))+geom_path(mapping=aes(lower,l_m),linetype="dashed")+
                    geom_path(mapping=aes(upper,l_m),linetype="dashed")+ggtitle(paste("Rating curve for",name," Model 2 (log scale)"))+
                    ylab(expression(log(W-hat(c))))+xlab("log(Q)")+theme(plot.title = element_text(vjust=2))
                
                outputlist$rclog=rclog
            }
            
            
            if ("leifraun" %in% input$checkbox){
                rcleifraun=ggplot(observedPrediction)+geom_point(aes(W,residuals),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_path(aes(W,residupper),linetype="dashed")+geom_path(aes(W,residlower),linetype="dashed")+ylab(expression(paste("Q - ",hat(Q) ,"  [",m^3,'/s]',sep='')))+
                    ggtitle("Residuals")+xlab("W [m]")+theme(plot.title = element_text(vjust=2))
                
                outputlist$rcleifraun=rcleifraun
            } 
            if("leiflog" %in% input$checkbox){
                max=max(abs(observedPrediction$standardResiduals))
                if(max>4){
                    ylim=c(-(max+0.2),max+0.2)
                }else{
                    ylim=c(-4,4)
                }
                rcleiflog=ggplot(observedPrediction)+geom_point(aes(l_m,standardResiduals),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_abline(intercept = 2, slope = 0,linetype="dashed")+geom_abline(intercept = -2, slope = 0,linetype="dashed")+ylim(ylim)+
                    ylab(expression(epsilon[i]))+ggtitle("Standardized residuals")+xlab(expression(log(W-hat(c))))+
                    theme(plot.title = element_text(vjust=2))
                
                
                outputlist$rcleiflog=rcleiflog
            }
            smoothbeta=ggplot(data=betaData)+geom_path(aes(W,fit))+
                geom_path(aes(W,lower),linetype="dashed")+geom_path(aes(W,upper),linetype="dashed")+
                ylab(expression(b+beta(W)))+ggtitle("b parameter as a function of stage W")+xlab("W [m]")+
                theme(plot.title = element_text(vjust=2))+theme_bw()
                outputlist$smoothbeta=smoothbeta
            
            
            return(outputlist)
        } 
        
        
        
        
    })
    output$plots1 <- renderUI({
        if(length(plotratingcurve1())!=0){
            plot_output_list <- lapply(1:(length(plotratingcurve1())), function(i) {
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
#             sessionkeepermodel1=reactiveValuesToList(sessionkeepermodel1)
#             if(counter$i<length(sessionkeepermodel1)){
#             counter=reactiveValuesToList(counter)
#             i=as.character(counter$i)
#             sessionkeepermodel1[[i]][[1]]
#         }else{
            plotratingcurve1()[[1]]
        # }
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
    output$TableOfData1 <- renderGvis({
        if(!is.null(model1())){
            table=model1()$TableOfData
            gvisTable(table,options=list(
                                page='enable',
                                pageSize=30,
                                width=550
                            ))
        }
        
    })
  
    output$FitTable1 <- renderGvis({
        if(!is.null(model1())){
            FitTable1=as.data.frame(model1()$FitTable)
            gvisTable(FitTable1,options=list(
                page='enable',
                pageSize=30,
                width=550
            ))
        }
        
    })
    output$LowerTable1 <- renderGvis({
        if(!is.null(model1())){
            LowerTable1=as.data.frame(model1()$LowerTable)
            gvisTable(LowerTable1,options=list(
                page='enable',
                pageSize=30,
                width=550
            ))
        }
        
    })
    output$UpperTable1 <- renderGvis({
        if(!is.null(model1())){
            UpperTable1=as.data.frame(model1()$UpperTable)
            gvisTable(UpperTable1,options=list(
                page='enable',
                pageSize=30,
                width=550
            ))
        }
        
    })
    output$plots2 <- renderUI({
        if(length(plotratingcurve2())!=0){
            plot_output_list <- lapply(1:(length(plotratingcurve2())-1), function(i) {
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
    
    output$TableOfData2 <- renderGvis({
        if(!is.null(model2())){
            table=as.data.frame(model2()$TableOfData)
            gvisTable(table,options=list(
                page='enable',
                pageSize=30,
                width=550
            ))
        }
        
        
    })
    output$FitTable2 <- renderGvis({
        if(!is.null(model2())){
            FitTable2=as.data.frame(model2()$FitTable)
            gvisTable(FitTable2,options=list(
                page='enable',
                pageSize=30,
                width=550
            ))
        
        } 
    })
    output$LowerTable2 <- renderGvis({
        if(!is.null(model2())){
            LowerTable2=as.data.frame(model2()$LowerTable)
            gvisTable(LowerTable2,options=list(
                page='enable',
                pageSize=30,
                width=550
            ))
            
        } 
    })
    output$UpperTable2 <- renderGvis({
        if(!is.null(model2())){
            UpperTable2=as.data.frame(model2()$UpperTable)
            gvisTable(UpperTable2,options=list(
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
    
    observeEvent(input$plot1_click,{
        if("raun"%in% input$checkbox){
            observedData=as.data.frame(data()$observedData_before)
            res <- nearPoints(observedData, input$plot1_click,xvar = "Q", yvar = "W", allRows = TRUE,threshold=5)
            if(any(res$selected_) & input$clickopts=='exclude'){
                vals$keeprows=xor(vals$keeprows,res$selected_)
            }else if(input$clickopts=='force'){
                force$W=c(force$W,input$plot1_click$y)
                force$Q=c(force$Q,input$plot1_click$x)
            }else if(input$clickopts=='dummy'){
                dummy$W=c(dummy$W,input$plot1_click$y)
                dummy$Q=c(dummy$Q,input$plot1_click$x)
            }
        }
        
    })
    observeEvent(input$plot5_click,{
        if("raun"%in% input$checkbox){
            observedData=data()$observedData_before
            res <- nearPoints(observedData, input$plot5_click,xvar = "Q", yvar = "W", allRows = TRUE,threshold=5)
            if(any(res$selected_) & input$clickopts=='exclude'){
                vals$keeprows=xor(vals$keeprows,res$selected_)
            }else if(input$clickopts=='force'){
                force$W=c(force$W,input$plot5_click$y)
                force$Q=c(force$Q,input$plot5_click$x)
            }else if(input$clickopts=='dummy'){
                dummy$W=c(dummy$W,input$plot5_click$y)
                dummy$Q=c(dummy$Q,input$plot5_click$x)
            }
        }
        
    })
#      counter=reactiveValues(i=1)
#     sessionkeepermodel1=reactiveValues()
#     sessionkeepermodel2=reactiveValues()
#     
#      observeEvent(input$go,{
#          i=reactiveValuesToList(counter)
#          i=as.character(counter$i)
#          # if(is.null(sessionkeepermodel1[[i]])){
#          sessionkeepermodel1[[i]]=plotratingcurve1()
#          #}
#          counter$i=counter$i+1
#          
#      })
#      observeEvent(input$go,{
#          
#          counter=reactiveValuesToList(counter)
#          i=as.character(counter$i)
#          sessionkeepermodel2[[i]]=plotratingcurve2()
#          
#      })
#     observeEvent(input$forward,{
#         counter$i=counter$i+1
#         
#     })
#     observeEvent(input$back,{
#         counter$i=counter$i-1
#     })
    observeEvent(input$reset,{
        n=nrow(data()$observedData_before)
        vals$keeprows=rep(TRUE,n)
        dummy$W=NULL
        dummy$Q=NULL
        force$W=NULL
        force$Q=NULL
    })
    
    output$xlsxexport <- downloadHandler(
        filename= function(){
            name=input$name
            if(nchar(name)==0){
                name=gsub("\\.[^.]*$","",input$file$name)
            }
            paste(name,'xlsx',sep=".")
        },
        content = function(file){
            wb <- createWorkbook()
            saveWorkbook(wb,file)
            tablelist=list()
            if(!is.null(model1())){
                tablelist$TableOfData1=model2()$TableOfData
                tablelist$FitTable1=model1()$FitTable
                tablelist$LowerTable1=model1()$LowerTable
                tablelist$UpperTable1=model1()$UpperTable
                tablelist$plotTable1=model1()$plotTable
            }
            if(!is.null(model2())){
                tablelist$TableOfData2=model2()$TableOfData
                tablelist$FitTable2=model2()$FitTable
                tablelist$LowerTable2=model2()$LowerTable
                tablelist$UpperTable2=model2()$UpperTable
                tablelist$plotTable2=model2()$plotTable
            }
            lapply(names(tablelist),function(x) write.xlsx(tablelist[[x]],file,sheetName=x,append=TRUE,row.names=FALSE))
        }
    )
    
    output$downloadImages <- downloadHandler(
        filename = function() {
            filename=input$name
            if(nchar(filename)==0){
                filename=gsub("\\.[^.]*$","",input$file$name)
            }
            paste(filename,'html', sep=".")
        },
        content <- function(file) {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            setwd(owd)
            
                src <- normalizePath('images.Rmd')
                file.copy(src, 'images.Rmd')
                out <- render('images.Rmd',html_document())
                file.rename(out, file)
        }
        
    )
    
    
    output$downloadReport <- downloadHandler(
            filename = function() {
                filename=input$name
                if(nchar(filename)==0){
                filename=gsub("\\.[^.]*$","",input$file$name)
                }
                paste(filename,'pdf', sep=".")
            },
            content <- function(file) {
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                setwd(owd)
                if("mdl1" %in% input$checkbox2 & ("mdl2" %in% input$checkbox2)==FALSE ){
                    src <- normalizePath('myreport1.Rmd')
                    file.copy(src, 'myreport1.Rmd')
                    out <- render('myreport1.Rmd',pdf_document())
                    }
                
                else if("mdl2" %in% input$checkbox2 & ("mdl1" %in% input$checkbox2)==FALSE ){
                    src <- normalizePath('myreport2.Rmd')
                    file.copy(src, 'myreport2.Rmd')
                    out <- render('myreport2.Rmd',pdf_document())
                    }
                else if("mdl1" %in% input$checkbox2 & "mdl2" %in% input$checkbox2 ){
                    src <- normalizePath('myreport.Rmd')
                    file.copy(src, 'myreport.Rmd')
                    out <- render('myreport.Rmd',pdf_document())
                }
                file.rename(out, file)
                
            }
    )
    
    outputOptions(output, "plots1", suspendWhenHidden = FALSE)
    outputOptions(output, "plots2", suspendWhenHidden = FALSE)

})


