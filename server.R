suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(googleVis))
library(RCmodels)
library(ggplot2)
library(xlsx)
library(Cairo)
library(rmarkdown)

options(shiny.usecairo=T)



vals<-reactiveValues(keeprows=NULL)
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
        cleandata=clean(input$file,dummy=dummy,force=force,keeprows=vals$keeprows,shiny=TRUE,advanced=input$checkboxA,exclude=input$checkboxY,excludedates=input$dates, includedates=input$slider)
        if(length(vals$keeprows)==0 ){
        vals$keeprows= rep(TRUE,nrow(cleandata$wq))
        }
        return(cleandata)
         
    })
    output$hakk <- renderPrint({
    input$dates
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
        tafla=NULL
        rctafla=NULL
        outputlist=list()
        if(!is.null(plotlist$qvdata)) {
            realdata=plotlist$realdata
            simdata=plotlist$simdata
            keep=cleandata$qvdata_before[vals$keeprows, ,drop=FALSE]
            exclude=cleandata$qvdata_before[!vals$keeprows, ,drop=FALSE]
            
            if ("raun" %in% input$checkbox){
                rcraun=ggplot(simdata)+theme_bw()+geom_point(data=keep,aes(Q,W))+geom_path(aes(exp(fit),W))+
                    geom_path(aes(exp(lower),W),linetype="dashed")+geom_path(aes(exp(upper),W),linetype="dashed")+
                    ggtitle(paste("Rating curve for",input$name))+ylab("W  [m]")+xlab(expression(paste("Q  [",m^3,'/s]',sep='')))+
                    theme(plot.title = element_text(vjust=2))+coord_cartesian(xlim = ranges1$x, ylim = ranges1$y)+
                    geom_point(data=exclude,aes(Q,W),fill=NA,col="black",alpha=0.75,shape=21)
                if(any(dim(dummy))){
                    rcraun=rcraun+geom_point(data=dummy,aes(Q,W),fill="red",col="red")
                }
                if(any(dim(force))){
                    rcraun=rcraun+geom_point(data=force,aes(Q,W),fill="blue",col="blue")
                }
                outputlist$rcraun=rcraun
            }
            if("log" %in% input$checkbox){
                rclog=ggplot(realdata)+geom_path(mapping=aes(fit,l_m))+theme_bw()+geom_point(mapping=aes(Q,l_m))+geom_path(mapping=aes(lower,l_m),linetype="dashed")+
                    geom_path(mapping=aes(upper,l_m),linetype="dashed")+ggtitle(paste("Rating curve for",input$name,"(log scale)"))+
                    ylab(expression(log(W-hat(c))))+xlab("log(Q)")+theme(plot.title = element_text(vjust=2))
                
                outputlist$rclog=rclog
            }
            
            
            if ("leifraun" %in% input$checkbox){
                
                rcleifraun=ggplot(realdata)+geom_point(aes(W,residraun),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_path(aes(W,residupper),linetype="dashed")+geom_path(aes(W,residlower),linetype="dashed")+ylab(expression(paste("Q - ",hat(Q) ,"  [",m^3,'/s]',sep='')))+
                    ggtitle("Residual plot")+xlab("W  [m]")+theme(plot.title = element_text(vjust=2))
                
                outputlist$rcleifraun=rcleifraun
            } 
            if("leiflog" %in% input$checkbox){
                rcleiflog=ggplot(realdata)+geom_point(aes(l_m,residlog),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_abline(intercept = 2, slope = 0,linetype="dashed")+geom_abline(intercept = -2, slope = 0,linetype="dashed")+
                    ylab(expression(epsilon[i]))+ggtitle("Residual plot (log scale)")+xlab(expression(log(W-hat(c))))+
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
        tafla=NULL
        outputlist=list()
        if(!is.null(plotlist$qvdata)) {
            ypodata=plotlist$ypodata
            betadata=plotlist$betadata
            realdata=plotlist$realdata
            keep=cleandata$qvdata_before[vals$keeprows, ,drop=FALSE]
            exclude=cleandata$qvdata_before[!vals$keeprows, ,drop=FALSE]
            
            if ("raun" %in% input$checkbox){
                rcraun=ggplot(ypodata)+theme_bw()+geom_point(data=keep,aes(Q,W))+geom_path(aes(exp(fit),W))+
                    geom_path(aes(exp(lower),W),linetype="dashed")+geom_path(aes(exp(upper),W),linetype="dashed")+
                    ggtitle(paste("Rating curve for",input$name))+ylab("W [m]")+xlab(expression(paste("Q  [",m^3,'/s]',sep='')))+
                    theme(plot.title = element_text(vjust=2))+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)+
                    geom_point(data=exclude,aes(Q,W),fill=NA,col="black",alpha=0.75,shape=21)
                if(any(dim(dummy))){
                    rcraun=rcraun+geom_point(data=dummy,aes(Q,W),fill="red",col="red")
                }
                if(any(dim(force))){
                    rcraun=rcraun+geom_point(data=force,aes(Q,W),fill="blue",col="blue")
                }
                outputlist$rcraun=rcraun
            }
            if("log" %in% input$checkbox){
                rclog=ggplot(realdata)+geom_path(mapping=aes(fit,l_m))+theme_bw()+geom_point(mapping=aes(Q,l_m))+geom_path(mapping=aes(lower,l_m),linetype="dashed")+
                    geom_path(mapping=aes(upper,l_m),linetype="dashed")+ggtitle(paste("Rating curve for",input$name,"(log scale)"))+
                    ylab(expression(log(W-hat(c))))+xlab("log(Q)")+theme(plot.title = element_text(vjust=2))
                
                outputlist$rclog=rclog
            }
            
            
            if ("leifraun" %in% input$checkbox){
                rcleifraun=ggplot(realdata)+geom_point(aes(W,residraun),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_path(aes(W,residupper),linetype="dashed")+geom_path(aes(W,residlower),linetype="dashed")+ylab(expression(paste("Q - ",hat(Q) ,"  [",m^3,'/s]',sep='')))+
                    ggtitle("Residual plot")+xlab("W  [cm]")+theme(plot.title = element_text(vjust=2))
                
                outputlist$rcleifraun=rcleifraun
            } 
            if("leiflog" %in% input$checkbox){
                rcleiflog=ggplot(realdata)+geom_point(aes(l_m,residlog),color="red")+theme_bw()+geom_abline(intercept = 0, slope = 0)+
                    geom_abline(intercept = 2, slope = 0,linetype="dashed")+geom_abline(intercept = -2, slope = 0,linetype="dashed")+
                    ylab(expression(epsilon[i]))+ggtitle("Residual plot (log scale)")+xlab(expression(log(W-hat(c))))+
                    theme(plot.title = element_text(vjust=2))
                
                
                outputlist$rcleiflog=rcleiflog
            }
            smoothbeta=ggplot(data=betadata)+geom_path(aes(W,fit))+
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
  
    output$fitrctafla1 <- renderGvis({
        if(!is.null(model1())){
            fitrctafla1=as.data.frame(model1()$fitrctafla)
            gvisTable(fitrctafla1,options=list(
                page='enable',
                pageSize=30,
                width=550
            ))
        }
        
    })
    output$lowerrctafla1 <- renderGvis({
        if(!is.null(model1())){
            lowerrctafla1=as.data.frame(model1()$lowerrctafla)
            gvisTable(lowerrctafla1,options=list(
                page='enable',
                pageSize=30,
                width=550
            ))
        }
        
    })
    output$upperrctafla1 <- renderGvis({
        if(!is.null(model1())){
            upperrctafla1=as.data.frame(model1()$upperrctafla)
            gvisTable(upperrctafla1,options=list(
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
    output$fitrctafla2 <- renderGvis({
        if(!is.null(model2())){
            fitrctafla2=as.data.frame(model2()$fitrctafla)
            gvisTable(fitrctafla2,options=list(
                page='enable',
                pageSize=30,
                width=550
            ))
        
        } 
    })
    output$lowerrctafla2 <- renderGvis({
        if(!is.null(model2())){
            lowerrctafla2=as.data.frame(model2()$lowerrctafla)
            gvisTable(lowerrctafla2,options=list(
                page='enable',
                pageSize=30,
                width=550
            ))
            
        } 
    })
    output$upperrctafla2 <- renderGvis({
        if(!is.null(model2())){
            upperrctafla2=as.data.frame(model2()$upperrctafla)
            gvisTable(upperrctafla2,options=list(
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
            qvdata=as.data.frame(data()$qvdata_before)
            res <- nearPoints(qvdata, input$plot1_click,xvar = "Q", yvar = "W", allRows = TRUE,threshold=5)
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
            qvdata=data()$qvdata_before
            res <- nearPoints(qvdata, input$plot5_click,xvar = "Q", yvar = "W", allRows = TRUE,threshold=5)
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
    observeEvent(input$reset,{
        wq=data()$wq
        vals$keeprows=rep(TRUE,nrow(wq))
        dummy$W=NULL
        dummy$Q=NULL
        force$W=NULL
        force$Q=NULL
    })
    
    output$xlsxexport <- downloadHandler(
        filename= function(){
            name=input$name
            if(nchar(name)==0){
                name="River1"
            }
            paste(name,'xlsx',sep=".")
        },
        content = function(file){
            wb <- createWorkbook()
            saveWorkbook(wb,file)
            tablelist=list()
            if(!is.null(model1())){
                tablelist$data1=model1()$tafla
                tablelist$fitfullrc1=model1()$fitrctafla
                tablelist$lowerfullrc1=model1()$lowerrctafla
                tablelist$upperfullrc1=model1()$upperrctafla
                tablelist$plotfullrc1=model1()$plottafla
            }
            if(!is.null(model2())){
                tablelist$data2=model2()$tafla
                tablelist$fitfullrc2=model2()$fitrctafla
                tablelist$lowerfullrc2=model2()$lowerrctafla
                tablelist$upperfullrc2=model2()$upperrctafla
                tablelist$plotfullrc2=model2()$plottafla
            }
            lapply(names(tablelist),function(x) write.xlsx(tablelist[[x]],file,sheetName=x,append=TRUE,row.names=FALSE))
        }
    )
    
    observeEvent(input$downloadword, {
        names1=c('image1_model1','image2_model1','image3_model1','image4_model1')
        names2=c('image1_model2','image2_model2','image3_model2','image4_model2','image5_model2')
        if(length(plotratingcurve1())!=0){
            for(i in 1:length(plotratingcurve1())){
                
                postscript(file=paste(names1[i],'eps',sep='.'),width = 10, height = 7.5)
                print(plotratingcurve1()[[i]])
                dev.off()
            }
        }
        if(length(plotratingcurve2())!=0){
            for(i in 1:length(plotratingcurve2())){
                
                pdf(paste(names2[i],'pdf',sep='.'))
                print(plotratingcurve2()[[i]])
                dev.off()
            }
        }
    })
    output$downloadImages <- downloadHandler(
        filename = function() {
            filename=gsub("\\.[^.]*$","",input$file$name)
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
                filename=gsub("\\.[^.]*$","",input$file$name)
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

