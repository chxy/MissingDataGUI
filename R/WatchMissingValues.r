globalVariables(c("observation", "variable","Reordered_Observation"))
##' The Main Window of Missing Data GUI.
##'
##' This function is to open the missing data GUI. The widgets shown
##' in the GUI include: a table of all variables in the dataset, a
##' checkbox group of categorical variables to condition on, a table
##' of variables which have missing values to coloy by, a radio of
##' imputation methods, a radio of graph types, three command buttons,
##' and a graphics device. In this GUI the user can: 1)change the name
##' and class of the selected variable; 2)look at the numeric summary
##' for the missing values in the selected variables; 3)look at the
##' plot of imputed data, under one of the imputation methods and one
##' of the graph types and one color-by variable, with or without the
##' conditions; 4)export the imputed data as well as the missing
##' shadow matrix, and save them to a data file(csv).
##'
##' The missing data GUI consists of two tabs. In the summary tab,
##' there are a list of all variables, a list of variables having
##' missing values to color by, two radios for imputation methods and
##' graph types respectively, a checkbox group for the conditional
##' variables, four buttons and a graphics device. In the help tab,
##' the layout is the same as the summary tab.  But when the users
##' move their mouse on those widgets, or click any of those radios or
##' buttons, the functions of all widgets will be described at the
##' place of the graphics device. The attributes of the variables can
##' be changed. If the user double clicks on any variables in the top
##' left table of missing-data GUI, an attribute window will pop
##' up. Then the name could be edited, and the class could be changed
##' to one of the four classes: integer, numeric, factor, and
##' character. When a numeric variable is changed to a categorical
##' variable, the condtions in the bottom left checkbox group will be
##' updated. If the list of the color by variables is very long, the
##' selector allows text entry to find the variable when this widget
##' is active.
##' @param h A list with components obj referring to the button "Watch
##' Missing Values" in \code{\link{MissingDataGUI}}.
##' @param data A data frame which is shown in the missing-data
##' GUI. If it is null, then parameter gt must not be null.
##' @param gt A widget created by gtable(). It should be passed from
##' \code{\link{MissingDataGUI}}.
##' @param ... Other parameters to be passed to this function.
##' @return NULL
##' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
##' @importFrom reshape2 melt
##' @importFrom reshape add.all.combinations
##' @importFrom grid viewport
##' @examples
##' if(interactive()){
##' data(tao)
##' WatchMissingValues(data=tao)
##' data(brfss)
##' WatchMissingValues(data=brfss)
##' }
##'
WatchMissingValues = function(h, data=NULL, gt=NULL, ...){
  if (is.null(data) & is.null(gt)) {
    gmessage("There's no input.", icon="error")
    return()
  }
  
  if (is.null(data)) {
    if (length(svalue(gt)) == 0) {
      gtfile = gt[1,]
    } else {
      gtfile = svalue(gt)[1]
    }
    dataset = read.csv(file = gtfile, header = TRUE)
  } else {
    dataset = data
  }
  
  rows = nrow(dataset)
  cols = ncol(dataset)
  halfcol = as.integer(cols/2)
  dataclass = as.character(sapply(dataset, class))
  vNApct = sapply(dataset, function(avec){mean(is.na(avec))})
  
  if (sum(vNApct)==0) {
    if (cols%%2==0) {
      chr_shadowmatrix = unlist(sapply(dataset[,(halfcol+1):cols],as.character))
      if (all(dataclass[(halfcol+1):cols]=="logical") | 
        all(chr_shadowmatrix %in% c("TRUE","FALSE")) | 
        all(chr_shadowmatrix %in% c("0","1"))) {
        imp_dat = data.frame(dataset[,1:halfcol],row_number=1:rows)
        tmp = dataset[,1:halfcol]
        tmp[sapply(dataset[,(halfcol+1):cols],as.logical)] = NA
        dataset = tmp
        is_imputed_data = TRUE
        gmessage(paste('There are no missing values in this data set. The last', halfcol, 
                       'columns are used as the shadow matrix.'), icon = "info")
      } else {
        is_imputed_data = FALSE
        gmessage('There are no missing values in this data set. If the data is combined 
                  with a shadow matrix, then the number of columns of the shadow matrix
                  is not the same as original data.', icon = "error")
      }
    } else {
      is_imputed_data = FALSE
      gmessage('There are no missing values in this data set. If the data is combined 
                with a shadow matrix, then the number of columns of the shadow matrix
                is not the same as original data.', icon = "error")
    }
  } else {is_imputed_data = FALSE}
  vname = as.character(names(dataset))
  dataclass = as.character(sapply(dataset, class))
  NAcol = which(sapply(dataset, function(avec){all(is.na(avec))}))
  vNApct = sapply(dataset, function(avec){mean(is.na(avec))})

  #####------------------------------------------------------#####
  ##  VariableOptions is the handler when double clicking gt4.  ##
  ##  It gives a new window for                                 ##
  ##          editing the attributes of variables.              ##
  #####------------------------------------------------------#####
  VariableOptions = function(h, ...) {
    gt11input = gwindow("Attributes", visible = T, width = 300,
                        height = 200)
    gt11input0 = ggroup(horizontal = FALSE, container = gt11input,
                        expand = TRUE)
    gt11input1 = ggroup(container = gt11input0, expand = TRUE)
    gt11input11 = glabel("Name:", container = gt11input1)
    gt11input12 = gedit(text = svalue(gt11), container = gt11input1,
                        expand = TRUE)
    
    gt11input2 = ggroup(container = gt11input0, expand = TRUE)
    gt11input21 = glabel("Class:", container = gt11input2)
    gt11input22 = gcombobox(union(gt11[svalue(gt11, index = TRUE),
                                       3], c("integer", "numeric", "character", "factor")),
                            container = gt11input2, expand = TRUE)
    
    gt11input3 = ggroup(container = gt11input0, expand = TRUE)
    gt11input31 = gbutton("Ok", container = gt11input3, expand = TRUE,
                          handler = function(h, ...) {
                            if (svalue(gt11input12) != "") {
                              colnames(dataset)[colnames(dataset)==as.character(gt11[svalue(gt11, index = TRUE), 2])] <<- svalue(gt11input12)
                              tmpcolorby = radio125[,]
                              tmpcolorby[tmpcolorby==as.character(gt11[svalue(gt11, index = TRUE), 2])]=svalue(gt11input12)
                              gt11[svalue(gt11, index = TRUE), 2] = svalue(gt11input12)
                              gt11[svalue(gt11, index = TRUE), 3] = svalue(gt11input22)
                              check123[,] = gt11[gt11[,3] %in%
                                c('factor','logical','character'),2]
                              radio125[,] = tmpcolorby
                              dispose(gt11input)
                            }
                            else {
                              gmessage("Variable name could not be empty!")
                              svalue(gt11input12) = svalue(gt11)
                            }
                          })
    gt11input32 = gbutton("Cancel", container = gt11input3,
                          expand = TRUE, handler = function(h, ...) {
                            dispose(gt11input)
                          })
  }
  
  #####------------------------------#####
  ##  NumSmry is the handler of gb145.  ##
  ##  (gbutton: Numeric Summary)        ##
  #####------------------------------#####
  NumSmry = function(h,...) {
    name_select = svalue(gt11, index = TRUE)
    n = length(name_select)
    if (n == 0) {
      gmessage("Please select at least one variable!")
      return()
    }
    tmpdat = dataset[,gt11[name_select,2]]
    if (n>1) {
      totalmissingpct = mean(is.na(tmpdat))
      varmissingpct = mean(sapply(tmpdat,function(avec){any(is.na(avec))}))
      casemissingpct = 1-mean(complete.cases(tmpdat))
      No_of_Case_missing = table(apply(tmpdat, 1, function(avec){sum(is.na(avec))}))
      No_of_Case = rep(0,(n+1))
      No_of_Case[n+1-as.integer(names(No_of_Case_missing))]=No_of_Case_missing[names(No_of_Case_missing)]
      No_of_Case[n+1] = sum(complete.cases(tmpdat))
      missingsummary = data.frame(No_of_miss_by_case=n:0,
                                  No_of_Case=No_of_Case,
                                  Percent=round(No_of_Case/nrow(tmpdat)*100,1))
    } else {
      totalmissingpct = mean(is.na(tmpdat))
      varmissingpct = ifelse(totalmissingpct>0, 1, 0)
      casemissingpct = 1-mean(complete.cases(tmpdat))
      No_of_Case_missing = sum(is.na(tmpdat))
      No_of_Case = c(No_of_Case_missing, length(tmpdat)-No_of_Case_missing)
      missingsummary = data.frame(No_of_miss_by_case=1:0,
                                  No_of_Case=No_of_Case,
                                  Percent=round(No_of_Case/length(tmpdat)*100,1))
    }
    missingsummary = missingsummary[order(missingsummary$No_of_miss_by_case, decreasing=FALSE),]
    
    NumSumforMisVal <- gwindow("Numeric Summary for Missing Values", visible = T, width = 350, height = 300, parent = combo1)
    groupN1 = ggroup(container = NumSumforMisVal, horizontal = FALSE, expand = TRUE)
    labelN11 = glabel('Missing:', container=groupN1, pos=0)
    labelN12 = glabel(paste("    ",round(totalmissingpct*100,2),
                            "% of the numbers",sep=""), container=groupN1)
    labelN13 = glabel(paste("    ",round(varmissingpct*100,2),
                            "% of variables",sep=""), container=groupN1)
    labelN14 = glabel(paste("    ",round(casemissingpct*100,2),
                            "% of samples",sep=""), container=groupN1)
    
    groupN15= ggroup(container = groupN1, use.scrollwindow = TRUE,
                     horizontal = FALSE, expand = TRUE)
    missingsummary$No_of_miss_by_case = as.integer(missingsummary$No_of_miss_by_case)
    missingsummary$No_of_Case = as.integer(missingsummary$No_of_Case)
    missingsummary$Percent = as.character(missingsummary$Percent)
    tableN15 = gtable(missingsummary, container=groupN15, expand = TRUE)
  }
  
  #####----------------------------#####
  ##  Graph is the handler of gb144.  ##
  ##  (gbutton: Watch the data)       ##
  #####----------------------------#####
  Graph = function(h,...) {
    graphics.off()
    
    name_select = svalue(gt11, index = TRUE)
    imp_method = svalue(gr142)
    graphtype = svalue(gr143)
    n = length(name_select)
    cond = check123[svalue(check123,index=T)]
    if (length(cond)==0) cond = NULL
    if (imp_method=='Below 10%') cond = NULL
    colorby = as.character(svalue(radio125))
    if (length(colorby)==0) {
      colorby = "Missing Any Variables"
    } else {
      if ("Missing Any Variables" %in% colorby) {
        colorby = "Missing Any Variables"
      } else {
        if ('Missing on Selected Variables' %in% colorby) {
          colorby = 'Missing on Selected Variables'
        }
      }
    }
    
    if (n == 0) {
      gmessage("Please select at least one variable!",
               icon = "error")
      return()
    }
    
    if (!exists('imp_dat')) {
      dat = imputation(origdata=dataset[,c(gt11[name_select,2],cond),drop=FALSE],
                       method=imp_method, vartype=as.character(gt11[name_select,3]),
                       missingpct=as.numeric(as.character(gt11[name_select,4])),
                       condition=cond)
      dat = data.frame(dat)
      colnames(dat)[1:n]=c(gt11[name_select,2])
    } else {
      dat = data.frame(imp_dat[,c(gt11[name_select,2])],imp_dat[,ncol(imp_dat)])
    }
    for (i in 1:n){
      eval(parse(text=paste("dat[,i]=as.",as.character(gt11[name_select,3])[i],"(as.character(dat[,i]))",sep="")))
    }
    if (colorby=='Missing on Selected Variables') {
      Missing <- !complete.cases(dataset[,gt11[name_select,2]])
    } else {
      if (colorby=='Missing Any Variables') {
        Missing <- !complete.cases(dataset)
      } else {
        Missing <- !complete.cases(dataset[,colorby])
      }
    }
    Missing <- Missing[dat[,ncol(dat)]]
    
    if (graphtype=="Histogram/Barchart") {
      for (i in 1:n) {
        glay15[i, 1, expand = TRUE] = ggraphics(container = glay15, expand = TRUE)
        if (is.numeric(dat[,i]) #&
            #as.numeric(as.character(gt11[name_select,4]))[i]>0
            ) {
          tmpdat = data.frame(dat,Missing=Missing)
          print(qplot(tmpdat[,i],data=tmpdat,geom='histogram',
                      fill=Missing, xlab=names(tmpdat)[i]))
        }
        if (is.character(dat[,i])) {
          tmpdat = data.frame(dat,Missing=Missing)
          print(qplot(tmpdat[,i],data=tmpdat,geom='histogram',
                      fill=Missing, xlab=names(tmpdat)[i])+coord_flip())
        }
        if (is.factor(dat[,i]) &
          as.numeric(as.character(gt11[name_select,4]))[i]<1) {
          tmpdat = data.frame(dat,Missing=Missing)
          print(qplot(tmpdat[,i],data=tmpdat,geom='histogram',
                      fill=Missing, xlab=names(tmpdat)[i])+coord_flip())
        }
      }
    }
    if (graphtype=="Spinogram/Spineplot") {
      for (i in 1:n) {
        glay15[i, 1, expand = TRUE] = ggraphics(container = glay15, expand = TRUE)
        if (is.numeric(dat[,i])) {
          tmpdat = data.frame(dat,Missing=Missing)
          print(qplot(tmpdat[,i],data=tmpdat,geom='histogram',
                      fill=Missing, position="fill", xlab=names(tmpdat)[i]))
        }
        if (is.character(dat[,i])) {
          tmpdat = data.frame(dat,Missing=Missing)
          print(qplot(tmpdat[,i],data=tmpdat,geom='histogram',
                      fill=Missing, position="fill", xlab=names(tmpdat)[i])+coord_flip())
        }
        if (is.factor(dat[,i]) &
          as.numeric(as.character(gt11[name_select,4]))[i]<1) {
          tmpdat = data.frame(dat,Missing=Missing)
          print(qplot(tmpdat[,i],data=tmpdat,geom='histogram',
                      fill=Missing, position="fill", xlab=names(tmpdat)[i])+coord_flip())
        }
      }
    }
    if (graphtype=="Pairwise Plots") {
      if (n > 5) {
        gmessage("You selected more than five variables! Only the first five are displayed.", icon = "warning")
        name.select = name_select[1:5]
        n = 5
      }
      glay15[1, 1, expand = TRUE] = ggraphics(container = glay15, expand = TRUE)
      if (n==2) {
        print(qplot(dat[,1],dat[,2], color=Missing, geom='jitter',alpha=I(0.7),
                    size=I(3),xlab=colnames(dat)[1],ylab=colnames(dat)[2]) + 
              theme(legend.position='bottom'))
      } else {
        dat$Missings=factor(Missing)
        print(ggpairs(dat,columns=1:n,colour="Missings", fill="Missings",alpha=I(0.5)))
      }
    }
    if (graphtype=="Parallel Coordinates") {
      if (n==1) {
        gmessage('You only selected one variable. Cannot plot the parallel coordinates.',
		 icon = "error")
        return()
      }
      if (any(c('character','factor') %in% as.character(gt11[name_select,3]))){
        gmessage('The parallel coordinates plot is only drawn for numeric variables. Please choose the variables again.', icon = "error")
        return()
      }
      glay15[1, 1, expand = TRUE] = ggraphics(container = glay15, expand = TRUE)
      dat$Missing=Missing
      print(ggpcp(dat,vars=names(dat)[1:n])+geom_line(aes(colour=Missing))+ 
            theme(legend.position='bottom'))
    }
    if (graphtype=="Missingness Map"){
      glay15[1, 1, expand = TRUE] = ggraphics(container = glay15, expand = TRUE)
      Mapdat=data.frame(is.na(dataset[,gt11[name_select,2]]))
      Mapdat$observation=1:nrow(Mapdat)
      mapdat=melt(Mapdat,"observation")
      colnames(mapdat)[3]="Missing"
      mapdat$variable=factor(mapdat$variable,levels=rev(levels(mapdat$variable)),
                             labels=rev(levels(mapdat$variable)))
      print(qplot(observation,variable,data=mapdat,geom='tile',fill=Missing,main='Original Missingness Map'))
      
      glay15[2, 1, expand = TRUE] = ggraphics(container = glay15, expand = TRUE)
      Mapdat2=Mapdat[order(rowSums(Mapdat[,-(n+1)]),decreasing=TRUE),]
      Mapdat2=Mapdat2[,order(colSums(Mapdat2),decreasing=TRUE)]
      Mapdat2$Reordered_Observation=1:nrow(Mapdat2)
      mapdat=melt(Mapdat2[,-1],"Reordered_Observation")
      colnames(mapdat)[3]="Missing"
      mapdat$variable=factor(mapdat$variable,levels=rev(levels(mapdat$variable)),
                             labels=rev(levels(mapdat$variable)))
      print(qplot(Reordered_Observation,variable,data=mapdat,geom='tile',fill=Missing,main='Sorted by the number of missing in variables and observations'))
      
      glay15[3, 1, expand = TRUE] = ggraphics(container = glay15, expand = TRUE)
      dist31=dist(Mapdat[,-(n+1)])
      order31=hclust(dist31,"ward")$order
      Mapdat3=Mapdat[order31,-(n+1)]
      dist32=dist(t(Mapdat[,-(n+1)]))
      order32=hclust(dist32,"ward")$order
      Mapdat3=Mapdat3[,order32]
      Mapdat3$Reordered_Observation=1:nrow(Mapdat3)
      mapdat=melt(Mapdat3[,],"Reordered_Observation")
      colnames(mapdat)[3]="Missing"
      mapdat$variable=factor(mapdat$variable,levels=rev(levels(mapdat$variable)),
                             labels=rev(levels(mapdat$variable)))
      print(qplot(Reordered_Observation,variable,data=mapdat,geom='tile',fill=Missing,main='Sorted by hierarchical clustering of missingness'))
    }
    
  }
  
  #####---------------------------------#####
  ##  ExportData is the handler of gb146.  ##
  ##  (gbutton: Export the data)           ##
  #####---------------------------------#####
  ExportData = function(h,...){
    name_select = svalue(gt11, index = TRUE)
    imp_method = svalue(gr142)
    FileSuffix = paste('_impute_',gsub('[^a-z,A-Z,0-9]',"",imp_method),sep='')
    graphtype = svalue(gr143)
    n = length(name_select)
    cond = check123[svalue(check123,index=T)]
    if (length(cond)==0) cond = NULL
    
    if (n == 0) {
        svalue(gt11) = 1:nrow(gt11)
        name_select = svalue(gt11, index = TRUE)
        n = length(name_select)
        gmessage("All variables are selected to export.")
    }
    
    if (!exists('imp_dat')) {
        dat = imputation(origdata=dataset[,c(gt11[name_select,2],cond)],
                         method=imp_method, vartype=as.character(gt11[name_select,3]),
                         missingpct=as.numeric(as.character(gt11[name_select,4])),
                         condition=cond)
        dat = data.frame(dat[,-ncol(dat)],
                         is.na(dataset[dat[,ncol(dat)],gt11[name_select,2]]))
        colnames(dat)[1:(2*n)]=c(gt11[name_select,2],
                                 paste('Missing', gt11[name_select,2], sep='_'))
    } else {
        dat = imp_dat
    }
    
    entire_dat = data.frame(dataset,is.na(dataset))
    colnames(entire_dat)=c(colnames(dataset),paste('Missing',colnames(dataset),sep='_'))
    entire_dat[,colnames(dat[,1:(2*n)])] = dat[,1:(2*n)]
    
    ExpData = function(opa,opb){
        opa = svalue(opa)
        opb = svalue(opb)
        if (opa=='All columns' && opb) return(entire_dat)
        if (opa=='Selected columns' && opb) return(dat[,1:(2*n)])
        if (opa=='All columns' && !opb) return(entire_dat[,1:ncol(dataset)])
        if (opa=='Selected columns' && !opb) return(dat[,1:n])
    }
    
    gExport = gwindow("Export Options", visible = T, width = 300, height = 200)
    ExGroup = ggroup(container = gExport, expand = TRUE, horizontal = FALSE)
    ExFrame = gframe(text = "Export", container = ExGroup)
    ExRadio = gradio(c('All columns','Selected columns'), container = ExFrame)
    ExCheck = gcheckbox(text = "Export the imputed data with a shadow matrix which consists of TRUE's and FALSE's.\nTRUE's represent for the missing values, and FALSE's represent for non-missings.", checked=TRUE, container = ExGroup)
    ExGroupB = ggroup(container = ExGroup, expand = FALSE, horizontal = TRUE)
    ExLabel2 = glabel(text = "Directory  ", container = ExGroupB)
    ExpText2 = gtext(text = getwd(), width=400, height=20, container = ExGroupB)
    ExButtoB = gbutton(text = "Browse...", container = ExGroupB, handler = function(h,...){
      ExFile = gfile(text = 'filename', type = 'selectdir')
      if (!is.na(ExFile)) svalue(ExpText2) = ExFile
    })
    ExGroupT = ggroup(container = ExGroup, expand = FALSE, horizontal = TRUE)
    ExLabel1 = glabel(text = "File name ", container = ExGroupT)
    ExpText1 = gtext(text = "", width=400, height=30, container = ExGroupT)
    ExGroupB = ggroup(container = ExGroup, expand = TRUE, horizontal = TRUE)
    addSpace(ExGroupB, 20)
    ExButto1 = gbutton(text = "Save as .csv format", container = ExGroupB, handler=function(h,...){
        filename = paste(gsub('\\.csv$','',svalue(ExpText1)),FileSuffix,'.csv',sep='')
        write.csv(ExpData(ExRadio,ExCheck), file=paste(svalue(ExpText2),'/',filename,sep=''), row.names=FALSE)
        gmessage("The data is exported!")
    })
    addSpace(ExGroupB, 20)
    ExButto2 = gbutton(text = "Save as .rda format", container = ExGroupB, handler=function(h,...){
        filename = paste(gsub('\\.rda$','',svalue(ExpText1)),FileSuffix,'.rda',sep='')
        fexdat = ExpData(ExRadio,ExCheck)
        save(fexdat, file=paste(svalue(ExpText2),'/',filename,sep=''))
        gmessage("The data is exported!")
    })
    addSpace(ExGroupB, 20)
    ExButto3 = gbutton(text = "Save as data frame", container = ExGroupB, handler=function(h,...){
        filename = svalue(ExpText1)
        fexdat = ExpData(ExRadio,ExCheck)
        eval(parse(text=paste(filename,'<<- fexdat')))
        gmessage(paste("An R data frame is saved with the name ",filename))
        })
  }
  
  #####---------------------------------#####
  ##  ExportData is the handler of gb148.  ##
  ##  (gbutton: Save the plot)             ##
  #####---------------------------------#####
  SavePlot = function(h,...){
    name_select = svalue(gt11, index = TRUE)
    imp_method = svalue(gr142)
    graphtype = svalue(gr143)
    n = length(name_select)
    cond = check123[svalue(check123,index=T)]
    if (length(cond)==0) cond = NULL
    if (imp_method=='Below 10%') cond = NULL
    colorby = as.character(svalue(radio125))
    if (length(colorby)==0) {
      colorby = "Missing Any Variables"
    } else {
      if ("Missing Any Variables" %in% colorby) {
        colorby = "Missing Any Variables"
      } else {
        if ('Missing on Selected Variables' %in% colorby) {
          colorby = 'Missing on Selected Variables'
        }
      }
    }
    
    if (n == 0) {
      gmessage("Please select at least one variable!",
               icon = "error")
      return()
    }
    
    if (!exists('imp_dat')) {
      dat = imputation(origdata=dataset[,c(gt11[name_select,2],cond)],
                       method=imp_method, vartype=as.character(gt11[name_select,3]),
                       missingpct=as.numeric(as.character(gt11[name_select,4])),
                       condition=cond)
      dat = data.frame(dat)
      colnames(dat)[1:n]=c(gt11[name_select,2])
    } else {
      dat = data.frame(imp_dat[,c(gt11[name_select,2])],imp_dat[,ncol(imp_dat)])
    }
    
    for (i in 1:n){
      eval(parse(text=paste("dat[,i]=as.",as.character(gt11[name_select,3])[i],"(as.character(dat[,i]))",sep="")))
    }
    if (colorby=='Missing on Selected Variables') {
      Missing <- !complete.cases(dataset[,gt11[name_select,2]])
    } else {
      if (colorby=='Missing Any Variables') {
        Missing <- !complete.cases(dataset)
      } else {
        Missing <- !complete.cases(dataset[,colorby])
      }
    }
    Missing <- Missing[dat[,ncol(dat)]]
    
    if (graphtype=="Histogram/Barchart") {
      savename = gfile(type="save")
      for (i in 1:n) {
        png(filename = paste(savename,'_hist_',i,'.png',sep=''),width = 7, height = 5,units = "in", res=90)
        if (is.numeric(dat[,i]) #&
            #as.numeric(as.character(gt11[name_select,4]))[i]>0
            ) {
          tmpdat = data.frame(dat,Missing=Missing)
          print(qplot(tmpdat[,i],data=tmpdat,geom='histogram',
                      fill=Missing, xlab=names(tmpdat)[i]))
        }
        if (is.character(dat[,i])) {
          tmpdat = data.frame(dat,Missing=Missing)
          print(qplot(tmpdat[,i],data=tmpdat,geom='histogram',
                      fill=Missing, xlab=names(tmpdat)[i])+coord_flip())
        }
        if (is.factor(dat[,i]) &
          as.numeric(as.character(gt11[name_select,4]))[i]<1) {
          tmpdat = data.frame(dat,Missing=Missing)
          print(qplot(tmpdat[,i],data=tmpdat,geom='histogram',
                      fill=Missing, xlab=names(tmpdat)[i])+coord_flip())
        }
        dev.off()
      }
    }
    if (graphtype=="Spinogram/Spineplot") {
      savename = gfile(type="save")
      for (i in 1:n) {
        png(filename = paste(savename,'_spinogram_',i,'.png',sep=''),width = 7, height = 5,units = "in", res=90)
        if (is.numeric(dat[,i])) {
          tmpdat = data.frame(dat,Missing=Missing)
          print(qplot(tmpdat[,i],data=tmpdat,geom='histogram',
                      fill=Missing, position="fill", xlab=names(tmpdat)[i]))
        }
        if (is.character(dat[,i])) {
          tmpdat = data.frame(dat,Missing=Missing)
          print(qplot(tmpdat[,i],data=tmpdat,geom='histogram',
                      fill=Missing, position="fill", xlab=names(tmpdat)[i])+coord_flip())
        }
        if (is.factor(dat[,i]) &
          as.numeric(as.character(gt11[name_select,4]))[i]<1) {
          tmpdat = data.frame(dat,Missing=Missing)
          print(qplot(tmpdat[,i],data=tmpdat,geom='histogram',
                      fill=Missing, position="fill", xlab=names(tmpdat)[i])+coord_flip())
        }
        dev.off()
      }
    }
    if (graphtype=="Pairwise Plots") {
      if (n > 5) {
        gmessage("You selected more than five variables! Only the first five are displayed.", icon = "warning")
        name.select = name_select[1:5]
        n = 5
      }
      savename = gfile(type="save")
      png(filename = paste(savename,'_pairwise.png',sep=''), width = 2*n, height = 2*n, units = "in", res=90)
      if (n==2) {
        print(qplot(dat[,1],dat[,2], color=Missing, geom='jitter',alpha=I(0.7),
                    size=I(3),xlab=colnames(dat)[1],ylab=colnames(dat)[2]))
      } else {
        dat$Missings=factor(Missing)
		print(print(ggpairs(dat,columns=1:n,colour="Missings", fill="Missings",alpha=I(0.5))))
      }
      dev.off()
    }
    if (graphtype=="Parallel Coordinates") {
      if (n==1) {
        gmessage('You only selected one variable. Cannot plot the parallel coordinates.',
		 icon = "error")
        return()
      }
      if (any(c('character','factor') %in% as.character(gt11[name_select,3]))){
        gmessage('The parallel coordinates plot is only drawn for numeric variables. Please choose the variables again.', icon = "error")
        return()
      }
      savename = gfile(type="save")
      png(filename = paste(savename,'_pcp.png',sep=''),width = (n+2), height = 4, units = "in", res=90)
      dat$Missing=Missing
      print(ggpcp(dat,vars=names(dat)[1:n])+geom_line(aes(colour=Missing)))
      #	+ geom_point(subset=dat[Missing,1:n],colour='blue'))
      dev.off()
    }
    if (graphtype=="Missingness Map"){
      savename = gfile(type="save")
      png(filename = paste(savename,'_map_1.png',sep=''),width = 6, height = max(4,round(n/8)), units = "in", res=90)
      Mapdat=data.frame(is.na(dataset[,gt11[name_select,2]]))
      Mapdat$observation=1:nrow(Mapdat)
      mapdat=melt(Mapdat,"observation")
      colnames(mapdat)[3]="Missing"
      print(qplot(observation,variable,data=mapdat,geom='tile',fill=Missing))
      dev.off()

      png(filename = paste(savename,'_map_2.png',sep=''),width = 6, height = max(4,round(n/8)), units = "in", res=90)
      Mapdat2=Mapdat[order(rowSums(Mapdat[,-(n+1)]),decreasing=TRUE),]
      Mapdat2=Mapdat2[,order(colSums(Mapdat2),decreasing=TRUE)]
      Mapdat2$Reordered_Observation=1:nrow(Mapdat2)
      mapdat=melt(Mapdat2[,-1],"Reordered_Observation")
      colnames(mapdat)[3]="Missing"
      mapdat$variable=factor(mapdat$variable,levels=rev(levels(mapdat$variable)),
                             labels=rev(levels(mapdat$variable)))
      print(qplot(Reordered_Observation,variable,data=mapdat,geom='tile',fill=Missing))
      dev.off()
      
      png(filename = paste(savename,'_map_3.png',sep=''),width = 6, height = max(4,round(n/8)), units = "in", res=90)
      dist31=dist(Mapdat[,-(n+1)])
      order31=hclust(dist31,"ward")$order
      Mapdat3=Mapdat[order31,-(n+1)]
      dist32=dist(t(Mapdat[,-(n+1)]))
      order32=hclust(dist32,"ward")$order
      Mapdat3=Mapdat3[,order32]
      Mapdat3$Reordered_Observation=1:nrow(Mapdat3)
      mapdat=melt(Mapdat3[,],"Reordered_Observation")
      colnames(mapdat)[3]="Missing"
      mapdat$variable=factor(mapdat$variable,levels=rev(levels(mapdat$variable)),
                             labels=rev(levels(mapdat$variable)))
      print(qplot(Reordered_Observation,variable,data=mapdat,geom='tile',fill=Missing))
      dev.off()

    }
    
  }
  
  #####-------------------------------#####
  ##  New window for missing values      ##
  #####-------------------------------#####
  combo1 <- gwindow("Missing Values", visible = T, width = 1000,
                    height = 750)
  tab <- gnotebook(container = combo1)
  
  #####------------------------------------------------#####
  ##  In the first tab we can:                            ##
  ##  (1) Watch and change the name or type of variables. ##
  ##  (2) Numeric or graphic summary.                     ##
  ##  (3) Save the imputation and plots.                  ##
  #####------------------------------------------------#####
  group11 = ggroup(container = tab, label = "Summary", expand = TRUE, horizontal = FALSE)
  group1100 = ggroup(container = group11, expand = TRUE)
  group12 = ggroup(container = group1100, use.scrollwindow = TRUE,
                   horizontal = FALSE, expand = TRUE)
  size(group12) = c(200,750)
  nametable = data.frame(Items=1:length(vname), Variables=vname,
                         Class=dataclass, NApct=as.character(round(vNApct,3)))
  nametable$Variables = as.character(nametable$Variables)
  nametable$Class = as.character(nametable$Class)
  gt11 = gtable(nametable, multiple = T, container = group12,
                expand = TRUE, chosencol = 2)
  size(gt11) = c(150,600)
  addhandlerdoubleclick(gt11, handler = VariableOptions)
  
  label121 = glabel('Categorical variables to condition on',container=group12)
  check123 = gcheckboxgroup(nametable$Variables[nametable$Class %in%
    c('factor','logical','character')], container=group12, use.table=TRUE,
                            handler = Graph)
  size(check123) = c(150,150)
  
  group13 = ggroup(horizontal = FALSE, container = group1100,
                   expand = TRUE)
  group14 = ggroup(horizontal = TRUE, container = group13)
  size(group14) = c(500,160)
  tmpcolorby = data.frame(`Color by the missing of`= c('Missing Any Variables',
                                                       'Missing on Selected Variables',nametable[vNApct>0,2]))
  tmpcolorby[,1]=as.character(tmpcolorby[,1])
  radio125 = gtable(tmpcolorby, container=group14, expand=TRUE, multiple=TRUE)
  addHandlerKeystroke(radio125, handler = function(h,...){})
  gframe142 = gframe(text = "Imputation Method", container = group14)
  gr142 = gradio(c('Below 10%','Median','Mean','Random value',
                   'Regression','Nearest neighbor','Multiple Imputation','Mode'),
                 container = gframe142, handler = function(h,...){
                   if (svalue(gr142)=='Below 10%') {
                     svalue(check123) = FALSE
                   }
                 })
  if (is_imputed_data) {enabled(gr142) = FALSE}
  gframe143 = gframe(text = "Graph Type", container = group14)
  gr143 = gradio(c('Histogram/Barchart','Spinogram/Spineplot','Pairwise Plots',
                   'Parallel Coordinates','Missingness Map'), container = gframe143)
  
  group144 = ggroup(horizontal = FALSE, container = group14)
  gb145 = gbutton('Numeric summary', container = group144,
                  handler = NumSmry)
  gb144 = gbutton("P l o t", container = group144,
                  handler = Graph)
  gb146 = gbutton('Export the data', container = group144,
                  handler = ExportData)
  gb148 = gbutton('Save the plot', container = group144,
                  handler = SavePlot)
  gb147 = gbutton('Q u i t', container = group144,
                  handler = function(h,...){
                    dispose(combo1)
                  })
  
  group15 = ggroup(horizontal = FALSE, container = group13,
                   expand = TRUE, use.scrollwindow = TRUE)
  glay15 = glayout(container = group15, expand = TRUE, use.scrollwindow = TRUE)
 
  #####------------------------------------------------#####
  ##  In the second tab we can:                           ##
  ##  Look at the help documents.                         ##
  #####------------------------------------------------#####
  group21 = ggroup(container = tab, label = "Help", expand = TRUE, horizontal = FALSE)
  group2100 = ggroup(container = group21, expand = TRUE)
  group22 = ggroup(container = group2100, use.scrollwindow = TRUE,
                   horizontal = FALSE, expand = T)
  size(group22) = c(200,750)
  gt21 = gtable(nametable, multiple = T, container = group22,
                expand = TRUE, chosencol = 2)
  size(gt21) = c(150,600)
  addHandlerMouseMotion(gt21, handler = function(h,...){
    if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   This table displays all variables in the data set and reports the type and the percentage of missing values for each variable.\n\n   We can sort the variables by NA's percent.\n\n   If we doubleclick one row, we can change the variable name, as well as the data type."))
	})
  
  label221 = glabel('Categorical variables to condition on',container=group22)
  check223 = gcheckboxgroup(nametable$Variables[nametable$Class %in%
                            c('factor','logical','character')], 
                            container=group22, use.table=TRUE)
  size(check223) = c(150,150)
  addHandlerMouseMotion(check223, handler = function(h,...){
    if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   This list displays all categorical variables. We can make multiple selection on them.\n\n   Once we select one or more variables, the data set will be divided into small groups based on the selected categorical variable(s).\n\n   And the imputation will be made in each group.\n\n   If the imputation method is 'Below 10%', then the selected conditioning variables are ignored."))
	})
  
  group23 = ggroup(horizontal = FALSE, container = group2100,
                   expand = TRUE)
  group24 = ggroup(horizontal = TRUE, container = group23)
  size(group24) = c(500,160)
  radio225 = gtable(data.frame(`Color by the missing of`= c('Missing Any Variables',
                    'Missing on Selected Variables', nametable[vNApct>0,2])),
                    container = group24, expand=TRUE, handler=function(h,...){
                      if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   This list displays all variables which have missing values.\n\n   If the user chooses one of them, the color of the plot showing on the right will change based on whether the cases being NA on that variable or not.\n\n   The user can also choose several variables. Then the color of the plot will be based on whether the cases have missing values on any of those variable.\n\n   The first row 'Missing Any Variables' means whether this case being complete or not.\n\n   The first row 'Missing on Selected Variables' means whether the cases have missing values on any of the selected variable.\n\n   The widget allows text entry to find a particular variable if the list is quite long."))
	})
  addHandlerMouseMotion(radio225, handler = function(h,...){
    if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   This list displays all variables which have missing values.\n\n   If the user chooses one of them, the color of the plot showing on the right will change based on whether the cases being NA on that variable or not.\n\n   The user can also choose several variables. Then the color of the plot will be based on whether the cases have missing values on any of those variable.\n\n   The first row 'Missing Any Variables' means whether this case being complete or not.\n\n   The first row 'Missing on Selected Variables' means whether the cases have missing values on any of the selected variable.\n\n   The widget allows text entry to find a particular variable if the list is quite long."))
	})
  
  gframe242 = gframe(text = "Imputation Method", container = group24)
  gr242 = gradio(c('Below 10%','Median','Mean','Random value',
                   'Regression','Nearest neighbor','Multiple Imputation','Mode'),
                 container = gframe242, handler = function(h,...){
                   if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   This list displays all the imputation methods.\n\n   Users can select one of them if the data are not already imputed.\n\n      (1)'Below 10%' means NA's of one variable will be replaced by the value which equals to the minimum of the variable minus 10% of the range. Under this status the selected conditioning variables are ignored.\n\n      (2)'Median' means NA's will be replaced by the median of this variable (omit NA's).\n\n      (3)'Mean' means NA's will be replaced by the mean of this variable (omit NA's).\n\n      (4)'Random value' means NA's will be replaced by any values of this variable (omit NA's) which are randomly selected.\n\n      (5)'Regression' uses function 'aregImpute' from package 'Hmisc'. It requires at lease one case to be complete, and at least two variables to be selected.\n\n      (6)'Nearest neighbor' replaces NA's by its nearest neighbor. It requires at lease one case to be complete, at least two variables to be selected, and no character variables. It returns median for the case if all values in it are NA's.\n\n      (7)'Multiple Imputation' uses function 'imp.norm' from package 'norm'. It requires all selected variables to be numeric(at least integer), and at least two variables to be selected. Sometimes it cannot converge, then the programme will leave NA's without imputation.\n\n      (8)'Mode' is a method for imputing categorical variables. It requires all selected variables to be character or factor or logical. It will replace NA's by the mode of the variable (omit NA's)."))
	})
  addHandlerMouseMotion(gr242, handler = function(h,...){
    if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   This list displays all the imputation methods.\n\n   We can only select one of them.\n\n      (1)'Below 10%' means NA's of one variable will be replaced by the value which equals to the minimum of the variable minus 10% of the range. Under this status the selected conditioning variables are ignored.\n\n      (2)'Median' means NA's will be replaced by the median of this variable (omit NA's).\n\n      (3)'Mean' means NA's will be replaced by the mean of this variable (omit NA's).\n\n      (4)'Random value' means NA's will be replaced by any values of this variable (omit NA's) which are randomly selected.\n\n      (5)'Regression' uses function 'aregImpute' from package 'Hmisc'. It requires at lease one case to be complete, and at least two variables to be selected.\n\n      (6)'Nearest neighbor' replaces NA's by its nearest neighbor. It requires at lease one case to be complete, at least two variables to be selected, and no character variables. It returns median for the case if all values in it are NA's.\n\n      (7)'Multiple Imputation' uses function 'imp.norm' from package 'norm'. It requires all selected variables to be numeric(at least integer), and at least two variables to be selected. Sometimes it cannot converge, then the programme will leave NA's without imputation.\n\n      (8)'Mode' is a method for imputing categorical variables. It requires all selected variables to be character or factor or logical. It will replace NA's by the mode of the variable (omit NA's)."))
	})
  
  gframe243 = gframe(text = "Graph Type", container = group24)
  gr243 = gradio(c('Histogram/Barchart','Spinogram/Spineplot','Pairwise Plots',
                   'Parallel Coordinates','Missingness Map'), container = gframe243,
                 handler = function(h,...){
                   if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   This frame shows all plots we can make.\n\n      (1)'Histogram/Barchart' will display histograms (numeric variables) and barcharts(categorical variables) for each variable selected.\n\n      (2)'Spinogram/Spineplot' shows the spineplot for each selected variable.\n\n      (3)'Pairwise Scatterplots' displays n*(n-1)/2 scatterplots if we select n variables. When n>5, then only the first 5 variables are displayed.\n\n      When n=2, a scatterplot is drawn for the two variables. When 2<n<=5, the function 'ggpairs' from package 'GGally' is used.\n\n      (4)'Parallel Coordinates' displays parallel coordinates plot for the selected variables.\n\n      (5)'Missingness Map' shows the positions of missing values in all the observations from the variable selected, regardless the imputation."))
	})
  addHandlerMouseMotion(gr243, handler = function(h,...){
    if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   This frame shows all plots we can make.\n\n      (1)'Histogram/Barchart' will display histograms (numeric variables) and barcharts(categorical variables) for each variable selected.\n\n      (2)'Spinogram/Spineplot' shows the spineplot for each selected variable.\n\n      (3)'Pairwise Scatterplots' displays n*(n-1)/2 scatterplots if we select n variables. When n>5, then only the first 5 variables are displayed.\n\n      When n=2, a scatterplot is drawn for the two variables. When 2<n<=5, the function 'ggpairs' from package 'GGally' is used.\n\n      (4)'Parallel Coordinates' displays parallel coordinates plot for the selected variables.\n\n      (5)'Missingness Map' shows the positions of missing values in all the observations from the variable selected, regardless the imputation."))
	})
  
  group244 = ggroup(horizontal = FALSE, container = group24)
  gb245 = gbutton('Numeric summary', container = group244,
                  handler = function(h,...){
                    if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   Clicking this button will create another window which presents the numeric summaries for missing values.\n\n   In this summary window, the missing percentage of all the numbers, variables, and cases are presented.\n\n   Besides, there is a table of the missing levels. The table has n+1 rows, where n = # of selected variables. For each i in 0:n, the table gives the count of cases which have i missing values, as well as the percentage of those cases."))
		})
  addHandlerMouseMotion(gb245, handler = function(h,...){
    if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   Clicking this button will create another window which presents the numeric summaries for missing values.\n\n   In this summary window, the missing percentage of all the numbers, variables, and cases are presented.\n\n   Besides, there is a table of the missing levels. The table has n+1 rows, where n = # of selected variables. For each i in 0:n, the table gives the count of cases which have i missing values, as well as the percentage of those cases."))
	})
  
  gb244 = gbutton("P l o t", container = group244,
                  handler = function(h,...){
                    if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   Clicking this button will draw a plot based on the options the user chooses.\n\n   All the n variables the user selected should be displayed, except that when the graph type is 'Pairwise Scatterplots' and n>5, then only the first 5	variables are displayed."))
		})
  addHandlerMouseMotion(gb244, handler = function(h,...){
    if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   Clicking this button will draw a plot based on the options the user chooses.\n\n   All the n variables the user selected should be displayed, except that when the graph type is 'Pairwise Scatterplots' and n>5, then only the first 5	variables are displayed."))
	})
  
  gb246 = gbutton('Export the data', container = group244,
                  handler = function(h,...){
                    if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   Clicking this button will export the imputed data based on the other chosen options.\n\n   Users could name the exported data, and the information of imputation method will be completed in the file name automatically.\n\n   Two file formats are provided: csv and rda. For both formats, the number of columns in the exported data depends on the demand of users. There are 2*n columns if the user selected n variables. The first n columns are the imputed data, and the second n columns are the 'shadow matrix' which indicate whether the values are missing or not in the original dataset."))
		})
  addHandlerMouseMotion(gb246, handler = function(h,...){
    if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   Clicking this button will export the imputed data based on the other chosen options.\n\n   Users could name the exported data, and the information of imputation method will be completed in the file name automatically.\n\n   Two file formats are provided: csv and rda. For both formats, the number of columns in the exported data depends on the demand of users. There are 2*n columns if the user selected n variables. The first n columns are the imputed data, and the second n columns are the 'shadow matrix' which indicate whether the values are missing or not in the original dataset."))
	})
  
  gb248 = gbutton('Save the plot', container = group244,
                  handler = function(h,...){
                    if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   Clicking this button will save the plot(s) to png file(s) based on the options the user chooses.\n\n   A user can define the file name for the plot, and the graph type will be suffixed automatically."))
		})
  addHandlerMouseMotion(gb248, handler = function(h,...){
    if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   Clicking this button will save the plot(s) to png file(s) based on the options the user chooses.\n\n   A user can define the file name for the plot, and the graph type will be suffixed automatically."))
	})
  
  gb247 = gbutton('Q u i t', container = group244,
                  handler = function(h,...){
                    if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   Clicking this button will destroy the main window."))
		})
  addHandlerMouseMotion(gb247, handler = function(h,...){
    if (exists('text25')) svalue(text25) = capture.output(cat("\n\n   Clicking this button will destroy the main window."))
	})
  
  
  group25 = ggroup(horizontal = FALSE, container = group23,
                   expand = TRUE, use.scrollwindow = TRUE)
  text25 = gtext(container = group25, expand = TRUE,
                 use.scrollwindow = TRUE, font.attr=c(family="monospace"))
  
  svalue(tab)=1
}
