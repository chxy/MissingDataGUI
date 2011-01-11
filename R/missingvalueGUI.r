##' exportPattern "^[^\\.]"
NULL


##' Impute the Data Without Consideration of Conditions.
##' This function is to impute missing data by one of the eight
##' methods without the condition. When the condtions are imported,
##' they are then romoved from the data frame, and then the imputation
##' will be applied without conditions. Details of the eight methods
##' can be found in \code{\link{imputation}}.
##'
##' See \code{\link{imputation}}.
##'
##' @param dat A data frame whose missing values need to be imputed.
##' @param method The imputation method selected from the missing data
##' GUI. Must be one of 'Below 10%','Median','Mean','Random
##' value','Regression','Nearest neighbor','Multiple
##' Imputation','Mode'.
##' @param vartype  A vector of the classes of dat. The length is
##' the same as the number of columns of dat. The value should be
##' from "integer", "numeric", "factor", and "character".
##' @param cond A vector of categorical variables. There are no
##' missing values in those variables.
##' @return The imputed data frame with missing shadow matrix.
##' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
singleimputation = function(dat, method, vartype, cond) {
	dat=dat[,setdiff(colnames(dat),cond)]
	if (is.vector(dat)) {n=1} else {n = ncol(dat)}	
	shadow = is.na(dat)
	if (n!=1) {
		missingpct = sapply(dat, function(avec){mean(is.na(avec))})
		if (method == 'Below 10%') {
		for (i in 1:n) {
			if (vartype[i] %in% c('integer','numeric','logical') & 
			missingpct[i]>0) {
				dat[shadow[,i],i] = min(dat[,i], na.rm=TRUE)*1.1-
					max(dat[,i],na.rm=TRUE)/10
			}
			if (vartype[i] == "character") {
				dat[shadow[,i],i] = 'NAN'
			}
			if (vartype[i] == "factor" & missingpct[i]<1 &
			missingpct[i]>0) {
				dat[,i]=factor(dat[,i],levels = c('NAN',levels(factor(dat[,i]))))
				dat[shadow[,i],i] = 'NAN'
			}
			if (vartype[i] == "factor" & missingpct[i]==0) {
				dat[,i]=factor(dat[,i])
			}
		}
	}
		if (method == 'Median') {
		for (i in 1:n) {
			dat[,i] = as.numeric(impute(dat[,i], fun=median))
		}
	}
		if (method == 'Mean') {
		for (i in 1:n) {
			dat[,i] = as.numeric(impute(dat[,i], fun=mean))
		}
	}
		if (method == 'Random value') {
		for (i in 1:n) {
			dat[,i] = as.numeric(impute(dat[,i], fun='random'))
		}
	}
		if (method == 'Multiple Imputation') {
		if (!(any(c('factor','character') %in% vartype))) {
			s = prelim.norm(as.matrix(dat))
			thetahat = em.norm(s)
			rngseed(1234567)
			dat2 = imp.norm(s,thetahat,as.matrix(dat))
			if (any(sapply(dat2,function(avec){any(c(Inf,NaN) %in% avec)}))) {
				gmessage("This method doesn't converge. 
					Leave the NA's without imputation.",
					icon = "warning")
			} else {dat = dat2}
		}
	}
		if (method == 'Regression') {
		if (sum(complete.cases(dat))!=0) {
			formula0 = as.formula(paste('~ ',paste(names(dat),collapse=' + ')))
			f = aregImpute(formula0, data=dat)
			res = f$imputed				
			for (i in 1:length(res)) {
				dat[rownames(res[[i]]),names(res)[i]]=res[[i]][,1]
			}
		}
	}
		if (method == 'Nearest neighbor') {
		if (sum(complete.cases(dat))!=0 & (!("character" %in% vartype))) {
			myNNdat = dat[complete.cases(dat),]
			Missing_any = factor(apply(shadow,1,any),
				levels=c(FALSE,TRUE),labels=c('Not','Missing'))
			for (i in which(Missing_any=='Missing')){
				usecol = which(!is.na(dat[i,]))
				if (length(usecol)!=0){
					NNdat = myNNdat
					a = rbind(dat[i,], NNdat)[,usecol]
					NNdat$distance = dist(a)[1:nrow(NNdat)]			
					NNdat = NNdat[order(NNdat$distance,decreasing=FALSE),]
					dat[,1:n][i,-usecol] = NNdat[,1:n][1,-usecol]
				} else {
					for (j in 1:n) {
						dat[i,j] = median(dat[,j], na.rm=TRUE)
					}
				}
			}	
		}
	}
		if (method == 'Mode') {
			if (any(c('integer','numeric') %in% vartype)) {
				gmessage("The variables you selected contain numeric data. But I don't want to compute mode for numeric variables.",
					icon = "warning")
			} else {
				for (i in 1:n) {
					dat[shadow[,i],i] = names(sort(table(na.omit(dat[,i])),dec=TRUE))[1]
				}
			}
		}
	} else {
		missingpct = mean(is.na(dat))
		if (method == 'Below 10%') {
			if (vartype %in% c('integer','numeric','logical') & 
			missingpct>0) {
				dat[shadow] = min(origdata, na.rm=TRUE)*1.1-
					max(origdata,na.rm=TRUE)/10
			}
			if (vartype=='character') {
				dat[shadow] = 'NAN'
			}
			if (vartype=='factor' & missingpct<1 &
			missingpct>0) {
				dat=factor(dat,levels = c('NAN',levels(factor(dat))))
				dat[shadow] = 'NAN'
			}
			if (vartype=='factor' & missingpct==0) {
				dat=factor(dat)
			}
		}
		if (method == 'Median') {
			dat = as.numeric(impute(dat, fun=median))
		}
		if (method == 'Mean') {
			dat = as.numeric(impute(dat, fun=mean))
		}
		if (method == 'Random value') {
			dat = as.numeric(impute(dat, fun='random'))
		}
		if (method == 'Mode') {
			if (any(c('integer','numeric') %in% vartype)) {
				gmessage("The variables you selected contain numeric data. But I don't want to compute mode for numeric variables.",
					icon = "warning")
				dat = NULL
			} else {
				dat[shadow] = names(sort(table(na.omit(dat)),dec=TRUE))[1]
			}
		}
	}
	return(data.frame(dat,shadow))
}


##' Impute the missing data with the method selected under the
##' condition.
##' This function provides eight methods for imputation
##' with categorical varaibles as conditions.
##'
##' The imputation methods: (1)'Below 10%' means NA's of one variable
##' will be replaced by the value which equals to the minimum of the
##' variable minus 10% of the range. (2)'Median' means NA's will be
##' replaced by the median of this variable (omit NA's). (3)'Mean'
##' means NA's will be replaced by the mean of this variable (omit
##' NA's). (4)'Random value' means NA's will be replaced by any values
##' of this variable (omit NA's) which are randomly selected.
##' (5)'Regression' uses function \code{\link[Hmisc]{aregImpute}} from package
##' \pkg{Hmisc}. It requires at least two variables to be selected.
##' (6)'Nearest neighbor' replaces NA's by its nearest neighbor. It
##' requires at least two variables to be selected, and no character
##' variables. It returns median for the case if all values in it are
##' NA's. (7)'Multiple Imputation' uses functions from package
##' \pkg{norm}. It requires all selected variables to be numeric (at least
##' integer), and at least two variables to be selected. (8)'Mode' is
##' a method for imputing categorical variables. It requires all
##' selected variables to be character or factor or logical. It will
##' replace NA's by the mode of the variable (omit NA's).
##' @param origdata A data frame whose missing values need to be
##' imputed. This data frame should be selected from the missing data
##' GUI.
##' @param method The imputation method selected from the missing data
##' GUI. Must be one of 'Below 10%','Median','Mean','Random
##' value','Regression','Nearest neighbor','Multiple
##' Imputation','Mode'.
##' @param vartype A vector of the classes of origdata. The length is
##' the same as the number of columns of origdata. The value should be
##' from "integer", "numeric", "factor", and "character".
##' @param missingpct A vector of the percentage of missings of the
##' variables in origdata. The length is the same as the number of
##' columns of origdata. The value should be between 0 and 1.
##' @param condition A vector of categorical variables. The dataset
##' will be partitioned based on those variables, and then the
##' imputation is implemented in each group. There are no missing
##' values in those variables. If it is null, then there is no
##' division. The imputation is based on the whole dataset.
##' @return The imputed data frame with missing shadow matrix.
##' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
imputation = function(origdata, method, vartype, missingpct, condition=NULL){
	n=length(vartype)
	dat = origdata
	if (n==1){
		if (method %in% c('Regression','Nearest neighbor','Multiple Imputation')) {
			gmessage('You only selected one variable. 
				Cannot impute by regression, nearest neighbor,
				or multiple imputation.',
				icon = "error")
			res = NULL
		} else {
			if (is.null(condition)) {
				origshadow = is.na(origdata)
				if (method == 'Below 10%') {
					if (vartype %in% c('integer','numeric','logical') & 
					as.numeric(as.character(missingpct))>0) {
						dat[origshadow] = min(origdata, na.rm=TRUE)*1.1-
							max(origdata,na.rm=TRUE)/10
					}
					if (vartype=='character') {
						dat[origshadow] = 'NAN'
					}
					if (vartype=='factor' & 
					as.numeric(as.character(missingpct))<1 &
					as.numeric(as.character(missingpct))>0) {
						dat=factor(dat,levels = c('NAN',levels(factor(dat))))
						dat[origshadow] = 'NAN'
					}
					if (vartype=='factor' & 
					as.numeric(as.character(missingpct))==0) {
						dat=factor(dat)
					}
				}
				if (method == 'Median') {
					dat = as.numeric(impute(dat, fun=median))
				}
				if (method == 'Mean') {
					dat = as.numeric(impute(dat, fun=mean))
				}
				if (method == 'Random value') {
					dat = as.numeric(impute(dat, fun='random'))
				}
				if (method == 'Mode') {
					if (any(c('integer','numeric') %in% vartype)) {
						gmessage("The variables you selected contain numeric data. 
							But I don't want to compute mode for numeric variables.",
							icon = "warning")
						dat = NULL
					} else {
						dat[origshadow] = names(sort(table(na.omit(dat)),dec=TRUE))[1]
					}
				}
				if (!is.null(dat)) {
					Missing_any = factor(origshadow,levels=c(FALSE,TRUE))
					res = data.frame(dat,origshadow,Missing_any)
				} else {res=NULL}
			} else {
				dat = ddply(origdata,condition,singleimputation, 
					method=method,vartype=vartype, cond=condition)
				if (!is.null(dat)) {
					dat = dat[,2:3]
					Missing_any = factor(dat[,2],
						levels=c(FALSE,TRUE))
					res = data.frame(dat,Missing_any)
				} else {res = NULL}
			}
		}
	} else {
		origshadow = is.na(origdata[,1:n])
		if (is.null(condition)) {
			if (method == 'Below 10%') {
			for (i in 1:n) {
				if (vartype[i] %in% c('integer','numeric','logical') & 
				as.numeric(as.character(missingpct))[i]>0) {
					dat[origshadow[,i],i] = min(origdata[,i], na.rm=TRUE)*1.1-
						max(origdata[,i],na.rm=TRUE)/10
				}
				if (vartype[i] == "character") {
					dat[origshadow[,i],i] = 'NAN'
				}
				if (vartype[i] == "factor" & 
				as.numeric(as.character(missingpct))[i]<1 & as.numeric(as.character(missingpct))[i]>0) {
					dat[,i]=factor(dat[,i],levels = c('NAN',levels(factor(dat[,i]))))
					dat[origshadow[,i],i] = 'NAN'
				}
				if (vartype[i] == "factor" & 
				as.numeric(as.character(missingpct))[i]==0) {
					dat[,i]=factor(dat[,i])
				}
			}
			}
			if (method == 'Median') {
			for (i in 1:n) {
				dat[,i] = as.numeric(impute(dat[,i], fun=median))
			}
			}
			if (method == 'Mean') {
			for (i in 1:n) {
				dat[,i] = as.numeric(impute(dat[,i], fun=mean))
			}
			}
			if (method == 'Random value') {
			for (i in 1:n) {
				dat[,i] = as.numeric(impute(dat[,i], fun='random'))
			}
			}
			if (method == 'Multiple Imputation') {
			if (any(c('factor','character') %in% vartype)) {
				gmessage("Not every variable is numeric. Cannot impute 
					missing values under the multivariate normal model.",
					icon = "warning")
				dat=NULL
			} else {
				s = prelim.norm(as.matrix(dat))
				thetahat = em.norm(s)
				rngseed(1234567)
				dat = imp.norm(s,thetahat,as.matrix(dat))
				if (any(sapply(dat,function(avec){any(c(Inf,NaN) %in% avec)}))) {
					gmessage("This method doesn't converge. 
						Leave the NA's without imputation.",
						icon = "warning")
					dat = origdata
				}
			}
			}
			if (method == 'Regression') {
				if (sum(complete.cases(origdata))==0) {
					gmessage('All the samples have NAs. 
						Cannot impute by regression.',
						icon = "warning")
					dat=NULL
				} else {
					formula0 = as.formula(paste('~ ',paste(names(origdata),collapse=' + ')))
					f = aregImpute(formula0, data=origdata)
					res = f$imputed				
					for (i in 1:length(res)) {
						dat[rownames(res[[i]]),names(res)[i]]=res[[i]][,1]
					}
				}
			}
			if (method == 'Nearest neighbor') {
				if (sum(complete.cases(origdata))==0) {
					gmessage("All the samples have NA's. 
						Cannot impute by nearest neighbors.",
						icon = "warning")
					dat=NULL
				} else {
					if ("character" %in% vartype) {
						gmessage('Cannot impute with character.',
							icon = "warning")
						dat=NULL
					} else {
						myNNdat = dat[complete.cases(dat),]
						Missing_any = factor(apply(origshadow,1,any),
							levels=c(FALSE,TRUE))
						for (i in which(Missing_any=='TRUE')){
							usecol = which(!is.na(dat[i,]))
							if (length(usecol)!=0){
								NNdat = myNNdat
								a = rbind(dat[i,], NNdat)[,usecol]
								NNdat$distance = dist(a)[1:nrow(NNdat)]			
								NNdat = NNdat[order(NNdat$distance,decreasing=FALSE),]
								dat[,1:n][i,-usecol] = NNdat[,1:n][1,-usecol]
							} else {
								for (j in 1:n) {
									dat[i,j] = median(dat[,j], na.rm=TRUE)
								}
							}
						}						
					}
				}
			}
			if (method == 'Mode') {
				if (any(c('integer','numeric') %in% vartype)) {
					gmessage("The variables you selected contain numeric data. But I don't want to compute mode for numeric variables.", icon = "warning")
					dat = NULL
				} else {
					for (i in 1:n) {
						dat[origshadow[,i],i] = names(sort(table(na.omit(dat[,i])),dec=TRUE))[1]
					}
				}
			}
			if (!is.null(dat)) {
				Missing_any = factor(apply(origshadow,1,any),levels=c(FALSE,TRUE))
				res = data.frame(dat, origshadow ,Missing_any)
			} else {res = NULL}
		} else {
			dat = ddply(origdata,condition,singleimputation, method=method,vartype=vartype, cond=condition)
			if (!is.null(dat)) {
				dat = dat[,(ncol(dat)-2*n+1):ncol(dat)]
				Missing_any = factor(apply(dat[,(n+1):(2*n)],1,any),levels=c(FALSE,TRUE))
				res = data.frame(dat,Missing_any)
			} else {res = NULL}
		}
	}
	return(res)
}


##' The Main Window of Missing Data GUI.
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
##' @param h A list with components obj referring to the button "Watch
##' Missing Values" in \code{\link{missingdataGUI}}.
##' @param data A data frame which is shown in the missing-data
##' GUI. If it is null, then parameter gt must not be null.
##' @param gt A widget created by gtable(). It should be passed from
##' \code{\link{missingdataGUI}}.
##' @param ... Other parameters to be passed to this function.
##' @return NULL
##' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
##' @examples
##' \dontrun{
##' WatchMissingValues(data=airquality)
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
	dataset = read.csv(file = gtfile, head = TRUE)
    } else {
        dataset = data
    }
	rows = nrow(dataset)
	vname = as.character(names(dataset))
	dataclass = as.character(sapply(dataset, class))
	NAcol = which(sapply(dataset, function(avec){all(is.na(avec))}))
	vNApct = sapply(dataset, function(avec){mean(is.na(avec))})
	if (sum(vNApct)==0) {
		gmessage('There are no missing values in this data set...',
			icon = "error")
	}
	
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
					gt11[svalue(gt11, index = TRUE), 2] = svalue(gt11input12)
					gt11[svalue(gt11, index = TRUE), 3] = svalue(gt11input22)
					check123[,] = gt11[gt11[,3] %in% 
						c('factor','logical','character'),2]
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
		
		NumSumforMisVal <- gwindow("Numeric Summary for Missing Values", visible = T, width = 350, height = 300, parent = combo1)
		groupN1 = ggroup(cont = NumSumforMisVal, horizontal = FALSE, expand = TRUE)
		labelN11 = glabel('Missing:', cont=groupN1, pos=0)
		labelN12 = glabel(paste("    ",round(totalmissingpct*100,2),
			"% of the numbers",sep=""), cont=groupN1)
		labelN13 = glabel(paste("    ",round(varmissingpct*100,2),
			"% of variables",sep=""), cont=groupN1)
		labelN14 = glabel(paste("    ",round(casemissingpct*100,2),
			"% of samples",sep=""), cont=groupN1)
			
		groupN15= ggroup(container = groupN1, use.scrollwindow = TRUE, 
			horizontal = FALSE, expand = TRUE)
		missingsummary$No_of_miss_by_case = as.integer(missingsummary$No_of_miss_by_case)
		missingsummary$No_of_Case = as.integer(missingsummary$No_of_Case)
		missingsummary$Percent = as.character(missingsummary$Percent)
		tableN15 = gtable(missingsummary, cont=groupN15, expand = TRUE)		
	}
	
    #####----------------------------#####
    ##  Graph is the handler of gb144.  ##
	##  (gbutton: Watch the data)       ##
    #####----------------------------#####
    Graph = function(h, ...) {
		graphics.off()
        group151 = ggraph = list()
		glay15 = glayout(container = group15, expand = TRUE, use.scrollwindow = TRUE)
		
        name_select = svalue(gt11, index = TRUE)
		imp_method = svalue(gr142)
		graphtype = svalue(gr143)
		n = length(name_select)
		cond = check123[svalue(check123,index=T)]
		if (length(cond)==0) cond = NULL
		colorby = svalue(radio125)
		if (length(colorby)==0) {
			colorby = "Missing Any Variables Selected"
		} else {
			colorby = as.character(colorby)
		}
		
        if (n == 0) {
            gmessage("Please select at least one variable!",
				icon = "error")
            return()
        }
		

		dat = imputation(origdata=dataset[,c(gt11[name_select,2],cond)],
			method=imp_method, vartype=as.character(gt11[name_select,3]),
			missingpct=as.numeric(as.character(gt11[name_select,4])),
			condition=cond)
		dat = data.frame(dat)
		colnames(dat)[1:(2*n)]=c(gt11[name_select,2],
			paste('Missing', gt11[name_select,2], sep='_'))
		if (colorby=='Missing Any Variables Selected') {
			Missing <<- dat[,ncol(dat)]
		} else {
			Missing <<- is.na(dataset[,colorby])
		}
		
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
		if (graphtype=="Pairwise Plots") {
			if (n > 5) {
				gmessage("You selected more than five variables!
					Only the first five are displayed.",
					icon = "warning")
				name.select = name_select[1:5]
				n = 5
			}
			glay15[1, 1, expand = TRUE] = ggraphics(container = glay15, expand = TRUE)
			if (n==2) {
				print(qplot(dat[,1],dat[,2], color=Missing, geom='jitter',alpha=I(0.7),xlab=colnames(dat)[1],ylab=colnames(dat)[2]))
			} else {
			print(ggpairs(dat[,1:n], 
				upper = "blank", 	
				lower = list(continuous = "points", discrete = "ratio",
					aes_string = aes_string(position="position_jitter(width=1)")), 
				diag = list(continuous = "bar", discrete = "bar"),					
				color="Missing", fill="Missing",alpha=I(0.5)) )
			}
		}
		if (graphtype=="Parallel Coordinates") {
			if (n==1) {
				gmessage('You only selected one variable. 
					Cannot plot the parallel coordinates.',
					icon = "error")
				return()
			}
			if (any(c('character','factor') %in% as.character(gt11[name_select,3]))){
				gmessage('The parallel coordinates plot is only drawn for numeric variables. Please choose the variables again.', icon = "error")
				return()
			}			
			glay15[1, 1, expand = TRUE] = ggraphics(container = glay15, expand = TRUE)
			print(ggpcp(dat[,1:n])+geom_line(aes(colour=Missing))+
				geom_point(subset=dat[Missing=='Missing',1:n],colour='blue'))
		}

	}

	#####---------------------------------#####
    ##  ExportData is the handler of gb146.  ##
	##  (gbutton: Export the data)           ##
    #####---------------------------------#####
	ExportData = function(h,...){
		name_select = svalue(gt11, index = TRUE)
		imp_method = svalue(gr142)
		graphtype = svalue(gr143)
		n = length(name_select)
		cond = check123[svalue(check123,index=T)]
		if (length(cond)==0) cond = NULL
		
        if (n == 0) {
            gmessage("Please select at least one variable!")
            return()
        }

		dat = imputation(origdata=dataset[,c(gt11[name_select,2],cond)],
			method=imp_method, vartype=as.character(gt11[name_select,3]),
			missingpct=as.numeric(as.character(gt11[name_select,4])),
			condition=cond)
		dat = data.frame(dat)
		colnames(dat)[1:(2*n)]=c(gt11[name_select,2],
			paste('Missing', gt11[name_select,2], sep='_'))

		if (!is.na(gf <- gfile(type = "save"))) {
			filename = paste('_impute_',gsub('[^a-z,A-Z,0-9]',"",imp_method),".csv",sep='')
            write.csv(dat[,1:(2*n)], file = gsub('.csv',filename,gf), row.names = FALSE)
            gmessage("The data are exported!")
        }
		
	}
	
	#####-------------------------------#####
    ##  New window for missing values      ##
    #####-------------------------------#####
 	combo1 <- gwindow("Missing Values", visible = T, width = 1000, 
        height = 700)
    tab <- gnotebook(container = combo1)

    #####------------------------------------------------#####
    ##  In the first tab we can:                            ##
    ##  (1) Watch and change the name or type of variables. ##
    ##  (2) Numeric or graphic summary.                     ##
    ##  (3)                                                 ##
    #####------------------------------------------------#####
    group11 = ggroup(cont = tab, label = "Summary", expand = T)
    group12 = ggroup(container = group11, use.scrollwindow = TRUE, 
        horizontal = FALSE, expand = T)

    nametable = data.frame(Items=1:length(vname), Variables=vname, 
		Class=dataclass, NApct=as.character(round(vNApct,3)))
	nametable$Variables = as.character(nametable$Variables)
	nametable$Class = as.character(nametable$Class)
    gt11 = gtable(nametable, multiple = T, container = group12, 
        expand = TRUE, chosencol = 2)
    addhandlerdoubleclick(gt11, handler = VariableOptions)

	label121 = glabel('Categorical variables to condition on',container=group12)
	check123 = gcheckboxgroup(nametable$Variables[nametable$Class %in% 
		c('factor','logical','character')], container=group12, use.table=TRUE,
		handler = Graph)
	
    group13 = ggroup(horizontal = FALSE, container = group11, 
        expand = TRUE)
	group14 = ggroup(horizontal = TRUE, container = group13)

	radio125 = gtable(data.frame(`Color by the missing of`=
		c('Missing Any Variables Selected',nametable[vNApct>0,2])), container = group14,expand=TRUE)
	addHandlerKeystroke(radio125, handler = function(h,...){})
	gframe142 = gframe(text = "Imputation Method", container = group14)
	gr142 = gradio(c('Below 10%','Median','Mean','Random value',
		'Regression','Nearest neighbor','Multiple Imputation','Mode'),
		container = gframe142, handler = function(h,...){
			if (svalue(gr142)=='Below 10%') {
				svalue(check123) = FALSE
				enabled(check123) = FALSE
			} else {
				enabled(check123) = TRUE
			}
		})
	gframe143 = gframe(text = "Graph Type", container = group14)
	gr143 = gradio(c('Histogram/Barchart','Pairwise Plots',
		'Parallel Coordinates'), container = gframe143)

	group144 = ggroup(horizontal = FALSE, container = group14)
	gb145 = gbutton('Numeric summary', container = group144, 
        handler = NumSmry)
	gb144 = gbutton("Plot", container = group144, 
        handler = Graph)
	gb146 = gbutton('Export the data', container = group144, 
        handler = ExportData)
	gb147 = gbutton('Quit', container = group144, 
        handler = function(h,...){
			dispose(combo1)
		})
	
	group15 = ggroup(horizontal = FALSE, container = group13, 
        expand = TRUE, use.scrollwindow = TRUE)
    glay15 = glayout(container = group15, expand = TRUE, use.scrollwindow = TRUE)
    
	#####------------------------------------------------#####
    ##  In the second tab we can:                           ##
    ##  Look at the help documents.  ##
    #####------------------------------------------------#####
    group21 = ggroup(cont = tab, label = "Help", expand = T)
    group22 = ggroup(container = group21, use.scrollwindow = TRUE, 
        horizontal = FALSE, expand = T)

    gt21 = gtable(nametable, multiple = T, container = group22, 
        expand = TRUE, chosencol = 2)
    addHandlerMouseMotion(gt21, handler = function(h,...){
		svalue(text25) = capture.output(cat("
	This table displays all variables in the data set and
	reports the type and the percentage of missing values
	for each variable.\n
	We can sort the variables by NA's percent.\n
	If we doubleclick one row, we can change the variable
	name, as well as the data type."))
	})
	
	label221 = glabel('Categorical variables to condition on',container=group22)
	check223 = gcheckboxgroup(nametable$Variables[nametable$Class %in% 
		c('factor','logical','character')], container=group22, use.table=TRUE)
	addHandlerMouseMotion(check223, handler = function(h,...){
		svalue(text25) = capture.output(cat("
	This list displays all categorical variables.
	We can make multiple selection on them.\n
	Once we select one or more variables, the data
	set will be divided into small groups based on
	the selected categorical variable(s).\n
	And the imputation will be made in each group."))
	})
	
    group23 = ggroup(horizontal = FALSE, container = group21, 
        expand = TRUE)
	group24 = ggroup(horizontal = TRUE, container = group23)

	radio225 = gtable(data.frame(`Color by the missing of`=
		c('Missing Any Variables Selected',nametable[vNApct>0,2])), 
		container = group24, expand=TRUE, handler=function(h,...){
		svalue(text25) = capture.output(cat("
	This list displays all variables which have
	missing values.\n
	If we choose one of them, the color of the
	plot showing on the right will change based
	on whether the cases being NA on that variable
	or not.\n
	The first row 'Missing Any Variables Selected' means
	whether this case being complete or not."))
	})
	addHandlerMouseMotion(radio225, handler = function(h,...){
		svalue(text25) = capture.output(cat("
	This list displays all variables which have
	missing values.\n
	If we choose one of them, the color of the
	plot showing on the right will change based
	on whether the cases being NA on that variable
	or not.\n
	The first row 'Missing Any Variables Selected' means
	whether this case being complete or not."))
	})
	
	gframe242 = gframe(text = "Imputation Method", container = group24)
	gr242 = gradio(c('Below 10%','Median','Mean','Random value',
		'Regression','Nearest neighbor','Multiple Imputation','Mode'), 
		container = gframe242, handler = function(h,...){
		svalue(text25) = capture.output(cat("
	This list displays all the imputation methods.\n
	We can only select one of them.\n
	(1)'Below 10%' means NA's of one variable will
	be replaced by the value which equals to the
	minimum of the variable minus 10% of the range.\n
	(2)'Median' means NA's will be replaced by the
	median of this variable (omit NA's).\n
	(3)'Mean' means NA's will be replaced by the
	mean of this variable (omit NA's).\n
	(4)'Random value' means NA's will be replaced
	by any values of this variable (omit NA's)
	which are randomly selected.\n
	(5)'Regression' uses function 'aregImpute' from
	package 'Hmisc'. It requires at lease one case
	to be complete, and at least two variables to be
	selected.\n
	(6)'Nearest neighbor' replaces NA's by its nearest
	neighbor. It requires at lease one case to be
	complete, at least two variables to be selected,
	and no character variables. It returns median
	for the case if all values in it are NA's.\n
	(7)'Multiple Imputation' uses function 'imp.norm'
	from package 'norm'. It requires all selected 
	variables to be numeric(at least integer), and
	at least two variables to be selected. Sometimes
	it cannot converge, then the programme will leave
	NA's without imputation.\n
	(8)'Mode' is a method for imputing categorical
	variables. It requires all selected variables
	to be character or factor or logical. It will
	replace NA's by the mode of the variable
	(omit NA's)."))
	})
	addHandlerMouseMotion(gr242, handler = function(h,...){
		svalue(text25) = capture.output(cat("
	This list displays all the imputation methods.\n
	We can only select one of them.\n
	(1)'Below 10%' means NA's of one variable will
	be replaced by the value which equals to the
	minimum of the variable minus 10% of the range.\n
	(2)'Median' means NA's will be replaced by the
	median of this variable (omit NA's).\n
	(3)'Mean' means NA's will be replaced by the
	mean of this variable (omit NA's).\n
	(4)'Random value' means NA's will be replaced
	by any values of this variable (omit NA's)
	which are randomly selected.\n
	(5)'Regression' uses function 'aregImpute' from
	package 'Hmisc'. It requires at lease one case
	to be complete, and at least two variables to be
	selected.\n
	(6)'Nearest neighbor' replaces NA's by its nearest
	neighbor. It requires at lease one case to be
	complete, at least two variables to be selected,
	and no character variables. It returns median
	for the case if all values in it are NA's.\n
	(7)'Multiple Imputation' uses function 'imp.norm'
	from package 'norm'. It requires all selected 
	variables to be numeric(at least integer), and
	at least two variables to be selected. Sometimes
	it cannot converge, then the programme will leave
	NA's without imputation.\n
	(8)'Mode' is a method for imputing categorical
	variables. It requires all selected variables
	to be character or factor or logical. It will
	replace NA's by the mode of the variable
	(omit NA's)."))
	})
	
	gframe243 = gframe(text = "Graph Type", container = group24)
	gr243 = gradio(c('Histogram/Barchart','Pairwise Plots',
		'Parallel Coordinates'), container = gframe243,
		handler = function(h,...){
		svalue(text25) = capture.output(cat("
	This frame shows all plots we can make.\n
	(1)'Histogram' will display histograms for each
	variable selected.\n
	(2)'Pairwise Scatterplots' displays n*(n-1)/2
	scatterplots if we select n variables. It uses
	function 'ggpairs' from package 'GGally'.\n
	(3)'Parallel Coordinates' displays parallel
	coordinates plot for the selected variables."))
	})
	addHandlerMouseMotion(gframe243, handler = function(h,...){
		svalue(text25) = capture.output(cat("
	This frame shows all plots we can make.\n
	(1)'Histogram' will display histograms for each
	variable selected.\n
	(2)'Pairwise Scatterplots' displays n*(n-1)/2
	scatterplots if we select n variables. It uses
	function 'ggpairs' from package 'GGally'.\n
	(3)'Parallel Coordinates' displays parallel
	coordinates plot for the selected variables."))
	})
	
	group244 = ggroup(horizontal = FALSE, container = group24)
	gb245 = gbutton('Numeric summary', container = group244, 
        handler = function(h,...){
			svalue(text25) = capture.output(cat("
	Clicking this button will create another window
	which presents the numeric summaries for missing
	values."))
		})
	addHandlerMouseMotion(gb245, handler = function(h,...){
		svalue(text25) = capture.output(cat("
	Clicking this button will create another window
	which presents the numeric summaries for missing
	values."))
	})
	
	gb244 = gbutton("Plot", container = group244, 
        handler = function(h,...){
			svalue(text25) = capture.output(cat("
	Clicking this button will draw a plot based on
	the options you choose."))
		})
	addHandlerMouseMotion(gb244, handler = function(h,...){
		svalue(text25) = capture.output(cat("
	Clicking this button will draw a plot based on
	the options you choose."))
	})
	
	gb246 = gbutton('Export the data', container = group244, 
        handler = function(h,...){
			svalue(text25) = capture.output(cat("
	Clicking this button will export the imputed data
	based on the options you choose."))
		})
	addHandlerMouseMotion(gb246, handler = function(h,...){
		svalue(text25) = capture.output(cat("
	Clicking this button will export the imputed data
	based on the options you choose."))
	})
	
	
	gb247 = gbutton('Quit', container = group244, 
        handler = function(h,...){
			svalue(text25) = capture.output(cat("
	Clicking this button will destroy the main window."))
		})
	addHandlerMouseMotion(gb247, handler = function(h,...){
		svalue(text25) = capture.output(cat("
	Clicking this button will destroy the main window."))
	})

	
	group25 = ggroup(horizontal = FALSE, container = group23, 
        expand = TRUE, use.scrollwindow = TRUE)
    text25 = gtext(container = group25, expand = TRUE, 
		use.scrollwindow = TRUE, font.attr=c(family="monospace"))
	svalue(tab)=1
}


##' The Starting of Missing Data GUI.
##' This function starts an open-files GUI, allowing 1) selecting one
##' or more data files; 2)opening the main missing-data GUI for one
##' data file.
##'
##' If more than one files are listed in the window but no file is
##' focused when clicking the "Watch Missing Values", then the first
##' file is selected for the main missing-data GUI. If more than one
##' files are focused, then the first file of the focused files is
##' selected for the main GUI.
##' @param data A data frame which is shown in the main missing-data
##' GUI. If it is null, then the open-files GUI opens.
##' @return NULL
##' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
##' @examples
##' \dontrun{
##' missingdataGUI()
##' missingdataGUI(airquality)
##' }
##'
missingdataGUI = function(data=NULL) {
    if (is.null(data)) {
        combo0 = gwindow("Open A File...", visible = TRUE)
        group = ggroup(horizontal = FALSE, container = combo0)
        f.list = matrix(nrow = 0, ncol = 1, dimnames = list(NULL, "File"))
        gt = gtable(f.list, multiple = T, container = group, expand = T)
        gb1 = gbutton("Open", container = group, handler = function(h,
            ...) gt[,] = na.omit(rbind(gt[, , drop = FALSE], matrix(file.choose(),
            dimnames = list(NULL, "File")))))
        gb2 = gbutton("Watch Missing Values", container = group, 
			handler = function(h, ...) WatchMissingValues(h, data=NULL, gt=gt))
    } else {
        if (is.data.frame(data)) {
            WatchMissingValues(data=data)
        } else {
            gmessage("Please use a data frame.")
            warning("The input needs to be a data frame.")
        }
    }
}


##' West Pacific Tropical Atmosphere Ocean Data, 1993 & 1997.
##' Real-time data from moored ocean buoys for improved detection,
##' understanding and prediction of El Niño and La Niña.
##'
##' The data is collected by the Tropical Atmosphere Ocean project (
##' \url{http://www.pmel.noaa.gov/tao/index.shtml}).
##'
##' Format: a data frame with 736 observations on the following 8
##' variables. \describe{\item{\code{year}}{A factor with levels \code{1993}
##' \code{1997}.} \item{\code{latitude}}{A factor with levels \code{-5} 
##' \code{-2} \code{0}.} \item{\code{longitude}}{A factor with levels \code{-110}
##' \code{-95}.} \item{\code{sea.surface.temp}}{Sea surface temperature(°C),
##' measured by the TAO buoys at one meter below the surface.}
##' \item{\code{air.temp}}{Air temperature(°C), measured by the TAO buoys
##' three meters above the sea surface.} \item{\code{humidity}}{Relative
##' humidity(%), measured by the TAO buoys 3 meters above the sea
##' surface.} \item{\code{uwind}}{The East-West wind vector components(M/s).
##' TAO buoys measure the wind speed and direction four meters above
##' the sea surface. If it is positive, the East-West component of
##' the wind is blowing towards the East. If it is negative, this
##' component is blowing towards the West.}\item{\code{vwind}}{The North-South
##' wind vector components(M/s). TAO buoys measure the wind speed
##' and direction four meters above the sea surface. If it is positive,
##' the North-South component of the wind is blowing towards the North.
##' If it is negative, this component is blowing towards the South.}}
##' @name tao
##' @docType data
##' @usage data(tao)
##' @source \url{http://www.pmel.noaa.gov/tao/data_deliv/deliv.html}
##' @keywords datasets
##' @examples
##' \dontrun{
##' data(tao)
##' missingdataGUI(tao)
##' }
##'
NULL


##' The Behavioral Risk Factor Surveillance System (BRFSS) Survey Data, 2009.
##' The data is a subset of the 2009 survey from BRFSS, an ongoing
##' data collection program designed to measure behavioral risk factors
##' for the adult population (18 years of age or older) living in
##' households. 
##'
##' Also see the codebook: \url{http://ftp.cdc.gov/pub/data/brfss/codebook_09.rtf}
##'
##' Format: a data frame with 736 observations on the following 8
##' variables. \describe{\item{\code{STATE}}{A factor with 52 levels.
##' The labels and states corresponding to the labels are as follows.
##' 1:Alabama, 2:Alaska, 4:Arizona, 5:Arkansas, 6:California, 8:Colorado,
##' 9:Connecticut, 10:Delaware, 11:District of Columbia, 12:Florida,
##' 13:Georgia, 15:Hawaii, 16:Idaho, 17:Illinois, 18:Indiana, 19:Iowa,
##' 20:Kansas, 21:Kentucky, 22:Louisiana, 23:Maine, 24:Maryland,
##' 25:Massachusetts, 26:Michigan, 27:Minnesota, 28:Mississippi,
##' 29:Missouri, 30:Montana, 31:Nebraska, 32:Nevada, 33:New Hampshire,
##' 34:New Jersey, 35:New Mexico, 36:New York, 37:North Carolina,
##' 38:North Dakota, 39:Ohio, 40:Oklahoma, 41:Oregon, 42:Pennsylvania,
##' 44:Rhode Island, 45:South Carolina, 46:South Dakota, 47:Tennessee,
##' 48:Texas, 49:Utah, 50:Vermont, 51:Virginia, 53:Washington, 54:West 
##' Virginia, 55:Wisconsin, 56:Wyoming, 66:Guam, 72:Puerto Rico,
##' 78:Virgin Islands} \item{\code{SEX}}{A factor with levels \code{Male}
##' \code{Female}.} \item{\code{AGE}}{a numeric vector}
    \item{\code{HISPANC2}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{VETERAN2}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5}}
    \item{\code{MARITAL}}{a factor with levels \code{Married} \code{Divorced} \code{Widowed} \code{Separated} \code{NeverMarried} \code{UnmarriedCouple}}
    \item{\code{CHILDREN}}{a numeric vector}
    \item{\code{EDUCA}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6}}
    \item{\code{EMPLOY}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{7} \code{8}}
    \item{\code{INCOME2}}{a factor with levels \code{<10k} \code{10-15k} \code{15-20k} \code{20-25k} \code{25-35k} \code{35-50k} \code{50-75k} \code{>75k} \code{Dontknow} \code{Refused}}
    \item{\code{WEIGHT2}}{a numeric vector}
    \item{\code{HEIGHT3}}{a numeric vector}
    \item{\code{PREGNANT}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{GENHLTH}}{a factor with levels \code{Excellent} \code{VeryGood} \code{Good} \code{Fair} \code{Poor} \code{Refused}}
    \item{\code{PHYSHLTH}}{a numeric vector}
    \item{\code{MENTHLTH}}{a numeric vector}
    \item{\code{POORHLTH}}{a numeric vector}
    \item{\code{HLTHPLAN}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{CAREGIVE}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{QLACTLM2}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{DRNKANY4}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{ALCDAY4}}{a numeric vector}
    \item{\code{AVEDRNK2}}{a numeric vector}
    \item{\code{SMOKE100}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{SMOKDAY2}}{a factor with levels \code{Everyday} \code{Somedays} \code{Not@All}}
    \item{\code{STOPSMK2}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{LASTSMK1}}{a factor with levels \code{3} \code{4} \code{5} \code{6} \code{7} \code{8}}
    \item{\code{FRUIT}}{a numeric vector}
    \item{\code{GREENSAL}}{a numeric vector}
    \item{\code{POTATOES}}{a numeric vector}
    \item{\code{CARROTS}}{a numeric vector}
    \item{\code{VEGETABL}}{a numeric vector}
    \item{\code{FRUITJUI}}{a numeric vector}
    \item{\code{BMI4}}{a numeric vector}
	}
##' @name brfss
##' @docType data
##' @usage data(brfss)
##' @source \url{http://www.cdc.gov/BRFSS/technical_infodata/surveydata/2009.htm}
##' @keywords datasets
##' @examples
##' \dontrun{
##' data(brfss)
##' missingdataGUI(brfss)
##' }
##'
NULL