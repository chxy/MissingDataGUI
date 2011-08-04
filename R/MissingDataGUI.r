
##' Impute the Data Without Consideration of Conditions.
##'
##' This function is to impute missing data by one of the eight
##' methods without the condition. When the condtions are imported,
##' they are then romoved from the data frame, and then the imputation
##' will be applied without conditions. Details of the eight methods
##' can be found in \code{\link{imputation}}.
##'
##' See \code{\link{imputation}}.
##'
##' @param dat A data frame whose missing values need to be
##' imputed. The last column should be the row number passed by
##' \code{\link{imputation}}.
##' @param method The imputation method selected from the missing data
##' GUI. Must be one of 'Below 10%','Median','Mean','Random
##' value','Regression','Nearest neighbor','Multiple
##' Imputation','Mode'.
##' @param vartype  A vector of the classes of dat. The length is
##' the same as the number of columns of dat. The value should be
##' from "integer", "numeric", "factor", and "character".
##' @param cond A vector of categorical variables. There are no
##' missing values in those variables.
##' @return The imputed data frame with a column of row numbers
##' matching with the last column of dat, to pass back to
##' \code{\link{imputation}}.
##' @exportPattern "^[^\\.]"
##' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
singleimputation = function(dat, method, vartype, cond) {
	row_number_2011 = dat[,ncol(dat)]
	dat=dat[,-ncol(dat)]
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
			tmpres = f$imputed
			for (i in 1:length(tmpres)) {
				dat[rownames(tmpres[[i]]),names(tmpres)[i]]=tmpres[[i]][,1]
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
					k5NNdat = NNdat[order(NNdat$distance,decreasing=FALSE),][1:min(5,nrow(NNdat)),]
					if (nrow(k5NNdat)>1 & n-length(usecol)>1) {
						dat[,1:n][i,-usecol] = apply(k5NNdat[,1:n][,-usecol],2,mean)
					} else {
						if (nrow(k5NNdat)==1) {
							dat[,1:n][i,-usecol] = k5NNdat[,1:n][1,-usecol]
						} else {
							dat[,1:n][i,-usecol] = mean(k5NNdat[,1:n][,-usecol])
						}
					}
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
				dat[shadow] = min(dat, na.rm=TRUE)*1.1-
					max(dat,na.rm=TRUE)/10
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
	return(data.frame(dat,row_number=row_number_2011))
}


##' Impute the missing data with the method selected under the
##' condition.
##'
##' This function provides eight methods for imputation with
##' categorical varaibles as conditions.
##'
##' The imputation methods: (1)'Below 10%' means NA's of one variable
##' will be replaced by the value which equals to the minimum of the
##' variable minus 10% of the range. (2)'Median' means NA's will be
##' replaced by the median of this variable (omit NA's). (3)'Mean'
##' means NA's will be replaced by the mean of this variable (omit
##' NA's). (4)'Random value' means NA's will be replaced by any values
##' of this variable (omit NA's) which are randomly selected.
##' (5)'Regression' uses function \code{\link[Hmisc]{aregImpute}} from
##' package \pkg{Hmisc}. It requires at least two variables to be
##' selected.  (6)'Nearest neighbor' replaces NA's by its nearest
##' neighbor. It requires at least two variables to be selected, and
##' no character variables. It returns median for the case if all
##' values in it are NA's. (7)'Multiple Imputation' uses functions
##' from package \pkg{norm}. It requires all selected variables to be
##' numeric (at least integer), and at least two variables to be
##' selected. (8)'Mode' is a method for imputing categorical
##' variables. It requires all selected variables to be character or
##' factor or logical. It will replace NA's by the mode of the
##' variable (omit NA's).
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
##' @return The imputed data frame with the last column being the row
##' number from the original dataset. During the procedure of the
##' function, rows may be exchanged, thus a column of row number could
##' keep track of the original row number and then help to find the
##' shadow matrix.
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
					res = data.frame(dat,row_number=1:nrow(dat))
				} else {res=NULL}
			} else {
				dat = ddply(data.frame(origdata,row_number_2011=1:nrow(origdata)),
					condition,singleimputation,method=method,vartype=vartype,
					cond=condition)
				if (!is.null(dat)) {
					res = dat[,(ncol(dat)-1):ncol(dat)]
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
					tmpres = f$imputed
					for (i in 1:length(tmpres)) {
						dat[rownames(tmpres[[i]]),names(tmpres)[i]]=tmpres[[i]][,1]
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
								k5NNdat = NNdat[order(NNdat$distance,decreasing=FALSE),][1:min(5,nrow(NNdat)),]
								if (nrow(k5NNdat)>1 & n-length(usecol)>1) {
									dat[,1:n][i,-usecol] = apply(k5NNdat[,1:n][,-usecol],2,mean)
								} else {
									if (nrow(k5NNdat)==1) {
										dat[,1:n][i,-usecol] = k5NNdat[,1:n][1,-usecol]
									} else {
										dat[,1:n][i,-usecol] = mean(k5NNdat[,1:n][,-usecol])
									}
								}
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
				res = data.frame(dat, row_number=1:nrow(origdata))
			} else {res = NULL}
		} else {
			dat = ddply(data.frame(origdata,row_number_2011=1:nrow(origdata)),condition,singleimputation, method=method,vartype=vartype, cond=condition)
			if (!is.null(dat)) {
				res = dat[,(ncol(dat)-n):ncol(dat)]
			} else {res = NULL}
		}
	}
	return(res)
}


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


		dat = imputation(origdata=dataset[,c(gt11[name_select,2],cond),drop=FALSE],
			method=imp_method, vartype=as.character(gt11[name_select,3]),
			missingpct=as.numeric(as.character(gt11[name_select,4])),
			condition=cond)
		dat = data.frame(dat)
		colnames(dat)[1:n]=c(gt11[name_select,2])
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
				gmessage("You selected more than five variables!
					Only the first five are displayed.",
					icon = "warning")
				name.select = name_select[1:5]
				n = 5
			}
			glay15[1, 1, expand = TRUE] = ggraphics(container = glay15, expand = TRUE)
			if (n==2) {
				print(qplot(dat[,1],dat[,2], color=Missing, geom='jitter',alpha=I(0.7),
					size=I(3),xlab=colnames(dat)[1],ylab=colnames(dat)[2]))
			} else {
			dat$Missings=factor(Missing)
			print(ggpairs(dat,columns=1:n,
				upper = "blank",
				lower = list(continuous = "points", discrete = "ratio",
					aes_string = aes_string(position="position_jitter(width=1)")),
				diag = list(continuous = "bar", discrete = "bar"),
				color="Missings", fill="Missings",alpha=I(0.5)) )
			# ggpairs(iris,colour=species)
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
			dat$Missing=Missing
			print(ggpcp(dat,vars=names(dat)[1:n])+geom_line(aes(colour=Missing)))
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
		dat = data.frame(dat[,-ncol(dat)],
			is.na(dataset[dat[,ncol(dat)],gt11[name_select,2]]))
		colnames(dat)[1:(2*n)]=c(gt11[name_select,2],
			paste('Missing', gt11[name_select,2], sep='_'))

		if (!is.na(gf <- gfile(type = "save"))) {
			filename = paste('_impute_',gsub('[^a-z,A-Z,0-9]',"",imp_method),".csv",sep='')
            write.csv(dat[,1:(2*n)], file = ifelse(grepl("\\.csv$",gf),gsub('\\.csv$',filename,gf),paste(gf,filename,sep='')), row.names = FALSE)
            gmessage("The data are exported!")
        }

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

		dat = imputation(origdata=dataset[,c(gt11[name_select,2],cond)],
			method=imp_method, vartype=as.character(gt11[name_select,3]),
			missingpct=as.numeric(as.character(gt11[name_select,4])),
			condition=cond)
		dat = data.frame(dat)
		colnames(dat)[1:n]=c(gt11[name_select,2])
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
				gmessage("You selected more than five variables!
					Only the first five are displayed.",
					icon = "warning")
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
			print(ggpairs(dat,columns=1:n,
				upper = "blank",
				lower = list(continuous = "points", discrete = "ratio",
					aes_string = aes_string(position="position_jitter(width=1)")),
				diag = list(continuous = "bar", discrete = "bar"),
				color="Missings", fill="Missings",alpha=I(0.5)) )
			# ggpairs(iris,colour=species)
			}
			dev.off()
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
			savename = gfile(type="save")
			png(filename = paste(savename,'_pcp.png',sep=''),width = (n+2), height = 4, units = "in", res=90)
			dat$Missing=Missing
			print(ggpcp(dat,vars=names(dat)[1:n])+geom_line(aes(colour=Missing)))
			#	+ geom_point(subset=dat[Missing,1:n],colour='blue'))
			dev.off()
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
    group11 = ggroup(cont = tab, label = "Summary", expand = TRUE, horizontal = FALSE)
	group1100 = ggroup(container = group11, expand = TRUE)
    group12 = ggroup(container = group1100, use.scrollwindow = TRUE,
        horizontal = FALSE, expand = TRUE)

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

    group13 = ggroup(horizontal = FALSE, container = group1100,
        expand = TRUE)
	group14 = ggroup(horizontal = TRUE, container = group13)

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
	gframe143 = gframe(text = "Graph Type", container = group14)
	gr143 = gradio(c('Histogram/Barchart','Spinogram/Spineplot','Pairwise Plots',
		'Parallel Coordinates'), container = gframe143)

	group144 = ggroup(horizontal = FALSE, container = group14)
	gb145 = gbutton('Numeric summary', container = group144,
        handler = NumSmry)
	gb144 = gbutton("Plot", container = group144,
        handler = Graph)
	gb146 = gbutton('Export the data', container = group144,
        handler = ExportData)
	gb148 = gbutton('Save the plot', container = group144,
        handler = SavePlot)
	gb147 = gbutton('Quit', container = group144,
        handler = function(h,...){
			dispose(combo1)
		})

	group15 = ggroup(horizontal = FALSE, container = group13,
        expand = TRUE, use.scrollwindow = TRUE)
    glay15 = glayout(container = group15, expand = TRUE, use.scrollwindow = TRUE)

	#group1101 = ggroup(cont = group11)
	#text16 = gtext(text = NULL, height = 50, container = group1101, expand=TRUE)

	#####------------------------------------------------#####
    ##  In the second tab we can:                           ##
    ##  Look at the help documents.  ##
    #####------------------------------------------------#####
    group21 = ggroup(cont = tab, label = "Help", expand = TRUE, horizontal = FALSE)
	group2100 = ggroup(container = group21, expand = TRUE)
    group22 = ggroup(container = group2100, use.scrollwindow = TRUE,
        horizontal = FALSE, expand = T)

    gt21 = gtable(nametable, multiple = T, container = group22,
        expand = TRUE, chosencol = 2)
    addHandlerMouseMotion(gt21, handler = function(h,...){
		if (exists('text25')) svalue(text25) = capture.output(cat("
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
		if (exists('text25')) svalue(text25) = capture.output(cat("
	This list displays all categorical variables.
	We can make multiple selection on them.\n
	Once we select one or more variables, the data
	set will be divided into small groups based on
	the selected categorical variable(s).\n
	And the imputation will be made in each group.\n
	If the imputation method is 'Below 10%', then
	the selected conditioning variables are ignored."))
	})

    group23 = ggroup(horizontal = FALSE, container = group2100,
        expand = TRUE)
	group24 = ggroup(horizontal = TRUE, container = group23)

	radio225 = gtable(data.frame(`Color by the missing of`=
		c('Missing Any Variables','Missing on Selected Variables',nametable[vNApct>0,2])),
		container = group24, expand=TRUE, handler=function(h,...){
		if (exists('text25')) svalue(text25) = capture.output(cat("
	This list displays all variables which have
	missing values.\n
	If the user chooses one of them, the color of the
	plot showing on the right will change based
	on whether the cases being NA on that variable
	or not.\n
	The user can also choose several variables. Then the
	color of the plot will be based on whether the cases
	have missing values on any of those variable.\n
	The first row 'Missing Any Variables' means
	whether this case being complete or not.\n
	The first row 'Missing on Selected Variables' means
	whether the cases have missing values on any of the
	selected variable.\n
	The widget allows text entry to find a particular
	variable if the list is quite long."))
	})
	addHandlerMouseMotion(radio225, handler = function(h,...){
		if (exists('text25')) svalue(text25) = capture.output(cat("
	This list displays all variables which have
	missing values.\n
	If the user chooses one of them, the color of the
	plot showing on the right will change based
	on whether the cases being NA on that variable
	or not.\n
	The user can also choose several variables. Then the
	color of the plot will be based on whether the cases
	have missing values on any of those variable.\n
	The first row 'Missing Any Variables' means
	whether this case being complete or not.\n
	The first row 'Missing on Selected Variables' means
	whether the cases have missing values on any of the
	selected variable.\n
	The widget allows text entry to find a particular
	variable if the list is quite long."))
	})

	gframe242 = gframe(text = "Imputation Method", container = group24)
	gr242 = gradio(c('Below 10%','Median','Mean','Random value',
		'Regression','Nearest neighbor','Multiple Imputation','Mode'),
		container = gframe242, handler = function(h,...){
		if (exists('text25')) svalue(text25) = capture.output(cat("
	This list displays all the imputation methods.\n
	We can only select one of them.\n
	(1)'Below 10%' means NA's of one variable will
	be replaced by the value which equals to the
	minimum of the variable minus 10% of the range.
	Under this status the selected conditioning
	variables are ignored.\n
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
		if (exists('text25')) svalue(text25) = capture.output(cat("
	This list displays all the imputation methods.\n
	We can only select one of them.\n
	(1)'Below 10%' means NA's of one variable will
	be replaced by the value which equals to the
	minimum of the variable minus 10% of the range.
	Under this status the selected conditioning
	variables are ignored.\n
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
	gr243 = gradio(c('Histogram/Barchart','Spinogram/Spineplot','Pairwise Plots',
		'Parallel Coordinates'), container = gframe243,
		handler = function(h,...){
		if (exists('text25')) svalue(text25) = capture.output(cat("
	This frame shows all plots we can make.\n
	(1)'Histogram/Barchart' will display histograms
	(numeric variables) and barcharts(categorical
	variables) for each variable selected.\n
	(2)'Spinogram/Spineplot' shows the spineplot
	for each selected variable.\n
	(3)'Pairwise Scatterplots' displays n*(n-1)/2
	scatterplots if we select n variables. When n>5,
	then only the first 5 variables are displayed.\n
	When n=2, a scatterplot is drawn for the two
	variables. When 2<n<=5, the function 'ggpairs'
	from package 'GGally' is used.\n
	(4)'Parallel Coordinates' displays parallel
	coordinates plot for the selected variables."))
	})
	addHandlerMouseMotion(gr243, handler = function(h,...){
		if (exists('text25')) svalue(text25) = capture.output(cat("
	This frame shows all plots we can make.\n
	(1)'Histogram/Barchart' will display histograms
	(numeric variables) and barcharts(categorical
	variables) for each variable selected.\n
	(2)'Spinogram/Spineplot' shows the spineplot
	for each selected variable.\n
	(3)'Pairwise Scatterplots' displays n*(n-1)/2
	scatterplots if we select n variables. When n>5,
	then only the first 5 variables are displayed.\n
	When n=2, a scatterplot is drawn for the two
	variables. When 2<n<=5, the function 'ggpairs'
	from package 'GGally' is used.\n
	(4)'Parallel Coordinates' displays parallel
	coordinates plot for the selected variables."))
	})

	group244 = ggroup(horizontal = FALSE, container = group24)
	gb245 = gbutton('Numeric summary', container = group244,
        handler = function(h,...){
			if (exists('text25')) svalue(text25) = capture.output(cat("
	Clicking this button will create another window
	which presents the numeric summaries for missing
	values.\n
	In this summary window, the missing percentage
	of all the numbers, variables, and cases are
	presented. \n
	Besides, there is a table of the missing levels.
	The table has n+1 rows, where n = # of selected
	variables. For each i in 0:n, the table gives
	the count of cases which have i missing values,
	as well as the percentage of those cases."))
		})
	addHandlerMouseMotion(gb245, handler = function(h,...){
		if (exists('text25')) svalue(text25) = capture.output(cat("
	Clicking this button will create another window
	which presents the numeric summaries for missing
	values.\n
	In this summary window, the missing percentage
	of all the numbers, variables, and cases are
	presented. \n
	Besides, there is a table of the missing levels.
	The table has n+1 rows, where n = # of selected
	variables. For each i in 0:n, the table gives
	the count of cases which have i missing values,
	as well as the percentage of those cases."))
	})

	gb244 = gbutton("Plot", container = group244,
        handler = function(h,...){
			if (exists('text25')) svalue(text25) = capture.output(cat("
	Clicking this button will draw a plot based on
	the options the user chooses.\n
	All the n variables the user selected should be
	displayed, except that when the graph type is
	'Pairwise Scatterplots' and n>5, then only the
	first 5	variables are displayed."))
		})
	addHandlerMouseMotion(gb244, handler = function(h,...){
		if (exists('text25')) svalue(text25) = capture.output(cat("
	Clicking this button will draw a plot based on
	the options the user chooses.\n
	All the n variables the user selected should be
	displayed, except that when the graph type is
	'Pairwise Scatterplots' and n>5, then only the
	first 5	variables are displayed."))
	})

	gb246 = gbutton('Export the data', container = group244,
        handler = function(h,...){
			if (exists('text25')) svalue(text25) = capture.output(cat("
	Clicking this button will export the imputed data
	based on the options the user chooses.\n
	A user can define the file name for the exported
	data, and the imputation method will be suffixed
	automatically.\n
	In the exported csv file, there are 2*n columns
	if the user selected n variables. The first n
	columns are the imputed data, and the second n
	columns are the 'shadow matrix' which indicate
	whether the values are missing or not in the
	original dataset."))
		})
	addHandlerMouseMotion(gb246, handler = function(h,...){
		if (exists('text25')) svalue(text25) = capture.output(cat("
	Clicking this button will export the imputed data
	based on the options the user chooses.\n
	A user can define the file name for the exported
	data, and the imputation method will be suffixed
	automatically.\n
	In the exported csv file, there are 2*n columns
	if the user selected n variables. The first n
	columns are the imputed data, and the second n
	columns are the 'shadow matrix' which indicate
	whether the values are missing or not in the
	original dataset."))
	})

	gb248 = gbutton('Save the plot', container = group244,
        handler = function(h,...){
			if (exists('text25')) svalue(text25) = capture.output(cat("
	Clicking this button will save the plot(s) to png
	file(s) based on the options the user chooses.\n
	A user can define the file name for the plot, and
	the graph type will be suffixed automatically."))
		})
	addHandlerMouseMotion(gb248, handler = function(h,...){
		if (exists('text25')) svalue(text25) = capture.output(cat("
	Clicking this button will save the plot(s) to png
	file(s) based on the options the user chooses.\n
	A user can define the file name for the plot, and
	the graph type will be suffixed automatically."))
	})

	gb247 = gbutton('Quit', container = group244,
        handler = function(h,...){
			if (exists('text25')) svalue(text25) = capture.output(cat("
	Clicking this button will destroy the main window."))
		})
	addHandlerMouseMotion(gb247, handler = function(h,...){
		if (exists('text25')) svalue(text25) = capture.output(cat("
	Clicking this button will destroy the main window."))
	})


	group25 = ggroup(horizontal = FALSE, container = group23,
        expand = TRUE, use.scrollwindow = TRUE)
    text25 = gtext(container = group25, expand = TRUE,
		use.scrollwindow = TRUE, font.attr=c(family="monospace"))

	# group2101 = ggroup(cont = group21)
	# text26 = gtext(text = NULL, height = 50, container = group2101, expand=TRUE)
	# addHandlerKeystroke(text26, handler = function(h,...){
		# if (exists('text25')) svalue(text25) = capture.output(cat("
	# The code for the plots is texted here. Currently it
	# cannot be executed directly. The user need to import
	# the dataset and use the correct paramter to make a
	# similar plot."))
	# })
	# addHandlerClicked(text26, handler = function(h,...){
		# if (exists('text25')) svalue(text25) = capture.output(cat("
	# The code for the plots is texted here. Currently it
	# cannot be executed directly. The user need to import
	# the dataset and use the correct paramter to make a
	# similar plot."))
	# })
	# addHandlerMouseMotion(text26, handler = function(h,...){
		# if (exists('text25')) svalue(text25) = capture.output(cat("
	# The code for the plots is texted here. Currently it
	# cannot be executed directly. The user need to import
	# the dataset and use the correct paramter to make a
	# similar plot."))
	# })

	svalue(tab)=1
}


##' The Starting of Missing Data GUI.
##'
##' This function starts an open-files GUI, allowing 1) selecting one
##' or more data files; 2)opening the main missing-data GUI for one
##' data file. The missing data GUI consists of two tabs. In the
##' summary tab, there are a list of all variables, a list of
##' variables having missing values to color by, two radios for
##' imputation methods and graph types respectively, a checkbox group
##' for the conditional variables, four buttons and a graphics
##' device. In the help tab, the layout is the same as the summary
##' tab.  But when the users move their mouse on those widgets, or
##' click any of those radios or buttons, the functions of all widgets
##' will be described at the place of the graphics device. The
##' attributes of the variables can be changed. If the user double
##' clicks on any variables in the top left table of missing-data GUI,
##' an attribute window will pop up. Then the name could be edited,
##' and the class could be changed to one of the four classes:
##' integer, numeric, factor, and character. When a numeric variable
##' is changed to a categorical variable, the condtions in the bottom
##' left checkbox group will be updated. If the list of the color by
##' variables is very long, the selector allows text entry to find the
##' variable when this widget is active.
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
##' if (interactive()) {
##' MissingDataGUI()
##'
##' data(tao)
##' MissingDataGUI(tao)
##'
##' data(brfss)
##' MissingDataGUI(brfss)
##' }
##'
MissingDataGUI = function(data=NULL) {
    if (is.null(data)) {
        combo0 = gwindow("Open A File...", visible = TRUE)
        group = ggroup(horizontal = FALSE, container = combo0)
        f.list = matrix(nrow = 0, ncol = 1, dimnames = list(NULL, "File"))
        gt = gtable(f.list, multiple = T, container = group, expand = T)
		gb1 = gbutton("Open", container = group, handler = function(h,...) gt[,] = union(gt[,],na.omit(gfile(multiple=TRUE))))
        gb2 = gbutton("Watch Missing Values", container = group,handler = function(h, ...) WatchMissingValues(h, data=NULL, gt=gt))
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
##'
##' Real-time data from moored ocean buoys for improved detection,
##' understanding and prediction of El Ni'o and La Ni'a.
##'
##' The data is collected by the Tropical Atmosphere Ocean project (
##' \url{http://www.pmel.noaa.gov/tao/index.shtml}).
##'
##' Format: a data frame with 736 observations on the following 8
##' variables. \describe{\item{\code{year}}{A factor with levels
##' \code{1993} \code{1997}.} \item{\code{latitude}}{A factor with
##' levels \code{-5}  \code{-2} \code{0}.} \item{\code{longitude}}{A
##' factor with levels \code{-110} \code{-95}.}
##' \item{\code{sea.surface.temp}}{Sea surface temperature(degree
##' Celsius), measured by the TAO buoys at one meter below the
##' surface.}  \item{\code{air.temp}}{Air temperature(degree Celsius),
##' measured by the TAO buoys three meters above the sea surface.}
##' \item{\code{humidity}}{Relative humidity(%), measured by the TAO
##' buoys 3 meters above the sea surface.} \item{\code{uwind}}{The
##' East-West wind vector components(M/s).  TAO buoys measure the wind
##' speed and direction four meters above the sea surface. If it is
##' positive, the East-West component of the wind is blowing towards
##' the East. If it is negative, this component is blowing towards the
##' West.}\item{\code{vwind}}{The North-South wind vector
##' components(M/s). TAO buoys measure the wind speed and direction
##' four meters above the sea surface. If it is positive, the
##' North-South component of the wind is blowing towards the North.
##' If it is negative, this component is blowing towards the South.}}
##' @name tao
##' @docType data
##' @usage data(tao)
##' @source \url{http://www.pmel.noaa.gov/tao/data_deliv/deliv.html}
##' @keywords datasets
##' @examples
##' if (interactive()) {
##' data(tao)
##' MissingDataGUI(tao)
##' }
##'
NULL


##' The Behavioral Risk Factor Surveillance System (BRFSS) Survey
##' Data, 2009.
##'
##' The data is a subset of the 2009 survey from BRFSS, an ongoing
##' data collection program designed to measure behavioral risk
##' factors for the adult population (18 years of age or older) living
##' in households.
##'
##' Also see the codebook:
##' \url{http://ftp.cdc.gov/pub/data/brfss/codebook_09.rtf}
##'
##' Format: a data frame with 736 observations on the following 8
##' variables. \describe{\item{\code{STATE}}{A factor with 52 levels.
##' The labels and states corresponding to the labels are as follows.
##' 1:Alabama, 2:Alaska, 4:Arizona, 5:Arkansas, 6:California,
##' 8:Colorado, 9:Connecticut, 10:Delaware, 11:District of Columbia,
##' 12:Florida, 13:Georgia, 15:Hawaii, 16:Idaho, 17:Illinois,
##' 18:Indiana, 19:Iowa, 20:Kansas, 21:Kentucky, 22:Louisiana,
##' 23:Maine, 24:Maryland, 25:Massachusetts, 26:Michigan,
##' 27:Minnesota, 28:Mississippi, 29:Missouri, 30:Montana,
##' 31:Nebraska, 32:Nevada, 33:New Hampshire, 34:New Jersey, 35:New
##' Mexico, 36:New York, 37:North Carolina, 38:North Dakota, 39:Ohio,
##' 40:Oklahoma, 41:Oregon, 42:Pennsylvania, 44:Rhode Island, 45:South
##' Carolina, 46:South Dakota, 47:Tennessee, 48:Texas, 49:Utah,
##' 50:Vermont, 51:Virginia, 53:Washington, 54:West  Virginia,
##' 55:Wisconsin, 56:Wyoming, 66:Guam, 72:Puerto Rico, 78:Virgin
##' Islands} \item{\code{SEX}}{A factor with levels \code{Male}
##' \code{Female}.} \item{\code{AGE}}{A numeric vector from 7 to 97.}
##' \item{\code{HISPANC2}}{A factor with levels \code{Yes} \code{No}
##' corresponding to the question: are you Hispanic or Latino?}
##' \item{\code{VETERAN2}}{A factor with levels \code{1} \code{2}
##' \code{3} \code{4} \code{5}. The question for this variable is:
##' Have you ever served on active duty in the United States Armed
##' Forces, either in the regular military or in a National Guard or
##' military reserve unit? Active duty does not include training for
##' the Reserves or National Guard, but DOES include activation, for
##' example, for the Persian Gulf War. And the labels are meaning: 1:
##' Yes, now on active duty; 2: Yes, on active duty during the last 12
##' months, but not now; 3: Yes, on active duty in the past, but not
##' during the last 12 months; 4: No, training for Reserves or
##' National Guard only; 5: No, never served in the military.}
##' \item{\code{MARITAL}}{A factor with levels \code{Married}
##' \code{Divorced} \code{Widowed} \code{Separated}
##' \code{NeverMarried} \code{UnmarriedCouple}.}
##' \item{\code{CHILDREN}}{A numeric vector giving the number of
##' children less than 18 years of age in household.}
##' \item{\code{EDUCA}}{A factor with the education levels \code{1}
##' \code{2} \code{3} \code{4} \code{5} \code{6} as 1: Never attended
##' school or only kindergarten; 2: Grades 1 through 8 (Elementary);
##' 3: Grades 9 through 11 (Some high school); 4: Grade 12 or GED
##' (High school graduate); 5: College 1 year to 3 years (Some college
##' or technical school); 6: College 4 years or more  (College
##' graduate).} \item{\code{EMPLOY}}{A factor showing the employment
##' status with levels \code{1} \code{2} \code{3} \code{4} \code{5}
##' \code{7} \code{8}. The labels mean --  1: Employed for wages; 2:
##' Self-employed; 3: Out of work for  more than 1 year; 4: Out of
##' work for less that 1 year;  5: A homemaker; 6: A student; 7:
##' Retired; 8: Unable to work.}  \item{\code{INCOME2}}{The annual
##' household income from all sources with levels \code{<10k}
##' \code{10-15k} \code{15-20k} \code{20-25k} \code{25-35k}
##' \code{35-50k} \code{50-75k} \code{>75k} \code{Dontknow}
##' \code{Refused}.} \item{\code{WEIGHT2}}{The weight without shoes in
##' pounds.} \item{\code{HEIGHT3}}{The weight without shoes in
##' inches.} \item{\code{PREGNANT}}{Whether pregnant now with two
##' levels \code{Yes} and \code{No}.}  \item{\code{GENHLTH}}{The
##' answer to the question "in general your health is" with levels
##' \code{Excellent} \code{VeryGood} \code{Good} \code{Fair}
##' \code{Poor} \code{Refused}.}  \item{\code{PHYSHLTH}}{The number of
##' days during the last 30 days that the respondent's physical health
##' was not good.  -7 is for "Don't know/Not sure", and -9 is for
##' "Refused".}  \item{\code{MENTHLTH}}{The number of days during the
##' last 30 days that the respondent's mental health was not good.  -7
##' is for "Don't know/Not sure", and -9 is for "Refused".}
##' \item{\code{POORHLTH}}{The number of days during the last 30 days
##' that poor physical or mental health keep the respondent from doing
##' usual activities, such as self-care, work, or recreation. -7 is
##' for "Don't know/Not sure", and -9 is for "Refused".}
##' \item{\code{HLTHPLAN}}{Whether having any kind of health care
##' coverage, including health insurance, prepaid plans such as HMOs,
##' or government plans such as Medicare. The answer has two levels:
##' \code{Yes} and \code{No}.}  \item{\code{CAREGIVE}}{Whether
##' providing any such care or assistance to a friend or family member
##' during the past month, with levels \code{Yes} and \code{No}.}
##' \item{\code{QLACTLM2}}{ Whether being limited in any way in any
##' activities because of physical, mental, or emotional problems,
##' with levels  \code{Yes} and \code{No}.}
##' \item{\code{DRNKANY4}}{Whether having had at least one drink of
##' any alcoholic beverage such as beer, wine, a malt beverage or
##' liquor during the past 30 days, with levels \code{Yes} and
##' \code{No}.}  \item{\code{ALCDAY4}}{The number of days during the
##' past 30 days that the respondent had at least one drink of any
##' alcoholic beverage. -7 is for "Don't know/Not sure", and -9 is
##' for "Refused".} \item{\code{AVEDRNK2}}{The number of drinks on the
##' average the respondent had on the days when he/she drank, during
##' the past 30 days. -7 is for "Don't know/Not sure", and -9 is for
##' "Refused".} \item{\code{SMOKE100}}{ Whether having smoked at least
##' 100 cigarettes in the entire life, with levels \code{Yes} and
##' \code{No}.} \item{\code{SMOKDAY2}}{ The frequency of days now
##' smoking, with levels \code{Everyday} \code{Somedays} and
##' \code{NotAtAll}(not at all).}  \item{\code{STOPSMK2}}{Whether
##' having stopped smoking for  one day or longer during the past 12
##' months because the respondent was trying to quit smoking, with
##' levels \code{Yes} and \code{No}.} \item{\code{LASTSMK1}}{A factor
##' with levels \code{3} \code{4} \code{5} \code{6} \code{7} \code{8}
##' corresponding to the question: how long has it been since last
##' smokeing cigarettes regularly? The labels mean: 3: Within the past
##' 6 months (3 months but less than 6 months ago); 4: Within the past
##' year (6 months but less than 1 year ago); 5: Within the past 5
##' years (1 year but less than 5 years ago); 6: Within the past 10
##' years (5 years but less than 10 years ago); 7: 10 years or more;
##' 8: Never smoked regularly.} \item{\code{FRUIT}}{The number of
##' fruit the respondent eat every year, not counting juice. -7 is for
##' "Don't know/Not sure", and -9 is for "Refused".}
##' \item{\code{GREENSAL}}{The number of servings of green   salad the
##' respondent eat every year. -7 is for "Don't  know/Not sure",
##' and -9 is for "Refused".} \item{\code{POTATOES}}{ The number of
##' servings of potatoes, not including french fries, fried potatoes,
##' or potato chips, that the respondent eat every year. -7 is for
##' "Don't know/Not sure", and -9 is for "Refused".}
##' \item{\code{CARROTS}}{The number of carrots the respondent eat
##' every year. -7 is for  "Don't know/Not sure", and -9 is for
##' "Refused".}  \item{\code{VEGETABL}}{The number of servings of
##' vegetables   the respondent eat every year, not counting carrots,
##' potatoes, or salad. -7 is for "Don't know/Not sure", and -9 is
##' for "Refused".} \item{\code{FRUITJUI}}{The number of fruit juices
##' such as orange, grapefruit, or tomato that the respondent drink
##' every year. -7 is   for "Don't know/Not sure", and -9 is for
##' "Refused".}  \item{\code{BMI4}}{Body Mass Index (BMI). Computed by
##' WEIGHT in Kilograms/(HEIGHT in Meters * HEIGHT3 in Meters).
##' Missing if any of WEIGHT2 or HEIGHT3 is missing.} }
##' @name brfss
##' @docType data
##' @usage data(brfss)
##' @source \url{http://www.cdc.gov/BRFSS/technical_infodata/surveydata/2009.htm}
##' @keywords datasets
##' @examples
##' if (interactive()) {
##' data(brfss)
##' MissingDataGUI(brfss)
##' }
##'
NULL
