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
##' @importFrom plyr ddply
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
            dat[origshadow] = names(sort(table(na.omit(dat)),decreasing=TRUE))[1]
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
            dat[origshadow[,i],i] = names(sort(table(na.omit(dat[,i])),decreasing=TRUE))[1]
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
