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
    if (is.null(origdata)) return(NULL)
    n=ncol(origdata)
    if (n==1 && substr(method,1,3) == 'MI:'){
        gmessage('You only selected one variable. Cannot apply the multiple imputation.', icon = "error")
        return(NULL)
    }
    dat = list(d1=origdata)
    origshadow = is.na(origdata[,1:n])
    if (is.null(condition)) {
      if (method == 'Below 10%') {
        for (i in 1:n) {
          if (vartype[i] %in% c('integer','numeric','logical') &
            as.numeric(as.character(missingpct))[i]>0) {
            dat$d1[origshadow[,i],i] = min(origdata[,i], na.rm=TRUE)*1.1-
              max(origdata[,i],na.rm=TRUE)/10
          }
          if (vartype[i] == "character") {
            dat$d1[origshadow[,i],i] = 'NAN'
          }
          if (vartype[i] == "factor" &
            as.numeric(as.character(missingpct))[i]<1 & as.numeric(as.character(missingpct))[i]>0) {
            dat$d1[,i]=factor(dat$d1[,i],levels = c('NAN',levels(factor(dat$d1[,i]))))
            dat$d1[origshadow[,i],i] = 'NAN'
          }
          if (vartype[i] == "factor" &
            as.numeric(as.character(missingpct))[i]==0) {
            dat$d1[,i]=factor(dat$d1[,i])
          }
        }
        names(dat)="Below 10%"
      }
      else if (method == 'Simple') {
        dat$d2=dat$d1
        for (i in 1:n) {
          dat$d1[,i] = as.numeric(impute(dat$d1[,i], fun=median))
          if (vartype[i] %in% c('integer','numeric')) {
              dat$d2[,i] = as.numeric(impute(dat$d2[,i], fun=mean))
          } else {
              dat$d2[origshadow[,i],i] = names(sort(table(na.omit(dat$d2[,i])),decreasing=TRUE))[1]
          }
        }
        names(dat)=c('Median','Mean/Mode')
      }
      else if (method == 'Hot-deck') {
          if (n==1 || sum(complete.cases(origdata))==0 || "character" %in% vartype) {
              if (n==1) warning_message='You only selected one variable. Cannot apply the nearest neighbor imputation. Only the random value imputation is given.'
              if (sum(complete.cases(origdata))==0) warning_message="All the observations have missing values. Cannot find the nearest neighbor."
              if ("character" %in% vartype) warning_message='Cannot impute the nearest neighbor with one or more character variables.'
              gmessage(warning_message, icon='warning')
              for (i in 1:n) {
                  dat$d1[,i] = as.numeric(impute(dat$d1[,i], fun='random'))
              }
              names(dat)='Random Value'
          } else {
              dat$d2=dat$d1
              for (i in 1:n) {
                  dat$d1[,i] = as.numeric(impute(dat$d1[,i], fun='random'))
              }
              CmpltDat = dat$d2[complete.cases(dat$d2),]
              for (i in which(!complete.cases(dat$d2))){
                  usecol = which(!is.na(dat$d2[i,]))
                  if (length(usecol)>0){
                      NNdat = CmpltDat
                      a = rbind(dat$d2[i,], NNdat)[,usecol]
                      NNdat$distance = dist(a)[1:nrow(NNdat)]
                      k5NNdat = NNdat[order(NNdat$distance,decreasing=FALSE),][1:min(5,nrow(NNdat)),]
                      dat$d2[i,-usecol] = colMeans(k5NNdat[,1:n][,-usecol,drop=FALSE])
                  } else {
                      dat$d2[i,] = sapply(dat$d2, median, na.rm=TRUE)
                  }
              }
              names(dat)=c('Random Value','Nearest Neighbor')
          }
      }
      else if (method == 'MI:norm') {
          if (any(c('factor','character') %in% vartype)) {
              gmessage("Not every variable is numeric. Cannot impute missing values under the multivariate normal model.", icon = "error")
              return(NULL)
          } else {
              dat$d3=dat$d2=dat$d1
              s = prelim.norm(as.matrix(dat$d1))
              thetahat = em.norm(s)
              rngseed(1234567)
              dat$d1 = imp.norm(s,thetahat,as.matrix(dat$d1))
              dat$d2 = imp.norm(s,thetahat,as.matrix(dat$d2))
              dat$d3 = imp.norm(s,thetahat,as.matrix(dat$d3))
              if (any(sapply(dat,function(x){any(c(Inf,NaN) %in% x)}))) {
                  gmessage("The algorithm doesn't converge. Return the original data with missing values.", icon = "warning")
                  dat = list(d1=origdata)
              }
              names(dat)=paste('MI',1:3)
          }
      }
      else if (method == 'MI:pmm') {
        if (sum(complete.cases(origdata))==0) {
            gmessage('All the observations have missing values. Cannot impute by Hmisc::aregImpute.', icon = "warning")
            return(NULL)
        } else {
            dat$d3=dat$d2=dat$d1
            formula0 = as.formula(paste('~ ',paste(names(origdata),collapse=' + ')))
            f = aregImpute(formula0, data=origdata, n.impute=3)
            tmpres = f$imputed
            for (i in which(!sapply(tmpres,is.null))) {
                dat$d1[rownames(tmpres[[i]]),names(tmpres)[i]]=tmpres[[i]][,1]
                dat$d2[rownames(tmpres[[i]]),names(tmpres)[i]]=tmpres[[i]][,2]
                dat$d3[rownames(tmpres[[i]]),names(tmpres)[i]]=tmpres[[i]][,3]
            }
            names(dat)=paste('MI',1:3)
        }
      }
      res = lapply(dat, function(x) data.frame(x, row_number=1:nrow(x)))
    } else {
      dat$d1 = ddply(data.frame(origdata,row_number_2011=1:nrow(origdata)),condition,singleimputation, method=method,vartype=vartype, cond=condition)
      res = dat
      res$d1 = res$d1[,(ncol(dat$d1)-n):ncol(dat$d1)]
    }
  return(res)
}
