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
          dat[shadow[,i],i] = names(sort(table(na.omit(dat[,i])),decreasing=TRUE))[1]
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
        dat[shadow] = names(sort(table(na.omit(dat)),decreasing=TRUE))[1]
      }
    }
  }
  return(data.frame(dat,row_number=row_number_2011))
}
