##' Impute the missing data with the method selected under the
##' condition.
##'
##' This function provides eight methods for imputation with
##' categorical varaibles as conditions.
##'
##' The imputation methods: This list displays all the imputation methods.
##' Users can make one selection. (1) 'Below 10%' means NA's 
##' of one variable will be replaced by the value which equals 
##' to the minimum of the variable minus 10% of the range. 
##' Under this status the selected conditioning variables are 
##' ignored. If the data are already imputed, then this item 
##' will show the imputed result. (2) 'Simple' will create two 
##' tabs: Median and Mean/Mode. 'Median' means NA's will be 
##' replaced by the median of this variable (omit NA's). 
##' 'Mean/Mode' means NA's will be replaced by the mean of the 
##' variable (omit NA's) if it is quantitative, and by the 
##' mode of the variable (omit NA's) if it is categorical. 
##' (3) 'Hot-deck' contains two methods: 'Random Value' and 
##' 'Nearest Neighbor'. 'Random Value' means NA's will be 
##' replaced by any values of this variable (omit NA's) which 
##' are randomly selected. 'Nearest neighbor' will replace the 
##' NA's by the mean of five nearest neighbors. It requires at 
##' lease one case to be complete, at least two variables to 
##' be selected, and no character variables. It returns median 
##' for the case if all values in it are NA's.
##' (4) 'MI:areg' uses function \code{\link[Hmisc]{aregImpute}} 
##' from package \pkg{Hmisc}. It requires at lease one case to 
##' be complete, and at least two variables to be selected.
##' (5) 'MI:norm' uses function \code{\link[norm]{imp.norm}} 
##' from package \pkg{norm}. It requires all selected variables 
##' to be numeric(at least integer), and at least two variables 
##' to be selected. Sometimes it cannot converge, then the 
##' programme will leave NA's without imputation.
##' (6) 'MI:mice' uses the \pkg{mice} package.
##' (7) 'MI:mi' employes the \pkg{mi} package.
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
##' @param row_var A column name (character) that defines the ID of rows.
##' @param loop if it is in an internal loop by the conditional variables,
##' then loop=condtion.
##' @return The imputed data frame with the last column being the row
##' number from the original dataset. During the procedure of the
##' function, rows may be exchanged, thus a column of row number could
##' keep track of the original row number and then help to find the
##' shadow matrix.
##' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
##' @importFrom mice mice
##' @importFrom mice mice.impute.pmm
##' @importFrom mi mi
##' @importFrom arm bayesglm
imputation = function(origdata, method, vartype, missingpct, condition=NULL,row_var=NULL,loop=NULL){
    if (is.null(origdata)) return(NULL)
    row_NO = if (is.null(row_var)) {1:nrow(origdata)} else {origdata[,row_var]}
    cond = if (is.null(loop)) {NULL} else {origdata[,loop,drop=FALSE]}
    origdata = origdata[,setdiff(colnames(origdata),c(row_var,loop)),drop=FALSE]
    n=ncol(origdata)
    if (n==1 && substr(method,1,3) == 'MI:'){
        gmessage('You only selected one variable. Cannot apply the multiple imputation.', icon = "error")
        return(NULL)
    }
    if (!is.null(condition)) {
        dat = by(data.frame(origdata,row_id=1:nrow(origdata)),origdata[,condition],imputation,method=method,vartype=vartype,missingpct=missingpct,condition=NULL,row_var='row_id',loop=condition)
        dat = dat[!sapply(dat,is.null)]
        k1=length(dat[[1]])
        k2=length(dat)
        res=list()
        for (j in 1:k1) res[[j]]=lapply(dat,function(x) x[[j]])
        names(res)=names(dat[[1]])
        res=lapply(res, function(x){
            tmp=NULL
            for (i in 1:k2) {
                tmp=rbind(tmp,x[[i]])
            }
            return(tmp)
        })
        return(res)
    }

    dat = list(d1=origdata)
    origshadow = is.na(origdata[,1:n,drop=FALSE])
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
            if (vartype[i] %in% c("factor","ordered") &&
                    as.numeric(as.character(missingpct))[i]<1 & as.numeric(as.character(missingpct))[i]>0) {
                dat$d1[,i]=factor(dat$d1[,i],levels = c('NAN',levels(factor(dat$d1[,i]))))
                dat$d1[origshadow[,i],i] = 'NAN'
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
        if (any(c('factor','character','ordered') %in% vartype)) {
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
            names(dat)=paste('norm',1:3)
        }
    }
    else if (method == 'MI:areg') {
        if (sum(complete.cases(origdata))==0) {
            gmessage('All the observations have missing values. Cannot impute by Hmisc::aregImpute.', icon = "warning")
            return(NULL)
        } else {
            dat$d3=dat$d2=dat$d1
            formula0 = as.formula(paste('~ ',paste(names(origdata),collapse=' + ')))
            set.seed(1234567)
            f = aregImpute(formula0, data=origdata, n.impute=3)
            tmpres = f$imputed
            for (i in which(!sapply(tmpres,is.null))) {
                dat$d1[rownames(tmpres[[i]]),names(tmpres)[i]]=tmpres[[i]][,1]
                dat$d2[rownames(tmpres[[i]]),names(tmpres)[i]]=tmpres[[i]][,2]
                dat$d3[rownames(tmpres[[i]]),names(tmpres)[i]]=tmpres[[i]][,3]
            }
            names(dat)=paste('areg',1:3)
        }
    }
    else if (method == 'MI:mice') {
        dat$d3=dat$d2=dat$d1
        set.seed(1234567)
        f = mice::mice(origdata, m=3, printFlag=FALSE)
        tmpres = f$imp
        for (i in which(!sapply(tmpres,is.null))) {
            dat$d1[rownames(tmpres[[i]]),names(tmpres)[i]]=tmpres[[i]][,1]
            dat$d2[rownames(tmpres[[i]]),names(tmpres)[i]]=tmpres[[i]][,2]
            dat$d3[rownames(tmpres[[i]]),names(tmpres)[i]]=tmpres[[i]][,3]
        }
        names(dat)=paste('mice',1:3)
    }
    else if (method == 'MI:mi') {
        dat$d3=dat$d2=dat$d1
        set.seed(1234567)
        f = mi::mi(origdata)
        tmpres = f@imp
        for (i in 1:length(tmpres[[1]])) {
            dat$d1[names(tmpres[[1]][[i]]@random),names(tmpres[[1]])[i]]=tmpres[[1]][[i]]@random
            dat$d2[names(tmpres[[2]][[i]]@random),names(tmpres[[2]])[i]]=tmpres[[2]][[i]]@random
            dat$d3[names(tmpres[[3]][[i]]@random),names(tmpres[[3]])[i]]=tmpres[[3]][[i]]@random
        }
        names(dat)=paste('mi',1:3)
    }
    res = lapply(dat, function(x) if (is.null(loop)) {data.frame(x, row_number=row_NO)} else{data.frame(x, cond, row_number=row_NO)})
    return(res)
}
