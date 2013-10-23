##' Compute the numeric summary of the missingness
##' 
##' @param dat A data frame.
##' @return A list including (1) a data frame 'missingsummary' that provides 
##' a table of missingness; (2) the total missing percentage; (3) the percent
##' of variables that contain missing values; (4) the ratio of observations
##' that have missings.
##' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
##' @export
##' @examples
##' data(tao)
##' compute_missing_pct(tao)
##' 
compute_missing_pct = function(dat){
    stopifnot(is.data.frame(dat))
    n = ncol(dat)
    totalmissingpct = mean(is.na(dat))
    varmissingpct = mean(sapply(dat,function(avec){any(is.na(avec))}))
    casemissingpct = 1-mean(complete.cases(dat))
    No_of_Case_missing = table(apply(dat, 1, function(avec){sum(is.na(avec))}))
    No_of_Case = rep(0,(n+1))
    No_of_Case[n+1-as.integer(names(No_of_Case_missing))]=No_of_Case_missing[names(No_of_Case_missing)]
    No_of_Case[n+1] = sum(complete.cases(dat))
    missingsummary = data.frame(No_of_miss_by_case=n:0,
                                No_of_Case=No_of_Case,
                                Percent=as.character(round(No_of_Case/nrow(dat)*100,1)))
    missingsummary = missingsummary[order(missingsummary$No_of_miss_by_case, decreasing=FALSE),]
    return(list(missingsummary=missingsummary,totalmissingpct=totalmissingpct,
                varmissingpct=varmissingpct,casemissingpct=casemissingpct))
}


mice_default = function(vec, dat){
    res = vec
    res[res %in% c("integer","numeric")] = "pmm"
    res[res == "logical"] = "logreg"
    for (i in which(vec %in% c('factor','character','ordered'))) {
        s = length(unique(na.omit(dat[,i])))
        if (s<3) {res[i] = "logreg"} else {
            res[i] = if (vec[i] == "ordered") {"polr"} else {"polyreg"}
        }
    }
    res[colSums(is.na(dat))==0] = ""
    return(res)
}
