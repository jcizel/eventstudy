##' TODO:
##'   - add module for missing value imputation (imputeTS)
##' @export
addSeries <- function(
                      edata,            #output of events2dataset
                      series_data,
                      x,                      
                      ixcol,
                      idcol='iso3',
                      datecol= 'date',
                      roll = 365,
                      thr_avail = 0.5){
    series_data  %>>%
        select_(idcol,datecol,x) ->
        dtx

                          (dtx %>>%
                           setkeyv(c(idcol,datecol)))[
                              edata %>>%
                                  setkeyv(c(idcol,datecol)), roll = roll] ->
                              data

    data[, avail := sum(!is.na(.SD))/length(.SD),by = ixcol,.SDcols = x]

    data %>>%
        subset(
            avail > thr_avail
        ) %>>%
        arrange_(
            ixcol,datecol
        ) %>>%
        select(
            -contains(x),
            contains(x)
        ) %>>%
        select(-avail) ->
        data2

    data %>>% select(contains(ixcol)) %>>% (.[[1L]]) %>>% unique %>>% length -> n1
    data2 %>>% select(contains(ixcol)) %>>% (.[[1L]]) %>>% unique %>>% length -> n2
                          (n1 - n2)/n1 -> attrition

    message(sprintf("Number of series:"))
    message(sprintf("       - before the addition of '%s': %s",x,n1))
    message(sprintf("       - after the addition of '%s': %s",x,n2))
    message(sprintf("Attrition due to availability of '%s': %s", x, attrition))

    attr(data2, 'n1') <- n1
    attr(data2, 'n2') <- n2
    attr(attrition, 'attrition') <- attrition

    return(data2)
}
