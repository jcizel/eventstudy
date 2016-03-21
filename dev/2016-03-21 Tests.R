library(data.table)
library(pipeR)
library(rlist)
library(dplyr)

load(file = '~/Data/DNB-BIS/cerutti.RData')

cerutti[, MPI := `MPI (Table A1_Annex1)` %>>% as.numeric]

cerutti %>>%
    select(
        iso3, date, eix = MPI
    ) %>>%
    subset(!is.na(iso3)) %>>%
    setkey(iso3,date) %>>%
    (dt ~ dt[, list(
         date,
         eix,
         e = eix - shift(eix,1)
     ), by = list(iso3)]) %>>%
    mutate(t = 1*(e != 0 & !is.na(e))) %>>%
    subset(t == 1) %>>%
    select(iso3,date) ->
    events
    
## Create empty balanced panel
cerutti %>>%
    subset(!is.na(iso3)) %>>%
    (iso3) %>>%
    unique ->
    id

date.min = '1998-01-01'
date.max  = '2014-12-31'

events2dataset(events, ids = id) ->
    out


## Get data on the outcome variable
FILE = "/Users/jankocizel/Data/DNB-BIS/ANALYSIS DATA/2015-06-14/dataset.RData"
load(FILE)

out[['treatment']] -> edata
dataset -> series_data

out[['treatment']] %>>%
    addSeries(
        series_data = dataset,
        x = 'banks',
        ixcol = 'treat_ix',
    ) %>>%
    addSeries(
        series_data = dataset,
        x = 'nonbanks',
        ixcol = 'treat_ix'
    ) %>>%
    addSeries(
        series_data = dataset,
        x = 'GFDD.DM.01',
        ixcol = 'treat_ix'
    )



addSeries <- function(
                      edata,            #output of events2dataset
                      series_data,
                      x,                      
                      ixcol,
                      idcol='iso3',
                      datecol= 'date',
                      roll = 365,
                      thr_avail = 0.5,
                      y2q = FALSE,
                      na.interpolation = NULL
                      ){
    dtx <- 
        series_data  %>>%
        select_(idcol,datecol,x) 

    data <-        
        (dtx %>>%
         setkeyv(c(idcol,datecol)))[
            edata %>>%
                setkeyv(c(idcol,datecol)), roll = roll] ->
        data

    if (y2q == TRUE){
        data[month(get(datecol)) != 12,
             (x) := NA]
    }

    if (!is.null(na.interpolation)){
        data %>>%
            split(. %>>% select_(ixcol) %>>% (.[[1L]])) %>>%
            list.map({
                . %>>%
                    select_(x) %>>%
                    (.[[1L]]) %>>%
                    ts %>>%
                    (t ~ try(na.interpolation(t,option = 'stine'))) ->
                    o

                    if ("try-error" %in% class(o)){
                        o <-
                            . %>>%
                            select_(x) %>>%
                            (.[[1L]]) %>>%
                            as.numeric
                    } else {
                        o <-
                            o %>>% as.numeric
                    }

                    .[, (x) := o]
            }) %>>%
             rbindlist ->
             data
    }
    
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

