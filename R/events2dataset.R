##' @export
events2dataset <- function(
                           events,
                           ids,
                           date.min = '1998-01-01',
                           date.max = '2014-12-31',
                           pre = 8,
                           post = 8
                           ){
    events %>>% mutate(t = 1) ->
        events    

    dates <- seq(
        from = date.min %>>% as.character %>>% as.Date,
        to = date.max %>>% as.character %>>% as.Date,
        by = '3 month'
    ) - 1

    CJ(ids, dates) %>>%
        setnames(
            old = names(.),
            new = c('iso3',
                    'date')
        ) ->
        data

    ## Add events
    (events %>>% setkey(iso3,date))[data %>>% setkey(iso3,date)] %>>%
        (dt ~ dt[is.na(t), t:=0]) ->
        data2

    data2 %>>% .raw2processed(pre = pre,
                              post = post) ->
        out

    return(out)
}

## Test:
## load(file = '~/Data/DNB-BIS/cerutti.RData')
## cerutti[, MPI := `MPI (Table A1_Annex1)` %>>% as.numeric]
## cerutti %>>%
##     select(
##         iso3, date, eix = MPI
##     ) %>>%
##     subset(!is.na(iso3)) %>>%
##     setkey(iso3,date) %>>%
##     (dt ~ dt[, list(
##          date,
##          eix,
##          e = eix - shift(eix,1)
##      ), by = list(iso3)]) %>>%
##     mutate(t = 1*(e != 0 & !is.na(e))) %>>%
##     subset(t == 1) %>>%
##     select(iso3,date) ->
##     events
## events2dataset(events, ids = id) ->
##     out


## Split to TS:
.raw2processed <- function(events,
                           pre = 8,
                           post = 8){
    events %>>%
        split(. %>>% (iso3)) ->
        l

    l %>>%
        list.map({
            ## message(.name)
            . %>>%
                .Apply2TS(pre = pre,
                          post = post)
        }) ->
         o

    ## Build treatment
    o %>>%
        list.map({
            .[['treatment']]
        }) %>>%
         rbindlist ->
         df_treat

    ## Build control
    o %>>%
        list.map({
            .[['control']]
        }) %>>%
         rbindlist ->
         df_ctrl

    df_ctrl %>>%
        split(.[['ctrl_ix']]) %>>%
        list.map({
            . %>>%
                .convertInfo2TS        
        })  %>>%
         rbindlist ->
         data_ctrl

    df_treat %>>%
        split(.[['treat_ix']]) %>>%
        list.map({
            . %>>%
                .convertInfo2TS        
        })  %>>%
         rbindlist ->
         data_treat

    return(list(
        control = data_ctrl,
        treatment = data_treat
    ))
}



.convertInfo2TS <- function(info){
    dates <- seq(
        from = info[['begin']] + 1,
        to = info[['end']] + 1,
        by = '3 month'
    ) - 1

    data.table(
        info %>>% select(-date) %>>% mutate(event_date = mid),
        date = dates
    ) %>>%
        mutate(
            tte = round_any(((date - event_date) %>>% as.numeric) / 365, 0.25)
        ) ->
        out

    return(out)
}


## ts %>>% .Apply2TS(pre = 8, post = 8) ->
##     o

## For each TS check:
.Apply2TS <-
    function(
             ts,
             pre = 6,
             post = 3
             ){
    by = 1

    ts %>>%
        (t) %>>%
        rollapply(
            width = pre,
            by = by,
            FUN = function(x){
                1*((x==1) %>>% any)
            }
        ) %>>%
        (c(.,rep(NA,pre-by))) ->
        t_pre

    ts %>>%
        (t) %>>%
        rev %>>%
        rollapply(
            width = post,
            by = by,
            FUN = function(x){
                1*((x==1) %>>% any)
            }
        ) %>>%
        (c(.,rep(NA,post-by))) %>>%
        rev ->
        t_post

    ts %>>%
        mutate(
            t_pre = t_pre,
            t_post = t_post,
            t_test = 1*(t_pre | t_post) 
        ) ->
        ts2

    ts2 %>>%
        subset(
            t_test == 1
        ) ->
        ts_treat

    ts2 %>>%
        subset(
            t_test == 0
        ) ->
        ts_control

    ## Mark usable portions of ts_control
    ## Usable means that there should be pre+post-1 consecutive observations

    if (NROW(ts_control) < pre + post - 1){
        ts_control %>>%
            mutate(
                avail = FALSE
            ) ->
            ts_control2
    } else {
        ts_control %>>%   
            (date) %>>%                   
            rollapply(
                width = pre + post - 1,
                by = by,
                FUN = function(x){
                    (seq(
                        from = x %>>% min + 1,
                        to = x %>>% max + 1,
                        by = '3 month'
                    ) - 1) %>>%
                        length %>>%
                        (. == (pre + post - 1))
                }
            ) %>>%
            (c(.,rep(NA,pre+post-by-1)))  ->
            avail

        ts_control %>>%
            mutate(
                avail = avail
            ) ->
            ts_control2
    }


    ts_control2 %>>%
        subset(
            avail == TRUE
        ) %>>%
        (dt~dt[,ctrl_ix := sprintf("C.%s.%s",
                                   iso3,1:.N)]) %>>%
        select(
            iso3,date,ctrl_ix
        ) %>>%
        mutate(
            begin = date,
            mid = (date + 1 + pre * months(3)) - 1,
            end = (date + 1 + (pre + post - 1) * months(3)) - 1        
        ) ->
        ts_control3

    ## Treatment
    ts %>>%
        subset(t == 1) %>>%
        (dt~dt[,treat_ix := sprintf("T.%s.%s",
                                    iso3,1:.N)]) %>>%
        select(
            iso3,date,treat_ix
        ) %>>%
        mutate(
            begin = (date + 1 - pre * months(3)) - 1,
            mid = date,
            end = (date + 1 + (post - 1) * months(3)) - 1        
        )  ->
        ts_treatment

        return(list(
            control = ts_control3,
            treatment = ts_treatment
        ))
}




