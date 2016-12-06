# taken basically verbatim from the render function from mrgsolve details
# https://github.com/metrumresearchgroup/mrgsolve/blob/master/rdev/inst/Rmd/mrgsolve_render_template.Rmd
model_details <- function(.mod) {
    d <- mrgsolve:::details(.mod)
    om <- mrgsolve::omat(.mod)
    dfom <- data.frame(name=unlist(mrgsolve::labels(om)),
                   value=diag(as.matrix(om)))

    sg <- mrgsolve::smat(.mod)
    dfsg <- data.frame(name=unlist(mrgsolve::labels(sg)),
                       value=diag(as.matrix(sg)))
    
    dfp <- 
        as.data.frame(t(as.data.frame(mrgsolve::allparam(.mod)))) %>% 
        setNames(.,"value") %>%
        dplyr::mutate(name=rownames(.))
    
    dfi <- as.data.frame(t(as.data.frame(init(.mod)))) %>%
        setNames(.,"value") %>%
        dplyr::mutate(name=rownames(.))
    
    dfj <- dplyr::bind_rows(dfp,dfi,dfom,dfsg)
    
    d <- dplyr::left_join(d,dfj,by="name")
    return(d)
}
