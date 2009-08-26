parse_IETF_language_tag <-
function(x)
{
    ## No support for grandfathered tags for now.
    ## Hence, assume the form is
    ##   language [-script] [-region] [-variant]
    ## where
    ##   language       2*3ALPHA        ISO 639 (shortest)
    ##                  4ALPHA          reserved for future use
    ##                  5*8ALPHA        registered language subtag
    ##   script         4ALPHA          ISO 15924
    ##   region         2ALPHA          ISO 3166
    ##                  3DIGIT          UN M.49
    ##   variant        5*8alphanum     registered variants
    ##                  DIGIT 3alphanum

    nms <- c("Language", "Script", "Region", "Variant")

    re_language <- "^[[:alpha:]]{2,8}$"
    re_script <- "^[[:alpha:]]{4}$"
    re_region <- "^([[:alpha:]]{2}|[[:digit:]]{3})$"
    re_variant <- "^([[:alnum:]]{5,8}|[[:digit:]][[:alnum:]]{3})$"
    
    process_chunks <- function(s) {
        out <- rep.int(NA_character_, 4L)
        names(out) <- nms
        n <- length(s)
        p <- s[1L]
        if(!grepl(re_language, p))
            stop(gettextf("Invalid language: %s", p))
        out[1L] <- p
        i <- 2L
        while(i <= n) {
            p <- s[i]
            if(grepl(re_script, p))
                out["Script"] <- p
            else if(grepl(re_region, p))
                out["Region"] <- p
            else if(grepl(re_variant, p))
                out["Variant"] <- p
            else
                stop(gettextf("Invalid subtag: %s", p))
            i <- i + 1L
            cat(i, "\n")
        }
        ## If there are leftovers ...
        if(i <= n)
            stop(gettextf("Invalid langtag: %s", s))
        out
    }

    do.call(rbind,
            lapply(strsplit(x, "-", fixed = TRUE), process_chunks))
}

## Not sure what to do with all grandfathered langtag from the IANA
## registry.  Some have a preferred language value ... but the rest?

