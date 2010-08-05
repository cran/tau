parse_IETF_language_tag <-
function(x, expand = FALSE)
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
        }
        ## If there are leftovers ...
        if(i <= n)
            stop(gettextf("Invalid langtag: %s", s))
        out
    }

    y <- do.call(rbind,
                 lapply(strsplit(x, "-", fixed = TRUE),
                        process_chunks))
    if(!expand)
        return(y)

    pos <- match(sprintf("%s=%s",
                         rep(tolower(colnames(y)), each = nrow(y)),
                         y),
                 IANA_language_subtag_registry$Index)
    ind <- !is.na(y)
    ## Note that descriptions can have more than one element, so give a
    ## character frame ...
    z <- rep.int(list(character()), length(y))
    z[ind] <- IANA_language_subtag_registry$Description[pos][ind]
    z <- as.data.frame(matrix(z, nrow(y)))
    dimnames(z) <- list(x, colnames(y))
    z
}

## Not sure what to do with all grandfathered langtag from the IANA
## registry.  Some have a preferred language value ... but the rest?

get_IANA_language_subtag_registry <-
function(con = "http://www.iana.org/assignments/language-subtag-registry")
{
    ## This is a collection of records in tag-value format, but
    ## unfortunately separated by '%%' lines rather than empty lines, so
    ## we cannot use read.dcf() directly.  Let us keep things simple:
    ## extract the records, write them out as DCF, and call read.dcf().

    lines <- readLines(con)
    pos <- grep("^%%", lines)
    ## The first chunk is something like
    ##   File-Date: 2009-03-13
    ## which we drop.
    lines[c(seq_len(pos[1L]), pos[-1L])] <- ""
    tcon <- textConnection(lines, encoding = "UTF-8")
    on.exit(close(tcon))
    db <- read.dcf(tcon, all = TRUE)
    ## Add index for lookups.
    db$Index <- sprintf("%s=%s", db$Type, db$Subtag)
    db$Type <- factor(db$Type)
    db
}

