parse_IETF_language_tag <-
function(x, expand = FALSE)
{
    n <- length(x)
    y <- rep.int(list(character()), n)
    names(y) <- x

    ## Start by expanding grandfathered tags if possible.
    pos <- match(x,
                 IANA_language_subtag_registry_grandfathered_table$Tag,
                 nomatch = 0L)
    x[pos > 0L] <-
        IANA_language_subtag_registry_grandfathered_table$"Preferred-Value"[pos]
    ## This will give NA for grandfathered tags with no replacement.

    pos <- which(!is.na(x))

    if(!length(pos)) {
        if(expand)
            y[] <- rep.int(list(list()), n)
        return(y)
    }

    x <- x[pos]

    ## Non-NA tags should now be as follows:
    ##   language [-script] [-region] *[-variant] *[-extension] [-privateuse]
    ## where
    ##   language       2*3ALPHA        ISO 639 (shortest)
    ##                  [-extlang]      extended language subtag
    ##                  4ALPHA          reserved for future use
    ##                  5*8ALPHA        registered language subtag
    ##   extlang        3*ALPHA         selected ISO 639 codes
    ##                                  (ignoring permantly reserved)
    ##   script         4ALPHA          ISO 15924
    ##   region         2ALPHA          ISO 3166
    ##                  3DIGIT          UN M.49
    ##   variant        5*8alphanum     registered variants
    ##                  DIGIT 3alphanum
    ##   extension      singleton-2*8ALNUM
    ##   singleton      ALNUM but not x
    ##   privateuse     x-1*8ALNUM

    re_language <- "[[:alpha:]]{2,8}"
    re_extlang <- "[[:alpha:]]{3}"
    re_script <- "[[:alpha:]]{4}"
    re_region <- "[[:alpha:]]{2}|[[:digit:]]{3}"
    re_variant <- "[[:alnum:]]{5,8}|[[:digit:]][[:alnum:]]{3}"
    re_extension <-
        "([0123456789ABCDEFGHIJKLMNOPQRSTUVWYZabcdefghijklmnopqrstuvwyz])-([[:alnum:]]{2,8})"
    re_privateuse <- "(x)-([[:alnum:]]{1,8})"

    ## <NOTE>
    ## There currently seems to be a problem with e.g.
    ##   grepl("^[[:alpha:]]{2,}", "123")
    ## in R 2.11.x and R 2.12.x: hence use perl = TRUE.
    ## </NOTE>

    ## Language.
    pat <- sprintf("^(%s)(-.*|$)", re_language)
    ind <- grepl(pat, x, perl = TRUE)
    if(!all(ind))
        stop("Invalid language tag(s):",
             paste("\n ", x[!ind], collapse = "\n"))
    y[pos] <-
        as.list(sprintf("Language=%s", sub(pat, "\\1", x, perl = TRUE)))
    x <- sub(pat, "\\2", x, perl = TRUE)
    ind <- nzchar(x)
    pos <- pos[ind]
    x <- x[ind]
    
    while(length(x)) {
        ## Use a loop so that we can stop when done.
    
        ## Extlang.
        pat <- sprintf("^-(%s)(-.*|$)", re_extlang)
        ind <- grepl(pat, x, perl = TRUE)
        if(any(ind)) {
            y[pos[ind]] <-
                Map(c,
                    y[pos[ind]],
                    sprintf("Extlang=%s",
                            sub(pat, "\\1", x[ind], perl = TRUE)))
            x[ind] <- sub(pat, "\\2", x[ind], perl = TRUE)
            ind <- nzchar(x)
            pos <- pos[ind]
            x <- x[ind]
            if(!length(x)) break
        }
        
        ## Script.
        pat <- sprintf("^-(%s)(-.*|$)", re_script)
        ind <- grepl(pat, x, perl = TRUE)
        if(any(ind)) {
            y[pos[ind]] <-
                Map(c,
                    y[pos[ind]],
                    sprintf("Script=%s",
                            sub(pat, "\\1", x[ind], perl = TRUE)))
            x[ind] <- sub(pat, "\\2", x[ind], perl = TRUE)
            ind <- nzchar(x)
            pos <- pos[ind]
            x <- x[ind]
            if(!length(x)) break
        }
        
        ## Region.
        pat <- sprintf("^-(%s)(-.*|$)", re_region)
        ind <- grepl(pat, x, perl = TRUE)
        if(any(ind)) {
            y[pos[ind]] <-
                Map(c,
                    y[pos[ind]],
                    sprintf("Region=%s",
                            sub(pat, "\\1", x[ind], perl = TRUE)))
            x[ind] <- sub(pat, "\\2", x[ind], perl = TRUE)
            ind <- nzchar(x)
            pos <- pos[ind]
            x <- x[ind]
            if(!length(x)) break
        }
        
        ## Variant(s).
        pat <- sprintf("^-(%s)(-.*|$)", re_variant)
        while(any(ind <- grepl(pat, x, perl = TRUE))) {
            y[pos[ind]] <-
                Map(c,
                    y[pos[ind]],
                    sprintf("Region=%s",
                            sub(pat, "\\1", x[ind], perl = TRUE)))
            x[ind] <- sub(pat, "\\2", x[ind], perl = TRUE)
            ind <- nzchar(x)
            pos <- pos[ind]
            x <- x[ind]
            if(!length(x)) break
        }
        
        ## Extension(s).
        pat <- sprintf("^-%s(-.*|$)", re_extension)
        while(any(ind <- grepl(pat, x, perl = TRUE))) {
            ## <NOTE>
            ## This discards the singleton prefixes for now.
            ## But storing them as names or in the tags both seems rather
            ## awkward ...
            y[pos[ind]] <-
                Map(c,
                    y[pos[ind]],
                    sprintf("Region=%s",
                            sub(pat, "\\2", x[ind], perl = TRUE)))
            ## </NOTE>
            x[ind] <- sub(pat, "\\3", x[ind], perl = TRUE)
            ind <- nzchar(x)
            pos <- pos[ind]
            x <- x[ind]
            if(!length(x)) break
        }
        
        ## Private use.
        pat <- sprintf("^-%s(-.*|$)", re_privateuse)
        ind <- grepl(pat, x, perl = TRUE)
        if(any(ind)) {
            y[pos[ind]] <-
                Map(c,
                    y[pos[ind]],
                    sprintf("Privateuse=%s",
                            sub(pat, "\\2", x[ind], perl = TRUE)))
            x[ind] <- sub(pat, "\\3", x[ind], perl = TRUE)
        }

        break
    }
    
    if(any(ind <- nzchar(x)))
        stop("Invalid language tag(s):",
             paste("\n ", names(y)[ind], collapse = "\n"))

    if(!expand) return(y)

    x <- tolower(unlist(y))
    pos <- match(x, IANA_language_subtag_registry$Index)
    z <- IANA_language_subtag_registry$Description[pos]
    ## Special case private use ranges.
    if(!all(sapply(z, length))) {
        pos <- match(x,
                     IANA_language_subtag_registry_private_use_index_table)
        z[pos > 0L] <- "Private use"
    }
    z <- Map(`names<-`,
             split(z, rep.int(seq_along(y), sapply(y, length))),
             y)
    names(z) <- names(y)
    z
}

get_IANA_language_subtag_registry <-
function(con = "http://www.iana.org/assignments/language-subtag-registry")
{
    ## This is a collection of records in tag-value format, but
    ## unfortunately separated by '%%' lines rather than empty lines, so
    ## we cannot use read.dcf() directly.  Let us keep things simple:
    ## extract the records, write them out as DCF, and call read.dcf().

    lines <- readLines(con)
    ## The first line is something like
    ##   File-Date: 2009-03-13
    ## which we drop for reading the records.
    fdate <- sub(".*: *", "", lines[1L])
    pos <- grep("^%%", lines)
    lines[c(seq_len(pos[1L]), pos[-1L])] <- ""
    tcon <- textConnection(lines, encoding = "UTF-8")
    on.exit(close(tcon))
    db <- read.dcf(tcon, all = TRUE)
    ## Add index for lookups.
    db$Index <- tolower(sprintf("%s=%s", db$Type, db$Subtag))
    db$Type <- factor(db$Type)
    attr(db, "File_Date") <- fdate
    db
}

IANA_language_subtag_registry_language_private_use_subtags <-
    outer(letters[1L : 20L], letters,
          function(u, v) sprintf("q%s%s", u, v))
IANA_language_subtag_registry_script_private_use_subtags <-
    outer(c("a", "b"), letters[1L : 24L],
          function(u, v) sprintf("Qa%s%s", u, v))
IANA_language_subtag_registry_region_private_use_subtags <-
    c(sprintf("Q%s", LETTERS[13L : 26L]),
      sprintf("X%s", LETTERS))

IANA_language_subtag_registry_private_use_index_table <-
    tolower(c(sprintf("Language=%s",
                      IANA_language_subtag_registry_language_private_use_subtags),
              sprintf("Script=%s",
                      IANA_language_subtag_registry_script_private_use_subtags),
              sprintf("Region=%s",
                      IANA_language_subtag_registry_region_private_use_subtags)))

