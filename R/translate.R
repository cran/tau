translate_Unicode_latin_ligatures <-
function(x)
{
    ## Note that we cannot use chartr() ...
    ## Note that using \uxxxx is not portable (grr), hence we provide
    ## the Unicode characters themselves, inserting them via Emacs'
    ## C-x 8 RET (ucs-insert).
    x <- gsub("Ĳ", "IJ", x)
    x <- gsub("ĳ", "ij", x)
    x <- gsub("Œ", "OE", x)
    x <- gsub("œ", "oe", x)
    x <- gsub("ﬀ", "ff", x)
    x <- gsub("ﬁ", "fi", x)
    x <- gsub("ﬂ", "fl", x)
    x <- gsub("ﬃ", "ffi", x)
    x <- gsub("ﬄ", "ffl", x)
    x <- gsub("ﬅ", "st", x)
    x <- gsub("ﬆ", "st", x)
    x
}
