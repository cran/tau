readBytes <-
function(con)
{
    s <- rawToChar(readBin(con, raw(), .Machine$integer.max))
    Encoding(s) <- "bytes"
    s
}

readChars <-
function(con, encoding = "")    
{
    s <- rawToChar(readBin(con, raw(), .Machine$integer.max))
    iconv(s, from = encoding, to = "UTF-8", sub = "byte")
}
