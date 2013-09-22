
# translate a numeric vecotor to factor in 
# a human readable format.
# this is only for 2^k to "K","M","G" translation
# it uses translate_to_human_readable
num_vec_to_readable_factor <- function(v)
{
    v = factor(v)
    l = levels(v)
    l = as.numeric(l)
    levels(v) = sapply(l, translate_to_human_readable)
    return(v)
}

# translate a number to 3K 3M 3G ... format
# For example, 1024 will be translate to 1K
# it only handles 2^k numbers. if it is not,
# it will return the original
translate_to_human_readable <- function(x)
{
    absx = abs(x)
    unit = ""
    if ( absx < 1024 ) {
        divider = 1
        unit = ""
    } else if ( absx < 1024*1024 ) {
        divider = 1024
        unit = "K"
    } else if ( absx < 1024*1024*1024) {
        divider = 1024 * 1024
        unit = "M"
    } else {
        divider = 1024 * 1024 * 1024
        unit = "G"
    }
    
    if ( x %% divider == 0 ) {
        y = x/divider
        return (paste(y,unit,sep=""))        
    } else {
        return (as.character(x))
    }
     
}
