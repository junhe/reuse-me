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

# This function takes a numeric vector as input and
# output a char vector. 
# This one will round the nubmer to 1 xiao shu dian.
format_2exp <- function(x) {
   {
	exps <- seq(10, 70, by=10)
    limits <- c(0, 2^exps) # 0, 1024, 1024*1024, ...
    prefix <- c("", "K", "M", "G", "T", "P", "E", "Z")
  
    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)
    limits[1] = 1
    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE),
          prefix[i])
  }
}

