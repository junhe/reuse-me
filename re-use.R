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

   # this function can be used by scale_x_continuous(breaks=auto_2exp_breaks())
   # It create several breaks and round them to a 2^(10*x). 
   # it is always like, 3*1024*1024, 41*1024*1024*1024
   # This is usually used with format_si()
   auto_2exp_breaks <- function(...) {
        function (x) {
            xmax = max(x)
            xmin = min(x)

            # Pick 4 breaks
            mybreaks = seq(0, xmax, length.out=6)

            # find the interval indices of the breaks
            exps <- seq(10, 70, by=10)
            limits <- c(0, 2^exps) # 0, 1024, 1024*1024, ...
            indices = findInterval(abs(mybreaks), limits)

            # normalize the breaks to a rounded number
            # so it looks nicer
            limits[1] = 1
            break_bases = limits[indices]
            rounded_breaks = round(mybreaks/break_bases, 0)
            rounded_breaks = round(rounded_breaks/2,0)*2 # we CS people like this
            pretty_breaks = rounded_breaks*break_bases
            pretty_breaks = unique(pretty_breaks)
           
            return (pretty_breaks)
        }
    }

    format_si_factor <- function(x, ...) {
        x = factor(x)
        l = as.numeric(levels(x))
        levels(x) = format_si(l, ...)
        return (x)
    }

    # so 3*1024*1024 will become 3G
    format_si <- function(x, show.number=T, show.unit=T, round.to=1, appendix="") {
       {
        exps <- seq(10, 70, by=10)
        limits <- c(0, 2^exps) # 0, 1024, 1024*1024, ...
        prefix <- c("", "K", "M", "G", "T", "P", "E", "Z")
     
        # Vector with array indices according to position in intervals
        indices <- findInterval(abs(x), limits)
        limits[1] = 1
        number = NULL
        if ( show.number ) {
            number = format(round(x/limits[indices], round.to),
                     trim=TRUE, scientific=FALSE)
        }
        unit = NULL
        if ( show.unit ) {
            unit = prefix[indices]
        }
        paste(number, unit, appendix, sep="")
      }
    }

    # This is used in scale_y_continuous(labels=format_2exp()) to 
    # format the labels, so 3*1024*1024 will become 3G
    format_2exp <- function(round.to=1, appendix="", ...) {
       function (x) {
        exps <- seq(10, 70, by=10)
        limits <- c(0, 2^exps) # 0, 1024, 1024*1024, ...
        prefix <- c("", "K", "M", "G", "T", "P", "E", "Z")
     
        # Vector with array indices according to position in intervals
        i <- findInterval(abs(x), limits)
        limits[1] = 1
        print(length(x))
        ret = paste(format(round(x/limits[i], round.to),
                     trim=TRUE, scientific=FALSE, ...),
              prefix[i], appendix, sep="")
        return(ret)
      }
    }


