{ 
    # for ploting.
    # extend the min and max of physical block 
    # so the figure can show more
    # INPUT: _extlist
    get_extended_xlimits <- function(df)
    {
        xmin = min(df$Physical_start)
        xmax = max(df$Physical_end)
        xlen = xmax - xmin
        xadjust = xlen * 0.2
        xmin = xmin-xadjust
        xmax = xmax+xadjust
        return (c(xmin, xmax))
    }

    # subset of the files, not directory
    # INPUT: _extlist
    regular_file_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('file', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }

    # Extract the year and season out of monitor_time
    # INPUT: _extlist
    add_year_season <- function (df) {
        extract_year <- function( monitor_time ) {
            y = substr(as.character(monitor_time), 5,9)
            y = as.numeric(y)
        }

        extract_season <- function( monitor_time ) {
            s = substr(as.character(monitor_time), 17,21)
            s = as.numeric(s)
        }

        df$monitor_year = extract_year(df$monitor_time)
        df$monitor_season = extract_season(df$monitor_time)
        return (df)
    }

    # subset of the files that I created
    # INPUT: _extlist
    user_file_dir_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('pid', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }
}


