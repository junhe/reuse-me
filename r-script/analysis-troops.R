# This set of functions if for tests conducted in Sept 2nd.
# In these test, the file system fragmentation is setted as
# beta distribution, the parameters of workload is same as 
# before. The only difference between this and result data before
# is that this one has framentation configuration.
require(ggplot2)
require(reshape)



sme <- function()
{
    source("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct22/analysis-troops.R")
}


files2df <- function(dirpath)
{
    #require(sqldf)
    dflist = list()
    files = list.files(dirpath)
    print(files)
    dfvec=NULL
    for (f in files) {
        if ( !grepl('zparsed', f) ) {
            next
        }
        fpath = paste(dirpath, f, sep="/")
        print(fpath)
        fidx = sub("^.*\\.", "", f)
        dfvec = append(dfvec, fidx)
        dflist[[fidx]] = read.table(fpath, header=T)
    }
    #print (str(dflist))
    # put walkman config to all other df

    #dfvec = c("_extlist", "_extstats", "_extstatssum", "_freefrag_sum",
              #"_freefrag_hist", "_freeblocks", "_freeinodes",
              #"_walkman_config")
    #conf = dflist[['_walkman_config']][,c("hostname", "jobid", "nyears", "nseasons_per_year",
                                          #"np", "ndir_per_pid", "nfile_per_dir", "nwrites_per_file",
                                          #"wsize", "wstride", "startoff")]
    
    conf = dflist[['_walkman_config']]

    for ( dfname in dfvec[ dfvec!='_walkman_config'] ) {
        print( paste( "Merging", "......." ) )
        dflist[[dfname]] = merge(dflist[[dfname]], conf, by=c("jobid")) 
    }

    return (dflist)
}

trooptrial002_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct22/datahub/trooptrial002/")

    # convert number to string and fill it with leading zeros
    zfill <- function (n, w)
    {
        return(formatC(n, width = w, format = "d", flag = "0"))
    }

    calculate_hole_per_file <- function(df)
    {
        df = arrange(df, Logical_start)
        df$ext_id = 0:(nrow(df)-1)
        df = arrange(df, Physical_start)
        n = nrow(df)
        df$Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = df$Next_phy_start - (df$Physical_end + 1)
        df$time_and_file = paste(df$monitor_time, df$filepath)
        df$file_ext_id = paste(df$filepath, zfill(df$ext_id, 5), sep=":") 
        #df$info = paste(df$Level_index, df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        df$info = paste(df$Level_index, sep=",")
        return (df)
    }

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

    regular_file_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('file', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }


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

    user_file_dir_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('pid', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }

    add_group_no_offset <- function(df)
    {
        groupsize = 128*1024*1024/4096
        df$group_no = floor(df$Physical_start / groupsize)
        df$group_start = df$group_no * groupsize
        df$in_group_start = df$Physical_start %% groupsize 
        return(df)
    }

    plot_physical_blocks_flip <- function(df, nseasons)
    {
        df = subset(df, -1 != Level_index)
        df = regular_file_subset(df)
        df = user_file_dir_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]
        
        df = add_group_no_offset(df)

        df = subset(df, monitor_time %in% c("year00000.season00001", 
                                            "year00004.season00001", 
                                            "year00006.season00001",
                                            "year00007.season00001",
                                            ""
                                            ))
        p <- ggplot(df, aes()) +
            theme_bw()+
            #geom_vline(aes(xintercept = group_start), 
                       #color="pink", 
                       #size=1 )+
            geom_segment(aes(y=Physical_start, 
                             yend=Physical_end+1,
                             x=file_ext_id, 
                             xend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            #geom_text(aes(x=Physical_start+13, y=file_ext_id, 
                          #label=info,
                          #color=filepath
                          #), size=3)+
            theme( axis.text.x = element_blank() ) +
            #theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            #scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            #scale_x_continuous(labels=groupboundaries)+
            #scale_x_continuous(limits=nicelimits)+
            #scale_x_continuous(limits=c(25000, 50000))+
            theme( axis.text.y = element_blank(), 
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank()                   
                   ) +
            theme(axis.ticks.y=element_blank())+
            ylab("Physical block")+
            xlab("Logical block number (grouped by file)")
        print (p)
    }

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, -1 != Level_index)
        df = regular_file_subset(df)
        df = user_file_dir_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]
        
        df = add_group_no_offset(df)

        p <- ggplot(df, aes()) +
            theme_bw()+
            geom_vline(aes(xintercept = group_start), 
                       color="pink", 
                       size=1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       jobid~monitor_time, 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            geom_text(aes(x=Physical_start+13, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            #scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            #scale_x_continuous(labels=groupboundaries)+
            #scale_x_continuous(limits=nicelimits)+
            #scale_x_continuous(limits=c(25000, 50000))+
            #theme( axis.text.y = element_blank(), 
                   #panel.grid.major.y = element_blank(),
                   #panel.grid.minor.y = element_blank()                   
                   #) +
            theme(axis.ticks.y=element_blank())+
            ylab("")+
            xlab("Physical block")
        print (p)
    }

    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )
    #plot_physical_blocks_flip( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}

trooptrial004_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct22/datahub/trooptrial004/")

    calculate_hole_per_file <- function(df)
    {
        df = arrange(df, Physical_start)
        Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = Next_phy_start - (df$Physical_end + 1)
        return (df)
    }

    calculate_logical_hole_per_file <- function(df)
    {
        df = arrange(df, Logical_start)
        Next_phy_start = c(df$Physical_start[-1], NA)
        df$Logical_hole_after_me = Next_phy_start - (df$Physical_end + 1)
        return (df)
    }

    user_file_dir_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('pid', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }


    # filtered by jobid, monitor_time, filepath
    ddply_calc_measurements <- function(d)
    {
        if ( nrow(d) == 0 )
            return()

        ################################
        # Select only that ones that will be plot
        # Put the calculated data to this one row data frame
        selection4plot = c('jobid', 'monitor_time', 'filepath',
                      'wstride', 'wsize', 'w_hole', 'nwrites_per_file' # variables
                      )
        d_ret = d[, selection4plot]
        d_ret = head(d_ret, n=1)

        
        ################################
        # Preparation
        d$Physical_length = d$Physical_end - d$Physical_start + 1
        d = ddply(d, .(jobid, monitor_time, filepath, Level_index), 
                     calculate_hole_per_file)
        d = ddply(d,  .(jobid, monitor_time, filepath, Level_index), 
                                  calculate_logical_hole_per_file)
        #print("-------------------------------")
        #print(head(d)[,c('filepath', 'Hole_after_me')])
        #return()


        ################################
        # span
        inode_row = subset(d, Level_index == -1)
        if (nrow(inode_row) != 1) {
            print(c("nrow(inode_row):", nrow(inode_row)))
            stop("Each file should have one and only one inode row!")
        }

        data_ext_rows = subset(d, Level_index != -1 &
                                  Level_index == Max_level)
        ext_tree_block_rows = subset(d, Level_index != -1 &
                                        Level_index < Max_level)
       
        if ( nrow(data_ext_rows) == 0 ) {
            d_ret$d_span = NA
        } else {
            tmp_blocks =  c(data_ext_rows$Physical_start, 
                          data_ext_rows$Physical_end)
            d_ret$d_span = max(tmp_blocks) - min(tmp_blocks) + 1
        }

        if ( nrow(inode_row) == 0 ) {
            d_ret$i_span = NA
        } else {
            tmp_blocks = c(data_ext_rows$Physical_start, 
                           data_ext_rows$Physical_end,
                           inode_row$Physical_start)
            d_ret$i_span = max(tmp_blocks) - min(tmp_blocks) + 1
        }

        if ( nrow(inode_row) > 0 && nrow(data_ext_rows) > 0 ) {
            tmp_blocks = c(data_ext_rows$Physical_start, 
                           data_ext_rows$Physical_end,
                           ext_tree_block_rows$Physical_start,
                           ext_tree_block_rows$Physical_end,
                           inode_row$Physical_start)  
            d_ret$full_span = max(tmp_blocks) - min(tmp_blocks) + 1
        } else {
            d_ret$full_span = NA
        }



        ################################
        # Number of extents
        d_ret$n_data_extents = sum( d$Level_index == d$Max_level & 
                              d$Level_index != -1, 
                             na.rm=F )
        d_ret$n_ext_tree_block = sum( d$Level_index < d$Max_level & 
                                d$Level_index != -1, 
                             na.rm=F )

        ################################
        # file size
        # ?
        d_ret$file_size = d_ret$wstride * (d_ret$nwrites_per_file-1) + d_ret$wsize

        ################################
        # Layout score of data blocks
        ################################

        ################################
        #  Physical layout score:
        #   (#_of_blocks - #_of_extents)/(#_of_blocks - 1)
        nblocks = sum(data_ext_rows$Physical_length)
        nholes = nrow( subset(data_ext_rows, Hole_after_me > 0) )
        
        # Physical layout score
        if ( nblocks == 0 ) {
            # some files do not have data extents (small directory?)
            d_ret$Physical_layout_score = NA            
        } else {
            if ( nblocks == 1 ) {
                # Only one block 
                d_ret$Physical_layout_score = 1
            } else {
                d_ret$Physical_layout_score = (nblocks - 1 - nholes)/(nblocks - 1)
                #print(paste("score", d_ret$Physical_layout_score, "nblocks", nblocks, "nholes", nholes))
                #if ( nholes==121 ) {
                    #print(data_ext_rows[,c(selection4plot[-1], 'Hole_after_me', "Physical_start", "Physical_end", "Physical_length")])
                #}
            }
        }


        ################################
        #  Logical layout score:
        #   Optimal blocks:
        #    1. physical contiguous
        #    2. logical contiguous
        #  
        #  Algorithm:
        #   1. Sort by logical_start
        #   2. Calculate physical holes
        #   3. The non-zero holes indicate a non-optimal block
        n_logical_holes = nrow( subset(data_ext_rows, 
                                       Logical_hole_after_me > 0 |
                                       Logical_hole_after_me < 0 ) ) # NA does not count
        
        if ( nblocks == 0 ) {
            d_ret$Logical_layout_score = NA
        } else {
            if ( nblocks == 1 ) {
                d_ret$Logical_layout_score = 1
            } else {
                d_ret$Logical_layout_score = (nblocks - 1 - n_logical_holes)/(nblocks - 1)
            }
        }

        #print("-------------------------------")
        #print(head(d_ret)[,c(selection4plot, 'd_span', 'i_span', 'full_span', 'n_data_extents', 'n_ext_tree_block')])
        #print(head(d_ret)[,c(selection4plot, 'Physical_layout_score')])
        ##print(c(nrow(data_ext_rows), nrow(ext_tree_block_rows)))
        #return()
        return (d_ret)
    }

    # plot number of extents for the file
    n_extents_plot <- function(d)
    {
        d = user_file_dir_subset(d)
        d = ddply(d, .(jobid, monitor_time, filepath), ddply_calc_measurements)
        #print ((d[,-(1:2)]))
        d$wsize_factor = factor(d$wsize)
        p <- ggplot(d, aes(x=factor(w_hole), y=factor(nwrites_per_file))) +
            geom_tile(aes(fill=n_data_extents), color='orange') +
            facet_grid(wsize_factor~.)
        print(p)
    }

    d = lst[['_extlist']]
    n_extents_plot(d)

}

trooptrial006_main <- function()
{
    testname = "006"
    if ( ! exists(paste('glst', testname, sep="")) ) {
        print(c(testname, "does not exist! Loading from file..."))
        glst006 <<- files2df(paste("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct22/datahub/trooptrial", 
                                   testname, "/", sep=""))
        lst <- glst006
    } else {
        print(c(testname, "exists"))
        lst <- glst006
    }

    calculate_hole_per_file <- function(df)
    {
        df = arrange(df, Physical_start)
        Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = Next_phy_start - (df$Physical_end + 1)
        return (df)
    }

    calculate_logical_hole_per_file <- function(df)
    {
        df = arrange(df, Logical_start)
        Next_phy_start = c(df$Physical_start[-1], NA)
        df$Logical_hole_after_me = Next_phy_start - (df$Physical_end + 1)
        return (df)
    }

    user_file_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('file', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }

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


    # filtered by jobid, monitor_time, filepath
    ddply_calc_measurements <- function(d)
    {
        if ( nrow(d) == 0 )
            return()

        ################################
        # Select only that ones that will be plot
        # Put the calculated data to this one row data frame
        selection4plot = c('jobid', 'monitor_time', 'filepath',
                      'wstride', 'wsize', 'w_hole', 'nwrites_per_file', 'fsync_per_write' # variables
                      )
        d_ret = d[, selection4plot]
        d_ret = head(d_ret, n=1)

        
        ################################
        # Preparation
        d$Physical_length = d$Physical_end - d$Physical_start + 1
        d = ddply(d, .(jobid, monitor_time, filepath, Level_index), 
                     calculate_hole_per_file)
        d = ddply(d,  .(jobid, monitor_time, filepath, Level_index), 
                                  calculate_logical_hole_per_file)
        #print("-------------------------------")
        #print(head(d)[,c('filepath', 'Hole_after_me')])
        #return()


        ################################
        # span
        inode_row = subset(d, Level_index == -1)
        if (nrow(inode_row) != 1) {
            print(c("nrow(inode_row):", nrow(inode_row)))
            stop("Each file should have one and only one inode row!")
        }

        data_ext_rows = subset(d, Level_index != -1 &
                                  Level_index == Max_level)
        ext_tree_block_rows = subset(d, Level_index != -1 &
                                        Level_index < Max_level)
       
        if ( nrow(data_ext_rows) == 0 ) {
            d_ret$d_span = NA
        } else {
            tmp_blocks =  c(data_ext_rows$Physical_start, 
                          data_ext_rows$Physical_end)
            d_ret$d_span = max(tmp_blocks) - min(tmp_blocks) + 1
        }

        if ( nrow(inode_row) == 0 ) {
            d_ret$i_span = NA
        } else {
            tmp_blocks = c(data_ext_rows$Physical_start, 
                           data_ext_rows$Physical_end,
                           inode_row$Physical_start)
            d_ret$i_span = max(tmp_blocks) - min(tmp_blocks) + 1
        }

        if ( nrow(inode_row) > 0 && nrow(data_ext_rows) > 0 ) {
            tmp_blocks = c(data_ext_rows$Physical_start, 
                           data_ext_rows$Physical_end,
                           ext_tree_block_rows$Physical_start,
                           ext_tree_block_rows$Physical_end,
                           inode_row$Physical_start)  
            d_ret$full_span = max(tmp_blocks) - min(tmp_blocks) + 1
        } else {
            d_ret$full_span = NA
        }



        ################################
        # Number of extents
        d_ret$n_data_extents = sum( d$Level_index == d$Max_level & 
                              d$Level_index != -1, 
                             na.rm=F )
        d_ret$n_ext_tree_block = sum( d$Level_index < d$Max_level & 
                                d$Level_index != -1, 
                             na.rm=F )

        ################################
        # file size
        # ?
        d_ret$file_size = d_ret$wstride * (d_ret$nwrites_per_file-1) + d_ret$wsize

        ################################
        # Layout score of data blocks
        ################################

        ################################
        #  Physical layout score:
        #   (#_of_blocks - #_of_extents)/(#_of_blocks - 1)
        nblocks = sum(data_ext_rows$Physical_length)
        d_ret$n_data_blocks = nblocks
        nholes = nrow( subset(data_ext_rows, Hole_after_me > 0) )
        
        # Physical layout score
        if ( nblocks == 0 ) {
            # some files do not have data extents (small directory?)
            d_ret$Physical_layout_score = NA            
        } else {
            if ( nblocks == 1 ) {
                # Only one block 
                d_ret$Physical_layout_score = 1
            } else {
                d_ret$Physical_layout_score = (nblocks - 1 - nholes)/(nblocks - 1)
                #print(paste("score", d_ret$Physical_layout_score, "nblocks", nblocks, "nholes", nholes))
                #if ( nholes==121 ) {
                    #print(data_ext_rows[,c(selection4plot[-1], 'Hole_after_me', "Physical_start", "Physical_end", "Physical_length")])
                #}
            }
        }


        ################################
        #  Logical layout score:
        #   Optimal blocks:
        #    1. physical contiguous
        #    2. logical contiguous
        #  
        #  Algorithm:
        #   1. Sort by logical_start
        #   2. Calculate physical holes
        #   3. The non-zero holes indicate a non-optimal block
        n_logical_holes = nrow( subset(data_ext_rows, 
                                       Logical_hole_after_me > 0 |
                                       Logical_hole_after_me < 0 ) ) # NA does not count
        
        if ( nblocks == 0 ) {
            d_ret$Logical_layout_score = NA
        } else {
            if ( nblocks == 1 ) {
                d_ret$Logical_layout_score = 1
            } else {
                d_ret$Logical_layout_score = (nblocks - 1 - n_logical_holes)/(nblocks - 1)
            }
        }

        #print("-------------------------------")
        #print(head(d_ret)[,c(selection4plot, 'd_span', 'i_span', 'full_span', 'n_data_extents', 'n_ext_tree_block')])
        #print(head(d_ret)[,c(selection4plot, 'Physical_layout_score')])
        ##print(c(nrow(data_ext_rows), nrow(ext_tree_block_rows)))
        #return()
        return (d_ret)
    }

    # I want to compare different span in the same plot
    # I want to compare different layout scores in the same plot
    # I want to see number of data extents and tree blocks in the same figure. 
    troops_plot <- function(d)
    {
        measures = c('d_span', 'i_span', 'full_span', 
                     'n_data_extents', 'n_ext_tree_block',
                     'Physical_layout_score', 'Logical_layout_score'
                     )
        #measures = c( 
                     #'n_data_extents', 'n_ext_tree_block'
                     #)

        d = user_file_subset(d)
        d = ddply(d, .(jobid, monitor_time, filepath), ddply_calc_measurements)
        



        #plot_layout_score(d)
        #plot_span(d, normalize=T)
        #plot_span(d, normalize=F)
        plot_n_extents(d)
        return()

        for (measure in measures) {
            d$text_label = format(d[,measure], digit=3, scientifc=T)
            p <- ggplot(d, aes(x=factor(w_hole), y=factor(nwrites_per_file)))+
                  geom_tile(aes_string(fill=measure)) +
                  scale_fill_gradient(low="white", high="red")+
                  geom_text(aes(label=text_label), color='blue')+
                  facet_grid(fsync_per_write~wsize)
            windows()
            print(p)
        }
    }

    plot_n_extents <- function(d)
    {
        selection4plot = c('jobid', 'monitor_time', 'filepath',
                      'wstride', 'wsize', 'w_hole', 'nwrites_per_file', 'fsync_per_write' # variables
                      )
        d = melt(d, id=selection4plot, 
                 measure=c('n_data_extents', 'n_ext_tree_block'))
        d$value_text = format(d$value, big.mark=",")

        d$wsize = factor(d$wsize)
        wsizes = levels(d$wsize)
        d$nwrites_per_file = factor(d$nwrites_per_file)
        d$fsync_per_write = factor(d$fsync_per_write)
        d$w_hole = num_vec_to_readable_factor(d$w_hole)
        #levels(d$wsize) = paste("wSize:", levels(d$wsize), sep="")
        levels(d$nwrites_per_file) = paste("nWrites:", levels(d$nwrites_per_file), sep="")
        levels(d$fsync_per_write) = paste("fsync", levels(d$fsync_per_write), sep="")
        #levels(d$w_hole) = paste("hole:", levels(d$w_h

        for ( ws in wsizes ) {
            print(ws)
            dplot = subset(d, wsize == ws)
            p <- ggplot(dplot, aes(x=w_hole, y=value, color=variable))+
                  geom_bar(aes(fill=variable), stat="identity", position='dodge') +
                  geom_text(aes(y=value, label=value_text, hjust=variable), 
                            position=position_dodge(width=1), 
                            size=2.5, angle=90, color='black')+
                  facet_grid(nwrites_per_file~fsync_per_write) +
                  theme( axis.text.x = element_text(angle=45) ) + 
                  ylab("Number of Data Extents/Extent Tree Blocks") +
                  scale_y_continuous(labels=comma) +
                  ggtitle( paste("Write size:", num_vec_to_readable_factor(ws)) )
            windows()
            print(p)
        }
    }

    plot_span <- function(d, normalize=T)
    {
        if ( normalize ) {
            d[,c('i_span', 'd_span', 'full_span')] = d[,c('i_span', 'd_span', 'full_span')] / d$n_data_blocks
        } 
        selection4plot = c('jobid', 'monitor_time', 'filepath',
                      'wstride', 'wsize', 'w_hole', 'nwrites_per_file', 'fsync_per_write' # variables
                      )
        d = melt(d, id=selection4plot, 
                 measure=c('d_span', 'i_span', 'full_span'))
        d$value_text = format(d$value, digits=3, scientific=T)

        d$wsize = factor(d$wsize)
        wsizes = levels(d$wsize)
        d$nwrites_per_file = factor(d$nwrites_per_file)
        d$fsync_per_write = factor(d$fsync_per_write)
        d$w_hole = num_vec_to_readable_factor(d$w_hole)
        #levels(d$wsize) = paste("wSize:", levels(d$wsize), sep="")
        levels(d$nwrites_per_file) = paste("nWrites:", levels(d$nwrites_per_file), sep="")
        levels(d$fsync_per_write) = paste("fsync", levels(d$fsync_per_write), sep="")
        #levels(d$w_hole) = paste("hole:", levels(d$w_h

        for ( ws in wsizes ) {
            print(ws)
            dplot = subset(d, wsize == ws)
            p <- ggplot(dplot, aes(x=w_hole, y=value, color=variable))+
                  geom_bar(aes(fill=variable), stat="identity", position='dodge') +
                  geom_text(aes(y=value+1000, label=value_text, hjust=variable), 
                            position=position_dodge(width=1), 
                            size=2.5, angle=90, color='black')+
                  facet_grid(nwrites_per_file~fsync_per_write) +
                  theme( axis.text.x = element_text(angle=45) ) + 
                  ylab("Span") +
                  scale_y_continuous(labels=comma)
                  ggtitle( paste("Write size:", num_vec_to_readable_factor(ws)) )
            windows()
            print(p)
        }
    }

    plot_layout_score <- function(d)
    {
        selection4plot = c('jobid', 'monitor_time', 'filepath',
                      'wstride', 'wsize', 'w_hole', 'nwrites_per_file', 'fsync_per_write' # variables
                      )
        d = melt(d, id=selection4plot, 
                 measure=c('Physical_layout_score', 'Logical_layout_score'))
        d$value_text = sprintf("%2.2f", d$value)

        
        d$wsize = factor(d$wsize)
        wsizes = levels(d$wsize)
        d$nwrites_per_file = factor(d$nwrites_per_file)
        d$fsync_per_write = factor(d$fsync_per_write)
        d$w_hole = factor(d$w_hole)
        #levels(d$wsize) = paste("wSize:", levels(d$wsize), sep="")
        #levels(d$nwrites_per_file) = paste("nWrites:", levels(d$nwrites_per_file), sep="")
        #levels(d$fsync_per_write) = paste("fsync", levels(d$fsync_per_write), sep="")
        #levels(d$w_hole) = paste("hole:", levels(d$w_hole), sep="")

        for ( ws in wsizes ) {
            print(ws)
            dplot = subset(d, wsize == ws)
            p <- ggplot(dplot, aes(x=w_hole, y=value))+
                  geom_bar(aes(fill=variable), stat="identity", position='dodge') +
                  geom_text(aes(y=value+0.1, color=variable, label=value_text), position=position_dodge(width=1), size=3)+
                  facet_grid(nwrites_per_file~fsync_per_write)
            windows()
            print(p)
        }
    }

    d = lst[['_extlist']]
    troops_plot(d)

}

trooptrial007_main <- function()
{
    testname='007'
    DoCalculation <- function()
    {
        return (F)
        #return (T)
        #if ( !is.null(g_list_exists[[testname]]) ) {
            ## g_list_exits is marked
            #return (F)
        #} else {
            #if ( !exists( 'g_list_exists' ) ) {
                #g_list_exists <<- list()
            #}
            #g_list_exists[[testname]] = T
            #return (T)
        #}

    }

    if ( DoCalculation() ) {
        print(c(testname, "does not exist! Loading from file..."))
        lst <<- files2df(paste("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct22/datahub/trooptrial", 
                                   testname, "/", sep=""))
    } else {
        print(c(testname, "exists"))
    }


    calculate_hole_per_file <- function(df)
    {
        df = arrange(df, Physical_start)
        Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = Next_phy_start - (df$Physical_end + 1)
        return (df)
    }

    calculate_logical_hole_per_file <- function(df)
    {
        df = arrange(df, Logical_start)
        Next_phy_start = c(df$Physical_start[-1], NA)
        df$Logical_hole_after_me = Next_phy_start - (df$Physical_end + 1)
        return (df)
    }

    user_file_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('file', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }


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

    format_si <- function(...) {
        function (x) {
            translate_to_human_readable(x)
        }
    }

    # filtered by jobid, monitor_time, filepath
    ddply_calc_measurements <- function(d)
    {
        if ( nrow(d) == 0 )
            return()

        ################################
        # Select only that ones that will be plot
        # Put the calculated data to this one row data frame
        selection4plot = c('jobid', 'monitor_time', 'filepath',
                      'wstride', 'wsize', 'w_hole', 'nwrites_per_file', 'fsync_per_write' # variables
                      )
        d_ret = d[, selection4plot]
        d_ret = head(d_ret, n=1)

        
        ################################
        # Preparation
        d$Physical_length = d$Physical_end - d$Physical_start + 1
        d = ddply(d, .(jobid, monitor_time, filepath, Level_index), 
                     calculate_hole_per_file)
        d = ddply(d,  .(jobid, monitor_time, filepath, Level_index), 
                                  calculate_logical_hole_per_file)
        #print("-------------------------------")
        #print(head(d)[,c('filepath', 'Hole_after_me')])
        #return()


        ################################
        # span
        inode_row = subset(d, Level_index == -1)
        if (nrow(inode_row) != 1) {
            print(c("nrow(inode_row):", nrow(inode_row)))
            stop("Each file should have one and only one inode row!")
        }

        data_ext_rows = subset(d, Level_index != -1 &
                                  Level_index == Max_level)
        ext_tree_block_rows = subset(d, Level_index != -1 &
                                        Level_index < Max_level)
       
        if ( nrow(data_ext_rows) == 0 ) {
            d_ret$d_span = NA
        } else {
            tmp_blocks =  c(data_ext_rows$Physical_start, 
                          data_ext_rows$Physical_end)
            d_ret$d_span = max(tmp_blocks) - min(tmp_blocks) + 1
        }

        if ( nrow(inode_row) == 0 ) {
            d_ret$i_span = NA
        } else {
            tmp_blocks = c(data_ext_rows$Physical_start, 
                           data_ext_rows$Physical_end,
                           inode_row$Physical_start)
            d_ret$i_span = max(tmp_blocks) - min(tmp_blocks) + 1
        }

        if ( nrow(inode_row) > 0 && nrow(data_ext_rows) > 0 ) {
            tmp_blocks = c(data_ext_rows$Physical_start, 
                           data_ext_rows$Physical_end,
                           ext_tree_block_rows$Physical_start,
                           ext_tree_block_rows$Physical_end,
                           inode_row$Physical_start)  
            d_ret$full_span = max(tmp_blocks) - min(tmp_blocks) + 1
        } else {
            d_ret$full_span = NA
        }



        ################################
        # Number of extents
        d_ret$n_data_extents = sum( d$Level_index == d$Max_level & 
                              d$Level_index != -1, 
                             na.rm=F )
        d_ret$n_ext_tree_block = sum( d$Level_index < d$Max_level & 
                                d$Level_index != -1, 
                             na.rm=F )

        ################################
        # file size
        # ?
        d_ret$file_size = d_ret$wstride * (d_ret$nwrites_per_file-1) + d_ret$wsize
        if ( nrow(data_ext_rows) == 0 ) {
            d_ret$max_logical_block_num = NA
        } else {
            d_ret$max_logical_block_num = max(data_ext_rows$Logical_end)
        }

        ################################
        # Layout score of data blocks
        ################################

        ################################
        #  Physical layout score:
        #   (#_of_blocks - #_of_extents)/(#_of_blocks - 1)
        nblocks = sum(data_ext_rows$Physical_length)
        d_ret$n_data_blocks = nblocks
        nholes = nrow( subset(data_ext_rows, Hole_after_me > 0) )
        
        # Physical layout score
        if ( nblocks == 0 ) {
            # some files do not have data extents (small directory?)
            d_ret$Physical_layout_score = NA            
        } else {
            if ( nblocks == 1 ) {
                # Only one block 
                d_ret$Physical_layout_score = 1
            } else {
                d_ret$Physical_layout_score = (nblocks - 1 - nholes)/(nblocks - 1)
                #print(paste("score", d_ret$Physical_layout_score, "nblocks", nblocks, "nholes", nholes))
                #if ( nholes==121 ) {
                    #print(data_ext_rows[,c(selection4plot[-1], 'Hole_after_me', "Physical_start", "Physical_end", "Physical_length")])
                #}
            }
        }


        ################################
        #  Logical layout score:
        #   Optimal blocks:
        #    1. physical contiguous
        #    2. logical contiguous
        #  
        #  Algorithm:
        #   1. Sort by logical_start
        #   2. Calculate physical holes
        #   3. The non-zero holes indicate a non-optimal block
        n_logical_holes = nrow( subset(data_ext_rows, 
                                       Logical_hole_after_me > 0 |
                                       Logical_hole_after_me < 0 ) ) # NA does not count
        
        if ( nblocks == 0 ) {
            d_ret$Logical_layout_score = NA
        } else {
            if ( nblocks == 1 ) {
                d_ret$Logical_layout_score = 1
            } else {
                d_ret$Logical_layout_score = (nblocks - 1 - n_logical_holes)/(nblocks - 1)
            }
        }

        #print("-------------------------------")
        #print(head(d_ret)[,c(selection4plot, 'd_span', 'i_span', 'full_span', 'n_data_extents', 'n_ext_tree_block')])
        #print(head(d_ret)[,c(selection4plot, 'Physical_layout_score')])
        ##print(c(nrow(data_ext_rows), nrow(ext_tree_block_rows)))
        #return()
        return (d_ret)
    }

    # I want to compare different span in the same plot
    # I want to compare different layout scores in the same plot
    # I want to see number of data extents and tree blocks in the same figure. 
    troops_plot <- function(d)
    {

        
        if ( DoCalculation() ) {
            #measures = c('d_span', 'i_span', 'full_span', 
                         #'n_data_extents', 'n_ext_tree_block',
                         #'Physical_layout_score', 'Logical_layout_score'
                         #)
            d = user_file_subset(d)
            global_d <<- ddply(d, .(jobid, monitor_time, filepath), ddply_calc_measurements)
        }
        
        #plot_layout_score(global_d)
        #plot_span(global_d, normalize=T)
        plot_span(global_d, normalize=F)
        #plot_n_extents(global_d)
        return()

        #for (measure in measures) {
            #d$text_label = format(d[,measure], digit=3, scientifc=T)
            #p <- ggplot(d, aes(x=factor(w_hole), y=factor(nwrites_per_file)))+
                  #geom_tile(aes_string(fill=measure)) +
                  #scale_fill_gradient(low="white", high="red")+
                  #geom_text(aes(label=text_label), color='blue')+
                  #facet_grid(fsync_per_write~wsize)
            #windows()
            #print(p)
        #}
    }

    plot_n_extents <- function(d)
    {
        selection4plot = c('jobid', 'monitor_time', 'filepath',
                      'wstride', 'wsize', 'w_hole', 'nwrites_per_file', 'fsync_per_write' # variables
                      )
        d = melt(d, id=selection4plot, 
                 measure=c('n_data_extents', 'n_ext_tree_block'))
        d$value_text = format(d$value, big.mark=",")

        d$wsize = factor(d$wsize)
        wsizes = levels(d$wsize)
        d$nwrites_per_file = factor(d$nwrites_per_file)
        d$fsync_per_write = factor(d$fsync_per_write)
        d$w_hole = num_vec_to_readable_factor(d$w_hole)
        #levels(d$wsize) = paste("wSize:", levels(d$wsize), sep="")
        levels(d$nwrites_per_file) = paste("nWrites:", levels(d$nwrites_per_file), sep="")
        levels(d$fsync_per_write) = paste("fsync", levels(d$fsync_per_write), sep="")
        #levels(d$w_hole) = paste("hole:", levels(d$w_h

        for ( ws in wsizes ) {
            print(ws)
            dplot = subset(d, wsize == ws)
            p <- ggplot(dplot, aes(x=w_hole, y=value, color=variable))+
                  geom_bar(aes(fill=variable), stat="identity", position='dodge') +
                  geom_text(aes(y=value, label=value_text, hjust=variable), 
                            position=position_dodge(width=1), 
                            size=2.5, angle=90, color='black')+
                  facet_grid(nwrites_per_file~fsync_per_write) +
                  theme( axis.text.x = element_text(angle=45) ) + 
                  ylab("Number of Data Extents/Extent Tree Blocks") +
                  scale_y_continuous(labels=comma) +
                  ggtitle( paste("Write size:", num_vec_to_readable_factor(ws)) )
            windows()
            print(p)
        }
    }

    plot_span <- function(d, normalize=T)
    {
        print(d$max_logical_block_num)
        return()
        if ( normalize ) {
            d[,c('i_span', 'd_span', 'full_span')] = d[,c('i_span', 'd_span', 'full_span')] / (d$max_logical_block_num + 1)
        } 
        selection4plot = c('jobid', 'monitor_time', 'filepath',
                      'wstride', 'wsize', 'w_hole', 'nwrites_per_file', 'fsync_per_write' # variables
                      )
        d = melt(d, id=selection4plot, 
                 measure=c('d_span', 'i_span', 'full_span'))
        d$value_text = format(d$value, digits=3, scientific=T)

        d$wsize = factor(d$wsize)
        wsizes = levels(d$wsize)
        d$nwrites_per_file = factor(d$nwrites_per_file)
        d$fsync_per_write = factor(d$fsync_per_write)
        d$w_hole = num_vec_to_readable_factor(d$w_hole)
        #levels(d$wsize) = paste("wSize:", levels(d$wsize), sep="")
        levels(d$nwrites_per_file) = paste("nWrites:", levels(d$nwrites_per_file), sep="")
        levels(d$fsync_per_write) = paste("fsync", levels(d$fsync_per_write), sep="")
        #levels(d$w_hole) = paste("hole:", levels(d$w_h

        for ( ws in wsizes ) {
            print(ws)
            dplot = subset(d, wsize == ws)
            #print(head(dplot))
            p <- ggplot(dplot, aes(x=w_hole, y=value, color=variable))+
                  geom_bar(aes(fill=variable), stat="identity", position='dodge') +
                  geom_text(aes(y=value+1000, label=value_text, hjust=variable ), 
                            position=position_dodge(width=1), 
                            size=2.5, angle=90, color='black')+
                  facet_grid(nwrites_per_file~fsync_per_write) +
                  theme( axis.text.x = element_text(angle=45) ) + 
                  ylab("Span")+ 
                  scale_y_continuous(labels=format_si)+
                  ggtitle( paste("Normalization:", normalize, ",", "Write size:", num_vec_to_readable_factor(ws)) )
            windows()
            print(p)
        }
    }

    plot_layout_score <- function(d)
    {
        selection4plot = c('jobid', 'monitor_time', 'filepath',
                      'wstride', 'wsize', 'w_hole', 'nwrites_per_file', 'fsync_per_write' # variables
                      )
        d = melt(d, id=selection4plot, 
                 measure=c('Physical_layout_score', 'Logical_layout_score'))
        d$value_text = sprintf("%2.2f", d$value)

        
        d$wsize = factor(d$wsize)
        wsizes = levels(d$wsize)
        d$nwrites_per_file = factor(d$nwrites_per_file)
        d$fsync_per_write = factor(d$fsync_per_write)
        d$w_hole = num_vec_to_readable_factor(d$w_hole)
        #levels(d$wsize) = paste("wSize:", levels(d$wsize), sep="")
        levels(d$nwrites_per_file) = paste("nWrites:", levels(d$nwrites_per_file), sep="")
        levels(d$fsync_per_write) = paste("fsync", levels(d$fsync_per_write), sep="")
        #levels(d$w_hole) = paste("hole:", levels(d$w_hole), sep="")

        for ( ws in wsizes ) {
            print(ws)
            dplot = subset(d, wsize == ws)
            p <- ggplot(dplot, aes(x=w_hole, y=value))+
                  geom_bar(aes(fill=variable), stat="identity", position='dodge') +
                  geom_text(aes(y=value+0.1, color=variable, label=value_text), 
                            position=position_dodge(width=1), size=2.5)+
                  facet_grid(nwrites_per_file~fsync_per_write) +
                  theme( axis.text.x = element_text(angle=45) ) + 
                  ylab("Layout Score (0~1, Higher is better)") +
                  ggtitle( paste("Write size:", num_vec_to_readable_factor(ws)) )
            windows()
            print(p)
        }
    }

    d = lst[['_extlist']]
    troops_plot(d)

}

trooptrial008_main <- function()
{
    testname='008'
    DoCalculation <- function()
    {
        return (F)
        #return (T)
        #if ( !is.null(g_list_exists[[testname]]) ) {
            ## g_list_exits is marked
            #return (F)
        #} else {
            #if ( !exists( 'g_list_exists' ) ) {
                #g_list_exists <<- list()
            #}
            #g_list_exists[[testname]] = T
            #return (T)
        #}

    }

    if ( DoCalculation() ) {
        print(c(testname, "does not exist! Loading from file..."))
        lst <<- files2df(paste("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct22/datahub/trooptrial", 
                                   testname, "/", sep=""))
    } else {
        print(c(testname, "exists"))
    }


    calculate_hole_per_file <- function(df)
    {
        df = arrange(df, Physical_start)
        Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = Next_phy_start - (df$Physical_end + 1)
        return (df)
    }

    calculate_logical_hole_per_file <- function(df)
    {
        df = arrange(df, Logical_start)
        Next_phy_start = c(df$Physical_start[-1], NA)
        df$Logical_hole_after_me = Next_phy_start - (df$Physical_end + 1)
        return (df)
    }

    user_file_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('file', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }


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

    # filtered by jobid, monitor_time, filepath
    ddply_calc_measurements <- function(d)
    {
        if ( nrow(d) == 0 )
            return()

        ################################
        # Select only that ones that will be plot
        # Put the calculated data to this one row data frame
        selection4plot = c('jobid', 'monitor_time', 'filepath',
                      'wstride', 'wsize', 'w_hole', 'nwrites_per_file', 'fsync_per_write' # variables
                      )
        d_ret = d[, selection4plot]
        d_ret = head(d_ret, n=1)

        
        ################################
        # Preparation
        d$Physical_length = d$Physical_end - d$Physical_start + 1
        d = ddply(d, .(jobid, monitor_time, filepath, Level_index), 
                     calculate_hole_per_file)
        d = ddply(d,  .(jobid, monitor_time, filepath, Level_index), 
                                  calculate_logical_hole_per_file)
        #print("-------------------------------")
        #print(head(d)[,c('filepath', 'Hole_after_me')])
        #return()


        ################################
        # span
        inode_row = subset(d, Level_index == -1)
        if (nrow(inode_row) != 1) {
            print(c("nrow(inode_row):", nrow(inode_row)))
            stop("Each file should have one and only one inode row!")
        }

        data_ext_rows = subset(d, Level_index != -1 &
                                  Level_index == Max_level)
        ext_tree_block_rows = subset(d, Level_index != -1 &
                                        Level_index < Max_level)
       
        if ( nrow(data_ext_rows) == 0 ) {
            d_ret$d_span = NA
        } else {
            tmp_blocks =  c(data_ext_rows$Physical_start, 
                          data_ext_rows$Physical_end)
            d_ret$d_span = max(tmp_blocks) - min(tmp_blocks) + 1
        }

        if ( nrow(inode_row) == 0 ) {
            d_ret$i_span = NA
        } else {
            tmp_blocks = c(data_ext_rows$Physical_start, 
                           data_ext_rows$Physical_end,
                           inode_row$Physical_start)
            d_ret$i_span = max(tmp_blocks) - min(tmp_blocks) + 1
        }

        if ( nrow(inode_row) > 0 && nrow(data_ext_rows) > 0 ) {
            tmp_blocks = c(data_ext_rows$Physical_start, 
                           data_ext_rows$Physical_end,
                           ext_tree_block_rows$Physical_start,
                           ext_tree_block_rows$Physical_end,
                           inode_row$Physical_start)  
            d_ret$full_span = max(tmp_blocks) - min(tmp_blocks) + 1
        } else {
            d_ret$full_span = NA
        }



        ################################
        # Number of extents
        d_ret$n_data_extents = sum( d$Level_index == d$Max_level & 
                              d$Level_index != -1, 
                             na.rm=F )
        d_ret$n_ext_tree_block = sum( d$Level_index < d$Max_level & 
                                d$Level_index != -1, 
                             na.rm=F )

        ################################
        # file size
        # ?
        d_ret$file_size = d_ret$wstride * (d_ret$nwrites_per_file-1) + d_ret$wsize
        if ( nrow(data_ext_rows) == 0 ) {
            d_ret$max_logical_block_num = NA
        } else {
            d_ret$max_logical_block_num = max(data_ext_rows$Logical_end)
        }


        ################################
        # Layout score of data blocks
        ################################

        ################################
        #  Physical layout score:
        #   (#_of_blocks - #_of_extents)/(#_of_blocks - 1)
        nblocks = sum(data_ext_rows$Physical_length)
        d_ret$n_data_blocks = nblocks
        nholes = nrow( subset(data_ext_rows, Hole_after_me > 0) )
        
        # Physical layout score
        if ( nblocks == 0 ) {
            # some files do not have data extents (small directory?)
            d_ret$Physical_layout_score = NA            
        } else {
            if ( nblocks == 1 ) {
                # Only one block 
                d_ret$Physical_layout_score = 1
            } else {
                d_ret$Physical_layout_score = (nblocks - 1 - nholes)/(nblocks - 1)
                #print(paste("score", d_ret$Physical_layout_score, "nblocks", nblocks, "nholes", nholes))
                #if ( nholes==121 ) {
                    #print(data_ext_rows[,c(selection4plot[-1], 'Hole_after_me', "Physical_start", "Physical_end", "Physical_length")])
                #}
            }
        }


        ################################
        #  Logical layout score:
        #   Optimal blocks:
        #    1. physical contiguous
        #    2. logical contiguous
        #  
        #  Algorithm:
        #   1. Sort by logical_start
        #   2. Calculate physical holes
        #   3. The non-zero holes indicate a non-optimal block
        n_logical_holes = nrow( subset(data_ext_rows, 
                                       Logical_hole_after_me > 0 |
                                       Logical_hole_after_me < 0 ) ) # NA does not count
        
        if ( nblocks == 0 ) {
            d_ret$Logical_layout_score = NA
        } else {
            if ( nblocks == 1 ) {
                d_ret$Logical_layout_score = 1
            } else {
                d_ret$Logical_layout_score = (nblocks - 1 - n_logical_holes)/(nblocks - 1)
            }
        }

        #print("-------------------------------")
        #print(head(d_ret)[,c(selection4plot, 'd_span', 'i_span', 'full_span', 'n_data_extents', 'n_ext_tree_block')])
        #print(head(d_ret)[,c(selection4plot, 'Physical_layout_score')])
        ##print(c(nrow(data_ext_rows), nrow(ext_tree_block_rows)))
        #return()
        return (d_ret)
    }


    plot_n_extents <- function(d)
    {
        selection4plot = c('jobid', 'monitor_time', 'filepath',
                      'wstride', 'wsize', 'w_hole', 'nwrites_per_file', 'fsync_per_write' # variables
                      )
        d = melt(d, id=selection4plot, 
                 measure=c('n_data_extents', 'n_ext_tree_block'))
        d$value_text = format(d$value, big.mark=",")

        d$wsize = factor(d$wsize)
        wsizes = levels(d$wsize)
        d$nwrites_per_file = factor(d$nwrites_per_file)
        d$fsync_per_write = factor(d$fsync_per_write)
        d$w_hole = format_si_factor(d$w_hole, appendix="B")
        #levels(d$wsize) = paste("wSize:", levels(d$wsize), sep="")
        levels(d$nwrites_per_file) = paste("nWrites:", levels(d$nwrites_per_file), sep="")
        levels(d$fsync_per_write) = paste("fsync", levels(d$fsync_per_write), sep="")
        #levels(d$w_hole) = paste("hole:", levels(d$w_h

        for ( ws in wsizes ) {
            print(ws)
            dplot = subset(d, wsize == ws)
            .e <- environment()
            yymax = max(dplot$value, na.rm=T) * 1.25
            p <- ggplot(dplot, aes(x=w_hole, y=value, color=variable), 
                        environment=.e)+
                  geom_bar(aes(fill=variable), stat="identity", position='dodge') +
                  geom_text(aes(y=yymax, label=value_text), 
                            hjust=1,
                            position=position_dodge(width=1), 
                            size=2.5, angle=90)+
                  facet_grid(nwrites_per_file~fsync_per_write) +
                  theme( axis.text.x = element_text(angle=45) ) + 
                  ylab("Number of Data Extents/Extent Tree Blocks") +
                  scale_y_continuous(labels=comma) +
                  theme(legend.title=element_blank(), legend.position='top')+
                  ggtitle( paste("Write size:", num_vec_to_readable_factor(ws)) )
            windows()
            print(p)
            #return()
        }
    }

    plot_span <- function(d, normalize=T)
    {

        # Calculate normalization if needed
        if ( normalize ) {
            d[,c('i_span', 'd_span', 'full_span')] = 
                d[,c('i_span', 'd_span', 'full_span')] / (d$max_logical_block_num+1)
        }

        # wide to long conversion
        selection4plot = c('jobid', 'monitor_time', 'filepath',
                      'wstride', 'wsize', 'w_hole', 'nwrites_per_file', 'fsync_per_write' # variables
                      )
        d = melt(d, id=selection4plot, 
                 measure=c('d_span', 'i_span', 'full_span'))


        if ( normalize == F ) {
            # for non-normalized span, we use Bytes instead of number of blocks
            d$value = d$value*4096
        }

        # prepare value text
        if ( normalize ) {
            append.char = ""
        } else {
            append.char = "B"
        }
        d$value_text = format_si(d$value, 
                                 show.number=T, 
                                 show.unit=T, 
                                 round.to=0,
                                 appendix=append.char)  #TODO: use K, M, G with out number here, use bytes all oveeer the place?

        d$wsize = factor(d$wsize)
        wsizes = levels(d$wsize)
        d$nwrites_per_file = factor(d$nwrites_per_file)
        d$fsync_per_write = factor(d$fsync_per_write)
        d$w_hole = format_si_factor(d$w_hole, appendix="B")
        #levels(d$wsize) = paste("wSize:", levels(d$wsize), sep="")
        levels(d$nwrites_per_file) = paste("nWrites:", levels(d$nwrites_per_file), sep="")
        levels(d$fsync_per_write) = paste("fsync", levels(d$fsync_per_write), sep="")
        #levels(d$w_hole) = paste("hole:", levels(d$w_h

        for ( ws in wsizes ) {
            print(ws)
            dplot = subset(d, wsize == ws)
            #print(head(dplot))
            .e <- environment()
            yymax = max(dplot$value, na.rm=T) * 1.2
            print(dplot$value)
            print(c("yymax:",yymax))
            p <- ggplot(dplot, aes(x=w_hole, y=value, color=variable), environment=.e)+
                  geom_bar(aes(fill=variable), stat="identity", position='dodge') +
                  geom_text(aes(y=yymax, label=value_text), 
                            hjust=1,
                            position=position_dodge(width=1), 
                            size=2.5, angle=90)+
                  facet_grid(nwrites_per_file~fsync_per_write) +
                  theme( axis.text.x = element_text(angle=45) ) + 
                  ylab(paste("Span (Normalization=", normalize, ")"))+ 
                  scale_y_continuous(
                                     #breaks=seq(0, yymax, by=1024*1024*1024), 
                                     #limits=auto_limits(),
                                     limits=c(0, yymax*1.2),
                                     breaks=auto_2exp_breaks(),
                                     labels=format_2exp(round.to=1, appendix=append.char))+
                  theme(legend.title=element_blank(), legend.position='top')+
                  ggtitle( paste("Write size:", num_vec_to_readable_factor(ws)) )
            windows()
            print(p)
            #return()
        }
    }

    plot_layout_score <- function(d)
    {
        selection4plot = c('jobid', 'monitor_time', 'filepath',
                      'wstride', 'wsize', 'w_hole', 'nwrites_per_file', 'fsync_per_write' # variables
                      )
        d = melt(d, id=selection4plot, 
                 measure=c('Physical_layout_score', 'Logical_layout_score'))
        d$value_text = sprintf("%2.2f", d$value)

        
        d$wsize = factor(d$wsize)
        wsizes = levels(d$wsize)
        d$nwrites_per_file = factor(d$nwrites_per_file)
        d$fsync_per_write = factor(d$fsync_per_write)
        d$w_hole = format_si_factor(d$w_hole, appendix="B")
        #levels(d$wsize) = paste("wSize:", levels(d$wsize), sep="")
        levels(d$nwrites_per_file) = paste("nWrites:", levels(d$nwrites_per_file), sep="")
        levels(d$fsync_per_write) = paste("fsync", levels(d$fsync_per_write), sep="")
        #levels(d$w_hole) = paste("hole:", levels(d$w_hole), sep="")

        for ( ws in wsizes ) {
            print(ws)
            dplot = subset(d, wsize == ws)
            .e <- environment()
            yymax = max(dplot$value, na.rm=T) * 1.2
            p <- ggplot(dplot, aes(x=w_hole, y=value), environment=.e)+
                  geom_bar(aes(fill=variable), stat="identity", position='dodge') +
                  geom_text(aes(y=yymax, color=variable, label=value_text), 
                            hjust=1,
                            position=position_dodge(width=1), 
                            size=2.5, angle=90)+
                  facet_grid(nwrites_per_file~fsync_per_write) +
                  theme( axis.text.x = element_text(angle=45) ) + 
                  scale_y_continuous(limits=c(0, yymax)) +
                  ylab("Layout Score (0~1, Higher is better)") +
                  theme(legend.title=element_blank(), legend.position='top')+
                  ggtitle( paste("Write size:", num_vec_to_readable_factor(ws)) )
            windows()
            print(p)
            #return()
        }
    }

    # I want to compare different span in the same plot
    # I want to compare different layout scores in the same plot
    # I want to see number of data extents and tree blocks in the same figure. 
    troops_plot <- function(d)
    {
        if ( DoCalculation() ) {
            d = user_file_subset(d)
            global_d <<- ddply(d, .(jobid, monitor_time, filepath), ddply_calc_measurements)
        }
        
        plot_layout_score(global_d)
        #plot_span(global_d, normalize=T)
        #plot_span(global_d, normalize=F)
        #plot_n_extents(global_d)
        return()
    }

    d = lst[['_extlist']]
    troops_plot(d)

}

