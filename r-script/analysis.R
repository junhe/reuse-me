# This set of functions if for tests conducted in Sept 2nd.
# In these test, the file system fragmentation is setted as
# beta distribution, the parameters of workload is same as 
# before. The only difference between this and result data before
# is that this one has framentation configuration.
require(ggplot2)
require(reshape)



sme <- function()
{
    source("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/analysis.R")
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

pick_by_stride <- function(keys, stride) 
{
    n = length(keys)
    selects = seq(1,n,by=stride)
    keys = sort(keys)
    return (keys[selects])
}

# You speicify either df or rfile. 
# rfile has to have a data frame named df
fdist_free_space_hist2 <- function(df=NULL, rfile="")
{
    if ( is.null(df) ) {
        load(file=rfile)#, globalenv())
    } else {
        # pick monitors
        pickedMons = pick_by_stride(unique(df$monitor_time), stride=1)
        df = subset(df, monitor_time %in% pickedMons)

        # build factor
        df$Free_Space_Dist_ID = paste("alpha:", df$alpha, ",", "beta:", df$beta, sep="")
        df$Free_Space.Dist_ID = factor(df$Free_Space_Dist_ID)
        df$Workload_ID = paste("np", df$np, ",",
                               "nd", df$ndir_per_pid, ",",
                               "nf", df$nfile_per_dir, ",",
                               "nw", df$nwrites_per_file, "\n",
                               "ws", df$wsize, ",",
                               "wst", df$wstride, ",",
                               "so", df$startoff,
                               sep="")
        df$Workload_ID = factor(df$Workload_ID)

        # pick free spaces
        nFree_Space_Dist_ID = 6 
        distids = unique(df$Free_Space_Dist_ID)
        print(distids)
        picked_distids = head(distids, n=nFree_Space_Dist_ID)
        df = subset(df, Free_Space_Dist_ID %in% picked_distids)

        # fragment data calculation
        df$Fragment_Block_Count = df$end - df$start + 1
        df$Fragment_Log2Size = log(df$Fragment_Block_Count*4096, 2)
        
        #print(summary(df$Workload_ID))
        save(df, file="h7check01.freespacehist.Rdata")
        return(NULL)
    }

    df <- df_filter(df, 
            p_np=4, p_nd=4, p_nf=4, p_nw=4096, 
            p_ws=1024, p_wst=c(2048), p_so=0, p_alpha=2, p_beta=5,
            p_monitor_time=c("year00000.season00000",
                            "year00001.season00000")
                             #"year00002.season00000")
            #p_monitor_time=monitors
            ) 

    df$Free_Space_Dist_ID = paste("alpha:", df$alpha, ",", "beta:", df$beta, sep="")
    df$Free_Space.Dist_ID = factor(df$Free_Space_Dist_ID)
    df$Workload_ID = paste("np", df$np, ",",
                           "nd", df$ndir_per_pid, ",",
                           "nf", df$nfile_per_dir, ",",
                           "nw", df$nwrites_per_file, "\n",
                           "ws", df$wsize, ",",
                           "wst", df$wstride, ",",
                           "so", df$startoff,
                           sep="")
    df$Workload_ID = factor(df$Workload_ID)

    df$Fragment_Block_Count = df$end - df$start + 1
    df$Fragment_Log2Size = log(df$Fragment_Block_Count*4096, 2)


    lvls = unique( as.character(df$Workload_ID) )
    df$Workload_ID = factor(df$Workload_ID, levels=lvls)

    mlvl = length(unique(as.character(df$Workload_ID)))
    ranges = list( 1:11, 12:22, 23:33) 

    for (myrng in ranges) {
        dfp = df
        p = ggplot(dfp, aes(
                           x=Fragment_Log2Size,
                           #x=Fragment_Block_Count, 
                           color=monitor_time
                           )) +
            geom_freqpoly(binwidth=1)+
            #geom_histogram(aes(fill=monitor_time), position='dodge') +
            #geom_density()+
            #facet_grid(Free_Space_Dist_ID~Workload_ID)+
            scale_x_continuous(breaks=seq(12, 28, by=2),
                               labels=(2^seq(12, 28, by=2)/1024))+
            xlab("Fragment Size(KB)")+
            theme(axis.text.x=element_text(angle=45,hjust=1))
            #scale_y_log10(limits=c(0.001, 1e6))
        print(p)
        return()
    }
}


fdist_free_space_hist_E <- function(df)
{
    # pick monitors
    #pickedMons = pick_by_stride(unique(df$monitor_time), stride=1)
    #df = subset(df, monitor_time %in% pickedMons)
    df = subset(df, 
                    alpha == 5 & 
                    beta == 5 &
                    np == 8 &
                    ndir_per_pid == 4 &
                    nfile_per_dir == 16 &
                    nwrites_per_file == 4096 &
                    wsize == 128 &
                    wstride == 256 & 
                    startoff == 0) 

    df$Free_Space_Dist_ID = paste("alpha:", df$alpha, ",", "beta:", df$beta, sep="")
    df$Free_Space.Dist_ID = factor(df$Free_Space_Dist_ID)
    df$Workload_ID = paste("np", df$np, ",",
                           "nd", df$ndir_per_pid, ",",
                           "nf", df$nfile_per_dir, ",",
                           "nw", df$nwrites_per_file, "\n",
                           "ws", df$wsize, ",",
                           "wst", df$wstride, ",",
                           "so", df$startoff,
                           sep="")
    df$Workload_ID = factor(df$Workload_ID)

    # pick free spaces
    nFree_Space_Dist_ID = 6 
    distids = unique(df$Free_Space_Dist_ID)
    print(distids)
    picked_distids = head(distids, n=nFree_Space_Dist_ID)
    df = subset(df, Free_Space_Dist_ID %in% picked_distids)

    # pick workloads
    nWorkload_ID = 20 
    wlids = unique(df$Workload_ID)
    print(wlids)
    picked_wlids = head(wlids, n=nWorkload_ID)
    df = subset(df, Workload_ID %in% picked_wlids)


    df$Fragment_Block_Count = df$end - df$start + 1
    df$Fragment_Log2Size = log(df$Fragment_Block_Count*4096, 2)
    
    print(head(df))
    
    p = ggplot(df, aes(
                       x=Fragment_Log2Size,
                       #x=Fragment_Block_Count, 
                       color=monitor_time
                       )) +
        geom_freqpoly(binwidth=1)+
        #geom_histogram(aes(fill=monitor_time), position='dodge') +
        #geom_density()+
        facet_grid(Free_Space_Dist_ID~Workload_ID)+
        scale_x_continuous(breaks=seq(12, 28, by=2),
                           labels=(2^seq(12, 28, by=2)/1024))+
        xlab("Fragment Size(KB)")+
        theme(axis.text.x=element_text(angle=45,hjust=1))+
        scale_y_log10(limits=c(0.001, 1e6))
    print(p)
}



fdist_free_space_hist <- function(df)
{
    # pick monitors
    pickedMons = pick_by_stride(unique(df$monitor_time), stride=1)
    df = subset(df, monitor_time %in% pickedMons)


    df$Free_Space_Dist_ID = paste("alpha:", df$alpha, ",", "beta:", df$beta, sep="")
    df$Free_Space.Dist_ID = factor(df$Free_Space_Dist_ID)
    df$Workload_ID = paste("np", df$np, ",",
                           "nd", df$ndir_per_pid, ",",
                           "nf", df$nfile_per_dir, ",",
                           "nw", df$nwrites_per_file, "\n",
                           "ws", df$wsize, ",",
                           "wst", df$wstride, ",",
                           "so", df$startoff,
                           sep="")
    df$Workload_ID = factor(df$Workload_ID)

    # pick free spaces
    nFree_Space_Dist_ID = 6 
    distids = unique(df$Free_Space_Dist_ID)
    print(distids)
    picked_distids = head(distids, n=nFree_Space_Dist_ID)
    df = subset(df, Free_Space_Dist_ID %in% picked_distids)

    # pick workloads
    nWorkload_ID = 20 
    wlids = unique(df$Workload_ID)
    print(wlids)
    picked_wlids = head(wlids, n=nWorkload_ID)
    df = subset(df, Workload_ID %in% picked_wlids)


    df$Fragment_Block_Count = df$end - df$start + 1
    df$Fragment_Log2Size = log(df$Fragment_Block_Count*4096, 2)
    
    print(head(df))
    
    p = ggplot(df, aes(
                       x=Fragment_Log2Size,
                       #x=Fragment_Block_Count, 
                       color=monitor_time
                       )) +
        geom_freqpoly(binwidth=1)+
        #geom_histogram(aes(fill=monitor_time), position='dodge') +
        #geom_density()+
        facet_grid(Free_Space_Dist_ID~Workload_ID)+
        scale_x_continuous(breaks=seq(12, 28, by=2),
                           labels=(2^seq(12, 28, by=2)/1024))+
        xlab("Fragment Size(KB)")+
        theme(axis.text.x=element_text(angle=45,hjust=1))+
        scale_y_log10()
    print(p)
}

fdist_meta_data_blocks <- function(df)
{

    # pick monitors
    pickedMons = pick_by_stride(unique(df$monitor_time), stride=1)
    df = subset(df, monitor_time %in% pickedMons)

    df$Free_Space_Dist_ID = paste("alpha:", df$alpha, ",", "beta:", df$beta, sep="")
    df$Free_Space.Dist_ID = factor(df$Free_Space_Dist_ID)
    df$Workload_ID = paste("np", df$np, ",",
                           "nd", df$ndir_per_pid, ",",
                           "nf", df$nfile_per_dir, ",",
                           "nw", df$nwrites_per_file, "\n",
                           "ws", df$wsize, ",",
                           "wst", df$wstride, ",",
                           "so", df$startoff,
                           sep="")
    df$Workload_ID = factor(df$Workload_ID)


    p = ggplot(df, aes(
                       x=monitor_time,
                       y=fs_nmetablocks,
                       #y=fs_ndatablocks,
                       fill=monitor_time
                       )) +
        #geom_histogram(position='dodge') +
        geom_bar(stat="identity", position='dodge')+
        geom_text(aes(label=fs_nmetablocks, y=fs_nmetablocks), 
                    position=position_dodge(width=1),
                  size=4, angle=90)+
        facet_grid(Free_Space_Dist_ID~Workload_ID)+
        theme(axis.text.x=element_text(angle=45,hjust=1))+
        #theme(strip.text.x=element_text(size=5))+
        xlab("Time")
    print("after ggplot")
    print(p)
}

df_filter <- function(df, p_np, p_nd, p_nf, p_nw, p_ws, p_wst, p_so, p_alpha, p_beta, p_monitor_time) 
{
    df = subset(df, 
                    monitor_time %in% p_monitor_time &
                    alpha == p_alpha & 
                    beta == p_beta &
                    np == p_np &
                    ndir_per_pid == p_nd &
                    nfile_per_dir == p_nf &
                    nwrites_per_file == p_nw &
                    wsize == p_ws &
                    wstride %in% p_wst & 
                    startoff == p_so 
                    )
    return (df)
}

# for results with extlist
plot_physical_blocks_single_file_view <- function(df)
{
    #tms = unique(as.character(df$monitor_time))
    #pickedtms = head(tms, n=nseasons)
    #df = subset(df, monitor_time %in% pickedtms)# & wsize==1 & wstride==4096 )
   
    # we need only the leaf extents, not the internal node
    df = subset(df, Max_level == Level_index)

    df$rowid = row.names(df)
    rowsize = 1000
    df = ddply(df, .(rowid), ddply_trans_wide, rowsize=rowsize)
    df$y = df$y *rowsize

    df$filepath = factor( df$filepath, levels=sample(levels(df$filepath)) )

    print(df)

    # clearer annotation
    df$wstride = factor(df$wstride)
    df$wsize = factor(df$wsize)
    levels(df$wstride) = paste("Stride:", levels(df$wstride))
    levels(df$wsize) = paste("Size:", levels(df$wsize))
    p <- ggplot(df, aes()) +
        geom_segment(aes(x=x.start, xend=x.end+1,
                         y=y, yend=y,
                         color=monitor_time), size=1)+
        geom_text(aes(x=x.start, y=y, label=Level_index), size=2)+
        geom_text(aes(x=x.end, y=y, label=Length), size=4)+
        facet_grid(filepath~wstride) +
        theme( axis.text.x = element_blank() ) +
        xlab(paste("x range:", rowsize, sep="")) +
        ylab("block number") +
        ylim(c(0, 1.1e6)) +
        xlim(c(0, rowsize)) 

    print (p)
}


# for results with extlist
plot_physical_blocks <- function(df, nseasons)
{
    tms = unique(as.character(df$monitor_time))
    pickedtms = head(tms, n=nseasons)
    
    df = subset(df, monitor_time %in% pickedtms)# & wsize==1 & wstride==4096 )
    
    df$rowid = row.names(df)
    rowsize = 1000
    df = ddply(df, .(rowid), ddply_trans_wide, rowsize=rowsize)
    df$y = df$y *rowsize

    df$filepath = factor( df$filepath, levels=sample(levels(df$filepath)) )

    print(df)

    # clearer annotation
    df$wstride = factor(df$wstride)
    df$wsize = factor(df$wsize)
    levels(df$wstride) = paste("Stride:", levels(df$wstride))
    levels(df$wsize) = paste("Size:", levels(df$wsize))
    p <- ggplot(df, aes()) +
        geom_segment(aes(x=x.start, xend=x.end+1,
                         y=y, yend=y,
                         color=filepath), size=1)+
        facet_grid(wstride~wsize) +
        theme( axis.text.x = element_blank() ) +
        xlab(paste("x range:", rowsize, sep="")) +
        ylab("block number") +
        ylim(c(0, 1.1e6)) +
        xlim(c(0, rowsize))

    print (p)
}

# The input has to be only one row
# It splits a long segment to multiple
# smaller segments.
ddply_trans_wide <- function(df, rowsize=10000)
{
    start.row.y = floor(df$Physical_start/rowsize)
    start.row.x = df$Physical_start%%rowsize
    
    end.row.y = floor(df$Physical_end/rowsize)
    end.row.x = df$Physical_end%%rowsize
    
    if (end.row.y == start.row.y) {
        df$x.start = start.row.x
        df$x.end = end.row.x

        df$y = start.row.y
        return (df)
    } else {
        tp = head(df, n=1) 
        tp$x.start = 0
        tp$x.end = 0
        tp$y = 0
        
        # for start row
        r.start = tp
        r.start$x.start = start.row.x
        r.start$x.end = rowsize 
        r.start$y = start.row.y
        
        # for end row
        r.end = tp
        r.end$x.start = 0
        r.end$x.end = end.row.x
        r.end$y = end.row.y

        df.ret = rbind(r.start, r.end)
        if ( end.row.y > start.row.y + 1 ) {
            # there's line(s) in the middle
            for ( i in (start.row.y+1):(end.row.y-1)){
                r.mid = tp
                r.mid$x.start = 0
                r.mid$x.end = rowsize
                r.mid$y = i

                df.ret = rbind(df.ret, r.mid)
            }
        }
        return(df.ret)
    }

}

# the input df should be 
lookat_one_file <- function()
{
    dfglb <<- df_filter(list.h7check01[['_extlist']], 
            p_np=4, p_nd=4, p_nf=4, p_nw=1024, 
            p_ws=4096, p_wst=4096, p_so=0, p_alpha=2, p_beta=5,
            p_monitor_time='year00001.season00000') 

    print(c("max level summary", summary(dfglb$Max_level)))

    plot_physical_blocks(dfglb, 100)
    return()
    
    tmp = subset(dfglb, Max_level > 0)
    print(nrow(tmp))
    plot_physical_blocks(tmp, 1)
    return ()


    firstfile = tmp$filepath[1]
    fileext = subset(dfglb, filepath == firstfile)
    print(fileext[,2:11])
    #plot_physical_blocks(fileext, 1)
}

lookat_one_file_A <- function()
{
    dfglb <<- df_filter(list.h7check01[['_extlist']], 
            p_np=4, p_nd=4, p_nf=4, p_nw=1024, 
            p_ws=4096, p_wst=4096, p_so=0, p_alpha=2, p_beta=5,
            p_monitor_time='year00999.season00999') 
    print(c('nrow:', nrow(dfglb)))
    print(head(dfglb))

    print(c("Max_level summary\n",summary(dfglb$Max_level)))
    #return(NULL)

    #selectedfiles = sample(unique(as.character(dfglb$filepath)), 100)
    #plot_physical_blocks(subset(dfglb, filepath %in% selectedfiles), 1)
    plot_physical_blocks(dfglb, 1)
    return()
    
    tmp = subset(dfglb, Max_level > 0)
    print(nrow(tmp))
    filepaths = unique(as.character(tmp$filepath))
    print(c("number of files has > 0 levels:", length(filepaths)))
    #selectedfiles = sample(filepaths, 100)
    #plot_physical_blocks(subset(tmp, filepath %in% selectedfiles), 1)
    plot_physical_blocks(tmp, 1)
    print("i am new")
    return ()


    samplefile = sample(tmp$filepath, 1)
    print(samplefile)
    fileext = subset(dfglb, filepath == samplefile)
    print(summary(dfglb$filepath))
    print(fileext[,2:11])
    plot_physical_blocks(fileext, 1)
}



lookat_one_file_B <- function()
{
    dfglb <<- df_filter(list.h7check01[['_extlist']], 
            p_np=8, p_nd=8, p_nf=16, p_nw=4096, 
            p_ws=64, p_wst=64, p_so=0, p_alpha=2, p_beta=5,
            p_monitor_time='year00001.season00000') 

    print(c("Max_level summary\n",summary(dfglb$Max_level)))

    selectedfiles = sample(unique(as.character(dfglb$filepath)), 100)
    plot_physical_blocks(subset(dfglb, filepath %in% selectedfiles), 1)
    #plot_physical_blocks(dfglb, 1)
    return()
    
    tmp = subset(dfglb, Max_level > 0)
    print(nrow(tmp))
    selectedfiles = sample(unique(as.character(tmp$filepath)), 100)
    plot_physical_blocks(subset(tmp, filepath %in% selectedfiles), 1)
    #plot_physical_blocks(tmp, 1)
    print("i am new")
    return ()


    samplefile = sample(tmp$filepath, 1)
    print(samplefile)
    fileext = subset(dfglb, filepath == samplefile)
    print(summary(dfglb$filepath))
    print(fileext[,2:11])
    plot_physical_blocks(fileext, 1)
}

lookat_one_file_C <- function()
{
    dfglb <<- df_filter(list.h7check01[['_extlist']], 
            p_np=8, p_nd=8, p_nf=16, p_nw=1024, 
            p_ws=256, p_wst=256, p_so=0, p_alpha=2, p_beta=5,
            p_monitor_time='year00001.season00000') 

    print(c("Max_level summary\n",summary(dfglb$Max_level)))

    #selectedfiles = sample(unique(as.character(dfglb$filepath)), 100)
    #plot_physical_blocks(subset(dfglb, filepath %in% selectedfiles), 1)
    ##plot_physical_blocks(dfglb, 1)
    #return()
    
    #tmp = subset(dfglb, Max_level > 0)
    #print(nrow(tmp))
    #selectedfiles = sample(unique(as.character(tmp$filepath)), 100)
    #plot_physical_blocks(subset(tmp, filepath %in% selectedfiles), 1)
    ##plot_physical_blocks(tmp, 1)
    #print("i am new")
    #return ()


    samplefile = sample(dfglb$filepath, 1)
    print(samplefile)
    fileext = subset(dfglb, filepath == samplefile)
    print(summary(dfglb$filepath))
    print(fileext[,2:11])
    plot_physical_blocks(fileext, 1)
}

lookat_one_file_D <- function()
{
    dfglb <<- df_filter(list.h7check01[['_extlist']], 
            p_np=8, p_nd=8, p_nf=16, p_nw=1024, 
            p_ws=256, p_wst=512, p_so=0, p_alpha=2, p_beta=5,
            p_monitor_time='year00001.season00000') 

    print(c("Max_level summary\n",summary(dfglb$Max_level)))

    #selectedfiles = sample(unique(as.character(dfglb$filepath)), 100)
    #plot_physical_blocks(subset(dfglb, filepath %in% selectedfiles), 1)
    ##plot_physical_blocks(dfglb, 1)
    #return()
    
    tmp = subset(dfglb, Max_level > 0)
    #print(nrow(tmp))
    #filepaths = unique(as.character(tmp$filepath))
    #print(c("number of files has > 0 levels:", length(filepaths)))
    #selectedfiles = sample(filepaths, 100)
    #plot_physical_blocks(subset(tmp, filepath %in% selectedfiles), 1)
    ##plot_physical_blocks(tmp, 1)
    #print("i am new")
    #return ()


    samplefile = sample(tmp$filepath, 1)
    print(samplefile)
    fileext = subset(dfglb, filepath == samplefile)
    print(summary(dfglb$filepath))
    print(fileext[,2:11])
    plot_physical_blocks(fileext, 1)
}

lookat_one_file_E <- function()
{
    monitors = levels(list.h7check01[['_extlist']]$monitor_time)
    dfglb <<- df_filter(list.h7check01[['_extlist']], 
            p_np=8, p_nd=4, p_nf=16, p_nw=4096, 
            p_ws=128, p_wst=256, p_so=0, p_alpha=5, p_beta=5,
            p_monitor_time=monitors) 

    print(c("Max_level summary\n",summary(dfglb$Max_level)))

    # look at histogram of length
    df = subset(dfglb, Level_index == Max_level)
    p <- ggplot(df, aes(x=Length, color=monitor_time))+
        geom_freqpoly(binwidth=4)+
        #geom_text(stat='bin', binwidth=1, aes(y=..count.., label=..count..), 
                    #position=position_dodge(width=50)) +
        scale_x_continuous(breaks=seq(0, 200, by=4))+
        xlab("Number of continuous blocks")+
        theme(axis.text.x=element_text(angle=45,hjust=1))+
        scale_y_log10()
    print(p)
    return(NULL)


    selectedfiles = sample(unique(as.character(dfglb$filepath)), 100)
    plot_physical_blocks(subset(dfglb, filepath %in% selectedfiles), 1)
    #plot_physical_blocks(dfglb, 1)
    return()
    
    tmp = subset(dfglb, Max_level > 0)
    #print(nrow(tmp))
    #filepaths = unique(as.character(tmp$filepath))
    #print(c("number of files has > 0 levels:", length(filepaths)))
    #selectedfiles = sample(filepaths, 100)
    #plot_physical_blocks(subset(tmp, filepath %in% selectedfiles), 1)
    ##plot_physical_blocks(tmp, 1)
    #print("i am new")
    #return ()

    samplefile = sample(tmp$filepath, 1)
    print(samplefile)
    fileext = subset(dfglb, filepath == samplefile)
    print(summary(dfglb$filepath))
    print(fileext[,2:11])
    plot_physical_blocks(fileext, 1)
}

plot_physical_start_hist <- function(df)
{
    p <- ggplot(df, aes(x=Physical_start, 
                        color=monitor_time, 
                        fill=monitor_time
                        ))+
        #geom_freqpoly(binwidth=128*1024*1024/4095)+
        geom_density(alpha=1/5)+
        facet_grid(monitor_time~wstride)+
        theme(axis.text.x=element_text(angle=45,hjust=1))
    print(p)
}

plot_ext_length_hist <- function(df)
{
    # look at histogram of length
    df = subset(dfglb, Level_index == Max_level)
    maxlen = max(df$Length)
    p <- ggplot(df, aes(x=Length, color=monitor_time, fill=monitor_time))+
        #geom_freqpoly()+
        geom_histogram(binwidth=1)+
        #geom_freqpoly(binwidth=1)+
        #geom_text(stat='bin', binwidth=1, aes(y=..count.., label=..count..), 
                    #position=position_dodge(width=20)) +
        #geom_text(stat='bin', binwidth=1, aes(y=..count..), 
                    #position=position_dodge(width=20)) +
        #scale_x_continuous(breaks=seq(0, maxlen*1.5, length.out=20))+
        scale_x_continuous(limits=c(0,500))+
        xlab("Number of continuous blocks..")+
        facet_grid(monitor_time~wstride)+ 
        theme(axis.text.x=element_text(angle=45,hjust=1))
        #scale_y_log10(breaks=2^seq(1,14))
    print(p)
}

plot_hole <- function(df) 
{
    df = subset(df, Level_index == Max_level)
    df = arrange(df, Physical_start)
    n = nrow(df)
    previous_end = c( df$Physical_start[1], df$Physical_end[-n]+1 )
    df$hole_size = df$Physical_start - previous_end 

    #p <- ggplot(df, aes(x=Physical_start, y=hole_size)) +
        #geom_point()
    print(summary(df$hole_size))
    p <- ggplot(df, aes(x=hole_size))+
        #geom_density()+
        geom_histogram(binwidth=1)+
        facet_grid(monitor_time~wstride)+
        scale_x_continuous(limits=c(0,300), breaks=0:10)
    print(p)
}


lookat_one_file_caseb <- function()
{
    #monitors = levels(list.h7check01[['_extlist']]$monitor_time)
    #dfglb <<- df_filter(list.h7check01[['_extlist']], 
            #p_np=4, p_nd=4, p_nf=4, p_nw=4096, 
            #p_ws=1024, p_wst=c(2048), p_so=0, p_alpha=2, p_beta=5,
            #p_monitor_time=c("year00999.season00999")
                             ##"year00002.season00000")
            ##p_monitor_time=monitors
            #) 
    #print(head(dfglb))
    #return()

    #df = subset(dfglb, Length==1 & 'file' %in% filepath)
    #print(df$filepath)
    #return()

    #print(c("Max_level summary\n",summary(dfglb$Max_level)))

    #dfglb = subset(dfglb, Physical_start < 250000)      
    #plot_hole(dfglb)
    #return()

    plot_ext_length_hist(dfglb)
    return()
    #dfglb = subset(dfglb, Length <= 24 & Length > 8)
    #plot_physical_start_hist(dfglb)
    #return()
    #df.ETB = subset(dfglb, Max_level > 0)
    #print(summary(df.ETB))

    # Select interesting files to see
    #selectedfiles = sample(unique(as.character(dfglb$filepath)), 4)
    #selectedfiles = sort(unique(as.character(dfglb$filepath)))[1:5]
    #selectedfiles = c(
                      #"./season001/pid0.dir0/0.0.file",
                      #"./season001/pid0.dir0/0.1.file",
                      #"./season001/pid0.dir0/0.2.file",
                      #"./season001/pid0.dir0/0.3.file"
                      #)
    selectedfiles = unique(as.character(df.ETB$filepath)) [1:4]
    
    #selectedfiles = paste("./season002/pid0.dir3/0.", 1:8, ".file", sep="")
    #selectedfiles = "./season004/pid0.dir36/0.13.file"
    print (selectedfiles)
    tmpdf = subset(dfglb, filepath %in% selectedfiles)
    #print (tmpdf)
    #plot_physical_blocks(tmpdf, 1)
    plot_physical_blocks_single_file_view(tmpdf)
    #plot_physical_blocks(dfglb, 1)

    return()
    
    tmp = subset(dfglb, Max_level > 0)
    #print(nrow(tmp))
    #filepaths = unique(as.character(tmp$filepath))
    #print(c("number of files has > 0 levels:", length(filepaths)))
    #selectedfiles = sample(filepaths, 100)
    #plot_physical_blocks(subset(tmp, filepath %in% selectedfiles), 1)
    ##plot_physical_blocks(tmp, 1)
    #print("i am new")
    #return ()

    samplefile = sample(tmp$filepath, 1)
    print(samplefile)
    fileext = subset(dfglb, filepath == samplefile)
    print(summary(dfglb$filepath))
    print(fileext[,2:11])
    plot_physical_blocks(fileext, 1)
}


test002_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test002/")


    calculate_hole_per_file <- function(df)
    {
        df = arrange(df, Logical_start)
        df$ext_id = 0:(nrow(df)-1)
        df = arrange(df, Physical_start)
        n = nrow(df)
        df$Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = df$Next_phy_start - (df$Physical_end + 1)
        df$time_and_file = paste(df$monitor_time, df$filepath)
        df$file_ext_id = paste( df$filepath, df$ext_id, sep=":") 
        df$info = paste(df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        return (df)
    }

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)

        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)
        print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries <= df$Physical_end)  ]
        print(groupboundaries)
        
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color='blue', size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=time_and_file), size=5)+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=Physical_start, y=file_ext_id, 
                          label=info
                          #color=time_and_file
                          ), size=2)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )
}


test003_main <- function()
{
    lst <- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test003/")


    calculate_hole_per_file <- function(df)
    {
        df = arrange(df, Logical_start)
        df$ext_id = 0:(nrow(df)-1)
        df = arrange(df, Physical_start)
        n = nrow(df)
        df$Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = df$Next_phy_start - (df$Physical_end + 1)
        df$time_and_file = paste(df$monitor_time, df$filepath)
        df$file_ext_id = paste( df$filepath, df$ext_id, sep=":") 
        df$info = paste(df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        return (df)
    }

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)

        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)
        print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries <= df$Physical_end)  ]
        print(groupboundaries)
        .e = environment()
        
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color='blue', size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=time_and_file), size=5)+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=Physical_start, y=file_ext_id, 
                          label=info
                          #color=time_and_file
                          ), size=3)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )
}


test003_2_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test003_2/")

    calculate_hole_per_file <- function(df)
    {
        df = arrange(df, Logical_start)
        df$ext_id = 0:(nrow(df)-1)
        df = arrange(df, Physical_start)
        n = nrow(df)
        df$Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = df$Next_phy_start - (df$Physical_end + 1)
        df$time_and_file = paste(df$monitor_time, df$filepath)
        df$file_ext_id = paste( df$filepath, df$ext_id, sep=":") 
        df$info = paste(df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        return (df)
    }

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)

        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)
        print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries <= df$Physical_end)  ]
        print(groupboundaries)
        .e = environment()
        
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color="#56B4E9", size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=time_and_file), size=5)+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=Physical_start, y=file_ext_id, 
                          label=info
                          #color=time_and_file
                          ), size=3)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    calculate_holes <- function(df)
    {
        df = arrange(df, start)
        df$ext_id = 0:(nrow(df)-1)

        n = nrow(df)
        df$Next_start = c(df$start[-1], NA)
        df$Hole_after_me = df$Next_start - df$after_end 
        df$info = paste(df$start, df$after_end-df$start, df$Hole_after_me, sep=",")
        return (df)
    }

    merge_neighbors <- function(df)
    {
        print("merge_neighbors.........................")
        df = calculate_holes(df)
        df = arrange(df, start)
        n = nrow(df)
        newstart = df$start[1]
        newdf = NULL
        #print(df)
        for (i in 1:n) {
            #print (i)
            if (is.na(df$Hole_after_me[i]) || 
                df$Hole_after_me[i] > 0 || 
                i == n) {
                #print ("in if")
                newrow = data.frame(start=newstart, after_end=df$after_end[i])
                #print(newrow)
                newdf = rbind(newdf, newrow)

                if (i != n) {
                    newstart = df$start[i+1]
                }
            }            
        }
        newdf = calculate_holes(newdf)
        #print("olddf")
        #print(df[, c('start', 'end', 'Hole_after_me')])
        #print("newdf")
        #print(newdf)
        return(newdf)
    }

    plot_physical_free_blocks <- function(df, nseasons)
    {
        df$after_end = df$end+1 
        df = ddply(df, .(jobid, monitor_time), merge_neighbors)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries <= df$Physical_end)  ]
        print(groupboundaries)
        
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color="#56B4E9", size=0.1 )+
            geom_segment(aes(x=start, 
                             xend=after_end,
                             y=ext_id, 
                             yend=ext_id
                             ), size=3)+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=start, y=ext_id+2, 
                          label=info
                          #color=time_and_file
                          ), size=1)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }

    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.freeblocks )

}

test004_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test004/")

    calculate_hole_per_file <- function(df)
    {
        df = arrange(df, Logical_start)
        df$ext_id = 0:(nrow(df)-1)
        df = arrange(df, Physical_start)
        n = nrow(df)
        df$Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = df$Next_phy_start - (df$Physical_end + 1)
        df$time_and_file = paste(df$monitor_time, df$filepath)
        df$file_ext_id = paste( df$filepath, df$ext_id, sep=":") 
        df$info = paste(df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        return (df)
    }

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)

        df = subset(df, Physical_start > 540000) # choose only extents of files, not directory, this is a ugly hack

        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)
        print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        #groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        groupboundaries = groupboundaries[ groupboundaries >= min(df$Physical_start)  & groupboundaries <= max(df$Physical_end) ]
        print(groupboundaries)
        .e = environment()
        
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color="#56B4E9", size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=time_and_file), size=5)+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=Physical_start, y=file_ext_id, 
                          label=info
                          #color=time_and_file
                          ), size=3)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    calculate_holes <- function(df)
    {
        df = arrange(df, start)
        df$ext_id = 0:(nrow(df)-1)

        n = nrow(df)
        df$Next_start = c(df$start[-1], NA)
        df$Hole_after_me = df$Next_start - df$after_end 
        df$info = paste(df$start, df$after_end-df$start, df$Hole_after_me, sep=",")
        return (df)
    }

    merge_neighbors <- function(df)
    {
        print("merge_neighbors.........................")
        df = calculate_holes(df)
        df = arrange(df, start)
        n = nrow(df)
        newstart = df$start[1]
        newdf = NULL
        #print(df)
        for (i in 1:n) {
            #print (i)
            if (is.na(df$Hole_after_me[i]) || 
                df$Hole_after_me[i] > 0 || 
                i == n) {
                #print ("in if")
                newrow = data.frame(start=newstart, after_end=df$after_end[i])
                #print(newrow)
                newdf = rbind(newdf, newrow)

                if (i != n) {
                    newstart = df$start[i+1]
                }
            }            
        }
        newdf = calculate_holes(newdf)
        #print("olddf")
        #print(df[, c('start', 'end', 'Hole_after_me')])
        #print("newdf") 
        #print(newdf)
        return(newdf)
    }

    plot_physical_free_blocks <- function(df, nseasons)
    {
        df$after_end = df$end+1 
        df = ddply(df, .(jobid, monitor_time), merge_neighbors)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries <= df$Physical_end)  ]
        print(groupboundaries)
        
        print(df)
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color="#56B4E9", size=0.1 )+
            geom_segment(aes(x=start, 
                             xend=after_end,
                             y=ext_id, 
                             yend=ext_id
                             ), size=4, color='gray')+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=start, y=ext_id, 
                          label=info,
                          color='red'
                          ), size=2)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")+
            theme_bw()
        print (p)
    }

    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    windows()
    d.freeblocks = lst[['_freeblocks']]
    plot_physical_free_blocks( d.freeblocks )

}

test004_3_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test004_3/")

    calculate_hole_per_file <- function(df)
    {
        df = arrange(df, Logical_start)
        df$ext_id = 0:(nrow(df)-1)
        df = arrange(df, Physical_start)
        n = nrow(df)
        df$Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = df$Next_phy_start - (df$Physical_end + 1)
        df$time_and_file = paste(df$monitor_time, df$filepath)
        df$file_ext_id = paste( df$filepath, df$ext_id, sep=":") 
        df$info = paste(df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        return (df)
    }

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)

        df = subset(df, Physical_start > 540000) # choose only extents of files, not directory, this is a ugly hack

        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)
        print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        #groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        groupboundaries = groupboundaries[ groupboundaries >= min(df$Physical_start)  & groupboundaries <= max(df$Physical_end) ]
        print(groupboundaries)
        .e = environment()
        
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color="#56B4E9", size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=time_and_file), size=5)+
            facet_grid(monitor_time~., ) +
            #geom_text(aes(x=Physical_start, y=file_ext_id, 
                          #label=info
                          ##color=time_and_file
                          #), size=3)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    calculate_holes <- function(df)
    {
        df = arrange(df, start)
        df$ext_id = 0:(nrow(df)-1)

        n = nrow(df)
        df$Next_start = c(df$start[-1], NA)
        df$Hole_after_me = df$Next_start - df$after_end 
        df$info = paste(df$start, df$after_end-df$start, df$Hole_after_me, sep=",")
        return (df)
    }

    merge_neighbors <- function(df)
    {
        print("merge_neighbors.........................")
        df = calculate_holes(df)
        df = arrange(df, start)
        n = nrow(df)
        newstart = df$start[1]
        newdf = NULL
        #print(df)
        for (i in 1:n) {
            #print (i)
            if (is.na(df$Hole_after_me[i]) || 
                df$Hole_after_me[i] > 0 || 
                i == n) {
                #print ("in if")
                newrow = data.frame(start=newstart, after_end=df$after_end[i])
                #print(newrow)
                newdf = rbind(newdf, newrow)

                if (i != n) {
                    newstart = df$start[i+1]
                }
            }            
        }
        newdf = calculate_holes(newdf)
        #print("olddf")
        #print(df[, c('start', 'end', 'Hole_after_me')])
        #print("newdf") 
        #print(newdf)
        return(newdf)
    }

    plot_physical_free_blocks <- function(df, nseasons)
    {
        df$after_end = df$end+1 
        df = ddply(df, .(jobid, monitor_time), merge_neighbors)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries <= df$Physical_end)  ]
        print(groupboundaries)
        
        print(df)
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color="#56B4E9", size=0.1 )+
            geom_segment(aes(x=start, 
                             xend=after_end,
                             y=ext_id, 
                             yend=ext_id
                             ), size=4, color='gray')+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=start, y=ext_id, 
                          label=info,
                          color='red'
                          ), size=2)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")+
            theme_bw()
        print (p)
    }

    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    windows()
    d.freeblocks = lst[['_freeblocks']]
    plot_physical_free_blocks( d.freeblocks )

}

test005_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test005/")

    calculate_hole_per_file <- function(df)
    {
        df = arrange(df, Logical_start)
        df$ext_id = 0:(nrow(df)-1)
        df = arrange(df, Physical_start)
        n = nrow(df)
        df$Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = df$Next_phy_start - (df$Physical_end + 1)
        df$time_and_file = paste(df$monitor_time, df$filepath)
        df$file_ext_id = paste( df$filepath, df$ext_id, sep=":") 
        df$info = paste(df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        return (df)
    }

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)
        #df = subset(df, monitor_time %in% c("year00002.season00001", "year00003.season00001"))
        df = subset(df, monitor_time %in% c("year00003.season00001"))

        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        #df = subset(df, ext_id <16) # choose only extents of files, not directory, this is a ugly hack
        #df = subset(df, Physical_start < 1e5 & Physical_start > 35600) # choose only extents of files, not directory, this is a ugly hack
        print(df[1:20,c('Physical_start', 'Length', 'Hole_after_me', 'monitor_time', 'filepath')])
        return()

        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        #groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        groupboundaries = groupboundaries[ groupboundaries >= min(df$Physical_start)  & groupboundaries <= max(df$Physical_end) ]
        print(groupboundaries)
        .e = environment()
        
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=Physical_start, y=file_ext_id, 
                          label=info
                          #color=time_and_file
                          ), size=3)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    calculate_holes <- function(df)
    {
        df = arrange(df, start)
        df$ext_id = 0:(nrow(df)-1)

        n = nrow(df)
        df$Next_start = c(df$start[-1], NA)
        df$Hole_after_me = df$Next_start - df$after_end 
        df$info = paste(df$start, df$after_end-df$start, df$Hole_after_me, sep=",")
        return (df)
    }

    merge_neighbors <- function(df)
    {
        print("merge_neighbors.........................")
        df = calculate_holes(df)
        df = arrange(df, start)
        n = nrow(df)
        newstart = df$start[1]
        newdf = NULL
        #print(df)
        for (i in 1:n) {
            #print (i)
            if (is.na(df$Hole_after_me[i]) || 
                df$Hole_after_me[i] > 0 || 
                i == n) {
                #print ("in if")
                newrow = data.frame(start=newstart, after_end=df$after_end[i])
                #print(newrow)
                newdf = rbind(newdf, newrow)

                if (i != n) {
                    newstart = df$start[i+1]
                }
            }            
        }
        newdf = calculate_holes(newdf)
        #print("olddf")
        #print(df[, c('start', 'end', 'Hole_after_me')])
        #print("newdf") 
        #print(newdf)
        return(newdf)
    }

    plot_physical_free_blocks <- function(df, nseasons)
    {
        df$after_end = df$end+1 
        df = ddply(df, .(jobid, monitor_time), merge_neighbors)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries <= df$Physical_end)  ]
        print(groupboundaries)
        
        #print(df)
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color="#56B4E9", size=0.1 )+
            geom_segment(aes(x=start, 
                             xend=after_end,
                             y=ext_id, 
                             yend=ext_id
                             ), size=4, color='gray')+
            facet_grid(monitor_time~., ) +
            #geom_text(aes(x=start, y=ext_id, 
                          #label=info,
                          #color='red'
                          #), size=2)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")+
            theme_bw()
        print (p)
    }

    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.freeblocks )

}

test006_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test006/")

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
        df$file_ext_id = paste( df$filepath, df$ext_id, sep=":") 
        df$info = paste(df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        return (df)
    }

    regular_file_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('file', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }

    rename_years <- function(df)
    {
        levels(df$monitor_time) = sub('year00000', 'yr0.NoHole+NoFlush.',
                                      levels(df$monitor_time))
        levels(df$monitor_time) = sub('year00001', 'yr1.NoHole+Flush.',
                                      levels(df$monitor_time))
        levels(df$monitor_time) = sub('year00002', 'yr2.Hole+NoFlush',
                                      levels(df$monitor_time))
        levels(df$monitor_time) = sub('year00003', 'yr3.Hole+Flush',
                                      levels(df$monitor_time))
        return (df)
    }
    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)
        
        df = regular_file_subset(df)

        # rename the year
        df = rename_years(df)


        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        #print(df[1:20,c('Logical_start', 'Physical_start', 'Length', 'Hole_after_me', 'monitor_time', 'filepath')])

        #df = subset(df, Physical_start < 1e5)
        #print (df)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        #groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries >= min(df$Physical_start)  & groupboundaries <= max(df$Physical_end) ]
        groupboundaries = groupboundaries[ groupboundaries >= 33050  & groupboundaries <= 33090 ]
        print(groupboundaries)
        .e = environment()

        
        p <- ggplot(df, aes()) +
            #geom_vline(xintercept = groupboundaries, 
                       #color="gray", 
                       ##color="#56B4E9", 
                       #size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=ext_id, 
                             yend=ext_id,
                             color=filepath), size=5)+
            facet_grid(.~monitor_time, scales='free_x', space='free' ) +
            geom_text(aes(x=Physical_start+1e1, y=ext_id, 
                          label=info
                          #color=time_and_file
                          ), size=3)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    calculate_holes <- function(df)
    {
        df = arrange(df, start)
        df$ext_id = 0:(nrow(df)-1)

        n = nrow(df)
        df$Next_start = c(df$start[-1], NA)
        df$Hole_after_me = df$Next_start - df$after_end 
        df$info = paste(df$start, df$after_end-df$start, df$Hole_after_me, sep=",")
        return (df)
    }

    merge_neighbors <- function(df)
    {
        print("merge_neighbors.........................")
        df = calculate_holes(df)
        df = arrange(df, start)
        n = nrow(df)
        newstart = df$start[1]
        newdf = NULL
        #print(df)
        for (i in 1:n) {
            #print (i)
            if (is.na(df$Hole_after_me[i]) || 
                df$Hole_after_me[i] > 0 || 
                i == n) {
                #print ("in if")
                newrow = data.frame(start=newstart, after_end=df$after_end[i])
                #print(newrow)
                newdf = rbind(newdf, newrow)

                if (i != n) {
                    newstart = df$start[i+1]
                }
            }            
        }
        newdf = calculate_holes(newdf)
        #print("olddf")
        #print(df[, c('start', 'end', 'Hole_after_me')])
        #print("newdf") 
        #print(newdf)
        return(newdf)
    }

    plot_physical_free_blocks <- function(df, nseasons)
    {
        df$after_end = df$end+1 
        df = ddply(df, .(jobid, monitor_time), merge_neighbors)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries <= df$Physical_end)  ]
        print(groupboundaries)
        
        #print(df)
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color="#56B4E9", size=0.1 )+
            geom_segment(aes(x=start, 
                             xend=after_end,
                             y=ext_id, 
                             yend=ext_id
                             ), size=4, color='gray')+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=start, y=ext_id, 
                          label=info,
                          color='red'
                          ), size=2)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")+
            theme_bw()
        print (p)
    }

    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    windows()
    d.freeblocks = lst[['_freeblocks']]
    plot_physical_free_blocks( d.freeblocks )

}

test006a_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test006a/")

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
        df$info = paste(df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        return (df)
    }

    regular_file_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('file', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }

    rename_years <- function(df)
    {
        levels(df$monitor_time) = sub('year00000', 'yr0.NoHole+NoFlush.',
                                      levels(df$monitor_time))
        levels(df$monitor_time) = sub('year00001', 'yr1.NoHole+Flush.',
                                      levels(df$monitor_time))
        levels(df$monitor_time) = sub('year00002', 'yr2.Hole+NoFlush',
                                      levels(df$monitor_time))
        levels(df$monitor_time) = sub('year00003', 'yr3.Hole+Flush',
                                      levels(df$monitor_time))
        return (df)
    }
    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)
        
        df = regular_file_subset(df)


        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        #print(df[1:20,c('Logical_start', 'Physical_start', 'Length', 'Hole_after_me', 'monitor_time', 'filepath')])

        #df = subset(df, Physical_start < 1e5)
        #print (df)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries >= min(df$Physical_start)  & groupboundaries <= max(df$Physical_end) ]
        #groupboundaries = groupboundaries[ groupboundaries >= 33050  & groupboundaries <= 33090 ]
        print(groupboundaries)
        .e = environment()

        # rename the year
        df = rename_years(df)
        
        p <- ggplot(df, aes()) +
            #geom_vline(xintercept = groupboundaries, 
                       #color="gray", 
                       ##color="#56B4E9", 
                       #size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       #monitor_time~., 
                       .~monitor_time, 
                       scales='free', 
                       space='free' ) +
            geom_text(aes(x=Physical_start+1e1, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    calculate_holes <- function(df)
    {
        df = arrange(df, start)
        df$ext_id = 0:(nrow(df)-1)

        n = nrow(df)
        df$Next_start = c(df$start[-1], NA)
        df$Hole_after_me = df$Next_start - df$after_end 
        df$info = paste(df$start, df$after_end-df$start, df$Hole_after_me, sep=",")
        return (df)
    }

    merge_neighbors <- function(df)
    {
        print("merge_neighbors.........................")
        df = calculate_holes(df)
        df = arrange(df, start)
        n = nrow(df)
        newstart = df$start[1]
        newdf = NULL
        #print(df)
        for (i in 1:n) {
            #print (i)
            if (is.na(df$Hole_after_me[i]) || 
                df$Hole_after_me[i] > 0 || 
                i == n) {
                #print ("in if")
                newrow = data.frame(start=newstart, after_end=df$after_end[i])
                #print(newrow)
                newdf = rbind(newdf, newrow)

                if (i != n) {
                    newstart = df$start[i+1]
                }
            }            
        }
        newdf = calculate_holes(newdf)
        #print("olddf")
        #print(df[, c('start', 'end', 'Hole_after_me')])
        #print("newdf") 
        #print(newdf)
        return(newdf)
    }

    plot_physical_free_blocks <- function(df, nseasons)
    {
        df$after_end = df$end+1 
        df = ddply(df, .(jobid, monitor_time), merge_neighbors)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries <= df$Physical_end)  ]
        print(groupboundaries)
        
        #print(df)
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color="#56B4E9", size=0.1 )+
            geom_segment(aes(x=start, 
                             xend=after_end,
                             y=ext_id, 
                             yend=ext_id
                             ), size=4, color='gray')+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=start, y=ext_id, 
                          label=info,
                          color='red'
                          ), size=2)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")+
            theme_bw()
        print (p)
    }

    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}



test006b_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test006b/")

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
        df$info = paste(df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        return (df)
    }

    regular_file_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('file', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }

    rename_years <- function(df)
    {
        levels(df$monitor_time) = sub('year00000', 'yr0.NoHole+NoFlush.',
                                      levels(df$monitor_time))
        levels(df$monitor_time) = sub('year00001', 'yr1.NoHole+Flush.',
                                      levels(df$monitor_time))
        levels(df$monitor_time) = sub('year00002', 'yr2.Hole+NoFlush',
                                      levels(df$monitor_time))
        levels(df$monitor_time) = sub('year00003', 'yr3.Hole+Flush',
                                      levels(df$monitor_time))
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

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)
        
        df = regular_file_subset(df)


        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        #print(df[1:20,c('Logical_start', 'Physical_start', 'Length', 'Hole_after_me', 'monitor_time', 'filepath')])

        #df = subset(df, Physical_start < 1e5)
        #print (df)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries >= min(df$Physical_start)  & groupboundaries <= max(df$Physical_end) ]
        #groupboundaries = groupboundaries[ groupboundaries >= 33050  & groupboundaries <= 33090 ]
        print(groupboundaries)
        .e = environment()

        # rename the year
        df = rename_years(df)
        
        nicelimits = get_extended_xlimits(df)

        p <- ggplot(df, aes()) +
            #geom_vline(xintercept = groupboundaries, 
                       #color="gray", 
                       ##color="#56B4E9", 
                       #size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
                       #.~monitor_time, 
                       scales='free', 
                       space='free' ) +
            geom_text(aes(x=Physical_start+1e1, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=nicelimits)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}


test007_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test007/")

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
        df$info = paste(df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        return (df)
    }

    regular_file_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('file', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
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

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)
        
        df = regular_file_subset(df)


        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        #print(df[1:20,c('Logical_start', 'Physical_start', 'Length', 'Hole_after_me', 'monitor_time', 'filepath')])

        #df = subset(df, Physical_start < 1e5)
        #print (df)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        #groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries >= min(df$Physical_start)  & groupboundaries <= max(df$Physical_end) ]
        #groupboundaries = groupboundaries[ groupboundaries >= 33050  & groupboundaries <= 33090 ]
        print(groupboundaries)
        .e = environment()

        # rename the year
        #df = rename_years(df)

        # replace the monitor_time with meaningful write offest
        offsets = 2^(0:10)
        offsets = c(0, offsets)
        levels(df$monitor_time) = as.character(offsets) 
        groupboundaries = groupboundaries[ groupboundaries >= xmin  & groupboundaries <= xmax ]

        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       #monitor_time~., 
                       monitor_time~., 
                       scales='free', 
                       space='free' ) +
            geom_text(aes(x=Physical_start+1e1, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=c(xmin, xmax))+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}

test008_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test008/")

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
        df$info = paste(df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        return (df)
    }

    regular_file_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('file', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)
        
        df = regular_file_subset(df)


        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        #print(df[1:20,c('Logical_start', 'Physical_start', 'Length', 'Hole_after_me', 'monitor_time', 'filepath')])

        #df = subset(df, Physical_start < 1e5)
        #print (df)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        #groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries >= min(df$Physical_start)  & groupboundaries <= max(df$Physical_end) ]
        #groupboundaries = groupboundaries[ groupboundaries >= 33050  & groupboundaries <= 33090 ]
        print(groupboundaries)
        .e = environment()

        # rename the year
        #df = rename_years(df)

        xmin = min(df$Physical_start)
        xmax = max(df$Physical_end)
        xlen = xmax - xmin
        xadjust = xlen * 0.2
        xmin = xmin-xadjust
        xmax = xmax+xadjust
        
        # replace the monitor_time with meaningful write offest
        offsets = 16:35
        offsets = c(0, offsets)
        levels(df$monitor_time) = as.character(offsets) 
        groupboundaries = groupboundaries[ groupboundaries >= xmin  & groupboundaries <= xmax ]

        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       #monitor_time~., 
                       monitor_time~., 
                       scales='free', 
                       space='free' ) +
            geom_text(aes(x=Physical_start+1e1, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=c(xmin, xmax))+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}

test009_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test009/")

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
        df$info = paste(df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        return (df)
    }

    regular_file_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('file', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)
        
        df = regular_file_subset(df)


        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        #print(df[1:20,c('Logical_start', 'Physical_start', 'Length', 'Hole_after_me', 'monitor_time', 'filepath')])

        #df = subset(df, Physical_start < 1e5)
        #print (df)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        #groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries >= min(df$Physical_start)  & groupboundaries <= max(df$Physical_end) ]
        #groupboundaries = groupboundaries[ groupboundaries >= 33050  & groupboundaries <= 33090 ]
        print(groupboundaries)
        .e = environment()

        # rename the year
        #df = rename_years(df)

        xmin = min(df$Physical_start)
        xmax = max(df$Physical_end)
        xlen = xmax - xmin
        xadjust = xlen * 0.2
        xmin = xmin-xadjust
        xmax = xmax+xadjust
        
        # replace the monitor_time with meaningful write offest
        offsets = 0:10 
        offsets = 2^offsets
        offsets = c(0, offsets-1)
        levels(df$monitor_time) = as.character(offsets) 
        groupboundaries = groupboundaries[ groupboundaries >= xmin  & groupboundaries <= xmax ]

        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       #monitor_time~., 
                       monitor_time~., 
                       scales='free', 
                       space='free' ) +
            geom_text(aes(x=Physical_start+1e1, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=c(xmin, xmax))+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}





test007a_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test007a/")

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
        df$info = paste(df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        return (df)
    }

    regular_file_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('file', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)
        
        df = regular_file_subset(df)


        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        #print(df[1:20,c('Logical_start', 'Physical_start', 'Length', 'Hole_after_me', 'monitor_time', 'filepath')])

        #df = subset(df, Physical_start < 1e5)
        #print (df)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        #groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries >= min(df$Physical_start)  & groupboundaries <= max(df$Physical_end) ]
        #groupboundaries = groupboundaries[ groupboundaries >= 33050  & groupboundaries <= 33090 ]
        print(groupboundaries)
        .e = environment()

        # rename the year
        #df = rename_years(df)

        xmin = min(df$Physical_start)
        xmax = max(df$Physical_end)
        xlen = xmax - xmin
        xadjust = xlen * 0.2
        xmin = xmin-xadjust
        xmax = xmax+xadjust
        
        # replace the monitor_time with meaningful write offest
        offsets = rep(0:17, each=2)
        flushes = rep(0:1, 18)
        print (levels(df$monitor_time))
        levels(df$monitor_time) = as.character(paste(offsets, flushes, sep="\n")) 
        groupboundaries = groupboundaries[ groupboundaries >= xmin  & groupboundaries <= xmax ]

        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       #monitor_time~., 
                       monitor_time~., 
                       scales='free', 
                       space='free' ) +
            geom_text(aes(x=Physical_start+1e1, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=c(xmin, xmax))+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}

test007b_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test007b/")

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
        df$info = paste(df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        return (df)
    }

    regular_file_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('file', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)
        
        df = regular_file_subset(df)


        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        #print(df[1:20,c('Logical_start', 'Physical_start', 'Length', 'Hole_after_me', 'monitor_time', 'filepath')])

        #df = subset(df, Physical_start < 1e5)
        #print (df)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        #groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries >= min(df$Physical_start)  & groupboundaries <= max(df$Physical_end) ]
        #groupboundaries = groupboundaries[ groupboundaries >= 33050  & groupboundaries <= 33090 ]
        print(groupboundaries)
        .e = environment()

        # rename the year
        #df = rename_years(df)

        xmin = min(df$Physical_start)
        xmax = max(df$Physical_end)
        xlen = xmax - xmin
        xadjust = xlen * 0.2
        xmin = xmin-xadjust
        xmax = xmax+xadjust
        
        # replace the monitor_time with meaningful write offest
        #offsets = rep(0:17, each=2)
        #flushes = rep(0:1, 18)
        print (levels(df$monitor_time))
        levels(df$monitor_time) = c("write file0000 with fsync\nwrite file0001 without fsync", "write same file with fsync first \nand then without fsync") 
        groupboundaries = groupboundaries[ groupboundaries >= xmin  & groupboundaries <= xmax ]

        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       #monitor_time~., 
                       monitor_time~., 
                       scales='free', 
                       space='free' ) +
            geom_text(aes(x=Physical_start+1e1, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=c(xmin, xmax))+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}

test007c_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test007c/")

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
        df$info = paste(df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
        return (df)
    }

    regular_file_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('file', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)
        
        df = regular_file_subset(df)


        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)


        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)

        # pick one at a time
        #df = subset(df, monitor_time=="year00000.season00001")
        df = subset(df, monitor_time=="year00001.season00001")

        xmin = min(df$Physical_start)
        xmax = max(df$Physical_end)
        xlen = xmax - xmin
        xadjust = xlen * 0.2
        xmin = xmin-xadjust
        xmax = xmax+xadjust
        
        # replace the monitor_time with meaningful write offest
        #offsets = rep(0:17, each=2)
        #flushes = rep(0:1, 18)
        print (levels(df$monitor_time))
        #levels(df$monitor_time) = c("write file0000 with fsync\nwrite file0001 without fsync", "write same file with fsync first \nand then without fsync") 
        groupboundaries = groupboundaries[ groupboundaries >= xmin  & groupboundaries <= xmax ]

        p <- ggplot(df, aes()) +
            #geom_vline(xintercept = groupboundaries, 
                       #color="gray", 
                       ##color="#56B4E9", 
                       #size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
                       #.~monitor_time, 
                       scales='free', 
                       space='free' ) +
            geom_text(aes(x=Physical_start, y=file_ext_id, 
                          label=info
                          #color=filepath
                          ), size=3)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=c(xmin, xmax))+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}

test010_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test010/")

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
        df$info = paste(df$Level_index, df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
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

    user_file_dir_subset <- function (df)
    {
        # grep out only the files
        paths = unique(df$filepath)
        onlyregfiles = grep('pid', paths, value=T)
        df = subset(df, filepath %in% onlyregfiles)
        return (df)
    }

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        #df = subset(df, Max_level == Level_index)
        
        #df = regular_file_subset(df)
        df = user_file_dir_subset(df)


        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        #print(df[1:20,c('Logical_start', 'Physical_start', 'Length', 'Hole_after_me', 'monitor_time', 'filepath')])

        #df = subset(df, Physical_start < 1e5)
        #print (df)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        #groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries >= min(df$Physical_start)  & groupboundaries <= max(df$Physical_end) ]
        #groupboundaries = groupboundaries[ groupboundaries >= 33050  & groupboundaries <= 33090 ]
        print(groupboundaries)
        .e = environment()

        # replace the monitor_time with meaningful write offest
        strides = rep(2^(0:15), each=2)
        finalsync = rep(c('no\nSsync', 'Ssync'), 16)
        #offsets = 2^offsets
        #offsets = c(0, offsets-1)
        levels(df$monitor_time) = as.character(paste(strides, finalsync, sep='\n'))
        
        # name them so I can filter
        df$strides = df$monitor_time
        levels(df$strides) = as.character(strides)
        print("my strides")
        print(df$strides)
        print("my number strides")
        print(as.numeric(as.character(df$strides)))
        df$strides = as.numeric(as.character(df$strides))

        df$finalsync = df$monitor_time
        levels(df$finalsync) = finalsync

        # pick the ones I need
        df = subset(df, strides <= 8)
        #df = subset(df, strides > 8 & strides <= 512)
        #df = subset(df, strides > 512)

        nicelimits = get_extended_xlimits(df)

        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]
        df$monitor_time = factor(df$monitor_time)
        print (df[,c('info', 'monitor_time', 'Hole_after_me', 'strides')])
        #return()

        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            geom_text(aes(x=Physical_start+1e1, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=nicelimits, breaks=groupboundaries, labels=paste(groupboundaries, groupboundaries/32768, sep="\n"))+

            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}

# This is the one with inode block number
test010a_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test010a/")

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
        df$info = paste(df$Level_index, df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
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

    extract_year <- function( monitor_time ) {
        y = substr(as.character(monitor_time), 5,9)
        y = as.numeric(y)
    }

    extract_season <- function( monitor_time ) {
        s = substr(as.character(monitor_time), 17,21)
        s = as.numeric(s)
    }

    add_year_season <- function (df) {
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


    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        #df = subset(df, Max_level == Level_index)
        
        #df = regular_file_subset(df)

        df = user_file_dir_subset(df)

        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        #groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries >= min(df$Physical_start)  & groupboundaries <= max(df$Physical_end) ]
        #groupboundaries = groupboundaries[ groupboundaries >= 33050  & groupboundaries <= 33090 ]
        print(groupboundaries)

        df = add_year_season(df)

        
        strides = 2^(0:15)
        strides = rep(strides, each=4)
        syncs = rep(c('noflush','noflush','flush','flush'), 16)
        mytypes = paste(strides, syncs, sep='\n')
        print(mytypes)
        df$newtype = df$monitor_time
        levels(df$newtype) = mytypes
        print(df$newtype) 

        nicelimits = get_extended_xlimits(df)

        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]
        df$monitor_time = factor(df$monitor_time)
        #print (df[,c('info', 'monitor_time', 'Hole_after_me', 'strides')])
        #return()

        df = subset(df, monitor_season == 1)
        df = subset(df, monitor_year < 10)

        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       newtype~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            geom_text(aes(x=Physical_start+1e1, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=nicelimits, breaks=groupboundaries, labels=paste(groupboundaries, groupboundaries/32768, sep="\n"))+

            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}

test011_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test011/")

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
        df$info = paste(df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
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

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)
        #df = regular_file_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            geom_text(aes(x=Physical_start+1e1, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}


test011a_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test011a/")

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
        df$info = paste(df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
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

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)
        #df = regular_file_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            geom_text(aes(x=Physical_start+1e1, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}

test011b_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test011b/")

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
        df$info = paste(df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
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

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)
        #df = regular_file_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            geom_text(aes(x=Physical_start+1e1, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}


test011c_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test011c/")

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
        df$info = paste(df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
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

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)
        #df = regular_file_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            geom_text(aes(x=Physical_start+1e1, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}

test011d_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test011d/")

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
        df$info = paste(df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
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

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        df = subset(df, Max_level == Level_index)
        #df = regular_file_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            geom_text(aes(x=Physical_start+1e1, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}

test012_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test012/")

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
        df$info = paste(df$Level_index, df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
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

    # for results with extlist
    plot_physical_blocks <- function(df)
    {
        #df = subset(df, Max_level == Level_index)
        df = regular_file_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        p <- ggplot(df, aes()) +
            #geom_vline(xintercept = groupboundaries, 
                       #color="gray", 
                       ##color="#56B4E9", 
                       #size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
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
            scale_x_continuous(limits=nicelimits)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    

    plot_physical_blocks_flipable <- function(df, flip=F)
    {
        #df = subset(df, Max_level == Level_index)
        df = regular_file_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        p <- ggplot(df, aes()) +
            #geom_vline(xintercept = groupboundaries, 
                       #color="gray", 
                       ##color="#56B4E9", 
                       #size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) 

        if ( flip == T ) {
            p <- p +
                coord_flip()+
                #theme( axis.text.y = element_text(angle=45) ) +
                theme( axis.text.x = element_blank() ) 

        } else {
            p <- p + 
            geom_text(aes(x=Physical_start+13, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            #scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            scale_x_continuous(limits=nicelimits)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        }
        print (p)
    }


    merge_neighbors <- function(df)
    {
        print("merge_neighbors.........................")
        df = calculate_holes(df)
        df = arrange(df, start)
        n = nrow(df)
        newstart = df$start[1]
        newdf = NULL
        #print(df)
        for (i in 1:n) {
            #print (i)
            if (is.na(df$Hole_after_me[i]) || 
                df$Hole_after_me[i] > 0 || 
                i == n) {
                #print ("in if")
                newrow = data.frame(start=newstart, after_end=df$after_end[i])
                #print(newrow)
                newdf = rbind(newdf, newrow)

                if (i != n) {
                    newstart = df$start[i+1]
                }
            }            
        }
        newdf = calculate_holes(newdf)
        #print("olddf")
        #print(df[, c('start', 'end', 'Hole_after_me')])
        #print("newdf") 
        #print(newdf)
        return(newdf)
    }

    calculate_holes <- function(df)
    {
        df = arrange(df, start)
        df$ext_id = 0:(nrow(df)-1)

        n = nrow(df)
        df$Next_start = c(df$start[-1], NA)
        df$Hole_after_me = df$Next_start - df$after_end 
        df$info = paste(df$start, df$after_end-df$start, df$Hole_after_me, sep=",")
        return (df)
    }

    plot_physical_free_blocks <- function(df)
    {
        df$after_end = df$end+1 

        df = subset(df, monitor_time == "year00004.season00001")

        df = ddply(df, .(jobid, monitor_time), merge_neighbors)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries <= df$Physical_end)  ]
        print(groupboundaries)
        
        #print(df)
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color="#56B4E9", size=0.1 )+
            geom_segment(aes(x=start, 
                             xend=after_end,
                             y=ext_id, 
                             yend=ext_id
                             ), size=4, color='gray')+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=start, y=ext_id, 
                          label=info,
                          color='red'
                          ), size=2)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")+
            theme_bw()
        print (p)
    }

    d = lst[['_extlist']]
    print(head(d))
    #plot_physical_blocks( d )
    plot_physical_blocks_flipable( d, flip=T )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.freeblocks )

}

test012a_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test012a/")

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
        df$info = paste(df$Level_index, df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
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

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        #df = subset(df, Max_level == Level_index)
        df = regular_file_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        p <- ggplot(df, aes()) +
            #geom_vline(xintercept = groupboundaries, 
                       #color="gray", 
                       ##color="#56B4E9", 
                       #size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
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
            scale_x_continuous(limits=nicelimits)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    merge_neighbors <- function(df)
    {
        print("merge_neighbors.........................")
        df = calculate_holes(df)
        df = arrange(df, start)
        n = nrow(df)
        newstart = df$start[1]
        newdf = NULL
        #print(df)
        for (i in 1:n) {
            #print (i)
            if (is.na(df$Hole_after_me[i]) || 
                df$Hole_after_me[i] > 0 || 
                i == n) {
                #print ("in if")
                newrow = data.frame(start=newstart, after_end=df$after_end[i])
                #print(newrow)
                newdf = rbind(newdf, newrow)

                if (i != n) {
                    newstart = df$start[i+1]
                }
            }            
        }
        newdf = calculate_holes(newdf)
        #print("olddf")
        #print(df[, c('start', 'end', 'Hole_after_me')])
        #print("newdf") 
        #print(newdf)
        return(newdf)
    }

    calculate_holes <- function(df)
    {
        df = arrange(df, start)
        df$ext_id = 0:(nrow(df)-1)

        n = nrow(df)
        df$Next_start = c(df$start[-1], NA)
        df$Hole_after_me = df$Next_start - df$after_end 
        df$info = paste(df$start, df$after_end-df$start, df$Hole_after_me, sep=",")
        return (df)
    }

    plot_physical_free_blocks <- function(df)
    {
        df$after_end = df$end+1 

        df = subset(df, monitor_time == "year00004.season00001")

        df = ddply(df, .(jobid, monitor_time), merge_neighbors)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries <= df$Physical_end)  ]
        print(groupboundaries)
        
        #print(df)
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color="#56B4E9", size=0.1 )+
            geom_segment(aes(x=start, 
                             xend=after_end,
                             y=ext_id, 
                             yend=ext_id
                             ), size=4, color='gray')+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=start, y=ext_id, 
                          label=info,
                          color='red'
                          ), size=2)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")+
            theme_bw()
        print (p)
    }

    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    windows()
    d.freeblocks = lst[['_freeblocks']]
    plot_physical_free_blocks( d.freeblocks )

}


test002_analysis02_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test002/")

    # convert number to string and fill it with leading zeros
    zfill <- function (n, w)
    {
        return(formatC(n, width = w, format = "d", flag = "0"))
    }
    calculate_hole_per_file <- function(df)
    {
        df$Physical_length = df$Physical_end - df$Physical_start + 1

        df = arrange(df, Logical_start)
        df$ext_id = 0:(nrow(df)-1)
        df = arrange(df, Physical_start)
        n = nrow(df)
        df$Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = df$Next_phy_start - (df$Physical_end + 1)
        df$time_and_file = paste(df$monitor_time, df$filepath)
        df$file_ext_id = paste(df$filepath, zfill(df$ext_id, 5), sep=":") 
        df$info = paste(df$Level_index, df$Logical_start, df$Physical_start, df$Physical_length, df$Hole_after_me, sep=",")
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

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        #df = subset(df, Max_level == Level_index)
        #df = regular_file_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)
        nicelimits[1] = 0

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        p <- ggplot(df, aes()) +
            theme_bw()+
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            geom_text(aes(x=Physical_start+1e5, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            #scale_x_continuous(limits=nicelimits)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    merge_neighbors <- function(df)
    {
        print("merge_neighbors.........................")
        df = calculate_holes(df)
        df = arrange(df, start)
        n = nrow(df)
        newstart = df$start[1]
        newdf = NULL
        #print(df)
        for (i in 1:n) {
            #print (i)
            if (is.na(df$Hole_after_me[i]) || 
                df$Hole_after_me[i] > 0 || 
                i == n) {
                #print ("in if")
                newrow = data.frame(start=newstart, after_end=df$after_end[i])
                #print(newrow)
                newdf = rbind(newdf, newrow)

                if (i != n) {
                    newstart = df$start[i+1]
                }
            }            
        }
        newdf = calculate_holes(newdf)
        #print("olddf")
        #print(df[, c('start', 'end', 'Hole_after_me')])
        #print("newdf") 
        #print(newdf)
        return(newdf)
    }

    calculate_holes <- function(df)
    {
        df = arrange(df, start)
        df$ext_id = 0:(nrow(df)-1)

        n = nrow(df)
        df$Next_start = c(df$start[-1], NA)
        df$Hole_after_me = df$Next_start - df$after_end 
        df$info = paste(df$start, df$after_end-df$start, df$Hole_after_me, sep=",")
        return (df)
    }

    plot_physical_free_blocks <- function(df)
    {
        df$after_end = df$end+1 

        df = subset(df, monitor_time == "year00004.season00001")

        df = ddply(df, .(jobid, monitor_time), merge_neighbors)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries <= df$Physical_end)  ]
        print(groupboundaries)
        
        #print(df)
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color="#56B4E9", size=0.1 )+
            geom_segment(aes(x=start, 
                             xend=after_end,
                             y=ext_id, 
                             yend=ext_id
                             ), size=4, color='gray')+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=start, y=ext_id, 
                          label=info,
                          color='red'
                          ), size=2)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")+
            theme_bw()
        print (p)
    }

    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.freeblocks )

}


test003_2_analysis02_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test003_2/")

    # convert number to string and fill it with leading zeros
    zfill <- function (n, w)
    {
        return(formatC(n, width = w, format = "d", flag = "0"))
    }
    calculate_hole_per_file <- function(df)
    {
        df$Physical_length = df$Physical_end - df$Physical_start + 1

        df = arrange(df, Logical_start)
        df$ext_id = 0:(nrow(df)-1)
        df = arrange(df, Physical_start)
        n = nrow(df)
        df$Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = df$Next_phy_start - (df$Physical_end + 1)
        df$time_and_file = paste(df$monitor_time, df$filepath)
        df$file_ext_id = paste(df$filepath, zfill(df$ext_id, 5), sep=":") 
        df$info = paste(df$Level_index, df$Logical_start, df$Physical_start, df$Physical_length, df$Hole_after_me, sep=",")
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

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        #df = subset(df, Max_level == Level_index)
        #df = regular_file_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)
        nicelimits[1] = 0

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        p <- ggplot(df, aes()) +
            theme_bw()+
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            geom_text(aes(x=Physical_start+1e5, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            #scale_x_continuous(limits=nicelimits)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    merge_neighbors <- function(df)
    {
        print("merge_neighbors.........................")
        df = calculate_holes(df)
        df = arrange(df, start)
        n = nrow(df)
        newstart = df$start[1]
        newdf = NULL
        #print(df)
        for (i in 1:n) {
            #print (i)
            if (is.na(df$Hole_after_me[i]) || 
                df$Hole_after_me[i] > 0 || 
                i == n) {
                #print ("in if")
                newrow = data.frame(start=newstart, after_end=df$after_end[i])
                #print(newrow)
                newdf = rbind(newdf, newrow)

                if (i != n) {
                    newstart = df$start[i+1]
                }
            }            
        }
        newdf = calculate_holes(newdf)
        #print("olddf")
        #print(df[, c('start', 'end', 'Hole_after_me')])
        #print("newdf") 
        #print(newdf)
        return(newdf)
    }

    calculate_holes <- function(df)
    {
        df = arrange(df, start)
        df$ext_id = 0:(nrow(df)-1)

        n = nrow(df)
        df$Next_start = c(df$start[-1], NA)
        df$Hole_after_me = df$Next_start - df$after_end 
        df$info = paste(df$start, df$after_end-df$start, df$Hole_after_me, sep=",")
        return (df)
    }

    plot_physical_free_blocks <- function(df)
    {
        df$after_end = df$end+1 

        df = subset(df, monitor_time == "year00004.season00001")

        df = ddply(df, .(jobid, monitor_time), merge_neighbors)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries <= df$Physical_end)  ]
        print(groupboundaries)
        
        #print(df)
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color="#56B4E9", size=0.1 )+
            geom_segment(aes(x=start, 
                             xend=after_end,
                             y=ext_id, 
                             yend=ext_id
                             ), size=4, color='gray')+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=start, y=ext_id, 
                          label=info,
                          color='red'
                          ), size=2)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")+
            theme_bw()
        print (p)
    }

    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.freeblocks )

}


test004a_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test004a/")

    # convert number to string and fill it with leading zeros
    zfill <- function (n, w)
    {
        return(formatC(n, width = w, format = "d", flag = "0"))
    }
    calculate_hole_per_file <- function(df)
    {
        df$Physical_length = df$Physical_end - df$Physical_start + 1

        df = arrange(df, Logical_start)
        df$ext_id = 0:(nrow(df)-1)
        df = arrange(df, Physical_start)
        n = nrow(df)
        df$Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = df$Next_phy_start - (df$Physical_end + 1)
        df$time_and_file = paste(df$monitor_time, df$filepath)
        df$file_ext_id = paste(df$filepath, zfill(df$ext_id, 5), sep=":") 
        df$info = paste(df$Level_index, df$Logical_start, df$Physical_start, df$Physical_length, df$Hole_after_me, sep=",")
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

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        #df = subset(df, Max_level == Level_index)
        df = regular_file_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        p <- ggplot(df, aes()) +
            theme_bw()+
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            geom_text(aes(x=Physical_start+150, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            #scale_x_continuous(limits=nicelimits)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }

    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.freeblocks )

}


test004b_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test004b/")

    # convert number to string and fill it with leading zeros
    zfill <- function (n, w)
    {
        return(formatC(n, width = w, format = "d", flag = "0"))
    }
    calculate_hole_per_file <- function(df)
    {
        df$Physical_length = df$Physical_end - df$Physical_start + 1

        df = arrange(df, Logical_start)
        df$ext_id = 0:(nrow(df)-1)
        df = arrange(df, Physical_start)
        n = nrow(df)
        df$Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = df$Next_phy_start - (df$Physical_end + 1)
        df$time_and_file = paste(df$monitor_time, df$filepath)
        df$file_ext_id = paste(df$filepath, zfill(df$ext_id, 5), sep=":") 
        df$info = paste(df$Level_index, df$Logical_start, df$Physical_start, df$Physical_length, df$Hole_after_me, sep=",")
        #df$info = paste(df$Level_index, sep=",")
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

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        #df = subset(df, Max_level == Level_index)
        df = regular_file_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)
        #nicelimits[1] = 0

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        
        # Pick a subset to plot
        df = subset(df, monitor_time %in% c(
                                            "year00000.season00001",
                                            "year00001.season00001",
                                            #"year00002.season00001",
                                            #"year00003.season00001",
                                            #"year00004.season00001",
                                            #"year00005.season00001",
                                            ""
                                            ))
        levels(df$monitor_time) = revalue(levels(df$monitor_time), c(
                                                                     "year00000.season00001"="00:NoForcedSwitch\nNoFsync",
                                                                     "year00001.season00001"="01:NoForcedSwitch\nFsync",
                                                                     "year00002.season00001"="02:ForcedSwitch\nNoFsync",
                                                                     "year00003.season00001"="03:ForcedSwitch\nFsync",
                                                                     "year00004.season00001"="04:FixedCPU\nNoFsync",
                                                                     "year00005.season00001"="05:FixedCPU\nFsync"
                                                                     ))


        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath),
                        size=5)+
            facet_grid(
                       monitor_time~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            geom_text(aes(x=Physical_start+50, y=file_ext_id, 
                          label=info,
                          color=factor(Level_index)
                          ), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            #scale_x_continuous(limits=nicelimits)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
            theme_bw()
        print (p)
    }

    # for results with extlist
    plot_physical_blocks_metadata <- function(df, nseasons)
    {
        df = subset(df,  Max_level != Level_index)
        #df = regular_file_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)
        #nicelimits[1] = nicelimits[1] - 10
        #nicelimits[1] = nicelimits[1] - 2000
        #nicelimits[1] = 557000

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        
        # Pick a subset to plot
        df = subset(df, monitor_time %in% c(
                                            #"year00000.season00001",
                                            #"year00001.season00001",
                                            #"year00002.season00001",
                                            "year00003.season00001",
                                            #"year00004.season00001",
                                            #"year00005.season00001",
                                            ""
                                            ))
        levels(df$monitor_time) = revalue(levels(df$monitor_time), c(
                                                                     "year00000.season00001"="00:NoForcedSwitch\nNoFsync",
                                                                     "year00001.season00001"="01:NoForcedSwitch\nFsync",
                                                                     "year00002.season00001"="02:ForcedSwitch\nNoFsync",
                                                                     "year00003.season00001"="03:ForcedSwitch\nFsync",
                                                                     "year00004.season00001"="04:FixedCPU\nNoFsync",
                                                                     "year00005.season00001"="05:FixedCPU\nFsync"
                                                                     ))


        p <- ggplot(df, aes()) +
            #geom_vline(xintercept = groupboundaries, 
                       #color="gray", 
                       ##color="#56B4E9", 
                       #size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath),
                        size=5)+
            facet_grid(
                       monitor_time~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            geom_text(aes(x=Physical_start, y=file_ext_id, 
                          label=info,
                          color=factor(Level_index)
                          ), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            #scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            scale_x_continuous(limits=nicelimits)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
            theme_bw()
        print (p)
    }




    betterformat <- function(df)
    {
        #df = subset(df, Max_level == Level_index)
        #df = regular_file_subset(df)
        #df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        # Pick a subset to plot
        #df = subset(df, monitor_time %in% c(
                                            #"year00000.season00001",
                                            #"year00001.season00001",
                                            ##"year00002.season00001",
                                            ##"year00003.season00001",
                                            ##"year00004.season00001",
                                            ##"year00005.season00001",
                                            #""
                                            #))
        levels(df$monitor_time) = revalue(levels(df$monitor_time), c(
                                                                     "year00000.season00001"="00:NoForcedSwitch+NoFsync",
                                                                     "year00001.season00001"="01:NoForcedSwitch+Fsync",
                                                                     "year00002.season00001"="02:ForcedSwitch+NoFsync",
                                                                     "year00003.season00001"="03:ForcedSwitch+Fsync",
                                                                     "year00004.season00001"="04:FixedCPU+NoFsync",
                                                                     "year00005.season00001"="05:FixedCPU+Fsync"
                                                                     ))

        #columns = c(Level_index Max_level Entry_index N_Entry Logical_start Logical_end Physical_start Physical_end Length Flag filepath)
        #columns = c("Level_index","Max_level","Logical_start","Logical_end","Physical_start","Physical_end","filepath","monitor_time") 
        columns = c("Level_index","Max_level","Entry_index", "N_Entry", "Logical_start","Logical_end","Physical_start","Physical_end","filepath","monitor_time") 
        df = df[,columns]
        names(df)[which(names(df)=="monitor_time")]= "Operation_type"
        print(df)
        print(names(df))
        write.csv(df, file="cpuswitching.csv", row.names=F)
    }




    d = lst[['_extlist']]
    #print(head(d))
    plot_physical_blocks_metadata( d )
    #plot_physical_blocks( d )
    #betterformat(d)

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.freeblocks )

}



test005_analysis02_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test005/")

    # convert number to string and fill it with leading zeros
    zfill <- function (n, w)
    {
        return(formatC(n, width = w, format = "d", flag = "0"))
    }
    calculate_hole_per_file <- function(df)
    {
        df$Physical_length = df$Physical_end - df$Physical_start + 1
        df = arrange(df, Logical_start)
        df$ext_id = 0:(nrow(df)-1)
        df = arrange(df, Physical_start)
        n = nrow(df)
        df$Next_phy_start = c(df$Physical_start[-1], NA)
        df$Hole_after_me = df$Next_phy_start - (df$Physical_end + 1)
        df$time_and_file = paste(df$monitor_time, df$filepath)
        df$file_ext_id = paste(df$filepath, zfill(df$ext_id, 5), sep=":") 
        df$info = paste(df$Level_index, df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
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

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        #df = subset(df, Max_level == Level_index)
        #df = regular_file_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        # Pick a subset to plot
        df = subset(df, monitor_time %in% c(
                                            #"year00000.season00001",
                                            #"year00001.season00001",
                                            "year00002.season00001",
                                            "year00003.season00001",
                                            #"year00004.season00001",
                                            #"year00005.season00001",
                                            ""
                                            ))
        df = arrange(df, Physical_start)
        df = ddply(df, .(monitor_time), head, n=30)

        nicelimits = get_extended_xlimits(df)
        nicelimits[1] = 0

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]



        #df = df[,c('Logical_start', 'Physical_start', 'Physical_length', 'Hole_after_me', 'monitor_time', 'filepath', "Level_index")]
        #df = arrange(df, Physical_start)
        #print(df[1:30,])
        #return()
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
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
            scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            #scale_x_continuous(limits=nicelimits)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
   

    get_extended_xlimits_freeblocks <- function(df)
    {
        xmin = min(df$start, na.rm = T)
        xmax = max(df$after_end, na.rm = T)
        xlen = xmax - xmin
        xadjust = xlen * 0.1
        xmin = xmin-xadjust
        xmax = xmax+xadjust
        return (c(xmin, xmax))
    }

    merge_neighbors <- function(df)
    {
        print("merge_neighbors.........................")
        df = calculate_holes(df)
        df = arrange(df, start)
        n = nrow(df)
        newstart = df$start[1]
        newdf = NULL
        #print(df)
        for (i in 1:n) {
            #print (i)
            if (is.na(df$Hole_after_me[i]) || 
                df$Hole_after_me[i] > 0 || 
                i == n) {
                #print ("in if")
                newrow = data.frame(start=newstart, after_end=df$after_end[i])
                #print(newrow)
                newdf = rbind(newdf, newrow)

                if (i != n) {
                    newstart = df$start[i+1]
                }
            }            
        }
        newdf = calculate_holes(newdf)
        #print("olddf")
        #print(df[, c('start', 'end', 'Hole_after_me')])
        #print("newdf") 
        #print(newdf)
        return(newdf)
    }

    calculate_holes <- function(df)
    {
        df = arrange(df, start)
        df$ext_id = 0:(nrow(df)-1)

        n = nrow(df)
        df$Next_start = c(df$start[-1], NA)
        df$Hole_after_me = df$Next_start - df$after_end 
        df$info = paste(df$start, df$after_end-df$start, df$Hole_after_me, sep=",")
        return (df)
    }


    plot_physical_free_blocks <- function(df)
    {
        df$after_end = df$end+1 

        df = subset(df, monitor_time == "year00003.season00001")

        df = ddply(df, .(jobid, monitor_time), merge_neighbors)


        df = arrange(df, start)
        df = df[1:30,]

        nicelimits = get_extended_xlimits_freeblocks(df)
        nicelimits[1] = 0
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        #groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries <= df$Physical_end)  ]
        print(groupboundaries)
        
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        #print(df)
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color="#56B4E9", size=0.1 )+
            geom_segment(aes(x=start, 
                             xend=after_end,
                             y=ext_id, 
                             yend=ext_id
                             ), size=4, color='gray')+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=start, y=ext_id, 
                          label=info,
                          color='red'
                          ), size=2)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            #scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")+
            theme_bw()
        print (p)
    }


    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    windows()
    d.freeblocks = lst[['_freeblocks']]
    plot_physical_free_blocks( d.freeblocks )

}


test012ab_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test012ab/")

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
        df$info = paste(df$Level_index, df$Logical_start, df$Physical_start, df$Length, df$Hole_after_me, sep=",")
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

    # for results with extlist
    plot_physical_blocks <- function(df, nseasons)
    {
        #df = subset(df, Max_level == Level_index)
        df = regular_file_subset(df)
        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        nicelimits = get_extended_xlimits(df)

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        print(groupboundaries)
        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        p <- ggplot(df, aes()) +
            #geom_vline(xintercept = groupboundaries, 
                       #color="gray", 
                       ##color="#56B4E9", 
                       #size=0.1 )+
            geom_segment(aes(x=Physical_start, 
                             xend=Physical_end+1,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~., 
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
            scale_x_continuous(limits=nicelimits)+
            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")
        print (p)
    }
    
    merge_neighbors <- function(df)
    {
        print("merge_neighbors.........................")
        df = calculate_holes(df)
        df = arrange(df, start)
        n = nrow(df)
        newstart = df$start[1]
        newdf = NULL
        #print(df)
        for (i in 1:n) {
            #print (i)
            if (is.na(df$Hole_after_me[i]) || 
                df$Hole_after_me[i] > 0 || 
                i == n) {
                #print ("in if")
                newrow = data.frame(start=newstart, after_end=df$after_end[i])
                #print(newrow)
                newdf = rbind(newdf, newrow)

                if (i != n) {
                    newstart = df$start[i+1]
                }
            }            
        }
        newdf = calculate_holes(newdf)
        #print("olddf")
        #print(df[, c('start', 'end', 'Hole_after_me')])
        #print("newdf") 
        #print(newdf)
        return(newdf)
    }

    calculate_holes <- function(df)
    {
        df = arrange(df, start)
        df$ext_id = 0:(nrow(df)-1)

        n = nrow(df)
        df$Next_start = c(df$start[-1], NA)
        df$Hole_after_me = df$Next_start - df$after_end 
        df$info = paste(df$start, df$after_end-df$start, df$Hole_after_me, sep=",")
        return (df)
    }

    plot_physical_free_blocks <- function(df)
    {
        df$after_end = df$end+1 

        df = subset(df, monitor_time == "year00006.season00001")

        df = ddply(df, .(jobid, monitor_time), merge_neighbors)
        #print(df)
        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries <= df$Physical_end)  ]
        print(groupboundaries)
        
        #print(df)
        p <- ggplot(df, aes()) +
            geom_vline(xintercept = groupboundaries, color="#56B4E9", size=0.1 )+
            geom_segment(aes(x=start, 
                             xend=after_end,
                             y=ext_id, 
                             yend=ext_id
                             ), size=4, color='gray')+
            facet_grid(monitor_time~., ) +
            geom_text(aes(x=start, y=ext_id, 
                          label=info,
                          color='red'
                          ), size=2)+
            #theme( axis.text.x = element_blank() ) +
            theme( axis.text.y = element_blank() ) +
            scale_x_continuous(limits=c(0, fsblocks))+
            ylab("")+
            xlab("Physical block")+
            theme_bw()
        print (p)
    }

    d = lst[['_extlist']]
    print(head(d))
    plot_physical_blocks( d )

    windows()
    d.freeblocks = lst[['_freeblocks']]
    plot_physical_free_blocks( d.freeblocks )

}




test012b_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test012b/")

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

        df = subset(df, monitor_time %in% c("year00000.season00001", "year00004.season00001", "year00006.season00001"))
        p <- ggplot(df, aes()) +
            theme_bw()+
            #geom_vline(aes(xintercept = group_start), 
                       #color="pink", 
                       #size=1 )+
            geom_segment(aes(y=Physical_start, 
                             yend=Physical_end+1,
                             #x=file_ext_id, 
                             #xend=file_ext_id,
                             x=Logical_start,
                             xend=Logical_start,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~.
                       #~monitor_time, 
                       #scales='free',
                       #space='free'
                       ) +
            #geom_text(aes(x=Physical_start+13, y=file_ext_id, 
                          #label=info,
                          #color=filepath
                          #), size=3)+
            #theme( axis.text.x = element_blank() ) +
            #theme( axis.text.x = element_text(angle=45) ) +
            #theme( axis.text.y = element_blank() ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            #scale_x_continuous(limits=nicelimits, breaks=groupboundaries)+
            #scale_x_continuous(labels=groupboundaries)+
            #scale_x_continuous(limits=nicelimits)+
            #scale_x_continuous(limits=c(25000, 50000))+
            theme( 
                   #axis.text.y = element_blank(), 
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank()                   
                   ) +
            theme(axis.ticks.y=element_blank())+
            ylab("Physical block")+
            xlab("Logical block number")
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

        df = subset(df, monitor_time %in% c("year00000.season00001", "year00004.season00001", "year00006.season00001"))
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
                       monitor_time~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            #geom_text(aes(x=Physical_start+13, y=file_ext_id, 
                          #label=info,
                          #color=filepath
                          #), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
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
            ylab("")+
            xlab("Physical block")
        print (p)
    }

    # for results with extlist
    plot_physical_blocks_group_facet <- function(df, nseasons)
    {
        df$Physical_length = df$Physical_end - df$Physical_start + 1

        #df = subset(df, Level_index != -1)
        df = regular_file_subset(df)

        df = user_file_dir_subset(df)

        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        #groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries >= min(df$Physical_start)  & groupboundaries <= max(df$Physical_end) ]
        #groupboundaries = groupboundaries[ groupboundaries >= 33050  & groupboundaries <= 33090 ]
        print(groupboundaries)

        #df = add_year_season(df)

        nicelimits = get_extended_xlimits(df)
        nicelimits[1] = 32768 

        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        df = add_group_no_offset(df)
        df$group_no = factor(df$group_no)
        print(df$group_no)
        print(head(df))

        df = subset(df, monitor_time %in% c("year00000.season00001", "year00004.season00001", "year00006.season00001"))

        p <- ggplot(df, aes()) +
            theme_bw()+
            geom_vline(xintercept = 0, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=in_group_start, 
                             xend=in_group_start+Physical_length,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~group_no, 
                       scales='free',
                       space='free'
                       ) +
            geom_text(aes(x=in_group_start, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            #theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank(), 
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank()                   
                   ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            #scale_x_continuous(limits=nicelimits, breaks=groupboundaries, labels=paste("B:",groupboundaries, "\nG:", groupboundaries/32768, sep=""))+

            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")+
            theme(axis.ticks.y=element_blank())
        print (p)
    }
    
    d = lst[['_extlist']]
    print(head(d))
    #plot_physical_blocks( d )
    plot_physical_blocks_flip( d )

    #windows()
    #d.freeblocks = lst[['_freeblocks']]
    #plot_physical_free_blocks( d.fre~eblocks )

}

test012c_main <- function()
{
    lst <<- files2df("C:/Users/Jun/Dropbox/0-Research/0-metadata/docs/Oct01/Datahub/Tests/test012c/")

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

        df = subset(df, monitor_time %in% c(
                                            #"year00000.season00001", 
                                            #"year00004.season00001", 
                                            #"year00006.season00001",
                                            "year00007.season00001",
                                            ""
                                            ))
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
                       monitor_time~., 
                       #~monitor_time, 
                       scales='free',
                       space='free'
                       ) +
            #geom_text(aes(x=Physical_start+13, y=file_ext_id, 
                          #label=info,
                          #color=filepath
                          #), size=3)+
            theme( axis.text.x = element_text(angle=45) ) +
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
            ylab("")+
            xlab("Physical block")
        print (p)
    }

    # for results with extlist
    plot_physical_blocks_group_facet <- function(df, nseasons)
    {
        df$Physical_length = df$Physical_end - df$Physical_start + 1

        #df = subset(df, Level_index != -1)
        df = regular_file_subset(df)

        df = user_file_dir_subset(df)

        df = ddply(df, .(filepath, jobid, monitor_time), calculate_hole_per_file)

        fsblocks = 4*2^30/4096
        groupboundaries = (128*1024*1024/(4096)) * (0:32)
        #groupboundaries = groupboundaries[ groupboundaries <= fsblocks  ]
        #groupboundaries = groupboundaries[ groupboundaries >= min(df$Physical_start)  & groupboundaries <= max(df$Physical_end) ]
        #groupboundaries = groupboundaries[ groupboundaries >= 33050  & groupboundaries <= 33090 ]
        print(groupboundaries)

        #df = add_year_season(df)

        nicelimits = get_extended_xlimits(df)
        nicelimits[1] = 32768 

        groupboundaries = groupboundaries[ groupboundaries >= nicelimits[1] & groupboundaries <= nicelimits[2] ]

        df = add_group_no_offset(df)
        df$group_no = factor(df$group_no)
        print(df$group_no)
        print(head(df))

        df = subset(df, monitor_time %in% c("year00000.season00001", "year00004.season00001", "year00006.season00001"))

        p <- ggplot(df, aes()) +
            theme_bw()+
            geom_vline(xintercept = 0, 
                       color="gray", 
                       #color="#56B4E9", 
                       size=0.1 )+
            geom_segment(aes(x=in_group_start, 
                             xend=in_group_start+Physical_length,
                             y=file_ext_id, 
                             yend=file_ext_id,
                             color=filepath), size=5)+
            facet_grid(
                       monitor_time~group_no, 
                       scales='free',
                       space='free'
                       ) +
            geom_text(aes(x=in_group_start, y=file_ext_id, 
                          label=info,
                          color=filepath
                          ), size=3)+
            #theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.x = element_text(angle=45) ) +
            theme( axis.text.y = element_blank(), 
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank()                   
                   ) +
            #scale_x_continuous(limits=c(0, fsblocks))+
            #scale_x_continuous(limits=nicelimits, breaks=groupboundaries, labels=paste("B:",groupboundaries, "\nG:", groupboundaries/32768, sep=""))+

            #scale_x_continuous(limits=c(25000, 50000))+
            ylab("")+
            xlab("Physical block")+
            theme(axis.ticks.y=element_blank())
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

