ext3_metadata_size <- function(file_size)
{
    block_size = 4096;
    #size of the four parts
    mapsizes = c(12*block_size, #direct block
                 (block_size/4)*block_size, #indirect block
                 ((block_size/4)^2)*block_size, #double indirect block
                 ((block_size/4)^3)*block_size
                );
#    print(format(mapsizes, digits=16));

    rem_sz = file_size;
    meta_sz = 128; #inode size
    for (i in 1:4) {
#        print(paste("i:", i, "rem_sz:", rem_sz))
        partsz = mapsizes[i]
        if ( rem_sz <= partsz ) {
#            print("rem_sz<=partsz");
            #file end falls in here
            if ( i == 1 ) {
                break;
            } else {
                nblocks = rem_sz/block_size;
                nblocks_per_ind_block = block_size/4;

                pre_n_ind_blocks = 0
                for ( j in 1:(i-1) ) {
                    cur_n_ind_blocks = 0
                    if ( j == 1 ) {
                        # get number of indirect blocks
                        cur_n_ind_blocks = ceiling(nblocks/nblocks_per_ind_block);
                    } else {
                        cur_n_ind_blocks = ceiling(pre_n_ind_blocks/nblocks_per_ind_block);
                    }
                    meta_sz = meta_sz + cur_n_ind_blocks*block_size
                    pre_n_ind_blocks = cur_n_ind_blocks;
#                    print(paste("cur_n_ind_blocks:", cur_n_ind_blocks))
                }
            }

            break;
        } else {
#            print("rem_sz>partsz");
            if ( i == 4 ) {
                stop("file size is too large");
            } else if ( i %in% c(2,3) ) {
                nblocks = partsz/block_size;
                nblocks_per_ind_block = block_size/4;

                for ( j in 1:(i-1) ) {
                    meta_sz = meta_sz + block_size*(nblocks/(nblocks_per_ind_block^j))
                }
            }
            rem_sz = rem_sz - partsz
        }
    }
    meta_sz
}

plfs_on_ext3 <- function(size_per_proc, nwrites_per_proc, np) 
{
    plfs_index = 48*nwrites_per_proc*np;
    ext_meta_indexfile = ext3_metadata_size(48*nwrites_per_proc)*np
    ext_meta_datafile = ext3_metadata_size(size_per_proc)*np
    return ( data.frame(plfs_index, ext_meta_indexfile, ext_meta_datafile) )
}

ddply_plfs_on_ext3 <- function(df)
{
    tmp = plfs_on_ext3(df$size_per_proc, df$nwrites_per_proc, df$np);
    return(cbind(df, tmp))
}

plot_plfs_on_ext3 <- function()
{
    myseq = c()
    for ( i in 1:3 ) {
        myseq = c(myseq, 4^i);
    }
    df = expand.grid(size_per_proc=myseq*(1024*1024), 
                     nwrites_per_proc=myseq*16, 
                     np=myseq^2);

#print(df);
    df = ddply(df, .(size_per_proc, nwrites_per_proc, np),
            ddply_plfs_on_ext3);
    df$size_per_proc_MB = factor(df$size_per_proc/(1024*1024));
    print(head(df))
#p = ggplot(df, aes(x=np, y=nwrites_per_proc)) +
#            geom_point(aes(size=plfs_index)) +
#            facet_wrap(~factor(size_per_proc))
#    print(p);
    df.melt = melt(df, id=c("size_per_proc_MB", "nwrites_per_proc", "np"), 
                       measure=c("plfs_index", "ext_meta_indexfile", "ext_meta_datafile" ));
    df.melt$value=df.melt$value/(1024*1024);

    nw_order = paste("# of writes per proc:",sort(unique(df.melt$nwrites_per_proc)));
    df.melt$nwrites_per_proc = paste("# of writes per proc:", df.melt$nwrites_per_proc);
    df.melt$nwrites_per_proc = factor(df.melt$nwrites_per_proc, levels = nw_order);

    szproc_order = paste("size_per_proc_MB:", sort(unique(df.melt$size_per_proc_MB)));
    df.melt$size_per_proc_MB = paste("size_per_proc_MB:", df.melt$size_per_proc_MB);
    df.melt$size_per_proc_MB = factor(df.melt$size_per_proc_MB, levels=szproc_order);

    df.melt$text_ypos = 0;
    myadjust = 100
    df.melt$text_ypos[ df.melt$variable == "plfs_index" ] = 350+myadjust;
    df.melt$text_ypos[ df.melt$variable == "ext_meta_indexfile" ] = 400+myadjust;
    df.melt$text_ypos[ df.melt$variable == "ext_meta_datafile" ] = 450+myadjust;

    df.melt$sztext = format(df.melt$value, digit=4, scientific=F);
    print(df.melt)
    pp = ggplot(df.melt, aes(x=factor(np), y=value)) +
            geom_bar(aes(fill=variable), stat="identity", dodge="stack") +
            geom_text(aes(label=sztext, y=text_ypos, color=variable), size=3) +
            facet_grid(nwrites_per_proc~size_per_proc_MB) + 
            xlab("Num of Proc") + ylab("Metadata Size (MB)")
    print(pp)
    
}

# this function calcuates the most compact inode size for a file
# each extent node holds 340 index/leaf, each leaf holds 128MB
# this should be the case for PLFS, since PLFS always appending
ext4_metadata_size <- function(nwrites, wsize, doMerge=T)
{
    EXTENT_SIZE = 128*1024*1024

    if ( doMerge == TRUE ) {
        file_size = nwrites * wsize
        nExtents = ceiling(file_size / EXTENT_SIZE)
    } else {
        n_extent_per_write = ceiling(wsize / EXTENT_SIZE)
        nExtents = nwrites * n_extent_per_write
    }

    INODE_BASE = 256 # inode basic data structure size
    N_EXTENTS_PER_NODE = 340
    N_EXTENTS_L1 = 4*N_EXTENTS_PER_NODE
    N_EXTENTS_L2 = N_EXTENTS_L1 * N_EXTENTS_PER_NODE 
    BLOCKSIZE = 4096

    n_leaves = 0
    n_internals = 0
    if ( nExtents <= 4 ) {
        # do nothing
    } else if ( nExtents <= N_EXTENTS_L1 ) {
        n_leaves = ceiling(nExtents / N_EXTENTS_PER_NODE)
    } else if ( nExtents <= N_EXTENTS_L2 ) {
        n_leaves = ceiling(nExtents / N_EXTENTS_PER_NODE)
        n_internals = ceiling(n_leaves / N_EXTENTS_PER_NODE) 
    } else {
        stop("Extent map overflow")
    }

    total_sz = INODE_BASE + (n_leaves + n_internals) * BLOCKSIZE
    return (total_sz)
}


#####################################################
#####################################################
#####################################################
#####################################################
# HDFS

# This function only calcuate the HDFS part of metadata
# it does not include the underlying ext metadata
hdfs_metadata_size_v0.13.1 <- function(file_size)
{
    block_size = 64*1024*1024 #64 MB

    nblocks = ceiling(file_size/block_size)    

    # File basic cost
    inode_base = 152
    dir_entry = 64 #TODO: may not need this when we have 
                   # another part calculating dir cost
    filename_len = 13
    filename_sz = filename_len*2
    ref_FSDirectory = 8

    per_file_basic_cost = inode_base + dir_entry + filename_sz + ref_FSDirectory

    # Block cost
    n_replica = 3 # 3 replicas
    blockclass_sz = 32
    blockinfo_sz = 64 + 8*n_replica
    ref_inodeblocks = 8
    BlocksMap_entry = 48
    DatanodeDesc = 64 * n_replica

    per_block_cost = blockclass_sz + blockinfo_sz + ref_inodeblocks + 
                    BlocksMap_entry + DatanodeDesc 

    # Total cost
    total_sz = per_file_basic_cost + nblocks * per_block_cost
    return (total_sz)
}

# Transient data are not included
# This size is got from Hadoop 1.2.1
hdfs_metadata_size_v1.2.1 <- function(file_size)
{
    block_size = 64*1024*1024 #64 MB

    nblocks = ceiling(file_size/block_size)    

    # some assumptions
    n_replica = 3
    filename_len = 13

    #########################################
    #########################################
    # INodeFile cost for this file
    # One file has only one INodeFile
    INodeFile_size = 16 + 
                     24 + filename_len + # name
                     8 + 8 + 8 + 8 +
                     8 + 2 + 8 + 8 +
                     24 + nblocks * 8 # blocks
    # cost of Block
    per_Block_size = 16 + 
                     8 + 8 + 8
    total_Block_sz = nblocks * per_Block_size


    #########################################
    #########################################
    # cost of parent dir
    # One file has only one entry in parent directory
    INodeDirectory_parent = 8 # an entry in children

    #########################################
    #########################################
    # blockMaps in FSNameSystem
    # cost of BlockInfo for blocks of this file
    per_BlockInfo_size = 16 +
                     8 +
                     8 +
                     24 + 3*8*n_replica

    total_blockinfo_size = nblocks * per_BlockInfo_size

    # cost of BlocksMap
    BlocksMap_size = 8*nblocks # LightWeightedGSET entry reference, at least this size
                               # the GSET entries are actually BlockInfo itself


    #########################################
    #########################################
    # DatanodeDescriptor in FSNameSystem
    # DatanodeDescriptor actually only holds a reference to
    # the head of a list of BlockInfo in FSNameSystem.blocksMap, 
    # so it takes NO space


    #########################################
    #########################################
    #########################################
    #########################################
    #########################################
    #########################################
    total_sz = INodeFile_size + total_Block_sz + INodeDirectory_parent +
                total_blockinfo_size + BlocksMap_size

    return (total_sz)
}

hdfs_metadata_size_vproposal <- function(file_size)
{
    block_size = 64*1024*1024 #64 MB

    nblocks = ceiling(file_size/block_size)    

    filename_len = 13
    n_replica = 3

    inode_sz = 112+filename_len
    block_sz = 112+24*n_replica

    total_sz = inode_sz + nblocks*block_sz
    return (total_sz)
}


###################################################
###################################################
###################################################
###################################################

hdfs_ext_metadata_size <- function(file_size, hdfs_ver="1.2.1", ext_ver="4")
{
    hdfs_size = 0
    if ( hdfs_ver == "1.2.1" ) {
        hdfs_size = hdfs_metadata_size_v1.2.1(file_size)
    } else if ( hdfs_ver == "0.13.1" ) {
        hdfs_size = hdfs_metadata_size_v0.13.1(file_size)
    } else {
        stop("Invalid HDFS version")
    }

    block_size = 64*1024*1024 #64 MB
    n_whole_blocks = file_size %/% block_size
    block_rem = file_size %% block_size

    ext_size = 0
    if (ext_ver == "4") {
        ext_size = ext_size + ext4_metadata_size(1, block_size) * n_whole_blocks
        if ( block_rem > 0 ) {
            ext_size = ext_size + ext4_metadata_size(1, block_rem)
        }
    } else if (ext_ver == "3") {
        ext_size = ext_size + ext3_metadata_size(block_size) * n_whole_blocks
        if ( block_rem > 0 ) {
            ext_size = ext_size + ext3_metadata_size(block_rem)
        }
    } else {
        stop("Invalid EXT version")
    }
    #hdfs_ext_size = hdfs_size + ext_size
    return ( data.frame(hdfs_size, ext_size) )
}


#################################################

plfs_on_hdfs_ext <- function(size_per_proc, nwrites_per_proc, np, 
                             my_hdfs_ver, my_ext_ver) 
{
    plfs_index = 48*nwrites_per_proc*np;
    hdfs_ext_meta_indexfile = np * hdfs_ext_metadata_size(48*nwrites_per_proc,
                                                          my_hdfs_ver,
                                                          my_ext_ver)
    hdfs_ext_meta_datafile = np * hdfs_ext_metadata_size(size_per_proc,
                                                         my_hdfs_ver,
                                                         my_ext_ver)
    index_and_datafile = rbind(hdfs_ext_meta_indexfile, 
                               hdfs_ext_meta_datafile)
    #print(index_and_datafile)
    index_and_datafile = colSums(index_and_datafile)
    #print(t(as.data.frame(index_and_datafile)))
    index_and_datafile = t(as.data.frame(index_and_datafile))
    return ( cbind(plfs_index, index_and_datafile) )
}

ddply_plfs_on_hdfs_ext <- function(df)
{
    tmp = with(df, {
               plfs_on_hdfs_ext(size_per_proc, nwrites_per_proc, np,
                           hdfs_ver, ext_ver)
                   })
    return(cbind(df, tmp))
}

plot_plfs_on_hdfs_ext <- function()
{
    myseq = c()
    for ( i in 1:3 ) {
        myseq = c(myseq, 4^i);
    }
    my_hdfs_ver = c("1.2.1", "0.13.1")
    my_ext_ver = c("3", "4")
    df = expand.grid(size_per_proc=myseq*(1024*1024), 
                     nwrites_per_proc=myseq*16, 
                     np=myseq^2,
                     hdfs_ver=my_hdfs_ver,
                     ext_ver=my_ext_ver)
    df$rowid = row.names(df) # so I can do per row ddply() 
    
    df = ddply(df, .(rowid), ddply_plfs_on_hdfs_ext);
    df$size_per_proc_MB = factor(df$size_per_proc/(1024*1024));
    #print(head(df))
    
    df.melt = melt(df, id=c("size_per_proc_MB", "nwrites_per_proc", "np", 
                            "hdfs_ver", "ext_ver"), 
                       measure=c("plfs_index", "hdfs_size", "ext_size" ));
    df.melt$value=df.melt$value/(1024*1024);

    for ( my_hdfs_ver in unique(df.melt$hdfs_ver) ) {
        for ( my_ext_ver in unique(df.melt$ext_ver) ) {
            df.plot = subset(df.melt, hdfs_ver == my_hdfs_ver & ext_ver == my_ext_ver)
            
            #wanted_order = c("plfs_index","hdfs_size","ext_size")
            wanted_order = c("ext_size","hdfs_size","plfs_index")
            df.plot$variable = factor(df.plot$variable, levels=wanted_order)
            #nw_order = paste("# of writes per proc:",sort(unique(df.plot$nwrites_per_proc)));
            #df.plot$nwrites_per_proc = paste("# of writes per proc:", df.plot$nwrites_per_proc);
            #df.plot$nwrites_per_proc = factor(df.plot$nwrites_per_proc, levels = nw_order);
            
            df.plot = within(df.plot, {nwrites_per_proc=factor(nwrites_per_proc);
                             levels(nwrites_per_proc)=
                                paste("# of writes per proc:", levels(nwrites_per_proc))
                            })

            #szproc_order = paste("size_per_proc_MB:", sort(unique(df.plot$size_per_proc_MB)));
            #df.plot$size_per_proc_MB = paste("size_per_proc_MB:", df.plot$size_per_proc_MB);
            #df.plot$size_per_proc_MB = factor(df.plot$size_per_proc_MB, levels=szproc_order);

            df.plot = within(df.plot, {
                                size_per_proc_MB = factor(size_per_proc_MB);
                                levels(size_per_proc_MB) = 
                                    paste("size_per_proc_MB:", levels(size_per_proc_MB));
                            })

            df.plot$text_ypos = 0;
            maxs = ddply(df.plot, .(variable), subset, value==max(value))
            maxs = ddply(maxs, .(variable), head, n=1)
            print(maxs)
            max_y_val = sum(maxs$value)
            v_gap = max_y_val*0.075
            df.plot$text_ypos = mapvalues(df.plot$variable, 
                                          from=wanted_order,
                                          to= max_y_val + c(1,2,3)*v_gap )
            df.plot$text_ypos = as.numeric(as.character(df.plot$text_ypos))
            #df.plot$text_ypos[ df.plot$variable == "plfs_index" ] = 350+myadjust;
            #df.plot$text_ypos[ df.plot$variable == "ext_meta_indexfile" ] = 400+myadjust;
            #df.plot$text_ypos[ df.plot$variable == "ext_meta_datafile" ] = 450+myadjust;

            df.plot$sztext = format(df.plot$value, digit=4, scientific=F);
            print(df.plot)
            print(summary(df.plot))
            pp = ggplot(df.plot, aes(x=factor(np), y=value, order=variable, color=variable, fill=variable)) +
                    geom_bar(aes(), stat="identity", dodge="stack") +
                    geom_text(aes(label=sztext, y=text_ypos), size=3) +
                    facet_grid(nwrites_per_proc~size_per_proc_MB) + 
                    xlab("Num of Proc") + ylab("Metadata Size (MB)") +
                    ggtitle( paste("PLFS+HDFS v", my_hdfs_ver, "+EXT v", my_ext_ver,"", sep="") ) +
                    scale_color_discrete(breaks=rev(wanted_order)) +
                    scale_fill_discrete(breaks=rev(wanted_order)) 
            windows()
            print(pp)
        }
    } 
}

plot_plfs_on_hdfs_ext_v2 <- function()
{
    myseq = c()
    for ( i in 1:3 ) {
        myseq = c(myseq, 4^i);
    }
    my_hdfs_ver = c("1.2.1", "0.13.1")
    my_ext_ver = c("3", "4")
    df = expand.grid(size_per_proc=c(64, 256, 1024, 4096)*1024*1024, 
                     nwrites_per_proc=c(1, 4096), 
                     np=c(1, 8, 64, 512)*4096,
                     hdfs_ver=my_hdfs_ver,
                     ext_ver=my_ext_ver)
    df$rowid = row.names(df) # so I can do per row ddply() 
    
    df = ddply(df, .(rowid), ddply_plfs_on_hdfs_ext);
    df$size_per_proc_MB = factor(df$size_per_proc/(1024*1024));
    #print(head(df))
    
    df.melt = melt(df, id=c("size_per_proc_MB", "nwrites_per_proc", "np", 
                            "hdfs_ver", "ext_ver"), 
                       measure=c("plfs_index", "hdfs_size", "ext_size" ));
    df.melt$value=df.melt$value/(1024*1024);

    for ( my_hdfs_ver in unique(df.melt$hdfs_ver) ) {
        for ( my_ext_ver in unique(df.melt$ext_ver) ) {
            df.plot = subset(df.melt, hdfs_ver == my_hdfs_ver & ext_ver == my_ext_ver)
            
            #wanted_order = c("plfs_index","hdfs_size","ext_size")
            wanted_order = c("ext_size","hdfs_size","plfs_index")
            df.plot$variable = factor(df.plot$variable, levels=wanted_order)
            #df.plot = subset(df.plot, variable != "plfs_index") #I don't need this one here
            
            df.plot = within(df.plot, {nwrites_per_proc=factor(nwrites_per_proc);
                             levels(nwrites_per_proc)=
                                paste("# of writes per proc:", levels(nwrites_per_proc))
                            })

            df.plot = within(df.plot, {
                                size_per_proc_MB = factor(size_per_proc_MB);
                                levels(size_per_proc_MB) = 
                                    paste("size_per_proc_MB:", levels(size_per_proc_MB));
                            })

            df.plot$text_ypos = 0;
            maxs = ddply(df.plot, .(variable), subset, value==max(value))
            maxs = ddply(maxs, .(variable), head, n=1)
            #print(maxs)
            max_y_val = sum(maxs$value)
            v_gap = max_y_val*0.075
            df.plot$text_ypos = mapvalues(df.plot$variable, 
                                          from=wanted_order,
                                          to= max_y_val + c(1,2,3)*v_gap )
            df.plot$text_ypos = as.numeric(as.character(df.plot$text_ypos))
            #df.plot$text_ypos[ df.plot$variable == "plfs_index" ] = 350+myadjust;
            #df.plot$text_ypos[ df.plot$variable == "ext_meta_indexfile" ] = 400+myadjust;
            #df.plot$text_ypos[ df.plot$variable == "ext_meta_datafile" ] = 450+myadjust;

            df.plot$sztext = format(df.plot$value, digit=4, scientific=F);
            #print(df.plot)
            #print(summary(df.plot))
            pp = ggplot(df.plot, aes(x=factor(np), y=value, order=variable, color=variable, fill=variable)) +
                    geom_bar(aes(), stat="identity", dodge="stack") +
                    geom_text(aes(label=sztext, y=text_ypos), size=3) +
                    facet_grid(nwrites_per_proc~size_per_proc_MB) + 
                    xlab("Num of Proc") + ylab("Metadata Size (MB)") +
                    ggtitle( paste("PLFS+HDFS v", my_hdfs_ver, "+EXT v", my_ext_ver,"", sep="") ) +
                    scale_color_discrete(breaks=rev(wanted_order)) +
                    scale_fill_discrete(breaks=rev(wanted_order)) 
            windows()
            print(pp)
        }
    } 
}

