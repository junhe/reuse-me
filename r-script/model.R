ext_metadata_size <- function(file_size)
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
    ext_meta_indexfile = ext_metadata_size(48*nwrites_per_proc)*np
    ext_meta_datafile = ext_metadata_size(size_per_proc)*np
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
            xlab("Num of Proc") + ylab("Size (MB)")
    print(pp)
    
}


