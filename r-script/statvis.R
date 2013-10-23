# data frame list
# df.btio = rdmine()
# df.chombo = rdmine()
# df.flash64 = rdmine()
# df.lanlapp.1 = rdmine()
# df.lanlapp2.appio = rdmine()
# df.lanlapp2.col = rdmine()
# df.lanlapp2.ind = rdmine()
# df.lanlapp3.50000 = rdmine()
# df.fstest64 = rdmine()
# df.flash64.hdf5.chk = rdmine()
# df.flash64.hdf5.cnt = rdmine()
# df.flash64.hdf5.crn = rdmine()

create_list <- function ()
{
	attr(df.btio, "dfname") = "df.btio"
	attr(df.chombo, "dfname") = "df.chombo"
	attr(df.flash64 , "dfname") = "df.flash64"
	attr(df.lanlapp.1 , "dfname") = "df.lanlapp.1"
	attr(df.lanlapp2.appio , "dfname") = "df.lanlapp2.appio"
	attr(df.lanlapp2.col , "dfname") = "df.lanlapp2.col"
	attr(df.lanlapp2.ind, "dfname") = "df.lanlapp2.ind"
	attr(df.lanlapp3.50000 , "dfname") = "df.lanlapp3.50000"
	attr(df.fstest64, "dfname") = "df.fstest64"
	attr(df.flash64.hdf5.chk , "dfname") = "df.flash64.hdf5.chk"
	attr(df.flash64.hdf5.cnt, "dfname") = "df.flash64.hdf5.cnt"
	attr(df.flash64.hdf5.crn , "dfname") = "df.flash64.hdf5.crn"
	
	mylist <- list()
	mylist[[1]] = df.btio
	mylist[[2]] = df.chombo 
	mylist[[3]] = df.flash64 
	mylist[[4]] = df.lanlapp.1 
	mylist[[5]] = df.lanlapp2.appio 
	mylist[[6]] = df.lanlapp2.col 
	mylist[[7]] = df.lanlapp2.ind 
	mylist[[8]] = df.lanlapp3.50000 
	mylist[[9]] = df.fstest64 
	mylist[[10]] = df.flash64.hdf5.chk 
	mylist[[11]] = df.flash64.hdf5.cnt 
	mylist[[12]] = df.flash64.hdf5.crn 
	mylist
}

foreach_list <-function(mylist, FUN)
{

dfnames = c("df.btio",
			"df.chombo",
			"df.flash64",
			"df.lanlapp.1",
			"df.lanlapp2.appio",
			"df.lanlapp2.col",
			"df.lanlapp2.ind",
			"df.lanlapp3.50000",
			"df.fstest64",
			"df.flash64.hdf5.chk",
			"df.flash64.hdf5.cnt",
			"df.flash64.hdf5.crn")

	for(i in seq(1,12)) {
		print(attr(mylist[[i]], "dfname"))
		FUN(mylist[[i]])
		readline()
	}
	
}


plot_gantt_plfs_fileview <- function (df)
{
	df$ORG.PID = as.factor(df$ORG.PID)

	ggplot(df, aes(colour=ORG.PID)) +
		geom_segment(aes(x=Logical_offset, xend=Logical_tail, y=ORG.PID, yend=ORG.PID ), size=3) +
		xlab("Logical Offset") + ylab("Rank")
}



plot_gantt_plfs_fileview_sort <- function (df)
{	
	# order PID by the first logical offset
	df.hds = ddply(df, .(ORG.PID), function(x) head(x,1))
	df.hds = arrange(df.hds, Logical_offset)
	#orderedPid = factor(df.hds$ORG.PID, levels = df.hds$ORG.PID)
	orderedPid = df.hds$ORG.PID
	
	# reorder the ORG.PID levels
	df$ORG.PID = factor(df$ORG.PID, levels = orderedPid)

	ggplot(df, aes(colour=ORG.PID)) +
		geom_segment(aes(x=Logical_offset, xend=Logical_offset+Length, y=ORG.PID, yend=ORG.PID ), size=3) +
		xlab("Logical Offset") + ylab("Rank")
}


############################################################
plot_gantt_plfs <- function (df)
{
	df$ORG.PID = as.factor(df$ORG.PID)
	themin = min(df$Begin_timestamp)

	df$Begin_timestamp = df$Begin_timestamp - themin
	df$End_timestamp = df$End_timestamp - themin

	ggplot(df, aes(colour=ORG.PID)) +
		geom_segment(aes(x=Begin_timestamp, xend=End_timestamp, y=Logical_offset, yend=Logical_tail), size=1) +
		xlab("Time (sec)")   #+coord_flip()
}


############################################################
plot_gantt <- function (df)
{
 df$pid = as.factor(df$pid)
 themin = min(df$starttime)
 sn = 1:nrow(df)
 df = cbind(sn, df)
 df$starttime = df$starttime - themin
 df$endtime = df$endtime - themin
 df$tail = df$offset + df$length

 for ( mypid in levels(df$pid) ) {
  df.toplot = subset( df, pid == mypid )
  p = ggplot(df.toplot, aes()) +
   geom_segment(aes(x=offset, xend=tail, y=sn, yend=sn), size=1) +
   facet_wrap(~pid, ncol=4)
  print(p)
  readline()
 }
}

############################################################
# Output basic statistics of the map
# Write range
# Overall duration time
# Number of Writes
# Length statistics
map_stat <- function(df)
{
	stime = min(df$Begin_timestamp)
	df$Begin_timestamp = df$Begin_timestamp - stime
	df$End_timestamp = df$End_timestamp - stime
	
	
	df$duration = df$End_timestamp - df$Begin_timestamp
	
	
	pfileview = plot_gantt_plfs_fileview_sort(df)
	windows()
	print(pfileview)
	
	
	pgannt = plot_gantt_plfs(df)
	windows()
	print(pgannt)
	
	
	print("Write range")
	print(paste(min(df$Logical_offset), " to ",
	        max(df$Logical_tail)) )
	
	print("Overall duation time")
	overalltime = max(df$End_timestamp)-min(df$Begin_timestamp)
	print( overalltime )
	
	print("Aggragated write size")
	overallsize = sum(as.numeric(df$Length))
	print( paste(overallsize, "bytes or ", overallsize/(1024*1024), "MB") )
	
	print("Overall bandwidth (may be lower than peak)")
	overallbw = overallsize/overalltime
	print( paste(overallbw/(1024*1024), "MB/s") )
	
	print("Total number of writes (may not be accurate due to merging before tracing)")
	nwrites = nrow(df)
	print(nwrites)
	
	print("index size (48 bytes per entry)")
	print(paste(nwrites*48/(1024*1024), " MB"))
	
	print("Length summary 1")
	print(summary(df$Length))
	
	#print("Length summary 2")
	#print(summary(as.factor(df$Length)))
	
	phist_len = ggplot(df, aes(Length)) +
			geom_histogram(aes(y  =  ..count..), binwidth  =  1024) +
			ggtitle("Overall Length histogram from global data")
	windows()
	print(phist_len)
	
	
	phist_duration = ggplot(df, aes(duration)) +
			geom_histogram(aes(y  =  ..count..)) + 
			ggtitle("Overall duration histogram from global data")
	windows()
	print(phist_duration)
	
	# Holes and overlaps
	df = find_hole(df)
	print("Number of holes")
	print( nrow(subset(df, prefix_hole>0)) )
	print("Number of Overlap. May be already handled by PLFS")
	print( nrow(subset(df, prefix_hole<0)) )
	
	# IOPS histogram (op issue rate)
	phist_iops = ggplot(df, aes(Begin_timestamp)) +
			geom_histogram(aes(y  =  ..count..), binwidth  =  1.0) + ylab("Number of IO ops issued") + 
			ggtitle("from global data")
	windows()
	print(phist_iops)
	
	# Length vs logical offset globally
	plength_global = ggplot(df, aes(x=Logical_offset, y=Length, color=factor(Length))) +
			geom_point() + ggtitle("From global data")
	windows()
	print(plength_global)	
	
	# prefix_hole vs logical offset globally
	phole_global = ggplot(df, aes(x=Logical_offset, y=prefix_hole, color=factor(prefix_hole))) +
			geom_point() + ggtitle("From global data")
	windows()
	print(phole_global)
	
	
	########################################################
	# Do it locally
	########################################################
		
	
	# plot holes of a single pid
	df.pid = subset(df, ORG.PID == 3)
	df.pid = find_hole(df.pid)
	phole_pid = ggplot(df.pid, aes(x=Logical_offset, y=prefix_hole, color=factor(prefix_hole))) +
			geom_point() + ggtitle("From local data")
	windows()
	print(phole_pid)
	
	print("Length summary of a PID")
	print(summary(as.factor(df.pid$Length)))
	
	plength_pid = ggplot(df.pid, aes(x=Logical_offset, y=Length, color=factor(Length))) +
			geom_point() + ggtitle("From local data")
	windows()
	print(plength_pid)	
	
	# Merge contiguous
	#df.merged = merge_contiguous(df)
	#print("number of rows after merging")
	#print(nrow(df.merged))
}

############################################
########## To find hole in each PID group
############################################

find_hole <- function(map)
{
	# sort by logical offset
	map = arrange(map, Logical_offset)

	tail_len = length(map$Logical_tail)
	map$pre_tail = c(map$Logical_offset[1], map$Logical_tail[1:tail_len-1]+1) 
	map$prefix_hole = map$Logical_offset - map$pre_tail
	# If prefix hole is a postive nubmer
	#    then there is a hole
	# else if it is negative
	#    then there is AT LEAST an overlap
	# else 0
	#    then it is the first write or it is contiguous with the previous one
	map
}

find_hole_bypid <- function(df)
{
	ddply(df, .(ORG.PID), find_hole_in_pid)
}

##############################################
# df should have 1 row at least
mark_contiguous_seg <- function(df)
{
	df = find_hole(df)
	n = nrow(df)
	df$seg = rep(-1, n)
	segidx = 0
	for ( i in 1:n) {
		if (df$prefix_hole != 0 || i == 1) {
			segidx = segidx + 1
		}
		df$seg[i] = segidx
	}
	df
}

##############################################################################
############# merge local(per process) contiguous
##############################################################################

# the pid has to be the same
merge_contiguous_withinpid <- function(df)
{

	tail_len = length(df$Logical_tail)
	df$pre_tail = c(df$Logical_offset[1], df$Logical_tail[1:tail_len-1]+1) 
	df$prefix_hole = df$Logical_offset - df$pre_tail

	# mark all segment-start entry
	df$segstart = (df$prefix_hole != 0)
	df$segstart[1] = TRUE # this first one is always a start
	
	df$segend = c(df$segstart[-1], TRUE) # the last one is always an end
	
	starts = subset(df, segstart == TRUE)
	ends = subset(df, segend == TRUE)
	
	starts$Length = ends$Logical_offset + ends$Length - starts$Logical_offset
	starts$End_timestamp = ends$End_timestamp
	starts$Logical_tail = starts$Logical_offset + starts$Length - 1
	
	starts
}

merge_local_contig <- function(df)
{
	ddply(df, .(ORG.PID), merge_contiguous_withinpid)
}








# all rows in df should have the same seg number
shrink_contiguous_seg <- function(df)
{
	n = nrow(df)
	if ( n <= 1 ) {
		df
	} else {
		first = head(df,1)
		last = tail(df,1)
		first$Length = sum(as.numeric(df$Length))
		first$End_timestamp = last$End_timestamp
		first$Logical_tail = last$Logical_tail
		#first$Chunk_offset = NA
		first
	}
}


merge_contiguous <- function(df)
{
	df = mark_contiguous_seg(df)
	ddply(df, .(seg), shrink_contiguous_seg)
}


############################################

find_backjump <- function(df)
{
	df = arrange(df, Begin_timestamp)
	df$post_off = c( df$Logical_offset[-1], tail(df$Logical_offset, n=1) )
	df$strides = df$post_off - df$Logical_offset
	nbackjmps = sum(df$strides < 0)
	
	
	ret = data.frame( rbind( c(df$ORG.PID[1], nrow(df), nbackjmps) ))
	#print (ret)
	names(ret) = c("ORG.PID", "TotalEntries", "nBackJumps")
	#head(df)
	ret
}

ddply_find_backjump <- function(df)
{
	cnts = ddply(df, .(ORG.PID), find_backjump)
	#print ("Number of local backward jumps")
	#names(cnts) = c("ORG.PID","Backjump")
	print ( paste(attr(df, "dfname"), "  Total:", 
	        sum(cnts$TotalEntries), "  Backjumps", 
			sum(cnts$nBackJumps) ) )
}


find_sortandmerge_size <- function(df)
{
	df = arrange(df, Begin_timestamp)
	cnt = length(df$Logical_tail)
	df$pre_tail = c(-1, df$Logical_tail[1:cnt-1]+1) 
	df$prefix_hole = df$Logical_offset - df$pre_tail
	
	# if the prefix_hole is 0, then we can reduce one entry
	size_sortbytime = cnt - sum( df$prefix_hole==0 )
	
	#print (head(df))
	df = arrange(df, Logical_offset)
	cnt = length(df$Logical_tail)
	df$pre_tail = c(-1, df$Logical_tail[1:cnt-1]+1) 
	df$prefix_hole = df$Logical_offset - df$pre_tail
	
	# if the prefix_hole is 0, then we can reduce one entry
	size_sortbyoffset = cnt - sum( df$prefix_hole==0 )
	
	ret = c(df$ORG.PID[1], cnt, size_sortbytime, 
	        size_sortbyoffset, size_sortbytime-size_sortbyoffset)
	ret = data.frame( rbind(ret) )
	names(ret) = c("ORG.PID", "OldSize", "size_sortbytime", "size_sortbyoffset", "sortbyoffset_reduce")
	ret
}

ddply_find_sortandmerge_size <- function(df)
{
	cnts = ddply(df, .(ORG.PID), find_sortandmerge_size)
	total_reduction = sum( cnts$sortbyoffset_reduce )
	print (paste("Total reduction for this app: ",total_reduction, " etnries or ", 
				total_reduction*48, " bytes"))
	#print (cnts)
	#cnts
	appcnt = c( sum(cnts$OldSize), sum(cnts$size_sortbytime), sum(cnts$size_sortbyoffset) )
	appcnt = data.frame( rbind(appcnt) )
	names(appcnt) = c("OldSize", "size_sortandmerge_bytime", "size_sortandmerge_byoffset")
	appcnt$dfname = attr(df, "dfname")
	appcnt
}

plot_sortandmerge_size <- function(mylist)
{
	mydf = NULL
	for(i in seq(1,12)) {		
		myrow = ddply_find_sortandmerge_size(mylist[[i]])
		mydf = rbind(mydf, myrow)
	}
	names(mydf) = c("OldSize", "size_sortandmerge_bytime", "size_sortandmerge_byoffset", "dfname")
	print (mydf)
	tmp2 = melt(mydf, id=c("dfname"), measure=c("OldSize", "size_sortandmerge_bytime", "size_sortandmerge_byoffset"))
	
	tmp2$kbsize = floor(tmp2$value*48/1024)
	print((tmp2))
	
	p = ggplot(tmp2, aes(x=factor(variable), y=kbsize)) +
		geom_bar(aes(fill=factor(variable)), stat="identity") +
		#geom_point()+
		geom_text(aes(label=kbsize, y=kbsize+5000), color="red") +
		ylab("Size (KB)") + xlab("Operation") +
		scale_x_discrete(labels=c("No Op", "Sort by time then merge", "Sort by offset then merge"))+
		facet_wrap(~dfname, nrow=3) +
		opts(axis.text.x=theme_text(angle=45, hjust=1))
	print (p)
}


plot_sortandmerge_count <- function(mylist)
{
	mydf = NULL
	for(i in seq(1,12)) {		
		myrow = ddply_find_sortandmerge_size(mylist[[i]])
		mydf = rbind(mydf, myrow)
	}
	names(mydf) = c("OldSize", "size_sortandmerge_bytime", "size_sortandmerge_byoffset", "dfname")
	print (mydf)
	tmp2 = melt(mydf, id=c("dfname"), measure=c("OldSize", "size_sortandmerge_bytime", "size_sortandmerge_byoffset"))
	
	
	print((tmp2))
	
	p = ggplot(tmp2, aes(x=factor(variable), y=value)) +
		geom_bar(aes(fill=factor(variable)), stat="identity") +
		#geom_point()+
		geom_text(aes(label=value, y=value+50000, color=factor(variable))) +
		ylab("Count") + xlab("Operation") +
		scale_x_discrete(labels=c("No Op", "Sort by time then merge", "Sort by offset then merge"))+
		facet_wrap(~dfname, nrow=3) +
		opts(axis.text.x=theme_text(angle=45, hjust=1))
	print (p)
}

data_hole_fileview <- function(df)
{
	df = ddply(df, .(ORG.PID), find_hole)
	df.1 = df[,c("Logical_offset", "Length", "ORG.PID", "prefix_hole")]
	
	#sort it by starting offset
	# order PID by the first logical offset
	df.hds = ddply(df.1, .(ORG.PID), function(x) head(x,1))
	df.hds = arrange(df.hds, Logical_offset)
	#orderedPid = factor(df.hds$ORG.PID, levels = df.hds$ORG.PID)
	orderedPid = df.hds$ORG.PID
	
	
	
	# reorder the ORG.PID levels
	df.1$ORG.PID = factor(df.1$ORG.PID, levels = orderedPid)
	
	
	
	df.2 = df.1
	df.2 = within(df.2, {Logical_offset = Logical_offset - prefix_hole
				  Length = prefix_hole
				 })
	df.1$type = "data"
	df.2$type = "hole"
	df.3 = rbind(df.1, df.2)
	df.3$prefix_hole = NULL
	df.3$type = interaction(df.3$type, df.3$ORG.PID,  drop=T)
	#print (df.3$type)
	
	# reorder level
	orderedPid = rep(orderedPid, each=2)
	types = rep( c("data", "hole"), length(orderedPid)/2 )
	pidorder = paste(types, orderedPid, sep=".")
	df.3$type = factor(df.3$type, levels=pidorder)
	newlevels = sample(as.character(levels(factor(df.3$Length))))
	df.3$refactoredLen = factor(df.3$Length, levels=newlevels)
	
	
	p <- ggplot(df.3, aes()) +
			geom_segment(aes(x=Logical_offset, xend=Logical_offset+Length, y=factor(type), yend=factor(type), color=factor(refactoredLen)), size=10) +
			geom_text(aes(x=Logical_offset, y=factor(type), label=Logical_offset), size=1, color="black") +
			xlab("Logical Offset") + ylab("Type") 
			#xlim(1e9,3e9)
	print(p)	
}

##########################################
##########################################
##########################################
##########################################

generate_trace_bypattern <- function()
{
    # For LANL2.col
#   base = 26336 # where (offset) this pattern strats
#	segsize = 500032+63512000 # size of the largest segment shared by all processes
#	nseg = 70
#	len = 500094 # length of each write, in this version, all lengths are identical
#	np = 128

    base = 26336 # where (offset) this pattern strats
	segsize = 500032+63512000 # size of the largest segment shared by all processes
	nseg = 70
	len = 500094 # length of each write, in this version, all lengths are identical
	np = 128



	# iterators: pid and seg_i(which segment it is)
	# i want: $offset $length $pid
	
	# generate on pid by one pid
	df.t = NULL
	for ( pid in seq(0, np-1) ) {
		firstbyte = base+pid*len
		offs = seq(firstbyte, by=segsize, length.out=nseg)
		mydf = data.frame(Logical_offset=offs, Length=len, ORG.PID=pid)
		df.t = rbind(df.t, mydf)
	}
#print(df.t)	
    df.t
}

generate_trace_bypattern_chombo <- function()
{
    # For LANL2.col
#   base = 26336 # where (offset) this pattern strats
#	segsize = 500032+63512000 # size of the largest segment shared by all processes
#	nseg = 70
#	len = 500094 # length of each write, in this version, all lengths are identical
#	np = 128

    base = 1.8e08 # where (offset) this pattern strats
	nseg = 2
	np = 16
	len = floor((7.6e08-1.8e08)/np) # length of each write, in this version, all lengths are identical
	segsize = len*np # size of the largest segment shared by all processes



	# iterators: pid and seg_i(which segment it is)
	# i want: $offset $length $pid
	
	# generate on pid by one pid
	df.t = NULL
	for ( pid in seq(0, np-1) ) {
		firstbyte = base+pid*len
		offs = seq(firstbyte, by=segsize, length.out=nseg)
		mydf = data.frame(Logical_offset=offs, Length=len, ORG.PID=pid)
		df.t = rbind(df.t, mydf)
	}
#print(df.t)	
    df.t
}

# given a chunk of data, figure out how much data
# needs to be moved to its destination
get_movement_size <- function(off, len, pid, df.dest)
{
	bytes_rem = len
	trans = NULL #  FROM TO OFFSET SIZE
	
	while ( bytes_rem > 0 ) {
		cur_off = off + len - bytes_rem	
		cur_len = bytes_rem
#print( paste("current: ", cur_off, ", ", cur_len, ", ", pid) )
		# find the offset
		lower_range = which(df.dest$Logical_offset <= cur_off)
		if ( length(lower_range) == 0 ) {
			stop("something wrong! You have to find every picec of data")
		}
#print ("lower_range")
#print (lower_range)
		# Let me call the picec of data that might has cur_off, cur_len a chunk
		chunk_index = max(lower_range)
		chunk_off = df.dest$Logical_offset[chunk_index]
		chunk_len = df.dest$Length[chunk_index]
		chunk_pid = df.dest$ORG.PID[chunk_index]
#print( paste("chunk:", chunk_off, ", ", chunk_len, ", ", chunk_pid) )
		
		if ( cur_off > chunk_off+chunk_len ) {

			stop("shouldn't happen.")
		}
		
		bytes_move = 0
		if ( cur_off + cur_len <= chunk_off + chunk_len ) {
			# all included in this chunk
			bytes_rem = 0
			bytes_move = cur_len
		} else {
			# some of cur's bytes are beyond chunk			
			bytes_rem = (cur_off + cur_len) - (chunk_off + chunk_len)
			bytes_move = (chunk_off + chunk_len) - cur_off
		}
		
		if ( pid != chunk_pid ) {
			myv = c(pid, chunk_pid, cur_off, bytes_move)
			trans = rbind(data.frame(trans), rbind(myv))
#print(paste("have to move"))
#print(trans)
		} else {
#print (paste(pid,"no need to move") )
        }
#print( paste( "bytes_rem:", bytes_rem ))
#stop("SSSSSSSSSSSSS")
	}
#print("nrow(trans):", nrow(trans))
    if ( !is.null(trans)  ) {
        names(trans) = c("FROM", "TO", "OFFSET", "SIZE")
    }
	trans
}

get_movement_size_wrapper <- function(df, df.dest)
{
    if ( nrow(df) != 1 ) {
        stop("error in get_movement_size_wrapper")
    }
#print("in wrapper")
#print(df)
    get_movement_size(df$Logical_offset, df$Length, df$ORG.PID, df.dest)
}

# use the ordered pid in df.org to replace the pid in df.generated
ddply_replace_pid <- function(df.org, df.generated)
{
	df.org = arrange(df.org, Logical_offset)
	orderpid = unique(df.org$ORG.PID)
    #print (orderpid)

    df.ordered = ddply(df.generated, .(ORG.PID), replace_pid, pidorder=orderpid)
    df.ordered

}

# df is a group with same pid number
replace_pid <- function(df, pidorder)
{
	n = nrow(df)
    oldpid = df$ORG.PID[1]
    newpid = pidorder[oldpid+1]
    df$ORG.PID = rep(newpid, n)
    #print(paste("oldpid:", oldpid, "newpid:", newpid))
    df
}

move_main <- function()
{
    df.wanted = generate_trace_bypattern();
    df.wanted = ddply_replace_pid(df.lanlapp2.col, df.wanted)
    df.wanted = arrange(df.wanted, Logical_offset)
    print(head(df.wanted))
    # trim the df we have a little bit, it might be wider than wanted
    df.org = head(subset(merge_local_contig(df.lanlapp2.col), 
            Logical_offset >= 26336 & Logical_offset < 4360473568), n=100000)
    print(head(df.org)) 

    p.wanted = plot_gantt_plfs_fileview_sort(df.wanted)
    print(p.wanted)


#stop("stop")
    moves = ddply(df.org, .(Logical_offset), 
            get_movement_size_wrapper, df.dest = df.wanted)
    #print((moves))

    nwrites = nrow(df.org)
    nmoves = nrow(moves)
    szofmoves = sum(moves$SIZE)
    totalsz = sum( as.numeric(df.org$Length) )
    print( data.frame(nwrites, nmoves, szofmoves, totalsz) )
    
    moves
}

move_main_chombo <- function()
{
    # I only need the front 16 pid's.
    df.org = merge_local_contig(df.chombo)
    df.org = arrange(df.org, Logical_offset)
    front16pid = unique(df.org$ORG.PID)[1:16]
    df.org = subset(df.org, ORG.PID %in% front16pid) 
    df.org = subset(df.org, Logical_offset > 1.8e08 & (Logical_tail+1) < 7.6e08)
    print(head(df.org)) 

    df.wanted = generate_trace_bypattern_chombo();
    df.wanted = ddply_replace_pid(df.org, df.wanted)
    df.wanted = arrange(df.wanted, Logical_offset)
    print(head(df.wanted))
    p.wanted = plot_gantt_plfs_fileview_sort(df.wanted)
    print(p.wanted)
    # trim the df we have a little bit, it might be wider than wanted

#stop("stop")
    moves = ddply(df.org, .(Logical_offset), 
            get_movement_size_wrapper, df.dest = df.wanted)
    #print((moves))

    nwrites = nrow(df.org)
    nmoves = nrow(moves)
    szofmoves = sum(moves$SIZE)
    totalsz = sum( as.numeric(df.org$Length) )
    print( data.frame(nwrites, nmoves, szofmoves, totalsz) )
    
    moves
}


plot_movement <- function(df)
{
    df = arrange(df, OFFSET)
    orderFROM = unique(df$FROM)
    df$FROM = factor(df$FROM, levels=orderFROM)
    randomTO = sample(unique(df$TO))
    df$TO = factor(df$TO, levels=randomTO)
    p = ggplot(df, aes()) +
            geom_segment(aes(x=OFFSET, xend=OFFSET+SIZE, 
                             y=FROM, yend=FROM,
                             color=TO), size=3)
    print(p)
}


