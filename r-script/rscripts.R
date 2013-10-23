

df = rdmine()


# speed
ggplot(df, aes(y=speed, x=factor(do_merging))) +
	geom_boxplot() +
	facet_wrap(~mapfile, nrow=3) +
	expand_limits(y=0)

df$sort_flush = interaction("sort", df$do_sort, "flush", df$do_flush, drop=T)	
ggplot(df, aes(y=speed, x=factor(do_merging))) +
	geom_boxplot(aes(color=sort_flush)) +
	facet_wrap(~mapfile, nrow=3) +
	expand_limits(y=0)	
	
	
	

# add time	
ggplot(df, aes(y=add_time, x=factor(do_merging))) +
	geom_boxplot() +
	facet_wrap(~mapfile, nrow=3) +
	expand_limits(y=0)

ggplot(df, aes(y=add_time, x=factor(do_merging))) +
	geom_boxplot(aes(color=sort_flush)) +
	facet_wrap(~mapfile, nrow=3) +
	expand_limits(y=0)	

	
# sort flush time


df.noapp3 = subset(df, mapfile != "app3.map")
ggplot(df.noapp3, aes(y=sort_flush_time, x=sort_flush)) +
	geom_boxplot(aes(color=factor(do_merging))) +
	facet_wrap(~mapfile, nrow=3) +
	expand_limits(y=0)

	
	
windows()
df.app3 = subset(df, mapfile == "app3.map")
ggplot(df.noapp3, aes(y=sort_flush_time, x=sort_flush)) +
	geom_boxplot(aes(color=factor(do_merging))) +
	facet_wrap(~mapfile, nrow=3) +
	expand_limits(y=0)

	
#######################
df.noapp3 = subset(df, mapfile != "app3.map")
ggplot(df.noapp3, aes(y=sort_flush_time, x=factor(do_merging))) +
	geom_boxplot(aes(color=sort_flush)) +
	facet_wrap(~mapfile, nrow=3) +
	expand_limits(y=0)		
windows()
df.app3 = subset(df, mapfile == "app3.map")
ggplot(df.app3, aes(y=sort_flush_time, x=factor(do_merging))) +
	geom_boxplot(aes(color=sort_flush)) +
	facet_wrap(~mapfile, nrow=3) +
	expand_limits(y=0)	

	
# does merging help	reducing size
ggplot(df, aes(y=cnt/final_size, x=factor(do_merging))) +
	geom_bar(stat = "identity") +
	facet_wrap(~mapfile, nrow=3) +
	expand_limits(y=0)
	
	
	
###########################

df =  rdmine()

# readir time

ggplot(df, aes(y=readdir_time, x=index_dir)) +
	geom_boxplot() +
	opts(axis.text.x=theme_text(angle=45, hjust=1)) +
	expand_limits(y=0)


# readIndex time	
ggplot(df, aes(y=readindex_time, x=index_dir)) +
	geom_boxplot() +
	opts(axis.text.x=theme_text(angle=45, hjust=1)) +
	expand_limits(y=0)	

	
windows()	
tmp = subset(df, !(index_dir %in% c("app3.map", "btio.16pe.simple.map")))
ggplot(tmp, aes(y=readindex_time, x=index_dir)) +
	geom_boxplot() +
	opts(axis.text.x=theme_text(angle=45, hjust=1)) +
	expand_limits(y=0)		
	
# readIndex speed
ggplot(df, aes(y=(globalindex_size/readindex_time), x=index_dir)) +
	geom_boxplot() +
	opts(axis.text.x=theme_text(angle=45, hjust=1)) +
	expand_limits(y=0)	
# readIndex bandwidth
ggplot(df, aes(y=(globalindex_size*48/(readindex_time*1024*1024)), x=index_dir)) +
	geom_boxplot() +
	opts(axis.text.x=theme_text(angle=45, hjust=1)) +
	expand_limits(y=0)	+ ylab("read index bandwidth (MB/s)")
	
	
# build global time
	
ggplot(df, aes(y=buildglobal_time, x=index_dir)) +
	geom_boxplot() +
	opts(axis.text.x=theme_text(angle=45, hjust=1)) +
	expand_limits(y=0)	

windows()	
tmp = subset(df, !(index_dir %in% c("app3.map", "btio.16pe.simple.map")))	
ggplot(tmp, aes(y=buildglobal_time, x=index_dir)) +
	geom_boxplot() +
	opts(axis.text.x=theme_text(angle=45, hjust=1)) +
	expand_limits(y=0)		
	
	
# build global speed
ggplot(df, aes(y=globalindex_size/buildglobal_time, x=index_dir)) +
	geom_boxplot() +
	opts(axis.text.x=theme_text(angle=45, hjust=1)) +
	expand_limits(y=0)	

	
	
	
	
	
	
	
	
####################
# Fake workload



#size
plot_count<-function()
{
	df = rdmine()
	df.plot = subset(df, do_flush==0 & do_merging==1 & do_sort==1)
	df.plot = melt(df.plot, id=c("mapfile"), 
				measure=c("cnt","size_afterdumbmerge","size_aftersortandmerge","sort_flush_time","add_time"))
				
	df.plot = subset(df.plot, variable%in%c("cnt","size_afterdumbmerge","size_aftersortandmerge"))
	df.plot = ddply(df.plot, .(mapfile,variable), head, n=1)
	# plot only sizes
	ggplot(df.plot, 
			aes(y=value, x=factor(variable))) +
			geom_bar(stat="identity") + 
			geom_text(aes(label=value,y=value+150000))+
			opts(axis.text.x=theme_text(angle=45, hjust=1)) +
			scale_x_discrete(label=c("Original", "After Dumb Merge", "After Sort&Merge"))+
			facet_wrap(~mapfile) +
			ylab("Size (count)") + xlab("")
}
# plot sort_time
plot_sort_time<-function()
{
	df = rdmine()
	df.plot = subset(df, do_flush==0 & do_merging==1 & do_sort==1)
	df.plot = melt(df.plot, id=c("mapfile"), 
				measure=c("sort_flush_time"))
				
	#df.plot = subset(df.plot, variable%in%c("cnt","size_afterdumbmerge","size_aftersortandmerge"))
	#df.plot = ddply(df.plot, .(mapfile,variable), head, n=1)
	# plot only sizes
	ggplot(df.plot, 
			aes(y=value, x=mapfile)) +
			geom_boxplot() + 
			#geom_text(aes(label=value,y=value+150000))+
			opts(axis.text.x=theme_text(angle=45, hjust=1))
}
plot_sort_time()


# plot sort_time
plot_sortflush_time<-function()
{
	df = rdmine()
	df.plot = subset(df, do_flush==0 & do_merging==1 & do_sort==1)
	df.plot = melt(df.plot, id=c("mapfile"), 
				measure=c("sort_flush_time"))
				
	#df.plot = subset(df.plot, variable%in%c("cnt","size_afterdumbmerge","size_aftersortandmerge"))
	#df.plot = ddply(df.plot, .(mapfile,variable), head, n=1)
	# plot only sizes
	ggplot(df.plot, 
			aes(y=value, x=mapfile)) +
			geom_boxplot() + 
			#geom_text(aes(label=value,y=value+150000))+
			opts(axis.text.x=theme_text(angle=45, hjust=1))+
			ylab("Time (second)")
}
plot_sortflush_time()













