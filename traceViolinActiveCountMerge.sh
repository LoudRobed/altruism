#!/usr/bin/Rscript
library("ggplot2")

Args <- commandArgs(TRUE)
extract <- read.csv(file=Args[1],head=TRUE,sep=",")

splitedMedian <- lapply(strsplit(as.character(extract$median), " "),as.numeric) 

temp <- data.frame(iteration = (extract$iteration[[1]]/1000) , median =splitedMedian[[1]])
for ( i in seq(from = 2,to=length(splitedMedian),by=1)){
temp <- rbind( temp , data.frame(iteration = (extract$iteration[[i]]/1000) , median =splitedMedian[[i]]) )
 }


#pre-treatment
#if all data are equal for one iteration, just put 1
#c.f. https://github.com/hadley/ggplot2/issues/867
for (iteration in unique(temp$iteration))
{
	values <- temp[temp$iteration == iteration,2]
	if (length(unique(values)) == 1 && length(values) > 1)
	{
		val <- unique(values)
		temp <- temp[! temp$iteration == iteration,]
		temp <- rbind(temp,data.frame(iteration = iteration, median = val))
	}
}

postscript(Args[2])

#p <- ggplot(temp, aes(factor(iteration), as.numeric(median)))
p <- ggplot(temp, aes(factor(iteration), as.numeric(median)))
p <- p + geom_violin()
p <- p + stat_summary(fun.y="median", geom="point")
p <- p + xlab("Iteration (/1000)") 
p <- p + ylab("# Active agents") 
p + coord_cartesian(ylim=c(0,100))

dev.off()
