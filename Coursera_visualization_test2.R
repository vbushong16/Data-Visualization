
library(sqldf)
library(igraph)
require(reshape2)
library(plotrix)

### SETTING THE WORK DIRECTORY
wkdir = "~\\"
subdir = "Github\\Data-Visualization"

# create directory if it doesn't exist yet
dir.create(file.path(wkdir, subdir))
setwd(file.path(wkdir, subdir))


data = read.csv("coursera2.csv")
input_data = data

bsk.network<-graph.data.frame(input_data, directed=F)

# Plot the data.Some details about the graph can be specified in advance.
# For example we can separate some vertices (people) by color:

V(bsk.network)$name = strtrim(V(bsk.network)$name, 4)

V(bsk.network)$name[V(bsk.network)$name == "STAN"] = "STANDALONE"
V(bsk.network)$name[V(bsk.network)$name == "Pare"] = "PARENT"
V(bsk.network)$color<-ifelse(grepl("CD",V(bsk.network)$name), 'blue',
                             ifelse(grepl("CH",V(bsk.network)$name),'forest green',
                                    ifelse(grepl("WFB",V(bsk.network)$name),'firebrick',
                                           ifelse(grepl("STANDALONE",V(bsk.network)$name),'magenta',
                                                  ifelse(grepl("PARENT",V(bsk.network)$name),'orange','yellow'))))) 
                             #useful for highlighting certain people. Works by matching the name attribute of the vertex to the one specified in the 'ifelse' expression

# We can also color the connecting edges differently depending on the 'grade':

E(bsk.network)$color<-ifelse(E(bsk.network)$Amount > 10000000, "green", ifelse(E(bsk.network)$Amount < -10000000,"red","gray"))

# or depending on the different specialization ('spec'):

# E(bsk.network)$color<-ifelse(E(bsk.network)$spec=='X', "red", ifelse(E(bsk.network)$spec=='Y', "blue", "grey"))

# Note: the example uses nested ifelse expressions which is in general a bad idea but does the job in this case
# Additional attributes like size can be further specified in an analogous manner, either in advance or when the plot function is called:

V(bsk.network)$size<-degree(bsk.network)/3#here the size of the vertices is specified by the degree of the vertex, so that people supervising more have get proportionally bigger dots. Getting the right scale gets some playing around with the parameters of the scale function (from the 'base' package)
# Note that if the same attribute is specified beforehand and inside the function, the former will be overridden.

# And finally the plot itself:

par(mai=c(0,0,1,0)) 			#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
plot(bsk.network,	
     layout=layout_as_tree,#the graph to be plotted
     main='Bank Account Transaction network',	#specifies the title
     vertex.label.dist=-.1,			#puts the name labels slightly off the dots
     vertex.frame.color='black', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=1,			#the font of the name labels
     vertex.label=V(bsk.network)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=.5	#specifies the size of the font of the labels. can also be made to vary
)
ablineclip(v =c(-.67,-.4,-.13,.4), lty = 3,y1 = -1,y2 = .5)
legend(1,1,c("transactions > $10Million","transactions < $10Million"),cex = .5,lty=c(1,1), lwd=c(2,2),col=c("green","red"))
