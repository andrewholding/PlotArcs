#Settings
Shift<-10/dev.size("cm")[1] #Will correct vertical bar horizonatal positions,
                             #but only for the set window sizes, so needs adjustment
        
tick_shift <- 0.5 #How far ticks are from the main line
text_shift <- 0.40 #How far text is from the link
customPalette <- "Set3" #Color Palette
chartDotsize <- 0.16 #Set the size of the dots
highlightWidth<-3.5 #Width of Highlight
highlightColor<-"#FFFF0070"
stripeWidth<-4 #Width of Highlight
stripeColor<-"#DDEEFF70"
movieColor<-"#d0d0d0a0"
setwd("~/Desktop/PlotArcs/R")

arcs<-read.csv("allTrek.csv",stringsAsFactors = TRUE, check.names = FALSE)
arcs[nrow(arcs)+1, ]<-c(rep(NA,4),rep(0,ncol(arcs)-4), " ") #Adds another column to make graph render better.
looplink<-read.csv("allLinks.csv",stringsAsFactors = TRUE)
highlights<-read.csv("highlights.csv",stringsAsFactors = TRUE)
arcs<-arcs[,1:(length(colnames(arcs))-1)] #Hide Notes
categories<-read.csv("allCategories.csv",stringsAsFactors = TRUE)
rownames(categories)<-categories$Label

#index arcs
arcs<-cbind(data.frame(Index=seq(1:nrow(arcs))),arcs)

#Get indexes.
EpisodeNames<-paste0(arcs$Series,arcs$Episode)
EpisodeIndex<-arcs$Index
names(EpisodeIndex)<-EpisodeNames
looplink$index<-EpisodeIndex[paste0(looplink$Series,looplink$Episode)]
looplink_offsets<-looplink[c("Label","Offset", "xLabel", "yLabel")]
looplink<-looplink[c("index","var","Label")]
highlights$index<-EpisodeIndex[paste0(highlights$Series,highlights$Episode)]

library(reshape2)
arcs_long<-melt(arcs, id.vars=c("Index","Series","Episode","Name","Stardate"))

arcs_long$Label<-paste0(arcs_long$Series," (",arcs_long$Episode,") ",arcs_long$Name)
arcs_long$variable<-factor(gsub("[.]"," ",arcs_long$variable),levels=gsub("[.]"," ",names(arcs)[-1:-5]),ordered=TRUE)
arcs_long$Category<-factor(categories[as.character(arcs_long$variable),"Category"],ordered=TRUE)

arcs_order <- paste0(arcs$Series," (",arcs$Episode,") ",arcs$Name)

#Remove label from final column
arcs_order[arcs_order == "NA (NA) NA"] <- " "
arcs_long[arcs_long$Label == "NA (NA) NA","Label"] <-" "

#Set up colours
library(RColorBrewer)
mycolors<-colorRampPalette(brewer.pal(length(levels(arcs_long$Category)), customPalette))
FillColors<-mycolors(length(levels(arcs_long$Category)))
names(FillColors)<-as.character(levels(arcs_long$Category))



#Note - loops are linked the index, arcs[order(arcs$Episode),"Index"], if you 
#Reorder episodes, then you will have to correct the links.

#Start Plot
library(ggplot2)
library(ggpubr)
p<-ggplot( 
    data = arcs_long[arcs_long$value==1,], 
    aes(
        x=factor(Label, levels=arcs_order, ordered = TRUE),
        y=factor(variable)
    )
) +
    theme_pubr() +
    theme(    axis.line=element_blank(), 
              axis.ticks=element_blank(),
              legend.position="right")
#Background Bars (for some reason removing the geom_line breaks it)
p <-p+  geom_line( 
    data = arcs_long, 
    #position=position_dodge(width=-1),
    aes(
        x=factor(Label, levels=arcs_order, ordered = TRUE),
        y=factor(variable),
        group=(variable)
    ),
    color="grey70",
    size=0.1
)

###Verical Stripes

VerticalStripesIndex<-arcs$Index[(arcs$Index %% 2 == 1)]

VerticalStripes<-data.frame(index=rep(VerticalStripesIndex,2))

VerticalStripes$var<-c(
    rep(levels(arcs_long$variable)[1],nrow(VerticalStripes)/2),
    rep(levels(arcs_long$variable)[length(levels(arcs_long$variable))],nrow(VerticalStripes)/2)
)
VerticalStripes$Offset<-c(rep(-1.5,nrow(VerticalStripes)/2),rep(+1,nrow(VerticalStripes)/2))


p<-p + geom_path(data=as.data.frame(VerticalStripes),
                 aes(
                     x=index+Shift,
                     y=match(var, levels(arcs_long$variable))+Offset,
                     group=index
                 ),
                 
                 color=stripeColor,
                 size=stripeWidth
)


###Highlights

highlightEpisodesIndex<-highlights$index

highlight<-data.frame(index=rep(highlightEpisodesIndex,2))

highlight$var<-c(
    rep(levels(arcs_long$variable)[1],nrow(highlight)/2),
    rep(levels(arcs_long$variable)[length(levels(arcs_long$variable))],nrow(highlight)/2)
)
highlight$Offset<-c(rep(-1.5,nrow(highlight)/2),rep(+1,nrow(highlight)/2))


p<-p + geom_path(data=as.data.frame(highlight),
                 aes(
                     x=index+Shift,
                     y=match(var, levels(arcs_long$variable))+Offset,
                     group=index,
                     linetype="Recommended"
                 ),
                 
                 color=highlightColor,
                 size=highlightWidth
)

###Movies

highlightEpisodesIndex<-arcs[arcs$Series == "Star Trek", "Index"]

#Remove trailing NA (which is there to make the last row look nice"
highlightEpisodesIndex<-highlightEpisodesIndex[1:length(highlightEpisodesIndex)-1]


highlight<-data.frame(index=rep(highlightEpisodesIndex,2))

highlight$var<-c(
    rep(levels(arcs_long$variable)[1],nrow(highlight)/2),
    rep(levels(arcs_long$variable)[length(levels(arcs_long$variable))],nrow(highlight)/2)
)
highlight$Offset<-c(rep(-1.5,nrow(highlight)/2),rep(+1,nrow(highlight)/2))


p<-p + geom_path(data=as.data.frame(highlight),
                 aes(
                     x=index+Shift,
                     y=match(var, levels(arcs_long$variable))+Offset,
                     group=index,
                    linetype="Movie"
                 ),
                 
                 color=movieColor,
                 size=highlightWidth
)


#Background bars 2

p<-p+ geom_path( 
    data=arcs_long[ arcs_long$value >0,], 
    aes(
        x=as.numeric(factor(Label, levels=arcs_order, ordered = TRUE))+Shift,
        y=factor(variable),
        group=(variable)
    ),
    color="grey50",
    size=1.5
)



#Interlinks
p<-p+ geom_path(data=arcs_long[ arcs_long$value >0,],
                aes(
                    x=as.numeric(factor(Label, levels=arcs_order, ordered = TRUE))+Shift,
                    y=factor(variable),
                    group=Label
                ),
                
                color="grey40",
                linetype=3,
                size=0.5
)

#Loop links

q<-p
for (LOI in as.character(unique(looplink$Label))) #LOI = label of interest
{
    tick_shift_offset<-tick_shift+max(looplink_offsets[looplink_offsets$Label==LOI,]$Offset)
    
    looplink_crossbar<-cbind(looplink[looplink$Label==LOI,],data.frame(Offset=rep(tick_shift_offset,nrow(looplink[looplink$Label==LOI,]))))
    
     q<- q+ geom_path(data=looplink_crossbar,

                     aes(
                         x=as.numeric(index)+Shift,
                         y=max(which(levels(arcs_long$variable) %in% var))+Offset,
                         group=Label
                     ),
                     
                     color="grey70",
                     linetype=5,
                     size=0.75
    )
    
    
    looplink_ticks<-rbind(cbind(looplink[looplink$Label==LOI,],
                                Offset=rep(0,length(looplink[looplink$Label==LOI,"var"]))
    ),
    data.frame(  index=looplink[looplink$Label==LOI,"index"],
                 var=rep(levels(arcs_long$variable)[max(which(levels(arcs_long$variable) %in% looplink[looplink$Label==LOI,"var"]))],length(looplink[looplink$Label==LOI,"var"])),
                 Label=looplink[looplink$Label==LOI,"Label"],
                 Offset=rep(tick_shift_offset,length(looplink[looplink$Label==LOI,"var"]))
    )
    )
    
    looplink_ticks$var_index<-match(looplink_ticks$var, levels(arcs_long$variable)) 
    
    q<-q+ geom_path(data=looplink_ticks[order(looplink_ticks$index),],
                    aes(
                        x=index+Shift,
                        y=match(var, levels(arcs_long$variable))+Offset,
                        group=index
                    ),
                    
                    color="grey70",
                    size=0.75
    )
    
    
    
    looplink_text<-data.frame(index=mean(looplink_ticks[looplink_ticks$Offset==0,"index"]),
                              var=looplink_ticks[looplink_ticks$Offset==0,"var"][1],
                              Label=looplink_ticks[looplink_ticks$Offset==0,"Label"][1],
                              Offset=looplink_ticks[looplink_ticks$Offset==0,"Offset"][1]+max(looplink_offsets[looplink_offsets$Label==LOI,]$Offset),
                              var_index=max(looplink_ticks[looplink_ticks$Offset==0,"var_index"]),
                              xLabel=looplink_offsets[looplink_offsets$Label==looplink_ticks[looplink_ticks$Offset==0,"Label"][1],"xLabel"][1],
                              yLabel=looplink_offsets[looplink_offsets$Label==looplink_ticks[looplink_ticks$Offset==0,"Label"][1],"yLabel"][1]
    )
    
    
    q<-q+ geom_text(data=looplink_text,
                    aes(
                        x=index+Shift+xLabel,
                        y=var_index+Offset+tick_shift+text_shift+yLabel,
                        label=Label
                    ),
                    color="black",
                    size=3
    )
    
}
p<-q

#Solid Circles
p <- p +    
    geom_dotplot(data=arcs_long[arcs_long$value==1,],
                 binaxis="y",
                 aes(fill=factor(Category), color=factor(variable)),
                 color="black",
                 dotsize=chartDotsize
    ) 
#Open circles
p<-p +  
    geom_dotplot(data=arcs_long[arcs_long$value==2,],
                 binaxis="y",
                 aes(color=factor(Category)),
                 fill="white",
                 dotsize=chartDotsize
    )

#Tidy Axis
p <- p +  theme(axis.text.x = element_text(angle = -80, hjust = 0, vjust=0, size=9.5)) + 
    xlab("") + 
    ylab("")  




#Plotp 

qq<-p +scale_fill_manual(values=FillColors)+scale_color_manual(values=FillColors) +labs(fill ="Category")+guides(color=FALSE)

#This is a bit broken, but to add a second fill legend we add a different type
#of legend and force it back into a fill legend.


pdf("timeline.pdf", width=55, height=25,  useDingbats=FALSE)

p + scale_fill_manual(values=FillColors)  +
    labs(fill ="Category", linetype="Vertical Bars") + 
    scale_linetype_manual(values = c(1,1),
                          breaks = c("Recommended","Movie")
                          ) + 
    guides(color=FALSE,
           linetype=guide_legend(override.aes = list(color = c(highlightColor,movieColor))
            ) 
    )
dev.off()

trekMatrix<-data.matrix(arcs[,-1:-5])
rownames(trekMatrix)<-paste(arcs[,4],arcs[,5],arcs[,2])
heatmap(trekMatrix)
