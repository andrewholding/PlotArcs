
setwd("~/Desktop/plot arcs R")
arcs<-read.csv("TOS.csv",stringsAsFactors = TRUE)

looplink<-data.frame(index=c(3,4,7,8,9),var=c("Tribbles","Spock","Spock", "Kirk","Spock"),Label=c("Interesting","Interesting","Interesting","Love","Love"))


library(reshape2)
arcs_long<-melt(arcs, id.vars=c("Index","Series","Episode","Name","Stardate"))

arcs_long$Label<-paste0(arcs_long$Series," (",arcs_long$Episode,") ",arcs_long$Name)

#Set up colours
library(RColorBrewer)
mycolors<-colorRampPalette(brewer.pal(12, "Set3"))
FillColors<-mycolors(length(levels(arcs_long$variable)))
names(FillColors)<-as.character(levels(arcs_long$variable))

#Will correct vertical bar positions, but only for the set size
Shift<-2.40/dev.size("cm")[1]

arcs_order <- paste0(arcs$Series," (",arcs$Episode,") ",arcs$Name)

#Start Plot
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
              legend.position="none")
#Background Bars (for some reason removing the geom_line breaks it)
p <-p+  geom_line( 
    data = arcs_long, 
    aes(
        x=factor(Label, levels=arcs_order, ordered = TRUE),
        y=factor(variable),
        group=(variable)
    ),
    color="grey30",
    size=0.1
)

p<-p+ geom_path( 
    data=arcs_long[ arcs_long$value >0,], 
    aes(
        x=as.numeric(factor(Label, levels=arcs_order, ordered = TRUE))+Shift,
        y=factor(variable),
        group=(variable)
    ),
    color="grey",
    size=1.5
)

#Loop links
tick_shift <- 0.35 #How far ticks are from the main line
text_shift <- 0.35 #How far text is from the main line

q<-p
for (LOI in as.character(unique(looplink$Label))) #LOI = label of interest
{
   q<- q+ geom_path(data=looplink[looplink$Label==LOI,],
                 aes(
                     x=as.numeric(index)+Shift,
                     y=max(which(levels(arcs_long$variable) %in% var))+tick_shift,
                     group=Label
                 ),
                 
                 color="black",
                 size=0.75
    )
   
   
   looplink_ticks<-rbind(cbind(looplink[looplink$Label==LOI,],
                               Offset=rep(0,length(looplink[looplink$Label==LOI,"var"]))
                               ),
                            data.frame(  index=looplink[looplink$Label==LOI,"index"],
                                         var=rep(levels(arcs_long$variable)[max(which(levels(arcs_long$variable) %in% looplink[looplink$Label==LOI,"var"]))],length(looplink[looplink$Label==LOI,"var"])),
                                         Label=looplink[looplink$Label==LOI,"Label"],
                                         Offset=rep(tick_shift,length(looplink[looplink$Label==LOI,"var"]))
                                         )
                            )
   
   looplink_ticks$var_index<-match(looplink_ticks$var, levels(arcs_long$variable)) 
   
    q<-q+ geom_path(data=looplink_ticks[order(looplink_ticks$index),],
                 aes(
                     x=index+Shift,
                     y=match(var, levels(arcs_long$variable))+Offset,
                     group=index
                 ),
                 
                 color="black",
                 size=0.75
    )
    
    looplink_text<-data.frame(index=mean(looplink_ticks[looplink_ticks$Offset==0,"index"]),
                             var=looplink_ticks[looplink_ticks$Offset==0,"var"][1],
                             Label=looplink_ticks[looplink_ticks$Offset==0,"Label"][1],
                             Offset=looplink_ticks[looplink_ticks$Offset==0,"Offset"][1],
                             var_index=max(looplink_ticks[looplink_ticks$Offset==0,"var_index"])
                                )
    
    
    q<-q+ geom_text(data=looplink_text,
                    aes(
                        x=index+Shift,
                        y=var_index+Offset+tick_shift+text_shift,
                        label=Label
                    ),
                    color="black",
                    size=3
    )
    
}
p<-q


#Interlinks
p<-p+ geom_path(data=arcs_long[ arcs_long$value >0,],
             aes(
                 x=as.numeric(factor(Label, levels=arcs_order, ordered = TRUE))+Shift,
                 y=factor(variable),
                 group=Label
             ),
             
             color="grey70",
             size=0.8
)


#Solid Circles
p <- p +    
        geom_dotplot(data=arcs_long[arcs_long$value==1,],
            binaxis="y",
            aes(fill=factor(variable), color=factor(variable)),
            color="black"
        ) 
#Open circles
p<-p +  
    geom_dotplot(data=arcs_long[arcs_long$value==2,],
        binaxis="y",
        aes(color=factor(variable)),
        fill="white"
    )

#Tidy Axis
p <- p +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    xlab("") + 
    ylab("")  




#Plotp 

p +scale_fill_manual(values=FillColors)+scale_color_manual(values=FillColors)
