#This is a list of functions that help make visuals



#color vectors
#use when above average is good
cols_pos<-c("Above"="#2686A0","Average"="#747678","Below"="#A36B2B")
#use when above average is bad
cols_neg<-c("Above"="#A36B2B","Average"="#747678","Below"="#2686A0")

# Plots a single data point per group  relative to an "all" line
#input dataframe, and a vector specifying colors 

plot_group_comp<- function(data,color_vector){
   
  ggplot(
    data = data,aes(
      y = group_label,
      x = value
    ))+
    #set point to mark actual value
    geom_point(aes(color=avg_above), size=2)+
    #facet by comparison group - instead of all jumbled together
    facet_grid(comp~., switch="y", scales="free", space="free_y")+
    #group name label - line from "all" to value, and then text
    geom_segment(aes(yend=group_label, xend=all, color=avg_above))+
    geom_text(aes(x=value, y=group_label, label=group_label), size=3, nudge_y=.35)+
    #data value label
    geom_text(aes(x=value, y=group_label, label=scales::percent(value,accuracy=1)), size=3, nudge_y=-.35)+
    #set the all student average
    geom_vline(aes(xintercept=all))+
    scale_color_manual(values=color_vector)+
    theme(
      plot.title=element_text(size=20),
      plot.subtitle=element_text(size=15),
      plot.caption=element_text(size=10),
      axis.text.x=element_text(size=8, face="bold"),
      axis.title.x=element_blank(),
      axis.title.y = element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y= element_blank(),
      #control factet theme
      strip.background = element_blank(),
      #can't get strip text to look good
      strip.text.y = element_blank(),
      panel.border = element_rect(colour="gray75", size=.75, fill=NA),
      panel.spacing = unit(0, "lines"), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background=element_rect(color="white", fill="white"),
      legend.position="none")
  
}

#This comes after previous function and adds better axis formatting
#it offers one way of pre-populating where axis labels and diff rect fall
#ll is lower limit-generally a little lower than lowest value to show
#ul is upper limit- generally ~.05 above the highest value

plot_diff_rect<-function(g,data,ll,ul,stringlow,stringhigh){
  g+
  scale_x_continuous(expand=c(.02,.02), 
                     limits=c(ll, ul),
                     #sets labels for average line, and diff in rectangle as if axis labels
                     breaks=c((ll+.01),data%$% all[1],ul*.85,(ul-.01)), 
  labels=c(stringlow,"Student Average", stringhigh, "Difference from \nAverage"), 
  position="top")+
  scale_y_discrete(expand=c(0,1.2))+
  #build diff box
  #spacing of diff bar is determined on y axis
  geom_rect(data=data, aes(xmin=ul-.02, xmax=ul, ymin=-Inf, ymax=Inf), fill="grey") +
  geom_text(data=data, aes(label=paste0(round(diff*100,0), "%"), 
                           x=ul-.01, 
                           y=group_label, 
                           color=avg_above), fontface="bold", size=3)
}


#This makes data comparing UCSD to other campuses wide so it can be dumbelled
clean_wide_time<-function(data){
  
  data%>%
    select(year, campus,value)%>%
    pivot_wider(names_from=campus, values_from=value)%>%
    rename("All_UC"="All UC")%>%
    mutate(diff=round(UCSD-All_UC, 2))%>%
    mutate(All_UC=case_when(diff==0~UCSD+.001,
                            TRUE~All_UC))%>%
    mutate(year=as.factor(year))%>%
    mutate(year=fct_relevel(year, "2022","2021","2020","2018","2016"))
}

#requires wider data see above    
plot_time_dumbell<-function(data){
   #dumball chart is from package ggcharts
      dumbbell_chart(
          data=data,
           x=year,
           y2=All_UC,
           y1=UCSD,
           point_colors=c("#747678","#00629B"),
           legend=FALSE,
           sort=FALSE)+
             #set axis dimensions 
            
             #topline labels
         geom_text(data=filter(data, year=="2016"), aes(x=year, y=UCSD, label="UCSD"),
                                  color="#00629B", size=3, vjust=-1.5)+
          geom_text(data=filter(data, year=="2016"), aes(x=year, y=All_UC, label="All UC"),
                                    color="#747678", size=3, vjust=-1.5)+
            #Data % Labels
            geom_text(aes(y=UCSD,label= scales::percent(UCSD,accuracy=1)), size=3, nudge_x =-.25)+
          #doesn't show UC all % if there is no difference- prevent label overlap
        geom_text(data=filter(data, diff!=0),aes(y=All_UC,label= scales::percent(All_UC,accuracy=1)), size=3, nudge_x =-.25)+
             
               theme(
                 plot.title = element_text(size=15),
                 axis.title.y  = element_text(size=10),
                  axis.title.x = element_blank(),
                  axis.text.y = element_text(size=10),
                  panel.grid.major.x = element_blank(),
                 panel.grid.major.y = element_blank(),
                  plot.background=element_rect(color="white", fill="white"))
    
     }
#formatting drawn from - https://www.connorrothschild.com/post/dumbbell-plots


  
# plot one dot over time - deprecated? 
plot_time<-function(data){
  
  ggplot(data, aes(x = year, 
                   y = value,
                   group=campus,
                   color=campus)) +
    geom_textline(aes(label=campus), size= 4,hjust=.07) +
    scale_color_manual(values=c( "#747678","#00629B"))+
    geom_label_repel(aes(label= scales::percent(value,accuracy=1)), size=5)+
    scale_y_continuous(labels=scales::percent_format(accuracy=1))+
    theme_classic()+
    theme(
      plot.title = element_text(size=15),
      axis.title.x = element_text(size=15),
      axis.title.y = element_blank(),
      axis.text = element_text(size=15),
      panel.grid = element_blank(),
      legend.position  = "none")
}
  








