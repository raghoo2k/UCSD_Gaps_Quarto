

#comparisons between groups


plot_lollipop_div<-function(data){

  diverging_lollipop_chart(
  data = data,
  x = group_label,
  y = diff,
  lollipop_colors = c("#2686A0","#A36B2B"),
  #text_color = c("#2686A0","#A36B2B"),
  text_size=15,
  point_size = 5)+
  #geom_text_repel(aes(y=diff+.01*sign(diff),label=scales::percent(value,accuracy=1)), size=5)+
    geom_text(aes(y= diff+.02*sign(diff),label=scales::percent(value,accuracy=1)), size=5)+
    facet_grid(comp~., switch="y")+
    theme(
    plot.title=element_text(size=20),
    plot.subtitle=element_text(size=15),
    plot.caption=element_text(size=10),
    axis.title.y = element_blank(),
    #control factet theme
    strip.background = element_blank(),
    #can't get strip text to look good
    strip.text.y = element_blank(),
    panel.border = element_rect(colour="gray75", size=.75, fill=NA),
    panel.spacing = unit(0, "lines"), 
    plot.background=element_rect(color="white", fill="white"),
    legend.position="none")
}



#set the the geom text label to be offset slightly from datapoint- lines originate from all student average
# geom_textsegment(aes(yend=group_label, xend=all, label=paste(group_label)),
#hjust=-.000075, textcolour="black")+
#                 hjust=-.00005, textcolour="black")+

# lollipo format with text labels inline

plot_text_div<- function(data){
  ggplot(
    data = data,aes(
      y = group_label,
      x = value
    ))+
    #background line
    #geom_segment(data=data, aes(y=group_label, yend=group_label, x=.35, xend=.60),
                # color="#b2b2b2", size=0.15)+
    #set point to mark actual value
    geom_point(aes(color=avg_above), size=2)+# Make a long line
    #facet by comparison group - instead of all jumbled together
    facet_grid(comp~., switch="y", scales="free", space="free_y")+
    #group name label
    geom_segment(aes(yend=group_label, xend=all, color=avg_above))+
    geom_text(aes(x=value, y=group_label, label=group_label), size=3, nudge_y=.35)+
    #data label
    geom_text(aes(x=value, y=group_label, label=scales::percent(value,accuracy=1)), size=3, nudge_y=-.35)+
    #set the all student average
    geom_vline(aes(xintercept=all))+
    scale_color_manual(values=c("#A36B2B", "#2686A0"))+
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

#geom_text(data=filter(data, group_label=="Pell Grant Recipient"),
#aes(x=.625, y=group_label, label="Difference from '\n'Average"),
#color="black", size=3.1, vjust=-2, fontface="bold")+


# plot one dot over time
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


  
  
  




### Dividing polots- 

#after manually ordering response levels and assigning negative v positive

make_div<-function(data)
{
  data%>%
    mutate(div=case_when(vibe=="negative"~value*-1,
                         TRUE~value))%>%
    mutate(labels=round((value*100),0))%>%
    #need to add this to keep the responses ordered correctly 
    arrange(response)}

g_div<-function(data){
  ggplot(data=data,
         aes(y=group,
             x=div,
             fill=response,
             label=paste0(labels, "%")))+
    geom_col()+
    geom_fit_text(position="stack",show.legend = F)+
    scale_y_discrete(limits=rev)+
    guides(fill=guide_legend(nrow=1))+
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          axis.text=element_text(size=20),
          legend.position = "top")  
  
  
}
# Diverging color palate
#palette definition
#earth<-divergingx_hcl("Earth", n=6)


#3-is more severe
#pos3<-earth[6]
#pos2<-earth[5]
#pos1<-earth[4]
#neg3<-earth[1]
#neg2<-earth[2]
#neg1<-earth[3]

# Old Dot Plot

g_dotperc<-function(data){
  ggplot(data,
         aes(y = reorder(comp,desc(comp)), 
             x = value,
             color=comp,
             label=paste(group," \n ",paste0(perc, "%"))))+ 
    geom_point(size=2)+
    geom_line(aes(group = comp, color=comp)) +
    scale_color_manual(values=met.brewer("Veronese",5))+
    geom_text(size=7)+
    theme(axis.title.y = element_blank(),
          axis.title.x = element_text(size=20),
          axis.text=element_text(size=20),
          panel.grid.major = element_line(size=1),
          plot.caption=element_text(size=10),
          legend.position="none")
}






