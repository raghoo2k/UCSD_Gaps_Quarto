
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

# plot one dot over time
plot_time<-function(data){
  
  ggplot(data, aes(x = year, 
                   y = value,
                   group=campus,
                   color=campus,
                   label= scales::percent(value,accuracy=1))) +
    geom_line(size = 1) +
    scale_color_manual(values=c( "#747678","#00629B"))+
    #geom_point(size=2)+
    geom_label_repel(size=5)+
    scale_y_continuous(labels=scales::percent_format(accuracy=1))+
   
    theme_classic()+
    theme(
      plot.title = element_text(size=15),
      axis.title.x = element_text(size=15),
      axis.title.y = element_blank(),
      axis.text = element_text(size=15),
      panel.grid = element_blank()
    )

}




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






