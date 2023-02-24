# all data cleaning functions


#standardize order of groups in visuals - remove All, and only URG race
order_comp_group_urg<- function(data){
  data%>%
    filter(group!="All")%>%
    filter(comp!="Race")%>%
    mutate(comp=as_factor(comp))%>%
    mutate(comp=fct_relevel(comp,"Gender","Pell","First Gen","URG"))%>%
    mutate(group=as_factor(group))%>%
    mutate(group=fct_relevel(group,"Men","Women","Pell","Non-Pell","First Gen","Not First Gen", "URG", "Not URG"))
  
}




order_comp_group_all<-function(data){
  data%>%
    #new gen seems wrong? too moderate? and then Domestic unknown is clutter
    filter(comp!="New Gen"& comp!="All"& comp!="URG")%>%
    filter(group!="Domestic Unknown")%>%
    mutate(comp=as_factor(comp))%>%
    mutate(comp=fct_relevel(comp, "Pell","First Gen","Race"))%>%
    mutate(group=as_factor(group))%>%
    mutate(group=fct_relevel(group, "Pell","Non-Pell","First Gen","Not First Gen", "African American", "Hispanic/Latinx",	"American Indian","Asian/Pac Isl","White","International"))
   
}

#add nicer label text for groups
label_groups<-function(data){
  data%>%
  mutate(group_label=fct_recode(group,
                                "Not Pell Grant Recipient"="Non-Pell",
                                "Asian"="Asian/Pac Isl",
                                "Pell Grant Recipient"="Pell",
                                "First-Generation"="First Gen",
                                "Not First-Generation"="Not First Gen"))
}


#Heterogeniety in responses and number of levels makes it hard to automate

#standardize order of groups in visuals
order_comp_group_outofdate<- function(data){
  data%>%
    #new gen seems wrong? too moderate? and then Domestic unknown is clutter
    filter(comp!="New Gen")%>%
    filter(group!="Domestic Unknown")%>%
    mutate(comp=as_factor(comp))%>%
    mutate(comp=fct_relevel(comp, "Pell","First Gen","Race"))%>%
    mutate(group=as_factor(group))%>%
    #divide URG and not URG
    mutate(group_abv=fct_collapse(group,
                                  URG=c("African American", "Hispanic/Latinx",	"American Indian"),
                                  "Not URG"=c("Asian/Pac Isl","White","International")))%>%
    
    mutate(group=fct_relevel(group, "Pell","Non-Pell","First Gen","Not First Gen", "African American", "Hispanic/Latinx",	"American Indian","Asian/Pac Isl","White","International"))%>%
    mutate(group_abv=fct_relevel(group_abv, "Pell","Non-Pell","First Gen","Not First Gen", "URG","Not URG"))
}

#Code Factor Levels in response
clean_vibe_agree<-function(data){
  data%>%
  mutate(repsonse=as_factor(response))%>%
    mutate(response=fct_relevel(response, "Strongly Agree","Agree","Somewhat Agree","Strongly Disagree", "Disagree","Somewhat Disagree"))%>%
    mutate(vibe=fct_collapse(response,
                             negative=c("Somewhat Disagree","Disagree","Strongly Disagree"),
                             positive=c("Somewhat Agree","Agree","Strongly Agree")))
  
}

#Code Factor Levels in response
clean_vibe_agree_neg<-function(data){
  data%>%
    mutate(repsonse=as_factor(response))%>%
    mutate(response=fct_relevel(response, "Strongly Agree","Agree","Somewhat Agree","Strongly Disagree", "Disagree","Somewhat Disagree"))%>%
    mutate(vibe=fct_collapse(response,
                             positive=c("Somewhat Disagree","Disagree","Strongly Disagree"),
                             negative=c("Somewhat Agree","Agree","Strongly Agree")))
  
}

#Code Factor Levels in response
clean_vibe_satisfied<-function(data){
  data%>%
    mutate(repsonse=as_factor(response))%>%
    mutate(response=fct_relevel(response, "Very Satisfied","Satisfied","Somewhat Satisfied","Very Dissatisfied", "Dissatisfied","Somewhat Dissatisfied"))%>%
    mutate(vibe=fct_collapse(response,
                             negative=c("Very Dissatisfied", "Dissatisfied","Somewhat Dissatisfied"),
                             positive=c("Very Satisfied","Satisfied","Somewhat Satisfied")))
  
}


clean_vibe_foodsecure<-function(data){
  data%>%
    mutate(repsonse=as_factor(response))%>%
    mutate(response=fct_relevel(response,"Very low food security","Low food security","Food insecure", "Food security"))%>%
    mutate(vibe=fct_collapse(response,
                             negative=c("Very low food security","Low food security","Food insecure","insecure"),
                             positive=c("Food security")))
  
}

clean_vibe_true3<-function(data){
  data%>%
    mutate(repsonse=as_factor(response))%>%
    mutate(response=fct_relevel(response,"Often True","Sometimes True","Never True"))%>%
    mutate(vibe=fct_collapse(response,
                             negative=c("Often True","Sometimes True"),
                             positive=c("Never True")))
  
}
clean_vibe_freq<- function(data){
  data%>%
    mutate(repsonse=as_factor(response))%>%
    mutate(response=fct_relevel(response, "Never","Rarely","Occasionally","Very Often",
                                "Often","Somewhat Often"))%>%
    mutate(vibe=fct_collapse(response,
                             negative=c("Very Often",
                                        "Often","Somewhat Often","Occasionally"),
                             positive=c("Never","Rarely")))
}

clean_vibe_freq5<- function(data){
  data%>%
    mutate(repsonse=as_factor(response))%>%
    mutate(vibe=fct_collapse(response,
                             negative=c("All the Time","Frequently","Occasionally"),
                             positive=c("Never","Rarely")))
}

#Code Factor Levels in response
clean_vibe_rate<-function(data){
  data%>%
    mutate(repsonse=as_factor(response))%>%
    mutate(vibe=fct_collapse(response,
                             negative=c("Very Poor", "Poor","Fair"),
                             positive=c("Excellent","Very Good","Good")))
  
}




collapse_positive_vibe<-function(data){
  data%>%
  group_by(comp, group_label, vibe)%>%
    summarize(value=sum(value, na.rm=T),
              N=sum(N, na.rm=T))%>%
    #just show the positive responses
    filter(vibe=="positive")%>%
    mutate(perc=round(value*100))
}

collapse_negative_vibe<-function(data){
  data%>%
    group_by(comp, group_label, vibe)%>%
    summarize(value=sum(value, na.rm=T),
              N=sum(N, na.rm=T))%>%
    #just show the negative responses
    filter(vibe=="negative")%>%
    mutate(perc=round(value*100))
}



#Calculate diff fields 
create_diff_cols<-function(data){
  data%>%
rowwise(.)%>%
  mutate(diff=value-all)%>%
  ungroup(.)%>%
  mutate(avg_above=case_when(
    value>all~"Above",
    value<all~"Below",
    TRUE~"Average"))%>%
    mutate(diff=case_when(
      avg_above=="Average"~diff+.001,
      TRUE~diff
    ))%>%
  ungroup(.)%>%
  mutate(group_label=fct_reorder(group_label, diff))
  
}





