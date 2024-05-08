#0. Initiation###############
#if (!require("BiocManager", quietly = TRUE))
#install.packages("BiocManager")

##1) Required libraries###############
required.packages=c("ggplot2","dplyr","hrbrthemes","viridis","EnvStats","reshape2",
                    "ggforce","ComplexUpset","ggsignif","gridExtra","ggpubr","rstatix",
                    "pROC","flexmix","tidyr","tune","mgcv","boot")

#Don't know if needed:"ggsignif"
for (package in required.packages){
  #(if (!require(i)) biocLite(i))
  print(paste('Checking:',package))
  if(!require(package,character.only = T)){
    print(paste("INSTALLING: "))
    install.packages(package)
    BiocManager::install(package)
  }
 else{print("PASS")}
}
for (package in required.packages){
  library(package,character.only = T)
  print(paste('Loaded:', package))
}

##2) data import############

#wd<-getwd()
wd<-"C:/Users/renbo/OneDrive/Desktop/Manuscript writing/Data analysis/R"
setwd(wd)
files<-list.files(getwd())
files

data.file<-"raw_data/Trauma_clean_update_20240320.csv"
data.file

data<-read.table(data.file,header = T,sep = ",")
row.names(data) = data[,1]
data<-data[,-1]

##Important ICU stay time processing==============
# round up to integer days! i.e. 1.3 days-->2days


data[c(1:48),116]=sapply(data[c(1:48),55],function(x){
  if(x<=median(data[c(1:48),55])){"short_stay"}else{"long_stay"}
})
data[c(1:48),117]=sapply(data[c(1:48),55],function(x){
  if(x<=median(data[c(1:48),55])){0}else{1}
})
data[,117]<-as.numeric(data[,117])
colnames(data)[c(116)]<-"hospital.stay.length"
colnames(data)[c(117)]<-"hospital.stay.length_binary"




#called ICU_days

meta.CI.file<-"raw_data/Metadata_Tauma_Clinical_2024.csv"
meta.CI.file
meta.CI<-read.table(meta.CI.file,header = T,sep = ",")


#data.meta<-read.table(f.meta,header = F,sep = ",")


##3) color palettes################
##https://r-charts.com/color-palettes/


  #Control vs trauma
  color.panel<-c("#e0ecf4","#b34d4d")
  #Control, less_severe,major
  color.panel1<-c("#e0ecf4","#A0CBE8","#b34d4d")
  #Less severe vs major
  color.panel2<-c("#A0CBE8","#b34d4d")
#Other fixed palettes:
color.panel3<-c("#b34d4d","#5695A0","#E59DAA","#697983","#9E6B72")
color.panel4<-c("#E69F00","#56B4E9","#009E73","#D55E00","#697983")
color.panel5<-c("#49525E")
#color.panel5<-c("#c1272d","#0000a7","#eecc16","#008176","#b3b3b3")
#Reference color palette(not in use)
#cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##4) functions
fn_plot_mediator_indiviual <- function(mediator){
  fill.No=56
  x.No=56
  y.No=mediator
  ggplot(data[data$Group %in% c("Trauma","Control"),],
         aes_string(x=colnames(data)[x.No],y=colnames(data)[y.No])
  )+ 
    geom_violin(aes_string(fill=colnames(data)[fill.No]),
                color="black",size=0.05,scale = "width")+
    geom_jitter(width = 0.05,size=0.2)+
    scale_fill_manual(values = color.panel1)+
    #statistics
    #t.test default is Welch's t-test, which is best fit.
    #remember to put x,y in aes of ggplot, not under the geom_plot.
    #use this aes(label=after_stat(p.signif)) if want asterisk instead of p-value (p.format).
    geom_pwc(method = "t_test",
             p.adjust.method = "none",
             hide.ns = T,
             label = "p.signif",
             vjust=0.5)+
    
    theme_classic()+
    theme(legend.position = "none",axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12)
    )+
    #show sample size, for troubleshooting
    #stat_n_text(aes_string(x=colnames(data)[x.No],y=colnames(data)[y.No]),vjust = 0)+
    xlab ("")
}
fn_plot_clinical_indiviual <- function(clinical){
  fill.No=56
  x.No=56
  y.No=clinical
  
  meta.No <- y.No - 38
  print (meta.CI[meta.No,1]==colnames(data)[y.No])
  #reference
  ref.low=meta.CI[meta.No,3]
  ref.high=meta.CI[meta.No,4]
  ref.severe=meta.CI[meta.No,5]
  
  lowest.data=min (min(data[,y.No],na.rm=T),meta.CI[meta.No,3],na.rm=T)
  largest.data=max (max(data[,y.No],na.rm=T),meta.CI[meta.No,4],meta.CI[meta.No,5],na.rm = T)
  #print(c(lowest.data,largest.data))
  
  ggplot(data[data$Group %in% c("Trauma"),],aes_string(x=colnames(data)[x.No],y=colnames(data)[y.No]))+
    annotate("rect", xmin=-Inf,xmax=+Inf,ymin=ref.low,ymax=ref.high, alpha=0.2, fill=color.panel4[3])+
    #annotate("rect", xmin=-Inf,xmax=+Inf,ymin=ref.high,ymax=ref.severe, alpha=0.2, fill=color.panel3[3])+
    geom_hline(yintercept=c(ref.low), linetype="longdash",color=color.panel4[3])+
    geom_hline(yintercept=c(ref.high), linetype="longdash",color=color.panel3[3])+
    geom_hline(yintercept=c(ref.severe), linetype="longdash",color=color.panel3[1])+
    geom_violin(aes_string(fill=colnames(data)[fill.No]),
                color="black",size=0.05,
                #scale = "width"
                #width scale not enabled in CI, only in Mediator panel.
    )+
    geom_jitter(width = 0.05,size=0.2)+
    
    scale_fill_manual(values = color.panel2)+
    
    geom_pwc(method = "t_test",
             p.adjust.method = "none",
             hide.ns = T,
             #p, p.adj or p.signif
             label = "p.signif",
             vjust=0.5
    )+
    
    theme_classic()+
    theme(legend.position = "none",
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12)
    )+
    #scale_y_log10()+
    ylab(paste(colnames(data)[y.No],"(",meta.CI[meta.No,2],")")) +
    #coord_cartesian(ylim = c((lowest.data+0.2)*0.8,largest.data*1.3))+
    xlab ("")
}
fn_plot_Biomarker_ROC1<- function(PREDICTOR_COL,RESPONSE_COL,SAMPLE_RANGE,SEN,SPEC,RESP_TYPE){
  
  ##For debug use
  #SEN=70
  #SPEC=80
  TPP=SEN
  FPP=100-SPEC
  #PREDICTOR_COL=5
  #RESPONSE_COL=57
  RES_GR1="Trauma"
  RES_GR2="Control"
  RESP=c("binomial","gaussian","poisson")[RESP_TYPE]
  #SAMPLE_RANGE=c(1:72)
  
  print (paste("Predictor:",colnames(data)[PREDICTOR_COL],"Boolean responder:"))
  print (paste("Boolean responder:",colnames(data)[RESPONSE_COL]))
  print (paste("Sample size:",length(SAMPLE_RANGE)))
  
  ##a) Continuous x data (biomarker)
  Biomarker_exp <- data[,PREDICTOR_COL][SAMPLE_RANGE] 
  Biomarker_exp
  
  ##b) Known classification data(0 and 1). discrete.
  Responder<-data[,RESPONSE_COL][SAMPLE_RANGE]
  
  Responder[Responder==RES_GR1] <-1
  Responder[Responder==RES_GR2] <-0
  Responder <- sapply(Responder, as.numeric)
  #Responder
  
  #plot(x=Biomarker_exp,y=Responder)
  
  
  ##c) logistic regression to get the probability of y in each x position.
  #use glm to fit a logistic regression curve to the data
  glm.fit=glm(Responder ~ Biomarker_exp, family=RESP)
  #binomial, gaussian and poisson (count data) for the family choice of the responder.
  #rf.fit=randomForest(factor(Responder) ~ Biomarker_exp)
  
  #==>The probability will be stored in the fitted.value.
  #pass weight and the fitted.values stored in glm.fit into the lines() function
  #Add connected line segments to a plot
  #so that it draw a curve that tells us the predicted **probability** that an individual is obeses or not obese.
  #lines(Biomarker_exp, glm.fit$fitted.values)
  #lines(Biomarker_exp, rf.fit$votes[,1])
  
  #glm.fit$fitted.values contains estimated probabilities that each sample is obese.
  
  
  ##d) plot ROC curve
  ##==>ROC is about discrete outcome vs probability of it.
  
  ROC.data.ori<-roc(Responder ~ Biomarker_exp,quiet=T)
  
  #Pass known classifications for eachTotal.P.df#Pass known classifications for each sample, and the probability that each sample is group 1 or 2
  
  #Formatted version
  par(pty="square")
  roc(Responder ~ Biomarker_exp,plot=T,legacy.axes=T, percent=F, 
      xlab="False Positive ",ylab="True Positive ",
      col=color.panel3[2],lwd=2,quiet=T)
  text(x=0.25,y=0.10,label=paste("AUC=",round(ROC.data.ori$auc,2)))
  title(paste(colnames(data)[PREDICTOR_COL]),cex.main= 1.3,line = 3)
  
  par(pty="maximum")
  
  
  ###To get the range of threshold that achieves a specific range of sen/spec.
  ###in a specific part of the plot (i.e. specific range of sensitivity and specificity)
  #in ROC.data
  
  ROC.meta.ori<-data.frame(sensitivity=ROC.data.ori$sensitivities*100,
                           specificity=ROC.data.ori$specificities*100,
                           tpp=ROC.data.ori$sensitivities*100,
                           fpp=(1-ROC.data.ori$specificities)*100,
                           thresholds=ROC.data.ori$thresholds
  )
  
  head(ROC.meta.ori)
  
  
  
  Threshold<-ROC.meta.ori[ROC.meta.ori$tpp>TPP & ROC.meta.ori$fpp<FPP,]
  Threshold_range<-range(Threshold$thresholds)
  
  #Adding the range of predictor to a list.
  ##use <<- to make it updated outside the function.
  rangelist[[(PREDICTOR_COL-Range_of_predictors[1]+1)]][1] <<- colnames(data)[PREDICTOR_COL]
  rangelist[[(PREDICTOR_COL-Range_of_predictors[1]+1)]][2] <<- Threshold_range[1]
  rangelist[[(PREDICTOR_COL-Range_of_predictors[1]+1)]][3] <<- Threshold_range[2]
  #rangelist[1]
  print(paste("Expression range of biomarkers with sensitivity of",SEN,"% and specificity of",SPEC,"% to predict the outcome is:",Threshold_range[1],"to",Threshold_range[2]),sep="")
  print("")
  
}
fn_plot_Biomarker_ROC2<- function(PREDICTOR_COL,RESPONSE_COL,SAMPLE_RANGE,SEN,SPEC,RESP_TYPE){
  
  ##For debug use
  #SEN=70
  #SPEC=80
  TPP=SEN
  FPP=100-SPEC
  #PREDICTOR_COL=5
  #RESPONSE_COL=57
  RES_GR1="Major"
  RES_GR2="Less_Severe"
  RESP=c("binomial","gaussian","poisson")[RESP_TYPE]
  #SAMPLE_RANGE=c(1:72)
  
  print (paste("Predictor:",colnames(data)[PREDICTOR_COL],"Boolean responder:"))
  print (paste("Boolean responder:",colnames(data)[RESPONSE_COL]))
  print (paste("Sample size:",length(SAMPLE_RANGE)))
  
  ###a) Continuous x data (biomarker)#######
  Biomarker_exp <- data[,PREDICTOR_COL][SAMPLE_RANGE] 
  Biomarker_exp
  
  ###b) Known classification data(0 and 1). discrete.##########
  Responder<-data[,RESPONSE_COL][SAMPLE_RANGE]
  
  Responder[Responder==RES_GR1] <-1
  Responder[Responder==RES_GR2] <-0
  Responder <- sapply(Responder, as.numeric)
  #Responder
  
  #plot(x=Biomarker_exp,y=Responder)
  
  
  ###c) logistic regression to get the probability of y in each x position.##########
  #use glm to fit a logistic regression curve to the data
  glm.fit=glm(Responder ~ Biomarker_exp, family=RESP)
  #regression.summary<-list()
  #regression.summary[[2]]<-summary(glm.fit)
  
  #write.csv(summary(glm.fit),file = paste(colnames(data)[PREDICTOR_COL],"predictability_to",colnames(data)[RESPONSE_COL],".csv"))
  
  #binomial, gaussian and poisson (count data) for the family choice of the responder.
  #rf.fit=randomForest(factor(Responder) ~ Biomarker_exp)
  
  #==>The probability will be stored in the fitted.value.
  #pass weight and the fitted.values stored in glm.fit into the lines() function
  #Add connected line segments to a plot
  #so that it draw a curve that tells us the predicted **probability** that an individual is obeses or not obese.
  #lines(Biomarker_exp, glm.fit$fitted.values)
  #lines(Biomarker_exp, rf.fit$votes[,1])
  
  #glm.fit$fitted.values contains estimated probabilities that each sample is obese.
  
  
  ###d) plot ROC curve######
  ##==>ROC is about discrete outcome vs probability of it.
  
  ROC.data.ori<-roc(Responder ~ Biomarker_exp,quiet=T)
  
  #Pass known classifications for eachTotal.P.df#Pass known classifications for each sample, and the probability that each sample is group 1 or 2
  
  #Formatted version
  par(pty="square")
  roc(Responder ~ Biomarker_exp,plot=T,legacy.axes=T, percent=F, 
      xlab="False Positive ",ylab="True Positive ",
      col=color.panel3[1],lwd=2,quiet=T)
  text(x=0.25,y=0.10,label=paste("AUC=",round(ROC.data.ori$auc,2)))
  title(paste(colnames(data)[PREDICTOR_COL]),cex.main= 1.3,line = 3)
  
  par(pty="maximum")
  
  
  ###To get the range of threshold that achieves a specific range of sen/spec. 
  ###in a specific part of the plot (i.e. specific range of sensitivity and specificity)
  #in ROC.data
  
  ROC.meta.ori<-data.frame(sensitivity=ROC.data.ori$sensitivities*100,
                           specificity=ROC.data.ori$specificities*100,
                           tpp=ROC.data.ori$sensitivities*100,
                           fpp=(1-ROC.data.ori$specificities)*100,
                           thresholds=ROC.data.ori$thresholds
  )
  
  write.csv(ROC.meta.ori,file = paste(colnames(data)[PREDICTOR_COL],"&",colnames(data)[RESPONSE_COL],".csv"))
  
  
  
  Threshold<-ROC.meta.ori[ROC.meta.ori$tpp>TPP & ROC.meta.ori$fpp<FPP,]
  Threshold_range<-range(Threshold$thresholds)
  
  #Adding the range of predictor to a list.
  ##use <<- to make it updated outside the function.
  rangelist[[(PREDICTOR_COL-Range_of_predictors[1]+1)]][1] <<- colnames(data)[PREDICTOR_COL]
  rangelist[[(PREDICTOR_COL-Range_of_predictors[1]+1)]][2] <<- Threshold_range[1]
  rangelist[[(PREDICTOR_COL-Range_of_predictors[1]+1)]][3] <<- Threshold_range[2]
  #rangelist[1]
  print(paste("Expression range of biomarkers with sensitivity of",SEN,"% and specificity of",SPEC,"% to predict the outcome is:",Threshold_range[1],"to",Threshold_range[2]),sep="")
  print("")
  
}
fn_plot_4days_ci<-function(y.No){ 
  #Debug use only
  #clinical:CI1: 39~53
  #y.No=49
  
  SAMPLE_RANGE=c(1:48)
  #Metadata ID
  meta.No <- y.No - 38
  #CI name
  colnames(data)[y.No]
  
  #Severity grouping
  y.sev=data[SAMPLE_RANGE,56]
  #Day1
  y.1=data[SAMPLE_RANGE,y.No]
  #Day2
  y.2=data[SAMPLE_RANGE,y.No+31]
  #Day3
  y.3=data[SAMPLE_RANGE,y.No+46]
  #Day4
  y.4=data[SAMPLE_RANGE,y.No+61]
  #combined data frame.
  y.4days.df<-data.frame(day=c(1:4),rbind(y.1,y.2,y.3,y.4))
  
  y.4days.df.l<-y.4days.df %>% pivot_longer(cols = c(2:49),names_to = "Sample_ID",values_to = colnames(data)[y.No])
  y.4days.df.l<-data.frame(y.4days.df.l,severity=rep(y.sev,4))
  
  
  ref.low=meta.CI[meta.No,3]
  ref.high=meta.CI[meta.No,4]
  ref.severe=meta.CI[meta.No,5]
  
  
  ####extract data with at least two samples for t-test in ggplot
  #y specific!
  days=c(1,2,3,4)
  day.list<-list()
  for (i in days)
  {
    day.t=i
    day.list[[i]]<-sum(!is.na(subset(y.4days.df.l,(day %in% day.t) & (severity %in% "Major"))[,3]))>1 & 
      sum(!is.na(subset(y.4days.df.l,(day %in% day.t) & (severity %in% "Less_Severe"))[,3]))>1
  }
  #days[c(unlist(day.list))]
  subdata<-subset(y.4days.df.l,day %in% days[c(unlist(day.list))])
  ###
  
  ggplot(y.4days.df.l,aes_string(x="day",y=colnames(data)[y.No],
                                 group="severity"))+
    
    
    annotate("rect", xmin=-Inf,xmax=+Inf,ymin=ref.low,ymax=ref.high, alpha=0.2, fill=color.panel4[3])+
    #annotate("rect", xmin=-Inf,xmax=+Inf,ymin=ref.high,ymax=ref.severe, alpha=0.2, fill=color.panel3[3])+
    geom_hline(yintercept=c(ref.low), linetype="longdash",color=color.panel4[3],alpha=0.5)+
    geom_hline(yintercept=c(ref.high), linetype="longdash",color=color.panel3[3],alpha=0.5)+
    geom_hline(yintercept=c(ref.severe), linetype="longdash",color=color.panel3[1],alpha=0.5)+
    #mean data line graph
    stat_summary(geom="line",fun="mean",size=1,aes(color=severity))+
    #Here is what the ggplot2 book on page 83 says about mean_cl_boot(): Mean Standard error 
    stat_summary(geom="errorbar", fun.data=mean_cl_boot, width = 0.1,size=0.5, 
                 aes(color=severity,alpha=0.5)) + 
    scale_color_manual(values = color.panel1[c(2,3)])+
    #add statistics: Welch's t-test
    #just using data with at least two samples for t-test! Subdata
    stat_compare_means(data=subdata,
                       aes(group = severity), label = "p.signif",
                       label.y = rep(customized_range.list[[y.No-38]][2]*0.80+customized_range.list[[y.No-38]][1]*0.20,4),
                       na.rm=T,
                       hide.ns = T,
                       paired = F,
                       method = "t.test",
                       size=6,
                       #vjust=V
    )+
    theme(legend.position = "none",
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12)
          
    )+
    
    ylab(paste(colnames(data)[y.No],"(",meta.CI[meta.No,2],")"))
  
  # coord_cartesian(ylim = customized_range.list[[y.No-38]])+
  
  
  
}
fn_fit_score<-function(training,validation,responser){ 
  #Age
  Training_metric_1<-mean(data[,1][c(sort(training))])
  Validation_metric_1<-mean(data[,1][c(sort(validation))])
  
  #Sex
  Training_Female.No<-table(data[,2][c(sort(training))])[1]
  Validation_Female.No<-table(data[,2][c(sort(validation))])[1]
  #% of Female
  Training_Female_perc=Training_Female.No/training_size*100
  Validation_Female_perc=Validation_Female.No/(48-training_size)*100
  Training_metric_2<-unname(Training_Female_perc)
  Validation_metric_2<-unname(Validation_Female_perc)
  
  #Range of $responser in training and validation should be similar! Otherwise, it's like interpolate data out of standard curve!
  Training_metric_3<-mean(data[,responser][c(sort(training))])
  #ISS for training
  data[,responser][c(sort(training))]
  
  #responser (responser should be in the range: validation in the range of training set)
  a<-range(data[,responser][training])
  b<-range(data[,responser][validation])
  a
  b
  
  if ((b[2] < a[2] ) & (b[1]>a[1]))
  {
    metric_3=abs((b[1]-a[1]))+abs((a[2]-b[2]))
  } else {
    #Penalty score *5
    metric_3=abs((b[1]-a[1]))+abs((a[2]-b[2]))*5
  }
  
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
  #Final score (lower the better)#
  ##Formula can be modified here##
  Importance_of_Age_match=1
  Importance_of_Sex_match=1
  Importance_of_responser_match=1
  
  abs(Training_metric_1-Validation_metric_1)*Importance_of_Age_match+
    abs(Training_metric_2-Validation_metric_2)*Importance_of_Sex_match+
    abs(metric_3)*Importance_of_responser_match
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
  
}
fn_fit_split<-function(training_size,sample_range,responser,permutation){
  for (i in 1:permutation)
  {
    ##for debug only
    #i=2
    training_size<<-training_size
    responser<<-responser
    #initiation
    if (i==1){
      
      #sample_range=c(1:48)
      #training_size=38
      #permutation=50000
      
      training.list<<-list()
      validation.list<<-list()
      score.list<<-list()
      #best
      training.best.list<<-list()
      validation.best.list<<-list()
      score.best.list<<-list()
      #for plot
      score.list_running<<-list()
      #ini threshold
      score_threshold=200
      u=1
    }
    
    training <- sort(sample(sample_range,training_size,replace = FALSE))
    validation <- sort((sample_range[!(sample_range %in% training)]))
    
    #score
    score<-fn_fit_score(training=training,validation=validation,responser=responser)
    #all data
    training.list[[i]]<<-(training)
    validation.list[[i]]<<-(validation)
    score.list[[i]]<<-(score)
    
    if (score <= score_threshold)
    {#refresh the score_threshold
      score_threshold=score
      training.best.list[[u]]<<-(training)
      validation.best.list[[u]]<<-(validation)
      score.best.list[[u]]<<-(score)
      print (paste(i,":New lowest score is:",score_threshold))
      print (validation.best.list[[u]])
      u=u+1
    }
    #running score
    score.list_running[i]<<-score_threshold
  }
  
  
  Samples_all_split.df<-tibble(Training_set=training.list,testing_set=validation.list,Score=score.list)
  
  Samples_best_split.df<<-tibble(Training_set=training.best.list,testing_set=validation.best.list,Score=score.best.list)
  
  write.csv(data.frame(lapply(Samples_all_split.df, as.character), stringsAsFactors=FALSE),"Subgrouping_metrics_all_permutations.csv")
  write.csv(data.frame(lapply(Samples_best_split.df, as.character), stringsAsFactors=FALSE),"Subgrouping_metrics_best_fit.csv")
  
  ###plot the permutation===========
  permu_df<-data.frame(Permutation=rep(1:permutation),Deviation_Score=unlist(score.list))
  permu_best_df<-data.frame(Permutation=rep(1:permutation),Deviation_Score=unlist(score.list_running))
  
  plot_running<-ggplot(permu_best_df)+
    geom_line(aes(x=Permutation,y=Deviation_Score),size=1,color=color.panel3[1])+
    annotate(geom="text",label=paste("Lowest score=",round(min(permu_df$Deviation_Score),2)),
             x=permutation*0.01,y=min(permu_df$Deviation_Score)*5,size=5)+
    scale_x_log10()+
    theme(legend.position = "none",
          #axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size=14)
          
    )+
    labs(title = "Running Score")+
    ylab("Deviation Score")
  #plot_running
  plot_score<-ggplot(permu_df)+
    geom_line(aes(x=Permutation,y=Deviation_Score),size=0.2,color=color.panel3[1])+
    theme(legend.position = "none",
          #axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size=14)
          
    )+
    labs(title = "Instant Score")+
    ylab("Deviation Score")
  #plot_score
  figure<<-ggarrange(plot_score,plot_running,
                     labels = c("A","B"),ncol = 2,nrow = 1)
  
  figure
}
fn_plot_Residual<-function(reg_model,rlim,flim,caption,pval){
  ##Debug purpose
  #xlim=c(-15,15)
  # reg_model=multi.fit
  #  rlim=c(-5,5)
  #  flim=c(0,30)
  # caption="test"  
  ##
  if(missing(caption)){caption="Residual distribution"}
  if(missing(rlim)){rlim=NULL}
  if(missing(flim)){flim=NULL}
  if(missing(pval)){pval=T}
  
  
  res<<-data.frame(ID=c(1:length(residuals(reg_model))),
                   #observation=training_set[,responser],
                   fitted=fitted(reg_model),
                   residuals=residuals(reg_model))
  res_plot<<-ggplot(res,aes(x=residuals))+
    geom_histogram()+
    #coord_obs_pred(ratio = 1, xlim = NULL, ylim = c(0,NULL), expand = T, clip = "on")+
    coord_cartesian(xlim=rlim)+
    labs(title=caption)+
    theme(plot.title = element_text(hjust = 0.5))
  fvsr_plot<<-ggplot(res,aes(x=fitted,y=residuals))+geom_point(color="#595959")+
    #scale_x_log10()+
    #coord_obs_pred(ratio = 1, xlim = NULL, ylim = c(0,NULL), expand = T, clip = "on")+
    coord_cartesian(xlim=flim,ylim=rlim)+
    labs(title=paste("Fitted VS Residuals\n","n=",length(residuals(reg_model))))+
    theme(plot.title = element_text(hjust = 0.5))
  
  coefficients_data<-data.frame(variables=row.names(coefficients(summary(reg_model))),
                                coef=coefficients(summary(reg_model))[,1],
                                pvalue=coefficients(summary(reg_model))[,4])
  
  if(pval){pval_plot<<-ggplot(coefficients_data[!(rownames(coefficients_data) %in% "(Intercept)"),])+
    geom_col(aes(y=variables,x=-log10(pvalue)))+
    geom_vline(xintercept=-log10(0.05), linetype="longdash",color=color.panel4[3])+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12))
  
  #figure<-ggarrange(res_plot,fvsr_plot,pval_plot,
  #                  labels = c("A","B","C"),ncol = 2,nrow = 2)
  figure<<-ggarrange(
    ggarrange(res_plot,fvsr_plot,labels = c("A","B"),ncol = 2,nrow = 1),
    pval_plot,labels=c("","C"),nrow=2)
  }else{
    figure<<-ggarrange(res_plot,fvsr_plot,labels = c("A","B"),ncol = 2,nrow = 1)
  }
  
  
  figure
}
fn_fit_glm<-function(predictor_range,responser,distribution,rlim,flim){
  
  ##Debug only
  # predictor_range=c(4:15)
  # responser=55
  # distribution="gaussian"
  ##
  
  if(missing(rlim)){rlim=NULL}
  if(missing(flim)){flim=NULL}
  responser<<-responser
  Predictor<-colnames(data)[predictor_range]
  Responser<-colnames(data)[responser]
  
  if (length(predictor_range)==1){
    ###Case 1. single predictor
    Formula <- as.formula(paste(Responser,"~",Predictor)) 
    
    multi.fit<<-glm(formula = Formula,family=distribution,
                    data=training_set)
  } else {
    ###Case 2. multiple predictor
    multi.fit<<-glm(training_set[,responser] ~ ., family=distribution,
                    data=training_set[,predictor_range])
  }
  #create glm model
  assign(paste("multi.fit_glm_",length(predictor_range),"predictors","to",responser,sep = ""), multi.fit,envir = .GlobalEnv)
  assign("multi.fit_last_glm", multi.fit,envir = .GlobalEnv)
  
  #plot residuals and fitted values and p values.
  plot<-fn_plot_Residual(reg_model=multi.fit,
                         caption=paste(length(predictor_range),"predictors","to\n",colnames(data)[responser],sep = " "))
  print(summary(multi.fit))
  plot
} 
fn_plot_corr_preditedVSactual<-function(reg_model,predictor_range,responser,model_type,if_export){
  ##debug
  # reg_model=multi.fit_last_nls
  # predictor_range=4
  # responser=55
  # if_export=F
  # model_type="micmen"
  # model="glm"
  ##
  
  if (missing(if_export)){if_export=F}
  if (missing(predictor_range)){predictor_range=predictor_range}
  if (missing(responser)){responser=responser}
  
  summary(reg_model)
  best_fit_predictors<-colnames(training_set)[predictor_range]
  
  #Previous deprecated codes for making prediction 
  if(T){if(model_type=="glm"){
    best_fit_predictors.df<<-data.frame(ID=c("-",predictor_range),
                                        Predictor=c("Intercept",best_fit_predictors),
                                        Estimated_coefficient=coef(reg_model),
                                        Pvalue=coef(summary(reg_model))[,4],
                                        row.names = NULL)
    
    #predicted values
    for (i in 1:nrow(testing_set)){
      #for debug
      #i=1
      if (i==1){predicted_value<-list()}
      predicted_value[[i]]<-fn_predicted_response_glm(testing_set[i,best_fit_predictors])
    }
  }
    if(model_type=="micmen"){
      best_fit_predictors.df<<-data.frame(ID=c(predictor_range,"-","-"),
                                          Predictor=c(best_fit_predictors,"Vm","k"),
                                          Estimated_coefficient=c("-",coef(reg_model)),
                                          Pvalue=c("-",coef(summary(reg_model))[,4]),
                                          row.names = NULL)
      #predicted values for nlm
      for (i in 1:nrow(testing_set)){
        #for debug
        #i=1
        if (i==1){predicted_value<-list()}
        predicted_value[[i]]<-fn_predicted_response_nlm(testing_set[i,best_fit_predictors])
      }
    }}
  
  
  #####b) Prediction in the validation test=============
  Validation_df<<-data.frame(ID=testing_set$SAMPLE_ID,
                             Actual=testing_set[,responser],
                             Predicted=unlist(predicted_value))
  
  Responser<-colnames(data)[responser]
  Predictor<-colnames(data)[predictor_range]
  R_sqr<-summary(lm(Validation_df$Actual~Validation_df$Predicted))$adj.r.squared
  
  #####c) Correlation_validation_actualVSpredicted#################
  p_cor<<-ggplot(Validation_df,aes(x=Predicted,y=Actual))+
    geom_point()+
    geom_smooth(method='lm',color=color.panel3[1])+
    annotate("text",label=paste("R-squared=",sprintf("%.3f", round(R_sqr,3))), 
             -Inf, Inf, hjust = -0.5, vjust = 2,size=12/.pt)+
    labs(title=paste(Responser,"predicted by\n",Predictor))+
    theme(plot.title =element_text(hjust = 0.5,size=14),
          axis.text=element_text(size=12)
    )+
    coord_obs_pred(ratio = 1, xlim = NULL, ylim = c(0,NULL), expand = T, clip = "on")
  
  print(p_cor)
  
  if (if_export==T){ 
    #####a) Data output: Prediction Formula============
    write.csv(best_fit_predictors.df,"Trained_model_parameters.csv",row.names = T)
    write.csv(Validation_df,"Validation_actual_predicted.csv",row.names = T)
  }
  
}
fn_predicted_response_glm<-function(predictor_value){
  ##debug
  #Predictor_data<-testing_set[1, best_fit_predictors_ID]
  ##
  Predicted<-best_fit_predictors.df$Estimated_coefficient[1]+
    sum(predictor_value* best_fit_predictors.df$Estimated_coefficient[-1])
  print(Predicted)
}
fn_fit_nls<-function(predictor_range,responser,Vm,k,rlim,flim){
  
  ##Debug only
  # predictor_range=c(29)
  # responser=55
  # #Vm: maximum value of the response (coefficient a)
  # Vm=1
  # #k: input value that 1/2 of the Vm is attained.(coefficient b)
  # k=1
  #Value(response)=Vm*input/(K+input), input is the predictor
  ##
  if(missing(rlim)){rlim=NULL}
  if(missing(flim)){flim=NULL}
  
  responser<<-responser
  predictor_range<<- Responser<-colnames(data)[responser]
  Responser<<-colnames(data)[responser]
  Predictor<<-colnames(data)[predictor_range]
  
  Y<<-training_set[,Responser]
  X<<-training_set[,Predictor]
  
  if (length(predictor_range)==1){
    ###Case 1. single predictor
    Formula <- as.formula(Y ~ SSmicmen(X, Vm, k)) 
    multi.fit_nls<<-nls(formula = Formula)
    
    
  } else {
    ###Case 2. multiple predictor
    # multi.fit<<-glm(training_set[,responser] ~ ., family=distribution,
    #                 data=training_set[,predictor_range])
  }
  #create glm model
  assign(paste("multi.fit_nls_",length(predictor_range),"predictors","to",responser,sep = ""), multi.fit_nls,envir = .GlobalEnv)
  assign("multi.fit_last_nls", multi.fit_nls,envir = .GlobalEnv)
  
  #plot residuals and fitted values and p values.
  
  print(summary(multi.fit_nls))
  
} 
fn_predicted_response_nlm<-function(predictor_value){
  ##debug
  #Predictor_data<-testing_set[1, best_fit_predictors_ID]
  ##
  Vm<<-as.numeric(best_fit_predictors.df[2,3])
  k<<-as.numeric(best_fit_predictors.df[3,3])
  Predicted<- Vm*(predictor_value)/(k+predictor_value)
  print(Predicted)
}
fn_plot_validation_predVSactual<-function(model,val_set){
  ##debug use only
  model=multi.fit_glm_4predictorsto55
  val_set=testing_set
  ##
  responser=model$formula[[2]]
  predicted<-predict(model,newdata=val_set)
  actual <-val_set[,paste(responser)]
  Validation_df<-data.frame(predicted=predicted,actual=actual)
  
  R_sqr<-summary(lm(Validation_df$actual~Validation_df$predicted))$adj.r.squared
  
  p_cor<<-ggplot(Validation_df,aes(x=predicted,y=actual))+
    geom_point()+
    geom_smooth(method='lm',color=color.panel3[1])+
    annotate("text",label=paste("R-squared=",sprintf("%.3f", round(R_sqr,3))), 
             -Inf, Inf, hjust = -0.5, vjust = 2,size=12/.pt)+
    labs(title=paste(model$formula))+
    theme(plot.title =element_text(hjust = 0.5,size=14),
          axis.text=element_text(size=12)
    )+
    coord_obs_pred(ratio = 1, xlim = NULL, ylim = c(0,NULL), expand = T, clip = "on")
  
  print(p_cor)
  
}
#

#1. Demographics####################


#plot for demographics 1
##1) Age between trauma and control######################
pdf ("Age_distribution_traumaVSctrl_test.pdf",width = 2,height = 3)

fill.No=57
x.No=57
y.No=1
ggplot(data,aes_string(x=colnames(data)[x.No], 
       y=colnames(data)[y.No]))+ 
  geom_violin(aes_string(fill=colnames(data)[fill.No]),
              color="grey",size=0.05)+
  geom_dotplot(
               binaxis = "y",
               stackdir="center",
               binwidth = 0.5)+
  stat_compare_means(#t.test default is Welch's t-test, which is best fit.
    #remember to put x,y in aes of ggplot, not under the geom_plot.
    #use this aes(label=after_stat(p.signif)) if want asterisk instead of p-value (p.format).
    aes(label = after_stat(p.signif)),
    comparisons = list(c(1,2)),
    method = "t.test",
    bracket.size = 0.5,
    size=5
  )+
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.15)))+
  #coord_cartesian(ylim = c(15,55))+
  #scale_fill_brewer(palette="BuPu")
  scale_fill_manual(values = color.panel)+
  theme_classic()+
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        )+
  xlab ("")+
  
  stat_n_text(aes_string(x=colnames(data)[x.No], 
                         y=colnames(data)[y.No]))
dev.off()


#plot for demographics 2
##2) Race  between trauma and control#######################
pdf ("Race_TraumaVsCtrl_test.pdf",width = 5,height = 3)

fill.No=57
x.No=57
y.No=3


ggplot(data,aes_string(fill=colnames(data)[fill.No], y=colnames(data)[y.No]))+ 
  geom_bar(color="grey",size=0.05)+
  geom_text(stat="count",
            aes(label=after_stat(count)),
                position=position_stack(vjust=0.5))+
  
  
  scale_fill_manual(values = color.panel)+
  theme_classic()+
  theme(legend.position = "top",
        legend.text = element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
  )+
  xlab ("Count")+
  labs(fill="")

dev.off()


#plot for demographics 3
##3) Sex  between trauma and control#####################

pdf ("Sex_traumaVSctrl_test.pdf",width = 2.2,height = 3)

fill.No=57
x.No=2
y.No=2

ggplot(data,aes_string(fill=colnames(data)[fill.No], x=colnames(data)[x.No]))+ 
  geom_bar(color="grey",size=0.05)+
  #geom_dotplot(aes_string(x=colnames(data)[x.No], y=colnames(data)[y.No]),binaxis = "y",stackdir="center")+
  #scale_fill_brewer(palette="BuPu")+
 geom_text(stat="count", 
           aes(label=after_stat(count)),
           position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = color.panel)+
  theme_classic()+
  theme(legend.position = "top",
        axis.text=element_text(size=12),
        axis.title=element_text(size=12)
        )+
  labs(fill="")+
  ylab("Count")

dev.off()


#plot for demographics 4
##4) ICU time  between major and less severe trauma#################

pdf ("ICU_majorVSless_severe_test.pdf",width = 2,height = 3.8)


fill.No=56
x.No=56
y.No=55

ggplot(data[data$Group %in% c("Trauma"), ],
       aes_string(x=colnames(data)[x.No], 
                  y=colnames(data)[y.No])
       )+ 
  
  geom_violin(aes_string(fill=colnames(data)[fill.No]),
              color="grey",size=0.05)+
  
  geom_dotplot(binaxis = "y",
               stackdir="center",
               binwidth = 0.5)+
  
  #statistics
  stat_compare_means(#t.test default is Welch's t-test, which is best fit.
                     #remember to put x,y in aes of ggplot, not under the geom_plot.
                     #use this aes(label=after_stat(p.signif)) if want asterisk instead of p-value (p.format).
                     aes(label = after_stat(p.signif)),
                     comparisons = list(c("Less_Severe","Major")),
                     method = "t.test",
                     bracket.size = 0.5,
                     size=5
                     )+
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.15)))+
  #coord_cartesian(ylim = c(-5,45))+
  scale_fill_manual(values = color.panel2)+
  theme_classic()+
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
        )+
  xlab ("")+
  
  stat_n_text(aes_string(x=colnames(data)[x.No], 
                       y=colnames(data)[y.No]),
              vjust = 0.5)
  
  



dev.off()


##5) Injury characteristics_Upset plot############


###a. Data preparation: filling 0 to those without injury########
data[c(1:48),c(58:67)][!data[c(1:48),c(58:67)]==""]<-"1"
data[c(1:48),c(58:67)][data[c(1:48),c(58:67)]==""]<-"0"
data[,c(58:67)] <- sapply(data[,c(58:67)], as.numeric)



###b. Traditional upset plot with UpsetR###########
unloadNamespace ("ComplexUpset")
library("UpSetR")

pdf ("Mechanism_of_injuries_traditional_Upset.pdf",
     width = 10,height = 10)

upset(data[c(1:48),c(58:67)],
        nsets=20,
        mb.ratio = c(0.7, 0.3),
        mainbar.y.label = "Intersected samples",mainbar.y.max = NULL,
        sets.x.label = "Incident size",set_size.show = T,
        set_size.scale_max = 47.5,
        )
dev.off()

###c. Complex upset plot with ComlexUpset##########
unloadNamespace ("UpSetR")
library("ComplexUpset")

pdf ("Mechanism_of_injuries_complex_Upset.pdf",
     width = 6,height = 4.5)
Injury_types<-colnames(data)[58:67]
  upset(data[c(1:48),c(58:68)],
      Injury_types,
      base_annotations=list(
                            'Intersected samples'=intersection_size(
                              counts=T,
                              mapping=aes(fill=factor(Injury_mechanism,
                                                      levels = c("MVC","MCC","Pedestrian","Mechanical","Fall "))
                                          ),
                              
                              bar_number_threshold=1,
                                                                  )
                            + scale_fill_manual(values = color.panel3)
                            + labs(fill="")
                            + ylim(c(0, 25))
                            + theme_classic()
                            + theme(legend.position = "right",legend.direction = "vertical")
                            + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
                            ),
      set_sizes=(upset_set_size(filter_intersections=TRUE,
                                geom=geom_bar(
                                  aes(fill=factor(Injury_mechanism,
                                                  levels = c("MVC","MCC","Pedestrian","Mechanical","Fall ")
                                                  )
                                      ),
                                  width=0.8
                                              ),
                                position='right'
                                )
                            + ylab("Incident size")
                            + scale_fill_manual(values = color.panel3)
                            + theme_classic()
                            + theme(legend.position = "none", axis.line.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),)
                            + geom_text(aes(label=..count..), hjust=-0.2, stat='count')
                            + expand_limits(y=53)
                ),
      width_ratio = 0.3,
      min_size=1,
      height_ratio=0.7,
      guides='over'
      )

dev.off()

#2. Biomarker panel############
#Age between trauma and control

##reshape the dataframe for biomarker columns##############
data.melt.biomarker<-melt(data,
                measure.vars = colnames(data)[c(4:15)])
colnames(data.melt.biomarker)[c(44,45,105,106)]=c("severity","group","biomarker","expression")
#convert the group (character) to factor type.
data.melt.biomarker$group<-as.factor(data.melt.biomarker$group)




##1) Trauma vs Control, vertical#############################
#(by flipping horizontal with coord_flip()
#grouped by the factorized variable: biomarkers (melted) and then compare the levels of the value(expression), between ~Group:
stat.test1 <- data.melt.biomarker %>%
  group_by(biomarker) %>%
  t_test(expression ~ group) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p.adj")
#FDR=0.05, FDR`=FDR/n, p<FDR, adjusted: padjust<FDR, which equal: np < FDR. Here, the adjust p = np.
stat.test1 <- stat.test1 %>%
  add_xy_position(x = "biomarker")


fill.No=45
x.No=105
y.No=106



pdf ("biomarker_panel_controlvstrauma_test.pdf",
     width = 5,height = 6)



#pos<-position_dodge(0.6)
ggplot(data.melt.biomarker,
       aes_string(
                  x=colnames(data.melt.biomarker)[x.No], 
                  y=colnames(data.melt.biomarker)[y.No]
                  )
       )+
  geom_violin(aes_string(fill=colnames(data.melt.biomarker)[fill.No]),
                         color="grey",size=0.05)+
  geom_sina(aes_string(fill=colnames(data.melt.biomarker)[fill.No]),
            alpha=0.3,size=0.4)+
  
  scale_y_log10()+
  xlab("biomarkers")+ylab("copies/mL")+
  scale_fill_manual(values = color.panel)+
  theme_classic()+
  theme(legend.position = "top",
        legend.direction = "vertical",
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=12))+
  labs(fill="")+
  #Add statistics to the plot
  #Add statistics computed by rstatix, t-test with multi-comparison adjust (bonferroni method)
  stat_pvalue_manual(stat.test1,
                     label = "p.adj.signif", 
                     angle = 90,
                     hjust = 1, vjust = 1.5,
                     tip.length = 0,
                     #y.position = 0, 
                     step.increase = 0,
                     remove.bracket = T,
                     size=4,
                     
  )+
  #flip x and y
  #scale_x_discrete(limits=rev)+
  coord_flip()

dev.off()
  

##2) Major and less severe_vertical#################################
#Grouped by severity

stat.test2 <- data.melt.biomarker[data.melt.biomarker$group %in% c("Trauma"),] %>%
  group_by(biomarker) %>%
  t_test(expression ~ severity) %>%
  adjust_pvalue(method = "none") %>%
  add_significance("p.adj")
#FDR=0.05, FDR`=FDR/n, p<FDR, adjusted: padjust<FDR, which equal: np < FDR. Here, the adjust p = np.
stat.test2 <- stat.test2 %>%
  add_xy_position(x = "biomarker")

fill.No=44
x.No=105
y.No=106



pdf ("biomarker_panel_severity_vertical_test.pdf",
     width = 5,height = 6)


#pos<-position_dodge(0.6)
ggplot(data.melt.biomarker[data.melt.biomarker$group %in% c("Trauma"),],
       aes_string(
         x=colnames(data.melt.biomarker)[x.No], 
         y=colnames(data.melt.biomarker)[y.No]
       )
)+
  geom_violin(aes_string(fill=colnames(data.melt.biomarker)[fill.No]),
              color="grey",size=0.05)+
  geom_sina(aes_string(fill=colnames(data.melt.biomarker)[fill.No]),
            alpha=0.3,size=0.4)+
  
  xlab("biomarkers")+ylab("copies/mL")+
  scale_fill_manual(values = color.panel2)+
  theme_classic()+
  theme(legend.position = "top",
        legend.direction = "vertical",
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=12))+
  labs(fill="")+
  scale_y_log10()+
  coord_flip()+
  #Add statistics to the plot
  #Add statistics computed by rstatix, t-test with multi-comparison adjust (bonferroni method)
  stat_pvalue_manual(stat.test2,
                     label = "p.adj.signif", 
                     angle = 90,
                     hjust = 1.2, vjust = 1.5,
                     #y.position = 2, 
                     #step.increase = 0,
                     remove.bracket = T,
                     size=4,
                     hide.ns=T
                     
                     
  )+
  
  #flip x and y
  #scale_x_discrete(limits=rev)+
  coord_flip()

dev.off()




#3. Mediator Panel and Clinical Indicator####################
#Small panel, each one is a mediator.
#three types: control, less_severe_major

##1) Immune Mediators##########

  #For test
  #mediator_indiviual(16)

#[16:38]
pdf ("Mediator_panel.pdf",width = 10,height = 8)

  p <- list()
  
  for (i in 16:38)
  { 
    print(i)
    p[[i-15]] <- fn_plot_mediator_indiviual(mediator = i)
  }
  #gridExtra::grid.arrange
  do.call(grid.arrange,p)

dev.off()

##2) Clinical Indicators#################
#clinical_indiviual(45)


#For test
clinical_indiviual(49)
#[39:53]
 
pdf ("Clinical_indicators_log10.pdf",width = 6,height = 8)

  ##
  p <- list()
  
  for (i in 39:53)
  { 
    print(i)
    p[[i-38]] <- fn_plot_clinical_indiviual(clinical = i)
  }
  #Package source for the grid: gridExtra::grid.arrange
  do.call(grid.arrange,p)

dev.off()

#4.correlation




#4. Logistic regression (binomial linear regression) to associate the outcome of biomarker expression ############


##1) Trauma/Control prediction by biomarkers############


#Biomarker_ROC(PREDICTOR_COL=4,RESPONSE_COL=57,SAMPLE_RANGE=c(1:72),SEN=80,SPEC=80,RESP_TYPE = 1)
#Biomarker_ROC(PREDICTOR_COL=5,RESPONSE_COL=56,RES_GR1="Major",RES_GR2="Less_Severe",SAMPLE_RANGE=c(1:48))


pdf ("Prediction_BiomarkerToGroup_excludeInRNA-seq.pdf",width = 8,height = 6)
par(pty="square")
##
layout<-layout(rbind(c(1:4),c(5:8),c(9:12)))
#layout.show(layout)

#c(1:72)[!(data[,115] %in% "Yes")] out of 20 RNA-seq
# or pre-allocate for slightly more efficiency
Range_of_predictors=c(4:15)
rangelist = list()
rangelist = vector("list", length = length(Range_of_predictors))
for (i in Range_of_predictors)
{ 
  fn_plot_Biomarker_ROC1(PREDICTOR_COL = i,57,SAMPLE_RANGE = c(1:72)[!(data[,115] %in% "Yes")],SEN=85,SPEC=85,RESP_TYPE=1)
}
par(pty="maximum")
dev.off()

#Arrange the rangelist
rangelist<-t(as.data.frame(rangelist) )
row.names(rangelist) <- NULL
colnames(rangelist)<-c("Predictor","Lower limit","Upper limit")
rangelist
#output the results                  
write.csv(as.data.frame(rangelist) ,"Range of predictor_forTraumaVSControl.csv",row.names = F)


##2) Major/Less_Severe trauma prediction by biomarkers##############


pdf ("Prediction_BiomarkerToSeverity_excludeInRNA-seq.pdf",width = 8,height = 6)
par(pty="square")
##
layout<-layout(rbind(c(1:4),c(5:8),c(9:12)))
#layout.show(layout)
Range_of_predictors=c(4:15)

# or pre-allocate for slightly more efficiency
Range_of_predictors=c(4:15)
rangelist = list()
regression.summary = list()
rangelist = vector("list", length = length(Range_of_predictors))
for (i in Range_of_predictors)
{ 
  fn_plot_Biomarker_ROC2(PREDICTOR_COL = i,56,SAMPLE_RANGE = c(1:48)[!(data[c(1:48),115] %in% "Yes")],SEN=70,SPEC=70,RESP_TYPE=1)
}
par(pty="maximum")
dev.off()

#Arrange the rangelist
rangelist<-t(as.data.frame(rangelist) )
row.names(rangelist) <- NULL
colnames(rangelist)<-c("Predictor","Lower limit","Upper limit")
rangelist
#output the results                  
write.csv(as.data.frame(rangelist) ,"Range of predictor_forSeverity.csv",row.names = F)


### e) Negative as using Age to predict outcome########
pdf ("Age_Prediction of trauma or severity_negative control.pdf",width = 6,height = 4)
  rangelist = list()
  regression.summary = list()
  layout<-layout(rbind(c(1:2)))
  par(pty="square")
  fn_plot_Biomarker_ROC1(PREDICTOR_COL=1,RESPONSE_COL=57,SAMPLE_RANGE=c(1:72),SEN=30,SPEC=30,RESP_TYPE = 1)
  fn_plot_Biomarker_ROC2(PREDICTOR_COL=1,RESPONSE_COL=56,SAMPLE_RANGE=c(1:48),SEN=30,SPEC=30,RESP_TYPE = 1)
  
  par(pty="maximum")
dev.off()

### f) Total plasma RNA conentration to predict outcome##########
  Range_of_predictors=69
  rangelist = list()
  regression.summary = list()
  rangelist = vector("list", length = length(Range_of_predictors))
  
  pdf ("PlasmaRNA_Prediction of trauma or severity_compare.pdf",width = 6,height = 4)
  par(pty="square")
  
  fn_plot_Biomarker_ROC2(PREDICTOR_COL=69,RESPONSE_COL=56,SAMPLE_RANGE=c(1:48),SEN=70,SPEC=70,RESP_TYPE = 1)
  
  par(pty="maximum")
  dev.off()

  
  #Arrange the rangelist
  
  rangelist<-t(as.data.frame(rangelist) )
  row.names(rangelist) <- NULL
  colnames(rangelist)<-c("Predictor","Lower limit","Upper limit")
  rangelist
  #output the results                  
  write.csv(as.data.frame(rangelist) ,"Range of PlasmaRNAConcentration_forSeverity.csv",row.names = F)
  

##3) Multivariate correlation of severity with several biomarkers########
SAMPLE_RANGE=c(1:48)[!(data[c(1:48),115] %in% "Yes")]
RES_GR1="Major"
RES_GR2="Less_Severe"
Responder<-data[SAMPLE_RANGE,56]
Responder[Responder==RES_GR1] <-1
Responder[Responder==RES_GR2] <-0
Responder <- sapply(Responder, as.numeric)

multi.fit<-glm(Responder ~ ., family="binomial",
               data=data[SAMPLE_RANGE,c(4:15)])

pdf ("Multi12_predict_roc_Severity.pdf",width = 4,height = 4)
par(pty="square")
ROC.combined<-roc(Responder ~ multi.fit$fitted.values,plot=T,legacy.axes=T, percent=F, 
    xlab="False Positive ",ylab="True Positive ",
    col=color.panel3[1],lwd=2,quiet=T)
text(x=0.25,y=0.10,label=paste("AUC=",sprintf("%.2f", round(ROC.combined$auc,2))))
text(x=0.25,y=0.20,label=paste("P = 0.005"))
title("Biomarker panels (12) -> Severity",cex.main= 1.3,line = 3)

par(pty="maximum")
dev.off()

roc(Responder ~ .,family="binomial",data=data[SAMPLE_RANGE,c(4:15)],plot=T)
ROC.combined$sensitivities
ROC.meta.multi<-data.frame(sensitivity=ROC.combined$sensitivities*100,
                         specificity=ROC.combined$specificities*100,
                         tpp=ROC.combined$sensitivities*100,
                         fpp=(1-ROC.combined$specificities)*100,
                         thresholds=ROC.combined$thresholds)
write.csv(ROC.meta.multi,"Multi12_predict_roc_Severity_metadata.csv")
summary(multi.fit)

##4) Multivariate correlation (selected from ISS regression with biomarkers) of severity with several biomarkers########
SAMPLE_RANGE=c(1:48)
RES_GR1="Major"
RES_GR2="Less_Severe"
Responder<-data[,54][SAMPLE_RANGE]
Responder[Responder==RES_GR1] <-1
Responder[Responder==RES_GR2] <-0
Responder <- sapply(Responder, as.numeric)

multi.fit<-glm(Responder ~ ., family="gaussian",
               data=data[SAMPLE_RANGE,c(4:15)])

pdf ("Multi_(glm_ISS predicting4_6_7_10_15)_predict_roc_Severity.pdf",width = 4,height = 4)
par(pty="square")
ROC.combined<-roc(Responder ~ multi.fit$fitted.values,plot=T,legacy.axes=T, percent=F, 
                  xlab="False Positive ",ylab="True Positive ",
                  col=color.panel3[1],lwd=2,quiet=T)
text(x=0.25,y=0.10,label=paste("AUC=",sprintf("%.2f", round(ROC.combined$auc,2))))
text(x=0.25,y=0.20,label=paste("P = 0.001"))
title("Biomarker (ISS predictive 5) -> Severity",cex.main= 1.3,line = 3)

par(pty="maximum")
dev.off()

summary(multi.fit)


#5. Prediction of ICU stay (True prediction)=============
##Important ICU stay time processing=====
# round up to integer days! i.e. 1.3 days-->2days

ICU_integer<-data.frame(ICU_days=ceiling(data[,55]))
data[,115]<-ICU_integer
#called ICU_days
###Evaluation of deviation of estimated value from true value.##
#Deprecated, use the later version with same name!
#fn_plot_Residual<-function(reg_model,rlim,flim,caption){
    ##Debug purpose
    #xlim=c(-15,15)
      reg_model=multi.fit_BP12
      rlim=c(-5,5)
      flim=c(0,30)
    #caption=""  
    ##
res<<-data.frame(ID=c(1:length(residuals(reg_model))),
                  #observation=training_set[,responser],
                  fitted=fitted(reg_model),
                  residuals=residuals(reg_model))
res_plot<-ggplot(res,aes(x=residuals))+
     geom_histogram()+
     coord_cartesian(xlim=rlim)+
     labs(title=caption)+
     theme(plot.title = element_text(hjust = 0.5))
fvsr_plot<-ggplot(res,aes(x=fitted,y=residuals))+geom_point(color="#595959")+
     #scale_x_log10()+
     coord_cartesian(xlim=flim,ylim=rlim)+
     labs(title="Fitted VS Residuals")+
     theme(plot.title = element_text(hjust = 0.5))+
     annotate(geom="text", x=30, y=10, label=paste("n=",length(residuals(reg_model))),
           color="black")

coefficients_data<-data.frame(variables=row.names(coefficients(summary(reg_model))),
                              coef=coefficients(summary(reg_model))[,1],
                              pvalue=coefficients(summary(reg_model))[,4])

pval_plot<-ggplot(coefficients_data[-1,])+
  geom_col(aes(y=variables,x=-log10(pvalue)))+
  geom_vline(xintercept=-log10(0.05), linetype="longdash",color=color.panel4[3])+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))

#figure<-ggarrange(res_plot,fvsr_plot,pval_plot,
#                  labels = c("A","B","C"),ncol = 2,nrow = 2)
figure<-ggarrange(
  ggarrange(res_plot,fvsr_plot,labels = c("A","B"),ncol = 2,nrow = 1),
  pval_plot,labels=c("","C"),nrow=2
  )


figure
}

##1)Each individual linear model================================
###a) Define plot ranges==========
residual_limits_for_plot=c(-10,10)
fitted_limits_for_plot=c(1,40)
SAMPLE_RANGE=c(1:48)
Responder_ID<-"ICU_days"
Responder<-data[SAMPLE_RANGE,Responder_ID]
Distribution="poisson"

###b) All BP --> ICU stay. AIC:299=========
multi.fit_BP12<-glm(Responder ~ ., family=Distribution,
                        data=data[SAMPLE_RANGE,c(4:15)])
fn_plot_Residual(reg_model=multi.fit_BP12,
                  rlim=residual_limits_for_plot,flim=fitted_limits_for_plot,
                  caption="Residuals dist: 12BP-->ICU")
###c) significant 5 predictors --> ICU stay. AIC: 290=============
multi.fit_BP5<-glm(Responder ~ ., family=Distribution,
                          data=data[SAMPLE_RANGE,c(6,7,8,10,13)])
fn_plot_Residual(reg_model=multi.fit_BP5,
              rlim=residual_limits_for_plot,flim=fitted_limits_for_plot,
              caption="Residuals dist: 5BP-->ICU")
###d) ISS to ICU stay. this is the genuine prediction.#AIC: 363======
multi.fit_ISS<-glm(Responder ~ ISS, family=Distribution,
                   data=data[SAMPLE_RANGE,])
fn_plot_Residual(reg_model=multi.fit_ISS,
              rlim=residual_limits_for_plot,flim=fitted_limits_for_plot,
              caption="Residuals dist: ISS-->ICU")
###e) ISS+ 5BP to ICU stay. this is the genuine prediction.#AIC: 276 So far the best===========

multi.fit_BP5_ISS<-glm(Responder ~ ., family=Distribution,
                       data=data[SAMPLE_RANGE,c(6,7,8,10,13,54)])
fn_plot_Residual(reg_model=multi.fit_BP5_ISS,
              rlim=residual_limits_for_plot,flim=fitted_limits_for_plot,
              caption="Residuals dist: 5P+ISS-->ICU")

###f) Significant mediators (day1) #AIC: 301.42============

multi.fit_IM3<-glm(Responder ~ ., family=Distribution,
                       data=data[SAMPLE_RANGE,c(28,29,30)])
fn_plot_Residual(reg_model=multi.fit_IM3,
              rlim=residual_limits_for_plot,flim=fitted_limits_for_plot,
              caption="Residuals dist: 3MIs-->ICU")

#These immune mediators seem redundant to predict ICU stay time together with 5BP+ISS.
#Back to model 6).

###g) IL-6 is not predictive of length of ICU stay.AIC:415===========
multi.fit_IL6<-glm(Responder ~ IL.6, family=Distribution,
                   data=data[SAMPLE_RANGE,])
fn_plot_Residual(reg_model=multi.fit_IL6,
              rlim=residual_limits_for_plot,flim=fitted_limits_for_plot,
              caption="Residuals dist: IL-6-->ICU")

###h) All BP+ISS --> ICU stay. AIC:286=========
multi.fit_BP12_ISS<-glm(Responder ~ ., family=Distribution,
                    data=data[SAMPLE_RANGE,c(4:15,54)])
fn_plot_Residual(reg_model=multi.fit_BP12_ISS,
              rlim=residual_limits_for_plot,flim=fitted_limits_for_plot,
              caption="Residuals dist: 12BP-->ICU")




##2) Statistical metrics for each individual models=========================
model_list=c("multi.fit_BP12",
              "multi.fit_BP5",
              "multi.fit_ISS",
              "multi.fit_BP5_ISS",
              "multi.fit_IL6",
              "multi.fit_IM3")
model_list.df<-tibble(
                model=model_list,
                Responder=c(rep("ICU_days",length(model_list))),
                Data_range=list(c(4:15),
                             c(6,7,8,10,13),
                             54,
                             c(6,7,8,10,13,54),
                             16,
                             c(28,29,30))
                      )
  ##Define ranges#
  
  SAMPLE_RANGE=c(1:48)
  Responder_ID<-"ICU_days"
    Responder<-data[SAMPLE_RANGE,Responder_ID]
  residual_limits_for_plot=c(-10,10)
  fitted_limits_for_plot=c(1,40)
  
  
  #ini
  p <- list()
  AIC.list<-list()
  BIC.list<-list()
  Pval.list<-list()
  McFadden_R_squared.list<-list()
  
  for (i in c(1:length(model_list)))
  { 
  print(paste("Processing model",i,":",model_list[i]))
      #Debug use only
      #i=1
      #length(model_list.df$Data_range[[i]]) ==1
      ####
     
    if (length(model_list.df$Data_range[[i]])==1){
      ###Case 1. single predictor
      Predictor<-colnames(data)[model_list.df$Data_range[[i]]]
      Formula <- as.formula(paste(Responder_ID,"~",Predictor)) 
      multi.fit<-glm(formula = Formula,
                     family=Distribution,
                     data=data[SAMPLE_RANGE,])
         } 
    else {
      ###Case 2. multiple predictor
      multi.fit<-glm(Responder ~ ., family=Distribution,
                     data=data[SAMPLE_RANGE,c(model_list.df$Data_range[[i]])])
         }
    #add metrics for the model here
    AIC.list[i]<-AIC(multi.fit)
    BIC.list[i]<-BIC(multi.fit)
      #Null deviance
      Null.deviance<-with(summary(multi.fit), null.deviance)
      #Deviance
      Deviance<-with(summary(multi.fit), deviance)
      #Degree of freedom
      Df<-with(summary(multi.fit), df)[1]-1  
    Pval.list[i]<-pchisq(q=Null.deviance-Deviance, df=Df,lower.tail = F)
    McFadden_R_squared.list[i]<-1-Deviance/Null.deviance
    
    #store plots here
    p[[i]]<-fn_plot_Residual(reg_model=multi.fit,
                  rlim=residual_limits_for_plot,flim=fitted_limits_for_plot,
                  caption=paste(model_list.df$model[i],"→",Responder_ID))
  }
 
  ###a) Create the plot for individual metric: residual distr. residual vs fitted, p value for each predictor=========
  pdf ("Model_goodness_evaluation.pdf",width = 12,height = 18)
  do.call(grid.arrange,p)
  dev.off()
  

  #AIC, AIC (Akaike Information Criterion) is a measure of the quality of a statistical model.
  #Lower the better fit.
    #AIC(multi.fit)
  #AIC.list
  
  #BIC, The Bayesian Information Criterion, often abbreviated BIC, is a metric that is used to compare the goodneess of models.
  #Lower the better.
    #BIC(multi.fit)
  #BIC.list
  
  #calculate McFadden's R-squared for model
  #McFadden’s R-Squared = 1 – (log likelihoodmodel / log likelihoodnull)
    #McFadden_R_squared=1-Deviance/Null.deviance
    #This is how much the model explains the data.
    #In practice, values over 0.40 indicate that a model fits the data very well
  #McFadden_R_squared.list
  
  #p value: Goodness of Fit Test. calculated from Chi-squared
  #lower.tail: If TRUE, the probability to the left of q in the Chi-Square distribution is returned. If FALSE, the probability to the right of q in the Chi-Square distribution is returned. Default is TRUE.
    #p_value_model_fit<-pchisq(q=Null.deviance-Deviance, df=Df,lower.tail = F)
  #Pval.list
  
  #Add metrics to data frame
  model_list.df["AIC"]<-unlist(AIC.list)
  model_list.df["BIC"]<-unlist(BIC.list)
  model_list.df["R^2"]<-unlist(McFadden_R_squared.list)
  model_list.df["Pval"]<-unlist(Pval.list)
  model_list.df["-log10Pval"]<-(-log10(unlist(Pval.list)))
  
###b) Model comparison: AIC, BIC, R-squared, P-value============
  write.csv(data.frame(lapply(model_list.df, as.character), stringsAsFactors=FALSE),"glm_model_metrics.csv")
  
  ##reshape the metrics data frame##
  model_list.df.melted<-melt(model_list.df,
                            measure.vars = colnames(model_list.df)[c(4:6,8)])
  #convert the group (character) to factor type.
  model_list.df.melted$variable<-factor(model_list.df.melted$variable,levels =c("AIC","BIC","R^2","-log10Pval"))

  #AIC and BIC plot
  a<-ggplot(subset(model_list.df.melted,variable %in% c("AIC","BIC")))+
    geom_col(aes(fill=variable,y=model,x=value),position="dodge")+
    # geom_text(aes(label=round(value,0),y=model,x=value,group=variable),
    #           position = position_dodge(width = 1),
    #           colour="#595959")+
    scale_fill_manual(values = color.panel1)+
    theme_classic()+
    theme(legend.position = "right",
          legend.direction = "vertical",
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size=14) ,
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)
    )+
    labs(fill="Metrics",title = "Fitness of model to the dataset \n (Lower is better)",y="")+
    coord_cartesian(xlim=c(200,270))
  #Pvalue
  b<-ggplot(subset(model_list.df.melted,variable %in% "-log10Pval"))+
    geom_col(aes(fill=variable,y=model,x=value),position="dodge")+
    geom_text(aes(label=round(value,2),y=model,x=value),
              position = position_stack(vjust = 0.9),
              colour="#595959"
    )+
    scale_fill_manual(values = color.panel1[2])+
    theme_classic()+
    theme(legend.position = "none",
          legend.direction = "vertical",
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size=14) ,
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)
    )+
    labs(fill="Metrics",title = "Fitness of model to the dataset \n (Higher is better)",
         x="-log10Pvalue",y="")+
    coord_cartesian(xlim=c(0,15))
  #R squared plot
  c<-ggplot(subset(model_list.df.melted,variable %in% "R^2"))+
    geom_col(aes(fill=variable,y=model,x=value),position="dodge")+
    #geom_vline(xintercept=0.4, linetype="longdash",color=color.panel4[3])+
    geom_text(aes(label=sprintf("%.2f", round(value,2)),y=model,x=value),
              position = position_stack(vjust = 0.9),
              colour="#595959"
    )+
    scale_fill_manual(values = color.panel1[2])+
    theme_classic()+
    theme(legend.position = "none",
          legend.direction = "vertical",
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size=14) ,
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)
    )+
    labs(fill="Metrics",title = "McFadden’s R-Squared \n (Higher is better)",
         x="McFadden’s R-squared",y="")
  
  c
  figure<-ggarrange(a,b,c,labels = c("A","B","C"),ncol = 3,nrow = 1)
  ###c) Comparison of models AIC, BIC, R-squared, P-value==============
  pdf ("Multiple model_metrics.pdf",width = 20,height = 5)
  figure  
  dev.off()
  
  
  
  
#6. Stage 2: multi time points clinical data. Relating to Section 3.2)=======

#Clinical Indicators: multiple time points##
#clinical_indiviual(45)
  

  #Have a test here
  #range:39~53 (total:15 CIs)
  fn_plot_4days_ci(50)
  
  
##1) Trends of clinical indicators, 4 days===========
  #for placing significant symbols
  customized_range.list<-list(
    c(4,21),
    c(7.5,18),
    c(22,55),
    c(3,5.5),
    c(7,22),
    c(0.4,1.4),
    c(0,300),
    c(0,200),
    c(0.3,3.5),
    c(25,125),
    c(0,5),
    c(0,2),
    c(20,50),
    c(8,19),
    c(100,410)
  )
  
  
  pdf ("Clinical_indicators_4days_trend.pdf",width = 8,height = 10)
  
    ##
    p <- list()
    for (i in 39:53)
    { 
      print(i)
      p[[i-38]] <- fn_plot_4days_ci(y.No=i)
    }
    #Package source for the grid: gridExtra::grid.arrange
    do.call(grid.arrange,p)
  
  dev.off()
  
  
  
 
#7. [Universal responser] Splitting data set for training and validation===================
  #
 
  ##only run this section to generate a NEW set of training set and validation set 
  
  if(T){fn_fit_split(training_size = 38,
                     sample_range = c(1:48)[!is.na(data[,55])],
                     responser = 55,
                     permutation = 40000)}
  
  #If some data are to be excluded, add [!(c(1:48) %in% c(EXCLUDED_RANGE))]
  
  pdf ("Subgrouping_metrics_Deviation_Score.pdf",width = 8,height = 4)
    figure
  dev.off()
  
  png ("Subgrouping_metrics_Deviation_Score.png",width = 1600,height = 800,units = "px",res=200)
  figure
  dev.off()
    
###ATTENTION: Since it is randomly sampled, output results in Section 7 will be different from each run.=============
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
##only run this section to generate a NEW set of training set and validation set 
if(T){samples_best_split.df_0321<-Samples_best_split.df}
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
#select the splitted sets
data.frame(samples_best_split.df_0321)
selected=20

training_set_ID<-unlist(samples_best_split.df_0321[selected,1])
testing_set_ID<-unlist(samples_best_split.df_0321[selected,2])
score_0321<-unlist(samples_best_split.df_0321[selected,3])


training_set_ID
testing_set_ID
score_0321

#######!! !! !! Export and Import of training and validation sets#################
#These are complete and splitted training set and validation set.
training_set<-cbind(data[training_set_ID,],purpose="Training",SAMPLE_ID=training_set_ID)
testing_set<-cbind(data[testing_set_ID,],purpose="Testing",SAMPLE_ID=testing_set_ID)
subset_melted<-rbind(training_set,testing_set)
if(T){write.csv(subset_melted,"Importable_training_testing_sets.csv")}
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
#Import previous training and validation sets..
if(F){subset_melted<-read.table("Importable_training_testing_sets.csv",header = T,row.names =1,sep = ",")
training_set<-subset_melted[subset_melted$purpose=="Training",]
testing_set<-subset_melted[subset_melted$purpose=="Testing",]
}

subset_melted$purpose <- factor(subset_melted$purpose, 
                                levels=c("Training","Testing"))

##1)Describption of training set and validation set####################
###a) Age between two sets#############################################
pdf ("Age_distribution_trainingVSvalidation.pdf",width = 2,height = 4)


fill.No=118
x.No=118
y.No=1

ggplot(subset_melted,aes_string(x=colnames(subset_melted)[x.No], 
                       y=colnames(subset_melted)[y.No]))+ 
  geom_boxplot(aes_string(fill=colnames(subset_melted)[fill.No]),
              color="black",size=0.05,alpha=0.75)+
  geom_dotplot(
    binaxis = "y",
    stackdir="center",
    binwidth = 0.5)+
  stat_compare_means(#t.test default is Welch's t-test, which is best fit.
    #remember to put x,y in aes of ggplot, not under the geom_plot.
    #use this aes(label=after_stat(p.signif)) if want asterisk instead of p-value (p.format).
    aes(label = after_stat(p.signif)),
    comparisons = list(c(1,2)),
    method = "t.test",
    bracket.size = 0.5,
    size=5
  )+
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.15)))+
  #coord_cartesian(ylim = c(15,55))+
  #scale_fill_brewer(palette="BuPu")
  scale_fill_manual(values = rep(color.panel4[2],2))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.text.x = element_text(angle=90,vjust=0.5,hjust=1)
  )+
  xlab ("")+
  
  stat_n_text(aes_string(x=colnames(subset_melted)[x.No], 
                         y=colnames(subset_melted)[y.No]))
dev.off()



###b) Sex distribution between two sets==================
  
  pdf ("Sex_distribution_trainingVSvalidation.pdf",width = 2.5,height = 3)
  
  fill.No=118
  x.No=2
  y.No=2
  
  ggplot(subset_melted,aes_string(fill=colnames(subset_melted)[fill.No], x=colnames(subset_melted)[x.No]))+ 
    geom_bar(color="grey",size=0.05)+
    #geom_dotplot(aes_string(x=colnames(subset_melted)[x.No], y=colnames(subset_melted)[y.No]),binaxis = "y",stackdir="center")+
    #scale_fill_brewer(palette="BuPu")+
    geom_text(stat="count", 
              aes(label=after_stat(count)),
              position = position_stack(vjust = 0.5))+
    scale_fill_manual(values = color.panel)+
    theme_classic()+
    theme(legend.position = "top",
          legend.title = element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size=14)
    )+
    labs(title = "Sex distribution",)+
    ylab("Count")
  
  dev.off()
  
  
  ###c) Responser distribution between two sets===================
  pdf (paste(colnames(training_set)[responser],"_distribution_trainingVSvalidation.pdf",sep=""),width = 2,height = 4)
  
  fill.No=118
  x.No=118
  y.No=responser
  ggplot(subset_melted,aes_string(x=colnames(subset_melted)[x.No], 
                                  y=colnames(subset_melted)[y.No]))+ 
    geom_violin(aes_string(fill=colnames(subset_melted)[fill.No]),
                 color="black",size=0.05,alpha=0.75)+
    geom_dotplot(
      binaxis = "y",
      stackdir="center",
      binwidth = 0.5)+
    stat_compare_means(#t.test default is Welch's t-test, which is best fit.
      #remember to put x,y in aes of ggplot, not under the geom_plot.
      #use this aes(label=after_stat(p.signif)) if want asterisk instead of p-value (p.format).
      aes(label = after_stat(p.signif)),
      comparisons = list(c(1,2)),
      method = "t.test",
      bracket.size = 0.5,
      size=5
    )+
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.15)))+
    #coord_cartesian(ylim = c(15,55))+
    #scale_fill_brewer(palette="BuPu")
    scale_fill_manual(values = rep(color.panel4[2],2))+
    theme_classic()+
    theme(legend.position = "none",
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          axis.text.x = element_text(angle=90,vjust=0.5,hjust=1)
    )+
    xlab ("")+
    
    stat_n_text(aes_string(x=colnames(subset_melted)[x.No], 
                           y=colnames(subset_melted)[y.No]))
  dev.off()
  

 ##2)Linear Model by the training set ===================================
 
  #From previous function, responser=55
  # responser
  # colnames(data)[responser]
  # 
  ####Define plot ranges==========
  #
  #Responser_df<-training_set[,responser]
  
  #
  
  #12BP
  fn_fit_glm(predictor_range = c(4:15),responser = 55,distribution = "gaussian")
  
  #IM
  fn_fit_glm(predictor_range = c(16:38),responser = 55,distribution = "gaussian")
  
  
  #BP+IM
  fn_fit_glm(predictor_range = c(4:15,29,30),responser = 55,distribution = "gaussian")
  
  
  #selected BP+IM+ISS
  fn_fit_glm(predictor_range = c(13,14,21,29,30,54),responser = 55,distribution = "gaussian")

  ####End of individual model comparison============
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  #####(a) Statistical metrics for each individual models=========================
  model_list=c("12BP",
               "23IM",
               "sBP+sIM",
               "sBP+sIM+ISS"
               )
  model_list.df<-tibble(
    model=model_list,
    Responser=c(rep("ICU_days",length(model_list))),
    Predictor_range=list(
                    c(4:15),
                    c(16:38),
                    c(13,14,21,29,30),
                    c(13,14,21,29,30,54)
                    )
    )
  ##Define ranges#
  Responder_ID<-colnames(training_set)[responser]
  Responder_ID
  Responser<-training_set[,responser]
  Responser
  residual_limits_for_plot=c(-15,15)
  fitted_limits_for_plot=c(1,25)
  
  
  #ini
  p <- list()
  AIC.list<-list()
  BIC.list<-list()
  Pval.list<-list()
  McFadden_R_squared.list<-list()
  
  for (i in c(1:length(model_list)))
  { 
    print(paste("Processing model",i,":",model_list[i]))
    #Debug use only
    #i=1
    #length(model_list.df$Data_range[[i]]) ==1
    ####
    
    if (length(model_list.df$Predictor_range[[i]])==1){
      ###Case 1. single predictor
      Predictor<-colnames(training_set)[model_list.df$Predictor_range[[i]]]
      Formula <- as.formula(paste(Responser_ID,"~",Predictor)) 
      multi.fit<-glm(formula = Formula,
                     family=Distribution,
                     data=training_set)
    } 
    else {
      ###Case 2. multiple predictor
      multi.fit<-glm(Responser ~ ., family=Distribution,
                     data=training_set[,c(model_list.df$Predictor_range[[i]])])
    }
    #add metrics for the model here
    AIC.list[i]<-AIC(multi.fit)
    BIC.list[i]<-BIC(multi.fit)
    #Null deviance
    Null.deviance<-with(summary(multi.fit), null.deviance)
    #Deviance
    Deviance<-with(summary(multi.fit), deviance)
    #Degree of freedom
    Df<-with(summary(multi.fit), df)[1]-1  
    Pval.list[i]<-pchisq(q=Null.deviance-Deviance, df=Df,lower.tail = F)
    McFadden_R_squared.list[i]<-1-Deviance/Null.deviance
    
    #store plots here
    p[[i]]<-fn_plot_Residual(reg_model=multi.fit,
                             rlim=residual_limits_for_plot,flim=fitted_limits_for_plot,
                             caption=paste(model_list.df$model[i],"->",Responser_ID))
  }
  
  #####(b) Create the plot for individual metric: residual distr. residual vs fitted, p value for each predictor=========
  pdf ("Model_goodness_evaluation.Training.set_0321.pdf",width = 15,height = 15)
  do.call(grid.arrange,p)
  dev.off()
  
  
{
  #AIC, AIC (Akaike Information Criterion) is a measure of the quality of a statistical model.
  #Lower the better fit.
  #AIC(multi.fit)
  #AIC.list
  
  #BIC, The Bayesian Information Criterion, often abbreviated BIC, is a metric that is used to compare the goodneess of models.
  #Lower the better.
  #BIC(multi.fit)
  #BIC.list
  
  #calculate McFadden's R-squared for model
  #McFadden’s R-Squared = 1 – (log likelihoodmodel / log likelihoodnull)
  #McFadden_R_squared=1-Deviance/Null.deviance
  #This is how much the model explains the data.
  #In practice, values over 0.40 indicate that a model fits the data very well
  #McFadden_R_squared.list
  
  #p value: Goodness of Fit Test. calculated from Chi-squared
  #lower.tail: If TRUE, the probability to the left of q in the Chi-Square distribution is returned. If FALSE, the probability to the right of q in the Chi-Square distribution is returned. Default is TRUE.
  #p_value_model_fit<-pchisq(q=Null.deviance-Deviance, df=Df,lower.tail = F)
  #Pval.list
  
  #Add metrics to data frame
  model_list.df["AIC"]<-unlist(AIC.list)
  model_list.df["BIC"]<-unlist(BIC.list)
  model_list.df["R^2"]<-unlist(McFadden_R_squared.list)
  model_list.df["Pval"]<-unlist(Pval.list)
  model_list.df["-log10Pval"]<-(-log10(unlist(Pval.list)))
  
  #####(c) Data output: overall metrics: AIC, BIC, R-squared, P-value============
  write.csv(data.frame(lapply(model_list.df, as.character), stringsAsFactors=FALSE),"glm_model_metrics_TrainingSet.csv")
  #R2>0.4 meaning a good fit.
  
  ##reshape the metrics data frame##
  model_list.df.melted<-melt(model_list.df,
                             measure.vars = colnames(model_list.df)[c(4:6,8)])
  #convert the group (character) to factor type.
  model_list.df.melted$variable<-factor(model_list.df.melted$variable,levels =c("AIC","BIC","R^2","-log10Pval"))
  
  #AIC and BIC plot
  a<-ggplot(subset(model_list.df.melted,variable %in% c("AIC","BIC")))+
    geom_col(aes(fill=variable,y=model,x=value),position="dodge")+
    # geom_text(aes(label=round(value,0),y=model,x=value,group=variable),
    #           position = position_dodge(width = 1),
    #           colour="#595959")+
    scale_fill_manual(values = color.panel1)+
    theme_classic()+
    theme(legend.position = "right",
          legend.direction = "vertical",
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size=14) ,
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)
    )+
    labs(fill="Metrics",title = "Fitness of model to the dataset \n (Lower is better)",y="")+
    coord_cartesian(xlim=c(100,200))
  #Pvalue
  b<-ggplot(subset(model_list.df.melted,variable %in% "-log10Pval"))+
    geom_col(aes(fill=variable,y=model,x=value),position="dodge")+
    geom_text(aes(label=round(value,2),y=model,x=value),
              position = position_stack(vjust = 0.9),
              colour="#595959"
    )+
    scale_fill_manual(values = color.panel1[2])+
    theme_classic()+
    theme(legend.position = "none",
          legend.direction = "vertical",
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size=14) ,
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)
    )+
    labs(fill="Metrics",title = "Fitness of model to the dataset \n (Higher is better)",
         x="-log10Pvalue",y="")
    #coord_cartesian(xlim=c(0,15))
  #R squared plot
  c<-ggplot(subset(model_list.df.melted,variable %in% "R^2"))+
    geom_col(aes(fill=variable,y=model,x=value),position="dodge")+
    #geom_vline(xintercept=0.4, linetype="longdash",color=color.panel4[3])+
    geom_text(aes(label=sprintf("%.2f", round(value,2)),y=model,x=value),
              position = position_stack(vjust = 0.9),
              colour="#595959"
    )+
    scale_fill_manual(values = color.panel1[2])+
    theme_classic()+
    theme(legend.position = "none",
          legend.direction = "vertical",
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5,size=14) ,
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)
    )+
    labs(fill="Metrics",title = "McFadden’s R-Squared \n (Higher is better)",
         x="McFadden’s R-squared",y="")
  
  figure<-ggarrange(a,b,c,labels = c("A","B","C"),ncol = 3,nrow = 1)
 }
  #####(d) Comparison of models AIC, BIC, R-squared, P-value==============
  pdf ("Multiple model_metrics_Trainingset.pdf",width = 10,height = 4)
  figure  
  dev.off()

  
  ####Best linear model:  ============
  ##The best fit
  #copy the best fitted range of predictors
  best_fit_predictors_ID=c(5,14,16,22,29,34,38)
  fn_fit_glm(predictor_range = best_fit_predictors_ID,responser = 55,distribution = "gaussian")
  
 

  
  fn_plot_corr_preditedVSactual(reg_model=multi.fit_glm_7predictorsto55,predictor_range=best_fit_predictors_ID,responser=55,model_type="glm",if_export=F)
  
  
  fn_plot_validation_predVSactual(model=multi.fit_glm_4predictorsto55,val_set = testing_set)
  
 
  
  
  
  
  
  
  fn_fit_glm(predictor_range = best_fit_predictors_ID,responser = 55,distribution = "gaussian")
  
  
  ##glm#############
  
  ##nlm############
  
  
  
  ##3)Non-linear Model by the training set ===================================
  #Definition
  
  
  
  #Test
  
  i=4;k=55
  fn_fit_nls(predictor_range = i,responser = k,1,1)
  #
  fn_plot_Residual(multi.fit_last_nls,caption=paste(Predictor,"to\n",colnames(data)[responser],sep = " "))
  
  
  fn_plot_corr_preditedVSactual(multi.fit_last_nls,model_type = "micmen",
                                predictor_range = i,responser=k, if_export=F)
  res_plot
  fvsr_plot
  pval_plot
  
  
  print(paste("Analyzing_",n,":",i,colnames(data)[i],"to",k,colnames(data)[k]))
  
  
 
  #
  
  
#4) General additive model (GAM) in package mgcv#################

gam.fit.m<-gam(data=training_set, 
               as.formula(paste(colnames(training_set)[55], "~",
                                paste("s(",colnames(training_set)[c(4:36)],")", collapse = "+"),
                                sep = "")),
               method = "REML")
summary(gam.fit.m)
  
gam.fit.m_BP2_IM5<-gam(data=training_set, 
               as.formula(paste(colnames(training_set)[55], "~",
                                paste("s(",colnames(training_set)[c(5,14,16,22,29,34,38)],")", collapse = "+"),
                                sep = "")),
               method = "REML")
summary(gam.fit.m_BP2_IM5)

gam.fit.m_BP1<-gam(data=training_set, 
               as.formula(paste(colnames(training_set)[55], "~",
                                paste("s(",colnames(training_set)[c(5)],")", collapse = "+"),
                                sep = "")),
               method = "REML")
summary(gam.fit.m_BP1)



fn_plot_Residual(gam.fit.m_BP1,pval = F)
fn_plot_Residual(gam.fit.m_BP2_IM5,pval = F)

predicted<-predict(gam.fit.m_BP2_IM5,newdata=testing_set)
predicted  

fn_plot_validation_predVSactual(model=gam.fit.m_BP1,val_set = testing_set)
  
  

#########################
library(caret)
folds=createF


data[c(1:48),118]=sapply(data[c(1:48),55],function(x){
  if(x<=median(data[c(1:48),55])){"0"}else{"1"}
})
colnames(data)[118]<-"hospital.stay.length"
data[118]
#logistic regression#################
ggplot(training_set,aes_string(x=colnames(training_set)[56],y=colnames(training_set)[117]))+
  geom_jitter(height=0.05,alpha=0.2)+
  geom_smooth(method="glm",method.args=list(family="binomial"))
  theme_minimal()
  
plot(data[c(1:48),c(4:15)])
  
  
Formula1=as.formula(paste(colnames(training_set)[117],"~",
                          colnames(training_set[6]),sep = ""))
lr.fit <- glm(formula=Formula1,
              data=training_set,
              family="binomial")
  
summary(lr.fit)

