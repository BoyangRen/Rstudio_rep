#"Hmisc" returns both coefficients and the p-value
library("Hmisc","ComplexHeatmap","spatstat.utils")


wd<-"C:/Users/renbo/OneDrive/Desktop/Manuscript writing/Data analysis/R"
setwd(wd)
files<-list.files(getwd())
files

data.file<-"Correlation/Trauma_clean_update_20240621_rearragned.csv"
data.file

data<-read.table(data.file,header = T,sep = ",",comment.char = "",check.names = F)
row.names(data) = data[,1]
data<-data[,-1]

colnames(data)
#miRNA: col:4~15
#95% CI: col:116~127



##Correlation study
#syndecan-1(21, Syndecan-1) has the highest correlation (always high no matter samples size)
#Enolase2 (29,Enolase2) has most changes
data_filtered<-data[,c(1:53)][data$Filter=="include",]
#[data$Group=="Trauma",]

length(data_filtered$`BT-01-hsa-miR-199a-3p`)


# data[c(1:48),118]=sapply(data[c(1:48),55],function(x){
#   if(x<=median(data[c(1:48),55])){"0"}else{"1"}




colnames(data_filtered)
cor.mat<-rcorr(as.matrix(data_filtered))
  # Extract the correlation coefficients
#table! just biomarker towards metrics
cor.tab.r<-cor.mat$r[c(1:12),-c(1:12)]
  # Extract p-values
cor.tab.p<-cor.mat$P[c(1:12),-c(1:12)]
colnames(cor.tab.r)
annotation.metrics<-read.csv("Correlation/heatmap_matric annotation.csv",row.names = "outcome")
annotation.metrics



color.g<-c("#42B540","#0099B4","#925E9F","#FDAF91")

#Complex heatmap
cor.mat$n
pdf ("Correlation_trauma_biomarkers_95CIfirst8_trauma38.pdf",width = 16,height = 5.5)
color <- colorRampPalette((c("#1C6CAA", "white", "red")))(100)
ComplexHeatmap::pheatmap(cor.tab.r,cluster_rows = F,cluster_cols = F,color=color,
                         gaps_col=c(14,27,32,36),breaks = seq(-1,1,length.out=101),
                         #annotation_col=annotation.metrics,
                         top_annotation = HeatmapAnnotation(df=annotation.metrics),
                         #treeheight_row=F,
                         row_names_side = "left",
                         heatmap_legend_param = list(title="Pearson's r"),
                         annotation_legend =T,
                         #display_numbers = signif(cor.tab.p,digits=1),
                         cell_fun = function(j, i, x,y,w, h, col) { # add text to each grid
                          val.r=signif(cor.tab.r,digits=2)[i,j]
                          val.p=signif(cor.tab.p,digits=1)[i,j]
                          text=paste(val.r,"\n","(",val.p,")",sep="")
                           grid.text(text,x,y,gp = gpar(fontsize = 5))
                         })
  
 dev.off() 
 windows()
 
 
 ##Individual correlation, details
 library("ggplot2","ggpubr","ggrepel")
 
 
 x.name="BT-11-hsa-miR-95-3p"
 y.name="Syndecan-1"
 
 
 ggplot(data,
        aes(x=.data[[x.name]],
            y=.data[[y.name]]))+
   geom_point()+
   geom_smooth(method=lm,se=FALSE,color="#1C6CAA")+
   ggpubr::stat_cor(label.x.npc = 0.2,label.y.npc = 1)+
   ggpubr::stat_regline_equation(label.x.npc = 0.2, label.y.npc = 0.95)
  
 
 #Use facet_wrap(vars(class), nrow = 4) to make facet figures.
 ggplot(data,
        aes(x=.data[[x.name]],
            y=.data[[y.name]]))+
   geom_point()+
   geom_smooth(method=lm,se=FALSE,color="#1C6CAA")+
   ggpubr::stat_cor(label.x.npc = 0.2,label.y.npc = 1)+
   ggpubr::stat_regline_equation(label.x.npc = 0.2, label.y.npc = 0.95)+
   facet_wrap(vars(Group))
 
 #Label both correlation coefficients and regression R2
 ggplot(data,
        aes(x=.data[[x.name]],
            y=.data[[y.name]]))+
   geom_point()+
   geom_smooth(method=lm,se=FALSE,color="#1C6CAA")+
   ggpubr::stat_cor(label.x.npc = 0.2,label.y.npc = 1)+
   #ggpubr::stat_regline_equation(label.x.npc = 0.2, label.y.npc = 0.95)+
   ggpubr::stat_regline_equation(
     aes(label=paste(..eq.label..,..adj.rr.label..,sep="~~~~")),
     label.x.npc = 0.2, label.y.npc = 0.95
   )+
   facet_wrap(vars(Group))
 
 #Current configuration, one biomarker with large variation in sample quality (95% CI)
 ggplot(data,
        aes(x=.data[[x.name]],
            y=.data[[y.name]]))+
   geom_point()+
   geom_smooth(method=lm,se=FALSE,color="#1C6CAA")+
   ggpubr::stat_cor(label.x.npc = 0.2,label.y.npc = 1)+
   #ggpubr::stat_regline_equation(label.x.npc = 0.2, label.y.npc = 0.95)+
   ggpubr::stat_regline_equation(
     aes(label=paste(..eq.label..,..adj.rr.label..,sep="~~~~")),
     label.x.npc = 0.2, label.y.npc = 0.95
   )+
   facet_wrap(vars(Group))
 
dev.off()
windows()  
  

##quality assessment

quality=sapply(data[,paste("95%CI_",x.name,sep="")],function(x){
  if(x<=mean(na.omit(as.numeric(data[,paste("95%CI_",x.name,sep="")])))){"low 95%CI"}else{"high 95%CI"}
})
  

data.t<-cbind(data,quality)
 

#
ggplot(data.t,
       aes(x=.data[[x.name]],
           y=.data[[y.name]]))+
  geom_point(aes(color=quality))+
  geom_smooth(method=lm,se=FALSE,color="#1C6CAA")+
  ggpubr::stat_cor(label.x.npc = 0.2,label.y.npc = 1)+
  #ggpubr::stat_regline_equation(label.x.npc = 0.2, label.y.npc = 0.95)+
  ggpubr::stat_regline_equation(
    aes(label=paste(..eq.label..,..adj.rr.label..,sep="~~~~")),
    label.x.npc = 0.2, label.y.npc = 0.95
  )+
  facet_wrap(vars(quality))


#Label 95% CI on the chart.
ggplot(data.t,
       aes(x=.data[[x.name]],
           y=.data[[y.name]]))+
  geom_point(aes(color=quality))+
  geom_smooth(method=lm,se=FALSE,color="#1C6CAA")+
  ggpubr::stat_cor(label.x.npc = 0.2,label.y.npc = 1)+
  #ggpubr::stat_regline_equation(label.x.npc = 0.2, label.y.npc = 0.95)+
  ggpubr::stat_regline_equation(
    aes(label=paste(..eq.label..,..adj.rr.label..,sep="~~~~")),
    label.x.npc = 0.2, label.y.npc = 0.95
  )+
  geom_text_repel(label=data[,paste("95%CI_",x.name,sep="")])+
  facet_wrap(vars(quality),scales = "free_x")

#Combined, label 95% CI
ggplot(data.t,
       aes(x=.data[[x.name]],
           y=.data[[y.name]]))+
  geom_point(aes(color=quality))+
  geom_smooth(method=lm,se=FALSE,color="#1C6CAA")+
  ggpubr::stat_cor(label.x.npc = 0.2,label.y.npc = 1)+
  #ggpubr::stat_regline_equation(label.x.npc = 0.2, label.y.npc = 0.95)+
  ggpubr::stat_regline_equation(
    aes(label=paste(..eq.label..,..adj.rr.label..,sep="~~~~")),
    label.x.npc = 0.2, label.y.npc = 0.95
  )+
  geom_text_repel(label=data[,paste("95%CI_",x.name,sep="")],color="#7C878E")
  #facet_wrap(vars(quality),scales = "free_x")

data_t<-data
#[data$Group=="Trauma",]
plot.cor<-function(x.name,y.name,type,sep,color.by){
  Quality=sapply(data_t[,paste("95%CI_",x.name,sep="")],function(x){
    if(x<=0.35){"low 95%CI\n<=35%"}else{"high 95%CI\n>35%"}
  })
  
  #Previous binarization by mean of 95% CI
  # Quality=sapply(data_t[,paste("95%CI_",x.name,sep="")],function(x){
  #   if(x<=mean(na.omit(as.numeric(data_t[,paste("95%CI_",x.name,sep="")])))){"low 95%CI"}else{"high 95%CI"}
  # })
  data.t<-cbind(data_t,Quality)
  
  #default
  if(missing(type)){
    type="c"
    sep=""
  }
  
  if(missing(color.by)){color.by="#424242"}
    else if(color.by=="Quality"){color.by=Quality}
    else if(color.by=="Group"){color.by=Group}

  if(type=="s"){
    print("processing separately")
    p<-ggplot(data.t,
           aes(x=.data[[x.name]],
               y=.data[[y.name]]))+
      geom_point(aes(color=color.by))+
      geom_smooth(method=lm,se=FALSE,color="#1C6CAA")+
      ggpubr::stat_cor(label.x.npc = 0.2,label.y.npc = 1)+
      #ggpubr::stat_regline_equation(label.x.npc = 0.2, label.y.npc = 0.95)+
      ggpubr::stat_regline_equation(
        aes(label=paste(..eq.label..,..adj.rr.label..,sep="~~~~")),
        label.x.npc = 0.2, label.y.npc = 0.95
      )+
      geom_text_repel(label=data_t[,paste("95%CI_",x.name,sep="")],color="#757575")+
      facet_wrap(vars(Group),scales = "free")
  }

  else{
    print ("precessing by default: combined")
  p<-ggplot(data.t,
         aes(x=.data[[x.name]],
             y=.data[[y.name]]))+
    geom_point(aes(color=color.by))+
    geom_smooth(method=lm,se=FALSE,color="#1C6CAA")+
    ggpubr::stat_cor(label.x.npc = 0.2,label.y.npc = 1)+
    #ggpubr::stat_regline_equation(label.x.npc = 0.2, label.y.npc = 0.95)+
    ggpubr::stat_regline_equation(
      aes(label=paste(..eq.label..,..adj.rr.label..,sep="~~~~")),
      label.x.npc = 0.2, label.y.npc = 0.95
    )+
    geom_text_repel(label=data_t[,paste("95%CI_",x.name,sep="")],color="#757575")
  
  }
  ggsave(p, file=paste("cor.",y.name,"-",x.name,".png",sep=""), width = 10,height = 5,units = "in")
  p
}



# [1] "BT-01-hsa-miR-199a-3p"   "BT-04-hsa-miR-148a-3p"  
# [3] "BT-08-hsa-miR-29a-3p"    "BT-12-hsa-miR-145-5p"   
# [5] "BT-07-hsa-miR-27b-3p"    "BT-02-hsa-miR-145-3p"   
# [7] "BT-05-hsa-miR-125a-5p"   "BT-06-hsa-miR-199a-5p"  
# [9] "BT-03-hsa-miR-224-5p"    "BT-09-hsa-let-7c-5p"    
# [11] "BT-10-hsa-miR-125b-2-3p" "BT-11-hsa-miR-95-3p"    

#plot
for (x.term in colnames(data_t)[1:12]){
  print (paste("processing x: ",x.term))
  for (y.term in colnames(data_t)[13:52])
    { #Debug only
      #print (paste("processing y: ",y.term))
      # x.term=colnames(data_t)[1:1]
      # y.term=colnames(data_t)[52:52]
      plot.cor(x.name=x.term,y.name=y.term,
         type="s",sep="Group",color.by="Quality")
     
  }
}

x.name="BT-03-hsa-miR-224-5p"
y.name="BUN"
dev.off()
windows()

plot.cor(x.name="BT-10-hsa-miR-125b-2-3p",y.name="Syndecan-1",
         type="s",sep="Group",color.by="Quality")
