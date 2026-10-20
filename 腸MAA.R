setwd("/Users/gizen/Documents/AF/20250810")
library(MatchIt)
library(openxlsx)
library(tableone)
library(Maaslin2)
library(ComplexHeatmap)
library(circlize)
library(tidyverse)
library(reshape2)
library(viridis)

tempmeta=as.data.frame(read.csv("../5137_AF_meta_nonpaf_newHTN.csv",row.names = 1))
meta=as.data.frame(read.csv("../6cohort_matchdata.csv",check.names = F))
fdata=as.data.frame(read.xlsx("/Users/gizen/Documents/smoking/motu3/NewSpeciesMOTU3_5999.xlsx",rowNames = T,check.names = F,sep.names = " "))
new_old_name=as.data.frame(read.csv("/Users/gizen/Documents/smoking/motu3/SP_new_old.csv",row.names = 1))
rownames(new_old_name)=new_old_name$new



#METAdataとの順番の揃いと確認
fdata=modify_rownames(fdata)


set.seed(123)
meta_data <- meta  # ファイル名を適宜変更
data=fdata[rownames(meta5137),]

identical(rownames(data),rownames(meta5137))
data=data[,(colSums(data>0)/nrow(data))>0.1]
data=data[,(colMeans(data))>0.0001]
#菌種名に[]:などの符号がある化ける、ここはお通し番号に変える
spname=data.frame(feature=paste0("X",1:ncol(data)),name=colnames(data))
colnames(data)=spname$feature
rownames(spname)=spname$feature
pr=NULL
diskey=c("AF",
         "Non_PAF",
         "PAF",
         "IHD",
         "CHF",
         "PAD",
         "Stroke"
)
##MAA2ではシードの固定が必要ないですが、念のため固定している
for (i in diskey){
  set.seed(123)
  
  tempmeta=meta_data[meta_data$category==i,]
  tempp=NULL
  
  rownames(tempmeta)=tempmeta$id
  temp=Maaslin2(data[rownames(tempmeta),],tempmeta,
                standardize = T,
                "output",
                fixed_effects = c(i),
                min_abundance = 0.0,
                min_prevalence = 0,
                normalization = "NONE",
                plot_heatmap = FALSE,
                plot_scatter = FALSE
  )
  tempp=temp$results
  tempp=tempp[tempp$metadata %in% i,]
  
  tempp=merge(spname,tempp,by="feature")
  tempp=tempp[,-1]
  tempp$cate="PSM"
  tempp$fix="Nofix"
  colnames(tempp)[1]="feature"
  tempp$old=new_old_name[tempp$feature,]$old
  pr=rbind(pr,tempp)
}





fixkey=c("Age","sex","BMI","newHTN","cur.ever.smoking","Diabetes.mellitus","Dyslipidemia" ,"Alcohol","Antibiotics","Proton.pump.inhibitors" )

for (i in diskey){
  set.seed(123)
  
  tempmeta=meta5137
  tempp=NULL
  tempmeta=tempmeta[,c(i,fixkey)]
  tempmeta=na.omit(tempmeta)
  temp=Maaslin2(data[rownames(tempmeta),],tempmeta,
                standardize = T,
                "output",
                fixed_effects = c(i,fixkey),
                min_abundance = 0.0,
                min_prevalence = 0,
                normalization = "NONE",
                plot_heatmap = FALSE,
                plot_scatter = FALSE
  )
  tempp=temp$results
  tempp=tempp[tempp$metadata %in% i,]
  
  tempp=merge(spname,tempp,by="feature")
  tempp=tempp[,-1]
  tempp$cate="5137"
  tempp$fix=paste(fixkey,collapse = ",")
  colnames(tempp)[1]="feature"
  tempp$old=new_old_name[tempp$feature,]$old
  pr=rbind(pr,tempp)
}



for (i in diskey){
  set.seed(123)
  
  tempmeta=meta5137[,c(i,"Health")]
  tempmeta=na.omit(tempmeta)
  tempmeta=tempmeta[(tempmeta[,i]==1)|(tempmeta$Health==1),]
  tempp=NULL
  
  temp=Maaslin2(data[rownames(tempmeta),],tempmeta,
                standardize = T,
                "output",
                fixed_effects = c(i),
                min_abundance = 0.0,
                min_prevalence = 0,
                normalization = "NONE",
                plot_heatmap = FALSE,
                plot_scatter = FALSE
  )
  tempp=temp$results
  tempp=tempp[tempp$metadata %in% i,]
  
  tempp=merge(spname,tempp,by="feature")
  tempp=tempp[,-1]
  colnames(tempp)[1]="feature"
  tempp$old=new_old_name[tempp$feature,]$old
  tempp$cate="AFvsHealth"
  tempp$fix="nofix"
  
  pr=rbind(pr,tempp)
}


pr=pr[,c(-3,-7)]
write.csv(pr,"./7cohort_motusp_MAA_20250810.csv",row.names = F)

pr=NULL
singlekey=c("AF",
            "Non_PAF",
            "PAF",
            "IHD",
            "CHF",
            "PAD",
            "Stroke","Age","sex","BMI","newHTN","cur.ever.smoking","Diabetes.mellitus","Dyslipidemia" ,"Alcohol","Antibiotics","Proton.pump.inhibitors")
for (i in singlekey){
  set.seed(123)
  
  tempmeta=meta5137[,c(i),drop=F]
  tempmeta=na.omit(tempmeta)
  tempp=NULL
  
  temp=Maaslin2(data[rownames(tempmeta),],tempmeta,
                standardize = T,
                "output",
                fixed_effects = c(i),
                min_abundance = 0.0,
                min_prevalence = 0,
                normalization = "NONE",
                plot_heatmap = FALSE,
                plot_scatter = FALSE
  )
  tempp=temp$results
  tempp=tempp[tempp$metadata %in% i,]
  
  tempp=merge(spname,tempp,by="feature")
  tempp=tempp[,-1]
  tempp$cate="5137single"
  tempp$fix="nofix"
  colnames(tempp)[1]="feature"
  tempp$old=new_old_name[tempp$feature,]$old
  pr=rbind(pr,tempp)
}


write.csv(pr,"./7cohort_motusp_singlefactor_20250806.csv",row.names = F)