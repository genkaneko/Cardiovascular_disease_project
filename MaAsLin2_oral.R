library(MatchIt)
library(openxlsx)
library(tableone)
library(Maaslin2)
library(ComplexHeatmap)
library(circlize)
library(tidyverse)
library(reshape2)
library(viridis)

meta=as.data.frame(read.csv("oral_AF_meta_nonpaf_newHTN.csv",row.names = 1))
rownames(meta)=meta$metas
fdata=as.data.frame(t(read.xlsx("../saliva.motus3.sp.relative_abundance.xlsx",rowNames = T,check.names = F,sep.names = " ")))
new_old_name=as.data.frame(read.csv("smoking_abundance.csv",row.names = 1))
new_old_name=new_old_name[!is.na(new_old_name$old),]
rownames(new_old_name)=new_old_name$old

diskey=c("AF",
         "Non_PAF",
         "PAF",
         "IHD",
         "CHF",
         "PAD",
         "Stroke"
)

set.seed(123)
meta_data <- meta  # ファイル名を適宜変更
data=fdata[rownames(meta_data),]

identical(rownames(data),rownames(meta_data))
data=data[,(colSums(data>0)/nrow(data))>0.1]
data=data[,(colMeans(data))>0.0001]
#菌種名に[]:などの符号がある化ける、ここはお通し番号に変える
spname=data.frame(feature=paste0("X",1:ncol(data)),name=colnames(data))
colnames(data)=spname$feature
rownames(spname)=spname$feature

pr=NULL
fixkey=c("Age","sex","BMI","newHTN","cur.ever.smoking","Diabetes.mellitus","Dyslipidemia" ,"Alcohol","Antibiotics","Proton.pump.inhibitors" )

for (i in diskey){
  set.seed(123)
  
  tempmeta=meta_data
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
  colnames(tempp)[1]="old"
  tempp$feature=new_old_name[tempp$old,]$new
  pr=rbind(pr,tempp)
}



for (i in diskey){
  set.seed(123)
  
  tempmeta=meta_data[,c(i,"Health")]
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
  colnames(tempp)[1]="old"
  tempp$cate="AFvsHealth"
  tempp$fix="nofix"
  tempp$feature=new_old_name[tempp$old,]$new
  pr=rbind(pr,tempp)
}


pr=pr[,c(-3,-7)]
write.csv(pr,"./oral_7cohort_motusp_MAA_20250810.csv",row.names = F)
