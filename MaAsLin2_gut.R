# MaAsLin2 analysis of gut species (mOTUs v3); creates the inputs for ED_Fig1_cde.R and Fig3d.R.
# cate: PSM = matched cases vs controls; 5137 = all participants, adjusted for 10 covariates;
#       AFvsHealth = cases vs healthy participants (label used for all diseases); 5137single = single-factor models.
# Species with prevalence > 10% and mean abundance > 0.0001 are tested (normalization = "NONE", standardize = TRUE).
# Required packages: Maaslin2, openxlsx

library(openxlsx)
library(Maaslin2)

meta5137=as.data.frame(read.csv("5137_AF_meta_nonpaf_newHTN.csv",row.names = 1))
meta=as.data.frame(read.csv("6cohort_matchdata.csv",check.names = F))
fdata=as.data.frame(read.xlsx("NewSpeciesMOTU3_5999.xlsx",rowNames = T,check.names = F,sep.names = " "))
new_old_name=as.data.frame(read.csv("SP_new_old.csv",row.names = 1))
rownames(new_old_name)=new_old_name$new

# Define function
modify_rownames <- function(df) {
  old_names <- rownames(df)
  new_names <- sapply(old_names, function(name) {
    num_name <- suppressWarnings(as.numeric(name))
    if (!is.na(num_name)) {
      return(as.character(num_name))  # Drops leading zeros
    } else {
      return(name)  # Keep the original string
    }
  })
  rownames(df) <- new_names
  return(df)
}

# Align the sample order with the metadata and check it
fdata=modify_rownames(fdata)

set.seed(123)
meta_data <- meta 
data=fdata[rownames(meta5137),]

identical(rownames(data),rownames(meta5137))
data=data[,(colSums(data>0)/nrow(data))>0.1]
data=data[,(colMeans(data))>0.0001]
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

# Set seed
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
write.csv(pr,"./7cohort_motusp_MAA.csv",row.names = F)

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


write.csv(pr,"./7cohort_motusp_singlefactor.csv",row.names = F)
