pca<-data%>%
  select(-c(3,4,7,14:16))

# Impute missing values for HI and EI using
# predictive mean matching with MICE algo


init <- mice(pca, maxit = 0,seed = 121)
meth <- init$method
predM <- init$predictorMatrix

meth[c("health_index", "edu_index")]= c("pmm","pmm")
predM <- init$predictorMatrix

imputed <- mice(pca, method=meth, predictorMatrix=predM, m=5,seed=121)

# Assign all data with imputed values to 'pca'

pca <- complete(imputed,3)

#
pces<-pca%>%
  select(-c(1,2))
rownames(pces)<-pca[,1]
res<-prcomp(pces,scale=T)

p5<-fviz_eig(res,addlabels = TRUE)+theme_pubr()+ggtitle("")
ggsave("04_exhibits/screeplot.pdf",width=10, height=6, dpi=700)

gr.colours <- c(rgb(1,0.173,0.067),rgb(0.067,0.071,0.627),
                rgb(0.957,0.706,0),
                rgb(0.22,0.463,0.114),
                rgb(0.357,0.059,0),
                rgb(0.275,0.741,0.776))
cols<-viridis(4, alpha = 1, begin = 0, end = 1, direction = 1)


p6<-fviz_pca_ind(res,
                 col.ind = "cos2", # Color by the quality of representation
                 gradient.cols =cols ,
                 repel = TRUE     # Avoid text overlapping
)+theme_minimal() + xlab("PC1") + ylab("PC2")+ggtitle("")

ggsave("04_exhibits/cos2_ind.jpeg",width=10, height=6, dpi=700)

# Select relevant data for scatter plots

corr<-data.frame(data$state,data$pcap_nsdp,res$x[,1],res$x[,2],res$x[,3])
rownames(corr)<-NULL
colnames(corr)<-c("state", "pcap_nsdp",
                  "pc1", "pc2", "pc3")
