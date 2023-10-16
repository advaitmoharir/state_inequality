# ---------------------------------------------------------------
# Title: 01_scatter.R
# Purpose: Creates scatter plots for ES Principal Comps and MPI
# Author: Advait Moharir, Rajendran Narayanan
# Status: In Progress
# ---------------------------------------------------------------

# Input raw files

es<-read.csv("01_raw/es_raw.csv")
mpi<-read.csv("01_raw/mpi_raw.csv")%>%select(state,mpi_2015,mpi_2019)

# Merge and take log nsdp

data<-es%>%
  left_join(mpi,by='state')%>%
  mutate(log_nsdp=log(pcap_nsdp))



# ---------------------------------------------------------------
# SECTION 1: PCA CORRELATES
# ---------------------------------------------------------------

# Drop TFR and Life Exp due to missing values, as well as gdp and gini

pca<-data%>%
  select(-c(3,4,7,14:17))

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
pces<-prcomp(pces, scale=T)


# Select relevant data for scatter plots

corr<-data.frame(data$state, data$state_code,data$mpi_2015,
                 data$mpi_2019 ,pces$x[,1],pces$x[,2],pces$x[,3])
colnames(corr)<-c("state", "state_code", "mpi_2015", "mpi_2019",
                  "pc1", "pc2", "pc3")

# Correlation: PC1 and MPI

p1<-ggscatter(corr, "mpi_2015", "pc1", label="state_code",
              add = "reg.line",  # Add regressin line
              add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
              xlab="MPI (2015)", ylab="PC1")


p2<-ggscatter(corr, "mpi_2019", "pc1", label="state_code",
              add = "reg.line",
              add.params = list(color = "blue", fill = "lightgray"),  xlab="MPI (2019)", 
              ylab="PC1")

# save plots

fig<-plot_grid(p1,p2, ncol=2, nrow=1)

ggsave("04_exhibits/pc_mpi_correlates.jpg",dpi=1000, width=8, height=4)

# ---------------------------------------------------------------
# SECTION 2: CLUSTERING
# ---------------------------------------------------------------

# Calculate Manhattan distance for PC1 

df_cluster<-corr%>%
  select(pc1, pc2, pc3)

rownames(df_cluster)<-corr$state

# Choose clustering connecting and make plot

dist<-get_dist(df_cluster,method="manhattan")
hc <- hclust(d = dist, method = "ward.D2")

# Scale the PCs


# 6 clusters
p5<-fviz_dend(hc,rect=T, scale=T,horiz=T, cex = 0.5, k=6, main="")
ggsave("04_exhibits/cluster_6.jpeg",width=10, height=8, dpi=700)

# 9 clusters
p6<-fviz_dend(hc,rect=T, scale=T,horiz=T, cex = 0.5, k=9, main="")
ggsave("04_exhibits/cluster_9.jpeg",width=10, height=8, dpi=700)

# Rank clusters based on mean/median MPI

corr<-corr%>%
  drop_na()%>%
  mutate(cluster=ifelse(state_code %in% c("AR", "JH", "ML"),1,0),
         cluster=ifelse(state_code %in% c("MP", "BR", "UP"),2,cluster),
         cluster=ifelse(state_code %in% c("GJ", "UT", "RJ"),3,cluster),
         cluster=ifelse(state_code %in% c("AS", "CT", "OR", "TG"),4,cluster),
         cluster=ifelse(state_code %in% c("AP", "TN"),5,cluster),
         cluster=ifelse(state_code %in% c("KL"),6,cluster),
         cluster=ifelse(state_code %in% c("KA", "GA", "WB", "HP", "MH"),7,cluster),
         cluster=ifelse(state_code %in% c("NL", "MZ", "SK"),8,cluster),
         cluster=ifelse(state_code %in% c("MN", "PB","TR", "HR"),9,cluster))

# Calculate groupwise means

means<-corr%>%
  group_by(cluster)%>%
  arrange(cluster)%>%
  summarise_at(vars(mpi_2015), list(avg_mpi = mean, median_mpi=median))%>%
  arrange(avg_mpi)
  