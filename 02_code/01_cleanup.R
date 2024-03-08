# ---------------------------------------------------------------
# Title: 01_scatter.R
# Purpose: Creates scatter plots for ES Principal Comps and MPI
# Author: Advait Moharir, Rajendran Narayanan
# Status: In Progress
# ---------------------------------------------------------------

# Input raw files

es_new<-read.csv("01_raw/es_updated.csv")
es_old<-read.csv("01_raw/es_original.csv")
mpi<-read.csv("01_raw/mpi_raw.csv")%>%select(state,mpi_2015,mpi_2019)

# Merge and take log nsdp

data<-es_new%>%
  left_join(mpi,by='state')%>%
  mutate(log_nsdp=log(pcap_nsdp))



# ---------------------------------------------------------------
# SECTION 1: ES REPLICATION (Updated)
# ---------------------------------------------------------------

# vector of var names
x_names<-c("gini")
y_names<-c("life_expec","imr", "murder", "birth", "mental_health","tfr")

#vector of acis labels
# Define custom axis labels
x_labels <- c("gini" = "Gini Index")
y_labels <- c("life_expec" = "Life Expectancy", "imr" = "Infant Mortality Rate", 
              "murder" = "Murder Rate", "birth"="Birth Rate", "death"= "Death Rate"
              , "tfr"="Total Fertility Rate", "mental_health"="Mental health")

# init a plot list

plot_list<-list()

#with kerala

for (x_name in x_names) {
  for (y_name in y_names) {
    
    plot_name <- paste(x_name, y_name, sep = "_")
    # Create a ggscatter plot for the current x-y pair
    plot_list[[plot_name]] <- ggscatter(data = es_new, 
                   x = x_name, y = y_name,
                   title = "",
                   xlab = x_labels[x_name], ylab = y_labels[y_name],  add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int =F, # Add confidence interval
                   cor.coef = TRUE,  # Add correlation coefficient. see ?stat_cor
    cor.coeff.args = list(method = "pearson"))
    
  }
}

# Put all in grid

grid <- plot_grid(plotlist = plot_list, ncol = 3)
title <- ggdraw() +
  draw_label("With Kerala", size = 20, fontface = "bold", x = 0.5)

# Combine the title and the grid of plots
grid_with_title <- plot_grid(title, grid, ncol = 1, rel_heights = c(0.1, 0.9))
ggsave("04_exhibits/combined_plots_kerala.pdf", grid_with_title, width = 21, height = 15, units = "cm")

#without kerala

es_nk<-es_new[-13,]
plot_list2<-list()
#with kerala

for (x_name in x_names) {
  for (y_name in y_names) {
    
    plot_name <- paste(x_name, y_name, sep = "_")
    # Create a ggscatter plot for the current x-y pair
    plot_list2[[plot_name]] <- ggscatter(data = es_nk, 
                                        x = x_name, y = y_name,
                                        title = "",
                                        xlab = x_labels[x_name], ylab = y_labels[y_name],  add = "reg.line",  # Add regressin line
                                        add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                                        conf.int =F, # Add confidence interval
                                        cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                                        cor.coeff.args = list(method = "pearson"))
    
  }
}

# Put all in grid

grid <- plot_grid(plotlist = plot_list2, ncol = 3)
title <- ggdraw() +
  draw_label("Without Kerala", size = 20, fontface = "bold", x = 0.5)

# Combine the title and the grid of plots
grid_with_title <- plot_grid(title, grid, ncol = 1, rel_heights = c(0.1, 0.9))
ggsave("04_exhibits/combined_plots_nokerala.pdf", grid_with_title, width = 21, height = 15, units = "cm")








# ---------------------------------------------------------------
# SECTION 1: PCA CORRELATES
# ---------------------------------------------------------------

# Drop TFR and Life Exp due to missing values, as well as gdp and gini

pca<-data%>%
  select(-c(3,4,7,14))

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
# SECTION 2: CLUSTERING BASED ON THREE DATASETS
# ---------------------------------------------------------------

#Pull dataframes

cluster_pc<-corr%>%
  select(pc1)
corrjk<-corr[-10,]
cluster_mpi<-corrjk %>%
  select(mpi_2019)
cluster_shdi<-comp%>%
  select(shdi)


rownames(cluster_pc)<-corr$state
rownames(cluster_mpi)<-corrjk$state
rownames(cluster_shdi)<-comp$state


# Choose clustering connecting and make plot

dist_pc<-get_dist(cluster_pc,method="manhattan")
dist_mpi<-get_dist(cluster_mpi,method="manhattan")
dist_shdi<-get_dist(cluster_shdi,method="manhattan")
  
hc_pc <- hclust(d = dist_pc, method = "ward.D2")
hc_mpi <- hclust(d = dist_mpi, method = "ward.D2")
hc_shdi <- hclust(d = dist_shdi, method = "ward.D2")
# Scale the PCs


# 6 clusters
p5<-fviz_dend(hc_pc,rect=T, scale=T,horiz=T, cex = 0.5, k=6, main="")
ggsave("04_exhibits/cluster_pc.jpeg",width=10, height=8, dpi=700)
p6<-fviz_dend(hc_mpi,rect=T, scale=T,horiz=T, cex = 0.5, k=6, main="")
ggsave("04_exhibits/cluster_mpi.jpeg",width=10, height=8, dpi=700)
p7<-fviz_dend(hc_shdi,rect=T, scale=T,horiz=T, cex = 0.5, k=6, main="")
ggsave("04_exhibits/cluster_shdi.jpeg",width=10, height=8, dpi=700)


# ---------------------------------------------------------------
# SECTION 3: SHDI
# ---------------------------------------------------------------

shdi<-read_csv("01_raw/shdi_panel.csv")%>%
  select(3,6,8:36)%>%
  filter(year>1999)

p1<-shdi%>%
  filter(region %in% c("Bihar", "Maharashtra", "Kerala", "Gujarat"))%>%
  rename(state=region)%>%
  ggplot(aes(x=year,y=shdi, color=state))+
  geom_line()+theme_cowplot()+theme(legend.position="bottom")+
  scale_x_continuous(n.breaks=15) +
  scale_y_continuous(n.breaks=10)+ylab(" State HDI")+xlab("")

ggsave("04_exhibits/shdi.pdf",width=9, height=5, dpi=700)

comp<-shdi%>%
  rename(state=region)%>%
  filter(year==2019)%>%
  left_join(es_new,by=c("state"))%>%
  select(state,shdi,gini, incindex)%>%
  drop_na()

p2<-ggscatter(comp, "gini", "shdi", label="state",
              add = "reg.line",
              add.params = list(color = "blue", fill = "lightgray"),  xlab="Gini", 
              ylab="State HDI",  cor.coef = TRUE,  # Add correlation coefficient. see ?stat_cor
              cor.coeff.args = list(method = "pearson"))
ggsave("04_exhibits/shdi_gini.pdf",width=9, height=5, dpi=700)

p2<-ggscatter(comp, "gini", "incindex", label="state",
              add = "reg.line",
              add.params = list(color = "blue", fill = "lightgray"),  xlab="Gini", 
              ylab="State HDI",  cor.coef = TRUE,  # Add correlation coefficient. see ?stat_cor
              cor.coeff.args = list(method = "pearson"))
ggsave("04_exhibits/shdi_nsdp.pdf",width=9, height=5, dpi=700)


  