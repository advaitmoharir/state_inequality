# ---------------------------------------------------------------
# Title: 01_exhibits.R
# Purpose: Creates exhibits for India Forum article
# Author: Advait Moharir, Rajendran Narayanan
# Status: Complete
# ---------------------------------------------------------------


# ---------------------------------------------------------------
# SECTION 1: ES REPLICATION (Updated)
# ---------------------------------------------------------------

# Input raw files

es_new<-read.csv("01_data/es_updated.csv") # ES crossection (2019)
es_old<-read.csv("01_data/es_original.csv") # ES crossection (2016-17)


# Take log of per capita nsdp

data<-es_new%>%
  mutate(log_nsdp=log(pcap_nsdp))

# ES Replication

# vector of var names

x_names<-c("gini")
y_names<-c("life_expec","imr", "murder", "birth", "mental_health","tfr")

# vector of axis labels

x_labels <- c("gini" = "Gini Index")
y_labels <- c("life_expec" = "Life Expectancy", "imr" = "Infant Mortality Rate", 
              "murder" = "Murder Rate", "birth"="Birth Rate", "death"= "Death Rate"
              , "tfr"="Total Fertility Rate", "mental_health"="Mental health")

# init an plot list

plot_list<-list()

# with kerala 

# loop over x and y variables and make scatter plots

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
ggsave("03_exhibits/fig2a.jpeg", grid_with_title, width = 23, height = 15, units = "cm", dpi=1500)

# without kerala

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
ggsave("03_exhibits/fig2b.jpeg", grid_with_title, width = 23, height = 15, units = "cm",dpi=1500)



# ---------------------------------------------------------------
# SECTION 2: SHDI
# ---------------------------------------------------------------

# Read SHDI data
shdi<-read_csv("01_raw/shdi_panel.csv")%>%
  select(3,6,8:36)%>%
  filter(year>2000)

# set x axis range

breaks.vec <- seq(min(shdi$year), max(shdi$year), by = 4)

# Figure-1: Trends in SHDI for select states

p1<-shdi%>%
  filter(region %in% c("Bihar", "Maharashtra", "Kerala", "Gujarat"))%>%
  rename(state=region)%>%
  ggplot(aes(x=year,y=shdi, color=state))+
  scale_color_manual(values=c('red', 'blue','darkgreen', 'darkorange'))+
  geom_line()+theme_pubr()+theme(legend.position="bottom")+
  scale_x_continuous(breaks=breaks.vec) +
  scale_y_continuous(n.breaks=10)+ylab("State HDI")+xlab("")+labs(color=NULL)

ggsave("03_exhibits/fig1.jpeg",width=9, height=5, dpi=1500)

# Figure-3: SHDI v/s Gini (2011) correlation

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
ggsave("03_exhibits/fig3.jpeg",width=9, height=5, dpi=1500)




# ---------------------------------------------------------------
# SECTION 3: PCA
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

# Drop state names to run pca

pces<-pca%>%
  select(-c(1,2))
rownames(pces)<-pca[,1]
rownames(pces)[20]<-"Odisha"
res<-prcomp(pces,scale=T)

# Figure-5 Screeplot

p5<-fviz_eig(res,addlabels = TRUE)+theme_pubr()+ggtitle("")
ggsave("03_exhibits/fig5.jpeg",width=9, height=5, dpi=1500)

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

ggsave("03_exhibits/fig6.jpeg",width=9, height=5, dpi=1500)

# Select relevant data for scatter plots

corr<-data.frame(data$state,data$pcap_nsdp,res$x[,1],res$x[,2],res$x[,3])
rownames(corr)<-NULL
colnames(corr)<-c("state", "pcap_nsdp",
                  "pc1", "pc2", "pc3")

corr%>%select(-c(pc3))%>%filter(state %in% c("Mizoram","Goa","Kerala","Sikkim", "Bihar","Uttar Pradesh", "Madhya Pradesh",
                           "Chhattisgarh"))%>%
                arrange(desc(pc1))%>%
                kable(digits=2, col.names = 
        c("State", "Per Capita NSDP", "PC1", "PC2"),format="latex",
      booktabs=T)%>%
  save_kable("03_exhibits/tab1.tex")

# Variable contributions

res.var <- get_pca_var(res)
# res.var$coord          # Coordinates
var_coord <-as.data.frame( res.var$coord[,c(1,2)]) # Contributions to the PCs
rownames(var_coord) <-c("Health Index", "Education Index","Infant Mortality Rate",
                        "Murder Rate","Drug Use","Mental Health Index", "Birth Rate","Death Rate", 
                        "Log (NSDP Per Capita)")
# Output as latex

var_coord%>%arrange(desc(Dim.1))%>%
  kable(digits=2, col.names = 
          c("Variable", "PC1", "PC2"),
          format="latex",
        booktabs=T)%>%
  save_kable("03_exhibits/tab2.tex")

# --------------------------------------------------
# SECTION 4: CLUSTERING
# ---------------------------------------------------------------


#Pull dataframes

cluster_pc<-corr%>%
  select(pc1,pc2,pc3)

# Add states as rownames
rownames(cluster_pc)<-corr$state
rownames(cluster_pc)[20]<-"Odisha"

# Choose Manhattan distance and ward method

dist_pc<-get_dist(cluster_pc,method="manhattan")
hc_pc <- hclust(d = dist_pc, method = "ward.D2")


# 9 clusters

p7<-fviz_dend(hc_pc,rect=T, scale=T,horiz=T, cex = 0.75, k=9,
              k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#ff3333", "blue","orange4","green4","purple"),
              main="",labels_track_height=4)
ggsave("03_exhibits/fig7.jpeg",width=10, height=8, dpi=1500)

# 6 clusters
p8<-fviz_dend(hc_pc,rect=T, scale=T,horiz=T, cex =0.75, k=6,k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#ff3333", "blue"),
              main="", labels_track_height=4)
ggsave("03_exhibits/figa1.jpeg",width=10, height=8, dpi=1500)


#---------------------------------- END------------------------------------------------#












  