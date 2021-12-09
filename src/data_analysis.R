library(hrbrthemes)    # bubbleplots ggpairs
library(cowplot)       # grid plots
library(GGally)        # ggplot extension for graphical output
library(tibble)        # tidy data
library(dplyr)         # tidy data
library(clustertend)   # checking if clusterable
library(cluster)       # basic package for clustering
library(fpc)           # flexible (optimal) procedure for clustering alternative 1
library(NbClust)       # optimal number of clusters alternative 2
library(factoextra)    # optimal number of clusters alternative 3
library(optCluster)    # optimal number of clusters alternative 4
library(viridis)       # color palette
library(scales)        # scaling the axis
library(gridExtra)
library(FactoMineR)
library(factoextra)
library(ggpubr)
library(ggplot2)      
theme_set(theme_pubr())

#-- read DATA
data=read.csv2("assessment_matrix.csv",sep = "",header = TRUE,row.names = 1)

# list of groups  and number of members for the bubbleplots  
plt_lst=c("all_15","female_4","male_8","mix_3","bachelor_7","teacher_8")

set.seed(123)

###--------- Estimation of the optimal methods and cluster numbers
#
# check if data is clustered 0 is optimal clusterable
h <- clustertend::hopkins(data = data[1:4], n = nrow(data)-1)
h
#-- Flexible Procedures for Clustering
# perform a partitioning around medoids clustering with the number 
# of clusters estimated by optimum average silhouette width
pamk.best <- fpc::pamk(data[1:4],criterion="asw",
                       krange = 3:7,usepam = TRUE, 
                       ns=2, critout=TRUE)


##-- kmeans.best cluster results used for 
##
kmeans.best= fpc::kmeansruns(data[1:4],krange=4,critout=TRUE,
                             runs=10,criterion=c("asw"))
##---
data$kmeans.best=kmeans.best$cluster

plot(cluster::pam(data[1:4], kmeans.best$bestk))
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(cluster::pam(data[1:4], pamk.best$nc))


#-- NbClust Package for determining the best number of clusters
# NbClust package provides 30 indices for determining the number of clusters 
# and proposes to user the best clustering 
# identify optimal cluster number

nb_wardD2 = NbClust::NbClust(data[1:4], distance="euclidean", 
                             min.nc=3, max.nc=7, 
                             method="ward.D2", index="dindex",
                             alphaBeale = 0.05)
nb_kmeans = NbClust::NbClust(data[1:4], distance = "euclidean", 
                             min.nc=3, max.nc=7, 
                             method = "kmeans", 
                             index = "dindex",
                             alphaBeale = 0.05)

nb_best <- NbClust::NbClust(data[1:4], distance = "euclidean",
                            min.nc = 3, max.nc = 7, 
                            method = "kmeans", index ="all",
                            alphaBeale = 0.05)
data$nb_Best.partition=nb_best$Best.partition

# visualize nb_best bar chart
factoextra::fviz_nbclust(nb_best) + theme_minimal() +
  ggtitle("NbClust's optimal number of clusters")

# calculate and visualize elbow method
factoextra::fviz_nbclust(data[1:4], method="wss", FUNcluster=kmeans, iter.max = 30,print.summary = TRUE)+
  theme_classic()+ geom_vline(xintercept = 4 , linetype = 2)+
  labs(title= "K-means") + 
  xlab("Number of Clusters K") +
  ylab("Explained variance %")+
  ggtitle("NbClust's optimal number of clusters")

# optCluster performs statistical and/or biological validation 
# of clustering results and determines the optimal clustering 
# algorithm and number of clusters through rank aggreation.
## Analysis of Count Data using Internal and Stability Validation Measures
res_opt_clust <- optCluster::optCluster(data[1:4], 3:7, clMethods = c("kmeans"),distance = "Kendall",
                                        validation = c("internal","stability"), countData = TRUE)
summary(res_opt_clust)


#--- Visualisation of the cluster results

# visualize data
# panel.hist is a helper function for histograms in the diag of pairs
col="grey"
panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col=col , ...)
}


# cluster panel PAM und Kmeans mit silhoutte Grafik
#KMEANS -----------------------------
km <- factoextra::eclust(data[1:4], k = 4, seed = 123, FUNcluster = "kmeans",
                         hc_metric = "euclidian" ,hc_method = "complete")
km.clus <- factoextra::fviz_cluster(km,main = "kmeans eclust")
km.sil <- factoextra::fviz_silhouette(km)

#PAM CLUSTERING ---------------------
pam <- factoextra::eclust(data[1:4],seed=123, FUNcluster ="pam", k = 4,
                          hc_metric = "euclidian" ,hc_method = "complete")
pam.clus <- factoextra::fviz_cluster(pam,main = "PAM eclust")
pam.sil <- factoextra::fviz_silhouette(pam)

# create panel
list.plot <- list() #list to store the plots
list.plot[[1]] <- km.clus
list.plot[[2]] <- km.sil    
list.plot[[3]] <- pam.clus
list.plot[[4]] <- pam.sil
# plot it
cowplot::plot_grid(plotlist = list.plot, ncol = 2)




# using pairs for kmeans.best fpc package
pairs(data[1:4],cex=2, bg=c(1:4)[data$nb_Best.partition],pch = 21
      ,upper.panel = NULL,diag.panel = panel.hist,
      cex.labels = 1, font.labels = 1)
# GGally ggpairsusing ggplot for kmeans.best fpc package
# first sex subject
GGally::ggpairs(data[c(7,6,9)] , 
                aes(color = factor(nb_Best.partition,levels = 1:4, labels = c("type 3", "type 1","type 2","type 4"))),
                lower=list(continuous='points'),
                axisLabels='show',
                legend = 1,
                upper=list(continuous='blank'), 
                diag=list(continuous="barDiag"),
                title="Types of learning (fpc)") +

  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_fill_viridis_d(alpha = 0.5)

# second dimensons typ 1 = B2 B5 B8 B9 T6 T7 T8
#                  typ 2 = B6 T5 
#                  typ 3 = B3 T3 T4
#                  typ 4 = B1 T1 T2
GGally::ggpairs(data[c(1,2,3,4,8)] , 
                 aes(color = factor(nb_Best.partition)),
                lower=list(discrete = "facetbar"),
                axisLabels='show',
                legend = 1,

                upper=list(discrete='blank'), 
                title="Types of learning (fpc)") +
  scale_fill_viridis(discrete=TRUE,alpha=0.5)
  #theme(legend.position = "bottom", legend.title = element_blank())
  #scale_fill_viridis_d(alpha = 0.5)

### bubble charts for kmeans.best fpc package
## function to generate bubbleplots with ggplot
#https://stackoverflow.com/questions/46413926/bubble-plot-using-ggplot2
bubbleplot = function(data,title){
  g= ggplot(data,
            aes(x = dimensions, y = levels, 
                size = count  , label = count)) +#factor(count)
    geom_point(shape = 21, alpha=0.7,colour ="black",fill="orange") +
    geom_text(size = 3, color="black") +
    scale_size(range = c(6,18), guide = F) +
    scale_y_continuous(expand = expansion(add=.9),limits = c(0,3),oob = squish)+
    scale_x_continuous(labels=c("draft" ,"revise" ,"apply", "evaluate"),expand = waiver())+
    scale_fill_viridis(discrete=TRUE,alpha=0.5, name = "No. of Counts") +
    theme_cowplot()+
    ylab("Levels") +
    xlab("Dimensions") + 
    theme(legend.position = "bottom") + ggtitle(title)
  return(g)
}

## reorganise data for bubbleplots
#library(tibble)
res=lapply(names(data[1:4]),function(x){
  tibble::tibble(levels=data %>% pull(x) ) %>% add_column(dimensions = grep(x, colnames(data))) %>% tibble(sex=data %>% pull(5) )%>% tibble(degree.program=data %>% pull(6))
})
t_data=do.call("rbind", res)

# count per group done each by each for more transparence
all     = t_data %>% group_by(levels, dimensions)%>%  summarize(count=n())
female  = t_data %>% group_by(levels, dimensions)%>% filter(sex == "f")%>%  summarize(count=n())
male    = t_data %>% group_by(levels, dimensions)%>% filter(sex == "m")%>%  summarize(count=n())
mix     = t_data %>% group_by(levels, dimensions)%>% filter(sex == "mix")%>%  summarize(count=n())
bachelor= t_data %>% group_by(levels, dimensions)%>% filter(degree.program == "B")%>%  summarize(count=n())
teacher = t_data %>% group_by(levels, dimensions)%>% filter(degree.program == "T")%>%  summarize(count=n())

# create ggplot bubbleplots 
for (plt in plt_lst){
  split=strsplit(plt,"_")
  name=split[[1]][1]
  no=split[[1]][2]
  assign(paste0("g_",plt),bubbleplot(data = eval(parse(text =name)),paste(name,"n=",no)))
}

# create list and cowplot objects 
cowplot::plot_grid(plotlist = lapply(paste0("g_",plt_lst), function(x) {eval(parse(text =as.name(x)))}), 
                   ncol = 2,nrow = 3)

# ordination plots
res.ca <- FactoMineR::CA(data[1:4], graph = FALSE)
factoextra::fviz_ca_biplot(res.ca, repel = TRUE)
