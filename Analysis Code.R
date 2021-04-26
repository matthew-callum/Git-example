#Attempt at PCA analysis of Pollen Data
Pollen.pca<- ordi_pca(Pollen_Data)

#Attempt at RDA analysis of Pollen Data
Pollen.rda <- ordi_rda(Pollen_Data ~ crop + mangmnt, data=Farm_Data)

#With the changed dataset without the nest category we can use the RDA analysis and it works!
Pollen.rda
summary(Pollen.rda)
Pollen.pca
summary(Pollen.pca)

#How to test significance of the data
#This one tests each constrained axis
Pollen_Axis_Anova <-anova(Pollen.rda, by="axis")

#This one tests each explanatory variable
Pollen_Variable_Anova <-anova(Pollen.rda, by="margin")

#Look at the results of the anova results
summary(Pollen_Axis_Anova)
summary(Pollen_Variable_Anova)
Pollen_Axis_Anova
Pollen_Variable_Anova

#Now to create the graphs for showing the results
#This is a Triplot of the Samples, Attributes, Explanatory Variables
ordi_plot(Pollen.rda)

#This is a Biplot with Samples and Arrows
ordi_plot(Pollen.rda, display=c("sites", "bp"))

#This is a Biplot with samples and centroids
ordi_plot(Pollen.rda, display=c("sites", "cn"))

#This is a Biplot with Attributes and Centroids
ordi_plot(Pollen.rda, display=c("species", "cn"))


#This data should create an additional plot
Pollen.pca <- ordi_pca(Pollen_Data)
Pollen.sco <- ordi_scores(Pollen.pca, display="sites")
Pollen.sco <- mutate(Pollen.sco, Use = Farm_Data$mangmnt)

Pollen2.sco <- ordi_scores(Pollen.pca, display="sites")
Pollen2.sco<- mutate(Pollen.sco, Use = Farm_Data$crop)

Pollen3.sco <- ordi_scores(Pollen.rda, display="sites")
Pollen3.sco <- mutate(Pollen3.sco, Use = Farm_Data$mangmnt)


Pollen4.sco <- ordi_scores(Pollen.rda, display="sites")
Pollen4.sco <- mutate(Pollen4.sco, Use = Farm_Data$crop)


ggplot(Pollen.sco, aes(x=PC1, y=PC2, fill=Use)) +
  geom_point()+
  geom_chull(alpha=0.5) +
  theme_classic()

ggplot(Pollen2.sco, aes(x=PC1, y=PC2, fill=Use)) +
  geom_point()+
  geom_chull(alpha=0.5) +
  theme_classic()

#Holy Shit it worked

#This will supposedly create an enhanced plot that shows the correlation between Land management types and the species data 
ggplot(Pollen.sco, aes(x=PC1, y=PC2, fill=Use))+
  geom_point() +
  geom_chull(alpha=0.5) +
  scale_fill_discrete(name = "Land Management Type", 
                      labels = c("Conventional", "Organic")) +
  theme_classic()

#This will create an enhanced plot that shows the correlation between Crop types and species
ggplot(Pollen2.sco, aes(x=PC1, y=PC2, fill=Use)) +
  geom_point() +
  geom_chull(alpha=0.5) + 
  scale_fill_discrete(name = "Crop Type",
  labels = c("Cereal", "Grasslands", "Mass Flowering Crops")) +
  theme_classic()  

#With those graphs created we can begin to create a cluster Dendrogram
Pollen.dis <- vegdist(Pollen_Data)
Pollen.dis
plot(Pollen.dis)

Pollen.cla <- hclust(Pollen.dis, method="average")
plot(Pollen.cla)

#We can see 7 groups within the Cluster Dendrogram  we can determine how many clusters actually exist
Pollen.cls <- hclust(Pollen.dis, method="single")
Pollen.clc <- hclust(Pollen.dis, method="complete")

#Have a look at the correlation between the original dissimilarity matrix and the inter-group dissimiliratiy at each step up the classification tree
cor(Pollen.dis, cophenetic(Pollen.cla))
cor(Pollen.dis, cophenetic(Pollen.cls))
cor(Pollen.dis, cophenetic(Pollen.clc))

#The Pollen.cla had the highest correlation so we'll use that one instead
#Now we use this amount of groups to delineate the number of groups
plot(Pollen.cla)
rect.hclust(Pollen.cla, 3)
Pollen.grp <- cutree(Pollen.cla, 3)
Pollen.grp


#Within the Cluster Dendrogram we can identify 7 groups and use that to create additional graphics
Pollen_Groups_sco <- ordi_scores(Pollen.pca, display="sites")
Pollen_Groups_sco <- mutate(Pollen_Groups_sco, group_code = as.factor(Pollen.grp))


ggplot(Pollen_Groups_sco, aes(x=PC1, y=PC2, fill= group_code)) +
  geom_point() +
  geom_chull(alpha=0.5) +
  scale_fill_discrete(name= "Classification", 
                      labels = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5", "Group 6"))
                      
