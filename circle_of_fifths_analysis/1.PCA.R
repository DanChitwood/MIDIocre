#Read in packages
library(ggplot2)
library(viridis)
library(reshape2)

##################################
#Project chord onto note PCA space
##################################

data <- read.table("./0.chord_keys.txt", header=TRUE)

#Perform PCA and combine PCA scores with other data

pca <- prcomp(data[14:25])
summary(pca)
pca_scores <- cbind(data[1:13], pca$x)

#Isolate just projected information
key <- na.omit(pca_scores[c(11:12,14:25)])

#Visualize with labels
p <- ggplot(data=pca_scores, aes(PC1, PC2))
p + geom_point(alpha=0.05, colour="gray40") + geom_text(data=key, aes(PC1, PC2, label=only_key, colour=major_minor), size=10) + theme_bw() + coord_fixed() + scale_colour_manual(values=c("black","blue"))

#ggsave("chords.jpg")

#Visualize with points
p <- ggplot(data=pca_scores, aes(PC1, PC2))
p + geom_point(alpha=0.05, colour="gray40") + geom_point(data=key, aes(PC1, PC2, colour=major_minor), size=5, alpha=0.5) + theme_bw() + coord_fixed() + scale_colour_manual(values=c("black","blue"))

##################################
#Project interval onto note PCA space
##################################

data <- read.table("./0.interval_keys.txt", header=TRUE)

#Perform PCA and combine PCA scores with other data

pca <- prcomp(data[14:25])
summary(pca)
pca_scores <- cbind(data[1:13], pca$x)

#Isolate just projected information
key <- na.omit(pca_scores[c(11:12,14:25)])
key$interval <- as.factor(key$interval)
levels(key$interval) <- c("P1","m2","M2","m3","M3","P4","d5","P5","m6","M6","m7","M7")
key$interval <- factor(key$interval, levels=c("P5","P4","m7","M2","M6","m3","P1","m6","M3","M7","m2","d5"))

#Visualize with labels
p <- ggplot(data=pca_scores, aes(PC1, PC2))
p + geom_point(alpha=0.05, colour="gray40") + geom_text(data=key, aes(PC1, PC2, label=notes, colour=as.factor(interval)), size=7) + theme_bw() + coord_fixed() + scale_colour_manual(values=c("#a6cee3","#1f78b4","#b2df8a","#33a02c","gold","#fdbf6f","#ff7f00","#fb9a99","#e31a1c","#cab2d6","#6a3d9a","black"))

#ggsave("intervals.jpg")

#Visualize with points
p <- ggplot(data=pca_scores, aes(PC1, PC2))
p + geom_point(alpha=0.05, colour="gray40") + geom_point(data=key, aes(PC1, PC2, colour=as.factor(interval)), size=5, alpha=0.5) + theme_bw() + coord_fixed() + scale_colour_manual(values=c("#a6cee3","#1f78b4","#b2df8a","#33a02c","gold","#fdbf6f","#ff7f00","#fb9a99","#e31a1c","#cab2d6","#6a3d9a","black"))

###########################################
#Project Bach BWV works onto note PCA space
###########################################

data <- read.table("./0.bwv_keys.txt", header=TRUE)

#Perform PCA and combine PCA scores with other data

pca <- prcomp(data[14:25])
summary(pca)
pca_scores <- cbind(data[1:13], pca$x)

#Isolate just projected information
key <- na.omit(pca_scores[c(11:12,14:25)])

#Create major and minor bwv key dataframes
major <- droplevels(subset(key, major_minor!="minor"))
minor <- droplevels(subset(key, major_minor!="major"))

#Reorder major and minor key levels
major$only_key <- factor(major$only_key, levels=c("C","G","D","A","E","B","Fs","Cs","Gs","Ds","As","F"))
minor$only_key <- factor(minor$only_key, levels=c("a","e","b","fs","cs","gs","ds","as","f","c","g","d"))

#Visualize major keys
p <- ggplot(data=pca_scores, aes(PC1, PC2))
p + geom_point(alpha=0.05, colour="black", size=0.2) + geom_point(data=major, aes(PC1, PC2,colour=only_key), size=4, alpha=0.5) + theme_bw() + coord_fixed() + scale_colour_manual(values=c("#fdbf6f","#cab2d6","#33a02c","#ff7f00","#6a3d9a","#b15928","black","#1f78b4","#e31a1c","#fb9a99","#b2df8a","#a6cee3"))

#ggsave("bwv_major.jpg")

#Visualize minor keys
p <- ggplot(data=pca_scores, aes(PC1, PC2))
p + geom_point(alpha=0.05, colour="black", size=0.2) + geom_point(data=minor, aes(PC1, PC2,colour=only_key), size=4, alpha=0.5) + theme_bw() + coord_fixed() + scale_colour_manual(values=c("#fdbf6f","#cab2d6","#33a02c","#ff7f00","#6a3d9a","#b15928","black","#1f78b4","#e31a1c","#fb9a99","#b2df8a","#a6cee3"))

#ggsave("bwv_minor.jpg")

#Visualize average interval usage for bwv major/minor
#Isolate original scaled interval frequencies
no_chords <- data[-c(1:24),]
key_data <- no_chords[c(11:12,14:25)]
major_tile <- subset(key_data, major_minor!="minor")
minor_tile <- subset(key_data, major_minor!="major")

#Aggregate and visualize major interval frequencies as a heatmap
agg_major <- aggregate(major_tile[3:14], by=list(major_tile$only_key), FUN=mean)
colnames(agg_major)[1] <- "key"
melt_major <- melt(agg_major, id="key")

p <- ggplot(melt_major, aes(x=variable, y=key, fill=value))
p + geom_tile() + scale_fill_viridis() + coord_fixed() + theme(axis.text.x=element_text(angle=90))

#ggsave("bwv_major_intervals.jpg")

#Aggregate and visualize minor interval frequencies as a heatmap
agg_minor <- aggregate(minor_tile[3:14], by=list(minor_tile$only_key), FUN=mean)
colnames(agg_minor)[1] <- "key"
melt_minor <- melt(agg_minor, id="key")

p <- ggplot(melt_minor, aes(x=variable, y=key, fill=value))
p + geom_tile() + scale_fill_viridis() + coord_fixed() + theme(axis.text.x=element_text(angle=90))

#ggsave("bwv_minor_intervals.jpg")

###########################################
#Project year of death of composer onto PCA space
###########################################

data <- read.table("./0.bwv_keys.txt", header=TRUE)
just_classical <- droplevels(subset(data, classical==TRUE))

#Perform PCA and combine PCA scores with other data

pca <- prcomp(just_classical[14:25])
summary(pca)
pca_scores <- cbind(just_classical[1:13], pca$x)

#Visualize
p <- ggplot(data=pca_scores, aes(PC1, PC2, colour=-death))
p + geom_point(alpha=0.25, size=1) + theme_bw() + coord_fixed() + scale_color_viridis(option="inferno")

#ggsave("composer_death.jpg")

#######################################################################
#Visualize era-specific composers, tonality and transition to atonality
#######################################################################

data <- read.table("./0.bwv_keys.txt", header=TRUE)
just_classical <- droplevels(subset(data, classical==TRUE))

#Perform PCA and combine PCA scores with other data

pca <- prcomp(just_classical[14:25])
summary(pca)
pca_scores <- cbind(just_classical[1:13], pca$x)

#Visualize specific composers

palestrina <- droplevels(subset(pca_scores, pca_scores$composer=="palestrina"))
vivaldi <- droplevels(subset(pca_scores, pca_scores$composer=="vivaldi"))
wagner <- droplevels(subset(pca_scores, pca_scores$composer=="wagner"))
shostakovich <- droplevels(subset(pca_scores, pca_scores$composer=="shostakovich"))
schoenberg <- droplevels(subset(pca_scores, pca_scores$composer=="schoenberg"))

alpha = 0.6
size = 4

p <- ggplot(data=pca_scores, aes(PC1, PC2))
p + geom_point(colour="black", alpha=0.05) + 

geom_point(data=vivaldi, aes(PC1, PC2), colour="gold2", size=size, alpha=alpha) + 
geom_point(data=wagner, aes(PC1, PC2), colour="dodgerblue", size=size, alpha=alpha) + 
geom_point(data=shostakovich, aes(PC1, PC2), colour="orange4", size=size, alpha=alpha) + 
geom_point(data=schoenberg, aes(PC1, PC2), colour="#e7298a", size=size, alpha=alpha) + #Magenta
geom_point(data=palestrina, aes(PC1, PC2), colour="black", size=size, alpha=alpha) + 
coord_fixed() + theme_bw()

#ggsave("composer_examples.jpg")

