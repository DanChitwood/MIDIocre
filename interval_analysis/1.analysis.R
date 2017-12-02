#Read in packages
library(ggplot2)
library(viridis)
library(MASS)
library(ggrepel)
library(reshape2)
library(data.table)
library(plyr)
library(kohonen)

################################
#PCA by intervals
################################

#Read in data
data <- read.table("./0.comp_intervals.txt", header=TRUE)

#Limit analysis to just classical pieces
just_classical <- droplevels(subset(data, data$classical=="classical"))

#Perform a PCA on the "within" interval data
pca_within <- prcomp(just_classical[9:20])
within_scores <- cbind(just_classical[1:8], pca_within$x)

#Visualize the "within" PCA data by composer death date
p <- ggplot(within_scores, aes(PC1, PC2, colour=death))
p + geom_point(size=2, alpha=0.2) + scale_color_viridis(option="inferno") + theme_bw()

#ggsave("within_PC12.jpg")

p <- ggplot(within_scores, aes(PC3, PC4, colour=death))
p + geom_point(size=2, alpha=0.2) + scale_color_viridis(option="inferno") + theme_bw()

#ggsave("within_PC34.jpg")

#Perform a PCA on the "between" interval data
pca_between <- prcomp(just_classical[21:32])
between_scores <- cbind(just_classical[1:8], pca_between$x)

#Visualize the "between" PCA data by composer death date
p <- ggplot(between_scores, aes(PC1, PC2, colour=death))
p + geom_point(size=2, alpha=0.2) + scale_color_viridis(option="inferno") + theme_bw()

#ggsave("between_PC12.jpg")

p <- ggplot(between_scores, aes(PC3, PC4, colour=death))
p + geom_point(size=2, alpha=0.2) + scale_color_viridis(option="inferno") + theme_bw()

#ggsave("between_PC34.jpg")

#Combine "within" and "between" interval PCA data so that we can perform an LDA
pca_within <- prcomp(just_classical[9:20])
pca_between <- prcomp(just_classical[21:32])
both_pca <- cbind(just_classical, pca_within$x, pca_between$x)

colnames(both_pca)[33:56] <- c("wPC1","wPC2","wPC3","wPC4","wPC5","wPC6","wPC7","wPC8","wPC9","wPC10","wPC11","wPC12","bPC1","bPC2","bPC3","bPC4","bPC5","bPC6","bPC7","bPC8","bPC9","bPC10","bPC11","bPC12")

#Perform LDA by composer. Note: remove wPC12 bPC12 because of lack of variability
lda <- lda(data=both_pca, composer~wPC1+wPC2+wPC3+wPC4+wPC5+wPC6+wPC7+wPC8+wPC9+wPC10+wPC11+bPC1+bPC2+bPC3+bPC4+bPC5+bPC6+bPC7+bPC8+bPC9+bPC10+bPC11)

#Retrieve percent variance for LDs
ld_percent <- lda$svd/sum(lda$svd)*100

#Calculate LD scores
predict <- predict(lda, both_pca[c(33:43,45:55)])

#Create dataset with info + LD scores
lda_scores <- cbind(both_pca[1:32],predict$x)

################################
#Correlation LD scores with time
################################

#Aggreate LD scores by composer
agg_scores <- aggregate(lda_scores[c(6:7,33:54)], by=list(lda_scores$composer), mean)
colnames(agg_scores)[1] <- "composer"

#Create a table to store results correlating LDs with time
cor_results <- matrix(nrow=22, ncol=3)

#Run a loop correlating LDs with time, record LD, pvalue, and rho
for(i in 1:22) {

t <- cor.test(agg_scores$death, agg_scores[,i+3], method="spearman")	

cor_results[i,1] <- paste("LD",i,sep="")
cor_results[i,2] <- t$p.value
cor_results[i,3] <- t$estimate
	
}

#Format loop results and perform p adjust
colnames(cor_results) <- c("LD","pvalue","rho")
cor_results <- as.data.frame(cor_results)
cor_results$BH <- p.adjust(as.vector(cor_results$pvalue), method="BH")
cor_results$rho <- as.numeric(as.character(cor_results$rho))

#Plot out results, looking at correlation of various LDs with time
p <- ggplot(cor_results, aes( rho,-log(BH, base=10) ))
p + geom_point(size=8, alpha=0.5) + geom_hline(yintercept=-log(0.05, base=10)) + theme_bw()

#ggsave("LD_pvalues.jpg")

ld_percents <- as.data.frame(ld_percent)
ld_percents$ld <- rownames(ld_percents)
p <- ggplot(ld_percents, aes(x=as.numeric(ld), y=ld_percent))
p + geom_bar(stat="identity") + theme_bw()

#ggsave("LD_percents.jpg")

p <- ggplot(agg_scores, aes(death ,LD1 ))
p + geom_point(size=8, alpha=0.5) + theme_bw()

#ggsave("LD1_correlation.jpg")

p <- ggplot(data=agg_scores, aes(x=death, y=LD1))
p + geom_point(size=5, alpha=0.5) + theme_bw() + geom_text_repel(data=agg_scores, aes(death, LD1, label=composer), size=5)

#ggsave("LD_composer_names.jpg")

################################
#Self-organizing maps
################################

library(kohonen)

set.seed(2)

som.midi <- som(scale(agg_scores[4:25]), grid=somgrid(2,2,"hexagonal"), rlen=10000)

plot(som.midi, main="MIDI SOM")
plot(som.midi, type="counts", main="MIDI SOM")
plot(som.midi, type="quality", main="MIDI SOM")

node <- as.data.frame(som.midi$unit.classif)
colnames(node) <- "node"

agg_scores[26] <- node

p <- ggplot(data=agg_scores, aes(x=death, y=LD1, colour=as.factor(node)))
p + geom_point(size=5, alpha=0.5) + theme_bw() + geom_text_repel(data=agg_scores, aes(death, LD1, label=composer), size=5) + scale_colour_brewer(palette=2, type="qual")

#ggsave("death.jpg")

p <- ggplot(data=agg_scores, aes(x=LD1, y=LD2, colour=as.factor(node)))
p + geom_point(size=5, alpha=0.5) + theme_bw() + geom_text_repel(data=agg_scores, aes(LD1, LD2, label=composer), size=5) + scale_colour_brewer(palette=2, type="qual")

#ggsave("LD1_LD2.jpg")

p <- ggplot(data=agg_scores, aes(x=LD3, y=LD4, colour=as.factor(node)))
p + geom_point(size=5, alpha=0.5) + theme_bw() + geom_text_repel(data=agg_scores, aes(LD3, LD4, label=composer), size=5) + scale_colour_brewer(palette=2, type="qual")

#ggsave("LD3_LD4.jpg")


################################
#Use LDA to predict composer identity
################################

#First, calculate PC scores to perform LDA on
pca_within <- prcomp(just_classical[9:20])
pca_between <- prcomp(just_classical[21:32])

pca_scores <- cbind(just_classical[1:8], pca_within$x, pca_between$x)

priors <- rep(1/length(summary(just_classical$composer, maxsum=100000)),197)

cv <- lda(pca_scores[c(9:19,21:31)], grouping=pca_scores$composer, CV=TRUE, prior=priors)

#Create a dataframe with LDA predictions

results <- cbind(pca_scores[1:8], cv$class)
colnames(results)[9] <- "predicted_composer"

p <- ggplot(results, aes(predicted_composer, composer))
p + geom_point(position="jitter") + theme(axis.text.x=element_text(angle=90, hjust=1))

#Analyze the predictions as proportions for each composer

table <- table(results$composer, results$predicted_composer)

df <- cbind(margin.table(table,1), as.data.frame.matrix(table))

prop <- df[,3:199]/df[,2]
prop$rows <- rownames(prop)

m <- melt(prop, id="rows")
colnames(m) <- c("actual_composer","predicted_composer","value")
just_death <- unique(subset(just_classical[c(1,7)]))
m1 <- merge(m,just_death, by.x="actual_composer", by.y="composer")
colnames(m1) <- c("actual_composer","predicted_composer","value","actual_composer_death")
m2 <- merge(m1,just_death, by.x="predicted_composer", by.y="composer")
colnames(m2) <- c("predicted_composer","actual_composer","value","actual_composer_death","predicted_composer_death")

#Visualize LDA predictions as proportions, arranged by composer year of death

p <- ggplot(m2, aes(x=reorder(predicted_composer,predicted_composer_death,mean), y=reorder(actual_composer,actual_composer_death,mean), fill=sqrt(value)))

p + geom_tile() + scale_fill_viridis(na.value="black", limits=c(0,1), option="inferno", begin=1, end=0) + theme(axis.text.x=element_text(angle=90, size=6, hjust=1, vjust=0.5), axis.text.y=element_text(size=6))  + coord_fixed()

#ggsave("confusion_matrix_by_year.jpg")

#Visualize LDA predictions as proportions, arranged by SOM node, then year of death

comp_node <- agg_scores[c(1, 26)]

m2_node <- merge(m2, comp_node, by.x="actual_composer", by.y="composer")
colnames(m2_node)[6] <- "actual_node"

m2_node <- merge(m2_node, comp_node, by.x="predicted_composer", by.y="composer")
colnames(m2_node)[7] <- "predicted_node"

p <- ggplot(m2_node, aes(x=reorder(predicted_composer,predicted_node, mean), y=reorder(actual_composer,actual_node, mean), fill=sqrt(value)))

p + geom_tile() + scale_fill_viridis(na.value="black", limits=c(0,1), option="inferno", begin=1, end=0) + theme(axis.text.x=element_text(angle=90, size=6, hjust=1, vjust=0.5), axis.text.y=element_text(size=6))  + coord_fixed() + geom_hline(yintercept=c(1.5,58.5,162.5), colour="black") + geom_vline(xintercept=c(1.5,58.5,162.5), colour="black")

#ggsave("confusion_matrix_by_node.jpg")

#################
#Permutation, by composer
#################

##
#First, what is the actual correct composer prediction rate?
##

cv <- lda(pca_scores[c(9:19,21:31)], grouping=pca_scores$composer, CV=TRUE, prior=priors)

actual_composer <- as.data.frame(pca_scores$composer)
predicted_composer <- as.data.frame(cv$class)
composer_predict <- cbind(actual_composer, predicted_composer)
colnames(composer_predict) <- c("actual_composer", "predicted_composer")

composer_predict$correct <- ifelse(composer_predict$actual_composer == composer_predict$predicted_composer, 1, 0)

correct = sum(as.matrix(composer_predict$correct), na.rm=TRUE)/nrow(composer_predict)

#The correct prediction rate for 197 composers is: 0.1018676

##
#Second, permute composer identity and see how many times permuted prediction rate exceeds actual
##

data_permute <- pca_scores

#data_permute <- data

n = 1000

results_table <- matrix(nrow=n, ncol=1)

for(i in c(1:n ) ) {

print(i)

data_permute$composer <- sample(pca_scores$composer, replace=FALSE)

cv <- lda(data_permute[c(9:19,21:31)], grouping=data_permute$composer, CV=TRUE, prior=priors)

#lda_phyl <- lda(data_permute[12:190], grouping=data_permute$ordered_family, CV=TRUE, prior=rep(1/nlevels(data_permute$ordered_family),nlevels(data_permute$ordered_family)))

actual_composer <- as.data.frame(pca_scores$composer)
predicted_composer <- as.data.frame(cv$class)
composer_predict <- cbind(actual_composer, predicted_composer)
colnames(composer_predict) <- c("actual_composer", "predicted_composer")

composer_predict$correct <- ifelse(composer_predict$actual_composer == composer_predict$predicted_composer, 1, 0)

correct = sum(as.matrix(composer_predict$correct), na.rm=TRUE)/nrow(composer_predict)

results_table[i,1] <- correct

print(correct)

}

results <- as.data.frame(results_table)
results$greater <- ifelse(results[,1]>=0.1018676,"greater_or_equal","lesser")
colnames(results) <- c("result","greater_or_equal")
write.table(results,"composer_permute_results.txt")

number_greater <- nrow(results[results$result>=0.1018676,])
proportion_greater <- number_greater / nrow(results)
max <- max(results$result)
min <- min(results$result)
mean <- mean(results$result)
median <- median(results$result)

#For n = 1000
#Actual: 0.1018676
#Number greater: 0
#Proportion greater: 0
#Max: 0.001697793
#Min: 0.0001242287
#Mean: 0.000594683
#Median: 0.0005797342


################################
#Visualizing interval usage
################################

#Format data for heat map of note and interval usage over history

for_tile <- just_classical[c(1,7,9:32)]
agg_tile <- aggregate(for_tile[2:26], by=list(for_tile$composer), FUN=mean)
scale_tile <- scale(agg_tile[3:26])
tile_data <- cbind(agg_tile[1:2], scale_tile)
colnames(tile_data)[1] <- "composer"

#Hierarhical clustering, to arrange note and interval usage

clusters <- hclust(dist( t(tile_data[,3:26]) ))
plot(clusters, labels=colnames(tile_data)[3:26], cex=0.9)


#Reorder interval levels, and visualize as a heatmap

melt_tile <- melt(tile_data, id=c("composer","death"))
melt_tile$variable <- factor(melt_tile$variable, levels=c(
"modulated_distance_0_withinbin_normalized",
"modulated_distance_9_withinbin_normalized",
"modulated_distance_9_betweenbin_normalized",
"modulated_distance_8_withinbin_normalized",
"modulated_distance_8_betweenbin_normalized",
"modulated_distance_0_betweenbin_normalized",
"modulated_distance_6_withinbin_normalized",
"modulated_distance_6_betweenbin_normalized",
"modulated_distance_10_withinbin_normalized",
"modulated_distance_10_betweenbin_normalized",
"modulated_distance_5_withinbin_normalized",
"modulated_distance_5_betweenbin_normalized",
"modulated_distance_2_withinbin_normalized",
"modulated_distance_2_betweenbin_normalized",
"modulated_distance_1_withinbin_normalized",
"modulated_distance_1_betweenbin_normalized",
"modulated_distance_11_withinbin_normalized",
"modulated_distance_11_betweenbin_normalized",
"modulated_distance_3_withinbin_normalized",
"modulated_distance_3_betweenbin_normalized",
"modulated_distance_7_withinbin_normalized",
"modulated_distance_7_betweenbin_normalized",
"modulated_distance_4_withinbin_normalized",
"modulated_distance_4_betweenbin_normalized"
))

melt_tile_node <- merge(melt_tile, comp_node, by.x="composer", by.y="composer")

p <- ggplot(melt_tile_node, aes(x=variable, y=reorder(composer, node), fill=value))
p + geom_tile() + scale_fill_viridis(option="inferno") + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0, size=8), axis.text.y=element_text(size=5)) + geom_hline(yintercept=c(1.5,58.5,162.5), colour="white")

#ggsave("heatmap.jpg")
