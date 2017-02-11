install.packages("tm")
library(tm)
recipes = readLines("/Users/sunayakumar/Desktop/project/recipes combined.tsv")
recipes.new = apply(as.matrix(recipes), 1, function (x) gsub('\t',' ', x))
recipes.corpus = Corpus(VectorSource(recipes.new))
recipes.dtm = DocumentTermMatrix(recipes.corpus)
recipes.tdm = TermDocumentMatrix(recipes.corpus)
cuisines <- read.csv("/Users/sunayakumar/Desktop/project/cuisines.csv", header = FALSE)
mat = as.matrix(recipes.dtm)
length(summary(cuisines$V1))

#dtmss <- removeSparseTerms(recipes.dtm, 0.91)
#sam = sample(nrow(mat), size=2000, replace=FALSE)

km = kmeans(dist(mat[1:13408,], method = "euclidean"), 26)
km$cluster
sorted = sort(km$cluster)

# scale and try clustering? but how many centers?

standardised <- as.data.frame(scale(mat[1:13408,]))
Sample.scaled.2 <- data.frame(t(na.omit(t(standardised))))
pca.Sample.2 <- prcomp(Sample.scaled.2, retx=TRUE)
screeplot(pca.Sample.2, type="lines")
(pca.Sample.2$sdev)^2

recipes.new[(as.numeric(sorted)==3)]
cat(as.vector(cuisines$V1[temp]), sep="\n")
(data.frame(sort(summary(cuisines$V1[temp]), decreasing=TRUE)))
r = (recipes.new[temp])
r = paste(r, collapse=' ')
s = strsplit(r, " ")
sort(table(s), decreasing=TRUE)

mydata <- d
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
ylab="Within groups sum of squares")

tsne = Rtsne(unique(mat), dims = 2)
plot(tsne$Y, pch = 20, col = cuisines$V1, xlab = " ", ylab=" ", xaxt="n", yaxt="n")
legend("bottomright", legend=unique(cuisines$V1), col=unique(cuisines$V1), pch=20)
plot(tsne$Y, pch=1, main="tsne", col=cuisines$V1, cex=0.3)

tsne <- Rtsne(unique(mat[1:13408,]), dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)

## output
#Read the 11451 x 50 data matrix successfully!
#Using no_dims = 2, perplexity = 30.000000, and theta = 0.500000
#Computing input similarities...
#Normalizing input...
#Building tree...
#- point 0 of 11451
#- point 10000 of 11451
#Done in 22.22 seconds (sparsity = 0.012984)!
#Learning embedding...
#Iteration 50: error is 96.885140 (50 iterations in 13.29 seconds)
#Iteration 100: error is 95.554427 (50 iterations in 9.04 seconds)
#Iteration 150: error is 93.621675 (50 iterations in 8.81 seconds)
#Iteration 200: error is 93.616214 (50 iterations in 8.97 seconds)
#Iteration 250: error is 5.295105 (50 iterations in 9.57 seconds)
#Iteration 300: error is 3.597176 (50 iterations in 7.29 seconds)
#Iteration 350: error is 3.249061 (50 iterations in 7.79 seconds)
#Iteration 400: error is 3.047595 (50 iterations in 8.61 seconds)
#Iteration 450: error is 2.910885 (50 iterations in 8.02 seconds)
#Iteration 499: error is 2.811746 (50 iterations in 7.89 seconds)
##

store = list()
for (ii in 1:2681){
    store[ii] = max(results[ii,2:27])
}
table(as.numeric(cuisines$V1[10728:13408]))

##

install.packages("maxent")
library(maxent)
model <- maxent(mat[splat$b,],cuisines$V1[splat$b])
results <- predict(model,mat[splat$a,])

install.packages('caTools')
library(caTools)
splat = split(1:13408,letters[seq(length(x))%%2+1])

p <- ggplot(subset(wf, freq>1700), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p <- p + theme(
     panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
     panel.grid.minor = element_blank(),
     panel.grid.major = element_blank(),
     plot.background = element_rect(fill = "transparent",colour = NA), text = element_text(size=20))

t = tfidf(mat)
f <- colSums(t)
w <- data.frame(word=names(f), freq=f)

tfidf=function(mat){
    tf <- mat
    id=function(col){sum(!col==0)}
    idf <- log(nrow(mat)/apply(mat, 2, id))
    tfidf <- mat
    for(word in names(idf)){tfidf[,word] <- tf[,word] * idf[word]}
    return(tfidf)
}

## some notes

#frequency analysis. 
#clusters of ingredients. 
#visual. dessert clusters, savory clusters...

#frequency analysis of cuisines. 

#regression. 2000, 5000, data pipeline... 
##

cuisines <- read.csv("/Users/sunayakumar/Desktop/project/cuisines.csv", header = FALSE)
cuisines.corpus = Corpus(VectorSource(cuisines))
cuisines.dtm = DocumentTermMatrix(cuisines.corpus)
cuisines.tdm = TermDocumentMatrix(cuisines.corpus)

dshs = recipes.new
dshs = paste(dshs, collapse=' ')
dshs = strsplit(dshs, " ")
sort(table(dshs), decreasing=TRUE)


## sample output                                                                                                                                                                  
                                                                                                                                                                                                                                         
#all:			wheat			egg				butter			onion				garlic

#canada:		wheat			butter			egg				onion				garlic
#turkey:		garlic			onion			tomato			bell_pepper			butter
#carribean:		onion			garlic			black_pepper	vegetable_oil		tomato
#bangladesh:	cayenne			onion			turmeric		vegetable_oil		garlic
#india:			cumin			onion			turmeric		garlic				cayenne
#france:		butter			egg				wheat			onion				cream 
#italy:			garlic			olive_oil		tomato			basil				onion
#israel:		wheat			butter			black_pepper	egg					apple
#korean:		soy_sauce		garlic			sesame_oil		scallion			sesame_seed
##


# principle coordinate analysis
standardised <- as.data.frame(scale(mat[1:15,]))
Sample.scaled.2 <- data.frame(t(na.omit(t(standardised))))
pca.Sample.2 <- prcomp(Sample.scaled.2, retx=TRUE)
screeplot(pca.Sample.2, type="lines")
(pca.Sample.2$sdev)^2

# k-means clustering
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
data <- Sample.scaled.2
wss <- sapply(1:k.max,
function(k){kmeans(data, k, nstart=10 )$tot.withinss})

sse = [];
for (k in 1:27) {
    sse[k] = 0;
    clusters = kmeans(d, k);
    clusters.forEach(function(cluster) {
        mean = clusterMean(cluster);
        cluster.forEach(function(datapoint) {
            sse[k] = sse[k]+(datapoint - mean)^ 2);
        });
    });
}

# comparing accuracies of different classifiers
sample = sample(nrow(mat))
matrix = mat[sample,]
vector = cuisines[sample,]
container = create_container(matrix, as.numeric(vector), trainSize = 1:10727, testSize = 10728:13408, virgin = FALSE)
models = train_models(container, algorithms = c("MAXENT", "SVM", "SLDA", "BAGGING", "RF", "TREE"))
results = classify_models(container, models)
analytics = create_analytics(container, results)
summary(analytics)