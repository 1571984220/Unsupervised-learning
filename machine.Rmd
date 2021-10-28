---
title: "Machine learning coursework"
author: "JUNYI GUAN"
date: 'ID:10333271 '
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
loading the data first
```{r}
load("penguins.rda")
```
### Section 1:background and describe the data set
The data is on 333 penguins for 4 measured variables (bill length and depth, flipper length, weight),and also we have the data for each penguin (sex, species, islands )

In section 1,we need to found out wheather we need to split the data into different sex or not.So I using scatter plots for the data and Use different colors to represent Penguins in different sex
```{r}
pairs(X.penguins, col=as.integer(L.sex), pch=as.integer(L.sex)+14)
```

The red refer to male, black refer to female.It is clear from the scatter plots, male penguins have higher measurements than female.I decide to split the data into different sex.

###  In Section 2:briefly introduce the multivariate methods you are using to analyse the data.
since the data is base on 4 variables,In order to visualize the data well, I decide to reduce 4 dimension to 2 by just using pc1 and pc2.It is clear from the summary below the first two PCs capture 88.09% of total variation

```{r}
S.penguins = scale((X.penguins[, 1:4]), scale=TRUE) # center and standardise
pca.out = prcomp(S.penguins)
summary(pca.out)
x.penguins = pca.out$x[,1:2]
x.male = x.penguins[L.sex == "male",]
x.female = x.penguins[L.sex == "female",]

```

#### K-means
First,I choose different K(1,2,3...10) for K-means method to male and female data.Second ,I polt total within-group sum of squares and the between-group sum of squares vs different number of groups.
```{r}
kmax=10
bvec.male=numeric(kmax)
wvec.male=numeric(kmax)

bvec.female=numeric(kmax)
wvec.female=numeric(kmax)
for (k in 1:kmax)
{
  kmeans.outm = kmeans(x.male, k)
  bvec.male[k] = kmeans.outm$betweenss
  wvec.male[k] = kmeans.outm$tot.withinss
  kmeans.outf = kmeans(x.female, k)
  bvec.female[k] = kmeans.outf$betweenss
  wvec.female[k] = kmeans.outf$tot.withinss
  
}

par(mfcol=c(1,2))
plot(1:kmax, bvec.male, type="b", ylim=c(0, 600), xlab="no groups",
     ylab="Within/between Group SS", main="K-Means male Data")
points(1:kmax, wvec.male, type="b", col=2, lty="dashed", pch=2)
plot(1:kmax, bvec.female, type="b", ylim=c(0, 600), xlab="no groups",
     ylab="Within/between Group SS", main="K-Means female Data")
points(1:kmax, wvec.female, type="b", col=2, lty="dashed", pch=2)
```

It is clear that the optimal value for K is equal to 3,because between grop variation do not increase a lot if We still increase the number of clusters(k).

So I decide to choose K=3 to run K-means algorism.
```{r}
par(mfcol=c(1,2))
kmeans.out3m = kmeans(x.male, 3)
plot(x.male, col=kmeans.out3m$cluster, main="K-Means for male penguin")
kmeans.out3f = kmeans(x.female, 3)
plot(x.female, col=kmeans.out3f$cluster, main="K-Means for female penguin")
```

#### Gaussian mixture models
```{r}
library("mclust")
```

I use the original data set(4 variables)s (not the pc1 and pc2 in this question because I am affarid just using just 2 pc will influence the result).
I run GMM for (1,2,3...6) six differnt clusters and plot number of classes vs BIC
```{r}
par(mfcol=c(1,2))
X.male = X.penguins[L.sex == "male",]
X.female = X.penguins[L.sex == "female",]
gmm.optm = Mclust(X.male, G=1:6, verbose=FALSE,main="male penguin")
gmm.optf = Mclust(X.female, G=1:6, verbose=FALSE,main="female penguin")
plot(gmm.optm, what="BIC",modelNames="VVV")
plot(gmm.optf, what="BIC",modelNames="VVV")
```

The 2 plots show,The optimal number of cluster is 3 for male and 2 for female,according to the smallest BIC (highest BIC in R) to choose the optimal number.but 3 is also good in female.So I decide to choose K=3 to run GMM. 
```{r}
par(mfcol=c(1,2))
plot(gmm.optm, what="classification")
plot(gmm.optf, what="classification")
```

#### In Section 3:Compare with the known clusters given by the labels L.species and L.island.
```{r}
#split the islands and species data into male and female
L.species.male = L.species[L.sex == "male"]
L.species.female = L.species[L.sex == "female"]
L.islands.male = L.islands[L.sex == "male"]
L.islands.female = L.islands[L.sex == "female"]
```
First analysis L.species and compare the GMM and K-means

L.species Male
```{r}
table(L.species.male, kmeans.out3m$cluster)
table(L.species.male, gmm.optm$classification)
```

L.species Female
```{r}
table(L.species.female, kmeans.out3f$cluster)
table(L.species.female, gmm.optf$classification)
```
The result of L.species.male are Adelie(73),Chinstrap(34),Gentoo(61) and L.species.female are Adelie(73),Chinstrap(34),Gentoo(58)With 3 cluster for male and female penguins data, The corresponding misclassifications  for GMM are 0 and 3 and .However, The corresponding misclassifications  for K-means are 10 and 6.But both method for Gentoo are always correctly identified.Overall GMM gets more accurate classifications than K-means and I also can say different penguin species has obviously different.

L.islands Male
```{r}
table(L.islands.male, kmeans.out3m$cluster)
table(L.islands.male, gmm.optm$classification)
```

L.islands Female
```{r}
table(L.islands.female, kmeans.out3f$cluster)
table(L.islands.female, gmm.optf$classification)
```
The result of L.islands.male are Biscoe(83),Dream(62), Torgersen(23), and L.islands.female are Biscoe(80),Dream(61), Torgersen(24).GMM and K-means both have a lot misclassifications.But It is clear that 61 male penguins and 58 female penguins from Biscoe form a individual grop.

The reason for this phenomenon in my opinion is that compare to L.species,61 male penguins and 58 female penguins from Biscoe must be Gentoo.In other words Gentoo only live in Biscoe,since Gentoo is very different from other 2 penguins species and the number of Gentoo are exactly 61 (male) and 58 (female),and the number of Gentoo are much more than Adelie and Chinstrap.As a consequence, they can form a group with regard to L.islands.

The remainding two clusters have penguins are not always from just one island,that is a bad result.The reason in my opinion is that there are more than 1 species live in these 2 islands since the differnt between species is obvious,It is impossible the clustering result is the 2 islands ,otherwise the variation within groups is too large!