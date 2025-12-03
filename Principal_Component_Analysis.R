library(readr)
install.packages("car")
library(car)
data_orginal <- read_delim("HBATred (2).csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
data_orginal <- data_orginal[-1]

n <- length(data_orginal$x6)
nt <- round(0.85 * n)
sample <- sample(1:n, nt)

data <- data_orginal[sample,]

#Test for multicollinearity: 

model <- lm(x6 + x7 + x8 + x9 + x10 + x12 + x13 + x14 + x16 + x18 ~ ., data = data)
vif_values <- vif(model)

# Print the VIF values
print(vif_values)

#Descriptive Analysis

apply(data, 2, mean)
apply(data, 2, sd)
correlation <- cor(data)

#PCA

pca <- prcomp(data, scale = TRUE)
print(pca)
names(pca)
pca$x

pca_x <- pca$x
pca_x
pca_sdev <- pca$sdev
pca_sdev
pca_var <- pca_sdev^2
pca_var
scale <- pca$scale
scale
rotation <- pca$rotation
rotation

#Biplot

biplot(pca, scale = 0)


pve <- pca_var/sum(pca_var)

pve
sum(pve)

plot(pve, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       ylim = c(0,0.4), type = "b")

plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = "b")


