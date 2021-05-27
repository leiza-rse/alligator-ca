# Script by Allard Mees for alligator tool
# implemented on website

#Testdaten einladen:

mat <- as.matrix(read.table(file="../data/ca_potterlimes.csv", 
                            sep = "\t", 
                            header=TRUE)) 

# mat <- as.matrix(read.table(file="CADataInput.dat", header=TRUE)) 
lm = (length(mat)/3) 
prov <- unique(mat[1:lm]) 
pot <- unique(mat[(lm+1):(2*lm)])

m <- matrix(0,length(prov),length(pot))
rownames(m) <- prov
colnames(m) <- pot

for (i in 1:lm) {
  setrow = mat[i,1]
  setcol = mat[i,2]
  m[setrow, setcol] = as.integer(mat[i,3]) }

print(m)
# Einschub SCS:
ca_m <- ca(m)

print(ca(m))
plot(ca(m))
pdf(file="ca_Potters.pdf", width=11.5, height=8, title="Plot of CA of potter data",
    pointsize=10)
plot(ca(m))
dev.off()

#Funktioniert. Zur Erstellung von JPG's muss zuerst das Ding binair kreiert
# werden (grDevices laden),und dann mit <plot> gefuellt werden, dann device off.

library(grDevices)
jpeg(filename = "ca_Potters.jpg", width = 600, height = 400, quality = 150,
     restoreConsole = TRUE)
plot(ca(m))
dev.off()

#this functions also
#summary <- summary(ca(m))
#options(save.defaults=list(ascii=TRUE, safe=FALSE)) 
#save(summary, file = "summary.Rdata")

#aber dies noch ein tick elegante. Fetch it mit "cffile action = read"
sink("SinkCAValues.txt")
summary(ca(m)) 

ca_m[1]$coord[,1:3]

## Code von Dirk Seidensticker, Projekt ChronoCongo: 
# https://github.com/dirkseidensticker/ChronoCongo
# Correspondance Analysis
# =======================
library("FactoMineR")
library("factoextra")
res.ca <- CA(m, graph = FALSE)
ca.plt.12 <- fviz_ca_biplot(res.ca,
                            repel = TRUE)

# 2nd and 3rd axes
ca.plt.23 <- fviz_ca_biplot(res.ca,
                            axes = c(2, 3),
                            repel = TRUE)
# 3nd and 2rd axes
ca.plt.32 <- fviz_ca_biplot(res.ca,
                            axes = c(3, 2),
                            repel = TRUE)

# export coordinates
# ------------------

e <- get_ca(res.ca)
e_xyz <- e[1]$coord[,1:3]


# export eigenvalues 1-3 

e_eig <- res.ca$eig[1:3,]
n_eig <- nrow(res.ca$eig) # Anzahl Eigenvalues für Doku

# data dates einladen
dates <- read.csv2("../data/dates.csv", sep = ",", header = TRUE)

#join dates with ca_m / e
library(dplyr)
e_xyz <- as.data.frame(e_xyz) # für join data frame
e_xyz$name <- as.character(rownames(e_xyz)) # needs names as column

e_xyz_l <- right_join(e_xyz,
                          mat,
                          by = c("name" = "pottername"),
                      copy = TRUE)

res_ca_dates <- right_join(dates,
                           e_xyz_l,
                           by = c("name" = "limes" ),
                           copy = TRUE)

# herausfinden, 
# wie man unterschiedliche Teile in ein Dokument schreibt.

#Write data from R to a txt file: 
#https://www.dataanalytics.org.uk/sending-r-output-to-disk-files/

sink("SinkCAValues.txt")
summary(ca(m)) 
e_eig
# Stop sending output to file
> sink(file = NULL)