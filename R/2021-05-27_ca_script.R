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

e_col <- get_ca_col(res.ca)

e_xyz <- e_col[1]$coord[,1:3]


# export eigenvalues 1-3 

e_eig <- as.data.frame(res.ca$eig[1:3,])

n_eig <- nrow(res.ca$eig) # Anzahl Eigenvalues für Doku

# data dates einladen
dates <- read.csv2("../data/dates.csv", sep = ",", header = TRUE)

#join dates with ca_m / e
library(dplyr)
e_xyz <- as.data.frame(e_xyz) # für join data frame
e_xyz$name <- as.character(rownames(e_xyz)) # needs names as column

res_ca_dates <- right_join(dates,
                           e_xyz,
                           by = c("name" = "name" ),
                           copy = TRUE)

#Write data from R to a txt file: 
#https://www.dataanalytics.org.uk/sending-r-output-to-disk-files/
#http://www.cookbook-r.com/Data_input_and_output/Writing_text_and_output_from_analyses_to_a_file/ 
sink("../export/SinkCAValues.agt")
cat("#9999 \n")
cat("#true\n")
e_eig # todo: get hashtag vor jede zeile 
cat("#data \n")
res_ca_dates

# Stop sending output to file
sink(file = NULL)

#res_ca_dates -> name, x,y,z, from to fixed 
# Reihenfolge ändern und Dimnames umbenennen 
# res_ca_dates -> mit tab trennen,
# res_ca_dates -> Zeilennumern weg
