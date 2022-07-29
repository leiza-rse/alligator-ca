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

# rownames weg, headers weg, hashtag vorne ran und als spaltentrenner pipe

## e_eig anpassen
# Idee: e_eig als csv mit pipe separator abspeichern
#und dann einladen als string und sink in "../export/SinkCAValues.agt" 
write.table(e_eig, file = "../data/eigenvalues.txt", 
            sep = "|", 
            row.names = FALSE) #pipe separated file

eig_txt <- read.csv("../data/eigenvalues.txt", col.names = FALSE)
eig_txt <- eig_txt$FALSE. # eig_txt zu einem factor machen

## res_ca_dates anpassen

res_ca_dates2 <- data.frame (res_ca_dates$name, 
                             res_ca_dates$`Dim 1`, 
                             res_ca_dates$`Dim 2`, 
                             res_ca_dates$`Dim 3`,
                             res_ca_dates$from,
                             res_ca_dates$to,
                             res_ca_dates$fixed) 

colnames(res_ca_dates2) <- c("name", "x", "y", "z", "von", "bis", "fixed")

write.table(res_ca_dates2, file = "../data/res_ca_dates2.txt", 
              sep = "\t") # tab separated file created

#res_ca_dates_cl <- read.csv("../data/res_ca_dates2.txt", col.names = FALSE)
# Prob: Tabs scheinen zu "verschwinden"

#Write data from R to a txt file: 
#https://www.dataanalytics.org.uk/sending-r-output-to-disk-files/
#http://www.cookbook-r.com/Data_input_and_output/Writing_text_and_output_from_analyses_to_a_file/ 
sink("../export/SinkCAValues.agt") # name of file to write in
cat("#9999 \n")
cat("#true\n")

# https://stackoverflow.com/questions/45508982/r-data-frame-to-plain-text

# print eigenvalues-zusammenfassung
tc <- textConnection("str", "w")
sink(tc)   # divert output to tc connection
print(eig_txt, max.levels = 0)  # print in str string instead of console
sink()     # set the output back to console
close(tc)  # close connection
str <- substr(str,floor(length(str)/10)+4,nchar(str[1])) # we get rid of the row numbers that come with print
str <- paste0("# ",str, collapse= "\n")        # we build a proper unique string hastag and new lines

cat(str)

cat("\n")

cat("#data \n")

##print data 
tc2 <- textConnection("str2", "w")
sink(tc2)  
print(res_ca_dates2)  # print in str string instead of console
sink()     # set the output back to console
close(tc2)  # close connection
str2 <- substr(str2,floor(length(str2)/10+2),nchar(str2[1])) # we get rid of the row numbers that come with print
str2 <- paste0(str2, collapse= "\n")

cat(str2)

# Stop sending output to file
sink(file = NULL)

## Last Problem:
# res_ca_dates -> mit tab trennen,
# Idee (wie bei eigenvalues: in txt \t-separated speichern - gemacht, siehe oben )
# Einladen, ohne "tabbierisung zu verlieren"
# take txt-inhalt copy to other txt
# für sowas braucht man python. -.-
# R will immer gleich wieder ne Tabelle aus tab-sep file machen
