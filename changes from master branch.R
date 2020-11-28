source("http://bioconductor.org/biocLite.R")
biocLite("Rsubread")
install.packages("Rsubread")
install.packages("ggplot")
install.packages("ggpubr")
install.packages("plyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("scales")
install.packages("reshape2")
install.packages("RColorBrewer")
install.packages("dplyr")

library(plyr)
library(dplyr)
library(Rsubread)
library(ggplot2)
library(ggpubr)
library(plyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(reshape2)
library(RColorBrewer)

brewer.pal(n, name)
display.brewer.pal(n, name)

data2 = matrix(NA,2,4)
data2[1,1] =37.41
data2[1,2] =38.31
data2[1,3] =35.50
data2[1,4] =38.24
data2[2,1] =61.40
data2[2,2] =60.81
data2[2,3] =62.22
data2[2,4] =60.73

data2
par(mfrow=c(1,1))
colnames(data2) =c("default", "default s", "local", "local s")

#colnames(data1) =c("multimapped", "singlemapped")
#saveRDS(data1, file="bowtie2_method_analyze")

barplot(data2)


#  37.41,  38.31,  35.50,  38.24
#  61.40,  60.81,  62.22,  60.73


#   "37.41","38.31","35.50","38.24"
#    61.40, 60.81, 62.22,60.73


#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
############ TEIL 1 !!!
df2 <- data.frame(supp=rep(c("Single", "Multi"), each=4),
                  dose=rep(c("Default_n", "Default_S", "local_n", "local_S"),2),
                  len=c(61.40, 60.81, 62.22,60.73, 37.41,38.31,35.50,38.24))

df2
p =ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", )
p+ scale_fill_manual(values=c('#999999','#E69F00'))

df_sorted <- arrange(df2, dose, supp) 
head(df_sorted)

df_cumsum <- ddply(df_sorted, "dose",
                   transform, label_ypos=cumsum(len))

head(df_cumsum)

p = ggplot(data=df_cumsum, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity")+
  geom_text(aes(y=label_ypos, label=len), vjust=15.6, 
            color="white", size=4.5)+
  geom_text(aes(y=label_ypos, label=len), vjust=-10.6, 
            color="white", size=4.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()


p+ scale_fill_manual(values=c('#3b4d61','#6b7b8c'))
p+ scale_fill_manual(values=c('#E69F00','#999999'))
p + scale_fill_brewer(palette="Reds") + theme_minimal()
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
dat1.1 
dat2.1 
dat3.1 
dat4.1 
#loop  2:
dat1.2 
dat2.2 
dat3.2 
dat4.2 
#loop  3:
dat1.3 
dat2.3 
dat3.3 
dat4.3 
#loop  4:
dat1.4 
dat2.4 
dat3.4 
dat4.4 
#'
#'
#'
#'
############ TEIL 2 !!!
data2 = read_excel("C:/Users/andie/Downloads/Mappe 2 (1) (1).xlsx")
data2
View(data2)
## loop  1:
dat1.0 = data2$B[1]
dat2.0 = data2$E[1]
dat3.0 = data2$H[1]
dat4.0 = data2$K[1]
## loop  1:
dat1.1 = 0
dat2.1 = 0
dat3.1 = 0
dat4.1 = 0
#loop  2:
dat1.2 = 0
dat2.2 = 0
dat3.2 = 0
dat4.2 = 0
#loop  3:
dat1.3 = 0
dat2.3 = 0
dat3.3 = 0
dat4.3 = 0
#loop  4:
dat1.4 = 0
dat2.4 = 0
dat3.4 = 0
dat4.4 = 0


for (a in 2:10){ 
  time_lambda = 
    system.time({
      
      dat1.1 = dat1.1 + data2$B[a]
      dat2.1 = dat2.1 + data2$E[a]
      dat3.1 = dat3.1 + data2$H[a]
      dat4.1 = dat4.1 + data2$K[a]
    }
    )}
#'
#'
#'  ### Loop2
for (a in 11:20){ 
  time_lambda = 
    system.time({
      
      dat1.2 = dat1.2 + data2$B[a]
      dat2.2 = dat2.2 + data2$E[a]
      dat3.2 = dat3.2 + data2$H[a]
      dat4.2 = dat4.2 + data2$K[a]
    }
    )}
#'
#'
#'  ### Loop3
for (a in 21:30){ 
  time_lambda = 
    system.time({
      
      dat1.3 = dat1.3 + data2$B[a]
      dat2.3 = dat2.3 + data2$E[a]
      dat3.3 = dat3.3 + data2$H[a]
      dat4.3 = dat4.3 + data2$K[a]
    }
    )}
#'
#'
#'  ### Loop3
for (a in 31:44){ 
  time_lambda = 
    system.time({
      
      dat3.4 = dat3.4 + data2$H[a]
      dat4.4 = dat4.4 + data2$K[a]
    }
    )}

for (a in 31:42){ 
  time_lambda = 
    system.time({
      
      dat1.4 = dat3.4 + data2$H[a]
      dat2.4 = dat4.4 + data2$K[a]
    }
    )}

#'
#'
#'

len=c(dat1.0,dat2.0,dat3.0,dat4.0,
      dat1.1,dat2.1,dat3.1,dat4.1,
      dat1.2,dat2.2,dat3.2,dat4.2,
      dat1.3,dat2.3,dat3.3,dat4.3,
      dat1.4,dat2.4,dat3.4,dat4.4
)
######## Auswertung
supp=rep(c("0","1-9", "10-19","20-29",">30"), each=4)

df2 <- data.frame(supp=rep(c("E:  0","D:  1 - 9", "C:  10-19","B:  20-29","A:  >30"), each=4),
                  dose=rep(c("Default_n", "Default_S", "local_n", "local_S"),1),
                  len=c(dat1.0,dat2.0,dat3.0,dat4.0,
                        dat1.1,dat2.1,dat3.1,dat4.1,
                        dat1.2,dat2.2,dat3.2,dat4.2,
                        dat1.3,dat2.3,dat3.3,dat4.3,   # passt  # passt  # passt
                        dat1.4,dat2.4,dat3.4,dat4.4
                  ),
                  a = c("0","1-9", "10-19","20-29",">30"))
# '#eddb93','#90b9fd','#9cc1af','#999999','#f79a3c'
df2
p =ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", )+
  geom_text(aes(y=len, label=len), vjust=c(-0.2,-0.2,-0.5,-0.4,      0.9,0.9,1.0,0.9,    -2.8,-2.8,-2.8,-2.8,     -4.4, -4.4, -4.4, -4.4,    -6.1,-6.1,-6,-6),
            color= c("#d72631","#d72631","#d72631","#d72631","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white","white"), size=4.7)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()
# p+ scale_fill_manual(values=c('#8877fd','#90b9fd','#9cc1af','#fdcb77','#fd8877'))
# p+ scale_fill_manual(values=c('#999999','#d72631','#a2d5c5','#5c3c92','orange'))
p+ scale_fill_manual(values=c('#3b4d61','#6b7b8c','#077b8a','#a2d5c5','#d72631'))
p+ scale_fill_manual(values=c('#26495c','#c4a35a','#c66b3d','#a2d5c5','#e5e5dc'))
p+ scale_fill_manual(values=c('#316879','#f47a60','#7fe7dc','#ced7d8','#e5e5dc'))
p+ scale_fill_manual(values=c('#af4425','#c66b3d','#5d535e','#9a9eab','#ced7d8'))  # <- farbpalette 1
p+ scale_fill_manual(values=c('#c9a66b', '#ebdcb2','#5d535e','#9a9eab','#ced7d8')) # <- farbpalette 2
p+ scale_fill_manual(values=c( '#002c54' , '#008dcb' ,'#662e1c','#D55448','#ced7d8')) # <- farbpalette 3

#d72631 #a2d5c5 #077b8a #5c3c92 #3b4d61
#c99E10 #5d535e  #9a9eab 'c9a66b', '#ebdcb2'  '#662e1c',
col  = c('#3b4d61','#6b7b8c','#077b8a','#a2d5c5','#d72631')
col2 = c('#26495c','#c4a35a','#c66b3d','#a2d5c5','#e5e5dc') 
col3 = c('#316879','#f47a60','#7fe7dc','#ced7d8','#763626')
col5 = c( '#662e1c','#662e1c','#af4425','#af4425','#5d535e','#5d535e','#9a9eab','#9a9eab')
col6 = c( '#002c54' ,'#002c54', '#008dcb', '#008dcb' ,'#662e1c','#662e1c','#D55448','#D55448')
col7 = c( '#002c54' ,'#002c54', '#008dcb', '#008dcb' ,'#e8a735','#e8a735','#D55448','#D55448')
col_insert = c( 'white','#e8a735','#e8a735','#008dcb', '#008dcb' ,'#D55448','#B6452c','#002c54' ,'#002c54')
col_insertLegende = c( '#e8a735','#e8a735','#008dcb', '#008dcb' ,'#B6452c','#B6452c','#002c54' ,'#002c54')
col_darkLegende = c('#B6452c','#B6452c','#002c54' ,'#002c54')



col0 = c( '#002c54' , '#008dcb' ,'#662e1c','#D55448')


# '#002c54' ,'#002c54', '#008dcb', '#008dcb'  #D55448   '#e8a735'
#'
#'
#'
#'
#'
#'
#'
############# TEIL 3 !!!

data <- read_excel("C:/Users/andie/Downloads/Kopie von Book 3.xlsx")
data0 <- read_excel("C:/Users/andie/Downloads/doppel.xlsx")
data1 <- read_excel("C:/Users/andie/Downloads/Einzel1.xlsx")
data2 <- read_excel("C:/Users/andie/Downloads/Einzel2.xlsx")
data3 <- read_excel("C:/Users/andie/Downloads/Einzel1.2.xlsx")

colnames(data)  = c("Scores","probe1","probe2","probe3","probe4")
colnames(data0) = c("Scores","probe1","probe2","probe3","probe4")
colnames(data1) = c("Scores","probe1","probe2","probe3","probe4")
colnames(data2) = c("Scores","probe1","probe2","probe3","probe4")
colnames(data3) = c("Scores","probe1","probe2","probe3","probe4")

matplot( data  ,type = "o",  ylab = "Scores",  cex = 0,  lty = c(1),lwd=0, las = 1,   xlab = "Insert size Histogram",  bg = "23",col =col)
matplot( data0 ,type = "o",  ylab = "Scores",  cex = 0,  lty = c(1),lwd=0, las = 1,   xlab = "Insert size Histogram", bg = "23",col =col)
matplot( data1 ,type = "o",  ylab = "Scores",pch= c(1,2,4,5,6,7,8,9,10),  cex = 0,  lty = c(1),lwd=0, x =data1$Scores, las = 1,   xlab = "Insert size Histogram",  bg = "23",col =col_insert)
matplot( data2 ,type = "o",  ylab = "Scores",pch= c(1,2,4,5,6,7,8,9,10),  cex = 0,  lty = c(1),lwd=0, las = 1,   xlab = "Insert size Histogram",  bg = "23",col =col)
matplot( data3 ,type = "o",  ylab = "Scores",pch= c(1,2,4,5,6,7,8,9,10),  cex = 0,  lty = c(1),lwd=0, las = 1, x =data3$Scores,  xlab = "Insert size Histogram",  bg = "23",col =col)

legend("bottomleft", legend = c("Default_n","Default_S","local_n","local_S"), col = col[2:5], lty = c(1),
       ncol = 1, cex = 0.8)
#'
#'paste("Data(", 1:4, ")")
#'
#'
#'
#'
#'
#'
#'
##################### ALL 8 DATA ANALYSE

Data_1 <- read_excel("C:/Users/andie/Downloads/Insert histogram 8/Data 1.xlsx")
Data_1

matplot( Data_1 ,type = "o",  ylab = "",  cex = 0,  lty = c(1),lwd=2, las = 1, x = Data_1$count,  xlab = "Insert size Histogram",  bg = "20",col =  col_insert,xlim=c(37,401))
legend(365.15,2154007,pt.cex = 0 ,  pch= 1, c( "light low digest", "light low digest", "light high digest", "light high digest",    "dark low digest", "dark low digest", "dark high digest", "dark high digest"),box.lty = 0,box.col = 0, box.lwd = 3, cex = 1.1, lwd = 2.2, lty = c(1,1), col = col_insertLegende  , text.col = col_insertLegende , merge = TRUE)

# new data 

cuttoff_data <- read_excel("C:/Users/andie/Downloads/Insert histogram 8/cuttoff data.xlsx")
cuttoff_data

matplot( Data_1[1:5],type = "o",  ylab = "",  cex = 0,  lty = c(1),lwd=2, las = 1, x = Data_1$count,  xlab = "Insert size Histogram", xlim=c(37,401), bg = "20",col =  col_insert)
legend(285.15,2154007,pt.cex = 0  ,pch= 1, c( "light low digest", "light low digest", "light high digest", "light high digest"),box.lty = 0,box.col = 0, box.lwd = 4, cex = 0.9, lwd = 2.5, lty = c(1,1), col = col_insertLegende[1:4]  , text.col = col_insertLegende[1:4] , merge = TRUE)


matplot( Data_1[6:9],type = "o",  ylab = "",  cex = 0,  lty = c(1),lwd=2, las = 1, x = Data_1$count,  xlab = "Insert size Histogram", xlim=c(37,401), bg = "20",col =  col_insert[5:9])
legend(285.15,954007,pt.cex = 0  ,pch= 1, c( "dark low digest", "dark low digest", "dark high digest", "dark high digest"),box.lty = 0,box.col = 0, box.lwd = 4, cex = 0.9, lwd = 2.5, lty = c(1,1), col = col_darkLegende  , text.col = col_darkLegende , merge = TRUE)

?legend
?matplot

#'
#'
#'
#'
########### MultiQC plot


data2
data2 = matrix(NA,2,8)
data2[1,1] =59.7
data2[1,2] =57.8
data2[1,3] =60.1
data2[1,4] =60.8
data2[1,5] =58.5
data2[1,6] =58.1
data2[1,7] =60.8
data2[1,8] =58.1

data2[2,1] =39.7
data2[2,2] =41.8
data2[2,3] =39.3
data2[2,4] =37.8
data2[2,5] =40.5
data2[2,6] =36.6
data2[2,7] =38.3
data2[2,8] =39.5

data2
##############      LIGHT PART Multi vs single mapped 
#'
#'
#'
df2 <- data.frame(supp=rep(c("Single", "Multi"), each=4),
                  colorpanel=rep(c( "A: single mapping ","B: single mapping ","C: multi mapping ","D: multi mapping "), each=2),
                  dose=rep(c("light low digest1", "light low digest2", "light high digest1", "light high digest2"),2),
                  len=c(data2[1],data2[3],data2[5],data2[7],data2[2],data2[4],data2[6],data2[8]))

p =ggplot(data=df2, aes(x=dose, y=len, fill=colorpanel)) +
  geom_bar(stat="identity", )
p+ scale_fill_manual(values=c('#e8a735','#008dcb', '#008dcb' ,'#B6452c'))

df_sorted <- arrange(df2, dose, supp) 
df_cumsum <- ddply(df_sorted, "dose",
                   transform, label_ypos=cumsum(len))

q1 =ggplot(data=df_cumsum, aes(x=dose, y=len, fill=colorpanel)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 30,hjust = 1))   +
  geom_text(aes(y=label_ypos, label=len), vjust=c(2,  2,2,  2,  2,  2,2,  2),color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 30,hjust = 1))

q1 + scale_fill_manual(values=c('#f3d39a', '#7fc6e5', '#e8a735' ,'#008dcb'))  
q1 + scale_fill_manual(values=c('#6b7b8c','#6b7b8c','#3b4d61', '#3b4d61'))
#'
#'
#'
#'
#'
#'
##############      Dark PART Multi vs single mapped 
#'
#'
#'
df2 <- data.frame(supp=rep(c("Single", "Multi"), each=4),
                  colorpanel=rep(c( "A: single mapping ","B: single mapping ","C: multi mapping ","D: multi mapping "), each=2),
                  dose=rep(c("dark low digest1", "dark low digest2", "dark high digest1", "dark high digest2"),2),
                  len=c(data2[9],data2[11],data2[13],data2[15],data2[10],data2[12],data2[14],data2[16]))

p =ggplot(data=df2, aes(x=dose, y=len, fill=colorpanel)) +
  geom_bar(stat="identity", )
p+ scale_fill_manual(values=c('#e8a735','#008dcb', '#008dcb' ,'#B6452c'))

df_sorted <- arrange(df2, dose, supp) 
df_cumsum <- ddply(df_sorted, "dose",
                   transform, label_ypos=cumsum(len))

q1 =ggplot(data=df_cumsum, aes(x=dose, y=len, fill=colorpanel)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 30,hjust = 0.9))   +
  geom_text(aes(y=label_ypos, label=len), vjust=c(2,  2,2,  2,  2,  2,2,  2),color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 30,hjust = 1))

q1 + scale_fill_manual(values=c('#cb7c6b', '#668098', '#B6452c' ,'#002c54'),)


#Xx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xX
#Xx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xX
#Xx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xX
#Xx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xXx__xXx_xX
#
####### Dataset collection

"""
Hier kommen die Werte von samtool view, vergleich von vorher nachher
SRR5222795      : 204736974    137997930
SRR5222794      :  96413726     67123106
SRR5222793      : 190711526    130795692
SRR5222792      : 215784420    143671567
SRR5222791      : 207250166    144195141
SRR5222790      : 241548352    166810489
SRR5222789      : 150562838     97921483
SRR5222788      : 225899250    153264513
"""
install.packages("wesanderson")
library(wesanderson)

data_stats = matrix(NA,2,8)
data_stats[1,1] =204736974  #D
data_stats[1,2] =96413726   #D
data_stats[1,3] =190711526  #D
data_stats[1,4] =215784420  #D
data_stats[1,5] =207250166  #LIGHT
data_stats[1,6] =241548352  #LIGHT
data_stats[1,7] =150562838  #LIGHT
data_stats[1,8] =225899250  #LIGHT

data_stats[2,1] =137997930
data_stats[2,2] =67123106
data_stats[2,3] =130795692
data_stats[2,4] =143671567
data_stats[2,5] =144195141
data_stats[2,6] =166810489
data_stats[2,7] =97921483
data_stats[2,8] =153264513



# weiter gehts
#########             DARKNESS STATS
#'                    DARKNESS STATS
#'
#'
#'
df2 <- data.frame(supp=rep(c( "reads after cut",  "reads before cut"), each=4),
                  colorpanel=rep(c( "A: dark low d. uncut","B: dark high d. uncut","C: dark low d. cut","D: dark high d. cut"), each=2),
                  dose=rep(c( "dark high digest2", "dark high digest1","dark low digest2", "dark low digest1"),2),
                  len=c(data_stats[2],data_stats[4],data_stats[6],data_stats[8],data_stats[1],data_stats[3],data_stats[5],data_stats[7]))

df2
p =ggplot(data=df2, aes(x=dose, y=len, fill=colorpanel)) +
  geom_bar(stat="identity", )
p+ scale_fill_manual(values=c('#e8a735','#008dcb', '#008dcb' ,'#B6452c'))

df_sorted <- arrange(df2, dose, supp) 
df_cumsum <- ddply(df_sorted, "dose",
                   transform, label_ypos=cumsum(len))

q1 =ggplot(data=df_cumsum, aes(x=dose, y=len, fill=colorpanel)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 30,hjust = 1))   +
  geom_text(aes(y=label_ypos, label=len), vjust=c(-0,2,  -2,2,  -2,2,  -1.2,2),color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 30,hjust = 1))

q1 + scale_fill_manual(values=c('#668098', '#cb7c6b' ,'#002c54','#B6452c'))
'#cb7c6b'
#  '#e8a735','#e8a735','#008dcb', '#008dcb' ,'#B6452c','#B6452c','#002c54' ,'#002c54'
#'                    
#'
#'                            LIGHTNING  STATS
#'                            LIGHTNING  STATS
#'
#'
#'
#########
###########            LIGHTNING  STATS
df2 <- data.frame(supp=rep(c( "reads after cut",  "reads before cut"), each=4),
                  colorpanel=rep(c( "A: light low d. uncut","B: light high d. uncut","C: light low d. cut","D: light high d. cut"), each=2),
                  dose=rep(c( "light high digest2", "light high digest1","light low digest2", "light low digest1"),2),
                  len=c(data_stats[10],data_stats[12],data_stats[14],data_stats[16],data_stats[9],data_stats[11],data_stats[13],data_stats[15]))

df2
p =ggplot(data=df2, aes(x=dose, y=len, fill=colorpanel)) +
  geom_bar(stat="identity", )
p+ scale_fill_manual(values=c('#e8a735','#008dcb', '#008dcb' ,'#B6452c'))

df_sorted <- arrange(df2, dose, supp) 
df_cumsum <- ddply(df_sorted, "dose",
                   transform, label_ypos=cumsum(len))

q1 =ggplot(data=df_cumsum, aes(x=dose, y=len, fill=colorpanel)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 30,hjust = 1))   +
  geom_text(aes(y=label_ypos, label=len), vjust=c(-2,2,  -2,2,  -2,2,  -1.2,2),color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 30,hjust = 1))

q1 + scale_fill_manual(values=c('#7fc6e5','#f3d39a', '#008dcb', '#e8a735' ))





#  '#e8a735','#e8a735','#008dcb', '#008dcb' ,'#B6452c','#B6452c','#002c54' ,'#002c54'
#'
#'
##      #    strg + F   = replace value 
dat3  <- read.delim("C:/Users/andie/Downloads/Galaxy18-[multiBamSummary_on_data_15,_data_13,_and_others__bin_counts].tabular")

head(dat3)


dat.new<-dat3[,-c(1:3)]
head(dat.new)
#dat3[,grep("SRR",colnames(dat3))]
rownames(dat.new) <- paste0(dat3$X..chr.,":",dat3$X.start.,"-",dat3$X.end.)


mean.samp<-apply(dat.new,2,mean)
sd.samp<-apply(dat.new,2,sd)

table((mean.samp[1]+2*sd.samp[1]) > dat.new[,1])   # 1 : 8 für loop
idx<-((mean.samp[1]+2*sd.samp[1]) > dat.new[,1])   # 2* sd, standarddiviation mal 2-3 

# mx<-cbind(idx.1, idx.2, ..)
# mx.sum<-apply(mx,1,sum)
# idx<-mx.sum==8

head(dat.new[!idx,1])

head(rownames(dat.new[!idx,]))

bed.format<-data.frame(chr=dat3[!idx,"X..chr."],
                       start=dat3[!idx,"X.start."],
                       end=dat3[!idx,"X.end."],
                       name=rownames(dat.new[!idx,]),
                       score=dat.new[!idx,1],
                       strand=".")






head(bed.format)
write.table(bed.format, file="C:/Users/andie/Downloads/exampleBED.bed", quote=F, sep="\t", col.names = F,row.names = F)
#
