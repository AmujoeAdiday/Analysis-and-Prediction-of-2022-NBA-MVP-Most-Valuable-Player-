#install data.table
library(data.table)
library(ggplot2)
library(FactoMineR)

fulldf <- read.csv("standardizedallstats.csv", header=TRUE, stringsAsFactors=TRUE)
fulldf

attach(fulldf)
names(fulldf)
str(fulldf)

df<-fulldf[!(fulldf$Year < 2012 ),]
attach(df)

ordf <- df[order(Rank),]
ordf
sum(MVP > 0)
ordf[4885,]

# #removes non-numeric columns)
# setDT(df)[, c('MVP', 'Team', 'Tm', 'Pos', 'Year','X','Player') := NULL]
# #removes repetitive columns)
# setDT(df)[, c('X3P','X3PA','X3P.','X2P','X2P.','ORB','DRB') := NULL]
# #removes team stats
# setDT(df)[, c('Pts.Won','Pts.Max','Share','W','L','GB','PS.G','PA.G','Rank') := NULL]
# #removes other redundant stats
# setDT(df)[, c('G','MP','Age','Unnamed..0','eFG.') := NULL]
# #removes other redundant stats
# setDT(df)[, c('FG','FT') := NULL]
# 
# setDT(df)[, c('FG','FT') := NULL]

df <- ordf[ ,c("PTS","FTA","X2PA","X3PA","TS.","AST","TOV","TRB","STL","GS")]    
colnames(df) <- c('PTS','FTA','2PA','3PA','TS%','AST','TOV','TRB','STL','GS')
df

names(df)

str(df)                                       
class(df)


vardf <- var(df)
vardf

cordf <- cor(df)
cordf

df_mean <- sapply(df, mean)
df_sd <- sapply(df, sd)
df_min <- sapply(df, min)
df_max <- sapply(df, max)



df_pca <-prcomp(x = df,
                center = TRUE, 
                scale. = FALSE)
print(df_pca)
summary(df_pca) 

df_pca$x[,2] <- df_pca$x[,2]*-1

df_pca$rotation[,2] <- df_pca$rotation[,2]*-1



df_pca$x[,1:2]

max(df_pca$x[,1])
min(df_pca$x[,1])
max(df_pca$x[,2])
min(df_pca$x[,2])



# biplot(df_pca, xlim=c(-.05,.05),ylim=c(-.05,.05))
# 
# biplot(df_pca,
#        col=c('blue', 'red'),
#        cex=c(.8, .9),
#        xlabs = rep(".",14092),
#        xlim=c(-.05, .05),
#        main='PCA Results',
#        xlab='First Component',
#        ylab='Second Component',
#        expand=1.3)

require(graphics)
par(pty = "s",
    cex.main = 1.2,
    cex.lab = 1,
    font.main = 2,
    font.lab = 2,
    family = "sans",
    col.main = "gray10",
    col.lab = "gray10",
    fg = "gray10",
    las = 1)


plot.new()
plot.window(xlim = c(-4.7, 11),
            ylim = c(-6.1, 5),
            asp = 1.5)

axis(side = 1,
     at = c(-4,-2,0,2,4,6,8,10),
     labels = TRUE)
axis(side = 2,
     at = c(-6,-4,-2,0,2,4),
     labels = TRUE)
title(main = "Biplot for PCs 10 Year NBA Stats",
      line = 3,
      adj = .5)
title(xlab = paste("PC 1 (",
                   round(summary(df_pca)$importance[2]*100,
                         digits = 1),
                   "%)",
                   sep = ""),
      ylab = paste("PC 2 (",
                   round(summary(df_pca)$importance[5]*100,
                         digits = 1),
                   "%)",
                   sep = ""),
      line = 2,
      adj = 0.5)

points(x = df_pca$x[,1:2],
       pch = c(rep(20, times = 4885),
               rep(20, times = 133)),
       cex = .5,
       col = c(rep("gold", times=4885),
               rep("blue", times=133)))

par(new = TRUE, las = 1)
plot.window(xlim = c(-1+(3/8), 1+(3/8)), 
            ylim = c(-1-2/21, 1-2/21), 
            asp = 1)
axis(side = 3,
     at = c(-1,-.5,0,.5,1),
     labels = TRUE,
     col = "navy",
     col.ticks = NULL,
     lwd = 2,
     col.axis = "navy")
axis(side = 4,
     at = c(1, .5, 0,-.5,-1),
     labels = TRUE,
     col = "navy",
     col.ticks = NULL,
     lwd = 2,
     col.axis = "navy")

mtext((text = "PC 1 rotations"),
      side = 3,
      cex = 1,
      font = 2,
      family = "sans",
      col = "gray10",
      line = 2)
mtext((text = "PC 2 rotations"),
      side = 4,
      cex = 1,
      font = 2,
      family = "sans",
      col = "gray10",
      line = 2.75,
      las = 3)

box()
abline(v = 0, h = 0, lty = 2, col = "grey25")

arrows(x0 = 0, x1 = df_pca$rotation[,1], 
       y0 = 0, y1 = df_pca$rotation[,2], 
       col = "gray25", 
       length = 0.1, 
       lwd = 2,
       angle = 30)

text(x = df_pca$rotation[,1], y = df_pca$rotation[,2], 
     labels = row.names(df_pca$rotation), 
     cex = 1,
     font = 1,
     col = "gray10", 
     pos = c(4, 3, 2, 1, 3, 1),
     lwd = .2)

legend(x = "topleft", 
       legend = c("MVP candidates","non-MVP Candidates"),
       pch = c(20, 20),
       col = c("blue", "gold"),
       text.font = 2,
       cex = .8,
       pt.cex = 1.0,
       bty = "n",
       x.intersp = 0.5,
       y.intersp = 0.8,
       xpd = FALSE,
       adj = c(0, 0.25))


#Scree Plot
var_explained = df_pca$sdev^2 / sum(df_pca$sdev^2)


var_explained

