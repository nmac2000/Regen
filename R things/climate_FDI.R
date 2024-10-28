##############################
# Graphs for FDI copy climate variables
# Nat
# October 23, 2024
#############################
boxplot(PPT_sm ~ interaction(Year, FDI_count_bin), data=messy9,
        col=colors, 
        xlab="FDI Presence and Year after Fire", ylab="PPT_sm", main="PPT_sm 2009 - FDI")

points(jitter(as.numeric(interaction(messy9$Year, messy9$FDI_count_bin))), messy9$PPT_sm, 
       col=rgb(0, 0, 0, 0.7), 
       pch=16, 
       cex=.8)

boxplot(PPT_sm ~ interaction(Year, FDI_count_bin), data=messy10,
        col=colors, 
        xlab="FDI Presence and Year after Fire", ylab="PPT_sm", main="PPT_sm 2010 - FDI")

points(jitter(as.numeric(interaction(messy10$Year, messy10$FDI_count_bin))), messy10$PPT_sm, 
       col=rgb(0, 0, 0, 0.7), 
       pch=16, 
       cex=.8)

boxplot(PPT_sm ~ interaction(Year, FDI_count_bin), data=messy15,
        col=colors, 
        xlab="FDI Presence and Year after Fire", ylab="PPT_sm", main="PPT_sm 2015 - FDI")

points(jitter(as.numeric(interaction(messy15$Year, messy15$FDI_count_bin))), messy15$PPT_sm, 
       col=rgb(0, 0, 0, 0.7), 
       pch=16, 
       cex=.8)


boxplot(PPT_sm ~ interaction(Year, FDI_count_bin), data=messy17,
        col=colors, 
        xlab="FDI Presence and Year after Fire", ylab="PPT_sm", main="PPT_sm 2017 - FDI")

points(jitter(as.numeric(interaction(messy17$Year, messy17$FDI_count_bin))), messy17$PPT_sm, 
       col=rgb(0, 0, 0, 0.5), 
       pch=16, 
       cex=.8)
