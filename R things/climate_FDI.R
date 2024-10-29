##############################
# Graphs for FDI copy climate variables
# Nat
# October 23, 2024
#############################
boxplot(CMI_sm ~ interaction(Year, PLI_count_bin), data=messy,
        col=colors, 
        xlab="PLI Presence and Year after Fire", ylab="CMI_sm", main="CMI_sm All - PLI")

points(jitter(as.numeric(interaction(messy$Year, messy$PLI_count_bin))), messy$CMI_sm, 
       col=rgb(0, 0, 0, 0.5), 
       pch=16, 
       cex=.8)

boxplot(CMI_sm ~ interaction(Year, FDI_count_bin), data=messy,
        col=colors, 
        xlab="FDI Presence and Year after Fire", ylab="CMI_sm", main="CMI_sm All - FDI")

points(jitter(as.numeric(interaction(messy$Year, messy$FDI_count_bin))), messy$CMI_sm, 
       col=rgb(0, 0, 0, 0.5), 
       pch=16, 
       cex=.8)

