setwd("C:/Users/H/Desktop/RttPEDMAP/H/c1")
h_c1_inc = read.table("1MB-0He-0M.incidenceofcommonrunsperSNP", header = T, stringsAsFactors = F, sep = ",")
plot(h_c1_inc$Position,h_c1_inc$Runs, type = "l", col = "blue", pch = 16, xaxt = 'n', ylab = "Number of TOHs",
     xlab = "SNP Position (BTA1)", main = "SNP occurrences -BTA1",
     lwd = 2)
myTicks = axTicks(1)
axis(1, at = axTicks(1), labels = formatC(myTicks, format = 'd', big.mark=","))
abline(h=quantile(h_c1_inc$Runs,prob=1-2/100),col="black",lty=2)


library("ggplot2")
library("RColorBrewer")

##Graficar SNP occurrences por cromosoma

setwd("C:/Users/H/Desktop/RttPEDMAP/H")
h_inc = read.table("1MB-0He-0M.incidenceofcommonrunsperSNP", header = T, stringsAsFactors = F, sep = ",")
mycolors = colorRampPalette(brewer.pal(name="Dark2", n = 8))(29)
quantile(h_inc$Runs_Perc,prob=1-1/100)

h_inc$Runs_Perc = (h_inc$Runs)/36*100



p1 = ggplot(h_inc,aes(Position,Runs_Perc,colour = factor(h_inc$Chr))) +
  geom_point() + scale_color_manual(values = mycolors) + 
  geom_abline(slope = 0, intercept = 25, color="black",lty=2,lwd=1) + 
  labs(x="SNP position",y="Percentage of TOHs") + 
  scale_y_continuous(limits = c(0, 80))
pout <- p1 + 
  facet_grid(. ~ Chr, scales = "free_x") +
  theme(
    legend.position="none",
    axis.text.x = element_blank(),
    axis.ticks = element_blank())
pout

##############################################################

setwd("C:/Users/H/Desktop/RttPEDMAP/L")
l_inc = read.table("1MB-0He-0M.incidenceofcommonrunsperSNP", header = T, stringsAsFactors = F, sep = ",")
mycolors = colorRampPalette(brewer.pal(name="Dark2", n = 8))(29)
quantile(l_inc$Runs_Perc,prob=1-1/100)

l_inc$Runs_Perc = (l_inc$Runs)/25*100

p1 = ggplot(l_inc,aes(Position,Runs_Perc,colour = factor(l_inc$Chr))) +
  geom_point() + scale_color_manual(values = mycolors) + 
  geom_abline(slope = 0, intercept = 16, color="black",lty=2,lwd=1) + 
  labs(x="SNP position",y="Percentage of TOHs") + scale_y_continuous(limits = c(0, 80))
pout <- p1 + 
  facet_grid(. ~ Chr, scales = "free_x") +
  theme(
    legend.position="none",
    axis.text.x = element_blank(),
    axis.ticks = element_blank())
pout

