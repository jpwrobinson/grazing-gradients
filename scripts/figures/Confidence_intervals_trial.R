#add confidence intervals
devtools::install_github("remkoduursma/bootpredictlme4")
library(bootpredictlme4)
library(visreg)

p.coral.b <- predict(m.browser, newdata=data.frame(nd.b), re.form=NA, se.fit=TRUE, nsim=500)

visreg(m.browser, "hard.coral", ylab="biomass kgha^-1", gg=TRUE) + 
  xlab("Hard coral (%)") + ylab(expression(paste("biomass kg ", "ha"^-1)))+
  scale_color_manual(values = cols.named)



visreg(m.grazer, "hard.coral", ylab="biomass kgha^-1", gg=TRUE)
visreg(m.scraper, "hard.coral", ylab="biomass kgha^-1", gg=TRUE)




visreg(m.browser, "hard.coral", ylab="biomass kgha^-1") + 
  scale_y_log10(labels=comma, limit=c(10, 500)) +
  scale_color_manual(values = cols.named) +
  scale_x_continuous(breaks = coral.lab$breaks, labels = coral.lab$labels) +
  guides(col=F) +
  xlab("Hard coral (%)") + ylab(expression(paste("biomass kg ", "ha"^-1)))  +
  geom_rug(data=deciles, aes(hard.coral, browser), sides='b', alpha=1, col='grey50',size=1)


g1 <- ggplot(p.coral, aes(x, 10^y)) + 
  geom_line(size=linewidth, aes(color = factor(model))) + 
  #geom_point(data=h.means, aes(macroalgae, biom, color=FG), alpha=0.2, size=2) +
  labs(title = "") +
  scale_y_log10(labels=comma, limit=c(10, 500)) +
  scale_color_manual(values = cols.named) +
  scale_x_continuous(breaks = coral.lab$breaks, labels = coral.lab$labels) +
  guides(col=F) +
  xlab("Hard coral (%)") + ylab(expression(paste("biomass kg ", "ha"^-1)))  +
  geom_rug(data=deciles, aes(hard.coral, browser), sides='b', alpha=1, col='grey50',size=1)


g2 <- ggplot(p.algae, aes(x, 10^y)) + 
  geom_line(size=linewidth, aes(color = factor(model))) + 
  #geom_point(data=h.means, aes(macroalgae, biom, color=FG), alpha=0.2, size=2) +
  labs(title = "") +
  scale_y_log10(labels=comma, limit=c(10, 500)) +
  scale_color_manual(values = cols.named) +
  scale_x_continuous(breaks = ma.lab$breaks, labels = ma.lab$labels) +
  guides(col=F) +
  xlab("Macroalgae (%)") + ylab('')  + 
  geom_rug(data=deciles, aes(macroalgae, browser), sides='b', alpha=1, col='grey50',size=1)

g3 <- ggplot(p.substrate, aes(x, 10^y)) + 
  geom_line(size=linewidth, aes(color = model)) + 
  #geom_point(data=h.means, aes(substrate, biom, color=FG), alpha=0.2, size=2) +
  labs(title = "") +
  scale_y_log10(labels=comma, limit=c(10, 500)) +
  scale_color_manual(values = cols.named) +
  guides(col=F) +
  scale_x_continuous(breaks = substrate.lab$breaks, labels = substrate.lab$labels) +
  xlab("Available substrate (%)") + ylab("") +
  geom_rug(data=deciles, aes(substrate, browser), sides='b', alpha=1, col='grey50',size=1)

g4 <- ggplot(p.complexity, aes(x, 10^y)) + 
  geom_line(size=linewidth, aes(color = model)) + 
  #geom_point(data=h.means, aes(substrate, biom, color=FG), alpha=0.2, size=2) +
  labs(title = "") +
  scale_y_log10(labels=comma, limit=c(10, 500)) +
  scale_color_manual(values = cols.named) +
  guides(col=F) +
  scale_x_continuous(breaks = complexity.lab$breaks, labels = complexity.lab$labels) +
  xlab("Complexity") + ylab(expression(paste("biomass kg ", "ha"^-1))) +
  geom_rug(data=deciles, aes(complexity, browser), sides='b', alpha=1, col='grey50',size=1)

g5 <- ggplot(p.rubble, aes(x, 10^y)) + 
  geom_line(size=linewidth, aes(color = model)) + 
  #geom_point(data=h.means, aes(rubble, biom, color=FG), alpha=0.2, size=2) +
  labs(title = "") +
  scale_y_log10(labels=comma, limit=c(10, 500)) +
  scale_color_manual(values = cols.named) +
  guides(col=F) +
  scale_x_continuous(breaks = rubble.lab$breaks, labels = rubble.lab$labels) +
  xlab("Rubble (%)") + ylab("") +
  geom_rug(data=deciles, aes(rubble, browser), sides='b', alpha=1, col='grey50',size=1)


g6 <- ggplot(fish.master, aes(x, 10^y)) + 
  geom_line(size=linewidth, aes(color = model)) + 
  #geom_point(data=h.means, aes(fish.biom, biom, color=FG), alpha=0.2, size=2) +
  labs(title = "") +
  scale_y_log10(labels=comma, limit=c(10, 1500)) +
  scale_color_manual(values = cols.named) +
  scale_x_continuous(breaks = fishable.lab$breaks, labels = comma(fishable.lab$labels)) +
  guides(col=F) +
  xlab(expression(paste("Fishable biomass (kg ", "ha"^-1,")"))) + ylab("") +
  geom_rug(data=deciles, aes(fish.biom, browser), sides='b', alpha=1, col='grey50',size=1)


grid.arrange(g1, g4, g5, g6)
