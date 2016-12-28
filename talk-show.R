library(dplyr)
library(ggplot2)
library(scales)

tymoshenko <- list.files("~/shuster2/Антон Геращенко", pattern="*.jpg", full.names=TRUE)
yula <- do.call("rbind",lapply(tymoshenko,function(x){as.numeric(unique(na.omit(as.numeric(unlist(strsplit(unlist(substring(x,48,56)), "[^0-9]+"))))))}))

sava <- list.files("~/shuster2/Сергій Соболєв", pattern="*.jpg", full.names=TRUE)
yul <- do.call("rbind",lapply(sava,function(x){as.numeric(unique(na.omit(as.numeric(unlist(strsplit(unlist(substring(x,48,56)), "[^0-9]+"))))))}))

reva <- list.files("~/shuster2/Савік Шустер", pattern="*.jpg", full.names=TRUE)
rev <- do.call("rbind",lapply(reva,function(x){as.numeric(unique(na.omit(as.numeric(unlist(strsplit(unlist(substring(x,48,56)), "[^0-9]+"))))))}))

kobolev <- list.files("~/shuster2/Анатолій Гриценко", pattern="*.jpg", full.names=TRUE)
kob <- do.call("rbind",lapply(kobolev,function(x){as.numeric(unique(na.omit(as.numeric(unlist(strsplit(unlist(substring(x,48,62)), "[^0-9]+"))))))}))

kobolev2 <- list.files("~/shuster2/Сергій Лещенко", pattern="*.jpg", full.names=TRUE)
kob2 <- do.call("rbind",lapply(kobolev2,function(x){as.numeric(unique(na.omit(as.numeric(unlist(strsplit(unlist(substring(x,48,56)), "[^0-9]+"))))))}))

kobolev3 <- list.files("~/shuster2/Дмитро Добродомов", pattern="*.jpg", full.names=TRUE)
kob3 <- do.call("rbind",lapply(kobolev3,function(x){as.numeric(unique(na.omit(as.numeric(unlist(strsplit(unlist(substring(x,48,65)), "[^0-9]+"))))))}))

kobolev4 <- list.files("~/shuster2/Віталій Касько", pattern="*.jpg", full.names=TRUE)
kob4 <- do.call("rbind",lapply(kobolev4,function(x){as.numeric(unique(na.omit(as.numeric(unlist(strsplit(unlist(substring(x,48,56)), "[^0-9]+"))))))}))



tl <- list("Антон Геращенко"=yula, "Сергій Соболєв"=yul, "Савік Шустер"=rev, "Анатолій Гриценко"=kob,
           "Сергій Лещенко"=kob2,"Дмитро Добродомов"=kob3,"Віталій Касько"=kob4)
stripchart(tl,
           main="Multiple stripchart for comparision",
           xlab="yula",
           ylab="yula2",
           method="jitter",
           col=c("orange","red"),
           pch=16
)


library(beeswarm)
beeswarm(tl,
         pch = 16,
         xlab = "", ylab = "Follow-up time (months)",
         labels = c("ER neg", "ER pos"))
legend("topright", legend = c("Yes", "No"),
       title = "Censored", pch = 16, col = 1:2)

b_body <- lapply(filenames, upload_file)

b_body2 <- lapply(b_body, function(x){if(length(x)>1) {x}})


beeswarm(tl, col=sample(colors(), 27), pch=19, method="swarm", cex=0.5)

par(las=1)
beeswarm(tl, col=sample(colors(), 27), pch=19, method="center", 
         cex=0.5, horizontal=TRUE, xlab="Annual Income, Dollars", 
         ylab="Occupation Category", main="Distribution of Income, by Occupation Caetgory", 
         bty="n")


ggplot() + geom_line(aes(tl$`Антон Геращенко`,2)) +
  geom_vline(xintercept = tl$`Сергій Соболєв`,3,4) +
  geom_vline(xintercept = tl$`Савік Шустер`,5,6) +
  geom_vline(xintercept = tl$`Анатолій Гриценко`,7,8)+
  geom_vline(xintercept = tl$`Сергій Лещенко`,,8)
geom_vline(xintercept = tl$`Дмитро Добродомов`,7,8)
geom_vline(xintercept = tl$`Віталій Касько`,7,8)

library(lubridate)
td <- seconds_to_period(5000)
hms(sprintf('%02d:%02d:%02d',  td@hour, minute(td), second(td)))

ggplot() +
  geom_linerange(aes(x=tl$`Антон Геращенко`, y=0, ymin=9, ymax=10),color="lightblue") +
  geom_linerange(aes(x=tl$`Сергій Соболєв`, y=0, ymin=7.5, ymax=8.5),color="lightblue") +
  geom_linerange(aes(x=tl$`Савік Шустер`, y=0, ymin=6, ymax=7),color="lightblue") +
  geom_linerange(aes(x=tl$`Анатолій Гриценко`, y=0, ymin=4.5, ymax=5.5),color="lightblue") +
  geom_linerange(aes(x=tl$`Сергій Лещенко`, y=0, ymin=3, ymax=4),color="lightblue") +
  geom_linerange(aes(x=tl$`Дмитро Добродомов`, y=0, ymin=1.5, ymax=2.5),color="lightblue") +
  geom_linerange(aes(x=tl$`Віталій Касько`, y=0, ymin=0, ymax=1),color="lightblue") +
  scale_x_continuous(breaks = c(1800,3600,5400,7200,9000,10800,12600)) + theme_classic()
xlab('Genomic location') +
  ylab('Minor allele frequency')

# Сума

pravo <- list.files("~/Право на владу",recursive = T)  
freq = sapply(pravo, function(x){as.numeric(unique(na.omit(as.numeric(unlist(strsplit(unlist(substring(x,1,40)), "[^0-9]+"))))))})
tok <- data.frame(name = sub("/.*", "", attr(freq,"names")))
tok_sum4_pravo <- tok %>% group_by(name) %>% summarise(freq=n())
tok_sum4_pravo <- tok_sum4_pravo %>% arrange(freq)

ggplot() + 
  geom_bar(aes(name, freq), data = tok_sum4_pravo, stat="identity", position ="stack", fill = "lightblue") + 
  scale_y_continuous(breaks = c(300,600,900,1200,1500,1800,2100,2400), labels = comma) +
  scale_x_discrete(limits=tok_sum4_pravo$name) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none") + coord_flip()

luts <- list.files("~/Право на владу/Юрій Луценко", pattern="*.jpg", full.names=TRUE)
luts1 <- do.call("rbind",lapply(luts,function(x){as.numeric(unique(na.omit(as.numeric(unlist(strsplit(unlist(substring(x,30,60)), "[^0-9]+"))))))}))

nata <- list.files("~/Право на владу/Наталія Мосейчук", pattern="*.jpg", full.names=TRUE)
nata1 <- do.call("rbind",lapply(nata,function(x){as.numeric(unique(na.omit(as.numeric(unlist(strsplit(unlist(substring(x,30,70)), "[^0-9]+"))))))}))

ivan <- list.files("~/Право на владу/Сергій Іванов", pattern="*.jpg", full.names=TRUE)
ivan1 <- do.call("rbind",lapply(ivan,function(x){as.numeric(unique(na.omit(as.numeric(unlist(strsplit(unlist(substring(x,45,70)), "[^0-9]+"))))))}))

vas <- list.files("~/Право на владу/Ольга Василевська", pattern="*.jpg", full.names=TRUE)
vas1 <- do.call("rbind",lapply(vas,function(x){as.numeric(unique(na.omit(as.numeric(unlist(strsplit(unlist(substring(x,30,70)), "[^0-9]+"))))))}))

tl3 <- list("Юрій Луценко"=luts1, "Наталія Мосійчук"=nata1, "Сергій Іванов"=ivan1, "Ольга Василевська"=vas1)
ggplot() +
  geom_linerange(aes(x=tl3$`Юрій Луценко`, y=0, ymin=4.5, ymax=5.5),color="lightblue") +
  geom_linerange(aes(x=tl3$`Наталія Мосійчук`, y=0, ymin=3, ymax=4),color="lightblue") +
  geom_linerange(aes(x=tl3$`Сергій Іванов`, y=0, ymin=1.5, ymax=2.5),color="lightblue") +
  geom_linerange(aes(x=tl3$`Ольга Василевська`, y=0, ymin=0, ymax=1),color="lightblue") +
  scale_x_continuous(breaks = c(900,1800,2700,3600,4500,5400)) + theme_classic()

ggplot(pravo_channel, aes(x=Time, y=`1+1`)) + geom_area(fill="lightblue") +
  scale_y_continuous(breaks = c(0,250000,500000,750000,1000000,1250000), labels = comma) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none")



ggplot(pravo_channel,aes(x=Time)) + 
  scale_y_continuous(breaks = c(0,500000,1000000,1500000))+
  geom_line(aes(y=INTER),color="blue", size=1) +
  geom_area(aes(y=`1+1`),color="red",  fill="red",alpha=I(.1), size=1) +
  geom_line(aes(y=ICTV),color="orange", size=1) +
  geom_line(aes(y=STB),color="pink", size=1) +
  geom_line(aes(y=`CHANNEL UKRAINE`),color="gold", size=1) + theme_classic()

ggplot() + 
  geom_area(Report_09_12[13:56,],aes(x=Time,y=`3S`), color="lightblue", fill="lightblue") + 
  scale_y_continuous(breaks = c(0,1000000,2000000,3000000))+
  geom_area(aes(y=INTER),color="blue", fill="blue") +
  geom_line(aes(y=INTER),color="blue") +
  geom_line(aes(y=`1+1`),color="red") +
  geom_line(aes(y=ICTV),color="orange") +
  geom_line(aes(y=STB),color="pink") +
  geom_line(aes(y=`CHANNEL UKRAINE`),color="gold") + theme_classic()


ggplot(Report_09_12[19:40,],aes(x=Time)) + 
  scale_y_continuous(breaks = c(0,1000000,2000000,3000000))+
  geom_area(aes(y=INTER),color="blue", fill="blue") +
  geom_line(aes(y=`3S`), color="lightblue") + 
  geom_line(aes(y=`1+1`),color="red") +
  geom_line(aes(y=ICTV),color="orange") +
  geom_line(aes(y=STB),color="pink") +
  geom_line(aes(y=`CHANNEL UKRAINE`),color="gold") + theme_classic()

ggplot(Report_09_12[13:56,],aes(x=Time)) + 
  scale_y_continuous(breaks = c(0,350000,500000,600000,700000,850000))+
  geom_area(aes(y=`3S`), color="lightblue", fill="lightblue") + theme_classic()

ggplot(Report_09_12[19:40,],aes(x=Time)) + 
  scale_y_continuous(breaks = c(0,500000,1000000,1500000))+
  geom_area(aes(y=INTER),color="lightblue", fill="lightblue") + theme_classic()



require(data.table)
require(ggplot2)
require(grid)

setsTimeline <- data.table(Set=c("x","y","z","x","y","z","x","y","z","x","y","z","x"),
                           StartDate=c(1380708900,1402963200,1420070400,1421280000,1410912000,1396310400,1397520000,1418860800,1404172800,1405382400,1395100800,1412121600,1413331200),
                           EndDate=  c(1395099900,1404171900,1421279100,1430985600,1412120700,1397519100,1402962300,1420069500,1405381500,1410911100,1396309500,1413330300,1418859900))

setsTimeline[,StartLabel:=as.POSIXct(StartDate,tz="UTC",origin="1970-01-01")]

breaks <- c(1380708900,1395100800,1402963200,1410912000,1418860800,1430985600)
labels <- as.POSIXct(breaks,tz="UTC",origin="1970-01-01")


library(ggplot2)
library(ggrepel)
ggplot(timeline_pv, aes(colour=Тема)) + 
  geom_segment(aes(x=Початок, xend=Кінець, y="Тема", yend="Тема"), size=10) +
  geom_text_repel(aes(x=Початок, y="Тема", label=Тема)) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none")
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        aspect.ratio=5e-02, 
        axis.text.x = element_text(colour='black', angle = 45, size = 10, hjust = 1, vjust = 1),
        legend.text = element_text(colour='black', size = 10),
        legend.title = element_text(colour='black', size = 0),
        legend.position = 'top',
        plot.title = element_text(colour='black', size = 10),
        panel.margin = unit(1, "cm")) +
  xlab(NULL) + 
  ylab(NULL) 


ggplot(mtcars, aes(wt, mpg)) +
  geom_point(color = 'red') +
  
  theme_classic(base_size = 16)




