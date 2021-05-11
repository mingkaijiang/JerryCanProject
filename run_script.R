### Script for Carly Callaghan to perform statistical analysis
### Project: Jerry can water quality
### Written by: Mingkai Jiang (m.jiang@westernsydney.edu.au)


############################# Set up #################################
### start by preparing all necessary code libraries
#### clear wk space
rm(list=ls(all=TRUE))

#### read prepare.R
source("prepare.R")


############################# Statistical analysis #################################
### read input
myDF <- read.csv("input/Carly_dataset_raw_11052021.csv")

### convert variable class
myDF$Date <- as.Date(myDF$Date, "%d-%b-%y")
myDF$Jerrycan <- as.factor(myDF$Jerrycan)


### summary data by means and sd, ignoring date for now
sumDF1 <- summaryBy(Free+Total+Temp+Turbidity+TDS+EC+pH~Location,
                   FUN=c(mean, sd), data=myDF, keep.names=T, na.rm=T)


### make plots
## free
p1 <- ggplot(data=sumDF1) +
    geom_bar(aes(x=Location, y=Free.mean, fill=Location), 
             stat="identity")+
    geom_errorbar(aes(x=Location, ymin=Free.mean-Free.sd, ymax=Free.mean+Free.sd), 
             stat="identity", width=0.2)+
    xlab("") +
    ylab("Free") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=12), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.position="none",
          legend.text.align=0,
          legend.direction="vertical")

## Total
p2 <- ggplot(data=sumDF1) +
    geom_bar(aes(x=Location, y=Total.mean, fill=Location), 
             stat="identity")+
    geom_errorbar(aes(x=Location, ymin=Total.mean-Total.sd, ymax=Total.mean+Total.sd), 
                  stat="identity", width=0.2)+
    xlab("") +
    ylab("Total") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=12), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.position="none",
          legend.text.align=0,
          legend.direction="vertical")

## Turbidity
p3 <- ggplot(data=sumDF1) +
    geom_bar(aes(x=Location, y=Turbidity.mean, fill=Location), 
             stat="identity")+
    geom_errorbar(aes(x=Location, ymin=Turbidity.mean-Turbidity.sd, ymax=Turbidity.mean+Turbidity.sd), 
                  stat="identity", width=0.2)+
    xlab("") +
    ylab("Turbidity (NTU)") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=12), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.position="none",
          legend.text.align=0,
          legend.direction="vertical")


## TDS
p4 <- ggplot(data=sumDF1) +
    geom_bar(aes(x=Location, y=TDS.mean, fill=Location), 
             stat="identity")+
    geom_errorbar(aes(x=Location, ymin=TDS.mean-TDS.sd, ymax=TDS.mean+TDS.sd), 
                  stat="identity", width=0.2)+
    xlab("") +
    ylab("TDS (mg/L)") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=12), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.position="none",
          legend.text.align=0,
          legend.direction="vertical")


## EC
p5 <- ggplot(data=sumDF1) +
    geom_bar(aes(x=Location, y=EC.mean, fill=Location), 
             stat="identity")+
    geom_errorbar(aes(x=Location, ymin=EC.mean-EC.sd, ymax=EC.mean+EC.sd), 
                  stat="identity", width=0.2)+
    xlab("") +
    ylab("EC (us/cm)") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=12), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.position="none",
          legend.text.align=0,
          legend.direction="vertical")

## pH
p6 <- ggplot(data=sumDF1) +
    geom_bar(aes(x=Location, y=pH.mean, fill=Location), 
             stat="identity")+
    geom_errorbar(aes(x=Location, ymin=pH.mean-pH.sd, ymax=pH.mean+pH.sd), 
                  stat="identity", width=0.2)+
    xlab("") +
    ylab("pH") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=12), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.position="none",
          legend.text.align=0,
          legend.direction="vertical")

## temperature
p7 <- ggplot(data=sumDF1) +
    geom_bar(aes(x=Location, y=Temp.mean, fill=Location), 
             stat="identity")+
    geom_errorbar(aes(x=Location, ymin=Temp.mean-Temp.sd, ymax=Temp.mean+Temp.sd), 
                  stat="identity", width=0.2)+
    xlab("") +
    ylab("Temperature (degree C)") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=12), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.position="none",
          legend.text.align=0,
          legend.direction="vertical")


## save plot
combined_plot <- plot_grid(p1, p2, p3, p4, p5, p6, p7,
                           ncol=2, align="vh", axis = "l")

save_plot(paste0("output/overall_effect_comparison.pdf"),
          combined_plot, base_width=10, base_height = 8)


### Make temporal comparison
sumDF2 <- summaryBy(Free+Total+Temp+Turbidity+TDS+EC+pH~Location+Date,
                    FUN=c(mean, sd), data=myDF, keep.names=T, na.rm=T)


### plot 
p1 <- ggplot(data=sumDF2) +
    geom_point(aes(x=Date, y=Free.mean, fill=Location), pch=21, size=3)+
    geom_errorbar(aes(x=Date, ymin=Free.mean-Free.sd, ymax=Free.mean+Free.sd, col=Location),
                  width=0.2)+
    xlab("") +
    ylab("Free") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=12), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.position="none",
          legend.text.align=0,
          legend.direction="vertical")

p2 <- ggplot(data=sumDF2) +
    geom_point(aes(x=Date, y=Total.mean, fill=Location), pch=21, size=3)+
    geom_errorbar(aes(x=Date, ymin=Total.mean-Total.sd, ymax=Total.mean+Total.sd, col=Location),
                  width=0.2)+
    xlab("") +
    ylab("Total") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=12), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.position="none",
          legend.text.align=0,
          legend.direction="vertical")


p3 <- ggplot(data=sumDF2) +
    geom_point(aes(x=Date, y=Turbidity.mean, fill=Location), pch=21, size=3)+
    geom_errorbar(aes(x=Date, ymin=Turbidity.mean-Turbidity.sd, ymax=Turbidity.mean+Turbidity.sd, col=Location),
                  width=0.2)+
    xlab("") +
    ylab("Turbidity (NTU)") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=12), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.position="none",
          legend.text.align=0,
          legend.direction="vertical")

p4 <- ggplot(data=sumDF2) +
    geom_point(aes(x=Date, y=TDS.mean, fill=Location), pch=21, size=3)+
    geom_errorbar(aes(x=Date, ymin=TDS.mean-TDS.sd, ymax=TDS.mean+TDS.sd, col=Location),
                  width=0.2)+
    xlab("") +
    ylab("TDS (mg/L)") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=12), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.position="none",
          legend.text.align=0,
          legend.direction="vertical")

p5 <- ggplot(data=sumDF2) +
    geom_point(aes(x=Date, y=EC.mean, fill=Location), pch=21, size=3)+
    geom_errorbar(aes(x=Date, ymin=EC.mean-EC.sd, ymax=EC.mean+EC.sd, col=Location),
                  width=0.2)+
    xlab("") +
    ylab("EC (us/cm)") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=12), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.position="none",
          legend.text.align=0,
          legend.direction="vertical")

p6 <- ggplot(data=sumDF2) +
    geom_point(aes(x=Date, y=pH.mean, fill=Location), pch=21, size=3)+
    geom_errorbar(aes(x=Date, ymin=pH.mean-pH.sd, ymax=pH.mean+pH.sd, col=Location),
                  width=0.2)+
    xlab("") +
    ylab("pH") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=12), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.position="none",
          legend.text.align=0,
          legend.direction="vertical")

p7 <- ggplot(data=sumDF2) +
    geom_point(aes(x=Date, y=Temp.mean, fill=Location), pch=21, size=3)+
    geom_errorbar(aes(x=Date, ymin=Temp.mean-Temp.sd, ymax=Temp.mean+Temp.sd, col=Location),
                  width=0.2)+
    xlab("") +
    ylab("Temperature (degree C)") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=12), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.position="none",
          legend.text.align=0,
          legend.direction="vertical")


## save plot
combined_plot <- plot_grid(p1, p2, p3, p4, p5, p6, p7,
                           ncol=2, align="vh", axis = "l")

save_plot(paste0("output/temporal_effect_comparison.pdf"),
          combined_plot, base_width=12, base_height = 8)


### Perform statistical test - prepare
myDF <- data.table(myDF)
myDF$Date <- as.factor(as.character(myDF$Date))
myDF$Location <- as.factor(myDF$Location)

##########################################################
### make statistics and plots - free
bxp <- ggboxplot(
    myDF, x = "Date", y = "Free",
    color = "Location", palette = "jco"
)
bxp

### anova test
res.aov <- anova_test(
    data = myDF, dv = Free, wid = Jerrycan,
    within = c(Location, Date))
get_anova_table(res.aov)

### post hoc
one.way <- myDF %>%
    group_by(Date) %>%
    anova_test(dv = Free, wid = Jerrycan, within = Location) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni")
one.way


### pair-wise comparison 
pwc <- myDF %>%
    group_by(Date) %>%
    pairwise_t_test(
        Free ~ Location, paired = TRUE,
        p.adjust.method = "bonferroni"
    )
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "Date")
pdf("output/Free_statistics.pdf", width=12, height=10)
bxp + 
    stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
    labs(
        subtitle = get_test_label(res.aov, detailed = TRUE),
        caption = get_pwc_label(pwc)
    )
dev.off()



##########################################################
### make statistics and plots - total
bxp <- ggboxplot(
    myDF, x = "Date", y = "Total",
    color = "Location", palette = "jco"
)
bxp

### anova test
res.aov <- anova_test(
    data = myDF, dv = Total, wid = Jerrycan,
    within = c(Location, Date))
get_anova_table(res.aov)

### post hoc
one.way <- myDF %>%
    group_by(Date) %>%
    anova_test(dv = Total, wid = Jerrycan, within = Location) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni")
one.way


### pair-wise comparison 
pwc <- myDF %>%
    group_by(Date) %>%
    pairwise_t_test(
        Total ~ Location, paired = TRUE,
        p.adjust.method = "bonferroni"
    )
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "Date")
pdf("output/Total_statistics.pdf", width=12, height=10)
bxp + 
    stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
    labs(
        subtitle = get_test_label(res.aov, detailed = TRUE),
        caption = get_pwc_label(pwc)
    )
dev.off()



##########################################################
### make statistics and plots - turbidity
bxp <- ggboxplot(
    myDF, x = "Date", y = "Turbidity",
    color = "Location", palette = "jco"
)
bxp

### anova test
res.aov <- anova_test(
    data = myDF, dv = Turbidity, wid = Jerrycan,
    within = c(Location, Date))
get_anova_table(res.aov)

### post hoc
one.way <- myDF %>%
    group_by(Date) %>%
    anova_test(dv = Turbidity, wid = Jerrycan, within = Location) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni")
one.way


### pair-wise comparison 
pwc <- myDF %>%
    group_by(Date) %>%
    pairwise_t_test(
        Turbidity ~ Location, paired = TRUE,
        p.adjust.method = "bonferroni"
    )
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "Date")
pdf("output/Turbidity_statistics.pdf", width=12, height=10)
bxp + 
    stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
    labs(
        subtitle = get_test_label(res.aov, detailed = TRUE),
        caption = get_pwc_label(pwc)
    )
dev.off()


##########################################################
### make statistics and plots - TDS
bxp <- ggboxplot(
    myDF, x = "Date", y = "TDS",
    color = "Location", palette = "jco"
)
bxp

### anova test
res.aov <- anova_test(
    data = myDF, dv = TDS, wid = Jerrycan,
    within = c(Location, Date))
get_anova_table(res.aov)

### post hoc
#one.way <- myDF %>%
#    group_by(Date) %>%
#    anova_test(dv = TDS, wid = Jerrycan, within = Location) %>%
#    get_anova_table() %>%
#    adjust_pvalue(method = "bonferroni")
#one.way
#

### pair-wise comparison 
#pwc <- myDF %>%
#    group_by(Date) %>%
#    pairwise_t_test(
#        TDS ~ Location, paired = TRUE,
#        p.adjust.method = "bonferroni"
#    )
#pwc

# Visualization: box plots with p-values
#pwc <- pwc %>% add_xy_position(x = "Date")
#pdf("output/TDS_statistics.pdf", width=12, height=10)
#bxp + 
#    stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
#    labs(
#        subtitle = get_test_label(res.aov, detailed = TRUE),
#        caption = get_pwc_label(pwc)
#    )
#dev.off()


##########################################################
### make statistics and plots - EC
bxp <- ggboxplot(
    myDF, x = "Date", y = "EC",
    color = "Location", palette = "jco"
)
bxp

### anova test
res.aov <- anova_test(
    data = myDF, dv = EC, wid = Jerrycan,
    within = c(Location, Date))
get_anova_table(res.aov)

### post hoc
one.way <- myDF %>%
    group_by(Date) %>%
    anova_test(dv = EC, wid = Jerrycan, within = Location) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni")
one.way


### pair-wise comparison 
pwc <- myDF %>%
    group_by(Date) %>%
    pairwise_t_test(
        EC ~ Location, paired = TRUE,
        p.adjust.method = "bonferroni"
    )
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "Date")
pdf("output/EC_statistics.pdf", width=12, height=10)
bxp + 
    stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
    labs(
        subtitle = get_test_label(res.aov, detailed = TRUE),
        caption = get_pwc_label(pwc)
    )
dev.off()

##########################################################
### make statistics and plots - pH
bxp <- ggboxplot(
    myDF, x = "Date", y = "pH",
    color = "Location", palette = "jco"
)
bxp

### anova test
res.aov <- anova_test(
    data = myDF, dv = pH, wid = Jerrycan,
    within = c(Location, Date))
get_anova_table(res.aov)

### post hoc
one.way <- myDF %>%
    group_by(Date) %>%
    anova_test(dv = pH, wid = Jerrycan, within = Location) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni")
one.way


### pair-wise comparison 
pwc <- myDF %>%
    group_by(Date) %>%
    pairwise_t_test(
        pH ~ Location, paired = TRUE,
        p.adjust.method = "bonferroni"
    )
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "Date")
pdf("output/pH_statistics.pdf", width=12, height=10)
bxp + 
    stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
    labs(
        subtitle = get_test_label(res.aov, detailed = TRUE),
        caption = get_pwc_label(pwc)
    )
dev.off()

##########################################################
### make statistics and plots - Temp
bxp <- ggboxplot(
    myDF, x = "Date", y = "Temp",
    color = "Location", palette = "jco"
)
bxp

### anova test
res.aov <- anova_test(
    data = myDF, dv = Temp, wid = Jerrycan,
    within = c(Location, Date))
get_anova_table(res.aov)

### post hoc
one.way <- myDF %>%
    group_by(Date) %>%
    anova_test(dv = Temp, wid = Jerrycan, within = Location) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni")
one.way


### pair-wise comparison 
pwc <- myDF %>%
    group_by(Date) %>%
    pairwise_t_test(
        Temp ~ Location, paired = TRUE,
        p.adjust.method = "bonferroni"
    )
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "Date")
pdf("output/Temp_statistics.pdf", width=12, height=10)
bxp + 
    stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
    labs(
        subtitle = get_test_label(res.aov, detailed = TRUE),
        caption = get_pwc_label(pwc)
    )
dev.off()


### end