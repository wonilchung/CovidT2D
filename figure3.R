rm(list=ls())

library(data.table)
library(dplyr)
library(lubridate)
library(survival)
library(survminer)
library(survsim)
library(glue)
library(ggplot2)
library(reshape)

WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WORKING_DIR)
load('figure3.RData')

# -------------------- #
# Figure 3-a
# -------------------- #

png("figure3_a.png", width = 870, height = 800, type = "cairo")
ggsurv <- ggsurvplot(fit3a, data = df_split2, 
                     size = 1.5, risk.table = F, 
                     conf.int = FALSE, pval = FALSE, censor = F,
                     ggtheme = theme_bw(), 
                     title = "T2D and SARS-CoV-2 (N=432K)",
                     xlab = "Time in Days", 
                     ylab = "Survival Probability", 
                     legend.title = "", 
                     legend.labs = c("Controls", "SARS-CoV-2 patients", "T2D patients", "T2D patients with SARS-CoV-2 infection"),
                     legend = c(0.3, 0.15), 
                     font.title = c(24, "bold", "black"),
                     font.x = c(21, "plain", "black"),
                     font.y = c(21, "plain", "black"), 
                     font.xtickslab = c(14, "plain", " black"),
                     font.ytickslab = c(14, "plain", " black"),
                     font.legend = c(16, "plain", "black"),
                     palette = c("#5C6BC0", "#F9A825", "#42A5F5", "#F3898B"),
                     break.time.by = 100,
                     ylim = c(0.85, 1), 
                     xlim = c(0, 950))

ggsurv$plot <- ggsurv$plot + theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(face = "bold"))
ggsurv
dev.off()

# -------------------- #
# Figure 3-b
# -------------------- #

png("figure3_b.png", width = 870, height = 800, type = "cairo")
ggsurv <- ggsurvplot(fit3b, data = df_split2, 
                     size = 1.5, risk.table = F, 
                     conf.int = FALSE, pval = FALSE, censor = F,
                     ggtheme = theme_bw(), 
                     title = glue("PRS Group(T2D) and SARS-CoV-2 (N=432K)"),
                     xlab = "Time in Days", 
                     ylab = "Survival Probability", 
                     legend.title = "", 
                     legend.labs = c("Low PRS group", "Medium PRS group", "High PRS gruop",
                                     "SARS-CoV-2 patients with Low PRS","SARS-CoV-2 patients with Medium PRS","SARS-CoV-2 patients with High PRS"),
                     legend = c(0.3, 0.15),
                     font.title = c(24, "bold", "black"),
                     font.x = c(21, "plain", "black"),
                     font.y = c(21, "plain", "black"),
                     font.xtickslab = c(14, "plain", " black"),
                     font.ytickslab = c(14, "plain", " black"),
                     font.legend = c(16, "plain", "black"),
                     palette = c("#9933CC", "#283593", "#42A5F5","#99CC33", "#F9A825","#F3898B"),
                     break.time.by = 100,
                     ylim = c(0.9, 1), 
                     xlim = c(0, 950))

ggsurv$plot <- ggsurv$plot + theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(face = "bold"))
ggsurv
dev.off()

# -------------------- #
# Figure 3-c
# -------------------- #

png("figure3_c1.png", width = 800, height = 300, type = "cairo")
obj = ggplot(ncd, aes(x=days, y=total_cases)) +
  geom_line(linewidth=1.5, color = "#283593") + 
  xlab("Time in Days") + ylab("# Confirmed Cases") +
  labs(fill="", colour="", linetype="", title="") + 
  theme_bw() +
  scale_x_continuous(limits = c(0, 950), breaks=seq(0,950,100), labels=seq(0,950,100))+
  scale_y_continuous(limits = c(0, 25000000), breaks=seq(0,25000000,4000000), labels=paste(seq(0,25,4),"M",sep=""))+
  scale_colour_manual(values=c("#283593","#5C6BC0","#1976D2","#42A5F5","#00695C","#009688","#EF6C00","#F9A825","#D32F2F","#F3898B")) +
  theme(plot.title = element_text(lineheight=.8, size=20), 
        axis.text=element_text(size=14), axis.title=element_text(size=21),legend.text=element_text(size=11))
print(obj)
dev.off()

png("figure3_c2.png", width = 800, height = 300, type = "cairo")
obj = ggplot(ncd, aes(x=days, y=total_deaths)) +
  geom_line(linewidth=1.5, color = "#EF6C00") + 
  xlab("Time in Days") + ylab("# Total Deaths") +
  labs(fill="", colour="", linetype="", title="") + 
  theme_bw() +
  scale_x_continuous(limits = c(0, 950), breaks=seq(0,950,100), labels=seq(0,950,100))+
  scale_y_continuous(limits = c(0, 200000), breaks=seq(0,200000,25000), labels=paste(seq(0,200,25),"K",sep=""))+
  scale_colour_manual(values=c("#283593","#5C6BC0","#1976D2","#42A5F5","#00695C","#009688","#EF6C00","#F9A825","#D32F2F","#F3898B")) +
  theme(plot.title = element_text(lineheight=.8, size=20), 
        axis.text=element_text(size=14), axis.title=element_text(size=21),legend.text=element_text(size=11))
print(obj)
dev.off()

# -------------------- #
# Figure 3-d
# -------------------- #

png("figure3_d.png", width = 1000, height = 620, type = "cairo")
obj = ggplot(v, aes(x=days, y=perc_variant, colour=variant, group=variant)) +
  geom_line(linewidth=1.2) + 
  xlab("Time in days") + ylab("Percentage of Variants (%)") +
  labs(fill="", colour="", linetype="", title="") + 
  theme_bw() +
  scale_x_continuous(limits = c(0, 950), breaks=seq(0,950,100), labels=seq(0,950,100))+
  scale_y_continuous(limits = c(0, 100), breaks=seq(0,100,10), labels=paste(seq(0,100,10),sep=""))+
  scale_colour_manual(values=c("#000080","#230000", "#F9A825","#FF0000","#197000","#800080","#008080","#1976D2")) +
  theme(plot.title = element_text(lineheight=.8, size=20), 
        axis.text=element_text(size=14), axis.title=element_text(size=21),legend.text=element_text(size=11))
print(obj)
dev.off()

