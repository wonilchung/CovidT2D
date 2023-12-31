rm(list=ls())

library(data.table)
library(dplyr)
library(survival)
library(survminer)
library(RColorBrewer)

WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WORKING_DIR)
load('figure4.RData')

# -------------------- #
# Figure 4-a
# -------------------- #

png("figure4_a.png", width = 760, height = 800, type = "cairo")
ggsurv <- ggsurvplot(fit4a, data = surv2,
                     size = 1.5, risk.table = F, conf.int = FALSE, pval = FALSE, censor = F, 
                     ggtheme = theme_bw(),
                     title = "T2D (N=101K)",
                     xlab = "Time in Days",
                     ylab = "Survival Probability", 
                     legend.title = "", 
                     legend.labs = c("Controls","T2D patients"),
                     legend = c(0.3, 0.15), 
                     font.title = c(24, "bold", "black"),
                     font.x = c(21, "plain", "black"),
                     font.y = c(21, "plain", "black"),
                     font.xtickslab = c(14, "plain", " black"), 
                     font.ytickslab = c(14, "plain", " black"),
                     font.legend = c(16, "plain", "black"), 
                     palette = c("#9933CC","#F3898B"), 
                     break.time.by = 100, ylim = c(0.9, 1), xlim = c(0, 950))

ggsurv$plot <- ggsurv$plot + theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(face = "bold"))
ggsurv
dev.off()

# -------------------- #
# Figure 4-b
# -------------------- #

png("figure4_b.png", width = 760, height = 800, type = "cairo")
ggsurv <- ggsurvplot(fit4b, data = surv2,
                     size = 1.5, risk.table = F, conf.int = FALSE, pval = FALSE, censor = F,
                     ggtheme = theme_bw(), 
                     title = "PRS Group(T2D) (N=101K)",
                     xlab = "Time in Days",
                     ylab = "Survival Probability",
                     legend.title = "", 
                     legend.labs = c("Low PRS group","Medium PRS group","High PRS group"), 
                     legend = c(0.3, 0.15), 
                     font.title = c(24, "bold", "black"), 
                     font.x = c(21, "plain", "black"), 
                     font.y = c(21, "plain", "black"),
                     font.xtickslab = c(14, "plain", " black"), 
                     font.ytickslab = c(14, "plain", " black"), 
                     font.legend = c(16, "plain", "black"),
                     palette = c("#9933CC","#F9A825","#F3898B"),
                     break.time.by = 100, ylim = c(0.9, 1), xlim = c(0, 950))
ggsurv$plot <- ggsurv$plot + theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(face = "bold"))
ggsurv
dev.off()

# -------------------- #
# Figure 4-c
# -------------------- #

png("figure4_c.png", width = 760, height = 800, type = "cairo")
ggsurv <- ggsurvplot(fit4c, data = surv2,
                     size = 1.5, risk.table = F, conf.int = FALSE, pval = FALSE, censor = F, 
                     ggtheme = theme_bw(),
                     title = "T2D and Age (N=101K)",
                     xlab = "Time in Days",
                     ylab = "Survival Probability", 
                     legend.title = "", 
                     legend.labs = c("Controls with <60 years","T2D patients with <60 years","Controls with >=60 years","T2D patients with >=60 years"),
                     legend = c(0.3, 0.15), 
                     font.title = c(24, "bold", "black"),
                     font.x = c(21, "plain", "black"),
                     font.y = c(21, "plain", "black"),
                     font.xtickslab = c(14, "plain", " black"), 
                     font.ytickslab = c(14, "plain", " black"),
                     font.legend = c(16, "plain", "black"), 
                     palette = c("#9933CC", "#42A5F5", "#F9A825","#F3898B"), 
                     break.time.by = 100, ylim = c(0.8, 1), xlim = c(0, 950))

ggsurv$plot <- ggsurv$plot + theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(face = "bold"))
ggsurv
dev.off()

# -------------------- #
# Figure 4-d
# -------------------- #

png("figure4_d.png", width = 760, height = 800, type = "cairo")
ggsurv <- ggsurvplot(fit4d, data = surv2,
                     size = 1.5, risk.table =F, conf.int = FALSE, pval = FALSE, censor = F,
                     ggtheme = theme_bw(),
                     title = "PRS Group(T2D) and Age (N=101K)",
                     xlab = "Time in Days",
                     ylab = "Survival Probability",
                     legend.title = "",
                     legend.labs = c("Low PRS group with <60 years","Medium PRS group <60 years","High PRS group <60 years","Low PRS group with >=60 years","Medium PRS group >=60 years","High PRS group >=60 years"),
                     legend = c(0.3, 0.15),
                     font.title = c(24, "bold", "black"),
                     font.x = c(21, "plain", "black"),
                     font.y = c(21, "plain", "black"),
                     font.xtickslab = c(14, "plain", " black"),
                     font.ytickslab = c(14, "plain", " black"),
                     font.legend = c(16, "plain", "black"),
                     palette = c("#9933CC", "#283593", "#42A5F5","#99CC33", "#F9A825","#F3898B"),
                     break.time.by = 100, ylim = c(0.8, 1), xlim = c(0, 950))
ggsurv$plot <- ggsurv$plot + theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(face = "bold"))
ggsurv
dev.off()
