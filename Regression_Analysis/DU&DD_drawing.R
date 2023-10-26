library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(ggpmisc)

df_localized <- read_excel("./localized_DU&DD.xls", sheet = "Sheet1")
#*****选取localized_DU中的Localized列和SR_DU_freq列进行绘图*****
localized_DU <- select(df_localized, 2,3)
localized_DU <- na.omit(localized_DU)
DU_lm_model <- lm(localized_DU$Localized ~ localized_DU$SR_DU_freq)
DU_p_value <- summary(DU_lm_model)$coefficients[2, 4]
DU_r_square <- summary(DU_lm_model)$r.squared
p <- ggplot(localized_DU, aes(x = SR_DU_freq, y = Localized)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "SR Extent Across 19 Cancer Types",
       y = "5-Year Relative Survival") +
  geom_label(aes(x =  0.34, y = 0.7,
                 label = paste("R² = ", round(DU_r_square,3), sep = "")), 
             hjust = 1, vjust = 1, label.size = NA, size = 5.5) +
  geom_label(aes(x = 0.322, y = 0.534,
                 label = paste("P = ", round(DU_p_value,3), sep = "")), 
             hjust = 1, vjust = 0, label.size = NA, size = 5.5)+
  theme_minimal() +
  theme(plot.margin = unit(c(0.7,0.7,0.7,0.7), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black",size = 12),
        axis.title.x = element_text(size = 16, margin = margin(t = 5)),
        axis.title.y = element_text(size = 16, margin = margin(r = 5)),
        legend.position = "none",
        panel.spacing = unit(1, "lines"))
ggsave("Localized_DU.png",plot = p, device = "png", path = "./",
       units="px", width=1610, height=1066.66, dpi = 300)
#*****选取localized_DD中的Localized列和SR_DD_freq列进行绘图*****
localized_DD <- select(df_localized, 2,4)
localized_DD <- na.omit(localized_DD)
DD_lm_model <- lm(localized_DD$Localized ~ localized_DD$SR_DD_freq)
DD_p_value <- summary(DD_lm_model)$coefficients[2, 4]
DD_r_square <- summary(DD_lm_model)$r.squared
q <- ggplot(localized_DD, aes(x = SR_DD_freq, y = Localized)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "SR Extent Across 19 Cancer Types",
       y = "5-Year Relative Survival") +
  geom_label(aes(x =  0.31, y = 0.7,
                 label = paste("R² = ", round(DD_r_square,3), sep = "")), 
             hjust = 1, vjust = 1, label.size = NA, size = 5.5) +
  geom_label(aes(x = 0.2656, y = 0.534,
                 label = paste("P = ", round(DD_p_value,3), sep = "")), 
             hjust = 1, vjust = 0, label.size = NA, size = 5.5)+
  theme_minimal() +
  theme(plot.margin = unit(c(0.7,0.7,0.7,0.7), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black",size = 12),
        axis.title.x = element_text(size = 16, margin = margin(t = 5)),
        axis.title.y = element_text(size = 16, margin = margin(r = 5)),
        legend.position = "none",
        panel.spacing = unit(1, "lines"))
ggsave("Localized_DD.png",plot = q, device = "png", path = "./",
       units="px", width=1610, height=1066.66, dpi = 300)


df_regional <- read_excel("./regional_DU&DD.xls", sheet = "Sheet1")
#*****选取regional_DU中的Regional列和SR_DU_freq列进行绘图*****
regional_DU <- select(df_regional, 2,3)
regional_DU <- na.omit(regional_DU)
DU_lm_model <- lm(regional_DU$Regional ~ regional_DU$SR_DU_freq)
DU_p_value <- summary(DU_lm_model)$coefficients[2, 4]
DU_r_square <- summary(DU_lm_model)$r.squared
p <- ggplot(regional_DU, aes(x = SR_DU_freq, y = Regional)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "SR Extent Across 19 Cancer Types",
       y = "5-Year Relative Survival") +
  geom_label(aes(x = 0.34, y = 0.56,
                 label = paste("R² = ", round(DU_r_square,3), sep = "")), 
             hjust = 1, vjust = 1, label.size = NA, size = 5.5) +
  geom_label(aes(x = 0.323, y = 0.34,
                 label = paste("P = ", round(DU_p_value,3), sep = "")), 
             hjust = 1, vjust = 0, label.size = NA, size = 5.5)+
  theme_minimal() +
  theme(plot.margin = unit(c(0.7,0.7,0.7,0.7), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black",size = 12),
        axis.title.x = element_text(size = 16, margin = margin(t = 5)),
        axis.title.y = element_text(size = 16, margin = margin(r = 5)),
        legend.position = "none",
        panel.spacing = unit(1, "lines"))
ggsave("Regional_DU.png",plot = p, device = "png", path = "./",
       units="px", width=1610, height=1066.66, dpi = 300)
#*****选取regional_DD中的Regional列和SR_DD_freq列进行绘图*****
regional_DD <- select(df_regional, 2,4)
regional_DD <- na.omit(regional_DD)
DD_lm_model <- lm(regional_DD$Regional ~ regional_DD$SR_DD_freq)
DD_p_value <- summary(DD_lm_model)$coefficients[2, 4]
DD_r_square <- summary(DD_lm_model)$r.squared
q <- ggplot(regional_DD, aes(x = SR_DD_freq, y = Regional)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "SR Extent Across 19 Cancer Types",
       y = "5-Year Relative Survival") +
  geom_label(aes(x = 0.32, y = 0.56,
                 label = paste("R² = ", round(DD_r_square,3), sep = "")), 
             hjust = 1, vjust = 1, label.size = NA, size = 5.5) +
  geom_label(aes(x = 0.303, y = 0.34,
                 label = paste("P = ", round(DD_p_value,3), sep = "")), 
             hjust = 1, vjust = 0, label.size = NA, size = 5.5)+
  theme_minimal() +
  theme(plot.margin = unit(c(0.7,0.7,0.7,0.7), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black",size = 12),
        axis.title.x = element_text(size = 16, margin = margin(t = 5)),
        axis.title.y = element_text(size = 16, margin = margin(r = 5)),
        legend.position = "none",
        panel.spacing = unit(1, "lines"))
ggsave("Regional_DD.png",plot = q, device = "png", path = "./",
       units="px", width=1610, height=1066.66, dpi = 300)

df_distant <- read_excel("./distant_DU&DD.xls", sheet = "Sheet1")
#*****选取distant_DU中的Distant列和SR_DU_freq列进行绘图*****
distant_DU <- select(df_distant, 2,3)
distant_DU <- na.omit(distant_DU)
DU_lm_model <- lm(distant_DU$Distant ~ distant_DU$SR_DU_freq)
DU_p_value <- summary(DU_lm_model)$coefficients[2, 4]
DU_r_square <- summary(DU_lm_model)$r.squared
p <- ggplot(distant_DU, aes(x = SR_DU_freq, y = Distant)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "SR Extent Across 19 Cancer Types",
       y = "5-Year Relative Survival") +
  geom_label(aes(x = max(SR_DU_freq), y = max(Distant),
                 label = paste("R² = ", round(DU_r_square,3), sep = "")), 
             hjust = 1, vjust = 1, label.size = NA, size = 5.5) +
  geom_label(aes(x = 0.886, y = 0.55,
                 label = paste("P = ", round(DU_p_value,3), sep = "")), 
             hjust = 1, vjust = 0, label.size = NA, size = 5.5)+
  theme_minimal() +
  theme(plot.margin = unit(c(0.7,0.7,0.7,0.7), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black",size = 12),
        axis.title.x = element_text(size = 16, margin = margin(t = 5)),
        axis.title.y = element_text(size = 16, margin = margin(r = 5)),
        legend.position = "none",
        panel.spacing = unit(1, "lines"))
ggsave("Distant_DU.png",plot = p, device = "png", path = "./",
       units="px", width=1610, height=1066.66, dpi = 300)
#*****选取distant_DD中的Distant列和SR_DD_freq列进行绘图*****
distant_DD <- select(df_distant,2,4)
distant_DD <- na.omit(distant_DD)
DD_lm_model <- lm(distant_DD$Distant ~ distant_DD$SR_DD_freq)
DD_p_value <- summary(DD_lm_model)$coefficients[2, 4]
DD_r_square <- summary(DD_lm_model)$r.squared
q <- ggplot(distant_DD, aes(x = SR_DD_freq, y = Distant)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "SR Extent Across 19 Cancer Types",
       y = "5-Year Relative Survival") +
  geom_label(aes(x = max(SR_DD_freq), y = max(Distant),
                 label = paste("R² = ", round(DD_r_square,3), sep = "")), 
             hjust = 1, vjust = 1, label.size = NA, size = 5.5) +
  geom_label(aes(x = 0.842, y = 0.55,
                 label = paste("P = ", round(DD_p_value,3), sep = "")), 
             hjust = 1, vjust = 0, label.size = NA, size = 5.5)+
  theme_minimal() +
  theme(plot.margin = unit(c(0.7,0.7,0.7,0.7), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black",size = 12),
        axis.title.x = element_text(size = 16, margin = margin(t = 5)),
        axis.title.y = element_text(size = 16, margin = margin(r = 5)),
        legend.position = "none",
        panel.spacing = unit(1, "lines"))
ggsave("Distant_DD.png",plot = q, device = "png", path = "./",
       units="px", width=1610, height=1066.66, dpi = 300)