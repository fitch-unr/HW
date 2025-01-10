require(ggplot2)
require(sas7bdat)
require(gridExtra)
require(grid)
require(scales)
require(withr)
require(tidyverse)
require (cowplot)

setwd("D://DarrowGroup/NHAPPS/state/Results/Strat_Results")


load('MetaAnalysis/Landcov_Meta8PT_comb.Rdata')
combined_PT_lc <- ggplot(new, aes(x = OR, y = def1)) + 
  geom_vline(xintercept = 1, linewidth = 0.05) + 
  geom_point(size = 3, position = position_dodge2(width = 0.4), aes(color = def, shape = shp)) + 
  geom_errorbarh(aes(xmax = UCI, xmin = LCI, color = def, shape = shp), 
                 linewidth = 0.5, height = 0.4, position = position_dodge2(width = 0.2)) +
  scale_x_continuous(breaks = c(0.95, 1.00, 1.05, 1.10), labels = c('0.95', '1.00', '1.05', '1.10')) + 
  coord_cartesian(xlim = c(0.95, 1.10)) +
  scale_y_discrete(limits = rev(c('2+ consecutive days', '3+ consecutive days', '4 consecutive days', 'per \u00b0C'))) +
  scale_color_manual(values = c('#225ea8', '#41b6c4', '#a1dab4'), name = 'Heatwave Definition') + 
  scale_shape_manual(values = c(17, 15, 19), limits = rev(c('>66%', '25-66%', '<25%')), name = "Med+High Intensity") +
  theme_minimal() + 
  theme(
    axis.title.y = element_blank(), 
    axis.text.x = element_text(size = 10), 
    axis.title.x = element_blank(),
    axis.title = element_text(size = 10, color = 'black'), 
    legend.text = element_text(size = 10, color = 'black'), 
    legend.title = element_text(size = 10, color = 'black'), 
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), # Centered title
  ) +
  ggtitle("Impervious Land Cover: Preterm Birth") +  # Add the centered title
  guides(color = "none")  # Remove the Heatwave Definition legend

combined_PT_lc <- ggdraw(combined_PT_lc) +
  draw_text("a", x = 0.02, y = 0.98, hjust = 0, vjust = 1, fontface = "bold", size = 14)

load('MetaAnalysis/Landcov_Meta8ET_comb.Rdata')
combined_ET_lc <- ggplot(new, aes(x = OR, y = def1)) + 
  geom_vline(xintercept = 1, linewidth = 0.05) + 
  geom_point(size = 3, position = position_dodge2(width = 0.4), aes(color = def, shape = shp)) + 
  geom_errorbarh(aes(xmax = UCI, xmin = LCI, color = def, shape = shp), 
                 linewidth = 0.5, height = 0.4, position = position_dodge2(width = 0.2)) +
  scale_x_continuous(breaks = c(0.95, 1.00, 1.05, 1.10), labels = c('0.95', '1.00', '1.05', '1.10')) + 
  coord_cartesian(xlim = c(0.95, 1.10)) +
  scale_y_discrete(limits = rev(c('2+ consecutive days', '3+ consecutive days', '4 consecutive days', 'per \u00b0C'))) +
  scale_color_manual(values = c('#225ea8', '#41b6c4', '#a1dab4'), name = 'Heatwave Definition') + 
  scale_shape_manual(values = c(17, 15, 19), limits = rev(c('>66%', '25-66%', '<25%')), name = "Med+High Intensity") +
  theme_minimal() + 
  theme(
    axis.title.y = element_blank(), 
    axis.text.x = element_text(size = 10), 
    axis.title.x = element_blank(),
    axis.title = element_text(size = 10, color = 'black'), 
    legend.text = element_text(size = 10, color = 'black'), 
    legend.title = element_text(size = 10, color = 'black'), 
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), 
  ) +
  ggtitle("Impervious Land Cover: Early-Term Birth") + 
  guides(color = "none")  

combined_ET_lc <- ggdraw(combined_ET_lc) +
  draw_text("b", x = 0.02, y = 0.98, hjust = 0, vjust = 1, fontface = "bold", size = 14)

load("MetaAnalysis/SDI_Meta8PT_comb.Rdata") 
combined_PT_sdi <- ggplot(new, aes(x = OR, y = def1)) + 
  geom_vline(xintercept = 1, linewidth = 0.05) + 
  geom_point(size = 3, position = position_dodge2(width = 0.4), aes(color = def, shape = shp)) + 
  geom_errorbarh(aes(xmax = UCI, xmin = LCI, color = def, shape = shp), 
                 linewidth = 0.5, height = 0.4, position = position_dodge2(width = 0.2)) +
  scale_x_continuous(breaks = c(0.95, 1.00, 1.05, 1.10), labels = c('0.95', '1.00', '1.05', '1.10')) + 
  coord_cartesian(xlim = c(0.95, 1.10)) +
  scale_y_discrete(limits = rev(c('2+ consecutive days', '3+ consecutive days', '4 consecutive days', 'per \u00b0C'))) +
  scale_color_manual(values = c('#225ea8', '#41b6c4', '#a1dab4'), name = 'Heatwave Definition') + 
  scale_shape_manual(values = c(17, 15), limits = rev(c('High', 'Low')), name = "Social Deprivation") +
  theme_minimal() + 
  theme(
    axis.title.y = element_blank(), 
    axis.text.x = element_text(size = 10), 
    axis.title.x = element_blank(),
    axis.title = element_text(size = 10, color = 'black'), 
    legend.text = element_text(size = 10, color = 'black'), 
    legend.title = element_text(size = 10, color = 'black'), 
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), 
  ) +
  ggtitle("Social Deprivation Index: Preterm Birth") +  
  guides(color = "none")  

combined_PT_sdi <- ggdraw(combined_PT_sdi) +
  draw_text("c", x = 0.02, y = 0.98, hjust = 0, vjust = 1, fontface = "bold", size = 14)

load("MetaAnalysis/SDI_Meta8ET_comb.Rdata")
combined_ET_sdi <- ggplot(new, aes(x = OR, y = def1)) + 
  geom_vline(xintercept = 1, linewidth = 0.05) + 
  geom_point(size = 3, position = position_dodge2(width = 0.4), aes(color = def, shape = shp)) + 
  geom_errorbarh(aes(xmax = UCI, xmin = LCI, color = def, shape = shp), 
                 linewidth = 0.5, height = 0.4, position = position_dodge2(width = 0.2)) +
  scale_x_continuous(breaks = c(0.95, 1.00, 1.05, 1.10), labels = c('0.95', '1.00', '1.05', '1.10')) + 
  coord_cartesian(xlim = c(0.95, 1.10)) +
  scale_y_discrete(limits = rev(c('2+ consecutive days', '3+ consecutive days', '4 consecutive days', 'per \u00b0C'))) +
  scale_color_manual(values = c('#225ea8', '#41b6c4', '#a1dab4'), name = 'Heatwave Definition') + 
  scale_shape_manual(values = c(17, 15), limits = rev(c('High', 'Low')), name = "Social Deprivation") +
  theme_minimal() + 
  theme(
    axis.title.y = element_blank(), 
    axis.text.x = element_text(size = 10), 
    axis.title.x = element_blank(),
    axis.title = element_text(size = 10, color = 'black'), 
    legend.text = element_text(size = 10, color = 'black'), 
    legend.title = element_text(size = 10, color = 'black'), 
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), # Centered title
  ) +
  ggtitle("Social Deprivation Index: Early-Term Birth") + 
  guides(color = "none")  

combined_ET_sdi <- ggdraw(combined_ET_sdi) +
  draw_text("d", x = 0.02, y = 0.98, hjust = 0, vjust = 1, fontface = "bold", size = 14)

# Combine the 4 plots and save
combined_plot <- grid.arrange(
  combined_PT_lc, combined_ET_lc, combined_PT_sdi, combined_ET_sdi, 
  nrow = 2, ncol = 2, 
  layout_matrix = rbind(c(1, 3), c(2, 4))
)

ggsave("PLOT_lc_sdi_FINAL.png", combined_plot, width = 12, height = 8, dpi = 300)

#Manually modified to remove duplicate legends 

