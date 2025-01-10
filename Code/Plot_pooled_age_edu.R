require(ggplot2)
require(sas7bdat)
require(gridExtra)
require(grid)
require(scales)
require(withr)
require(tidyverse)
require (cowplot)

setwd("D://DarrowGroup/NHAPPS/state/Results/Strat_Results")


load('MetaAnalysis/AGE_Meta8PT_comb.Rdata')
# Age plot with one legend and positioned correctly
combined_PT_age <- ggplot(new, aes(x = OR, y = def1)) + 
  geom_vline(xintercept = 1, linewidth = 0.05) + 
  geom_point(size = 3, position = position_dodge2(width = 0.4), aes(color = def, shape = shp)) + 
  geom_errorbarh(aes(xmax = UCI, xmin = LCI, color = def, shape = shp), 
                 linewidth = 0.5, height = 0.4, position = position_dodge2(width = 0.2)) +
  scale_x_continuous(breaks = c(0.95, 1.00, 1.05, 1.10), labels = c('0.95', '1.00', '1.05', '1.10')) + 
  coord_cartesian(xlim = c(0.95, 1.10)) +
  scale_y_discrete(limits = rev(c('2+ consecutive days', '3+ consecutive days', '4 consecutive days', 'per \u00b0C'))) +
  scale_color_manual(values = c('#225ea8', '#41b6c4', '#a1dab4'), name = 'Heatwave Definition') + 
  scale_shape_manual(values = c(17, 15, 19), limits = rev(c('35+', '25-34', '<25')), name = "Maternal Age") +
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
  ggtitle("Maternal Age: Preterm Birth") +  # Add the centered title
  guides(color = "none")  # Remove the Heatwave Definition legend

combined_PT_age <- ggdraw(combined_PT_age) +
  draw_text("a", x = 0.02, y = 0.98, hjust = 0, vjust = 1, fontface = "bold", size = 14)

load('MetaAnalysis/AGE_Meta8ET_comb.Rdata')
# Early-Term Age plot (same modifications)
combined_ET_age <- ggplot(new, aes(x = OR, y = def1)) + 
  geom_vline(xintercept = 1, linewidth = 0.05) + 
  geom_point(size = 3, position = position_dodge2(width = 0.4), aes(color = def, shape = shp)) + 
  geom_errorbarh(aes(xmax = UCI, xmin = LCI, color = def, shape = shp), 
                 linewidth = 0.5, height = 0.4, position = position_dodge2(width = 0.2)) +
  scale_x_continuous(breaks = c(0.95, 1.00, 1.05, 1.10), labels = c('0.95', '1.00', '1.05', '1.10')) + 
  coord_cartesian(xlim = c(0.95, 1.10)) +
  scale_y_discrete(limits = rev(c('2+ consecutive days', '3+ consecutive days', '4 consecutive days', 'per \u00b0C'))) +
  scale_color_manual(values = c('#225ea8', '#41b6c4', '#a1dab4'), name = 'Heatwave Definition') + 
  scale_shape_manual(values = c(17, 15, 19), limits = rev(c('35+', '25-34', '<25')), name = "Maternal Age") +
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
  ggtitle("Maternal Age: Early-Term Birth") +  # Add the centered title
  guides(color = "none")  # Remove the Heatwave Definition legend

combined_ET_age <- ggdraw(combined_ET_age) +
  draw_text("b", x = 0.02, y = 0.98, hjust = 0, vjust = 1, fontface = "bold", size = 14)

load('MetaAnalysis/EDU_Meta7PT_comb.Rdata')
# Education plot (same modifications as for Age)
combined_PT_ed <- ggplot(new, aes(x = OR, y = def1)) + 
  geom_vline(xintercept = 1, linewidth = 0.05) + 
  geom_point(size = 3, position = position_dodge2(width = 0.4), aes(color = def, shape = shp)) + 
  geom_errorbarh(aes(xmax = UCI, xmin = LCI, color = def, shape = shp), 
                 linewidth = 0.5, height = 0.4, position = position_dodge2(width = 0.2)) +
  scale_x_continuous(breaks = c(0.95, 1.00, 1.05, 1.10), labels = c('0.95', '1.00', '1.05', '1.10')) + 
  coord_cartesian(xlim = c(0.95, 1.10)) +
  scale_y_discrete(limits = rev(c('2+ consecutive days', '3+ consecutive days', '4 consecutive days', 'per \u00b0C'))) +
  scale_color_manual(values = c('#225ea8', '#41b6c4', '#a1dab4'), name = 'Heatwave Definition') + 
  scale_shape_manual(values = c(17, 15, 19), limits = rev(c('>HS', 'HS/GED', '<HS')), name = "Maternal Education") +
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
  ggtitle("Maternal Education: Preterm Birth") +  # Add the centered title
  guides(color = "none")  # Remove the Heatwave Definition legend

combined_PT_ed <- ggdraw(combined_PT_ed) +
  draw_text("c", x = 0.02, y = 0.98, hjust = 0, vjust = 1, fontface = "bold", size = 14)

load('MetaAnalysis/EDU_Meta7ET_comb.Rdata')
# Early-Term Education plot (same modifications)
combined_ET_ed <- ggplot(new, aes(x = OR, y = def1)) + 
  geom_vline(xintercept = 1, linewidth = 0.05) + 
  geom_point(size = 3, position = position_dodge2(width = 0.4), aes(color = def, shape = shp)) + 
  geom_errorbarh(aes(xmax = UCI, xmin = LCI, color = def, shape = shp), 
                 linewidth = 0.5, height = 0.4, position = position_dodge2(width = 0.2)) +
  scale_x_continuous(breaks = c(0.95, 1.00, 1.05, 1.10), labels = c('0.95', '1.00', '1.05', '1.10')) + 
  coord_cartesian(xlim = c(0.95, 1.10)) +
  scale_y_discrete(limits = rev(c('2+ consecutive days', '3+ consecutive days', '4 consecutive days', 'per \u00b0C'))) +
  scale_color_manual(values = c('#225ea8', '#41b6c4', '#a1dab4'), name = 'Heatwave Definition') + 
  scale_shape_manual(values = c(17, 15, 19), limits = rev(c('>HS', 'HS/GED', '<HS')), name = "Maternal Education") +
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
  ggtitle("Maternal Education: Early-Term Birth") +  # Add the centered title
  guides(color = "none")  # Remove the Heatwave Definition legend

combined_ET_ed <- ggdraw(combined_ET_ed) +
  draw_text("d", x = 0.02, y = 0.98, hjust = 0, vjust = 1, fontface = "bold", size = 14)

# Combine the plots with legends using grid.arrange
combined_plot <- grid.arrange(
  combined_PT_age, combined_ET_age, combined_PT_ed, combined_ET_ed, 
  nrow = 2, ncol = 2, 
  layout_matrix = rbind(c(1, 3), c(2, 4))
)

# Save the plot to a file
ggsave("PLOT_age_edu_FINAL.png", combined_plot, width = 12, height = 8, dpi = 300)

#Manually modified to remove duplicate legends 

