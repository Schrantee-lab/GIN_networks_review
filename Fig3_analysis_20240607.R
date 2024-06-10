
# PACKAGES 
library('readr')
library('ggplot2')
library('dplyr')
library('cowplot')
library('ggpie')
require(scales)
library(patchwork)

# SET WORKING DIRECTORY
setwd(' ')
MAIN_DIR <- getwd() 

# DATA 
dat <- as.data.frame(read_tsv('Data_extraction_table_20240606_articles.tsv')) 
dat <- dat[,c("sample_size", "network_size_nodes", "association", "modelling", "inclusion_strategy", "weighted", "deconfounding_level", "exclude")] # keep only columns that don't contain comments 
dat <- dat[dat$exclude == "no",] # keep only rows with papers that were not excluded 

# Ensure columns are numeric
dat$sample_size <- as.numeric(dat$sample_size)
dat$network_size_nodes <- as.numeric(dat$network_size_nodes)

# # Now the confounder categories
dat_conf <- as.data.frame(read_tsv('Data_confounder_categories_perc.tsv')) 


# CHECK
head(dat)
nrow(dat)
str(dat)

head(dat_conf)
nrow(dat_conf)
str(dat_conf)

## PREPARE RAINCLOUD PLOT FUNCTIONS

# Flat Violin Function
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolin <- ggproto("GeomFlatViolin", Geom,
                          setup_data = function(data, params) {
                            data$width <- data$width %||%
                              params$width %||% (resolution(data$x, FALSE) * 0.9)
                            data %>%
                              group_by(group) %>%
                              mutate(ymin = min(y),
                                     ymax = max(y),
                                     xmin = x,
                                     xmax = x + width / 2)
                          },
                          draw_group = function(data, panel_scales, coord) {
                            data <- transform(data, xminv = x,
                                              xmaxv = x + violinwidth * (xmax - x))
                            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                                             plyr::arrange(transform(data, x = xmaxv), -y))
                            newdata <- rbind(newdata, newdata[1,])
                            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
                          },
                          draw_key = draw_key_polygon,
                          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                                            alpha = NA, linetype = "solid"),
                          required_aes = c("x", "y")
)

## PREPARE DATA FOR PIE CHARTS 

# ASSOCIATION TYPE
association_sum <- ifelse((dat$association == "full correlation"), 'pairwise correlation', 
                          ifelse((dat$association == 'partial correlation'), 'partial correlation',
                                 ifelse((dat$association == 'correlation'), 'not specified',
                                        ifelse((dat$association == 'not specified'), "not specified",
                                               "other"))))

 
# EDGE INCLUSION STRATEGY 
inclusion_sum <- ifelse((dat$`inclusion_strategy` == "yes (thresholding)" | 
                           dat$`inclusion_strategy` == "yes (regularisation)" |
                           dat$`inclusion_strategy` == "yes (negative edges set to 0)"|
                           dat$`inclusion_strategy` == "yes (multiple)"|
                           dat$`inclusion_strategy` == "yes (cross validation)"|
                           dat$`inclusion_strategy` == "yes (minimum spanning tree)"|
                           dat$`inclusion_strategy` == "yes (normalised entropy and high amplitude co-fluctuations)"|
                           dat$`inclusion_strategy` == "yes (averages of multiple runs for a single participant)"|
                           dat$`inclusion_strategy` == "yes (correlation with behavioral outcomes)"|
                           dat$`inclusion_strategy` == "yes (edges involving nodes of interest)"),
                        'yes', 
                        ifelse((dat$`inclusion_strategy` == "both used and not used"), 
                               "multiple", 
                               ifelse((dat$`inclusion_strategy` == "no" |
                                         dat$`inclusion_strategy` == "not applicable"), 
                                      "no", "not specified")))

# EDGE WEIGHTS 
weight_sum <- ifelse((dat$weighted == "yes (absolutised)" | dat$weighted == "yes"), 'yes', 
                     ifelse((dat$weighted == 'both'), 'multiple','no'))

# MODELLING LEVEL
modelling_sum <- dat$modelling
modelling_sum <- ifelse(modelling_sum == 'multiple', 'individual', modelling_sum)

# DECONFOUNDING LEVEL
confounder_level_sum <- dat$deconfounding_level
confounder_level_sum <- ifelse((dat$deconfounding_level == "Before network estimation"), 'before network estimation', 
                          ifelse((dat$deconfounding_level == 'During network estimation'), 'during network estimation',
                                 ifelse((dat$deconfounding_level == 'After network estimation'), 'after network estimation',
                                        ifelse((dat$deconfounding_level == 'Multiple'), "multiple stages", NA))))


# DATA FRAME WITH ALL SUMMARIES 
all_pie_data <- data_frame(
  weight_sum,
  association_sum,
  modelling_sum,
  inclusion_sum,
  confounder_level_sum
)

# COLOR PALETTE
pie_cols <- c("#117733", # use for yes or most common option
              "#88CCEE", # use for multiple
              "#332288", # use for no or opposite of the most common option
              "#CC6677") # use for not specified  

# RAINCLOUD PLOTS 
# code adapted from https://wellcomeopenresearch.org/articles/4-63

# PANEL A
sample_size <- as.numeric(dat$`sample_size`) 

x <- 1:length(sample_size)
y <- sample_size
group <- 1

sample_dat <- data.frame(
  x,
  y,
  group
)

pA <- ggplot(sample_dat, aes(x=as.factor(group), y=y)) +
  geom_flat_violin(position = position_nudge(x = -.1, y = 0), adjust = 2, fill = pie_cols[1], color = NA) +
  geom_point(position = position_jitter(width = .15), aes(x = group - .3, y = y + .1), size = .75, color = pie_cols[1]) +
  ylab('Sample Size') +
  theme_cowplot() + guides(fill = FALSE) +
  ggtitle('A.', subtitle = "Sample size") +
  coord_cartesian(xlim = c(.75, 1.25)) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18),
        plot.margin = margin(.5, .5, .5, .5, "cm"), 
        axis.title = element_text(size = 16),
        axis.ticks = element_blank()) +
  geom_boxplot(aes(x = as.numeric(group), y = y), 
               position = position_nudge(x = -.1, y = 0),
               outlier.shape = 16, outlier.size = 1, alpha = .5, width = .1, colour = "black", outlier.color = "black") +
  scale_y_log10(labels = comma)

# PANEL B 
network_size <- na.omit(dat$`network_size_nodes`)

x <- 1:length(network_size)
y <- network_size
group <- 1

network_dat <- data.frame(
  x,
  y,
  group
)

pB <- ggplot(network_dat, aes(x=as.factor(group), y=y)) +
  geom_flat_violin(position = position_nudge(x = -.1, y = 0), adjust = 2, fill = pie_cols[2], color = NA) +
  geom_point(position = position_jitter(width = .15),
             
             aes(x = group - .3, y = y + .1), size = .75, color = pie_cols[2]) +
  ylab('Number of Nodes') +
  theme_cowplot() + guides(fill = FALSE) +
  ggtitle('B.', subtitle ='Network size') +
  coord_cartesian(xlim = c(.75, 1.25)) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18),
        plot.margin = margin(.5, .5, .5, .5, "cm"), 
        axis.title = element_text(size = 16),
        axis.ticks = element_blank()) +
  geom_boxplot(aes(x = as.numeric(group), y = y), 
               position = position_nudge(x = -.1, y = 0),
               outlier.shape = 16, outlier.size = 1, alpha = .5, width = .1, colour = 'black', outlier.color = pie_cols[2]) +
  scale_y_log10(labels = comma)

# PIE CHARTS 

# PANEL C
all_pie_data$association_sum <- factor(all_pie_data$association_sum, levels = c("pairwise correlation", "other", "partial correlation", "not specified"))

pC <- ggpie(
  data = all_pie_data, 
  group_key = "association_sum", 
  count_type = "full",
  label_info = "all", 
  label_type = "horizon", 
  fill_color = pie_cols,
  label_pos = 'in',
  label_size = 6,
  label_threshold = 100,
  border_color = NA,
  border_size = .5) +
  labs(fill = NULL) +
  ggtitle("C.", subtitle = "Association type") +
  theme(plot.title = element_text(size = 18, face = 'bold'),
        plot.subtitle = element_text(size = 18),
        plot.margin = margin(.5, .5, .5, .5, "cm"), 
        legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(ncol = 2))

# PANEL D
all_pie_data$inclusion_sum <- factor(all_pie_data$inclusion_sum, levels = c("yes", "multiple", "no", "not specified"))

pD <- ggpie(
  data = all_pie_data, 
  group_key = "inclusion_sum", 
  count_type = "full",
  label_info = "all", 
  label_type = "horizon", 
  fill_color = pie_cols,
  label_pos = 'in',
  label_size = 6,
  label_threshold = 100,
  border_color = NA,
  border_size = .5) +
  labs(fill = NULL) +
  ggtitle("D.", subtitle = "Edge inclusion strategy") +
  theme(plot.title = element_text(size = 18, face = 'bold'),
        plot.subtitle = element_text(size = 18),
        plot.margin = margin(.5, .5, .5, .5, "cm"), 
        legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(ncol = 2))

# PANEL E
all_pie_data$weight_sum <- factor(all_pie_data$weight_sum, levels = c("yes", "multiple", "no"))

pE <- ggpie(
  data = all_pie_data, 
  group_key = "weight_sum", 
  count_type = "full",
  label_info = "all", 
  label_type = "horizon", 
  fill_color = pie_cols[1:3],
  label_pos = 'in',
  label_size = 6,
  label_threshold = 100,
  border_color = NA,
  border_size = .5) +
  labs(fill = NULL) +
  ggtitle("E.", subtitle = "Edge weights") +
  theme(plot.title = element_text(size = 18, face = 'bold'),
        plot.subtitle = element_text(size = 18),
        plot.margin = margin(.5, .5, .5, .5, "cm"), 
        legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(ncol = 2))

# PANEL F
all_pie_data$modelling_sum <- factor(all_pie_data$modelling_sum, levels = c("individual", "multilevel", "aggregated", "not specified"))

pF <- ggpie(
  data = all_pie_data, 
  group_key = "modelling_sum", 
  count_type = "full",
  label_info = "all", 
  label_type = "horizon", 
  fill_color = pie_cols,
  label_pos = 'in',
  label_size = 6,
  label_threshold = 100,
  border_color = NA,
  border_size = .5) +
  labs(fill = NULL) +
  ggtitle("F.", subtitle = "Modelling") +
  theme(plot.title = element_text(size = 18, face = 'bold'),
        plot.subtitle = element_text(size = 18),
        plot.margin = margin(.5, .5, .5, .5, "cm"), 
        legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  guides(fill = guide_legend(ncol = 2))



#PANEL G1 - CONFOUNDING FACTORS
dat_conf$Fig_label <- paste0(dat_conf$Count, " (", dat_conf$Percentage, "%)")

pG1 <- ggplot(dat_conf, aes(x = reorder(Category, -Count), y = Count, fill = pie_cols[1])) +
  geom_bar(stat = "identity", fill = pie_cols[1]) +
  geom_text(aes(label = Fig_label), vjust = -0.3, size = 5, angle = 45, hjust = -0.1) +
  labs(
    title = "G.",
    subtitle = "Confounder categories", 
    y = "Counts"
  ) +
  theme_cowplot() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, color = "black"),  
    axis.text.y = element_text(size = 16), 
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),  
    legend.position = "none",
    plot.title = element_text(size = 18, face = 'bold'),
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(size = 16),
    plot.margin = margin(.5,.5,.5,.5, "cm"), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.ticks = element_blank()
  ) +
  ylim(c(0, 250))


# PANEL G2 - DECONFOUNDING ANALYSIS STAGE
all_pie_data$confounder_level_sum <- factor(all_pie_data$confounder_level_sum, levels = c("multiple stages", "before network estimation"))

pG2 <- ggpie(
  data = all_pie_data, 
  group_key = "confounder_level_sum", 
  count_type = "full",
  label_info = "all", 
  label_type = "horizon", 
  fill_color = pie_cols[1:2],
  label_pos = 'in',
  label_size = 6,
  label_threshold = 100,
  border_color = NA,
  border_size = .5) +
  labs(fill = NULL) +
  ggtitle("", subtitle = "Deconfounding analysis stage") +
  theme(plot.title = element_text(size = 18, face = 'bold'),
        plot.subtitle = element_text(size = 18),
        plot.margin = margin(.5, .5, .5, .5, "cm"), 
        legend.position = "bottom",
        legend.text = element_text(size = 14)
  ) +
  guides(fill = guide_legend(ncol = 2))

# EXPORT AS JPEG 
jpeg(filename = "multiplot_20240607.jpeg", res = 300, height = 12*505, width = 10*467) 
pA + pB +  pC + pD + pE + pF +  pG1 + pG2
  plot_layout(ncol = 2, nrow = 4) 
dev.off()


