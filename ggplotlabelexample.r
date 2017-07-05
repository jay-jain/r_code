# Demo for making labels of facets and x-axis in ggplot2
# QDR 4 July 2017

plotdat <- data.frame(species = c('shark','coelacanth','rabbit','monkey','gorilla','human'),
                      continent = factor(c(1,1,2,2,1,2)),
                      location = c('water','water','land','land','land','land'),
                      size = c(10,5,1,2,8,6))

library(cowplot) # A mod of ggplot2 that has prettier defaults.

# Create a simple scatterplot. 
# The aes() argument specifies that the species variable will map to the x axis, the size variable to the y axis, and the location variable to the color of the points (all within the "point" layer)
p <- ggplot(plotdat) +
  geom_point(aes(x = species, y = size, color = location))

p # Print the scatterplot with all default options

# Facet the plot by continent (automatically splits the data by the grouping factor and plots them separately)
p + facet_wrap(~ continent)


# The continent labels aren't informative so we will replace them with real names
p + facet_wrap(~ continent, labeller = labeller(continent = c('1'='Africa', '2'='Asia')))

# There are extraneous species in each panel of the facet, so we will drop unused ones
p + facet_wrap(~ continent, scales = 'free_x', labeller = labeller(continent = c('1'='Africa', '2'='Asia')))

# The species names are overlapping so let's turn them sideways by setting plot options with theme()
p + 
  facet_wrap(~ continent, scales = 'free_x', labeller = labeller(continent = c('1'='Africa', '2'='Asia'))) +
  theme(axis.text.x = element_text(angle = 45))

# Now the species names go up too high so let's set the justification so that they are below the axis line
p + 
  facet_wrap(~ continent, scales = 'free_x', labeller = labeller(continent = c('1'='Africa', '2'='Asia'))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# How about a border around each panel? Plus, the gray strip is ugly so let's get rid of it.
p + 
  facet_wrap(~ continent, scales = 'free_x', labeller = labeller(continent = c('1'='Africa', '2'='Asia'))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank()) +
  panel_border(colour = 'black')

# Save it to a file. (Uncomment final line to save plot)
p_final <- p + 
  facet_wrap(~ continent, scales = 'free_x', labeller = labeller(continent = c('1'='Africa', '2'='Asia'))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank()) +
  panel_border(colour = 'black')

# ggsave('~/fakeplot.png', p_final, height = 4, width = 6, dpi = 400)