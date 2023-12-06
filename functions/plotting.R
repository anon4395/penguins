#Create exploratory violin plot
plot_violin_figure <- function(culmen_length_data){
  culmen_length_data %>%
  ggplot(aes(fill=sex, x = species, y = culmen_length_mm))+
  #Specify violin plot, and its aesthetic properties for optimal visualisation.
  geom_violin(position= position_dodge(width=0.7), alpha=0.5, (aes(fill = sex)), width = 1, trim=TRUE, color="gray36", linewidth=0.4) +
  scale_fill_viridis(discrete=T) +
  #Add individual data points
  geom_point(alpha=0.7, color="gray36", position=position_jitterdodge(jitter.width=0.1, dodge.width=0.7))+
  #Add point to show the mean, and bar showing 1x standard deviation (maybe remove this)
  stat_summary(
    fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", alpha=0.8, color = "black", position = position_dodge(width=0.7)
  )+
  #Add labels, choose theme, centre plot title and change font size
  labs(title = "Sex differences in culmen lengths for three penguin species",
       x = "Species",
       y = "Culmen length (mm)") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 11))
}

#Function to save violin plot as a png
save_violin_plot_png <- function(culmen_length_data, 
                                  filename, size, res, scaling){
  agg_png(filename, width   =  size, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  culmen_length_violin_plot <- plot_violin_figure(culmen_length_data)
  print(culmen_length_violin_plot)
  dev.off()
}

#Function to save violin plot as vector
save_violin_plot_svg <- function(culmen_length_data, 
                                  filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  culmen_length_violin_plot <- plot_violin_figure(culmen_length_data)
  print(culmen_length_violin_plot)
  dev.off()
}

#Create explanatory scatter plot with means, error bars and relationship lines
plot_scatter_figure <- function(culmen_length_data) {
  culmen_length_data %>%
    ggplot()+
  #Plot the data points, making them slightly transparent and jittered so they can be seen more clearly.
  geom_point(aes(x = sex, y = culmen_length_mm, colour = species, group = species), 
    position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.2),alpha = 0.3, size=1.5) +
  #Add error bars to show the standard error of means. This is from the penguins_stats dataset we created.
  #Draw line between the means of each group
  stat_summary(aes(x = sex, group = species, y = predicted_culmen_length_mm, colour = species),
    fun = mean, geom ="line"
  )+
  #Add a point to mark the mean for each group
  stat_summary(aes(x = sex, group = species, y = predicted_culmen_length_mm, colour = species),
               fun = mean, geom = "point", size = 3, shape = 18
  )+
  #Add titles and adjust aesthetics
  labs(title = "Sex differences in culmen lengths for three penguin species", x = "Sex", y = "Culmen Length (mm)", color = "Species") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size=10), text = element_text(size = 10), legend.background = element_rect(color = "gray36"), legend.key.size = unit(0.7, "cm"), legend.text = element_text(size = 10, face = "italic"))+
  scale_colour_manual(values = c("#440154FF","#287D8EFF","#55C667FF"))
}

#Function to save this plot as a vector image
save_scatter_plot_svg <- function(culmen_length_data, 
                                  filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  scatter_plot <- plot_scatter_figure(culmen_length_data)
  print(scatter_plot)
  dev.off()
}

#Function to save this plot as png
save_scatter_plot_png <- function(culmen_length_data, 
                                  filename, size, res, scaling){
  agg_png(filename, width   =  size, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  scatter_plot <- plot_scatter_figure(culmen_length_data)
  print(scatter_plot)
  dev.off()
}
