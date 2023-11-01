

plot_activity_group <- function(class_to_plot, remove_no_data, legend=FALSE)
{
  if (remove_no_data == TRUE)
  {
    avd_with_groups %>%
      filter(cons=="No data" | is.na(class) | is.na(cons))  %>%
      select(id) %>%
      distinct()->
      bad_ids
  } else
  {
    avd_with_groups %>%
      filter(is.na(class))  %>%
      select(id) %>%
      distinct()->
      bad_ids
  }
  
  avd_with_groups %>%
    filter(!id %in% bad_ids$id) %>%
    filter(class == class_to_plot) ->
    plot_data
  
  # only does something if  has been leeft in 
  plot_data$cons[is.na(plot_data$cons)] =  "No data"
  
  value = c(1)
  
  pa_colours = viridis(8)
  plot_colours = c(
    'Every day' = pa_colours[7], 
    '4-5 days a week' = pa_colours[6],
    '2-3 days a week' = pa_colours[5],
    'Once a week' = pa_colours[4],
    '2-3 times a month' = pa_colours[3],
    'Less often'=  pa_colours[2],
    'No exercise' = pa_colours[1],
    'No data'= 'grey')
  
  # need to first join 
  plot <- ggplot(plot_data, aes(fill=cons, x= age, y = value)) + 
    geom_bar(position = "fill", stat="identity") +
    scale_fill_manual(values = plot_colours) +
    guides(fill = legend)
  
  avd_with_groups %>%
    filter(!is.na(class) | is.na(cons)) %>%
    select(class) ->
    pure_class
  
  
  
  percent <- pure_class %>%
    filter(class == class_to_plot) %>%
    nrow()*100 / nrow(pure_class)
  
  plot <- plot + ggtitle(paste("Class ", class_to_plot, ", ", round(percent, 2), "%"))
  
  return(plot)
  
}