#'
#'  Results data manipulation and graphics generation associated with writeup
#'  found here:
#'  
#'  
#'

### Libraries ------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(grid)
library(gridExtra)

### Setup ----------------------------------------------------------------------
setwd('C:/Users/billy.vreeland/openai_test_results/10_6_17/')

### Functions ------------------------------------------------------------------
ProcessResultsFolder <- function(folder_name) {
  
  # Read and format meta data
  meta_data <- read.csv(paste0(folder_name, '/test_meta_data.csv'),
                        header = FALSE)
  names(meta_data) <- c('env', 'max_episodes', 'target_update_freq', 
                        'replay_size', 'batch_size', 'learning_rate')
  
  # Read and format results
  results <- read.csv(paste0(folder_name, '/test_results.csv'), 
                      header = FALSE, skip = 1)
  names(results) <- c('episode', 'epsilon', 'avg_explore_rewards', 
                      'avg_test_rewards')
  
  # Merge
  meta_data$folder_name <- folder_name
  results$folder_name <- folder_name
  results <- merge(meta_data, results, by = 'folder_name')
  
  return(results)
}


### Main -----------------------------------------------------------------------

# Intake resaults data and run processing function, which merges meta data with 
# test results, done locally to generate processed_results_list.rds

# results_folders <- list.files('./test_results/',
#                               include.dirs = TRUE, full.names = TRUE)
# results <- results_folders %>%
#   lapply(ProcessResultsFolder)
# 
# saveRDS(results, 'processed_results_list.rds')
results <- readRDS('processed_results_list.rds')
head(results[[1]])

# Get last entry from each set of results and generate summary
# Environment is considered solved if episode reward > 195
results_ends <- results %>%
  lapply(function(x) tail(x, n = 1)) %>%
  bind_rows() %>%
  mutate(success = if_else(avg_test_rewards > 195.0, 1, 0))

ends_summary <- results_ends %>%
  group_by(target_update_freq, batch_size, learning_rate) %>%
  summarise(count = n(),
            successes = sum(success)) %>%
  mutate(success_rate = successes / count) %>%
  arrange(desc(success_rate, count)) %>%
  filter(target_update_freq %in% c(1, 2, 4, 8, 16, 32, 64)) %>%
  dplyr::rename(`Batch Size`=batch_size, `Success Rate`=success_rate)

# Generate heatmaps of test results as a function of learning rate and target 
# model weight update frequency
success_heatmap<- ends_summary %>%
  filter(`Batch Size` > 4,
         `Batch Size` < 512) %>%
  ggplot(aes(x = target_update_freq, y = learning_rate)) +
  geom_tile(aes(fill = `Success Rate`), color = 'white', size = 0.1) +
  facet_wrap( ~ `Batch Size`, labeller = label_both) + 
  scale_fill_gradientn(colours = brewer.pal(n = 11, name = "RdYlGn"), labels=percent) +
  scale_y_log10(breaks=c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1),
                labels=c('-5', '-4', '-3', '-2', '-1')) +
  scale_x_continuous(trans=log2_trans()) +
  theme(text = element_text(size=12),
        legend.position = 'bottom') +
  labs(title='Success Rates for Permutations of Batch Size, Learning Rate, and Target Update Frequency',
       subtitle='Keras DDQN with Experience Replay applied to CartPole environment, 10 iterations for each parameter combination',
       x = 'Target Update Frequency (Episodes)',
       y =expression(log[10]~'(Learning Rate)'))
success_heatmap

# Plot number of episodes required to solve environment as a function of batch size
# for the best performing learning rate and update frequency as indicated on the
# heatmap
episodes_needed <- results_ends %>%
  filter(learning_rate == 1e-2,
         target_update_freq == 2) %>%
  group_by(batch_size) %>%
  summarise(count = n(), 
            success_rate = sum(success) / 10,
            median_episode_finish = median(episode)) %>%
  ggplot(aes(x=batch_size, y=median_episode_finish)) +
  scale_x_continuous(trans=log2_trans()) + 
  ylim(c(0, 125)) +
  geom_point(size=2) +
  geom_line() +
  labs(title = 'Training Episodes Required to Solve Environment',
       subtitle = 'DDQN with Learning Rate = 1e-2 and Target Update Frequency = 2',
       x = expression(paste('Batch Size ', italic('(note log scale)'))),
       y = expression(paste('Median # of Episodes ', italic('(10 iterations / point)'))))
episodes_needed

# Plot results trajectories during training for batch size with fewest needed
# episodes as indicated in previous plot

# Get the last points of each run to mark on graph
layer_data <- results %>%
  bind_rows() %>%
  filter(target_update_freq == 2,
         batch_size %in% c(1024),
         learning_rate %in% c(1e-2, 1e-3),
         avg_test_rewards > 195.0)

# Colorblind palette from:
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
# Can sample to generate the palette and re-run the plot code below to change
# which paths get matched with which colors
cbPalette <- sample(c("#56B4E9", "#E69F00", "#999999","#000000", "#009E73", "#F0E442", "#D55E00","#0072B2", "#CC79A7", "#FFFFFF"))
print(cbPalette)
# [1] "#F0E442" "#56B4E9" "#E69F00" "#999999" "#009E73" "#000000" "#CC79A7" "#FFFFFF" "#0072B2" "#D55E00"

trajectories <- results %>%
  bind_rows() %>%
  filter(target_update_freq == 2,
         batch_size %in% c(1024),
         learning_rate %in% c(1e-2, 1e-3)) %>%
  ggplot(aes(x=episode, y=avg_test_rewards, colour=folder_name)) +
  geom_line(size=1.1) +
  geom_point(shape = 8, size = 3, stroke = 3, data = layer_data) +  # position='jitter'
  theme(legend.position = 'None') +
  scale_x_log10() + 
  scale_color_manual(values = cbPalette) +
  labs(title = 'Test Result Trajectory During 10 Training Iterations',
       subtitle = 'DDQN with Learning Rate = 1e-2, Target Update Frequency = 2, and Batch Size = 1024',
       x = expression(paste('Training Episode ', italic('(note log scale)'))),
       y = 'Average Rewards for 100 Test Episodes') + 
  theme(text = element_text(size=12))
trajectories







### Previous to delete ---------------------------------------------------------


p <- results_df %>%
  filter(learning_rate == 1e-4) %>%
  ggplot(aes(x=episode, y=avg_test_rewards)) +
  geom_line() + 
  geom_point() + 
  facet_grid(batch_size ~ target_update_freq)
p
  
  
  





# x <- results_df %>% filter(folder_name == './test_results/2017-10-09 17-02-19.434921--b95c6563-0c6e-4b7e-bfb0-fd733bcfee63')


i <- 12
head(results[[i]])
p <- results[[i]] %>%
  ggplot(aes(x=episode, y=avg_test_rewards)) +
  geom_point() +
  geom_line()
p






update_freq_summary <- results_ends %>%
  group_by(target_update_freq) %>% 
  summarise(count = n(),
            successes = sum(success)) %>%
  mutate(success_rate = successes / count, 
         metric = 'Update Frequency') %>%
  rename(value = target_update_freq)

batch_size_summary <- results_ends %>%
  group_by(batch_size) %>% 
  summarise(count = n(),
            successes = sum(success)) %>%
  mutate(success_rate = successes / count,
         metric = 'Batch Size') %>%
  rename(value = batch_size)

learning_rate_summary <- results_ends %>%
  group_by(learning_rate) %>% 
  summarise(count = n(),
            successes = sum(success)) %>%
  mutate(success_rate = successes / count,
         metric = 'Learning Rate') %>%
  rename(value=learning_rate)

agg_summaries <- bind_rows(update_freq_summary, batch_size_summary, learning_rate_summary)




freq_and_learning_summary <- results_ends %>%
  group_by(learning_rate, target_update_freq) %>%
  summarise(count = n(),
            successes = sum(success)) %>%
  mutate(success_rate = successes / count)

    
             
             
            

p_2 <- results %>%
  bind_rows() %>%
  filter(learning_rate == 1e-2,
         target_update_freq == 2) %>%
  ggplot(aes(x = episode, y = avg_test_rewards, colour = folder_name)) +
  geom_line() +
  geom_point() +
  facet_grid(batch_size ~ . , labeller = label_both) +
  theme(legend.position = 'None')
  # xlim(c(0, 300))
p_2


  
  
  ggplot(aes(x = episode)) + 
  geom_histogram() +
  scale_x_log10() + 
  facet_grid(batch_size ~ . , labeller = label_both) +
  theme(legend.position = 'None')
# xlim(c(0, 300))
p_3

y <- results_ends %>% 
  group_by(learning_rate, target_update_freq, batch_size) %>%
  summarise(count = n(),
            median_episode_finish = median(episode)) %>%
  arrange(median_episode_finish)


y <- head(y, n=10) %>%
  dplyr::select(`Learning Rate` = learning_rate, `Target Update Freq.` = target_update_freq,
                `Batch Size` = batch_size, `Median Episode Finish` = median_episode_finish) 
  
z <- grid.table(y)



ends_summary <- ends_summary %>%
  dplyr::select(target_update_freq, batch_size, learning_rate, success_rate)



pca_results <- prcomp(ends_summary, scale=TRUE)


biplot(pca_results)






