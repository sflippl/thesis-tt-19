library(muStat)
library(tidyverse)
df_change <- feather::read_feather('data/df_change.feather')
df_normalization <- 
    df_change %>% 
    filter(norm_id == 0) %>% 
    group_by(neuron_id) %>% 
    summarise(normalization = mean(spikerate))
df_change <- 
    df_change %>% 
    left_join(df_normalization, by = 'neuron_id') %>% 
    mutate(normalized_spikerate = spikerate/normalization)
sim_stat <- c()
for (i in 1:1000) {
    print(i)
    tmp <- df_center
    tmp[['is_perturbed']] <- sample(tmp[['is_perturbed']])
    ps <- 
        tmp %>% 
        filter(!(neuron_id %in% no_perturbations)) %>% 
        group_by(neuron_id) %>% 
        summarize(p_value = prentice.test(normalized_spikerate, is_perturbed, paste(change, norm_id))$p.value)
    sim_stat <- c(sim_stat, mean(ps$p_value <= 0.05))
}
save(sim_stat, file = 'data/sim_stat.RData')
