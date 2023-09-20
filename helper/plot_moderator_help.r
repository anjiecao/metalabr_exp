plot_moderators <- function(all_mod_df){
  
  axis_font_size = 6
  title_font_size = 7
  ## behavioral measure
  
  # shorten label 
  
  all_mod_df <- all_mod_df %>% 
    mutate(dataset_short = case_when(
      dataset == "Cross-situational word learning" ~ "X-situation",
      dataset == "Infant directed speech preference" ~ "IDS pref",
      dataset ==  "Label advantage in concept learning" ~ "Label adv", 
      dataset ==  "Language discrimination and preference" ~ "Lang disc & pref", 
      dataset ==  "Mutual exclusivity" ~ "Mutual exclusivity", 
      dataset ==  "Natural speech preference"  ~ "Nat. speech pref", 
      dataset ==  "Sound symbolism"  ~ "Sound symbolism" , 
      dataset ==  "Syntactic bootstrapping"     ~ "Syntac. bootstrapping", 
      dataset ==  "Vowel discrimination (native)"  ~ "Vowel disc (native)" , 
      dataset ==  "Vowel discrimination (non-native)" ~ "Vowel disc (non-native)", 
      dataset ==  "Abstract rule learning" ~ "Abstract rule learning", 
      dataset ==  "Familiar word recognition" ~ "Fam word recog", 
      dataset ==  "Mispronunciation sensitivity" ~ "Mispronun. sensitivi.", 
      dataset ==  "Simple arithmetic competences" ~ "Arithmetic", 
      dataset ==  "Word Segmentation (combined)" ~ "Word seg", 
      dataset ==  "Categorization bias" ~ "Categorization bias", 
      dataset ==  "Prosocial agents" ~ "Prosocial agents", 
      dataset ==  "Statistical word segmentation" ~ "Stat word seg", 
      dataset ==  "Online word recognition" ~ "Online word recog", 
      dataset ==  "Switch task" ~ "Switch task", 
      dataset ==  "Symbolic play" ~ "Symbolic play", 
      dataset ==  "Statistical sound category learning (habituation)" ~ "Stat sound category",
      dataset ==  "Gaze following (combined)" ~ "Gaze following"
    ))
  
   bm_p <- 
    all_mod_df %>% 
    filter(grepl("bmeasure", model_type)) %>% 
    filter(grepl("behavioral", term)) %>% 
    mutate(group = case_when(
      grepl("manual", term ) ~ "Manual",
      grepl("head_turn", term) ~ "HPP", 
      grepl("facial_expression", term) ~ "Facial Expression", 
      grepl("sucking", term) ~ "Sucking"
    )) %>% 
    ggplot(aes(x = reorder(dataset_short,estimate), y = estimate, color = group)) + 
    geom_pointrange(aes(y = estimate, ymin = lb, ymax = ub), 
                    position = position_dodge(width = .4)) +
    scale_color_manual(values = c("#E69F00", "#56B4E9", "red", "blue")) + 
    geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+ 
    #coord_flip() +
    ylim(-2.5, 3)+ 
    xlab("") + 
    scale_y_continuous(
      "Estimated Coefficient (effect sizes)", 
      sec.axis = sec_axis(~ . * 1, "Looking")
    ) + 
    
    labs(title = "Behavioral Measure \n (Baseline: looking)") + 
    theme_few() +
    theme(
      axis.title.y.left = element_text(size=axis_font_size),
      axis.title.y.right = element_text(size=axis_font_size, color = "gray", angle = 90, vjust = 0 , hjust=0),
      axis.ticks.y.right =  element_blank(),
      axis.text.y.right  =  element_blank(),
      legend.position = "top",
      axis.text=element_text(size=axis_font_size,angle = 90, vjust = 0, hjust=1),

            legend.title = element_blank(),
      legend.margin = margin(0, 0, 0, 0),
      legend.spacing.x = unit(0, "mm"),
      legend.spacing.y = unit(0, "mm"),
      legend.background = element_rect(fill = NA, color = NA), 
      legend.text=element_text(size=6),
      plot.title = element_text(hjust = 0.5, size = title_font_size)
    )  

  ## exposure phase
  
  ep_mod_p <- 
    all_mod_df %>% 
    filter(grepl("ep", model_type)) %>% 
    filter(grepl("exposure_phase", term)) %>% 
    mutate(group = case_when(
      grepl("habituation", term ) ~ "Habituation",
      grepl("conditioning", term) ~ "Conditioning"
    )) %>% 
  
    ggplot(aes(x = reorder(dataset_short,estimate), y = estimate, color = group)) + 
    geom_pointrange(aes(y = estimate, ymin = lb, ymax = ub), 
                    position = position_dodge(width = .4)) + 
    scale_color_manual(values = c( "#009E73", "#F0E442", "red", "blue")) + 
    geom_hline(yintercept = 0, color = "gray",linetype = "dashed") + 
    #coord_flip() +
    ylim(-2.5, 3)+ 
    xlab("") + 
    scale_y_continuous(
        "", 
        sec.axis = sec_axis(~ . * 1, "Familiarization")
      ) + 
    labs(title = "Stimuli Exposure Method \n (Baseline: Familiarization)") + 
    theme_few() +
    theme(
      legend.position = "top",
      axis.text=element_text(size=axis_font_size,angle = 90, vjust = 0.5, hjust=1),
      axis.title.y.right = element_text(size=axis_font_size, color = "gray", angle = 90, vjust = 0.5 , hjust=0.3),
      axis.ticks.y.right =  element_blank(),
      axis.text.y.right  =  element_blank(),
      #legend.position = c(0.7, 0.2),
      legend.title = element_blank(),
      legend.margin = margin(0, 0, 0, 0),
      legend.spacing.x = unit(0, "mm"),
      legend.spacing.y = unit(0, "mm"),
      legend.background = element_rect(fill = NA, color = NA), 
      legend.text=element_text(size=6),
      plot.title = element_text(hjust = 0.5, size = title_font_size)
    )  

  ## ns_stimuli 
  

  natural_mod_p <- 
    all_mod_df %>% 
    filter(grepl("sn", model_type)) %>% 
    filter(grepl("stimuli_natural", term)) %>% 
    ggplot(aes(x = reorder(dataset_short,estimate), y = estimate)) + 
    geom_pointrange(aes(y = estimate, ymin = lb, ymax = ub), 
                    position = position_dodge(width = .4)) +
    scale_color_manual(values = c("#D55E00")) + 
    geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+ 
    #coord_flip() +
    ylim(-2.5, 3)+ 
    xlab("") + 
    scale_y_continuous(
      "", 
      sec.axis = sec_axis(~ . * 1, " Artificial stimulus")
    ) + 
    labs(title = "Stimuli Naturalness \n (Baseline: Artificial stimulus)") + 
    theme_few() +
    theme(
      axis.text=element_text(size=axis_font_size,angle = 90, vjust = 0.5, hjust = 1),
      axis.title.y.right = element_text(size=axis_font_size, color = "gray", angle = 90, vjust = 0.5 , hjust=0.3),
      axis.ticks.y.right =  element_blank(),
      axis.text.y.right  =  element_blank(),
      legend.position = "none",
      legend.title = element_blank(),
      
      legend.background = element_rect(fill = NA, color = NA), 
      legend.text=element_text(size=6),
      plot.title = element_text(hjust = 0.5, size = title_font_size)
    )  



  (bm_p | ep_mod_p | natural_mod_p)
  
  
}



plot_major_author <- function(all_mod_df){
  axis_font_size = 6
  
  all_mod_df <- all_mod_df %>% 
    mutate(dataset_short = case_when(
      dataset == "Cross-situational word learning" ~ "X-situation",
      dataset == "Infant directed speech preference" ~ "IDS pref",
      dataset ==  "Label advantage in concept learning" ~ "Label adv", 
      dataset ==  "Language discrimination and preference" ~ "Lang disc & pref", 
      dataset ==  "Mutual exclusivity" ~ "Mutual exclusivity", 
      dataset ==  "Natural speech preference"  ~ "Natural speech preference", 
      dataset ==  "Sound symbolism"  ~ "Sound symbolism" , 
      dataset ==  "Syntactic bootstrapping"     ~ "Syntactic bootstrapping", 
      dataset ==  "Vowel discrimination (native)"  ~ "Vowel disc (native)" , 
      dataset ==  "Vowel discrimination (non-native)" ~ "Vowel disc (non-native)", 
      dataset ==  "Abstract rule learning" ~ "Abstract rule learning", 
      dataset ==  "Familiar word recognition" ~ "Fam word recog", 
      dataset ==  "Mispronunciation sensitivity" ~ "Mispronunciation sensitivity", 
      dataset ==  "Simple arithmetic competences" ~ "Arithmetic", 
      dataset ==  "Word Segmentation (combined)" ~ "Word seg", 
      dataset ==  "Categorization bias" ~ "Categorization bias", 
      dataset ==  "Prosocial agents" ~ "Prosocial agents", 
      dataset ==  "Statistical word segmentation" ~ "Stat word seg", 
      dataset ==  "Online word recognition" ~ "Online word recog", 
      dataset ==  "Switch task" ~ "Switch task", 
      dataset ==  "Symbolic play" ~ "Symbolic play", 
      dataset ==  "Statistical sound category learning (habituation)" ~ "Stat sound category",
      dataset ==  "Gaze following (combined)" ~ "Gaze following"
    ))
  
  
    all_mod_df %>% 
    filter(grepl("Author", type)) %>% 
    distinct(dataset, dataset_short, estimate, p.value, lb, ub) %>% 
    mutate(es_type = case_when(p.value < 0.05 ~ "Significant", TRUE ~ "Non-significant")) %>%   
    ggplot(aes(x = reorder(dataset_short,estimate), y = estimate)) + 
    geom_hline(yintercept = 0, linetype = "dashed")+ 
    geom_pointrange(aes(y = estimate, ymin = lb, ymax = ub), 
                    position = position_dodge(width = .4)) +
    geom_point(aes(y = estimate, color = es_type, color = es_type)) + 
    scale_color_manual(values = c("gray", "black")) +
    coord_flip()+
    guides(fill = guide_legend(override.aes = list(linetype = 0)),
             color = guide_legend(override.aes = list(linetype = 0)),  keyheight = 2)+ 
    xlab("") + 
    ylab("") + 
  
    labs(title = "Major author \n (Baseline: Non-Major author)") + 
      
    theme_few() +
      
    theme(
      legend.position = c(.8, .3),
      legend.title=element_blank(),
      legend.margin = margin(0, 0, 0, 0),
      legend.key.size = unit(0.2, "cm"),
      
      legend.spacing.x = unit(0, "mm"),
      legend.spacing.y = unit(0, "mm"),
      legend.background = element_rect(fill = "NA", color = "NA"), 
      legend.text=element_text(size=4),
      axis.text=element_text(size=axis_font_size),
      plot.title = element_text(hjust = 0.5, size = 6)
    )  
  
  
}

