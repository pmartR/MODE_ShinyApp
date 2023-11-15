library(tidyverse)

## Pull original counts data.frame
RNA <- pmartRdata::rnaseq_object 

## Add log counts per million alongside counts
temp_data <- RNA$e_data[,2:ncol(RNA$e_data)]
samp_sum <- apply(temp_data, 2, sum, na.rm = TRUE) + 1
div_sum <- sweep((temp_data + .5), 2, samp_sum, `/`)
lcpm <- log2(div_sum * 10^6)
lcpm$Transcript <- RNA$e_data$Transcript
lcpm <- lcpm %>% relocate(Transcript)

## Pivot longer 
pre_data <- left_join(
  RNA$e_data %>% 
    pivot_longer(2:ncol(RNA$e_data)) %>%
    rename(SampleName = name, Count = value),
  lcpm %>%
    pivot_longer(2:ncol(lcpm)) %>%
    rename(SampleName = name, LCPM = value),
  by = c("Transcript", "SampleName")
) %>%
  left_join(RNA$f_data[,c("SampleName", "Virus")], by = "SampleName") %>%
  left_join(RNA$e_meta, by = "Transcript")

############################
## TRELLI RNA-SEQ BOXPLOT ##
############################

pre_data %>%
  filter(Transcript == "ENSG00000141298") %>%
  ggplot(aes(x = Virus, y = LCPM, fill = Virus)) +
    geom_boxplot() +
    theme_bw() +
    ylab("Log Counts per Million") +
    ggtitle("ENSG00000141298") +
    theme(plot.title = element_text(hjust = 0.5))

##############################
## TRELLI RNA-SEQ HISTOGRAM ##
##############################

pre_data %>% 
  filter(Transcript == "ENSG00000141298") %>%
  ggplot(aes(x = LCPM)) +
    geom_histogram(fill = "steelblue", color = "black") +
    theme_bw() +
    ylab("Log Counts per Million") +
    ggtitle("ENSG00000141298") +
    theme(plot.title = element_text(hjust = 0.5))

############################
## TRELLI RNA-SEQ HEATMAP ##
############################

pre_data %>%
  filter(Gene == "RF00568") %>%
  ggplot(aes(x = Transcript, y = SampleName, fill = LCPM)) + 
    geom_tile() +
    theme_bw() + 
    xlab("Transcript") +
    ylab("Sample") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

################################
## TRELLI RNA-SEQ NONZERO BAR ##
################################

pre_data %>% 
  filter(Transcript == "ENSG00000233750") %>%
  arrange(Count) %>%
  group_by(Virus) %>%
  summarize(
    `Non-Zero Count` = sum(Count != 0) / length(Count),
    `Zero Count` = sum(Count == 0) / length(Count)
  ) %>% 
  pivot_longer(c(`Non-Zero Count`, `Zero Count`)) %>%
  rename(Proportion = value, Type = name) %>%
  mutate(Type = factor(Type, levels = c("Zero Count", "Non-Zero Count"))) %>%
  ggplot(aes(x = Virus, y = Proportion, fill = Type)) +
    geom_bar(position = "stack", stat = "identity") +
    theme_bw() +
    scale_fill_manual(values = c("black", "steelblue"))
  











