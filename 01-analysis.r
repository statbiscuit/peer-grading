load("student_and_tutor-grades.RData")
require(tidyverse)

################
#### TUTOR #####
#### COR #######
################

## betweem tutor correlation
tutor_grades %>%
    select(total, grader) %>%
    group_by(grader) %>%
    mutate(row = row_number()) %>%
    pivot_wider(., values_from = total, names_from  = grader) %>%
    select (-row) %>%
    cor()

## within tutor SD 

tutor_grades %>%
    select(total, grader) %>%
    group_by(grader) %>%
    summarise(sd = sd(total))


## tutor average

tutor_grades %>%
    select(total, file_name) %>%
    group_by(file_name) %>%
    summarise(average = mean(total)) %>%
    summarise(Average = num(mean(average), sigfig = 5), SD = num(sd(average), sigfig = 5))

tutor_grades %>%
    group_by(file_name) %>%
    summarise(across(criteria_1:criteria_5, mean)) %>%
    summarise(across(criteria_1:criteria_5,  \(x) mean(x, na.rm = TRUE))) %>%
    as.numeric() %>%
    round(., 3)

tutor_grades %>%
    group_by(file_name) %>%
    summarise(across(criteria_1:criteria_5, mean)) %>%
    summarise(across(criteria_1:criteria_5,  \(x) sd(x, na.rm = TRUE))) %>%
    as.numeric() %>%
    round(., 3)
    

################
#### STUDENT ###
#### COR #######
################

peer_grades %>%
    select(score, file_name) %>%
    filter(., !is.nan(score)) %>%
    group_by(file_name) %>%
    summarise(average = mean(score, na.omit = TRUE)) %>%
    summarise(Average = num(mean(average), sigfig = 5), SD = num(sd(average), sigfig = 5))

peer_grades %>%
    filter(., !is.nan(score)) %>%
    group_by(file_name) %>%
    summarise(across(item1:item5, mean)) %>%
    summarise(across(item1:item5,  \(x) mean(x, na.rm = TRUE))) %>%
    as.numeric() %>%
    round(., 3)
peer_grades %>%
    filter(., !is.nan(score)) %>%
    group_by(file_name) %>%
    summarise(across(item1:item5, mean)) %>%
    summarise(across(item1:item5, \(x) sd(x, na.rm = TRUE) )) %>%
    as.numeric() %>%
    round(., 3)
    

################################
## correlation with tutor grades
################################
## overall
t <- tutor_grades %>%
    group_by(file_name) %>%
    summarise(average = mean(total))
p <- peer_grades %>%
    group_by(file_name) %>%
    summarise(average = mean(score, na.omit = TRUE))
tp <- left_join(t, p, by = "file_name", suffix = c("_graders", "_peers"))
cor.test(x = tp$average_graders, y = tp$average_peers)
## by item
ta <- tutor_grades %>%
    group_by(file_name) %>%
    summarise(across(criteria_1:criteria_5, mean))
pa <- peer_grades %>%
    group_by(file_name) %>%
    summarise(across(item1:item5, mean, na.rm = TRUE))
tpa <- left_join(ta, pa, by = "file_name")
## item-by-item correlation
cor.test(x = tpa$criteria_1, y = tpa$item1)
cor.test(x = tpa$criteria_2, y = tpa$item2)
cor.test(x = tpa$criteria_3, y = tpa$item3)
cor.test(x = tpa$criteria_4, y = tpa$item4)
cor.test(x = tpa$criteria_5, y = tpa$item5)

###################
#### MODERATED ####
###################

final_grades %>%
    summarise(Average = num(mean(given_grade), sigfig = 5), SD = num(sd(given_grade), sigfig = 5))

fp <-  left_join(final_grades, p, by = "file_name") %>%
    select(file_name, given_grade, average)
## absolute difference with numerical tolerance of 0.01 (actually 0.00333). (CANVAS rounding)
table(abs(fp$given_grade - fp$average) <= 0.01)
## Moderated grades
fp[abs(fp$given_grade - fp$average) > 0.01, ] %>%
    summarise(given_grade - average)
##################
####### PLOTS ####
##################

## Overall tutor plot

data <- tutor_grades %>%
    group_by(file_name) %>%
    mutate(between_grader_sd = sd(total)) %>%
    arrange(., between_grader_sd) %>%
    mutate(file_name = factor(file_name, unique(file_name)))


data %>%  ggplot(., aes(x = fct_inorder(file_name), y = total/14)) +
    geom_line(aes(x = fct_inorder(file_name), y = total/14, group = file_name)) +
    geom_point(size = 2, alpha = 0.5) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ylab("") + xlab("Individual submissions ordered by between grader standard deviation") +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    geom_vline(xintercept = max(which(data$between_grader_sd == 0))/3) +
    geom_vline(xintercept = max(which(data$between_grader_sd == 1))/3) +
    annotate("text", x = max(which(data$between_grader_sd == 0))/6, y = 0.1, label = "SD = 0") +
    annotate("text", x = max(which(data$between_grader_sd == 0))/6, y = 0.05,
             label = paste("(n =", length(which(data$between_grader_sd == 0))/3,")")) +
    annotate("text", x = max(which(data$between_grader_sd == 1))/4.5, y = 0.1, label = "0 < SD ≤ 1") +
    annotate("text", x = max(which(data$between_grader_sd == 1))/4.5, y = 0.05,
             label = paste("(n =", length(which(data$between_grader_sd <= 1 & data$between_grader_sd > 0))/3,")")) +
    annotate("text", x = max(which(data$between_grader_sd == 1))/3 + 40, y = 0.1, label = "1 < SD ≤ 5.3") +
    annotate("text", x = max(which(data$between_grader_sd == 1))/3 + 40, y = 0.05,
             label = paste("(n =", length(which(data$between_grader_sd <= 5.3 & data$between_grader_sd > 1))/3,")"))
ggsave("plots/overall_tutor_grades_sd.png", width = 14, height = 5)


## Overall peer plot

data <- peer_grades %>%
    filter(!is.nan(score)) %>%
    group_by(file_name) %>%
    mutate(peer_sd = sd(score,na.rm = TRUE )) %>%
    arrange(., peer_sd) %>%
    mutate(file_name = factor(file_name, unique(file_name)))


data %>%  ggplot(., aes(x = fct_inorder(file_name), y = score/14)) +
    geom_line(aes(x = fct_inorder(file_name), y = score/14, group = file_name)) +
    geom_point(size = 2, alpha = 0.5) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ylab("") +
    xlab("Individual submissions ordered by within submission standard deviation") +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    geom_vline(xintercept = max(which(data$peer_sd == 0))/3) +
    geom_vline(xintercept = max(which(data$peer_sd == 1))/3) +
    geom_vline(xintercept = max(which(data$peer_sd == 2))/3) +
    annotate("text", x = max(which(data$peer_sd == 0))/6, y = 0.1, label = "SD = 0") +
    annotate("text", x = max(which(data$peer_sd == 0))/6, y = 0.05,
             label = paste("(n =", length(unique(data$file_name[which(data$peer_sd == 0)])),")")) +
    annotate("text", x = max(which(data$peer_sd == 1))/5.5, y = 0.1, label = "0 < SD ≤ 1") +
    annotate("text", x = max(which(data$peer_sd == 1))/5.5, y = 0.05,
             label = paste("(n =", length(unique(data$file_name[which(data$peer_sd > 0 & data$peer_sd <= 1)])),")")) +
    annotate("text", x = max(which(data$peer_sd == 2))/4 + 10, y = 0.1, label = "1 < SD ≤ 2") +
    annotate("text", x = max(which(data$peer_sd == 2))/4 + 10, y = 0.05,
             label = paste("(n =", length(unique(data$file_name[which(data$peer_sd > 1 & data$peer_sd <= 2)])),")")) +
    annotate("text", x = max(which(data$peer_sd == 2))/4 + 70, y = 0.1, label = "2 < SD < 5.9") +
    annotate("text", x = max(which(data$peer_sd == 2))/4 + 70, y = 0.05,
             label = paste("(n =", length(unique(data$file_name[which(data$peer_sd > 2 )])),")"))

ggsave("plots/overall_peer_grades_sd.png", width = 14, height = 5)

## Pair plots

rbind(data.frame(x = tp$average_graders/14, y =  tp$average_peers/14, cor = "Overall"),
      data.frame(x = tpa$criteria_1/2, y =  tpa$item1/2, cor = "Item 1"),
      data.frame(x = tpa$criteria_2/3, y =  tpa$item2/3, cor = "Item 2"),
      data.frame(x = tpa$criteria_3/2, y =  tpa$item3/2, cor = "Item 3"),
      data.frame(x = tpa$criteria_4/3, y =  tpa$item4/3, cor = "Item 4"),
      data.frame(x = tpa$criteria_5/4, y =  tpa$item5/4, cor = "Item 5") ) %>%
    ggplot(. , aes(y, x)) +  geom_smooth(method = "lm", col = "grey") +
    geom_point(alpha = 0.5, size = 2) + facet_wrap(~cor) +
    theme_classic() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    ylab("Tutor Grades") + xlab("Peer Grades")

ggsave("plots/compare_grades.png", width = 10, height = 5) 


#######################
##### COR CLUST #######
#######################

require(corrplot)
png("plots/compare_grades_cor.png")
tpa %>%
    select(where(is.numeric)) %>%
    rename(.,  c("TI1" = criteria_1,  "TI2" = criteria_2,  "TI3" = criteria_3, "TI4" = criteria_4,
                 "TI5" =  criteria_5,
                 "PI1" = item1,  "PI2" = item2,
                 "PI3" = item3,"PI4" =  item4 , "PI5" = item5)) %>%
    cor(.) %>%
    corrplot(.,  order = 'hclust', addrect = 4, diag = FALSE, addCoef.col = 'black', tl.pos = 'd',
             cl.pos = 'n', col = COL2('PiYG'))
dev.off()

#################
##### PCA #######
#################

pca <- tpa %>%
    select(where(is.numeric)) %>%
    rename(.,  c("TI1" = criteria_1,  "TI2" = criteria_2,  "TI3" = criteria_3,
                 "TI4" = criteria_4,"TI5" =  criteria_5,
                 "PI1" = item1,  "PI2" = item2,  "PI3" = item3,"PI4" =  item4 ,
                 "PI5" = item5)) %>%
    scale() %>%
    prcomp()
pca |> summary()
p1 <- factoextra::fviz_screeplot(pca)
p2 <- factoextra::fviz_pca_biplot(pca, geom = "point") +
    geom_point(alpha = 0.2)
patchwork::wrap_plots(p1, p2)
ggsave("plots/pca_plots.png", width = 10, height = 5) 
## PC1 of no interest really (all are correlated)
## PC2 difference between items 1 & 3 and 2 & 4; also, the tutor and peer item 5's
factoextra::fviz_pca_biplot(pca, c(2, 3), geom = "point") +
    geom_point (alpha = 0.2)
## Biplot PC1 & PC2
factoextra::fviz_pca_var(pca,repel = TRUE) + ggtitle("")
ggsave("plots/biplot.png", width = 5, height = 5) 
## PC3 splits items 2 (code structure) and 4 (plot components)
pca$rotation %>%
    as.data.frame() %>%
    mutate(variables = rownames(.)) %>%
    gather(PC, loading, PC1:PC3) %>%
    ggplot(aes(abs(loading), variables, fill = loading > 0)) +
    geom_col() +
    facet_wrap(~PC, scales = "free_y") +
    labs(x = "Absolute value of loading",y = NULL, fill = "Direction") +
    theme_classic() + scale_fill_manual(labels =  c("Negative", "Positive"),
                                        values = c("#8E0152", "#276419"))
ggsave("plots/pca_loadings.png", width = 12, height = 4) 
