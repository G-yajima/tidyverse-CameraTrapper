# はじめにパッケージを読む !

# インストールしてない人は要インストール
# install.packages("tidyverse")
# install.packages("ggplot2")

library(tidyverse)
library(ggplot2)

# ************* パスを指定しないと動きません ************* #
# データの読み込み
# setwd() getwd()は非推奨
# 文字化けする
Camera_data0 <- read_csv("Camera_data.csv")

# 文字化け回避
Camera_data0 <- read_csv("C:/RData/Camera_data.csv", 
                         locale = locale(encoding = "cp932"))

# メモ

# 説明編数データ
covariate_data0 <- read_csv("C:/RData/Covariate_data.csv")


# ********************************************************* #
# tidyverse !!

# shaping data
analysis_data <- Camera_data0 %>% 
  filter(Species == "シカ") %>% 
  group_by(station) %>% 
  summarise(y = n()) %>% 
  right_join(., covariate_data0, by = "station") %>% 
  mutate_at(vars(y), ~ifelse(is.na(.), 0, .)) %>% 
  select(-X1)
######################################################################
# tidyverseを使わない場合
# シカに絞る
deer_data <- subset(as.data.frame(Camera_data0), Species == "シカ")
# 各地点の撮影枚数を計算
station_vec <- unique(deer_data$station)
Nstation <- length(station_vec)
y <- rep(NA, Nstation)
for (i in 1:Nstation) {
  sub_data <- subset(deer_data, station == station_vec[i])
  y[[i]] <- nrow(sub_data)
}
# カメラデータ完成
cam_data <- data.frame(station = station_vec, y = y)
head(cam_data)
# 説明変数データ追加
merge_data <- merge(covariate_data0, cam_data, all.x = TRUE,by.y = "station")
# NAを0に
merge_data[is.na(merge_data)] <- 0 
# X1列の削除
merge_data <- merge_data[, colnames(merge_data) != "X1"]
merge_data <- merge_data[,c(1, 4, 2,3)]
######################################################################
# 資料説明のため分割
# XXXX <- data %>% としてないため,
# 処理後のデータがConsoleに表示されるだけ
# 1. 
Camera_data0 %>% 
  filter(Species == "シカ") 

# 2
Camera_data0 %>% 
  filter(Species == "シカ") %>% 
  group_by(station)

# 3 
Camera_data0 %>% 
  filter(Species == "シカ") %>% 
  group_by(station) %>% 
  summarise(y = n()) 

# 4
Camera_data0 %>% 
  filter(Species == "シカ") %>% 
  group_by(station) %>% 
  summarise(y = n()) %>% 
  right_join(., covariate_data0, by = "station")

# 5
Camera_data0 %>% 
  filter(Species == "シカ") %>% 
  group_by(station) %>% 
  summarise(y = n()) %>% 
  right_join(., covariate_data0, by = "station") %>% 
  mutate_at(vars(y), ~ifelse(is.na(.), 0, .))

# 6
Camera_data0 %>% 
  filter(Species == "シカ") %>% 
  group_by(station) %>% 
  summarise(y = n()) %>% 
  right_join(., covariate_data0, by = "station") %>% 
  mutate_at(vars(y), ~ifelse(is.na(.), 0, .)) %>% 
  select(-X1)
#######################################################
# write.csv(analysis_data, "analysis_data.csv") # csvに書き出す　read <---> write

# explanatory analysis

ggplot(data = analysis_data) +
  theme_bw() +
  geom_point(aes(x = vegetation, y = y, colour = habitat))

ggplot(data = analysis_data) +
  theme_bw() +
  geom_boxplot(aes(x = habitat, y = y))

# fiting glm
fit <- glm(y ~ habitat + vegetation, family = poisson(link = "log"), data = analysis_data)
summary(fit)

#####################################################

new_X <- seq(0, 4.2, 0.01)
pred_fst <- exp(fit$coefficients[1] + fit$coefficients[3]*new_X + fit$coefficients[2])
pred_crp <- exp(fit$coefficients[1] + fit$coefficients[3]*new_X)

# 作図
fit_plot <- ggplot()+
  theme_bw(base_size = 20)+
  geom_point(aes(x = vegetation, y = y, colour = habitat), size = 3, data = analysis_data) +
  geom_line(aes(x = new_X, y = pred_fst), linetype = "dotted",colour = "skyblue") +
  geom_line(aes(x = new_X, y = pred_crp), linetype = "dotted", colour = "red")

ggsave(filename = "fit_plot.png", plot = fit_plot, dpi = 300)
