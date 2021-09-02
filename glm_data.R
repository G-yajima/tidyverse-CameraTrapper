library(tidyverse)
library(ggplot2)
# 30地点のカメラデータを考える
# シカの撮影枚数は,vegetationに有意な正の影響を受け
# カメラを置いた場所が畑より森林の方が有意に正の影響を受ける
# 他の種に関しては超てきとー

# パラメータ
N = 30
alpha <- 0.37
beta1 <- 0.75
beta2 <- 0.32

# 共変量の生成
set.seed(123)
x1 <- rnorm(N, mean = 0, sd = 1) + 2
set.seed(1234)
x2_org <- rbinom(N, 1, 0.6)
x2 <- ifelse(x2_org == 1, "forest", "crop")

# 撮影枚数
log_lambda <- exp(alpha + beta1*x1 + beta2*x2_org)
round(log_lambda)
set.seed(12)
y <- rpois(N, log_lambda)
y[1] <- 0
# 整形後データ
aft_data <- tibble(y = y, x1 = x1, x2 = x2)
# 一応確認
res_glm <- glm(y ~ x1+x2, family = poisson(link = "log"), data = aft_data)
summary(res_glm)
new_X <- seq(0, 4.2, 0.01)
pred <- exp(res_glm$coefficients[1] + res_glm$coefficients[2]*new_X + res_glm$coefficients[3])
# 作図
ggplot()+
  theme_bw()+
  geom_point(aes(x = x1, y = y, colour = x2)) +
  geom_line(aes(x = new_X, y = pred), linetype = "dotted")

##################################################
# カメラデータを作る
camera_data0 <- aft_data %>% 
  mutate(station = row_number()) %>% 
  mutate_at(vars(station), ~ifelse(nchar(.) == 1,
                                   paste("BSN0", ., sep = ""),
                                   paste("BSN", ., sep = ""))
            )

raw_data0 <- list()
for (i in 1:nrow(camera_data0)) {
  raw_data0[[i]] <- rep(camera_data0$station[i], 
                       each = camera_data0$y[i])
}

raw_data <- unlist(raw_data0)
raw_date <- as.POSIXct("2020-12-01", tz="Japan") + 
  sample(seq(31*24*60*60)-1, size=length(raw_data), replace=TRUE)

# てきとーな種のデータ
dummy_data <- tibble(station = sample(camera_data0$station, 30, replace = TRUE),
                     Species = sample(c("イノシシ", "ウサギ", "キョン"), 30, replace = TRUE),
                     Datetime= as.POSIXct("2020-12-01", tz="Japan") + 
                       sample(seq(31*24*60*60)-1, size=30, replace=TRUE))

camera_data <- tibble(station = raw_data,
                      Datetime = raw_date,
                      Species = "シカ") %>% 
  bind_rows(dummy_data)



covariate_data <- camera_data0 %>% 
  select(station, x1, x2) %>% 
  rename(vegetation = x1, habitat = x2)

write.csv(camera_data, "Camera_data.csv")
write.csv(covariate_data, "Covariate_data.csv")
########################################################################

