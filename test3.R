# 加载相关的R包
library(readxl)
library(raster)
library(rgdal)
library(spdep)
library(INLA)
library(sp)

# 读取数据
data <- read_excel("test2.xlsx")
# 读取解释变量
Dem <- raster("Dem.tif")
MaxLST <- raster("MaxLST.tif")
MeanNDVI <- raster("MeanNDVI.tif")

# 构建模型
data$Dem_value <- extract(Dem, data[, c('X', 'Y')])
data$MaxLST_value <- extract(MaxLST, data[, c('X', 'Y')])
data$MeanNDVI_value <- extract(MeanNDVI, data[, c('X', 'Y')])

# 将数据离散化
data_grid <- raster::disaggregate(Dem, fact=2, method='bilinear') # 这里使用了Dem的分辨率来决定离散化的程度
data$X_new <- round(coordinates(data)[,'X'] / res(data_grid)[1]) * res(data_grid)[1] # 调整X坐标到最近的栅格中心
data$Y_new <- round(coordinates(data)[,'Y'] / res(data_grid)[2]) * res(data_grid)[2] # 调整Y坐标到最近的栅格中心
coordinates(data) <- ~X_new+Y_new

# 创建邻居列表，它是描述每个栅格和其邻居的关系的列表
neighbors <- cell2nb(nrow(data_grid), ncol(data_grid), type = "queen") # 使用queen方法来确定邻居关系
listw <- nb2listw(neighbors) # 将邻居关系转换为一个权重列表
mat <- listw2mat(listw) # 将权重列表转换为一个权重矩阵

# 创建一个索引，将每个观察值映射到对应的栅格
index <- which(!is.na(data_grid))
names(index) <- 1:length(index)



formula <- as ~ f(X, Y, model = "besag", param = c(1, 0.1)) + Dem_value + MaxLST_value + MeanNDVI_value

model <- inla(formula, data = data)

# 预测
study_area <- readOGR("MYS.shp")
prediction <- inla.predict(model, newdata = study_area)

# 将预测结果写入TIFF文件
writeRaster(prediction, filename = "risk_map.tif", format = 'GTiff')

