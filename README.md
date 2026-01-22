# map
Hubei Province
# 加载必要的包
library(tidyverse)
library(sf)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer) # 用于生成颜色方案

# 读取采样点数据
hubei_data <- read.table("湖北采样信息.txt", header = FALSE, skip = 1, 
                         col.names = c("SampleID", "Value", "Longitude", "Latitude"))

# 转换为sf对象
hubei_sf <- st_as_sf(hubei_data, coords = c("Longitude", "Latitude"), crs = 4326)

# 获取湖北省边界数据
china <- ne_states(country = "china", returnclass = "sf")
hubei_province <- china %>% filter(name == "Hubei")

# 获取湖北省地级市边界并添加拼音名称
china_cities <- st_read("https://geo.datav.aliyun.com/areas_v3/bound/geojson?code=420000_full", quiet = TRUE) %>%
  mutate(name_pinyin = case_when(
    name == "武汉市" ~ "Wuhan",
    name == "黄石市" ~ "Huangshi",
    name == "十堰市" ~ "Shiyan",
    name == "宜昌市" ~ "Yichang",
    name == "襄阳市" ~ "Xiangyang",
    name == "鄂州市" ~ "Ezhou",
    name == "荆门市" ~ "Jingmen",
    name == "孝感市" ~ "Xiaogan",
    name == "荆州市" ~ "Jingzhou",
    name == "黄冈市" ~ "Huanggang",
    name == "咸宁市" ~ "Xianning",
    name == "随州市" ~ "Suizhou",
    name == "恩施土家族苗族自治州" ~ "Enshi",
    name == "仙桃市" ~ "Xiantao",
    name == "潜江市" ~ "Qianjiang",
    name == "天门市" ~ "Tianmen",
    name == "神农架林区" ~ "Shennongjia",
    TRUE ~ name
  ))

# 为每个城市分配唯一颜色
set.seed(123) # 确保颜色可重复
city_colors <- data.frame(
  name_pinyin = unique(china_cities$name_pinyin),
  color = brewer.pal(12, "Set3") %>% 
    rep(length.out = length(unique(china_cities$name_pinyin)))
)

china_cities <- china_cities %>% 
  left_join(city_colors, by = "name_pinyin")

# 获取长江数据并裁剪到湖北省范围内
rivers <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical', returnclass = "sf")
yangtze <- rivers %>% 
  filter(name_en == "Yangtze") %>%
  st_intersection(hubei_province)

# 绘制地图
ggplot() +
  # 绘制湖北省边界
  geom_sf(data = hubei_province, fill = NA, color = "black", size = 0.8) +
  # 绘制彩色城市分区
  geom_sf(data = china_cities, aes(fill = name_pinyin), alpha = 0.5) +
  # 添加城市边界线
  geom_sf(data = china_cities, fill = NA, color = "gray30", size = 0.3) +
  # 绘制长江
  geom_sf(data = yangtze, color = "blue", size = 1.2) +
  # 绘制采样点
  geom_sf(data = hubei_sf, color = "red", size = 2, shape = 16) +
  # 添加城市拼音标签
  geom_sf_text(data = china_cities, aes(label = name_pinyin), 
               size = 3, color = "black", fontface = "bold") +
  # 设置颜色方案
  scale_fill_manual(values = setNames(city_colors$color, city_colors$name_pinyin)) +
  # 添加比例尺和指北针
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", which_north = "true") +
  # 设置标题和图例
  labs(title = "湖北省采样点分布图（城市分区彩色版）",
       subtitle = "红色点为采样点，蓝色线条为长江",
       fill = "城市名称",
       caption = "数据来源：采样数据、Natural Earth") +
  # 设置主题
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) +
  # 调整图例
  guides(fill = guide_legend(override.aes = list(alpha = 0.7)))

# 保存图片
ggsave("hubei_sampling_map_colored.png", width = 12, height = 10, dpi = 300)
