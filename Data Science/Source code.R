# Загрузка необходимых пакетов
# openintro (содержит данные и функции, связанные с статистикой)
library(openintro)
# tidyverse (объединяет несколько пакетов для работы с данными и создания графиков)
library(tidyverse)
#ggplot2 (для создания высококачественных графиков и визуализации данных)
library(ggplot2)
#readr (для чтения данных из текстовых файлов)
library(readr)
# leaflet (для создания интерактивных карт и визуализации географических данных)
library(leaflet)
#dplyr (для манипуляции и агрегации данных)
library(dplyr)
#corrplot (для визуализации корреляции между переменными)
library(corrplot)
# rpart (для построения решающих деревьев)
library(rpart)
#rpart.plot (для создания графиков решающих деревьев)
library(rpart.plot)
# randomForest (для построения случайных лесов)
library(randomForest)
#scales (для настройки шкал и форматирования графиков)
library(scales)


# 0. Загрузка данных


my_data <- read_csv("D:/лабы/AD/Eartquakes-1990-2023.csv")
head(my_data)
glimpse(my_data) 
summary(my_data)

# 1. Графики


# Распределение магнитуды 1

ggplot(data = my_data, aes(x = .data[["magnitudo"]])) +
  geom_histogram(fill = "blue", color = "black") +
  geom_density(aes(y = ..count..), color = "red", size = 1, bw = 0.2) +
  labs(x = "magnitudo", y = "Frequency", title = "Histogram with Density Line")

# Очищение магнитуды >= 0

my_data_filtered <- my_data %>%
  filter(magnitudo >= 0)

# Распределение магнитуды 2
ggplot(data = my_data_filtered, aes(x = magnitudo)) +
  geom_histogram(fill = "blue", color = "black") +  # Установите binwidth по желанию
  labs(x = "magnitudo", y = "Frequency", title = "Histogram of magnitudo") +
  scale_y_continuous(labels = comma_format()) +
  scale_x_continuous(breaks = seq(min(my_data_filtered$magnitudo), max(my_data_filtered$magnitudo), by = 0.25)) # Настройте интервал по желанию


# График плотности распределения глубины землетрясений в логарифмическом масштабе


ggplot(data = my_data_filtered, aes(x = .data[["depth"]])) +
  geom_density(aes(y = ..count..), color = "blue", fill = "blue", alpha = 0.3, size = 1, bw = 'nrd0') +  # Adjust bw and add fill
  scale_x_continuous(trans = 'log10', labels = scales::comma) +  
  labs(x = "Depth (log scale)", y = "Frequency", title = "Density Plot of Depth",) +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_y_continuous(labels = comma_format())



# Удаление колонки tsunami
my_data_filtered <- my_data_filtered %>%
  select(-tsunami)


# Группируем данные по годам и считаем количество землетрясений в каждом году
earthquake_count_by_year <- my_data_filtered %>%
  mutate(year = lubridate::year(date)) %>%  # Извлекаем год из даты
  group_by(year) %>%
  summarize(earthquake_count = n())

# Создаем график с количеством землетрясений по годам
ggplot(data = earthquake_count_by_year, aes(x = year, y = earthquake_count)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Earthquake Count by Year", x = "Year", y = "Number of Earthquakes")


# 2. Карта

# 50% данных
set.seed(42)  # Устанавливаем начальное значение для воспроизводимости
sampled_data <- my_data %>%
  sample_frac(0.25)

# Объект карты с начальными координатами и масштабом
map <- leaflet() %>%
  addTiles() %>%  # Добавьте фон карты
  setView(lng = 0, lat = 0, zoom = 2)  # Начальные координаты и масштаб

# точки землетрясений
map <- map %>% addCircleMarkers(
  data = sampled_data,
  lat = ~latitude,
  lng = ~longitude,
  radius = 3,  # Размер точек
  color = "red",  # Цвет точек
  popup = ~paste("Магнитуда:", magnitudo),  # Всплывающие подсказки
  label = ~paste("Магнитуда:", magnitudo)  # Метки
)

# Отображение карты
map




# 3. Магнитуда по странам

# Собираем США

my_data_filtered <- my_data_filtered %>%
  mutate(location_type = case_when(
    state %in% c(
      "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
      "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
      "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
      "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
      "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
      "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
      "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
      "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
      "Washington", "West Virginia", "Wisconsin", "Wyoming"
    ) ~ "USA",
    TRUE ~ state
  ))
usa_count <- my_data_filtered %>%
  filter(location_type == "USA") %>%  
  summarise(count = n())  

# Количество землетрясений в США
print(usa_count)


# Выбираем только интересующие нас страны
selected_countries <- c("Japan", "Nepal", "Canada","India", "Ecuador", "Philippines", "Pakistan", "El Salvador", "Mexico", "USA", "Turkey", "Indonesia", "Romania")

# Фильтрация данных для включения только выбранных стран
filtered_data <- my_data_filtered %>% 
  filter(state %in% selected_countries)


ggplot(filtered_data, aes(x = state, y = magnitudo, fill = state)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = colorRampPalette(c("blue", "purple"))(length(unique(filtered_data$state)))) +
  labs(
    x = "Country",
    y = "Earthquake Magnitude",
    title = "Boxplots of Earthquake Magnitude by Selected Countries",
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none",  # This removes the legend
    panel.grid.major.y = element_line(color = "grey80", size = 0.5),
  )





# 4. Зависимости


# Возьмем случайные 90% данных
set.seed(42)  
sampled_data <- my_data_filtered %>% sample_frac(0.9)  

# Глубина и магнитуда

ggplot(data = sampled_data, aes(x = depth, y = magnitudo)) +
  geom_point(color = "blue") +
  labs(x = "Depth of Earthquake (km)", y = "Magnitude of Earthquake")

# Значимость и магнитуда

ggplot(sampled_data, aes(x = magnitudo, y = significance)) +
  geom_point(color = "blue") +
  labs(x = "Magnitude of Earthquake", y = "Significance of Earthquake")+
  geom_smooth(method = "lm")


# 5. Корреляция

# Выбираем только численные столбцы
numeric_data <- my_data_filtered %>% select(time, latitude, longitude, depth, magnitudo, significance)


# Вычисление матрицы корреляции
correlation_matrix <- cor(numeric_data, use = "complete.obs") 
# Создаём палитру цветов, включающую белый цвет для нулевой корреляции
col <- colorRampPalette(c("darkblue", "white", "darkblue"))(200)

# Визуализация матрицы корреляции с полным диапазоном значений в легенде
corrplot(correlation_matrix, method="color", col=col, 
         order="hclust", tl.col="black", tl.srt=45, 
         is.corr=TRUE, addCoef.col = "black", # Добавляем коэффициенты корреляции
         number.cex = .7, # Размер текста для коэффициентов корреляции
         # Параметры для легенды
         cl.cex = 0.8, # Размер текста в легенде
         cl.ratio = 0.1 # Размер легенды относительно графика
)

# 6.Модели
# 6.1 Регрессия

set.seed(42)  # Устанавливаем начальное значение для воспроизводимости
sampled_data <- my_data_filtered %>%
  sample_frac(0.5)  # Выбираем случайные 50% данных

# Разделение данных на обучающую и тестовую выборки
set.seed(123)  # Установка начального числа для воспроизводимости результатов
training_index <- sample(1:nrow(sampled_data), 0.8 * nrow(sampled_data))
training_data <- sampled_data[training_index, ]
testing_data <- sampled_data[-training_index, ]

# Построение модели на обучающей выборке
model_training <- lm(magnitudo ~ longitude + latitude + depth + time, data = training_data)

# Проверка модели на тестовой выборке
predicted_values_test <- predict(model_training, newdata = testing_data)

# Расчет метрик для оценки модели
mse_test <- mean((testing_data$magnitudo - predicted_values_test)^2)
rmse_test <- sqrt(mse_test)

# Вывод метрик
print(paste("MSE:", mse_test))
print(paste("RMSE:", rmse_test))

# График остатков с синим цветом
ggplot(data = testing_data, aes(x = predicted_values_test, y = testing_data$magnitudo - predicted_values_test)) +
  geom_point(color = "blue") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Predicted Values", y = "Residuals") +
  ggtitle("Residuals vs Predicted Plot for Linear regression") +
  theme_minimal()


# Построение графика с фактическими значениями по оси X и предсказанными значениями по оси Y
ggplot(data = testing_data, aes(x = magnitudo, y = predicted_values_test)) +
  geom_point(color = "blue") +  # Точки для предсказаний
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + # Линия идеального соответствия
  labs(x = "Actual Magnitude", y = "Predicted Magnitude") +
  ggtitle("Actual vs Predicted Magnitude Plot for Linear Regression") +
  theme_minimal()


summary(model_training)

# 6.2 Дерево решений


# Создание модели дерева решений
model_dt <- rpart(magnitudo ~ longitude + latitude + depth + time, data = training_data, method = "anova")

# Визуализация дерева решений
rpart.plot(model_dt, main="Decision Tree for Earthquake Magnitude Prediction")

# Предсказание значений на тестовых данных
predicted_dt <- predict(model_dt, newdata = testing_data)

# Расчет метрик
# Среднеквадратическая ошибка (MSE)
mse_dt <- mean((testing_data$magnitudo - predicted_dt)^2)

# Корень из среднеквадратической ошибки (RMSE)
rmse_dt <- sqrt(mse_dt)

# Средняя абсолютная ошибка (MAE)
mae_dt <- mean(abs(testing_data$magnitudo - predicted_dt))

# Вывод метрик
print(paste("MSE for Decision Tree:", mse_dt))
print(paste("RMSE for Decision Tree:", rmse_dt))
print(paste("MAE for Decision Tree:", mae_dt))

# Создание данных для графика
plot_data <- data.frame(Actual = testing_data$magnitudo, Predicted = predicted_dt)

# Построение графика с фактическими значениями по оси X и предсказанными значениями по оси Y
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +  # Точки для предсказаний
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + # Линия идеального соответствия
  labs(x = "Actual Magnitude", y = "Predicted Magnitude") +
  ggtitle("Actual vs Predicted Magnitude Plot") +
  theme_minimal()

residuals_dt <- testing_data$magnitudo - predicted_dt
# Создание данных для графика остатков
residuals_data <- data.frame(Predicted = predicted_dt, Residuals = residuals_dt)

# Построение графика остатков
ggplot(residuals_data, aes(x = Predicted, y = Residuals)) +
  geom_point(color = "blue") +  # Точки для остатков
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Горизонтальная линия на уровне 0
  labs(x = "Predicted Magnitude", y = "Residuals") +
  ggtitle("Residuals vs Predicted Plot for Decision Tree") +
  theme_minimal()



# 6.3 Случайный лес

# Создание модели случайного леса
model_rf <- randomForest(magnitudo ~ longitude + latitude + depth + time, data = training_data,ntree=5)

# Предсказание значений на тестовых данных
predicted_rf <- predict(model_rf, newdata = testing_data)

# Расчет метрик
# Среднеквадратическая ошибка (MSE)
mse_rf <- mean((testing_data$magnitudo - predicted_rf)^2)

# Корень из среднеквадратической ошибки (RMSE)
rmse_rf <- sqrt(mse_rf)

# Средняя абсолютная ошибка (MAE)
mae_rf <- mean(abs(testing_data$magnitudo - predicted_rf))

# Вывод метрик
print(paste("MSE for Random Forest:", mse_rf))
print(paste("RMSE for Random Forest:", rmse_rf))
print(paste("MAE for Random Forest:", mae_rf))

# Визуализация важности переменных
importance_rf <- importance(model_rf)
varImpPlot(model_rf)

# Расчет остатков
residuals_rf <- testing_data$magnitudo - predicted_rf

# Построение графика остатков
ggplot(data = testing_data, aes(x = predicted_rf, y = residuals_rf)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Predicted Values", y = "Residuals") +
  ggtitle("Residuals vs Predicted Plot for Random Forest") +
  theme_minimal()

# Построение графика с фактическими значениями по оси X и предсказанными значениями по оси Y
ggplot(data = testing_data, aes(x = magnitudo, y = predicted_rf)) +
  geom_point(color = "blue") +
  geom_line(aes(y = magnitudo), color = "red", linetype = "dashed") +
  labs(x = "Actual Magnitude", y = "Predicted Magnitude") +
  ggtitle("Actual vs Predicted Magnitude Plot for Random Forest") +
  theme_minimal()








