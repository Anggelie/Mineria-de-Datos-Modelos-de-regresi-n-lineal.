library(ggplot2)
library(dplyr)

# 1) Convertir price a numérico
listings$price <- as.numeric(gsub("[$,]", "", listings$price))

# 2) Verificaciones rápidas
summary(listings$price)
class(listings$price)

set.seed(123)
sample_listings <- listings %>% sample_n(5000)

sample_listings2 <- sample_listings %>%
  filter(
    !is.na(price),
    !is.na(bedrooms),
    !is.na(accommodates),
    !is.na(room_type),
    price < 1000
  )


# CORRELACIONES

cor_bedrooms <- cor(sample_listings2$price, sample_listings2$bedrooms, use = "complete.obs")
cor_accommodates <- cor(sample_listings2$price, sample_listings2$accommodates, use = "complete.obs")

print(cor_bedrooms)
print(cor_accommodates)

# SCATTER PLOTS

ggplot(sample_listings2, aes(x = bedrooms, y = price)) +
  geom_jitter(alpha = 0.3) +
  labs(
    title = "Relación entre número de habitaciones y precio",
    x = "bedrooms",
    y = "price"
  )

ggplot(sample_listings2, aes(x = accommodates, y = price)) +
  geom_jitter(alpha = 0.3) +
  labs(
    title = "Relación entre capacidad y precio",
    x = "accommodates",
    y = "price"
  )

# COMPARACIÓN

ggplot(sample_listings2, aes(x = room_type, y = price)) +
  geom_boxplot() +
  labs(
    title = "Precio según tipo de habitación",
    x = "room_type",
    y = "price"
  )


summary(sample_listings2[, c("price", "bedrooms", "accommodates")])

table(sample_listings2$room_type)