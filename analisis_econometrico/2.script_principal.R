# Ejercicio 2: Econometria

library(tidyverse)

set.seed(123)

# Cargar dataset
gapminder <- read_csv("gapminder.csv")

# PARTE 1: INGRESO POR PERSONA

# 1.

arg_datos <- gapminder %>% 
  filter(country == "Argentina") %>% 
  arrange(year)

ggplot(arg_datos, aes(x = year, y = income_per_person)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolución temporal del Ingreso por Persona en Argentina",
    x = "Año",
    y = "Ingreso por persona (USD)"
  ) +
  theme_minimal()

#2.

# 2001-2010 y 1960-2000
arg_entr <- subset(arg_datos, year <= 2000)
arg_test  <- subset(arg_datos, year > 2000)

# (a) 
mod_lineal <- lm(income_per_person ~ year, data = arg_entr)

# (b)
mod_poly2  <- lm(income_per_person ~ poly(year, 2, raw = TRUE), data = arg_entr)

# (c)
mod_poly10 <- lm(income_per_person ~ poly(year, 10, raw = TRUE), data = arg_entr)

g_overfit <- ggplot(arg_datos, aes(x = year, y = income_per_person)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "green") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, color = "purple") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 10, raw = TRUE), se = FALSE, color = "orange") +
  labs(
    title = "Ajuste de Modelos sobre Argentina",
    x = "Año",
    y = "Ingreso por persona (USD)"
  ) +
  theme_minimal()

print(g_overfit)

#Grafico
error_entr <- c(
  Lineal  = sqrt(mean((arg_entr$income_per_person - fitted(mod_lineal))^2)),
  Grado2  = sqrt(mean((arg_entr$income_per_person - fitted(mod_poly2))^2)),
  Grado10 = sqrt(mean((arg_entr$income_per_person - fitted(mod_poly10))^2))
)

error_test <- c(
  Lineal  = sqrt(mean((arg_test$income_per_person - predict(mod_lineal, newdata = arg_test))^2)),
  Grado2  = sqrt(mean((arg_test$income_per_person - predict(mod_poly2,  newdata = arg_test))^2)),
  Grado10 = sqrt(mean((arg_test$income_per_person - predict(mod_poly10, newdata = arg_test))^2))
)

print(round(error_entr, 2))
print(round(error_test, 2))

#3.

#Extraer los ingresos de cada país ordenados cronológicamente
ing_arg <- (gapminder %>% filter(country == "Argentina") %>% arrange(year))$income_per_person
ing_bra <- (gapminder %>% filter(country == "Brazil")    %>% arrange(year))$income_per_person
ing_chl <- (gapminder %>% filter(country == "Chile")     %>% arrange(year))$income_per_person
ing_col <- (gapminder %>% filter(country == "Colombia")  %>% arrange(year))$income_per_person
ing_uru <- (gapminder %>% filter(country == "Uruguay")   %>% arrange(year))$income_per_person

tabla_ingresos <- data.frame(
  Argentina = ing_arg,
  Brasil    = ing_bra,
  Chile     = ing_chl,
  Colombia  = ing_col,
  Uruguay   = ing_uru
)

# a)
matriz_niveles <- cor(tabla_ingresos)
cat("\n=== (a) MATRIZ DE CORRELACIÓN DE NIVELES DE INGRESO ===\n")
print(round(matriz_niveles, 3))

# b) ∆Y /Y 
crec_arg <- diff(ing_arg) / ing_arg[-length(ing_arg)]
crec_bra <- diff(ing_bra) / ing_bra[-length(ing_bra)]
crec_chl <- diff(ing_chl) / ing_chl[-length(ing_chl)]
crec_col <- diff(ing_col) / ing_col[-length(ing_col)]
crec_uru <- diff(ing_uru) / ing_uru[-length(ing_uru)]

tabla_crecimiento <- data.frame(
  Argentina = crec_arg,
  Brasil    = crec_bra,
  Chile     = crec_chl,
  Colombia  = crec_col,
  Uruguay   = crec_uru
)

matriz_crecimiento <- cor(tabla_crecimiento)
cat("\n=== (b) MATRIZ DE CORRELACIÓN DE CRECIMIENTO INTERANUAL (Y/Y) ===\n")
print(round(matriz_crecimiento, 3))


# PARTE 2: ESPERANZA DE VIDA Y GÉNERO

# Filtrar un año en particular a elección (2010) y asegurar formato numérico sin NAs
parte_2 <- gapminder %>% 
  filter(year == 2010) %>% 
  mutate(
    life_expectancy_female = as.numeric(life_expectancy_female),
    life_expectancy        = as.numeric(life_expectancy),
    income_per_person      = as.numeric(income_per_person),
    child_mortality        = as.numeric(child_mortality),
    children_per_woman     = as.numeric(children_per_woman)
  ) %>% 
  filter(!is.na(life_expectancy) & !is.na(life_expectancy_female) & !is.na(income_per_person))


#5.

plot(parte_2$life_expectancy_female, parte_2$life_expectancy,
     main = "Esperanza de Vida Total vs. Femenina (Año 2010)",
     xlab = "Esperanza de vida femenina (años)",
     ylab = "Esperanza de vida total (años)",
     pch = 16, col = "blue")

#6.

modelo_simple <- lm(life_expectancy ~ life_expectancy_female, data = parte_2)
sum_simple    <- summary(modelo_simple)

# Agregar la línea de regresión al gráfico
abline(modelo_simple, col = "red", lwd = 2)

# R^2
R2_sum <- sum_simple$r.squared
y_hat  <- fitted(modelo_simple)
Y_obs  <- parte_2$life_expectancy
R2_man <- 1 - sum((Y_obs - y_hat)^2) / sum((Y_obs - mean(Y_obs))^2)

cat("\n=== Inciso 6: Modelo Simple ===\n")
print(sum_simple)
cat(sprintf("\nR^2 (summary) = %.4f | R^2 (manual) = %.4f\n\n", R2_sum, R2_man))


#7

parte_2 <- parte_2 %>% 
  mutate(diferencia = life_expectancy_female - life_expectancy)

test_lm <- lm(diferencia ~ 1, data = parte_2)
summary(test_lm)

#8.

mod_simple   <- lm(life_expectancy ~ life_expectancy_female, data = parte_2)
mod_multiple <- lm(life_expectancy ~ life_expectancy_female + income_per_person, data = parte_2)

summary(mod_simple)
summary(mod_multiple)
