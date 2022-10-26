# Tareas {-}



* Las tareas se envían por correo a <teresa.ortiz.mancera@gmail.com> con título: 
fundamentos-tareaXX (donde XX corresponde al número de tarea, 01..), usen siempre el mismo correo para enviar. 

* Las tareas deben incluir código y explicar claramente los resultados (si conocen [Rmarkdown](https://rmarkdown.rstudio.com) o 
[Quarto](https://quarto.org/) es muy conveniente para este propósito).

## 1. Visualización {-}

1. Describe 2 casos de cualquiera de las siguientes categorías: 1) dudas, 2) ejemplos que recuerdes de visualizaciones de baja calidad, 3) ejemplos de visualizaciones de alta calidad ó 3) conceptos que llamaron tu atención. Recuerda enviarla por
correo electrónico con título *fundamentos-tarea01*.


## 2. Análisis Exploratorio {-}

1. Realiza los ejercicios de los scripts 01_exploratorio.R, y 02_exploratorio.R del
proyecto *02-clase-exploratorio* ubicado en [RStudio.cloud](https://rstudio.cloud/). Puedes elegir
entre el ejercicio de propinas y el de casas, y debes hacer el de tomadores de té.



## 3. Tipos de estudio y PGD {-}

Para cada uno de los siguientes estudios, ubícalos en el recuadro y contesta lo 
que se pide. Envíen las respuestas por correo electrónico (con título 
fundamentos-tarea03).

![Inferencia estadística de acuerdo al tipo del diseño [@ramsey].](images/03_inferencia-estudio.png)

1. En 1930 se realizó un experimento en 20,000 niños de edad escolar de Inglaterra.
Los maestros fueron los responsables de asignar a los niños de manera aleatoria al
grupo de tratamiento -que consistía en recibir 350 ml de leche diaria - o al 
grupo de control, que no recibía suplementos alimenticios. Se registraron peso y 
talla antes y después del experimento. El estudio descubrió que los niños que
recibieron la leche ganaron más en peso en el lapso del estudio. Una 
investigación posterior descubrió que los niños del grupo control eran de mayor 
peso y talla que los del grupo de intervención, antes de iniciar el tratamiento. 
¿Qué pudo haber ocurrido? ¿Podemos 
utilizar los resultados del estudio para inferir causalidad?

1. Supongamos que de los registros de un conjunto de doctores se slecciona una 
muestra aleatoria de individuos americanos caucásicos y de americanos de 
ascendencia china, con el objetivo de comparar la presión arterial de las dos
poblaciones. Supongamos que a los seleccionados se les pregunta si quieren
participar y algunos rechazan. Se compara la distribución de presión arterial
entre los que accedieron a participar. ¿En que cuadro cae este estudio? ¿Qué 
supuesto es necesario para permitir inferencias a las poblaciones muestreadas?

1. Un grupo de investigadores reportó que el consumo moderado de alcohol estaba
asociado con un menor riesgo de demencia (Mukamal et al. (2003)). Su muestra 
consistía en 373 personas con demencia y 373 sin demencia. A los participantes 
se les pregintó cuánta cerveza, vino, o licor consumían. Se observó que aquellos
que consumían de 1-6 bebidas por semana tenían una incidencia menor de demencia
comparado a aquellos que se abstenían del alcohol. ¿se puede inferir causalidad?

2. Un estudio descubrió que los niños que ven más de dos horas diarias de 
televisión tienden a tener mayores niveles de colesterol que los que ven menos 
de dos horas diarias. ¿Cómo se pueden utilizar estos resultados?

1. Más gente se enferma de gripa en temporada de invierno, ¿esto prueba que las
temperaturas bajas ocasionan las gripas? ¿Qué otras variables podrían estar 
involucradas?

2. ¿Cuál es la diferencia entre un experimento aleatorizado y una muestra 
aleatoria?


## 4. Pruebas de hipótesis visuales y de permutación {-}

Resuelve la tarea-04 ubicada en nuestro RStudio.cloud, 
encontrarás un archivo .qmd que describe los ejercicios a realizar y si lo deseas puedes
utilizar este esqueleto para generar un archivo html con tus respuestas. 
Envía tu tarea por correo electrónico con el título fundamentos-tarea04.

## 5. Distribución muestral {-}

La tarea 5 es el proyecto de RStudio.Cloud con este nombre, el ejercicio está
en un archvio de qmd. Envíen el reporte html por correo electrónico (con 
título fundamentos-tarea05).

## 6. TCL e introducción a bootstrap {-}

La tarea 6 es el proyecto de RStudio.Cloud con este nombre, los ejercicios
están descritos en un archivo de R. Envíen un reporte por correo electrónico con 
las respuestas (con título fundamentos-tarea06).

## 7. Tarea conteo rápido {-}

Código de bootstrap


```r
boot_conteo <- function(){
  sample_2006 %>% 
    group_by(stratum) %>% 
    slice_sample(prop = 1, replace = TRUE) %>% 
    select(polling_id, stratum, pri_pvem:total) %>% # columnas relevantes
    pivot_longer(names_to = "party", values_to = "votes", 
                 cols = pri_pvem:otros) %>% # alargamos
    group_by(stratum, party) %>% 
    summarise(Y = sum(votes), X = sum(total), .groups = "drop") %>% 
    left_join(strata_sample_2006, by = "stratum") %>% # unimos tabla de pesos
    group_by(party) %>% 
    summarise(p_hat = 100 * sum(N / n * Y) / sum(N / n * X), .groups = "drop")
}
# Replicaciones bootstrap
boot_reps <- map_df(1:1000, ~boot_conteo(), .id = "sim")
```


## 8. Tarea ENIGH {-}

Para este ejercicio usaremos los datos de la [ENIGH 2014](https://www.inegi.org.mx/programas/enigh/tradicional/2014/). En particular
las variables alimentos, vestido, vivienda, salud, comunica, educacion y esparci 
(esparcimiento) que indican el gasto trimestral en cada una de las categorías. 

1. Calcula los deciles de ingreso usando la variable de ingreso corriente (ing_cor).

Debes tomar en cuenta el diseño de la muestra, puedes usar la función
`survey_quantile()` del paquete `srvyr` o `svyquantile()` del paquete `survey`.
Reporta las estimaciones y sus errores estándar usando el bootstrap de Rao y Wu.


```r
library(tidyverse)
library(readr)
library(dplyr)
library(srvyr)
concentrado_hogar <- read_csv("data/concentradohogar.csv")
# para facilitar los cálculos ponemos las variables en un vector
vars_cats <- c("alimentos", "vestido", "vivienda", "salud", "transporte",
               "comunica", "educacion", "esparci")

hogar <- concentrado_hogar %>% 
  select(folioviv, foliohog, ubica_geo, est_dis, upm, factor_hog, ing_cor,
         all_of(vars_cats))

# creamos una variable con el gasto total y el porcentaje de gasto en cada categoría
hogar_gastos <- hogar %>%
  mutate(gasto = alimentos + vestido + vivienda + salud + transporte + 
           comunica + educacion + esparci) %>% 
  dplyr::mutate_at(vars_cats, list(~. / gasto))

glimpse(hogar_gastos)
```


Para usar las funciones de `srvyr` lo primero es especificar el diseño de la 
muestra.


```r
enigh_design <- hogar_gastos %>% 
  as_survey_design(ids = upm, weights = factor_hog, strata = est_dis)
enigh_design
```

Elegimos *bootstrap* de Rao y Wu.


```r
enigh_boot <- enigh_design %>% 
  as_survey_rep(type = "subbootstrap", replicates = 300)

deciles_ingcor <- enigh_boot %>%
  srvyr::summarise(q_ing_cor = survey_quantile(ing_cor, seq(0.1, 1, 0.1), 
                                               vartype = NULL)) %>% 
  tidyr::pivot_longer(cols = everything())
deciles_ingcor
```

Creamos una nueva variable con los deciles.


```r
library(Hmisc)
enigh_boot_q <- enigh_boot %>%
  mutate(decil = cut2(ing_cor, cuts = deciles_ingcor$value, 
                      levels.mean = TRUE))
```

Y realizamos las estimaciones por decil.


```r
# estimaciones
estimaciones <- enigh_boot_q %>% 
  group_by(decil) %>% 
  srvyr::summarise_at(vars_cats, survey_mean, na.rm = TRUE) 

# errores estándar
estimaciones_se <- estimaciones %>% 
  select(decil, contains("se")) %>% 
  pivot_longer(-decil, names_to = "campo", values_to = "se") %>% 
  mutate(campo = stringr::str_extract(campo, ".*(?=_)"))

# unimos las dos para hacer la gráfica
estimaciones_tidy <- estimaciones %>% 
  select(decil, all_of(vars_cats)) %>% 
  pivot_longer(-decil, names_to = "campo", values_to = "est") %>% 
  left_join(estimaciones_se)
```

Y graficamos.


```r
ggplot(estimaciones_tidy) +
  geom_line(aes(x = as.integer(decil), y = est, group = campo, color = campo)) +
  scale_x_continuous("decil", breaks = 1:10)
```

Podemos hacer páneles para ver la variación en cada categoría.


```r
ggplot(estimaciones_tidy) +
  geom_line(aes(x = as.integer(decil), y = est, group = campo, color = campo), 
            show.legend = FALSE) +
  facet_wrap(~ campo, scales = "free_y", nrow = 2) +
  scale_x_continuous("decil", breaks = 1:10)
```

Y finalmente agregamos errores estándar.


```r
ggplot(estimaciones_tidy) +
  geom_line(aes(x = as.integer(decil), y = est, group = campo, color = campo), 
            show.legend = FALSE, alpha = 0.8) +
  geom_linerange(aes(x = as.integer(decil), ymin = est - 2 * se, ymax = est + 2 * se, 
                     group = campo, color = campo), show.legend = FALSE, alpha = 0.8) +
  facet_wrap(~ campo, scales = "free_y", nrow = 2) +
  scale_x_continuous("decil", breaks = 1:10)
```

### Mapas {-}

Si quisieran hacer un mapa con la mediana de ingreso corriente trimestral por
estado.

1. Lo primero que debemos hacer es agregar la variable entidad a tus datos. 
De acuerdo al diccionario de datos de la ENIGH la variable *ubicacion_geo*
contiene la clave de entidad.

    *ubica_geo: Ubicación geográfica, contiene la ubicación geográfica de la vivienda. 
    Los dos primeros dígitos representan la clave de la entidad, los siguientes tres 
    la clave del municipio y los últimos cuatro la clave de la localidad. Éstas 
    corresponden al Catálogo de claves de entidades federativas, municipios y 
    localidades, que está disponible en el sitio del INEGI.*

Por lo tanto debemos comenzar creando una variable de entidad, para ello usamos
la función `str_sub()` del paquete `stringr`.


```r
library(stringr)
enigh_boot_edo <- enigh_boot %>% 
  mutate(edo = str_sub(ubica_geo, 1, 2))
```

2. Una vez que tenemos la variable en el diseño usamos `group_by()` y `svy_median()`
para calcular la mediana en cada estado y el error estándar de las estimaciones.


```r
edo_ing_cor <- enigh_boot_edo %>% 
  group_by(edo) %>% 
  summarise(mediana = survey_median(ing_cor, vartype = "se")) 
```

Ahora vemos como hacer un mapa con los resultados de nuestro análisis, para ello
usaremos el paquete [mxmaps](https://www.diegovalle.net/mxmaps/index.html). En este 
caso el `data.frame` con la información de ingreso corriente por estado se 
llama `edo_ing_cor`.


```r
library(mxmaps)
data(mxstate.map) # datos para hacer el mapa instalar de github diegovalle/mxmaps
```





```r
mxstate_ing_cor <- mxstate.map %>% 
  left_join(edo_ing_cor, by = c("region" = "edo"))

# este es el mapa básico
mapa_ingcor <- ggplot(mxstate_ing_cor, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = mediana), color = "darkgray") +
  coord_map()

mapa_ingcor
```

Podemos agregar interactividad con el paquete `plotly`.


```r
library(plotly)

# función para formatear con comas los datos de ingreso
comma <- function(x) format(x, digits = 2, big.mark = ",")

mapa_ingcor <- ggplot(mxstate_ing_cor %>% mutate(ee = round(mediana_se)), 
                      aes(long, lat, group = group, 
                          text = str_c("ingreso corriente: ",  
                                       comma(mediana)), label = ee)) + 
  geom_polygon(aes(fill = mediana), color = "darkgray", size = 0.1) +
  scale_fill_distiller(palette = "YlGnBu", type = "div") + #paleta 
  labs(title = "Mediana de ingreso corriente", fill = "") + 
  theme_void() +
  coord_map()

ggplotly(mapa_ingcor, tooltip = c("text", "label"))
```

## 9. Máxima verosimilitud y bootstrap paramétrico {-}

Sean $X_1,...,X_n \sim N(\mu, 1)$ y $\theta = e^{\mu}$. Simula una muestra de  
tamaño $n = 100$ usando $\mu = 5$.

* Usa bootstrap paramétrico para construir un intervalo del 95% de confianza.

* Usa bootstrap no paramétrico y compara tus respuestas.

* Realiza un histograma de replicaciones bootstrap para cada método, estas son estimaciones de la distribución $\hat{\theta}$. Comparalos con la verdadera distribución de $\hat{\theta}$
(que puedes obtener vía simulación). ¿Cuál es la aproximación más cercana a la verdadera distribución?

## 10. Bootstrap y más pruebas de hipótesis {-}

1. Comparando dos tratamientos. Supongamos que a $n_1$ personas se les asigna el 
tratamiento 1 y a $n_2$ se les asigna el tratamiento 2. Sean $X_1$ el número de
personas que responden de manera favorable al tratamiento uno y $X_2$ el 
número de personas que responden de manera favorable al tratamiento 2. 
Suponemos $X_1 \sim Binomial(n_1, p_1)$ y $X_2 \sim Binomial(n_2, p_2)$. Sea
$\psi = p_1 - p_2$ la cantidad de interés:

* Encuentra el estimador de máxima verosimilitud para $\psi$.  
* (Opcinal) Usa el método [delta para más de un parámetro](https://en.wikipedia.org/wiki/Delta_method#Multivariate_delta_method) 
para encontrar el error estándaar asintótico de $\hat{\psi}_{MLE}$.  
* Supongamos $n_1 = n_2 = 200$, $X_1 = 160$ y $X_2 = 148$ construye un intervalo
del 90% de confianza para $\psi$ usando bootstrap paramétrico y (opcional) el 
método delta.
 
1. (Chihara) Los niveles de calcio en adultos saludables se distribuyen de acuerdo
a una Normal con media 9.5 mg/dl y desviación estándar desconocida. Un médico 
sospecha que la media de los niveles de calcio para mujeres en su comunidad es 
distinta. Colecta mediciones de 20 mujeres saludables y encuentra que la media 
es de 9.2 y la desviación estándar muestral de 1.1. Escribe la hipótesis nula, 
realiza una prueba de hipótesis e interpreta los resultados.


```r
# Respuestas
#Ejercicio 1: bootstrap
n_1 <- 200; n_2 <-  200
X_1 = 160; X_2 <- 148

p_1 <- X_1/n_1
p_2 <- X_2 / n_2

ee <- sqrt(p_1*(1 - p_1) / n_2 + p_2*(1 - p_2) / n_2)

#delta
(p_1 - p_2) - 1.645 * ee
(p_1 - p_2) + 1.645 * ee

# bootstarp
dif_p <- function() {
  p_1_star <- rbinom(1, 200, 160/200) / 200
  p_2_star <- rbinom(1, 200, 148/200) / 200
  p_1_star - p_2_star
}
ee_boot <- rerun(1000, dif_p()) |> flatten_dbl() |> sd()
(p_1 - p_2) - 1.645 * ee_boot
(p_1 - p_2) + 1.645 * ee_boot


# Ejercicio 2: t
mu_0 <- 9.5
ee_hat <- 1.1 / sqrt(20)
t <- (9.2 - mu_0) / ee_hat
2 * pt(t, df = 19)

# podemos calcular el error estándar con boostrap paramétrico
ee_boot_hat <- rerun(10000, rnorm(20, 9.5, 1.1) |> mean()) |> flatten_dbl() |> sd()
ee_boot_hat
ee_hat <- 1.1 / sqrt(20)
t <- (9.2 - mu_0) / ee_boot_hat
2 * pt(t, df = 19)

# Con simulación es equivalente a una prueba Wald, con la misma deficiencia que
# una prueba de Wald que asume la varianza conocida y da resultados un poco
# diferentes con tamaño de muestra chicos
sims_mu <- rerun(20000, rnorm(20, 9.5, 1.1) |> mean()) |> flatten_dbl()
2 * mean(sims_mu <= 9.2)

# Wald
w <- (9.2 - mu_0) / ee_hat
2 * pnorm(w)

# en este ejemplo era más apropuado la t por que n-1=19 si tiene diferencias
# entre la t y la normal
```

