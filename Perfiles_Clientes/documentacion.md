Documentación de Recomendaciones para Cambios en los Perfiles de Clientes
================
André Vargas
18 de enero de 2019

Introducción
============

En esta documentación se describirá de donde se obtuvieorn los datos, el modelo y el propósito de estos modelos. Estos modelos se hicieron con la finalidad de proporcionar información precisa sobre las diferencias de Genero por territorio, tipo de producto, giro de negocios, edad y montos vencidos, para cambiar e informar los perfiles de clientes actuales. Por lo que resuelve las siguientes preguntas:

1.  ¿Existe alguna diferencia entre Generos que explique el monto vencido?
2.  ¿Existe alguna diferencia entre generos por territorios que explique el monto vencido?
3.  ¿Existe alguna diferencia entre generos por edad que explique el monto vencido?
4.  ¿Existe alguna diferencia entre generos por territorios que explique el monto vencido?
5.  ¿Existe alguna diferencia entre generos por giro de negocio que explique el monto vencido?

Estas respuestas dan resultado a recomendaciones sobre cambios en los perfiles de clientes.

Bases de Datos
==============

La base de datos se obtuvo del cierre de mes del FCV Base. Este se crucó contra el Catálogo de Sucursales para obtener los territorios. Algunas variables son:

``` r
load("C:/Users/andrevargas/Documents/GitHub/Perfiles_Clientes/bases_genero.Rdata")
summary(fcv[1:8])
```

    ##  Oficina_Asesor     Territorio         Prod          SocioRea       
    ##  602    : 1589   HIDALGO :14388   4021   :58143   Min.   :  297061  
    ##  642    : 1431   NORTE   :11799   5010   : 9784   1st Qu.: 5596230  
    ##  113    : 1288   ORIENTE :12511   4011   : 2905   Median :12397044  
    ##  123    : 1247   PONIENTE:10896   3015   :  643   Mean   :24898020  
    ##  313    : 1246   PUEBLA  :13764   4321   :  433   3rd Qu.:34798170  
    ##  45     : 1203   SUR     : 9009   4032   :  234   Max.   :98899074  
    ##  (Other):64363                    (Other):  225                     
    ##      ValIni           MtoVen          CapVig             IntVig        
    ##  Min.   :  1000   Min.   :    0   Min.   :  -736.7   Min.   :-1595.51  
    ##  1st Qu.:  5000   1st Qu.: 1708   1st Qu.:     0.0   1st Qu.:    0.00  
    ##  Median :  6582   Median : 3389   Median :     0.0   Median :   95.42  
    ##  Mean   :  7051   Mean   : 3807   Mean   :   638.8   Mean   :  376.43  
    ##  3rd Qu.:  9000   3rd Qu.: 5350   3rd Qu.:     0.0   3rd Qu.:  404.40  
    ##  Max.   :180000   Max.   :84953   Max.   :180000.0   Max.   :36755.99  
    ## 

Modelos
=======

Los modelos utilizados fueron un "Random Forest". Este algoritmo tiene un fuerte poder predictivo y clasifica en orden las variables mas relevantes para explicar el fenómeno. Una vez clasificadas se utilizaron las variables que más explican varianza sobre el monto vencido se utlizó en una regresión lineal

``` r
library(randomForest)
load("C:/Users/andrevargas/Documents/GitHub/Perfiles_Clientes/bases_genero.Rdata")
varImpPlot(rf_gen)
```

![](documentacion_files/figure-markdown_github/Models-1.png)

``` r
load("C:/Users/andrevargas/Documents/GitHub/Perfiles_Clientes/bases_genero.Rdata")
summary(regx)
```

    ## 
    ## Call:
    ## lm(formula = MtoVen ~ Tasa_Final + ValIni + montosol + Prod + 
    ##     Genero * Territorio + Edad1 + Edad2 + No_Cuotas + Edo_Civil + 
    ##     Frec + Giro, data = fcv)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -76954  -1463    108   1464  29235 
    ## 
    ## Coefficients:
    ##                                                                  Estimate
    ## (Intercept)                                                    -2.240e+02
    ## Tasa_Final                                                     -2.724e+01
    ## ValIni                                                          3.955e-01
    ## montosol                                                        4.447e-02
    ## Prod4011                                                        1.659e+03
    ## Prod4012                                                        1.562e+03
    ## Prod4014                                                        4.784e+02
    ## Prod4021                                                        1.394e+03
    ## Prod4031                                                        1.580e+03
    ## Prod4032                                                       -5.446e+03
    ## Prod4221                                                        1.040e+03
    ## Prod4222                                                        1.415e+03
    ## Prod4311                                                        1.852e+03
    ## Prod4321                                                       -1.867e+03
    ## Prod4550                                                        1.379e+03
    ## Prod5010                                                        3.142e+02
    ## GeneroM                                                         2.016e+02
    ## TerritorioNORTE                                                 1.863e+02
    ## TerritorioORIENTE                                              -1.757e+02
    ## TerritorioPONIENTE                                             -3.524e+02
    ## TerritorioPUEBLA                                                2.717e+01
    ## TerritorioSUR                                                   8.885e+01
    ## Edad1                                                          -4.300e+04
    ## Edad2                                                          -9.170e+03
    ## No_Cuotas                                                       4.988e+01
    ## Edo_CivilS                                                      1.483e+02
    ## Frec                                                            1.191e+02
    ## GiroCOMPRA VENTA DE ARTICULOS NO CLASIFICADOS EN OTRA PARTE     3.568e+01
    ## GiroCOMPRA VENTA DE CALZADO                                     7.045e+01
    ## GiroCOMPRA VENTA DE ROPA                                        1.390e+02
    ## GiroEMPLEADO DEL SECTOR SERVICIOS                               1.082e+02
    ## GiroSERVICIOS DE ALIMENTOS EN LONCHERIAS TAQUERIAS Y TORTERIAS  6.700e+01
    ## GiroTIENDA DE ABARROTES Y MISCELANEA                           -1.223e+02
    ## GeneroM:TerritorioNORTE                                         2.413e+01
    ## GeneroM:TerritorioORIENTE                                       4.944e+00
    ## GeneroM:TerritorioPONIENTE                                     -3.819e+01
    ## GeneroM:TerritorioPUEBLA                                       -9.476e+00
    ## GeneroM:TerritorioSUR                                          -4.186e+01
    ##                                                                Std. Error
    ## (Intercept)                                                     1.447e+02
    ## Tasa_Final                                                      5.740e-01
    ## ValIni                                                          3.454e-03
    ## montosol                                                        2.211e-03
    ## Prod4011                                                        1.149e+02
    ## Prod4012                                                        7.240e+02
    ## Prod4014                                                        2.378e+03
    ## Prod4021                                                        1.046e+02
    ## Prod4031                                                        1.376e+03
    ## Prod4032                                                        2.019e+02
    ## Prod4221                                                        4.411e+02
    ## Prod4222                                                        2.122e+02
    ## Prod4311                                                        1.683e+03
    ## Prod4321                                                        1.615e+02
    ## Prod4550                                                        1.067e+03
    ## Prod5010                                                        1.108e+02
    ## GeneroM                                                         4.275e+01
    ## TerritorioNORTE                                                 3.740e+01
    ## TerritorioORIENTE                                               3.666e+01
    ## TerritorioPONIENTE                                              3.743e+01
    ## TerritorioPUEBLA                                                3.437e+01
    ## TerritorioSUR                                                   4.157e+01
    ## Edad1                                                           2.483e+03
    ## Edad2                                                           2.434e+03
    ## No_Cuotas                                                       9.745e-01
    ## Edo_CivilS                                                      1.819e+01
    ## Frec                                                            2.741e+00
    ## GiroCOMPRA VENTA DE ARTICULOS NO CLASIFICADOS EN OTRA PARTE     5.257e+01
    ## GiroCOMPRA VENTA DE CALZADO                                     3.868e+01
    ## GiroCOMPRA VENTA DE ROPA                                        3.361e+01
    ## GiroEMPLEADO DEL SECTOR SERVICIOS                               4.786e+01
    ## GiroSERVICIOS DE ALIMENTOS EN LONCHERIAS TAQUERIAS Y TORTERIAS  2.837e+01
    ## GiroTIENDA DE ABARROTES Y MISCELANEA                            3.772e+01
    ## GeneroM:TerritorioNORTE                                         6.164e+01
    ## GeneroM:TerritorioORIENTE                                       6.111e+01
    ## GeneroM:TerritorioPONIENTE                                      6.404e+01
    ## GeneroM:TerritorioPUEBLA                                        6.110e+01
    ## GeneroM:TerritorioSUR                                           6.604e+01
    ##                                                                t value
    ## (Intercept)                                                     -1.548
    ## Tasa_Final                                                     -47.447
    ## ValIni                                                         114.507
    ## montosol                                                        20.113
    ## Prod4011                                                        14.440
    ## Prod4012                                                         2.157
    ## Prod4014                                                         0.201
    ## Prod4021                                                        13.318
    ## Prod4031                                                         1.148
    ## Prod4032                                                       -26.974
    ## Prod4221                                                         2.358
    ## Prod4222                                                         6.670
    ## Prod4311                                                         1.101
    ## Prod4321                                                       -11.565
    ## Prod4550                                                         1.292
    ## Prod5010                                                         2.836
    ## GeneroM                                                          4.716
    ## TerritorioNORTE                                                  4.981
    ## TerritorioORIENTE                                               -4.792
    ## TerritorioPONIENTE                                              -9.416
    ## TerritorioPUEBLA                                                 0.791
    ## TerritorioSUR                                                    2.138
    ## Edad1                                                          -17.316
    ## Edad2                                                           -3.768
    ## No_Cuotas                                                       51.182
    ## Edo_CivilS                                                       8.153
    ## Frec                                                            43.447
    ## GiroCOMPRA VENTA DE ARTICULOS NO CLASIFICADOS EN OTRA PARTE      0.679
    ## GiroCOMPRA VENTA DE CALZADO                                      1.821
    ## GiroCOMPRA VENTA DE ROPA                                         4.136
    ## GiroEMPLEADO DEL SECTOR SERVICIOS                                2.260
    ## GiroSERVICIOS DE ALIMENTOS EN LONCHERIAS TAQUERIAS Y TORTERIAS   2.361
    ## GiroTIENDA DE ABARROTES Y MISCELANEA                            -3.242
    ## GeneroM:TerritorioNORTE                                          0.392
    ## GeneroM:TerritorioORIENTE                                        0.081
    ## GeneroM:TerritorioPONIENTE                                      -0.596
    ## GeneroM:TerritorioPUEBLA                                        -0.155
    ## GeneroM:TerritorioSUR                                           -0.634
    ##                                                                Pr(>|t|)
    ## (Intercept)                                                    0.121599
    ## Tasa_Final                                                      < 2e-16
    ## ValIni                                                          < 2e-16
    ## montosol                                                        < 2e-16
    ## Prod4011                                                        < 2e-16
    ## Prod4012                                                       0.031020
    ## Prod4014                                                       0.840516
    ## Prod4021                                                        < 2e-16
    ## Prod4031                                                       0.250896
    ## Prod4032                                                        < 2e-16
    ## Prod4221                                                       0.018373
    ## Prod4222                                                       2.58e-11
    ## Prod4311                                                       0.271008
    ## Prod4321                                                        < 2e-16
    ## Prod4550                                                       0.196410
    ## Prod5010                                                       0.004567
    ## GeneroM                                                        2.41e-06
    ## TerritorioNORTE                                                6.35e-07
    ## TerritorioORIENTE                                              1.65e-06
    ## TerritorioPONIENTE                                              < 2e-16
    ## TerritorioPUEBLA                                               0.429232
    ## TerritorioSUR                                                  0.032557
    ## Edad1                                                           < 2e-16
    ## Edad2                                                          0.000165
    ## No_Cuotas                                                       < 2e-16
    ## Edo_CivilS                                                     3.61e-16
    ## Frec                                                            < 2e-16
    ## GiroCOMPRA VENTA DE ARTICULOS NO CLASIFICADOS EN OTRA PARTE    0.497345
    ## GiroCOMPRA VENTA DE CALZADO                                    0.068539
    ## GiroCOMPRA VENTA DE ROPA                                       3.54e-05
    ## GiroEMPLEADO DEL SECTOR SERVICIOS                              0.023827
    ## GiroSERVICIOS DE ALIMENTOS EN LONCHERIAS TAQUERIAS Y TORTERIAS 0.018222
    ## GiroTIENDA DE ABARROTES Y MISCELANEA                           0.001187
    ## GeneroM:TerritorioNORTE                                        0.695391
    ## GeneroM:TerritorioORIENTE                                      0.935513
    ## GeneroM:TerritorioPONIENTE                                     0.550899
    ## GeneroM:TerritorioPUEBLA                                       0.876742
    ## GeneroM:TerritorioSUR                                          0.526199
    ##                                                                   
    ## (Intercept)                                                       
    ## Tasa_Final                                                     ***
    ## ValIni                                                         ***
    ## montosol                                                       ***
    ## Prod4011                                                       ***
    ## Prod4012                                                       *  
    ## Prod4014                                                          
    ## Prod4021                                                       ***
    ## Prod4031                                                          
    ## Prod4032                                                       ***
    ## Prod4221                                                       *  
    ## Prod4222                                                       ***
    ## Prod4311                                                          
    ## Prod4321                                                       ***
    ## Prod4550                                                          
    ## Prod5010                                                       ** 
    ## GeneroM                                                        ***
    ## TerritorioNORTE                                                ***
    ## TerritorioORIENTE                                              ***
    ## TerritorioPONIENTE                                             ***
    ## TerritorioPUEBLA                                                  
    ## TerritorioSUR                                                  *  
    ## Edad1                                                          ***
    ## Edad2                                                          ***
    ## No_Cuotas                                                      ***
    ## Edo_CivilS                                                     ***
    ## Frec                                                           ***
    ## GiroCOMPRA VENTA DE ARTICULOS NO CLASIFICADOS EN OTRA PARTE       
    ## GiroCOMPRA VENTA DE CALZADO                                    .  
    ## GiroCOMPRA VENTA DE ROPA                                       ***
    ## GiroEMPLEADO DEL SECTOR SERVICIOS                              *  
    ## GiroSERVICIOS DE ALIMENTOS EN LONCHERIAS TAQUERIAS Y TORTERIAS *  
    ## GiroTIENDA DE ABARROTES Y MISCELANEA                           ** 
    ## GeneroM:TerritorioNORTE                                           
    ## GeneroM:TerritorioORIENTE                                         
    ## GeneroM:TerritorioPONIENTE                                        
    ## GeneroM:TerritorioPUEBLA                                          
    ## GeneroM:TerritorioSUR                                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2375 on 72329 degrees of freedom
    ## Multiple R-squared:  0.3618, Adjusted R-squared:  0.3615 
    ## F-statistic:  1108 on 37 and 72329 DF,  p-value: < 2.2e-16

Al interpretar y analizar los resultados de la regresión anterior se llegaron a las siguientes recomendaciones y resultados:

Resultados
==========

Tasa de interés
---------------

La gente a la que se le otorga una tasa más alta tiene un menor monto vencido. Esto pude ser debido a tres posibilidades:

1.  Existe un sentido de reciprocidad. Al ser personas con poco o ningún acceso al crédito, estos son agradecidos al obtenerlo por primera vez.
2.  Existe un sentido de pertenencia. Esto por la historia que existe en FinComún al ser fundado.
3.  Existe miedo a endeudarse. Ya que es una tasa alta y por ser su primer crédito las personas tienden a pagar mejor.

Valor del Crédito
-----------------

Un incremento en el Valor Inicial del Crédito tiende a reducir el monto vencido. Aunque el efecto es pequeño, esta relación es interesante:

1.  Podría estar relacionada con el punto 3 o podría ser que ya que están solicitando un mayor monto y existe una mayor responsabilidad o miedo a endeudarse.

Monto Solicitado
----------------

1.  A mayor monto solicitado también el monto vencido se reduce. Muy probablemente, por las razones anteriores. Producto

Comparado con el producto 3015:
-------------------------------

1.  El producto 4011, 4012, 4021, 4221, 4222 y 5010, incrementan el monto vencido. Este efecto es dado porque el producto 3015 los demás productos tienen mucho mayor cantidad de créditos y la cantidad otorgada al crédito son mucho mayores. Se necesitaría hacer un análisis más refinado para saber si existen diferencias reales. Sin embargo, es importante recalcar que NO existe una relación entre el Género ni Edad con el tipo de producto. Lo cual nos dice que una diferenciación a nivel producto NO sería útil.

Genero
------

Comparado con las Mujeres:

1.  Los hombres incrementan el monto vencido. Esto inclusive tomando en cuenta que los créditos otorgados a mujeres es 1.8 veces mayor al que se le otorga a los hombre. SI observamos la tabla inferior se puede distinguir que: la media y mediana del Monto vencido es menor en créditos otorgados a mujeres, inclusive cuando la cantidad del crédito es casi el doble. También, de acuerdo a estudios y teorías nuestra población se comporta de la misma manera.

| Variables              | Mujer   | Hombre  |
|------------------------|---------|---------|
| Numero de Créditos     | 46503   | 25864   |
| Promedio Monto Vencido | 3657.75 | 4074.93 |
| Mediana Monto Vencido  | 3240.56 | 3664.69 |

Territorios
-----------

Comparado con Hidalgo:

1.  El Territorio Norte, Puebla Y Sur generan mayor monto Vencido, y Poniente Y Oriente lo disminuyen. Al igual que los productos esto requiere de un mejor análisis. Sin embargo es muy importante recalcar que no existe una relación entre el género y el territorio, haciendo que alguna diferenciación no sea efectiva.

Edad
----

1.  Mientras mayor sea la edad del cliente menor monto vencido generará. No existe relación entre la Edad y el Género, por lo cual diferenciar por edad y género no es útil.

Número de cuotas
----------------

1.  Mientras el número de cuotas sea mayor el monto vencido incrementará. Usando esta información podríamos mejorar la prospección. Mientras más joven sea la persona menor número cuotas se deberían de ofrecer.

Estado Civil
------------

1.  La gente Casada paga mejor que la soltera, sin tener diferencias por género. Por lo cual se recomienda no discriminar por esta relación.

Frecuencia de pagos
-------------------

1.  A mayor frecuencia de pagos mayor monto vencido. Se podría aplicar algo similar al número de cuotas.

Giros
-----

1.  Los giros fueron separados por número de créditos. Así se obtuvieron 7 categorías. Seis con la mayor cantidad de créditos y el resto en Otros. En comparación al resto de los giros (Otros), todos menos el giro de abarrotes y misceláneas, incrementan el monto vencido. Tampoco existe relación alguna entre el género y el giro.

Recomendaciones
---------------

1.  Ampliar la edad de las mujeres en todos los territorios y giros, al menos igual al a de los hombres.

2.  Buscar a gente alrededor de 39 años en adelante, sin embargo no discriminar a los más jóvenes.

3.  Para contrarrestar el efecto de la edad ,si son jóvenes, buscar que sean de preferencia:

    3.1 Casados,

    3.2 Que el plazo y frecuencia del crédito sean los menores.
