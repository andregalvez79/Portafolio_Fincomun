Documentación de Variaciones sobre el IMOR
================
André Vargas
16 de enero de 2019

Introducción
------------

En esta documentación se describirá de donde se obtuvieorn los datos, el modelo y el propósito de estos modelos. Estos modelos se hicieron con la finalidad de proporcionar información estadistica sobre el comportamiento y relación de la cartera total, la cartera vencida y el IMOR (índice de mora).

Por lo que resuelve las siguientes preguntas:

1.  ¿Qué relación y como se explica el desarrollo del IMOR por tipo de colocación?
2.  ¿Qué relación y como se explica el desarrollo del IMOR por territorios?
3.  ¿Qué territorios muestran un desarrollo del IMOR no esperado?
4.  ¿Qué sucursales son las que llevan este efecto no esperado?

Bases de Datos
--------------

Las bases de datos se obtuvieron de cruzar los archivos de cierre de mes "FCV Base" y "FCV Flujos" (Ver SAS query). Se utilizó R para acumular las bases cruzadas y obtener la base general que se utilizó para el análisis y modelos.

``` r
load("C:\\Users\\andrevargas\\Documents\\IMOR\\base_sucs.RData")
summary(fcvtot)
```

    ##  Oficina_Asesor       Tipox          fecha         Territorio 
    ##  Min.   : 11.0           :  72   2017_02: 191   HIDALGO :820  
    ##  1st Qu.: 53.0   Nuevo   :2203   2017_01: 190   NORTE   :777  
    ##  Median :121.0   Recompra:2201   2017_03: 188   ORIENTE :806  
    ##  Mean   :239.3                   2017_04: 188   PONIENTE:686  
    ##  3rd Qu.:346.0                   2017_06: 188   PUEBLA  :695  
    ##  Max.   :981.0                   2017_07: 188   SUR     :692  
    ##                                  (Other):3343                 
    ##        CT                CVen              CVig         
    ##  Min.   :     723   Min.   :      0   Min.   :       0  
    ##  1st Qu.:  637879   1st Qu.:  40694   1st Qu.:  583614  
    ##  Median : 1684210   Median :  72889   Median : 1577765  
    ##  Mean   : 3529668   Mean   : 106637   Mean   : 3423031  
    ##  3rd Qu.: 5858671   3rd Qu.: 127741   3rd Qu.: 5729751  
    ##  Max.   :20509747   Max.   :2253674   Max.   :19858647  
    ## 

Esta base se separo en dos:

1.  por recuperados,
2.  por nuevos. (ver generar\_bases)

Aquí se generaron dos modelos.

Modelos generar\_bases
----------------------

Se generaron dos modelos de tipo "mixed-models" que utiliza varias repiticiones sobre la misma observación y tmabién toma en consideración que para cada variable y observación existen diferentes condiciones de inicio (intercepto y pendeinte), de tal manera que se obtienen estimaciones generales sin sesgo.

En otras palabras estos modelos son ideales ya que toman en cuenta que cada territorio y/o sucursal tiene su propio tipo de condiciones externas que determinan sus condiciones. Así no afectando ni sesgando los estimadores ya que estos asumen que todos estos son homogeneos.

Siguiente ejemplos del resultado de uno de estos modelos.

``` r
load("C:\\Users\\andrevargas\\Documents\\IMOR\\base_sucs.RData")
summary(maxmodel3)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: imor ~ 1 + Var_CT_1 * Var_CVen_z + (1 + Var_CT_1 * Var_CVen_z |  
    ##     Territorio)
    ##    Data: fcvtotN
    ## Control: lmerControl(optCtrl = list(maxfun = 1e+09, calc.derivs = FALSE))
    ## 
    ## REML criterion at convergence: -787.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.0965 -0.6604 -0.1754  0.7064  2.6794 
    ## 
    ## Random effects:
    ##  Groups     Name                Variance  Std.Dev. Corr             
    ##  Territorio (Intercept)         1.574e-04 0.012547                  
    ##             Var_CT_1            3.946e-03 0.062821 -0.98            
    ##             Var_CVen_z          6.625e-07 0.000814 -0.99  1.00      
    ##             Var_CT_1:Var_CVen_z 7.467e-05 0.008641  0.90 -0.96 -0.95
    ##  Residual                       1.740e-04 0.013192                  
    ## Number of obs: 144, groups:  Territorio, 6
    ## 
    ## Fixed effects:
    ##                      Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)          0.080424   0.005240  4.976106  15.349 2.21e-05 ***
    ## Var_CT_1            -0.267626   0.032459  5.535435  -8.245  0.00026 ***
    ## Var_CVen_z           0.003587   0.001170 25.720980   3.066  0.00504 ** 
    ## Var_CT_1:Var_CVen_z -0.088104   0.012926  3.164742  -6.816  0.00544 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Vr_CT_1 Vr_CV_
    ## Var_CT_1    -0.762               
    ## Var_CVen_z  -0.276  0.176        
    ## V_CT_1:V_CV  0.232  0.191  -0.152
    ## convergence code: 0
    ## singular fit
    ## unused control arguments ignored

Con esta información se separó la base por territorios y se generaron modelos generales como el siguiente (ver generar\_bases\_sucursal):

``` r
load("C:\\Users\\andrevargas\\Documents\\IMOR\\base_sucs.RData")
summary(maxmodelsuc)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: imor ~ 1 + Var_CT_z * Var_CVen_z * sucursalf + (0 + Var_CT_z +  
    ##     Var_CVen_z | sucursalf)
    ##    Data: fcvtotR_Norte
    ## Control: lmerControl(optCtrl = list(maxfun = 1e+09, calc.derivs = FALSE))
    ## 
    ## REML criterion at convergence: -1637.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5193 -0.5005 -0.0528  0.4310  4.8050 
    ## 
    ## Random effects:
    ##  Groups    Name       Variance  Std.Dev. Corr
    ##  sucursalf Var_CT_z   0.0002107 0.01452      
    ##            Var_CVen_z 0.0002107 0.01452  0.00
    ##  Residual             0.0002107 0.01452      
    ## Number of obs: 384, groups:  sucursalf, 16
    ## 
    ## Fixed effects:
    ##                                    Estimate Std. Error         df t value
    ## (Intercept)                       2.252e-02  3.058e-03  3.200e+02   7.364
    ## Var_CT_z                         -1.246e-02  1.575e-02  3.198e+02  -0.791
    ## Var_CVen_z                        9.222e-03  1.526e-02  2.998e+02   0.604
    ## sucursalf53                       6.694e-03  4.467e-03  3.200e+02   1.498
    ## sucursalf54                       3.266e-03  4.342e-03  3.200e+02   0.752
    ## sucursalf57                      -4.699e-04  4.289e-03  3.200e+02  -0.110
    ## sucursalf58                      -1.944e-03  4.288e-03  3.200e+02  -0.453
    ## sucursalf81                       2.453e-02  4.385e-03  3.200e+02   5.594
    ## sucursalf83                       2.268e-02  4.631e-03  3.200e+02   4.899
    ## sucursalf84                       1.299e-02  4.280e-03  3.200e+02   3.034
    ## sucursalf85                       2.539e-02  4.435e-03  3.200e+02   5.725
    ## sucursalf87                       1.331e-02  4.360e-03  3.200e+02   3.053
    ## sucursalf101                      5.339e-03  4.439e-03  3.200e+02   1.203
    ## sucursalf104                     -5.397e-03  4.341e-03  3.200e+02  -1.243
    ## sucursalf111                     -1.051e-03  5.256e-03  3.200e+02  -0.200
    ## sucursalf121                     -8.175e-03  4.516e-03  3.200e+02  -1.810
    ## sucursalf122                     -4.515e-03  4.427e-03  3.200e+02  -1.020
    ## sucursalf131                      1.171e-02  4.690e-03  3.200e+02   2.497
    ## Var_CT_z:Var_CVen_z              -6.159e-03  8.680e-03  3.200e+02  -0.710
    ## Var_CT_z:sucursalf53              7.678e-03  2.198e-02  3.198e+02   0.349
    ## Var_CT_z:sucursalf54              1.134e-03  2.253e-02  3.198e+02   0.050
    ## Var_CT_z:sucursalf57              1.655e-02  2.287e-02  3.199e+02   0.724
    ## Var_CT_z:sucursalf58              3.838e-03  2.248e-02  3.198e+02   0.171
    ## Var_CT_z:sucursalf81             -2.826e-02  2.257e-02  3.198e+02  -1.252
    ## Var_CT_z:sucursalf83              7.220e-03  2.160e-02  3.198e+02   0.334
    ## Var_CT_z:sucursalf84             -1.679e-02  2.238e-02  3.198e+02  -0.750
    ## Var_CT_z:sucursalf85              6.631e-03  2.186e-02  3.198e+02   0.303
    ## Var_CT_z:sucursalf87              1.181e-02  2.190e-02  3.198e+02   0.539
    ## Var_CT_z:sucursalf101             9.832e-03  2.251e-02  3.198e+02   0.437
    ## Var_CT_z:sucursalf104             1.316e-02  2.189e-02  3.198e+02   0.601
    ## Var_CT_z:sucursalf111             1.134e-02  2.194e-02  3.198e+02   0.517
    ## Var_CT_z:sucursalf121             9.670e-03  2.289e-02  3.199e+02   0.422
    ## Var_CT_z:sucursalf122             7.585e-03  2.259e-02  3.198e+02   0.336
    ## Var_CT_z:sucursalf131             1.641e-03  2.275e-02  3.199e+02   0.072
    ## Var_CVen_z:sucursalf53            5.725e-04  2.191e-02  3.009e+02   0.026
    ## Var_CVen_z:sucursalf54           -4.424e-03  2.131e-02  2.988e+02  -0.208
    ## Var_CVen_z:sucursalf57           -4.718e-03  2.128e-02  2.987e+02  -0.222
    ## Var_CVen_z:sucursalf58           -4.945e-03  2.134e-02  2.989e+02  -0.232
    ## Var_CVen_z:sucursalf81            2.134e-03  2.157e-02  2.997e+02   0.099
    ## Var_CVen_z:sucursalf83            7.250e-03  2.223e-02  3.019e+02   0.326
    ## Var_CVen_z:sucursalf84            5.431e-03  2.151e-02  2.995e+02   0.252
    ## Var_CVen_z:sucursalf85           -3.259e-03  2.173e-02  3.003e+02  -0.150
    ## Var_CVen_z:sucursalf87            4.466e-03  2.156e-02  2.997e+02   0.207
    ## Var_CVen_z:sucursalf101          -5.664e-03  2.146e-02  2.994e+02  -0.264
    ## Var_CVen_z:sucursalf104          -6.539e-03  2.113e-02  2.981e+02  -0.309
    ## Var_CVen_z:sucursalf111          -6.700e-03  2.171e-02  3.002e+02  -0.309
    ## Var_CVen_z:sucursalf121          -8.481e-03  2.152e-02  2.996e+02  -0.394
    ## Var_CVen_z:sucursalf122          -5.428e-03  2.132e-02  2.988e+02  -0.255
    ## Var_CVen_z:sucursalf131          -3.605e-04  2.252e-02  3.028e+02  -0.016
    ## Var_CT_z:Var_CVen_z:sucursalf53   1.041e-02  1.378e-02  3.200e+02   0.755
    ## Var_CT_z:Var_CVen_z:sucursalf54   9.359e-03  9.314e-03  3.200e+02   1.005
    ## Var_CT_z:Var_CVen_z:sucursalf57   2.024e-02  1.342e-02  3.200e+02   1.508
    ## Var_CT_z:Var_CVen_z:sucursalf58   2.494e-03  1.292e-02  3.200e+02   0.193
    ## Var_CT_z:Var_CVen_z:sucursalf81   1.309e-02  1.014e-02  3.200e+02   1.291
    ## Var_CT_z:Var_CVen_z:sucursalf83  -8.645e-03  1.063e-02  3.200e+02  -0.813
    ## Var_CT_z:Var_CVen_z:sucursalf84  -2.450e-02  1.191e-02  3.200e+02  -2.057
    ## Var_CT_z:Var_CVen_z:sucursalf85   4.312e-03  1.120e-02  3.200e+02   0.385
    ## Var_CT_z:Var_CVen_z:sucursalf87   2.093e-02  1.154e-02  3.200e+02   1.814
    ## Var_CT_z:Var_CVen_z:sucursalf101  7.734e-03  9.969e-03  3.200e+02   0.776
    ## Var_CT_z:Var_CVen_z:sucursalf104  7.931e-03  9.632e-03  3.200e+02   0.823
    ## Var_CT_z:Var_CVen_z:sucursalf111  6.799e-03  1.254e-02  3.200e+02   0.542
    ## Var_CT_z:Var_CVen_z:sucursalf121  7.214e-03  1.641e-02  3.200e+02   0.440
    ## Var_CT_z:Var_CVen_z:sucursalf122  2.109e-03  1.059e-02  3.200e+02   0.199
    ## Var_CT_z:Var_CVen_z:sucursalf131  3.506e-03  1.168e-02  3.200e+02   0.300
    ##                                  Pr(>|t|)    
    ## (Intercept)                      1.52e-12 ***
    ## Var_CT_z                          0.42952    
    ## Var_CVen_z                        0.54602    
    ## sucursalf53                       0.13500    
    ## sucursalf54                       0.45249    
    ## sucursalf57                       0.91284    
    ## sucursalf58                       0.65063    
    ## sucursalf81                      4.76e-08 ***
    ## sucursalf83                      1.53e-06 ***
    ## sucursalf84                       0.00261 ** 
    ## sucursalf85                      2.38e-08 ***
    ## sucursalf87                       0.00246 ** 
    ## sucursalf101                      0.22990    
    ## sucursalf104                      0.21473    
    ## sucursalf111                      0.84158    
    ## sucursalf121                      0.07119 .  
    ## sucursalf122                      0.30849    
    ## sucursalf131                      0.01302 *  
    ## Var_CT_z:Var_CVen_z               0.47849    
    ## Var_CT_z:sucursalf53              0.72710    
    ## Var_CT_z:sucursalf54              0.95987    
    ## Var_CT_z:sucursalf57              0.46978    
    ## Var_CT_z:sucursalf58              0.86453    
    ## Var_CT_z:sucursalf81              0.21147    
    ## Var_CT_z:sucursalf83              0.73840    
    ## Var_CT_z:sucursalf84              0.45379    
    ## Var_CT_z:sucursalf85              0.76180    
    ## Var_CT_z:sucursalf87              0.59005    
    ## Var_CT_z:sucursalf101             0.66250    
    ## Var_CT_z:sucursalf104             0.54803    
    ## Var_CT_z:sucursalf111             0.60573    
    ## Var_CT_z:sucursalf121             0.67301    
    ## Var_CT_z:sucursalf122             0.73723    
    ## Var_CT_z:sucursalf131             0.94255    
    ## Var_CVen_z:sucursalf53            0.97917    
    ## Var_CVen_z:sucursalf54            0.83570    
    ## Var_CVen_z:sucursalf57            0.82467    
    ## Var_CVen_z:sucursalf58            0.81690    
    ## Var_CVen_z:sucursalf81            0.92127    
    ## Var_CVen_z:sucursalf83            0.74458    
    ## Var_CVen_z:sucursalf84            0.80086    
    ## Var_CVen_z:sucursalf85            0.88089    
    ## Var_CVen_z:sucursalf87            0.83602    
    ## Var_CVen_z:sucursalf101           0.79205    
    ## Var_CVen_z:sucursalf104           0.75720    
    ## Var_CVen_z:sucursalf111           0.75781    
    ## Var_CVen_z:sucursalf121           0.69377    
    ## Var_CVen_z:sucursalf122           0.79922    
    ## Var_CVen_z:sucursalf131           0.98724    
    ## Var_CT_z:Var_CVen_z:sucursalf53   0.45061    
    ## Var_CT_z:Var_CVen_z:sucursalf54   0.31571    
    ## Var_CT_z:Var_CVen_z:sucursalf57   0.13251    
    ## Var_CT_z:Var_CVen_z:sucursalf58   0.84710    
    ## Var_CT_z:Var_CVen_z:sucursalf81   0.19768    
    ## Var_CT_z:Var_CVen_z:sucursalf83   0.41680    
    ## Var_CT_z:Var_CVen_z:sucursalf84   0.04046 *  
    ## Var_CT_z:Var_CVen_z:sucursalf85   0.70043    
    ## Var_CT_z:Var_CVen_z:sucursalf87   0.07066 .  
    ## Var_CT_z:Var_CVen_z:sucursalf101  0.43842    
    ## Var_CT_z:Var_CVen_z:sucursalf104  0.41093    
    ## Var_CT_z:Var_CVen_z:sucursalf111  0.58815    
    ## Var_CT_z:Var_CVen_z:sucursalf121  0.66045    
    ## Var_CT_z:Var_CVen_z:sucursalf122  0.84223    
    ## Var_CT_z:Var_CVen_z:sucursalf131  0.76431    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## convergence code: 0
    ## unused control arguments ignored

Del modelo anterior se tomaron las sucursales que explican varianza sobre el IMOR. así generando varias regresiones por sucursal, por ejemplo:

``` r
load("C:\\Users\\andrevargas\\Documents\\IMOR\\base_sucs.RData")
summary(fcvsuc312_reg)
```

    ## 
    ## Call:
    ## lm(formula = imor ~ 1 + Var_CT_z * Var_CVen_z, data = fcvsuc312)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0128380 -0.0037067  0.0004523  0.0021379  0.0163007 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         0.017978   0.001614  11.142 4.99e-10 ***
    ## Var_CT_z            0.014092   0.007117   1.980   0.0616 .  
    ## Var_CVen_z          0.003775   0.002052   1.839   0.0808 .  
    ## Var_CT_z:Var_CVen_z 0.017022   0.007440   2.288   0.0332 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.007038 on 20 degrees of freedom
    ## Multiple R-squared:  0.4601, Adjusted R-squared:  0.3791 
    ## F-statistic: 5.681 on 3 and 20 DF,  p-value: 0.005554

Resultados
----------

De los modelos obtenidos se genera una tabla como la presentada abajo, que se envía al área de Riesgos.

<table>
<colgroup>
<col width="11%" />
<col width="11%" />
<col width="23%" />
<col width="22%" />
<col width="30%" />
</colgroup>
<thead>
<tr class="header">
<th><strong>Territorio</strong></th>
<th><strong>Sucursales</strong></th>
<th><strong>Relación Cartera Vencida</strong></th>
<th><strong>Relación Cartera Total</strong></th>
<th><strong>Relación Cartera Vencida y Total</strong></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Poniente</td>
<td>312</td>
<td>OK</td>
<td>OK</td>
<td>No Esperada</td>
</tr>
<tr class="even">
<td>Poniente</td>
<td>313</td>
<td>NA (Esperada)</td>
<td>NA (Esperada)</td>
<td>NA (No esperada)</td>
</tr>
<tr class="odd">
<td>Poniente</td>
<td>321</td>
<td>OK</td>
<td>NA (Esperada)</td>
<td>No Esperada</td>
</tr>
<tr class="even">
<td>Poniente</td>
<td>331</td>
<td>NA (Esperada)</td>
<td>OK</td>
<td>NA (Esperada)</td>
</tr>
<tr class="odd">
<td>Poniente</td>
<td>332</td>
<td>OK</td>
<td>NA (Esperada)</td>
<td>NA (No esperada)</td>
</tr>
<tr class="even">
<td>Poniente</td>
<td>336</td>
<td>NA (Esperada)</td>
<td>OK</td>
<td>NA (Esperada)</td>
</tr>
<tr class="odd">
<td>Poniente</td>
<td>341</td>
<td>NA (Esperada)</td>
<td>OK</td>
<td>NA (No esperada)</td>
</tr>
<tr class="even">
<td>Poniente</td>
<td>342</td>
<td>NA (Esperada)</td>
<td>NA (Esperada)</td>
<td>NA (Esperada)</td>
</tr>
<tr class="odd">
<td>Poniente</td>
<td>343</td>
<td>OK</td>
<td>OK</td>
<td>OK</td>
</tr>
<tr class="even">
<td>Poniente</td>
<td>344</td>
<td>NA (Esperada)</td>
<td>NA (Esperada)</td>
<td>NA (Esperada)</td>
</tr>
<tr class="odd">
<td>Poniente</td>
<td>345</td>
<td>NA (Esperada)</td>
<td>NA (Esperada)</td>
<td>NA (Esperada)</td>
</tr>
<tr class="even">
<td>Poniente</td>
<td>350</td>
<td>NA (Esperada)</td>
<td>OK</td>
<td>NA (Esperada)</td>
</tr>
<tr class="odd">
<td>Poniente</td>
<td>352</td>
<td>OK</td>
<td>OK</td>
<td>OK</td>
</tr>
</tbody>
</table>
