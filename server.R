# server.R Porcion server del calculo del CSR SISALRIL

library(shiny)
require(stats)



shinyServer(function(input, output ) {
    # funcion para hacer los calculos y guardar resultados
    salidax <- reactive(calculaCSR(input$ANO,input$ESQUEMA,input$ARS,input$PERCENTIL,input$MESES,input$PLAN))

# Tabs donde se muestra la salida   
    
       output$Resumen <- renderTable({
           if (input$ARS != "00"){
            salida <- salidax()
               salida[[1]]           
            
       } 
        })
        
        output$Reclamaciones <- renderPlot({
        # grafico de reclamaciones diarias   
            if (input$ARS != "00"){
                salida <- salidax()
                ini <- min(salida[[2]])
                fin <- max(salida[[2]])
            hist(salida[[2]], main="Distribucion de Reclamaciones Diarias", xlab="Reclamaciones Diarias",
                 xlim=c(ini,fin), ylab="Frecuencia de Reclamacioes", breaks=100,right=TRUE, col="red")
            
            }
        })
        output$Montos <- renderPlot({
            # grafico de montos de reclamaciones 
            if (input$ARS != "00"){
                salida <- salidax()
                ini <- min(salida[[3]])
                fin <- max(salida[[3]])
            hist(salida[[3]],main="Distribucion de Monto de Reclamaciones", xlab="Monto Reclamacion", 
                 xlim=c(ini,fin),ylab="Frecuencia", breaks=1000, right=TRUE, col="blue")
            
            }
        })
        # resultados del calculo del CSR
        output$CSR <- renderTable({
            if (input$ARS != "00"){
                salida <- salidax()
                salida[[4]]

            } 
            })   
        
        # FDP Pagos simulada
        
        output$FDPPagos <- renderPlot({
            if (input$ARS != "00"){
                s <- salidax()
                p <- plot(s[[6]], s[[5]], type="l", col="blue",main="Distribucion pagos mensual Acumulada",
                                     xlab="Monto pagado", ylab="Frec. Acumulada", xlim=c(min(s[[8]]), max(s[[8]]))*s[[10]])
                     p <- p + mtext(s[[7]]) + mtext(s[[12]], line = -5, side = 4) + abline(v=s[[11]], lty=2, col="red") + abline(v=s[[9]], lty=2, col="blue")
                     
                p
            }
            })  

       
 
}) # del ShinyServer

calculaCSR <- function(ANO,ESQUEMA,ARS,PERCENTIL,MESES,PLAN){
    
    ano <- as.numeric(ANO)   
    percentil <- as.numeric(PERCENTIL)
    meses <- as.numeric(MESES)
    return(calculos(ano,ESQUEMA,ARS,percentil,meses,PLAN))
    
}

calculos <- function(ano,esquema,ars,percentil,meses,plan){
    # Forma nombre de archivo de datos y completa datos de trabajo
    library(foreign)   ## para leer archivos SPSS
    inicia <- proc.time()      # Momento de inicio del proceso
    datos <- NULL
    
    if ( (meses < 2) | (!is.numeric(meses)) ) { meses <- 2 }

    if (esquema == "07") {  # para esquema 07
        archivo <- paste0("ARS",ars,ano,esquema,".sav")
        data <- read.spss(archivo, use.value.labels = FALSE, use.missings=TRUE)
        datosf1 <- data.frame(data)                                    ## convierte a data frame
        rm(data)
        if (plan == "0") {
            datosf <- subset(datosf1, Plan_Numero == "00000006",
                             select = c(Reclamacion_Monto_Autorizado,Reclamacion_Autorizacion_Fecha)) ## Selecciona solo plan basico
            rm(datosf1)
        }else {
            datosf <- datosf1
            rm(datosf1)
        }
        
        Monto_Pagado_ARS <- datosf$Reclamacion_Monto_Autorizado       ## Valor usado para calculos
        fecha <- as.character(datosf$Reclamacion_Autorizacion_Fecha) ## convierte fecha a caracter
        
    }else {                # para esquema 35
        archivo <- paste0("ARS",ars,ano,esquema,".sav")
        data <- read.spss(archivo, use.value.labels = FALSE, use.missings=TRUE)
        datosf <- data.frame(data)                                    ## convierte a data frame
        rm(data)
        Monto_Pagado_ARS <- datosf$Monto_Pagado       ## Valor usado para calculos
        fecha <- as.character(datosf$Episodio_Autorizacion_Fecha) ## convierte fecha a caracter
        
    }
    rm(datosf)
    Episodio_Fecha_Anio <- substr(fecha,1,4)
    Episodio_Fecha_Mes <- substr(fecha,5,6)
    Episodio_Fecha_Dia <- substr(fecha,7,8)
    datos <- data.frame(Monto_Pagado_ARS,Episodio_Fecha_Anio,Episodio_Fecha_Mes,Episodio_Fecha_Dia)   
    
    # lee el archivo de gastos de salud y codigos de ARS
    arsno <- ars
    CODARS <- read.csv("COD-ARS.csv", sep =",", header = TRUE, , as.is=TRUE, na.strings = "NA")
    arscod <- subset(CODARS, codigo == as.numeric(arsno), select = c(codigo, nombre,gasto_salud))
    ARS <- arscod$nombre
    gasto <- arscod$gasto_salud
    
    # prepara resumen de datos para el archivo completo
    casosa <- nrow(datos)                      ## Cantidad de episodios
    r <- subset(datos, Monto_Pagado_ARS > 0 , select = c(Episodio_Fecha_Mes,Episodio_Fecha_Dia, Monto_Pagado_ARS))
    summontoa <- sum(r$Monto_Pagado_ARS)   ## suma los montos de las reclamaciones del ano
    ma <- mean(r$Monto_Pagado_ARS)              ## Media de los pagos
    dea <- sd(r$Monto_Pagado_ARS)               ## Desviacion Estandar de los pagos
    menorreca <- min(r$Monto_Pagado_ARS)    ## monto de la reclamacion mas baja
    mayorreca <- max(r$Monto_Pagado_ARS)        ## monto de la reclamacion mas alta   
    
    # prepara resumen de datos para el archivo sin outliers
    mmax <- quantile(r$Monto_Pagado_ARS, percentil/100)  ## Monto maximo de reclamacion a incluir en analisis
    datos <- subset(r, Monto_Pagado_ARS <= mmax) 
    casosb <- nrow(datos)                      ## cantidad de episodios
    summontod <- sum(datos$Monto_Pagado_ARS)   ## suma de los montos de las reclamaciones
    m <- mean(datos$Monto_Pagado_ARS)          ## Media de los pagos
    de <- sd(datos$Monto_Pagado_ARS)           ## Desviacion Estandar de los pagos
    mayorrec <- max(datos$Monto_Pagado_ARS)    ## monto de la reclamacion mas baja
    menorrec <- min(datos$Monto_Pagado_ARS)    ## monto de la reclamacion mas alta
    
    c1 <- formatC(casosa, format="f", big.mark=',',digits=0)
    c2 <- formatC(summontoa, format="f", big.mark=',',digits=2)
    c3 <- formatC(ma, format="f", big.mark=',',digits=2)
    c4 <- formatC(dea, format="f", big.mark=',',digits=2)
    c5 <- formatC(menorreca, format="f", big.mark=',',digits=2)
    c6 <- formatC(mayorreca, format="f", big.mark=',',digits=2)
    c7 <- formatC(gasto, format="f", big.mark=',',digits=2)
    c11 <- formatC(casosb, format="f", big.mark=',',digits=0)
    c21 <- formatC(summontod, format="f", big.mark=',',digits=2)
    c31 <- formatC(m, format="f", big.mark=',',digits=2)
    c41 <- formatC(de, format="f", big.mark=',',digits=2)
    c51 <- formatC(menorrec, format="f", big.mark=',',digits=2)
    c61 <- formatC(mayorrec, format="f", big.mark=',',digits=2)
    c71 <- c(" ")
   
    reclad <- c(table(datos$Episodio_Fecha_Mes,datos$Episodio_Fecha_Dia))  # Construye vector con cantidad rec. diarias
    recladia <- ifelse(reclad > 0, reclad, NA)                             # cambia Ceros con NA
    recladia <- recladia[complete.cases(recladia)]                         # Extrae los NA
    
    mediarec <- mean(recladia)    # Media de las reclamaciones diarias
    derec <- sd(recladia)         # Desviacion estandar de las rec. diarias
    maxrec <- max(recladia)       # Maximo de las rec. diarias
    minrec <- min(recladia)       # Minimo de las rec. diarias
    salida2 <- recladia
    
    c8 <- formatC(max(recladia), format="f", big.mark=',',digits=0)
    c81 <- c(" ")    
        
    resumen <- matrix(c(c1,c2,c3,c4,c5,c6,c7,c8,c11,c21,c31,c41,c51,c61,c71,c81),ncol=2,byrow=FALSE)
    salida1 <- as.table(resumen) 
    colnames(salida1) <- c("Todos los Registros","Sin Registros Atipicos")
    rownames(salida1) <- c("Cantidad de Registros","Monto Total","Media del monto",
                           "Desv. Est. del monto","Mínimo", "Máximo","Costos de Salud","Máximo Rec/dia")
    
    # construye grafica FDP de los episodios diarios
    
        mon_histo <- hist(datos$Monto_Pagado_ARS,main="Distribucion de Monto de Reclamaciones",
                      xlab="Monto Reclamacion", ylab="Frecuencia", 
                      breaks=1000, right=TRUE, col="blue")    
    
    # CONSTRUYE GRAFICA FDP DE LOS MONTOS DE LAS RECLAMACIONES
    
    salida3 <- datos$Monto_Pagado_ARS    
    rec_histo <- hist(recladia, main="Distribucion de Reclamaciones Diarias",
                      xlab="Reclamaciones Diarias", ylab="Frecuencia de Reclamacioes",
                      breaks=100, right=TRUE, col="red")
    
    # CALCULA EL CSR PARA VARIOS VALORES DE VaR
    # Construye vector de probabilidades de montos
    monto_posible <- c(mon_histo$mids)
    monto_prob <- c(mon_histo$counts)/casosb
    
    # Construye vector de probabilidades de cantidad de reclamaciones
    rec_posible <- c(rec_histo$mids)
    rec_prob <- c(rec_histo$counts)/casosb
    rm(datos)
    rm(r)       ## borra estas data frame para ahorrar memoria

    set.seed(1, kind = NULL, normal.kind = NULL) # fija generador de numeros aleatorios
    suma <- c()
    for (l in 1:meses){ # Cantidad de meses a simular
        total_mes <- 0
        for ( i in 1:30){      # Simula 30 dias
            nr <- sample(rec_posible,1,replace=TRUE, rec_prob) # simula no. de recl. del dia
            mr <- sample(monto_posible,nr,replace=TRUE, monto_prob) # simula monto reclamacion
            total_dia <- sum(mr)                   # suma los montos de las reclamaciones de dia
            total_mes <- total_mes + total_dia     # acumula los montos diarios 
        }    
        suma <- c(suma,total_mes)                  # crea vector con los montos de cada mes
    }

    sumamedia <- mean(suma)
    proporcion <- 1              # ajuste por costo de salud, ahora no se usa
    sumamedia <- sumamedia*proporcion
    # calcula los diferentes cuartiles del VaR
    Var <- quantile(suma, c(0.5,0.8,0.85,0.90,0.95,0.995))*proporcion
    csr <- Var - sumamedia
    # muestra los diferentes CSR dependiendo del VaR 
    resumen <- matrix(c(formatC(Var, format="f", big.mark=',',digits=2),
                        formatC(sumamedia, format="f", big.mark=',',digits=2),
                        formatC(csr, format="f", big.mark=',',digits=2)," "), ncol=2,byrow=FALSE)
    salida4 <- as.table(resumen) 
    colnames(salida4) <- c("       V A R","     C S R")
    rownames(salida4) <- c("VaR al 50%","VaR al 80%","VaR al 85%","VaR al 90%","VaR al 95%", "Var al 99.5%","Promedio simulado")
    
    pagosc <- quantile(suma,0.995)*proporcion   
    ifelse(meses<30,clases <- 30, clases <- meses/5)
    # HACE GRAFICA DE PAGOS ACUMULADA CON LOS RESULTADOS DE LA SIMULACION
    suma_histo <-  hist(suma*proporcion,main="Distribucion de pagos mensual", breaks=clases,
                        xlab="Monto pagado", plot=TRUE, axes = TRUE ,xlim=c(min(suma), max(suma))*proporcion)
    
    cumfreq <- c(cumsum(suma_histo$counts)/meses)
    s5 <- cumfreq
    s6 <- suma_histo$mids
    
    duracion <- proc.time() - inicia
    m <- matrix(duracion)
    min <- formatC(m[3]/60, format="f", big.mark=',',digits=2)     # Duracion del proceso en minutos

    
    ## SALIDA DE RESULTADOS
    s12 <- paste0(meses," Simulaciones en ",min," Minutos")
    valor <- list(salida1,salida2,salida3,salida4,s5,s6,ARS,suma,sumamedia,proporcion,pagosc,s12)
    return(valor)
    
} # termina calculos