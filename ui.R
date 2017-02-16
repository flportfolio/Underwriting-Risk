# ui.R Porcion ui del calculo del CSR  SISALRIL

#  Programa para el calculo del Capital de Solvencia Requerido de las ARS
#  Por: Felipe Llaugel
#  Fecha: 26/01/2016
#
#  Supuestos:
#  a) Los datos tanto del esquema 07 como del 35 se asumen en formato SPSS y en el mismo folder que los
#  archivos ui.R y server.R
#  b) El formato para el nombre de archivos es: ARS01201407.sav, que significa que es de la ARS 01,
#  año 2014, esquema 07
#  c) Los nombres de los campos de trabajo en los archivos deben serguir las pautas del manual de
#  requerimiento de datos
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 










library(shiny)

shinyUI(pageWithSidebar (

  headerPanel("Superintendencia de Salud y Riesgos Laborales(SISALRIL)"),
  sidebarPanel(fluid = TRUE,
            
      
      selectInput("ARS", " Adm. de Riesgo de Salud:",width=800,selectize = FALSE,
                  c("Seleccione ARS" = "00","ARS CMD" = "01","ARS SALUD SEGURA" = "02","ARS APS" = "03","ARS SIMAG" = "05",
                    "ARS GRUPO MEDICO ASOCIADO" = "06","ARS DR. YUNEN, S. A." = "13",
                    "ARS UNIVERSAL, S.A." = "14","ARS MONUMENTAL" = "18","ARS FUTURO S. A." = "21",
                    "ARS HUMANO" = "23","ARS SEMUNASED" = "34","ARS ASEMAP" = "36","ARS SEMMA" ="42",
                    "ARS RENACER" = "43", "ARS PALIC-SALUD" = "49",
                    "ARS PLAN SALUD" = "50","ARS SENASA " = "52","ARS CONSTITUCION" = "53",
                    "ARS RESERVAS" = "56","ARS METASALUD" = "63", "ARS PN" = "72","ARS FFAA" ="73")),
      selectInput("ANO", " Año:",width=100,selectize = FALSE,
                  c("2014" = "2014","2015" = "2015","2016" = "2016","2017" = "2017","2018" = "2018",
                    "2019" = "2019","2020" = "2020")),
      radioButtons('ESQUEMA', " Esquema: ", c("35"="35","07"="07")),
      numericInput('PERCENTIL',' Percetil para excluir atipicos:',99,min=90, max=100,step=0.5),
      numericInput('MESES', ' Meses a Simular:',36,min=10, max=240, step=10),
      radioButtons('PLAN', " Sólo Plan Básico de Salud: ", c("Si"="0","No"="1")),
 
      submitButton("Calcula CSR")
      
  ),
  mainPanel(with = 8,
      h3("Capital de Solvencia Requerido (CSR)"),
      h5("El CSR se calcula estimando la función de pagos mensuales de la ARS, usando la información de los esquemas del año anterior al actual. El cálculo se hace ajustando la función de pagos mediante simulación montecarlo, usando la función de distribución probabilidades de la cantidad de reclamaciones diarias y la función distribución de probabilidades del monto de cada reclamación. Dependiendo de los parámetros especificados y de la configuración de la máquina, esta operación puede tardar cierto tiempo. "),
      
      tabsetPanel(
          tabPanel("Resumen", tableOutput("Resumen")), 
          tabPanel("FDP Rec.", plotOutput("Reclamaciones")),
          tabPanel("FDP Montos", plotOutput("Montos")), 
          tabPanel("CSR", tableOutput("CSR")), 
          tabPanel("FDPA Pagos", plotOutput("FDPPagos"))
      )
  )
) # cierra ShinyPanel
) # cierra ShinyUI

