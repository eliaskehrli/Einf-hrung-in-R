# load necessary libraries
library(lavaan)
library(psych)
library(tidySEM)
library(plotly)

# Load data
my.data <- read.csv("/Users/eliaskehrli/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master/Sport/SSR Basismodul/R-Stats/Strukturmodellierungsgleichungen/burnoutsport200.csv")

# Define the SEM model
bo.modell.pfadfehlt <- '
  # Measurement models
  Perfektionismus =~ standards + selbstkritik + alles.nichts
  Anforderungen   =~ menge + zeitdruck
  Overcommitment  =~ aufopferung + nicht.abschalten + gruebeln
  Burnout         =~ erschoepfung + depersonalisation 

  # Structural model
  Overcommitment  ~  d * Perfektionismus + a * Anforderungen
  Burnout         ~  b * Overcommitment 
  Anforderungen   ~~ Perfektionismus  # Correlation between exogenous factors

  # Indirect effects
  perf.oc.bo := d * b
  anf.oc.bo  := a * b
'

# Fit the SEM model
sem.ergebnis <- sem(model = bo.modell.pfadfehlt, data = my.data, estimator = "mlr")

# Prepare data for plotting with tidySEM
sem_data <- prepare_graph(sem.ergebnis)

# Create an interactive plot using plotly
plot <- plot_ly()

# Add nodes
plot <- plot %>%
  add_trace(
    data = sem_data$nodes,
    x = ~x, y = ~y, type = 'scatter', mode = 'markers+text',
    text = ~label, hoverinfo = 'text',
    marker = list(size = 20, color = 'lightblue'),
    showlegend = FALSE
  )

# Add edges
for (i in 1:nrow(sem_data$edges)) {
  # Extract starting and ending node positions
  start_node <- subset(sem_data$nodes, name == sem_data$edges$from[i])
  end_node <- subset(sem_data$nodes, name == sem_data$edges$to[i])
  
  # Add segments to represent edges
  plot <- plot %>%
    add_segments(
      x = start_node$x, xend = end_node$x,
      y = start_node$y, yend = end_node$y,
      line = list(color = ifelse(sem_data$edges$op[i] == "=~", "green", "blue")),
      hoverinfo = 'none'
    )
}

# Display the interactive plot
plot
