# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL_RP",
  modelDescr      = "Simple MNL model on mode choice RP data",
  indivID         = "PERSID", 
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
### the code would be: database = read.csv("data.csv",header=TRUE)
data = read.csv("F:/more work/the netherlands/4 wave/code data/data/fianldataformodel.csv",header=TRUE)
database <- data[order(data$PERSID), ]
### for data dictionary, use ?apollo_modeChoiceData

### Use only RP data
#database = subset(database,database$RP==1)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(ASC_multi   = 0,
              ASC_nmulti   = 0,
              DAGSOORT_m = 0,
              REISDUUR_m = 0
              
              )

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("ASC_nmulti")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["Multimodal"]]  = ASC_multi + DAGSOORT_m*DAGSOORT + REISDUUR_m*REISDUUR
  V[["NotMultimodal"]]  = ASC_nmulti

  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(Multimodal=1, NotMultimodal=2), 
    avail         = list(Multimodal=multi1, NotMultimodal=nmulti2), 
    choiceVar     = multimodal,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)