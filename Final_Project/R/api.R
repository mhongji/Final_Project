# R/api.R

# load everything your model needs
library(plumber)
library(readr)
library(dplyr)
library(caret)       # <— caret or tidymodels, whichever you used
library(yardstick)   # if your model uses yardstick metrics internally
library(randomForest) # if your RF comes from randomForest
library(rpart)       # if your tree comes from rpart
library(here)

#* @apiTitle Diabetes Prediction API
#* @apiDescription Predict the probability of diabetes using the best model.

# —— 1) Load the trained model ——
# Adjust the path if you saved your model elsewhere
model <- readRDS(here("model", "best_model.rds"))

# —— 2) Load data to compute defaults ——
# We only need this to calculate mean/mode defaults
df <- read_csv(here("data","diabetes_binary_health_indicators_BRFSS2015.csv")) %>%
  mutate(
    Diabetes_binary = factor(Diabetes_binary, levels = c(0,1), labels = c("No","Yes")),
    PhysActivity    = factor(PhysActivity,    levels = c(0,1), labels = c("No","Yes")),
    HighBP          = factor(HighBP,          levels = c(0,1), labels = c("No","Yes")),
    Sex             = factor(Sex,             levels = c(0,1), labels = c("Male","Female"))
  )

default_bmi     <- mean(df$BMI, na.rm = TRUE)
mode_val        <- function(x) names(sort(table(x), decreasing = TRUE))[1]
default_phys    <- mode_val(df$PhysActivity)
default_highbp  <- mode_val(df$HighBP)
default_sex     <- mode_val(df$Sex)

# —— 3) /pred endpoint ——
#* Predict probability of diabetes
#* @param bmi:number       Body‐Mass Index (numeric; default = dataset mean)
#* @param physActivity:string  Regular physical activity: "Yes" or "No"
#* @param highBP:string       High blood pressure: "Yes" or "No"
#* @param sex:string          Sex: "Male" or "Female"
#* @get /pred
function(bmi = default_bmi,
         physActivity = default_phys,
         highBP = default_highbp,
         sex = default_sex) {
  
  newdata <- tibble(
    BMI          = as.numeric(bmi),
    PhysActivity = factor(physActivity, levels = c("No","Yes")),
    HighBP       = factor(highBP,     levels = c("No","Yes")),
    Sex          = factor(sex,        levels = c("Male","Female"))
  )
  
  # predict probability for "Yes"
  prob <- predict(model, newdata, type = "prob")[, "Yes"]
  list(prob_diabetes = prob)
}

# —— 4) /info endpoint ——
#* Return author info and site URL
#* @get /info
function() {
  list(
    author = "Hongjing Mao",
    site   = "https://mhongji.github.io/Final_Project"
  )
}

