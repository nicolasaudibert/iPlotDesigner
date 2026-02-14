# button_observers.R - MAIN MODULE (Simplified)
# Uses core functions for clean, modular button management

# Load core functions first
source("modules/button_observers_core.R", local = TRUE, encoding = "UTF-8")

# Create select all/none observers for all variables
createSelectObservers(1, "var1")  # Variable 1
createSelectObservers(2, "var2")  # Variable 2
createSelectObservers(3, "var3")  # Variable 3

# Create reordering observers for all variables
createReorderingObservers(1, "var1")  # Variable 1
createReorderingObservers(2, "var2")  # Variable 2
createReorderingObservers(3, "var3")  # Variable 3

debugLog("BUTTON_OBSERVERS_MAIN: All button observers loaded with modular architecture", "UI_INIT")
