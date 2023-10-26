# Set a filename variable
filename = "fu.txt"

# Initialize variables to hold data
fu = []

# Read data from the file using the stats command
stats filename using 1 name "fu"

# Assign the extracted data to a variable
fu = fu_fu

# Plot the data (for example)
plot fu title "Data from File"
