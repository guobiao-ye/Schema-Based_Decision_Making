# Create the data frame
painting_data <- data.frame(
  schemaID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
  payoff = c(3, 4, 5, 2, 5, 6, 6, 2, 4, 3, 4, 5, 2, 3, 6),
  author = c("Dali", "Ciurlionis", "Velazquez", "Hokusai", "VanGogh", "Botticelli", 
             "Munkacsy", "Picasso", "DaVinci", "Chagall", "Monet", "Munch", 
             "OKeeffe", "Michelangelo", "Goya"),
  familirarity = c(0.388316151, 0.04467354, 0.079037801, 0.079037801, 0.615120275, 
                   0.219931271, 0.020618557, 0.560137457, 0.536082474, 0.085910653, 
                   0.463917526, 0.195876289, 0.072164948, 0.384879725, 0.161512027),
  new = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
  type = c("same", "same", "same", "replaced", "replaced", "replaced", "same", 
           "same", "replaced", "replaced", "new", "new", "new", "new", "new"),
  mean0 = c(24.40043609, 26.22740557, 38.11269084, 12.30300888, 29.885084, 
            47.54525908, 50.91984497, 22.92546818, 29.58977965, 21.77354907, 
            21.04766581, 26.4393834, 18.27657794, 19.82444596, 49.52130893),
  modvar = c(5.181766043, 11.11918442, 9.358035718, 4.169477774, 10.26733847, 
             18.83542809, 22.17450084, 17.18497774, 10.61967136, 3.720593243, 
             12.20478566, 10.82212773, 14.90293958, 5.696560695, 32.19337558)
)

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Write to CSV file
write.csv(painting_data, "data/painting_schemainfo2.csv", row.names = FALSE)
