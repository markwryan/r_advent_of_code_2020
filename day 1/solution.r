input <- read.csv("input.csv")
# Part 1
for (x in input$entry){
  for(y in input$entry) {
    if(sum(x,y) == 2020) {
      print(x*y)
    }
  }
}

#Part 2
for (x in input$entry){
  for(y in input$entry) {
    for(z in input$entry)
    if(sum(x,y,z) == 2020) {
      print(x*y*z)
    }
  }
}


