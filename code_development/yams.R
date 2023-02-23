  yaml::as.yaml("ry", handlers = list(
    "string" = function(x) {"ploop"}
  ))

    logical = function(x) {
      result <- ifelse(x, "TRUE", "FALSE")
      class(result) <- "verbatim"
      return(result)},

file  <- "x.yaml"
file2 <- "xx.yaml"


xx <- read_yaml(file = file, handlers = read_model_handlers())

write_yaml(x =xx, file = file2, handlers = write_model_handlers())


p <- list(123.456)

lol <- list("int#fix" = function(x) {  
                      as.numeric(x) + 1 
                    })


  my.float.handler <- function(x) { as.numeric(x) + 123 }
  yaml::yaml.load("123.456", handlers=list("float#fix"=my.float.handler))



yaml::as.yaml(p, handlers=list(int=my.float.handler))