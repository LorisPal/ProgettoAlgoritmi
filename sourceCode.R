graficoRc <-
  setRefClass(
    "graficoRc",
    fields = list(nodo = "vector", arco = "matrix", trovato="logical"),
    methods = list(
      initialize = function() {
        nodo <<- 0
        arco <<- matrix(0,1,1)
      },
      
      searchNode = function(node) {
        trovato <<- FALSE
        for (i in 1:length(nodo)) {
            if(node == nodo[[i]]) {
              trovato <<- TRUE
              break;
            }
          }
        if(trovato) {print("Nodo trovato: non aggiunto");}
        else {print("Nodo non trovato");}
      },
      
      addNode = function(x) {
        searchNode(x)
        if(!trovato) {nodo <<- c(nodo, x);}
      }
    )
  )