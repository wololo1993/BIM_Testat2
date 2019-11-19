# Dynamische Programmierung mit R

#' Dynamische Programmierung (2D)
#'
#' @param n Anzahl Zeilen der Matrix oder eine Liste mit
#' Zeilennamen, aus denen n ermittelt wird.
#' @param m Anzahl Spalten der Matrix oder eine Liste mit
#' Spaltennamen, aus denen m ermittelt wird.
#' @param value Funktion, die einen Wert ermittelt. Parameter
#' sind i, j, d.
#' @param prevstates Funktion, die den Vorgängerzustand ermittelt. 
#' Parameter sind i, j, d.
#' @param decicions Funktion, die alle erlaubten Zustände angibt. 
#' Parameter sind i, j.
#' @param optimal Funktion, die den optimalen Wert aus einer Liste 
#' von Zahlen ermitelt. Standard ist max().
#' @param acc Funktion, die den aktuellen Wert v und die bisherigen
#' akkumulierten Wert a verbindet. Standard ist f(v, a) = v + a.
#' @param a0 Initialwert für akkumulierte Werte. Standard ist 0.
#' @param stateorder Funktion, die für alle Zellen, nummeriert durch
#' k von 1 bis n * m, die aktuelle Zelle (i, j) berechnet. Dadurch
#' wird die Reihenfolge der Zustandsberechnung festgelegt.
#' Standard ist dynpro.stateorder.ulbr(), die Indexpositionen zeilenweise
#' von oben links nach unten rechts berechnet.
#'
#' @return Liste mit zwei Elementen: A ist die Matrix der 
#' akkumlierten Werte, D die die Matrix mit den getroffenen 
#' Entscheidungen.
#' @export
dynpro.matrix = function(rows, columns, 
                         value, prevstates, decisions, 
                         optimal = function(v) max(v),
                         acc = function(v, a) v + a,
                         a0 = 0,
                         stateorder = dynpro.stateorder.ulbr) {
  n = length(rows) # Dimension der Matrizen
  m = length(columns)
  A = matrix(0, nrow = n, ncol = m) # Akk. Werte
  rownames(A) = rows; colnames(A) = columns
  D = matrix(0, nrow = n, ncol = m) # Entscheidungen
  rownames(D) = rows; colnames(D) = columns
  
  for (k in 1:(n * m)) { # Durchlaufe alle Zellen...
    idx = stateorder(k, n, m) # und ermittele Zelle
    i = idx[1]; j = idx[2]
    ds = decisions(i, j) # Mögliche Entscheidungen
    v = sapply(ds, function(d) { # Durchlaufe alle Entscheidungen.
      pidx = prevstates(i, j, d) # Vorgängerzustand
      if (length(pidx) == 2) { # Gibt es einen Vorgängerzustand?
        ip = pidx[1]; jp = pidx[2]
        acc(value(i, j, d), A[ip, jp]) # Verbinde Werte (z.B. v + a).
      } else {
        acc(value(i, j, d), a0) # mit Initialwert (z.B. v + 0)
      }
    })
    vo = optimal(v) # Ermittele optimalen Wert.
    A[i, j] = vo
    D[i, j] = ds[which(v == vo)[1]] # Merke (erste) Entscheidung
  }
  list(A = A, D = D)
}

#' Pfad mit optimalen Entscheidungen.
#'
#' @param D Matrix mit getroffenen Entscheidungen
#' @param prevstates Funktion, die den Vorgängerzustand ermittelt. 
#' Parameter sind i, j, d.
#' @param i Ausgangszustand der optimalen Lösung (Zeile)
#' @param j Ausgangszustand der optimalen Lösung (Spalte)
#'
#' @return Liste der optimalen Entscheidungen.
#' @export
dynpro.path = function(D, prevstates, i, j) {
  # Ihre Lösung
  pathrec = function(i, j, p) {
    d = D[i, j] # Entscheidung
    p = c(p, d) # Füge diese zum Pfad hinzu.
    pidx = prevstates(i, j, d) # Vorgängerzustand
    if (length(pidx) == 2) { # Gibt es Vorgängerzustand?
      ip = pidx[1]; jp = pidx[2]
      pathrec(ip, jp, p) # Verfolge Pfad weiter zurück.
    } else {
      p # Anfang erreicht. Rekursionsabbruch.
    }
  }
  
  p = c() # Beginne mit leeren Pfad.
  # -OUT
  # +EXCSUBST 2 c()
  rev(pathrec(i, j, p)) # Ergebnis liegt zunächst rückwärts vor.
  # -EXCSUBST
}

#' Ermittelt die Zelle (i, j) in der Reihenfolge oben links
#' nach rechts unten.
#'
#' @param k Wert zwischen 1 und n * m.
#' @param n Anzahl Zeilen der Matrix.
#' @param m Anzahl Spalten der Matrix.
#'
#' @return
#' @export
dynpro.stateorder.ulbr = function(k, n, m) {
  row = (k - 1) %/% m + 1
  col = (k - 1) %% m + 1
  c(row, col)
}

dynpro.rows = function(m){
  p = c()
  for(i in 1:nrow(m)){
    p = c(p, c(i))
  }
  p
}
dynpro.columns = function(m){
  p = c()
  for(i in 1:ncol(m)){
    p = c(p, c(i))
  }
  p
}
dynpro.value = function(i, j, d){
  dynpro.m[i, j]
}
dynpro.prevstates = function(i, j, d){
  if(d == -1){
    c(0)
  }
  else if (d == 0){
    c(i, j - 1)
  }
  else if (d == 1){
    c(i - 1, j)
  }
}
dynpro.decisions = function(i, j) {
  if(i == 1 && j == 1){
    c(-1)
  }
  else if(i == 1){
    c(0) # nach recht
  }
  else if(j == 1){
    c(1) # nach unten
  }
  else{
    c(0, 1) #beides möglich
  }
}

dynpro.m = matrix(c(1, 2, 1, 4, 3, 2, 5, 1, 3, 3, 3, 2), nrow=3, ncol=4, byrow = TRUE) 
print(dynpro.m)
result = dynpro.matrix(dynpro.rows(dynpro.m), dynpro.columns(dynpro.m), dynpro.value, dynpro.prevstates, dynpro.decisions)
print(result)
print("Maximaler Wert:")
print(result[[1]][nrow(result[[1]]), ncol(result[[1]])])
print("Pfad:")
print(dynpro.path(result[[2]], dynpro.prevstates, nrow(result[[1]]), ncol(result[[1]])))

dynpro.m = matrix(c(1 , 2 , 1 , 4 , 3 , 1 , 3,
                    1 , 2 , 5 , 1 , 2 , 1 , 1,
                    5 , 2 , 3 , 2 , 1 , 4 , 7,
                    3 , 4 , 6 , 2 , 4 , 1 , 5,
                    2 , 7 , 3 , 1 , 4 , 1 , 1,
                    5 , 2 , 3 , 4 , 1 , 4 , 6,
                    1 , 3 , 3 , 1 , 1 , 5 , 5), nrow=7, ncol=7, byrow = TRUE) 
print(dynpro.m)
result = dynpro.matrix(dynpro.rows(dynpro.m), dynpro.columns(dynpro.m), dynpro.value, dynpro.prevstates, dynpro.decisions)
print(result)
print("Maximaler Wert:")
print(result[[1]][nrow(result[[1]]), ncol(result[[1]])])
print("Pfad:")
print(dynpro.path(result[[2]], dynpro.prevstates, nrow(result[[1]]), ncol(result[[1]])))


dynpro.m = matrix(c(7 , 8 , 5 , 3 , 9 , 3 , 9 , 4 , 7 , 3 , 6 , 9 , 3 , 2 , 9,
                    6 , 5 , 7 , 6 , 3 , 5 , 1 , 3 , 6 , 9 , 2 , 7 , 8 , 1 , 9,
                    3 , 3 , 6 , 3 , 6 , 3 , 7 , 3 , 5 , 7 , 3 , 8 , 9 , 3 , 2,
                    4 , 2 , 8 , 2 , 7 , 5 , 4 , 6 , 4 , 7 , 8 , 8 , 9 , 4 , 8,
                    9 , 8 , 8 , 7 , 4 , 7 , 8 , 9 , 8 , 1 , 3 , 5 , 9 , 3 , 9,
                    7 , 6 , 1 , 5 , 2 , 8 , 7 , 1 , 5 , 1 , 1 , 8 , 8 , 4 , 8,
                    4 , 8 , 4 , 4 , 3 , 5 , 7 , 4 , 1 , 1 , 3 , 1 , 6 , 6 , 4,
                    5 , 7 , 7 , 1 , 5 , 5 , 3 , 2 , 8 , 2 , 8 , 7 , 2 , 4 , 7,
                    7 , 3 , 6 , 2 , 5 , 6 , 5 , 6 , 5 , 4 , 1 , 6 , 1 , 6 , 2,
                    5 , 9 , 6 , 5 , 1 , 8 , 3 , 6 , 3 , 2 , 8 , 1 , 6 , 9 , 3,
                    8 , 2 , 9 , 6 , 6 , 7 , 5 , 3 , 6 , 7 , 3 , 1 , 1 , 6 , 3,
                    9 , 3 , 5 , 6 , 8 , 4 , 5 , 9 , 1 , 8 , 2 , 1 , 5 , 7 , 2,
                    6 , 7 , 5 , 9 , 2 , 6 , 8 , 3 , 3 , 2 , 7 , 5 , 8 , 6 , 8,
                    9 , 9 , 2 , 7 , 5 , 5 , 3 , 7 , 6 , 4 , 1 , 6 , 2 , 6 , 8,
                    1 , 3 , 1 , 7 , 1 , 6 , 4 , 2 , 5 , 6 , 5 , 6 , 8 , 8 , 6
), nrow=15, ncol=15, byrow = TRUE) 
print(dynpro.m)
result = dynpro.matrix(dynpro.rows(dynpro.m), dynpro.columns(dynpro.m), dynpro.value, dynpro.prevstates, dynpro.decisions)
print(result)
print("Maximaler Wert:")
print(result[[1]][nrow(result[[1]]), ncol(result[[1]])])
print("Pfad:")
print(dynpro.path(result[[2]], dynpro.prevstates, nrow(result[[1]]), ncol(result[[1]])))

