import scala.annotation.tailrec
/**
  * Point - reprezentacja kropki
  * @param x wiersz
  * @param y kolumna
  * @param doted true - pole jest zajęte, false - pole jest wolne
  * @param surrounded 'c' - pole otoczone przez komputer, 'p' - pole otoczone przez gracza, 'n' - pole wolne
  * @param isBox true - pole tworzy bazę
  * @param player 2 - wolne, 1 - komputer, 0 - gracz
  */
case class Point(x: Int, y: Int, doted: Boolean, surrounded: Char, isBox: Boolean, player: Int) {
  /**
    * drukuje odpowiednie sympole na planszy:
    * o - pole zajęte przez gracza
    * x - pole zajęte przez komputer
    * U - baza gracza
    * C - baza komputera
    */
  def printPoint() = {
    if (doted) {
      if (player == 0) {
        if (isBox)
          print(" U ")
        else
          print(" o ")
      } else if (player == 1) {
        if (isBox)
          print(" C ")
        else
          print(" x ")
      }
    }
    else {
      print("   ")
    }
  }
}

object mainWindow {
  def main(args: Array[String]): Unit = {
    println("Witaj w grze!\nPodaj rozmiar planszy: ")
    val size = scala.io.StdIn.readInt()

    /**
      * reprezentuje tablicę gry
      * @param arg macierz obiektów klasy Punkt
      */
    class Board(arg: Array[Array[Point]]) {
      val array = Array.ofDim[Point](size, size)
      for (i <- 0 to size - 1) {
        for (j <- 0 to size - 1) {
          array(i)(j) = arg(i)(j)
        }
      }
    }

    /**
      * drukuje tablicę
      * @param board tablica
      */
    def printBoard(board: Board): Unit = {
      print("   ")
      if (size > 9) {
        for (i <- 1 to 9) print(" " + i + " ")
        for (i <- 10 to size) print("" + i + " ")

        print("\n")
        for (i <- 1 to 9) {
          print(" " + i)
          print(" ")
          for (j <- 1 to size) {
            board.array(i - 1)(j - 1).printPoint()
          }
          print("\n")
        }
        for (i <- 10 to size) {
          print(i)
          print(" ")
          for (j <- 1 to size) {
            board.array(i - 1)(j - 1).printPoint()
          }
          print("\n")
        }
      }
      else {
        for (i <- 1 to size) print(" " + i + " ")
        print("\n")
        for (i <- 1 to size) {
          print(" ")
          print(i)
          print(" ")
          for (j <- 1 to size) {
            board.array(i - 1)(j - 1).printPoint()
          }
          print("\n")
        }
      }
    }

    val MainArray = Array.ofDim[Point](size, size)
    for (i <- 0 to size - 1) {
      for (j <- 0 to size - 1) {
        MainArray(i)(j) = Point(i, j, false, 'n', false, 2)
      }
    }

    /**
      * sprawdza czy punkt tworzy bazę
      * @param point punkt
      * @param board tablica aktualnego stanu gry
      * @return true - punkt tworzy bazę, false - punkt nie tworzy bazy
      */
    def checkBox(point: Point, board: Board): Boolean = {
      if (point.player == 2)
        return false

      val occupant = if (point.player == 1) 'c' else 'p'
      val x = point.x
      val y = point.y

      // sprawdzamy, czy nasi sąsiedzi są przez nas otoczeni, jeśli tak, to tworzymy bazę
      if (x + 1 < size && y + 1 < size) {
        if (board.array(point.x + 1)(point.y + 1).surrounded == occupant)
          return true
      }
      if (x + 1 < size) {
        if (board.array(point.x + 1)(point.y).surrounded == occupant)
          return true
      }
      if (x + 1 < size && y - 1 >= 0) {
        if (board.array(point.x + 1)(point.y - 1).surrounded == occupant)
          return true
      }
      if (y - 1 >= 0) {
        if (board.array(point.x)(point.y - 1).surrounded == occupant)
          return true
      }
      if (y + 1 < size) {
        if (board.array(point.x)(point.y + 1).surrounded == occupant)
          return true
      }
      if (x - 1 >= 0 && y + 1 < size) {
        if (board.array(point.x - 1)(point.y + 1).surrounded == occupant)
          return true
      }
      if (x - 1 >= 0) {
        if (board.array(point.x - 1)(point.y).surrounded == occupant)
          return true
      }
      if (x - 1 >= 0 && y - 1 >= 0) {
        if (board.array(point.x - 1)(point.y - 1).surrounded == occupant)
          return true
      }
      return false

    }

    /**
      * optymalizuje wybór ruchów, wybiera ruchy w których komputer ma co najmniej jednego nieotoczonego sąsiada
      * @param board aktualna tablica stanu gry
      * @param point punkt
      * @return true - wybrany punkt posiada przynajmniej jednego nieotoczonego sąsiada, false - wybrany punkt nie posiada ani jednego nieotoczonego sąsiada
      */
    def checkNeighbors(board: Board, point: Point): Boolean = {
      val x = point.x
      val y = point.y
      if (x + 1 < size && y + 1 < size) {
        if (board.array(point.x + 1)(point.y + 1).doted && board.array(point.x + 1)(point.y + 1).surrounded == 'n')
          return true
      }
      if (x + 1 < size) {
        if (board.array(point.x + 1)(point.y).doted && board.array(point.x + 1)(point.y).surrounded == 'n')
          return true
      }
      if (x + 1 < size && y - 1 >= 0) {
        if (board.array(point.x + 1)(point.y - 1).doted && board.array(point.x + 1)(point.y - 1).surrounded == 'n')
          return true
      }
      if (y - 1 >= 0) {
        if (board.array(point.x)(point.y - 1).doted && board.array(point.x)(point.y - 1).surrounded == 'n')
          return true
      }
      if (y + 1 < size) {
        if (board.array(point.x)(point.y + 1).doted && board.array(point.x)(point.y + 1).surrounded == 'm')
          return true
      }
      if (x - 1 >= 0 && y + 1 < size) {
        if (board.array(point.x - 1)(point.y + 1).doted && board.array(point.x - 1)(point.y + 1).surrounded == 'n')
          return true
      }
      if (x - 1 >= 0) {
        if (board.array(point.x - 1)(point.y).doted && board.array(point.x - 1)(point.y).surrounded == 'n')
          return true
      }
      if (x - 1 >= 0 && y - 1 >= 0) {
        if (board.array(point.x - 1)(point.y - 1).doted && board.array(point.x - 1)(point.y - 1).surrounded == 'n')
          return true
      }
      return false
    }

    /**
      * zwraca nową tablicę stanu gry z wstawionym nowym punktem oraz zaktualizowanymi flagami doted, occupied, isBox
      * @param board tablica stanu gry
      * @param x wiersz nowego punktu
      * @param y kolumna nowego punktu
      * @param player gracz, który stawia nowy punkt
      * @return zaktualizowana tablica
      */
    def makemove(board: Board, x: Int, y: Int, player: Int): Board = {
      val newArray = Array.ofDim[Point](size, size);
      for (i <- 0 to x - 1) {
        for (j <- 0 to size - 1) {
          newArray(i)(j) = board.array(i)(j)
        }
      }
      for (j <- 0 to y - 1) {
        newArray(x)(j) = board.array(x)(j)
      }
      newArray(x)(y) = Point(x, y, true, 'n', false, player)
      for (j <- y + 1 to size - 1) {
        newArray(x)(j) = board.array(x)(j)
      }
      for (i <- x + 1 to size - 1) {
        for (j <- 0 to size - 1) {
          newArray(i)(j) = board.array(i)(j)
        }
      }
      val newBoard = setSurrounded(new Board(newArray))
      val checkBoxBoard = setBox(newBoard)
      return checkBoxBoard
    }

    /**
      * dodaje element do listy
      * @param list lista elementów
      * @param elem element
      * @return nowa lista z dodanym elementem
      */
    def addToList[A](list: List[A], elem: A): List[A] = {
      elem :: list;
    }

    /**
      * sorawdza, czy podany punkt jest na liście
      * @param points lista punktów
      * @param point punkt
      * @return true - jest na liście, false - nie ma na liście
      */
    def isOnList(points: List[Point], point: Point): Boolean = {
      for (i <- 0 to points.length - 1) {
        if (points(i).x == point.x && points(i).y == point.y) {
          return true
        }
      }
      return false
    }

    /**
      * próbuje udowodnić, że pusty punkt jest otoczony
      * @param board tablica stanu gry
      * @param point wybrany punkt
      * @param list lista, która przechowuje sprawdzone już punkty
      * @return true - punkt jest otoczony, false - punkt nie jest otoczony
      */
    def tryToEscapeEmpty(board: Board, point: Point, list: List[Point]): (Boolean, Int) = {
      if (tryToEscape(board, point, 0, list))
        return (true, 1)
      else if (tryToEscape(board, point, 1, list))
        return (true, 0)
      else
        return (false, -1)
    }

    /**
      * próbuje udowodnić, że kropka gracza lub komputera jest otoczona
      *
      * @param board tablica stanu gry
      * @param point wybrany punkt
      * @param player gracz, dla którego sprawdzamy (przydatne w prztpadku sprawdzania pustych pól)
      * @param list  lista, która przechowuje sprawdzone już punkty
      * @return true - punkt jest otoczony, false - punkt nie jest otoczony
      */
    def tryToEscape(board: Board, point: Point, player: Int, list: List[Point]): Boolean = {
      if (point.x == size - 1 || point.y == size - 1 || point.x == 0 || point.y == 0) {
        return false; // jesteś odblokowany
      }
      val opponent = if (player == 1) 0 else 1
      val newList: List[Point] = addToList(list, point)

      val up = !(board.array(point.x - 1)(point.y).player == opponent) || ((board.array(point.x - 1)(point.y).player == opponent) && board.array(point.x - 1)(point.y).surrounded != 'n')
      val down = !(board.array(point.x + 1)(point.y).player == opponent) || ((board.array(point.x + 1)(point.y).player == opponent) && board.array(point.x + 1)(point.y).surrounded != 'n')
      val right = !(board.array(point.x)(point.y + 1).player == opponent) || ((board.array(point.x)(point.y + 1).player == opponent) && board.array(point.x)(point.y + 1).surrounded != 'n')
      val left = !(board.array(point.x)(point.y - 1).player == opponent) || ((board.array(point.x)(point.y - 1).player == opponent) && board.array(point.x)(point.y - 1).surrounded != 'n')

      if (up || down || right || left) {
        if (up) {
          if (!isOnList(newList, board.array(point.x - 1)(point.y))) {
            if (!tryToEscape(board, board.array(point.x - 1)(point.y), player, newList))
              return false // jestes odblokowany, bo punkt u gory jest odblokowany
          }
        }
        if (down) {
          if (!isOnList(newList, board.array(point.x + 1)(point.y))) {
            if (!tryToEscape(board, board.array(point.x + 1)(point.y), player, newList))
              return false // jestes odblokowany, bo punkt na dole jest odblokowany
          }
        }
        if (left) {
          if (!isOnList(newList, board.array(point.x)(point.y - 1))) {
            if (!tryToEscape(board, board.array(point.x)(point.y - 1), player, newList))
              return false // jestes odblokowany, bo punkt na lewo jest odblokowany
          }
        }
        if (right) {
          if (!isOnList(newList, board.array(point.x)(point.y + 1))) {
            if (!tryToEscape(board, board.array(point.x)(point.y + 1), player, newList))
              return false // jestes odblokowany, bo punkt na prawo jest odblokowany
          }
        }
      }
      return true; // JESTES OTOCZONY
    }


    /**
      * sprawdza czy dany punkt jest otoczony
      * @param board aktualny stan gry
      * @param point sprawdzany punkt
      * @param player gracz, dla którego sprawdzamy (przydatne w przypadku sprawdzania pustych pól)
      * @return
      */
    def isSurrounded(board: Board, point: Point, player: Int): Int = {
      val list: List[Point] = Nil
      if (player == 2) {
        val surroundyBy = tryToEscapeEmpty(board, point, list)._2
        return surroundyBy
      }
      else {
        if (tryToEscape(board, point, player, list)) {
          val surroundBy = if (player == 1) 0 else 1
          return surroundBy
        }
        else
          return -1
      }
    }

    /**
      * zwraca nową tablicę z aktualnymi flagami surrounded
      * @param board tablica stanu gry
      * @return zaktualizowana tablica stanu gry
      */
    def setSurrounded(board: Board): Board = {
      val array = Array.ofDim[Point](size, size);
      for (i <- 0 to size - 1) {
        for (j <- 0 to size - 1) {
          val point = board.array(i)(j)
          val player = point.player
          val doted = point.doted
          val isBox = point.isBox
          val occupiedBy = isSurrounded(board, point, player)
          array(i)(j) = if (occupiedBy != -1) {
            val occupied = if (occupiedBy == 0) 'p' else 'c'
            Point(i, j, doted, occupied, isBox, player)
          } else Point(i, j, doted, 'n', isBox, player)
        }
      }
      return new Board(array)
    }

    /**
      * oznacza flagi isBox, aby wydrukować punkty tworzące bazę z wielkich liter
      * @param board stan gry
      * @return aktualny stan gry
      */
    def setBox(board: Board): Board = {
      val array = Array.ofDim[Point](size, size);
      for (i <- 0 to size - 1) {
        for (j <- 0 to size - 1) {
          if (checkBox(board.array(i)(j), board)) {
            val player = board.array(i)(j).player
            array(i)(j) = Point(i, j, true, 'n', true, player)
          }
          else array(i)(j) = board.array(i)(j)
        }
      }
      new Board(array)
    }

    /**
      * zlicza otoczone punkty przeciwnika
      * @param board aktualny stan gry
      * @param player gracz, dla którego liczymy punkty
      * @return punkty gracza
      */

    def countPoint(board: Board, player: Int): Int = {
      val opponent = if (player == 1) 0 else 1
      val notThisPlayerDots = for {
        i <- 0 to size - 1
        j <- 0 to size - 1
        if board.array(i)(j).player == opponent
      } yield board.array(i)(j);

      val surroundDots = for (dot <- notThisPlayerDots if isSurrounded(board, dot, opponent) == player) yield dot
      surroundDots.length;
    }

    def max(a: Int, b: Int): Int = {
      if (a > b) a
      else b
    }

    def min(a: Int, b: Int): Int = {
      if (a < b) a
      else b
    }

    val D = 4;
    /**
      * znajduje ostatnio postawiony punkt
      * @param board_1 starsza tablica
      * @param board_2 młodsza tablica
      * @return punkt różniący starszą i młodszą tablicę
      */
    def findDiffrence(board_1: Board, board_2: Board): Point = {
      val possibleMoves = for {
        i <- 0 to size - 1
        j <- 0 to size - 1
        if board_2.array(i)(j).doted && !board_1.array(i)(j).doted
      } yield board_2.array(i)(j);
      possibleMoves.head
    }

    var bestPoint = new Point(0, 0, false, 'n', false, 0);
    /**
      * min_max dla gracza min, aktualnie używany algorytm alfabeta
      * @param board aktualny stan gry
      * @param depth głębokość
      * @return ocena tablicy stanu gry
      */
    def minmax_minizing(board: Board, depth: Int): Int = {
      val children = findMoves(board, 0)
      if (depth == 0 || children.isEmpty) {
        return rateBoard(board)
      } else {
        var bestValue = 100000
        for (child <- children) {
          val v = minmax_maximizing(child, depth - 1);
          bestValue = min(bestValue, v);
        }
        bestValue;
      }
    }
    /**
      * min_max dla gracza max, aktualnie używany algorytm alfabeta
      * @param board aktualny stan gry
      * @param depth głębokość
      * @return ocena tablicy stanu gry
      */
    def minmax_maximizing(board: Board, depth: Int): Int = {
      val children = findMoves(board, 1)
      if (depth == 0 || children.isEmpty) {
        return rateBoard(board)
      } else {
        var bestValue = -100000;
        for (child <- children) {
          val v = minmax_minizing(child, depth - 1);
          bestValue = max(bestValue, v);
          if (bestValue == v && depth == D) {
            bestPoint = findDiffrence(board, child);
          }
        }
        bestValue;
      }
    }

    /**
      * wyszukuje możliwe i najkorzystniejsze ruchy
      * @param board aktualna tablica stanu gry
      * @param player gracz, dla którego znajdujemy możliwe ruchy
      * @return sekwencja możliwych ruchów
      */
    def findMoves(board: Board, player: Int): IndexedSeq[Board] = {
      val children = for {
        i <- 0 to size - 1
        j <- 0 to size - 1
        if !board.array(i)(j).doted && board.array(i)(j).surrounded == 'n' && checkNeighbors(board, board.array(i)(j))
      } yield makemove(board, i, j, player)
      children
    }

    /**
      * ocenia bieżący stan gry
      * @param board tablica stanu gry
      * @return ocena
      */
    def rateBoard(board: Board): Int = {
      val computer = countPoint(board, 1)
      val player = countPoint(board, 0)
      val result = computer - player
      return result
    }

    /**
      * algorytm alfabeta dla gracza min
      * @param board stan gry
      * @param depth głębokość
      * @param alfa najlepsza dotychczasowa wartość dla gracza max
      * @param beta najlepsza dotychczasowa wartość dla gracza min
      * @return ocena stanu gry
      */
    def alfaMin(board: Board, depth: Int, alfa: Int, beta: Int): Int = {
      val children = findMoves(board, 0)
      if (depth == 0 || children.isEmpty) {
        return rateBoard(board)
      } else {
        var newBeta = beta
        for (child <- children) {
          val v = alfaMax(child, depth - 1, alfa, newBeta)
          if (v < newBeta && depth == D)
            bestPoint = findDiffrence(board, child)
          newBeta = math.min(newBeta, v)
          if (alfa >= newBeta) return alfa
        }
        return newBeta
      }
    }

    /**
      * algorytm alfabeta dla gracza max
      * @param board stan gry
      * @param depth głębokość
      * @param alfa najlepsza dotychczasowa wartość dla gracza max
      * @param beta najlepsza dotychczasowa wartość dla gracza min
      * @return ocena stanu gry
      */
    def alfaMax(board: Board, depth: Int, alfa: Int, beta: Int): Int = {
      val children = findMoves(board, 1)
      if (depth == 0 || children.isEmpty) {
        return rateBoard(board)
      } else {
        var newAlfa = alfa
        for (child <- children) {
          val v = alfaMin(child, depth - 1, newAlfa, beta)
          if (v > newAlfa && depth == D)
            bestPoint = findDiffrence(board, child)
          newAlfa = math.max(newAlfa, v)
          if (newAlfa >= beta) return beta
        }
        return newAlfa
      }
    }

    /**
      * znajduje najlepszy możliwy ruch
      * @param board stan gry
      * @return najlepszy możliwy ruch
      */
    def findBestMove(board: Board): Point = {
      println("-------------- Daj mi chwilę, niech pomyślę --------------")
      alfaMax(board, D, -1000, 1000);
      val X = bestPoint.x;
      val Y = bestPoint.y;
      board.array(X)(Y) // bestPoint
    }

    /**
      * zwraca liczbę zajętych miejsc
      * @param board stan gry
      * @return liczba zajętych miejsc
      */
    def countDoted(board: Board): Int = {
      val children = for {
        i <- 0 to size - 1
        j <- 0 to size - 1
        if board.array(i)(j).doted || board.array(i)(j).surrounded != 'n' && !board.array(i)(j).doted
      } yield board.array(i)(j)
      children.length
    }

    /**
      * obsługa stanu, gdy nie można już nigdzie postawić kropki
      * @param board aktualny stan gry
      * @return true - koniec gry, false - gra trwa
      */
    def GameOver(board: Board): Boolean = {
      val doted = countDoted(board)
      if (doted == size * size) {
        val computerPoints = countPoint(board, 1)
        val gamerPoints = countPoint(board, 0)
        val winner = if (computerPoints > gamerPoints) "komputer"
        else if (computerPoints < gamerPoints) "gracz" else "remis"
        if (winner == "remis")
          println("\nRemis")
        else println("\nGra skończona, wygrywa " + winner)
        return true
      }
      else return false
    }

    /**
      * obsługuje wczytywanie pozycji z konsoli
      * @param message wyświetlana wiadomość
      * @return wczytana pozycja
      */
    def readPosition(message: String): Int = {
      println(message);
      val x = (scala.io.StdIn.readLine())
      while (x.equals("") || x.toInt >= size){
        println("Zła wartość " + message)
        val x = (scala.io.StdIn.readLine())
      }
      return x.toInt
    }

    val MainBoard = new Board(MainArray);
    printBoard(MainBoard);

    game(MainBoard)

    /**
      * prowadzi grę
      * @param board aktualny stan gry
      */
    def game(board: Board): Unit = {
      println("\n============== Twoj ruch ==============");
      val x = readPosition("\nWybierz wiersz : ")
      val y = readPosition("\nWybierz kolumne : ")

      if (board.array(x)(y).doted || x > size || y > size || board.array(x)(y).surrounded != 'n') {
        if (x > size || y > size)
          println("\n@@@ To miejsce jest poza zakresem planszy!")
        else if (board.array(x)(y).surrounded != 'n')
          println("\n@@@ To miejsce jest niedozwolone, baza!")
        else
          println("\n@@@ To miejsce jest już zajete!")

        game(board)
      } else {
        val newBoard = makemove(board, x, y, 0)
        printBoard(newBoard)
        if (!GameOver(newBoard)) {
          println("\n============== Komputer wykonuje ruch ==============\n")
          val pointToMove = findBestMove(newBoard)
          println("Wybieram : x = " + (pointToMove.x + 1) + " y = " + (pointToMove.y + 1) + "\n");

          val newBoard2 = makemove(newBoard, pointToMove.x, pointToMove.y, 1);
          printBoard(newBoard2)
          println("\nTwoje punkty: " + countPoint(newBoard2, 0) + " Punkty przeciwnika: " + countPoint(newBoard2, 1))
          if (!GameOver(newBoard2))
            game(newBoard2);
        }
      }
    }
  }
}