import javafx.print.PageLayout

import scala.annotation.tailrec
// gracz - 0
// komputer - 1

case class Point(x: Int, y: Int, doted : Boolean, player : Int)
{
  def printPoint() = {
    if (doted) {
      if (player == 0){
        print(" o ");
      }else{
          print(" x ");
      }
    }
    else{
      print ("   ");
    }
  }
}

object mainWindow {
  def main(args: Array[String]): Unit = {
    println("Witaj w grze!\nPodaj rozmiar planszy: ")
    val size = scala.io.StdIn.readInt();

    class Board(arg : Array[Array[Point]]) {
      val array = Array.ofDim[Point](size, size);
      for (i <- 0 to size - 1) {
        for (j <- 0 to size - 1) {
          array(i)(j) = arg(i)(j);
        }
      }
    }

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
            board.array(i-1)(j-1).printPoint()
          }
          print("\n")
        }
        for (i <- 10 to size) {
          print(i)
          print(" ")
          for (j <- 1 to size) {
            board.array(i-1)(j-1).printPoint()
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
            board.array(i-1)(j-1).printPoint()
          }
          print("\n")
        }
        }
    }
    import scala.util.Random
    val MainArray = Array.ofDim[Point](size, size);
    for (i <- 0 to size - 1) {
      for (j <- 0 to size - 1) {
        val rand =scala.util.Random;
        MainArray(i)(j) = Point(i, j, false, 2);
      }
    }

    def makemove(board: Board,x:Int, y: Int, player: Int): Board ={
      val newArray = Array.ofDim[Point](size,size);
      for(i<- 0 to x-1){
        for(j<- 0 to size-1){
          newArray(i)(j) = board.array(i)(j)
        }
      }
      for(j<- 0 to y-1){
        newArray(x)(j) = board.array(x)(j)
      }
      newArray(x)(y) = Point(x,y,true,player);
      for(j<- y+1 to size-1){
        newArray(x)(j) = board.array(x)(j)
      }
      for(i<- x+1 to size-1){
        for(j<- 0 to size-1){
          newArray(i)(j) = board.array(i)(j)
        }
      }
      new Board(newArray);
    }

    def addToList(list : List[Point], point : Point): List[Point] ={
      point :: list;
    }

    def isOnList(points: List[Point],point: Point) : Boolean = {
      for(i <- 0 to points.length-1){
        if(points(i).x == point.x && points(i).y == point.y) {
          return true;
        }
      }
      return false;
    }

    def tryToEscape(board: Board,point: Point,player: Int, list: List[Point]):Boolean = {
      if (point.x == size - 1 || point.y == size - 1 || point.x == 0 || point.y == 0) {
        return false; // jesteś odblokowany
      }
      val newList: List[Point] = addToList(list,point)

      val up = !board.array(point.x-1)(point.y).doted || (board.array(point.x-1)(point.y).doted && board.array(point.x-1)(point.y).player==player)
      val down = !board.array(point.x+1)(point.y).doted || (board.array(point.x+1)(point.y).doted && board.array(point.x+1)(point.y).player==player)
      val right = !board.array(point.x)(point.y+1).doted || (board.array(point.x)(point.y+1).doted && board.array(point.x)(point.y+1).player==player)
      val left = !board.array(point.x)(point.y-1).doted || (board.array(point.x)(point.y-1).doted && board.array(point.x)(point.y-1).player==player)

      if(up || down || right || left){
        if(up){
          if(!isOnList(newList,board.array(point.x-1)(point.y))){
            if(!tryToEscape(board,board.array(point.x-1)(point.y),player,newList)){
              return false // jestes odblokowany, bo punkt u gory jest odblokowany
            }
          }
        }
        if(down){
          if(!isOnList(newList,board.array(point.x+1)(point.y))){
            if(!tryToEscape(board,board.array(point.x+1)(point.y),player,newList)){
              return false // jestes odblokowany, bo punkt u gory jest odblokowany
            }
          }
        }
        if(left){
          if(!isOnList(newList,board.array(point.x)(point.y-1))){
            if(!tryToEscape(board,board.array(point.x)(point.y-1),player,newList)){
              return false // jestes odblokowany, bo punkt u gory jest odblokowany
            }
          }
        }
        if(right){
          if(!isOnList(newList,board.array(point.x)(point.y+1))){
            if(!tryToEscape(board,board.array(point.x)(point.y+1),player,newList)){
              return false // jestes odblokowany, bo punkt u gory jest odblokowany
            }
          }
        }
      }
      return true; // JESTES OTOCZONY
    }
    /*
     isSurrounded sprawdza czy podany punkt jest otoczony, jeśli tak to zwraca true
    */
    def isSurrounded(board: Board, point: Point, player: Int): Boolean = {
      val list : List[Point] = Nil;
      val iSSurround = tryToEscape(board, point, player,list); // true jeśli jest otoczony
      if(iSSurround){
       //println("Znalazlem otoczony punkt na tej planszy " + (point.x+1) + " " + (point.y+1));
        true;
      }else{
        false;
      }
    }
    /*
      countPoint liczy punkty podanego w argumencie gracza przy danej planszy
      w liście surroundDots zwraca punkty które są otoczone
      oblicza długość tejże listy (type punktów ma dany gracz)
    */
    def countPoint(board: Board,player: Int): Int ={
      val notThisPlayerDots = for{
        i <- 0 to size-1
        j <- 0 to size-1
        if board.array(i)(j).doted && board.array(i)(j).player != player
      } yield board.array(i)(j);

      if(player == 1) {
        val surroundDots = for (dot <- notThisPlayerDots if isSurrounded(board, dot, 0)) yield dot
        surroundDots.length;
      }
      else{
        val surroundDots = for (dot<-notThisPlayerDots if isSurrounded(board,dot,1)) yield dot
        surroundDots.length;
      }
    }

    def max(a:Int, b:Int): Int ={
      if(a>b) a
      else b
    }
    def min(a:Int, b:Int): Int ={
      if(a<b) a
      else b
    }

    val D = 4; // to jest tak jakby stopień trudności, można zapytać gracza na samym początku

    def findDiffrence(board_1: Board, board_2 : Board):Point = {
      val possibleMoves = for{
        i <- 0 to size-1
        j <- 0 to size-1
        if board_2.array(i)(j).doted && !board_1.array(i)(j).doted
      } yield board_2.array(i)(j);
      possibleMoves.head
    }

    def checkNeighbors(board: Board, point: Point): Boolean = {
      if(point.x == size-1 || point.y == size-1 || point.x == 0 || point.y == 0) {
        return false
      }else{
        if(board.array(point.x+1)(point.y+1).doted)
          return true;
        if(board.array(point.x+1)(point.y).doted)
          return true;
        if(board.array(point.x+1)(point.y-1).doted)
          return true;
        if(board.array(point.x)(point.y-1).doted)
          return true;
        if(board.array(point.x)(point.y+1).doted)
          return true;
        if(board.array(point.x-1)(point.y+1).doted)
          return true;
        if(board.array(point.x-1)(point.y).doted)
          return true;
        if(board.array(point.x-1)(point.y-1).doted)
          return true;
        return false;
      }
    }

    var bestPoint = new Point(0,0,false,0);

    def minmax_minizing(board: Board,depth: Int): Int =
    {
      if(depth == 0){
        val computer = countPoint(board,1);
        val player = countPoint(board,0);
        val result = computer - player;
        //printBoard(board);
        //println("Dla tej planszy mamy punktów: " + computer + " - " + player);
        result;
      }else{
        // obliczamy dzieci dla danego wezla
          val children = for{
            i <- 0 to size-1
            j <- 0 to size-1
            if !board.array(i)(j).doted && checkNeighbors(board,board.array(i)(j))
          } yield makemove(board,i,j,0)
          var bestValue = 100000;
          for(child <- children){
            val v = minmax_maximizing(child,depth-1);
            bestValue = min(bestValue,v);
            if(depth == D && bestValue == v){
              bestPoint = findDiffrence(board,child);
              computerPoints = bestValue;
            }
          }
          bestValue;
      }
    }

    def minmax_maximizing(board: Board,depth: Int): Int =
    {
      if(depth == 0){
        // tutaj jest obliczona róznica pomiędzy punktami komputera i gracza
        val computer = countPoint(board,1);
        val player = countPoint(board,0);
        val result = computer - player;
        //printBoard(board);
        //println("Dla tej planszy mamy punktów: " + computer + " - " + player);
        result;
      }else{
        // obliczamy dzieci dla danego wezla
        val children = for{
          i <- 0 to size-1
          j <- 0 to size-1
          if !board.array(i)(j).doted && checkNeighbors(board,board.array(i)(j))
        } yield makemove(board,i,j,1)
        var bestValue = -100000;
        for(child <- children){
          val v = minmax_minizing(child,depth-1);
          bestValue = max(bestValue,v);
          if(depth == D && bestValue == v){
            bestPoint = findDiffrence(board,child);
          }
        }
        bestValue;
      }
    }

    def findBestMove(board: Board): Point ={
      println("-------------- Daj mi chwilę, niech pomyślę --------------")
      minmax_maximizing(board,D);
      val X = bestPoint.x;
      val Y = bestPoint.y;
      board.array(X)(Y) // bestPoint
    }


    val MainBoard = new Board(MainArray);
    printBoard(MainBoard);

    game(MainBoard)

    def game(board : Board) : Unit ={
      println("\n============== Twoj ruch ==============\nWybierz wiersz : ");
      val x = (scala.io.StdIn.readInt() - 1);
      println("\nWybierz kolumne : ");
      val y = (scala.io.StdIn.readInt() - 1);

      if(board.array(x)(y).doted || x> size || y > size){
        if(x> size || y > size)
          println("\n@@@ To miejsce jest poza zakresem planszy!")
        else
          println("\n@@@ To miejsce jest już zajete!")
        game(board)
      }else {
        val newBoard = makemove(board, x, y, 0)
        printBoard(newBoard)

        println("\n============== Komputer wykonuje ruch ==============\n")
        val pointToMove = findBestMove(newBoard)
        println("Wybieram : x = " + (pointToMove.x + 1) + " y = " + (pointToMove.y + 1) + "\n");

        val newBoard2 = makemove(newBoard, pointToMove.x, pointToMove.y, 1);
        printBoard(newBoard2)
        println("Twoje punkty: " + countPoint(MainBoard,0)+ " Punkty przeciwnika: "+ countPoint(MainBoard,1))
        // przeciwnika w sensie komputera
        game(newBoard2);
      }
    }
  }
}

