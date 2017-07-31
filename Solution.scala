
import hw.sudoku._
object Solution extends SudokuLike{
	type T = Board
	//Exercise 2
	def parse(str: String): Board = {
		val emptyBoard = new Board(0.to(8).map(x => 0.to(8).map(y => (x,y) -> List(1,2,3,4,5,6,7,8,9))).flatten.toMap)
		parseHelper(str.toList, emptyBoard , 0)
	}
	def parseHelper (list1 : List[Char], currBoard: Board, idx: Int) : Board = {
		list1 match {
		case head :: tail => if (head == '.') parseHelper (tail, currBoard, idx+1)
							else parseHelper(tail, currBoard.place(idx / 9, idx % 9, head.asDigit), idx+1)
		case Nil => currBoard
		}
	}
	//Exercise 1
	def peers (row: Int, col: Int): List[(Int, Int)] = 	{
		(0.to(8).toList.map(y => (y, col)) ++ 
		0.to(8).toList.map(x=> (row, x)) ++ 
		((row/3) *3).to(((row/3) *3)+2).toList.map(x => ((col/3) *3).to(((col/3) *3)+2).toList.map (y=> (x,y))).flatten).filter (_ != (row,col)).distinct
	}
}
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {

	//Exercsie 3
	def availableValuesAt(row: Int, col: Int): List[Int] ={
		available.getOrElse((row, col), 1.to(9).toList)
	}
	def valueAt(row: Int, col: Int): Option[Int] = {
		if (available(row,col).size != 1) None
		else Some(available(row,col)(0))
	}
	def isSolved(): Boolean = available.forall (x => x._2.size == 1)
	def isUnsolvable(): Boolean = 
	if (available.forall (x => x._2.size != 0) == true) false
	else true
	//Exercise 2.5
	def place (row: Int, col: Int, value: Int): Board ={
		require(availableValuesAt(row,col).contains(value))
		val avail = available + ((row, col) -> List(value))
		new Board(placeHelper(value, Solution.peers(row, col).toList, avail))
	}
	def placeHelper (value: Int, peers: List[(Int, Int)], currMap: Map[(Int, Int), List[Int]]) : Map[(Int, Int), List[Int]] = {
		peers match {
			case head :: tail => {
				if (currMap(head).contains(value)== false) placeHelper (value, tail, currMap)
				else {
					val avail = currMap + (head -> currMap(head).filter(x=> x!= value))
					if (avail(head).size == 1) placeHelper (value, tail, placeHelper(avail(head)(0), Solution.peers(head._1,head._2), avail))
					else placeHelper(value, tail, avail)
				 }	
			}
			case Nil => currMap
		}
	}
	//Exercise 4
	def nextStates(): List[Board] = {
		if (isUnsolvable()){
			List()
		}
		else {
			val coord = (0.to(8).toList.map(x => 0.to(8).toList.map(y => (x,y)))).flatten
			nextStatesHelper(coord).filterNot(x => x.isUnsolvable())
		}
	}
	def statesHelper ( list : List[Int], coord: (Int,Int)): List[Board] = {
		list match {
			case head :: tail => place(coord._1, coord._2, head) :: statesHelper (tail, coord)
			case Nil => Nil
		}
	}
	def nextStatesHelper (list: List[(Int,Int)]) : List[Board] = {
		list match {
			case head :: tail => {
				val x = available(head)
				if (x.size <= 1) nextStatesHelper(tail)
				else statesHelper (x, head) ::: nextStatesHelper(tail)
			}
			case Nil => Nil
		}
	}
	//Exercise 5
	def solve(): Option[Board] = {
		if (isSolved() == true) Some(this)
		else solveHelper(nextStates())
	}
	def solveHelper (currList: List[Board]): Option[Board] = {
		currList match {
			case head :: tail => {
				if (head.isSolved() == true) Some(head)
				else{ 
					val x = head.solve()
					if (x == None) None
					else if (x.get.isSolved() == true) x
					else solveHelper(tail)
				}
			}
			case Nil => None
		}
	}
	def function(list1:List[Option[Board]]): Option[Board] ={
 		list1 match{
		case head::tail=>
			if (head != None) head
			else function(tail)
		case Nil => None
		}
	}
}