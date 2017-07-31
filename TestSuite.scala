import Solution._
class TestSuite extends org.scalatest.FunSuite{
val fromCS121_1 =
"85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58"
val solvedStr = "..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3.."
	// def unparse (): String = {
	// 	1.to(9).map(row => 1.to(9).map (col => (row,col))).flatten
	// 	.map (coord => if size ==1 print the num else print . if size == empty ).mkString
	// }
	test ("append"){
	//	println (parse(fromCS121_1))
		//println (peers(4,2))
		assert(parse(fromCS121_1).solve()== None)
		//assert(parse(solvedStr) == new Board(Map()))
		//assert(parse("85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58") == new Board(Map()))

	}
}