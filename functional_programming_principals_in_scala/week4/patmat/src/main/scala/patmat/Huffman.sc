import patmat.{CodeTree, Fork, Leaf}

import scala.annotation.tailrec

object HuffmanTest {

  def weight(tree: CodeTree): Int = tree match {
    case Leaf(_, w) => w
    case Fork(l, r, _, _) => weight(l) + weight(r)
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(c, _) => List(c)
    case Fork(_, _, c, _) => c
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    def addToList(pair: (Char, Int), xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
      case List() => List(pair)
      case y :: ys =>
        if (pair._1 == y._1) (y._1, y._2 + pair._2) :: ys
        else y :: addToList(pair, ys)
    }
    chars match {
      case List() => List[(Char, Int)]()
      case x :: xs => addToList((x, 1), times(xs))
    }
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def insert (pair: (Char, Int), xs: List[Leaf]): List[Leaf] = xs match {
      case List() => List(Leaf(pair._1, pair._2))
      case y :: ys =>
        if (pair._2 <= y.weight) Leaf(pair._1, pair._2) :: xs
        else y :: insert (pair, ys)
    }
    freqs match {
      case List() => List[Leaf]()
      case x :: xs => insert(x, makeOrderedLeafList(xs))
    }
  }

  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case List() => false
    case _ :: xs => xs match {
      case List() => true
      case _ => false
    }
  }

  def combine(trees: List[CodeTree]): List[CodeTree] = {
    def insert (tree: CodeTree, xs: List[CodeTree]): List[CodeTree] = xs match {
      case List() => List(tree)
      case y :: ys =>
        if (weight(tree) <= weight(y)) tree :: xs
        else y :: insert(tree, ys)
    }
    if (singleton(trees)) trees
    else trees match {
      case List() => List[CodeTree]()
      case x :: xs => insert(makeCodeTree(x, xs.head), xs.tail)
    }
  }

  @tailrec
  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (done(trees)) trees
    else until(done, merge)(merge(trees))
  }

  def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  // Decode
  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def traverse(subTree: CodeTree, bits: List[Bit]): List[Char] = bits match {
      case x :: xs => subTree match {
        case Leaf(c, _) => c :: traverse(tree, x :: xs)
        case Fork(l, r, _, _) => x match {
          case 0 => traverse(l, xs)
          case 1 => traverse(r, xs)
          case _ => throw new Error("Only values (0, 1) are allowed for type Bit")
        }
        case _ => throw new Error("CodeTree subtype doesn't exist yet")
      }
      case List() => subTree match {
        case Leaf(c, _) => List(c)
        case _ => throw new Error("Ended on invalid CodeTree subtype")
      }
    }
    traverse(tree, bits)
  }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  def decodedSecret: List[Char] = decode(frenchCode, secret)

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    @tailrec
    def traverse(subTree: CodeTree, text: List[Char], acc: List[Bit]): List[Bit] = text match {
      case List() => acc
      case x :: xs => subTree match {
        case Leaf(_, _) => traverse(tree, xs, acc)
        case Fork(l, r, _, _) =>
          if (chars(l).contains(x)) traverse(l, x :: xs, acc :+ 0)
          else if (chars(r).contains(x)) traverse(r, x :: xs, acc :+ 1)
          else throw new Error("Element does not exist in tree")
      }
    }
    traverse(tree, text, List[Bit]())
  }

  type CodeTable = List[(Char, List[Bit])]

  @tailrec
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
    case x :: xs => if (x._1 == char) x._2 else codeBits(xs)(char)
    case List() => List[Bit]()
  }

  def convert(tree: CodeTree): CodeTable = {
    def traverse(subTree: CodeTree, acc: List[Bit]): CodeTable = subTree match {
      case Leaf(c, _) => List((c, acc))
      case Fork(l, r, _, _) => mergeCodeTables(traverse(l, acc :+ 0), traverse(r, acc :+ 1))
    }
    traverse(tree, List[Bit]())
  }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = text match {
      case List() => List[Bit]()
      case x :: xs => codeBits(convert(tree))(x) ::: quickEncode(tree)(xs)
  }

}

import HuffmanTest._
val chars: List[Char] = string2Chars("abaccab")
combine(makeOrderedLeafList(times(chars)))
createCodeTree(chars)

encode(frenchCode)(decodedSecret) == secret
codeBits(convert(frenchCode))('h') == List(0,0,1,1,1,0,1)
quickEncode(frenchCode)(decodedSecret) == secret