import week3.Ordering

val xss = List(List(1, 2, 3), List(1), List(1, 1, 3))

Ordering.sort(xss)(Int => Int)

case class Movie(title: String, rating: Int, duration: Int)

val movies = Seq(
  Movie("Interstellar", 9, 169),
  Movie("Inglourious Basterds", 8, 140),
  Movie("Fight Club", 9, 139),
  Movie("Zodiac", 8, 157)
)

Ordering.sort(movies)(movie => (movie.rating, movie.duration))