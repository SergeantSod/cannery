package object cannery {
  //TODO We could use cats' ValidatedNel here.
  type ErrorOr[T] = Either[String, T]
}
