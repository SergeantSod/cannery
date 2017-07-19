package object yaml_snippets {
  //TODO We could use cats' ValidatedNel here.
  type ErrorOr[T] = Either[String, T]
}
