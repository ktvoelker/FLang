
module qsort

val qsort: List Int -> List Int is
  fn of
    Nil -> Nil;
    Cons x xs ->
      case partition (< x) xs of
        Pair ls gs -> ls ++ Cons x gs;
      end;
  end

