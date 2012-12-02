
module ordStuff is rec {

  data Bool {
    False is *;
    True is *;
  }

  val (=): Ordering -> Ordering -> Bool
  is fn {
    if LT LT then True;
    if EQ EQ then True;
    if GT GT then True;
    else False;
  }

  data Ordering;
  data LT <: Ordering is *;
  data EQ <: Ordering is *;
  data GT <: Ordering is *;

  sig Ord is rec {
    type T;
    val compare: T -> T -> Ordering;
  }

  module sort: Ord -> * is
  fn ord -> rec {

    type T is ord.T;

    val qsortList: List T -> List T
    is fn {

      if Nil
      then Nil;

      if Cons x xs
      then case partition ((= EQ) * ord.compare x) xs of {
        if Pair ls gs then qsortList ls ++ Cons x (qsortList gs);
      }

    }

  }

}
