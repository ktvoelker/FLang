
module listStuff

abstract data Bool
data False <: Bool is *
data True <: Bool is *

val =: Ordering -> Ordering -> Bool is
  fn of
    LT LT -> True;
    EQ EQ -> True;
    GT GT -> True;
    _ _ -> False;
  end

abstract data Ordering
data LT <: Ordering is *
data EQ <: Ordering is *
data GT <: Ordering is *

sig Ord is
  type T
  val compare: T -> T -> Ordering
end

module sort: Ord -> * is
  fn ord -> rec
    type T = ord.T
    val qsortList: List T -> List T is
      fn of
        Nil -> Nil;
        Cons x xs -> case partition ((= EQ) * ord.compare x) xs of
          Pair ls gs -> qsortList ls ++ Cons x (qsortList gs);
        end;
      end
  end
end
