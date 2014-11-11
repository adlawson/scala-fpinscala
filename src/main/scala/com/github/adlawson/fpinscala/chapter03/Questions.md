### 3.1
> What will be the result of the following match expression?
```scala
val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42 
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101 
}
```

```scala
val x = 3 // case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y, where x == 1 and y == 2
```

### 3.7
> Can `product`, implemented using `foldRight`, immediately halt the recursion
and return `0.0` if it encounters a `0.0`? Why or why not? Consider how any
short-circuiting might work if you call foldRight with a large list. This is a
deeper question that we'll return to in chapter 5.

No, you can't immediately halt. `foldRight` has to traverse all the way to the
end of the list before it can start evaluating and collapsing the stack. If we
find a `0.0` on the way back up, we can skip over it and start again at `init`,
but we'd still end up evaluating the whole list.
