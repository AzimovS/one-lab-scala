package one.lab.demos.week.three

import scala.util.chaining._

object PipeTap extends App {

//  pipe
//  tap
//  Monad/Functor - Kleisli composition

  "Some string"
    .pipe(x => x.toList)
    .pipe(x => x.map(_.toByte))
    .tap(x => print(x))
}
