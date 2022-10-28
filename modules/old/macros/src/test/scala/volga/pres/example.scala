package volga.pres

import java.util.UUID

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._

final case class User(name: String, age: Int)

trait example {
  def name(id: UUID): String
  def balance(id: UUID): BigDecimal
  def plus(x: BigDecimal, y: BigDecimal): BigDecimal
  def user(name: String, balance: BigDecimal): User

  def total(main: UUID, secondary: UUID): User =
    user(name(main), plus(balance(main), balance(secondary)))
}

trait exampleCat[->[_, _], x[_, _], I] extends Cartesian[->, x, I] {
  def name: UUID -> String
  def balance: UUID -> BigDecimal
  def plus: (BigDecimal x BigDecimal) -> BigDecimal
  def user: (String x BigDecimal) -> User

  def total: (UUID x UUID) -> User =
    (product(name, balance) x balance) >> ar >> (id[String] x plus) >> user

}

trait bindingExample {
  def getName(id: UUID): String
  def getBalance(id: UUID): BigDecimal
  def makeUser(name: String, balance: BigDecimal): User
  def plus(x: BigDecimal, y: BigDecimal): BigDecimal


  def total(main: UUID, secondary: UUID) = {
    val name = getName(main)
    val mainBalance = getBalance(main)
    val secondaryBalance = getBalance(main)
    val totalBalance = plus(mainBalance, secondaryBalance)
    makeUser(name, totalBalance)
  }

}

trait bindingLamExample {
  def let[A, B](x: A)(cont: A => B): B = cont(x)

  def getName: UUID => String
  def getBalance: UUID => BigDecimal
  def plus: BigDecimal => BigDecimal => BigDecimal
  def makeUser: String => BigDecimal => User


  def total: UUID => UUID => User = main => secondary =>
    let(getName(main))(name =>
      let(getBalance(main))(mainBalance =>
        let(getBalance(secondary))(secondaryBalance =>
          let(plus(mainBalance)(secondaryBalance))(totalBalance =>
            makeUser(name)(totalBalance)
          ))))


}

abstract class monadicBindingExample[F[_] : Monad] {
  def getName: UUID => F[String]
  def getBalance: UUID => F[BigDecimal]
  def plus: BigDecimal => BigDecimal => F[BigDecimal]
  def makeUser: String => BigDecimal => F[User]

  def total: UUID => UUID => F[User] = main => secondary =>
    getName(main) flatMap (name =>
      getBalance(main) flatMap (mainBalance =>
        getBalance(secondary) flatMap (secondaryBalance =>
          plus(mainBalance)(secondaryBalance) flatMap (totalBalance =>
            makeUser(name)(totalBalance)
            ))))
}

abstract class monadicForExample[F[_] : Monad] {
  def getName(id: UUID): F[String]
  def getBalance(id: UUID): F[BigDecimal]
  def makeUser(name: String, balance: BigDecimal): F[User]
  def plus(x: BigDecimal, y: BigDecimal): F[BigDecimal]

  def total(main: UUID, secondary: UUID): F[User] =
    for {
      name <- getName(main)
      mainBalance <- getBalance(main)
      secondaryBalance <- getBalance(main)
      totalBalance <- plus(mainBalance, secondaryBalance)
      user <- makeUser(name, totalBalance)
    } yield user
}