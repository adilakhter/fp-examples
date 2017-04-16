package eg

import java.util.{Calendar, Date}



object CakePattern extends App {

  //from http://jonasboner.com/real-world-scala-dependency-injection-di/

  case class User(name: String)

  class UserRepository {
    def authenticate(user: User): User = {
      println("authenticating user: " + user)
      user
    }
    def create(user: User) = println("creating user: " + user)
    def delete(user: User) = println("deleting user: " + user)
  }


}



object CakePattern1 extends App {
  sealed  case class User (username: String)

  trait UserRepositoryComponent {
    def userRepository: UserRepository

    trait UserRepository {
      def find(username: String): User
    }
  }

  trait UserRepositoryComponentHibernateImpl extends
    UserRepositoryComponent {

    def userRepository: UserRepository = new UserRepositoryImpl

    class UserRepositoryImpl extends UserRepository {
      def find(username: String): User = {
        println("Find with Hibernate: " + username)
        User(username)
      }
    }
  }

  // Component definition, as before
  trait UserAuthorizationComponent {
    def userAuthorization: UserAuthorization

    trait UserAuthorization {
      def authorize(user: User)
    }
  }

  // Component implementation
  trait UserAuthorizationComponentImpl
    extends UserAuthorizationComponent {
    // Dependencies
    this: UserRepositoryComponent =>

    def userAuthorization = new UserAuthorizationImpl

    class UserAuthorizationImpl extends UserAuthorization {
      def authorize(user: User) {
        println("Authorizing " + user.username)
        // Obtaining the dependency and calling a method on it
        userRepository.find(user.username)
      }
    }
  }

}


object CakePattern2 extends App {

  // http://debasishg.blogspot.nl/2013/02/modular-abstractions-in-scala-with.html
  sealed trait Currency

  case object USD extends Currency

  case object EUR extends Currency

  case object AUD extends Currency

  val baseCurrency: Currency = USD

  val baseCurrencyFactor: Map[Currency, Double] = Map(USD -> 1, EUR -> 1.3, AUD -> 1.2)


  //account
  case class Account(no: String, name: String, openedOn: Date, status: String)

  trait BalanceComponent {
    type Balance

    def balance(amount: Double, currency: Currency, asOf: Date): Balance

    def inBaseCurrency(b: Balance): Balance
  }

  trait Portfolio {
    // abstract balance component
    // does not commit to any specific implementation
    val bal: BalanceComponent

    import bal._

    // Return type of balance
    // the balance in the return type of the method is actually
    // a path dependent typ.
    def currentPortfolio(account: Account): List[Balance]
  }

  trait SimpleBalanceComponent extends BalanceComponent {
    type Balance = (Double, Currency, Date)

    def balance(amount: Double, currency: Currency, asOf: Date): (Double, Currency, Date) =
      (amount, currency, asOf)

    def inBaseCurrency(b: Balance): (Double, Currency, Date) = ((b._1) * baseCurrencyFactor.get(b._2).get, baseCurrency, b._3)
  }

  // report balance as an ADT
  trait CustomBalanceComponent extends BalanceComponent {
    type Balance = BalanceRep

    case class BalanceRep(amount: Double, currency: Currency, asOf: Date)   // balance representation

    override def balance(amount: Double, currency: Currency, asOf: Date): BalanceRep = BalanceRep(amount, currency, asOf)

    override def inBaseCurrency(b: Balance): BalanceRep = BalanceRep((b.amount) * baseCurrencyFactor.get(b.currency).get, baseCurrency, b.asOf)
  }

  trait ClientPortfolio extends Portfolio {
    val bal: BalanceComponent
    import bal._

    override def currentPortfolio(account: Account) = {
      //.. actual impl will fetch from database
      List(
        balance(1000, EUR, Calendar.getInstance.getTime),
        balance(1500, AUD, Calendar.getInstance.getTime)
      )
    }
  }

  trait Auditing extends Portfolio {
    val semantics: Portfolio
    val bal: semantics.bal.type
    import bal._

    override def currentPortfolio(account: Account) = {
      semantics.currentPortfolio(account) map inBaseCurrency
    }
  }

  object SimpleBalanceComponent extends SimpleBalanceComponent
  object CustomBalanceComponent extends CustomBalanceComponent

  object ClientPortfolioAuditService1 extends Auditing {
    val semantics = new ClientPortfolio { val bal = SimpleBalanceComponent }
    val bal: semantics.bal.type = semantics.bal
  }

  object ClientPortfolioAuditService2 extends Auditing {
    val semantics = new ClientPortfolio { val bal = CustomBalanceComponent }
    val bal: semantics.bal.type = semantics.bal
  }

}


// Explicitly typed self Reference

object ExplicitSelfTypeReference {
abstract class Graph {
  type Edge
  type Node <: NodeIntf

  abstract class NodeIntf{
    def connectWith(node: Node): Edge
  }

  def nodes: List[Node]
  def edges: List[Edge]
  def addNode: Node
}

//abstract class DirectedGraph extends Graph {
//  type Edge <: EdgeImpl
//}


}


