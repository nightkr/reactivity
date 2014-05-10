package reactivity

trait Observable[+A] {
  def get: A

  final def apply(): A = get

  def addObserver(obs: Observer)

  def removeObserver(obs: Observer)

  /**
   * Return a new (externally) mutable observable initialized with the current value
   */
  def mutableCopy[B >: A]: Observable.Mutable[B] = {
    new Observable.Mutable(get)
  }

  def map[B](f: A => B): Observable[B] = new Observable.Mapped[A, B](this, f)

  def flatMap[B](f: A => Observable[B]): Observable[B] = new Observable.FlatMapped[A, B](this, f)

  override def toString: String = s"Observable($get)"
}

trait BaseObservable[A] extends Observable[A] {
  protected val initial: A

  private var value: A = initial

  private var observers = Set[Observer]()

  override def get: A = value

  protected def update(newValue: A) {
    value = newValue
    notifyObservers()
  }

  override def addObserver(obs: Observer) {
    observers += obs
  }

  override def removeObserver(obs: Observer) {
    observers -= obs
  }

  private[reactivity] def notifyObservers() {
    observers.foreach(_.recalculate())
  }
}

trait Observer {
  def recalculate()
}

object Observable {

  case class Immutable[A](get: A) extends Observable[A] {
    override def addObserver(obs: Observer) {}

    override def removeObserver(obs: Observer) {}
  }

  class Mutable[A](protected val initial: A) extends BaseObservable[A] {
    override def update(newValue: A) {
      super.update(newValue)
    }
  }

  private class Mapped[A, B](origin: Observable[A], f: A => B) extends {
    override protected val initial: B = f(origin.get)
  } with BaseObservable[B] with Observer {
    origin.addObserver(this)

    override def recalculate() {
      update(f(origin.get))
    }
  }

  private class FlatMapped[A, B](origin: Observable[A], f: A => Observable[B]) extends {
    var tempObservable = f(origin.get)

    override protected val initial: B = tempObservable.get
  } with BaseObservable[B] with Observer {
    origin.addObserver(this)
    tempObservable.addObserver(this)

    override def recalculate() {
      tempObservable.removeObserver(this)
      tempObservable = f(origin.get)
      tempObservable.addObserver(this)
      update(tempObservable.get)
    }
  }

}