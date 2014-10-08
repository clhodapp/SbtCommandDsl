
import sbt._

trait StateM[S] {
  case class State[A](run: S => (S, A)) {
    def map[B](f: A => B): State[B] = State { s =>
      val (newS, a) = run(s)
      (newS, f(a))
    }
    def flatMap[B](f: A => State[B]): State[B] = State { (s: S) =>
      val (newS, a) = run(s)
      f(a).run(newS)
    }
		def filter(f: A => Boolean)(implicit fa: UnsafePretty.FiltersAllowed.type): State[A] = State { s =>
			val (newState, a) = run(s)
			if (f(a)) (s, a)
			else throw new Exception("It's supposed to be irrefutable, you fool")
		}
		def withFilter(f: A => Boolean)(implicit fa: UnsafePretty.FiltersAllowed.type): State[A] = filter(f)
  }
	object State {
		def get: State[S] = State(s => (s, s))
		def put(s: S): State[Unit] = State(_ => (s, ()))
		def unit[A](a: A): State[A] = State(s => (s, a))
	}
	object UnsafePretty {
		implicit def everythingIsAState[A](a: A): State[A] = State.unit(a)
		implicit object FiltersAllowed
	}
}


object CommandDsl extends StateM[sbt.State] {

	import UnsafePretty._

  private[this] def extracted[T](f: sbt.Extracted => T): State[T] = State.get.map { sbtState =>
    f(Project.extract(sbtState))
  }

  def append(settings: Seq[Def.Setting[_]]): State[Unit] = for {
    sbtState <- State.get
    extracted <- Project.extract(sbtState)
    _ <- State.put(extracted.append(settings, sbtState))
  } yield ()

  def set(settings:Def.Setting[_]*): State[Unit] =
    append(settings)

  def currentLoader: State[ClassLoader] = extracted(_.currentLoader)

  def currentProject: State[ResolvedProject] = extracted(_.currentProject)

  def currentRef: State[ProjectRef] = extracted(_.currentRef)

  def currentUnit: State[LoadedBuildUnit] = extracted(_.currentUnit)

  def get[T](key: SettingKey[T]): State[T] = extracted(_.get(key))

  def get[T](key: TaskKey[T]): State[Task[T]] = extracted(_.get(key))

  def getOpt[T](key: SettingKey[T]): State[Option[T]] = extracted(_.getOpt(key))

  def rootProject[T]: State[URI => String] = extracted(_.rootProject)

  def runAggregated[T](key: TaskKey[T]): State[Unit] = for {
    sbtState <- State.get
    extracted <- Project.extract(sbtState)
    newState <- extracted.runAggregated(key, sbtState)
    _ <- State.put(newState)
  } yield ()


  def runTask[T](key: TaskKey[T]): State[T] = for {
    sbtState <- State.get
    extracted <- Project.extract(sbtState)
    (newState, result) <- extracted.runTask(key, sbtState)
    _ <- State.put(newState)
  } yield result

  def session: State[SessionSettings] = extracted(_.session)

  def structure: State[BuildStructure] = extracted(_.structure)

  def noArg(commandName: String)(f: State[Unit]): Command =
    Command.command(commandName)(sbtState => f.run(sbtState)._1)

  def singleArg(commandName: String)(f: String => State[Unit]): Command =
    Command.single(commandName)((sbtState, arg) => f(arg).run(sbtState)._1)

  def multiArg(commandName: String, argsHint: String)(f: Seq[String] => State[Unit]): Command =
    Command.args(commandName, argsHint)((sbtState, args) => f(args).run(sbtState)._1)

}

