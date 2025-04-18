// import Mill dependency
import mill._
import mill.define.Sources
import mill.modules.Util
import mill.scalalib.TestModule.ScalaTest
import scalalib._
import scalafmt._
// support BSP
import mill.bsp._

object Snax extends SbtModule with ScalafmtModule { m =>
  override def millSourcePath      = os.pwd
  override def scalaVersion        = "2.13.14"
  override def scalacOptions       =
    Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit"
    )
  override def ivyDeps             =
    Agg(
      ivy"org.scala-lang:scala-compiler:2.13.14",
      ivy"org.chipsalliance::chisel:6.4.0",
      ivy"edu.berkeley.cs::chiseltest:6.0.0"
    )
  override def scalacPluginIvyDeps =
    Agg(
      ivy"org.chipsalliance:::chisel-plugin:6.4.0"
    )
  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def ivyDeps =
      m.ivyDeps() ++ Agg(
        ivy"org.scalatest::scalatest::3.2.19"
      )
  }
}
