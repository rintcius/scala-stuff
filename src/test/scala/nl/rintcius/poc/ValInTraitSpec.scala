package nl.rintcius.poc

import org.specs2.mutable.Specification

/**
 * @author Rintcius Blok
 */
object ValInTraitSpec {

  /**
   * Working hypothesis:
   * - OK: *declare* an abstract val in trait
   * - NOT OK: *refer* to this abstract val from a *val*
   *
   * Counter arguments & Refinements welcome (with evidence)!
   */

  trait A_TraitWithVal {
    val someVal: String // OK!

    def aDef = someVal
    lazy val aLazyVal = someVal
    val aVal = someVal // NOT OK!
  }

  trait B_TraitExtendingA extends A_TraitWithVal {
    override val aVal = someVal // NOT OK!
    override lazy val aLazyVal = someVal
    override def aDef = someVal

    def bDef = someVal
    lazy val bLazyVal = someVal
    val bVal = someVal // NOT OK!
  }

  trait C_TraitRequiringA {
    self: A_TraitWithVal =>

    def cDef = someVal
    lazy val cLazyVal = someVal
    val cVal = someVal // NOT OK!
  }

  trait D_TraitHavingAAsVal {
    val aTraitWithVal: A_TraitWithVal // OK!

    def dDef = aTraitWithVal.someVal
    lazy val dLazyVal = aTraitWithVal.someVal
    val dVil = aTraitWithVal.someVal // NOT OK! (TRAIT D CANNOT EVEN BE INSTANTIATED)
  }

  trait E_TraitHavingAAsVal {
    val aTraitWithVal: A_TraitWithVal // OK!

    def eDef = aTraitWithVal.someVal
    lazy val eLazyVal = aTraitWithVal.someVal
    //difference with D: now without the evil/devil
    //val eVil = aTraitWithVal.someVal
  }
}

import ValInTraitSpec._

class ValInTraitSpec extends Specification {

  "A (the trait itself)" should {

    "[NOT OK] refer to abstract val from val" in {
      new A_TraitWithVal {
        val someVal = "OK"
      }.aVal must_== null
    }

    "[OK] refer to abstract val from lazy val" in {
      new A_TraitWithVal {
        val someVal = "OK"
      }.aLazyVal must_== "OK"
    }

    "[OK] refer to abstract val from lazy def" in {
      new A_TraitWithVal {
        val someVal = "OK"
      }.aDef must_== "OK"
    }
  }

  "B (extends trait)" should {

    "[NOT OK] refer to abstract val from val" in {
      new B_TraitExtendingA {
        val someVal = "OK"
      }.aVal must_== null
    }

    "[OK] refer to abstract val from lazy val" in {
      new B_TraitExtendingA {
        val someVal = "OK"
      }.aLazyVal must_== "OK"
    }

    "[OK] refer to abstract val from def" in {
      new B_TraitExtendingA {
        val someVal = "OK"
      }.aDef must_== "OK"
    }

    "[NOT OK] refer to abstract val from val" in {
      new B_TraitExtendingA {
        val someVal = "OK"
      }.bVal must_== null
    }

    "[OK] refer to abstract val from lazy val" in {
      new B_TraitExtendingA {
        val someVal = "OK"
      }.bLazyVal must_== "OK"
    }

    "[OK] refer to abstract val from def" in {
      new B_TraitExtendingA {
        val someVal = "OK"
      }.bDef must_== "OK"
    }
  }

  "C (requires trait)" should {

    "[NOT OK] refer to abstract val from val" in {
      new C_TraitRequiringA with A_TraitWithVal {
        val someVal = "OK"
      }.aVal must_== null
    }

    "[OK] refer to abstract val from lazy val" in {
      new C_TraitRequiringA with A_TraitWithVal {
        val someVal = "OK"
      }.aLazyVal must_== "OK"
    }

    "[OK] refer to abstract val from def" in {
      new C_TraitRequiringA with A_TraitWithVal {
        val someVal = "OK"
      }.aDef must_== "OK"
    }

    "[NOT OK] refer to abstract val from val" in {
      new C_TraitRequiringA with A_TraitWithVal {
        val someVal = "OK"
      }.cVal must_== null
    }

    "[OK] refer to abstract val from lazy val" in {
      new C_TraitRequiringA with A_TraitWithVal {
        val someVal = "OK"
      }.cLazyVal must_== "OK"
    }

    "[OK] refer to abstract val from def" in {
      new C_TraitRequiringA with A_TraitWithVal {
        val someVal = "OK"
      }.cDef must_== "OK"
    }


  }

  "D - has trait as value; D has val referring to abstract val" should {

    "[NOT OK] refer to abstract val from val => trait D cannot be instantiated" in {
      new D_TraitHavingAAsVal {
        val aTraitWithVal = new A_TraitWithVal {
          val someVal = "OK"
        }
      } must throwA[NullPointerException]
    }
  }

  "E - has trait as value; E has no val referring to abstract val" should {

    "[OK] refer to abstract val from lazy val" in {
      new E_TraitHavingAAsVal {
        val aTraitWithVal = new A_TraitWithVal {
          val someVal = "OK"
        }
      }.eLazyVal must_== "OK"
    }

    "[OK] refer to abstract val from def" in {
      new E_TraitHavingAAsVal {
        val aTraitWithVal = new A_TraitWithVal {
          val someVal = "OK"
        }
      }.eDef must_== "OK"
    }

    "[NOT OK] refer to abstract val from val" in {
      new E_TraitHavingAAsVal {
        val aTraitWithVal = new A_TraitWithVal {
          val someVal = "OK"
        }
      }.aTraitWithVal.aVal must_== null
    }

    "[OK] refer to abstract val from lazy val" in {
      new E_TraitHavingAAsVal {
        val aTraitWithVal = new A_TraitWithVal {
          val someVal = "OK"
        }
      }.aTraitWithVal.aLazyVal must_== "OK"
    }

    "[OK] refer to abstract val from def" in {
      new E_TraitHavingAAsVal {
        val aTraitWithVal = new A_TraitWithVal {
          val someVal = "OK"
        }
      }.aTraitWithVal.aDef must_== "OK"
    }
  }

}
