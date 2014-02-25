package nl.marcoalkema.test


// tests are used to ttest the behaviour of your code in a semi-automated fashion
class ExampleSpec extends Specification {

  "Foo" should {
    "do something" in {
      true must_== true
    }

    "do something else" in {
      true must_== true
    }
  }
}

