package one.lab.tasks.week.one

object Operators {

  /**
    * Simple person class, nothing special.
    * override [[toString]] method, so it will work same as [[getPersonInfo]].
    * @param name just name.
    * @param surname just surname.
    * @param age just age.
    */
  class Person(inputname: String, inputsurname: String, inputage: Int) {
    var name = inputname
    var surname = inputsurname
    var age = inputage
  }

  /**
    * Should return formatted string.
    * @param name just name.
    * @param surname just surname.
    * @param age just age.
    * @return should return string in the following format: "name surname age".
    */
  def getPersonInfo(name: String, surname: String, age: Int): String = {
    s"${name} ${surname} ${age}"
  }

  /**
    * should return Person info as in [[getPersonInfo]] method.
    * @param person [[Person]].
    * @return return should be the same as in [[getPersonInfo]].
    * @hint: try to override [[Person.toString]] method.
    */
  def getPersonInfoObject(person: Person): String = {
    s"${person.name} ${person.surname} ${person.age}"
  }

  /**
    * Just compare to Persons by their age and return elder one.
    * @param first person.
    * @param second person.
    * @return eldest person.
    */
  def getElderPerson(first: Person, second: Person): Person = {
    if (first.age > second.age) first
    else second
  }
}
