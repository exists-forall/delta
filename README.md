# delta-type-inference

This is the official home of Delta's type inference engine.  Delta is a next-generation programming language for solving hard problems with ease and confidence.  We're building Delta because your programming language should be like a great personal assistant, keeping track of thousands of tiny details so you can focus on what's important.

Delta offers many tools to help you fend off the dark forces of code complexity, and the most powerful of these is its type system.  A great type system documents your code, catches bugs, and spends the rest of its time getting out of your way -- it reduces your cognitive burden by doing some of your thinking for you.  The magic that makes this happen is all in this repository.

In particular, this repository contains a type solver, which is responsible for both type inference and type checking.  Delta was designed specifically to support robust type inference, so you almost never have to write an explicit type annotation (except for top-level functions, where types are important as documentation).  This often requires sophisticated non-local analysis, with the inference engine taking into account both how a value is constructed and how it is used.

## A peek at the type system

Jargon is great at making simple things very difficult to understand, but it can also be a very efficient way to communicate between two people who both know it.  For language geeks who know the jargon, Delta has a roughly ML-family nominal type system with Rank-1 parametric polymorphism, algebraic data types, extensible-row-based effect typing, subtyping, typeclasses, higher-kinded types and type-level currying, and a novel form of elidable existential phantom types.  Delta has no implicit let-polymorphism, and its support for subtyping is restricted to keep programmers sane and type inference efficient.

In plain english, here's what all that actually means:

- *"ML-family"*: Spiritually descended from the type system used in the language ML.  A lot of people find "ML-family" versus "everything else" to be a useful way to categorize type systems -- if, like me, you don't really care about that dichotomy, feel free to ignore it

- *"Nominal"*: In Delta, types which are otherwise identical can be considered different just because they have different names ("nominal" means "relating to names").  For example, you might have these two types:

  ```delta
  type Song {
    artist: String;
    year: Int;
  }

  type Painting {
    artist: String;
    year: Int;
  }
  ```

  Delta won't let you use a `Song` where a `Painting` is expected or vice-versa, even though the types are nearly identical. The only thing stopping you from using one in place of the other is that they have different names.  As this example hopefully illustrates, this can help keep you out of trouble.
  
- *"Rank-1 parametric polymorphism"*:  "Parametric polymorphism" is very similar to what other languages call "generics" or sometimes "templates."  It lets you have functions and types which are flexible and can work with any types, instead of having the types hard-coded.  For example, instead of having a function that reverses a list of strings, and a different function that reverses a list of integers, and yet another function that reverses a list of booleans, you can just have a single function that reverses a list of _anything_.

  ```delta
  // Functions which work with lists of hardcoded types
  def reverseStrings(lst: List<String>) -> List<String>;
  def reverseInts(lst: List<Int>) -> List<Int>;
  def reverseBools(lst: List<Bool>) -> List<Bool>;
  
  // A function which can work with lists of any type
  // Delta interprets the lowercase letter as a sort of "wildcard"
  def reverseAnything(List<t>) -> List<t>;
  ```
  
  "Rank-1" is really a way of saying what features Delta *doesn't* have (some languages are Rank-2 or even Rank-N), so it doesn't matter much here.

- *"Algebraic data types"*: Types can have several different variants, called "cases" in Delta.  Values of these types can be any of the variants.  For example:

  ```delta
  type Color {
    case RGB {
      r: UInt8;
      g: UInt8;
      b: UInt8;
    }
    case Grayscale {
      brightness: UInt8;
    }
    alpha: UInt8;
  }
  ```
  
  A value of type `Color` might be an `RGB` color, in which case it has a field for `r` (red), `g` (green), and `b` (blue), or it might be a `GrayScale` color, in which case it only has a field for `brightness`.  In this example, there is an extra [`alpha` (opacity)](https://en.wikipedia.org/wiki/Alpha_compositing) field that doesn't live in either `case`.  Because it's not inside a `case`, the `alpha` field is available on all values -- both `RGB` and `Grayscale` colors have the `alpha` field.  Cases are a bit like subclasses in object-oriented programming, except they let you specify the entire type hiearchy in one place instead of spreading it out across the program.

- *"Row-base effect typing"*: Function signatures in Delta tell you *everything* about a function's inputs and outputs, not just what its arguments and return values are.  Inputs and outputs which are "hidden" in most languages are explicitly documented and verified in Delta, which helps keep your code modular, makes complex codebases easier to navigate, and protects against large classes of subtle bugs.  These hidden inputs and outputs are often called "side effects," but an "effect" sounds like it only refers to hidden *outputs*, when in fact hidden *inputs* are just as important.  Delta calls these hidden inputs and outputs *"interactions,"* because they're a way for functions to *interact* directly with their wider environment without going through arguments or return values.

  ```delta
  // This function has no interactions
  // Its only input is its argument, and its only output is it return value
  def isPrime(x: Int) -> Bool;
  
  // This function interacts with a random number generator
  def randInt(min: Int, max: Int) ! Rand -> Int;
  
  // This function interacts with the outside world
  def askUser(prompt: String) ! IO -> String;
  ```

- *"Subtyping"*: Types can be implicitly converted to other types.  For example, you can pass a `UInt8` to a function that expects a `UInt32`.  It works for user-defined type too; you can pass an `RGB` color where a `Color` is expected.  *Using* subtyping is so easy that you often don't even realize that you're doing it, but it turns out that *implementing* subtypes in a type inference engine is actually quite tricky.  For this reason, many languages with otherwise-powerful type systems, like Haskell, don't support even basic subtyping.

- *"Typeclasses"*: Delta lets you define *"protocols,"* which are a way to define a set of capabilities that many different types may support, and then write code which works with any of those types.  For example, the `CanOrder` protocol defines a common interface for types which have an ordering (like `1 < 2`, or `"aardvark" < "zebra"`).  Other algorithms, like the `sortList` function, can then be defined for any types that implement that protocol.

  ```delta
  type OrderResult {
    case Ascending;
    case Equal;
    case Descending;
  }
  
  protocol CanOrder<t> {
    def order(first: t, second: t) -> OrderResult;
  }
  
  def sortList(lst: List<t>) -> List<t> where CanOrder<t>;
  ```
  
  Delta's "protocols" are somewhat similar to "traits," and "interfaces" in other languages, and they are almost identical to what Haskell calls "typeclasses."

- *"Higher-kinded types and type-level currying"*: Generic functions and types can take other generic types as type parameters.  For example, instead of having one function that maps a function over a linked list, and another function that maps a function over an array, and another function that maps a function over a binary tree, you can just have a single function that maps a function over anything that can be mapped over.  The reason this requires a special feature is that linked lists, arrays, and binary trees are all *themselves* generic types -- they take a type parameter saying what kind of type they're holding.

  ```delta
  // Functions which work with hardcoded container types:
  def mapList(lst: List<a>, func: (a -> b)) -> List<b>;
  def mapArray(arr: Array<a>, func: (a -> b)) -> Array<b>;
  def mapTree(tree: Tree<a>, func: (a -> b)) -> Tree<b>;
  
  // A function which works with *any* container
  // There are three wildcards here: 'a', 'b', and 'c'
  // 'a' and 'b' are ordinary types
  // 'c' is special because it takes a type parameter
  def map(container: c<a>, func: (a -> b)) -> c<b> where CanMap<c>;
  ```

- *"Elidable existential phantom types"*: This is a feature where the way it *technically* works (i.e., the way it's explained in jargon) is very different from the way you should actually think of it most of the time you're using it.  If you don't know what the words "elidable," "existential," or "phantom" mean in this context, just ignore them.  All you need to know is that Delta lets you create values with unique "tags" which prevent them from being used in places where they shouldn't.  For example, you may open several file handles, work with them for a while as part of a "session", and then end the session, which closes the files.  If you accidentally try to read from or write to any of those closed file handles during *another session*, Delta will notice this *at compile time* and inform you of the bug.  The way Delta does this is by tagging all the files from the first session with a tag unique to that session, so that when you try to use them in a different session it can tell that the tags don't match up  Most of the time, you basically don't have to look at or think about these tags -- they only show up if you're writing a library (like the file handling library), or when Delta catches a bug that would've gone unnoticed in another language.  If you've used Rust, you might notice that these tags work a bit like Rust's "lifetime parameters".

  ```delta
  def main() ! IO {
    closedHandle1, closedHandle2 = fileSession {
      handle1 = open("myFile.txt");
      handle2 = open("otherFile.txt");
      handle1.write("Hello...");
      handle2.write("...world!");
      handle1, handle2 // return the handles from the session block
    };
    // The two handles are now closed!
    fileSession {
      handle3 = open("thirdFile.txt");
      handle3.write("This is perfectly fine, since handle3 is open.");
      closedHandle1.write("Not okay!"); // Delta will catch this bug at compile-time
    };
  }
  ```
