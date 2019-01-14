# scala_practice2

### This collects various Scala applications I wrote over time, including


   1. Vowels group (Vowels, Vowels2, VowelIterate) 
   
      I have 3 implements of algorithm: Swap vowels: swapping only vowels in a string. 
      Ex. "hello" =>  "holle", "academic" => "icedamac", "aei" => "iea" etc.
        
       a) Vowels: this is foldRight solution and the most compact and clean version.  I don't need to reverse lookup (a
          filtered vowel list) or the end result.                                        
          when the processed char is a consonant, just pre-pend it, otherwise consume and prepend one character 
          from the lookup to the result list.  Since we process original list in the reverse order in foldRight,
          I essentially inject vowels as needed in reverse order.          
          
       b) Vowels2: Martin Odersky mentioned in his "Functional Programming Principles in Scala" online Coursera course, 
          for comprehension and the combination of flatMap & filter are exchangeable.  Fold solution and recursive solution
          are sometimes exchangeable too.                      
          However, fold has foldRight and foldLeft two options.  Efficient 
          recursive solution resorts to head and tail of Traversable trait.  It will process original list in order.  
          Therefore, I have to reverse filtered vowels to be a lookup to get swap vowels to work, 
          I provide both simple recursive and tail recursive solutions.  Notice I use ListBuffer solution in tail 
          recursive otherwise I have to reverse end result too.
          
       c) VowelIterate: the iterative (not functional) solution for swap vowels. This literally translate what 
          I did in Java.  However, in java I use 'for' loop instead of while.
          I have var variable j, j will iterate in reverse order to find a vowel when I traverse the original list in
          sequence and encounter a consonant. I cannot use for-comprehensions here. The for-comprehensions block is 
          a self-contained closure.  It won't be able to update external j variable. Therefore, j will be initialized to 
          its initial value s.length -1 in each for-comprehension loop.  I will get wrong answer "hollo" for "hello". 
          A good lesson to understand the difference between traditional for/ while loop and Scala for-comprehensions. 
       
   2. BoardGameSolver
    
      The board game is 10 x 10 matrix bondary.  I cannot step out of the boundary.
      x dimension is vertical line and y dimension is horizontal line.  The followings represents the direction and
      x and y changes.
        
            EAST(0, 1),
            SOUTH(-1, 0),
            WEST(0, -1),
            NORTH(1, 0)
            
      I will receive files each represents board game with step instructions like
      
            PLACE 0, 0, NORTH
            FORWARD
            FORWARD
            TURN RIGHT
            FORWARD
            STATUS
            
      I have to output "Avator is at (2,1) facing EAST"
      
      It's a coding question I applied for one previous job which I was accepted.  I wrote in Java when I applied.
      It makes good use of functional programming in Scala: case class, sealed class, case object(essentially java enum) 
      and pattern-match, Option and recursive processing.  It's well-organized and easy-read. It is an example how we
      can program in Scala way.
    
   3. Anagrams
   
      This originated from "Functional Programming Principles in Scala" course week5 (forcomp) assignment. 
      No assignment solution was ever published.
      
      Word anagrams means words with the same combination of characters.  Ex, "eat", "ate", "tea" are words anagrams of
      ('a', 1), ('e', 1), ('t', 1) which we call it "Occurrences" in this assignment.  Sentence anagrams go further. 
      "Yes", "man" has 14 sentence anagrams like List('en', 'as', 'my').  Each word use one combination of "Occurrences"
      and all words adding up together use exactly ('a', 1), ('e', 1), ('m', 1) ('n', 1), ('s', 1), ('y', 1) which is
      the "Occurrences" of "Yes", "man". 
      
      I draw all words from a dictionary file: formcomp/linuxwords.txt
      
      Anagrams show how I dissect such a complicated task into 6-7 functional steps. See Scaladoc for Anagrams
      The implementations are graceful overall from simple functions like 'wordOccurrences', 'wordAnagrams', 
      to complicated ones 'combinations', 'subtract' and 'add' of "Occurrences"; not to say the finale 
      "sentenceAnagrams" function.  
      
      AnagramsFunSpec is the unit test class.
      
   4. Telecoder
      
      '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", 
      '9' -> "WXYZ" shows the mappings between numeric code and characters on Telephone key pads.        
       
      Given a word "Java" (Ignore case), how can I do reverse look up and get telecode "5282"?
      
      Again, I draw all words from a dictionary file: formcomp/linuxwords.txt.  Given a string of numeric codes like
      "7225", "72252" or "7225247386", how can I find its encoded Set of sentence that each sentence (ignore spaces 
      for multiple words) maps to the exact given numeric code. See the details in Telecoder Scaladoc.
      
      Telecoder is an example used in a session taught in "Functional Programming Principles in Scala’ course.
      This is a preview of Anagrams assignment.  However, it's much simpler than Anagrams.  In Anagrams, 
      you can start with any combination of an "Occurrences":('a', 1), ('e', 1), ('m', 1) ('n', 1), ('s', 1), ('y', 1), 
      Here, you have to follow numeric tele-code in sequence.  It is simply "take n" and mapping to all words with 
      that numeric codes then recurse on "drop n" numeric codes.
      
      TelecodeFlatSpec is the unit test class.  
      
   5. MatrixBinarySearch
               
      It is a binary search on multi-dimension Array with ascending double values. It uses loop function with start and
      end ndex parameters recursively. See codes for more in details.
   
   6. Miscellaneous worksheets
      
      I draw materials either from "Functional Programming Principles in Scala" or from https://www.scala-exercises.org/
      
      a) list_fun.sc: my implementation of transpose, rotation (List(1,2,3,4); List(2,3,4,1),  identity matrix, 
      flatten(multiple nested layers), group using partition and pack using span and FizzBuzz. 
      There are a lot of fun programs.          
      
      b) sort.sc: InsertionSort and MergeSort Int and Generic type implementation using Ordering.
      
      c) stream1.sc: Steam implementation of simple and sieve implemtation of isPrime, primeUpTo.  
         Fibonacci Steam implementation.
         
      d) excerpt2.sc: A combinations of MyNote: a mimic implementation of case class and its companion object with 
         apply and unapply methods; a sealed trait with multiple case object and a function pattern match 
         those case objects; an implementation of Rational class which extends Ordered trait.     
      
      
   
      
          
         
        


