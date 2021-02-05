Unfix is a simple library providing infix and postfix notation in Clojure.

Usage:

    (infix [1 + 2] * 3)
    ;=> 9
    
    (postfix 3 2 1 + *)
    ;=> [9]

via. Leiningen:

    [unfix "1.0"]

via Maven:

    <dependency>
      <groupId>unfix</groupId>
      <artifactId>unfix</artifactId>
      <version>1.0</version>
    </dependency>

Testing: `clj -A:test`

More information can be found at the [official unfix site](http://fogus.me/fun/unfix)
