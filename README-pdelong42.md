I decided to fork the original author's project for a couple of reasons:

 - He had some fixes in Github with a more recent version (1.5) than what was
   up on Clojars (1.0).  So I decided to push this version myself, to my own
   namespace in Clojars, for my own use.

 - I had some modifications of my own that I wanted to introduce.

One change I made was to change the operator precedence to be more to my
liking.  For instance, I think '=' and its kin should bind less tightly than
'+', '-', '*', and '/'.  There are also a couple of bugs I'd like to try my
hand at fixing.  I may decide to do a pull request when I'm all done - but this
depends on the original author being amenable to it, naturally.

Examples of some of the bugs I've noticed...

The following expression does not evaluate to the correct result:

    (joy.unfix.infix/infix 1 - 2 * 3 + 4)

The following expression fails because it erroneously attempts to cast a
boolean to a numeric:

    (joy.unfix.infix/infix 1 + 3 * 2 = 7)

Though the following equivalent expression works just fine:

    (joy.unfix.infix/infix 7 = 1 + 3 * 2)

Both cases fail for similar reasons.

These cases have been added to the unit tests.
