CL-Mustache - {{ Mutache }} for Common Lisp
-------------------------------------------

I wrote this project mostly in my free time at my internship to get a better understanding of Lisp way of doing things and contribute to Lisp community. This is my first CL library and I'm still learning CL so there should be room for improvements. I'm open to all advices to improve the code.

Right now this project passes 67 of 88 tests in [mutsache specs](https://github.com/mustache/spec). 10 of 21 tests is about lambdas, which are not yet implemented(I need to add Common Lisp lambdas to YAML files in mustache specs and I don't know anything about YAML file format). Other 11 tests is about whitespaces, ie. removing/leaving newlines after/before tags(I'm trying to solve this with only changing parse tree).

Test suite is inspired by [this](http://msnyder.info/posts/2011/12/common-lisp-mustache/) blog post.

TODO
----

 - Lambdas
 - Whitespace problems
 - Reading from stream insted of strings could be better since users can easily create stream from a string with (with-input-from-string ..), (make-string-input-stream ..) etc.

