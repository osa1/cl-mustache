CL-Mustache - {{ Mustache }} for Common Lisp
-------------------------------------------

I wrote this project mostly in my free time at my internship to get a better understanding of Lisp way of doing things and contribute to Lisp community. This is my first CL library and I'm still learning CL so there should be room for improvements. I'm open to all advices to improve the code.

Right now this project passes 75 of 88 tests in [mutsache specs](https://github.com/mustache/spec). 9 out of 13 failing tests is about lambdas, which are not yet implemented(I need to add Common Lisp lambdas to YAML files in mustache specs and I don't know anything about YAML file format). Other 4 tests is about whitespaces, ie. whitespace before partials.

Test suite is inspired by [this](http://msnyder.info/posts/2011/12/common-lisp-mustache/) blog post.

Usage
-----

(mustache-render input-stream
                 (yason:parse json-path
                              :object-as :alist
                              :json-arrays-as-vectors t))

TODO
----

 - Lambdas
 - Whitespace problems

